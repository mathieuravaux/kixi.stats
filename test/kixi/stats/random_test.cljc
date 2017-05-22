(ns kixi.stats.random-test
  (:require [kixi.stats.random :as sut]
            [kixi.stats.core :as kixi]
            [clojure.test.check.generators :as gen]
            [clojure.test.check]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :as prop :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :as prop :refer [for-all]]
                 [clojure.test :refer [is deftest]]])))

(def test-opts
  {:num-tests 100
   :par 4})

(def gen-probability
  "Returns a double between 0 and 1 inclusive"
  (gen/fmap #(* % 0.001) (gen/such-that #(<= % 1000) gen/nat)))

(def gen-rate
  (gen/such-that pos? gen-probability))

(def gen-probabilities
  "Returns a vector of probabilities which sum to 1.0"
  (->> (gen/not-empty (gen/vector gen/s-pos-int))
       (gen/fmap (fn [vector]
                   (let [sum (apply + vector)]
                     (->> (concat vector [0 0])
                          (mapv #(double (/ % sum)))))))))

(def gen-shape
  "Returns a double > 0 and <= 10"
  (gen/fmap #(* % 0.001) (gen/choose 1 10000)))

(def gen-categories
  "Returns [[categories] [probabilities]]. Probabilities sum to 1.0"
  (gen/fmap #(vector (range (count %)) %) gen-probabilities))

(defspec seeded-draws-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/draw (sut/uniform a b) {:seed seed})
           (sut/draw (sut/uniform a b) {:seed seed})))
    (is (= (sut/draw (sut/exponential r) {:seed seed})
           (sut/draw (sut/exponential r) {:seed seed})))
    (is (= (sut/draw (sut/bernoulli p) {:seed seed})
           (sut/draw (sut/bernoulli p) {:seed seed})))
    (is (= (sut/draw (sut/binomial {:n n :p p}) {:seed seed})
           (sut/draw (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/draw (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})
           (sut/draw (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})))
    (is (= (sut/draw (sut/categorical ks ps) {:seed seed})
           (sut/draw (sut/categorical ks ps) {:seed seed})))))

(defspec seeded-samples-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/sample n (sut/uniform a b) {:seed seed})
           (sut/sample n (sut/uniform a b) {:seed seed})))
    (is (= (sut/sample n (sut/exponential r) {:seed seed})
           (sut/sample n (sut/exponential r) {:seed seed})))
    (is (= (sut/sample n (sut/bernoulli p) {:seed seed})
           (sut/sample n (sut/bernoulli p) {:seed seed})))
    (is (= (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})
           (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/sample n (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})
           (sut/sample n (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})))
    (is (= (sut/sample n (sut/categorical ks ps) {:seed seed})
           (sut/sample n (sut/categorical ks ps) {:seed seed})))))

(defn mean-convergence-reducer
  [mean]
  (fn
    ([] [0 0 0])
    ([[n m ss :as acc] e]
     (let [n' (inc n)
           m' (+ m (/ (- e m) n'))
           ss (+ ss (* (- e m') (- e m)))
           ci (* (/ ss n') 0.1)]
       (cond
         (> n' 1000000) (reduced false)
         (and (> n' 100)
              (<= (- mean ci) m' (+ mean ci)))
         (reduced true)
         :else [n' m' ss])))
    ([acc] acc)))

(defn converges-to-mean? [distribution mean]
  (transduce identity (mean-convergence-reducer mean) distribution))

(defspec sample-means-converge-to-parameter
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            n gen/nat]
    (is (converges-to-mean? (sut/uniform a b)
                            (+ a (/ (- b a) 2))))
    (is (converges-to-mean? (sut/exponential r)
                            (/ 1 r)))
    (is (converges-to-mean? (sut/binomial {:n n :p p})
                            (* n p)))
    (is (converges-to-mean? (sut/normal {:mu a :sd r})
                            a))
    (is (converges-to-mean? (sut/gamma {:shape s :scale (/ 1 r)})
                            (/ s r)))))

(defspec sample-summary-returns-categorical-sample-frequencies
  test-opts
  (for-all [seed gen/int
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (let [empty-bernoulli-counts {true 0 false 0}
          empty-category-counts (zipmap ks (repeat 0))]
      (is (= (sut/sample-summary n (sut/bernoulli p) {:seed seed})
             (->> (sut/sample n (sut/bernoulli p) {:seed seed})
                  (frequencies)
                  (merge empty-bernoulli-counts))))
      (is (= (sut/sample-summary n (sut/categorical ks ps) {:seed seed})
             (->> (sut/sample n (sut/categorical ks ps) {:seed seed})
                  (frequencies)
                  (merge empty-category-counts)))))))

(defspec uniform-does-not-exceed-bounds
  test-opts
  (for-all [seed gen/int
            [a b] (->> (gen/tuple gen/int gen/int)
                       (gen/such-that (fn [[a b]] (not= a b)))
                       (gen/fmap sort))]
    (let [draw (sut/draw (sut/uniform a b) {:seed seed})]
      (is (and (<= a draw) (< draw b))))))

(defspec exponential-returns-positive-floats
  test-opts
  (for-all [seed gen/int
            r gen-rate]
    (is (float? (sut/draw (sut/exponential r) {:seed seed})))
    (is (pos? (sut/draw (sut/exponential r) {:seed seed})))))

(defspec bournoulli-returns-boolean
  test-opts
  (for-all [seed gen/int
            p gen-probability]
    (is (contains? #{true false} (sut/draw (sut/bernoulli p) {:seed seed})))))

(defspec binomial-returns-integers
  test-opts
  (for-all [seed gen/int
            n gen/nat
            p gen-probability]
    (is (integer? (sut/draw (sut/binomial {:n n :p p}) {:seed seed})))))

(defspec normal-returns-floats
  test-opts
  (for-all [seed gen/int
            mu (gen/double* {:infinite? false :NaN? false})
            sd (gen/double* {:infinite? false :NaN? false :min 0})]
    (is (float? (sut/draw (sut/normal {:mu mu :sd sd}) {:seed seed})))))

(defspec gamma-returns-floats
  test-opts
  (for-all [seed gen/int
            shape gen-shape
            rate gen-rate]
    (is (float? (sut/draw (sut/gamma {:shape shape :scale (/ 0.5 rate)})  {:seed seed})))))

(defspec categorical-returns-supplied-categories
  test-opts
  (for-all [seed gen/int
            [ks ps] gen-categories]
    (is (contains? (set ks) (sut/draw (sut/categorical ks ps) {:seed seed})))))

(defspec bernoulli-probabilities-are-well-behaved
  test-opts
  (for-all [seed gen/int]
    (is (false? (sut/draw (sut/bernoulli 0.0) {:seed seed})))
    (is (true? (sut/draw (sut/bernoulli 1.0) {:seed seed})))))
