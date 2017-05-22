(ns kixi.stats.random.distributions
  (:refer-clojure :exclude [shuffle rand-int])
  (:require [kixi.stats.utils :refer [PI sqrt log exp cos pow]]
            [kixi.stats.random.protocols :refer [sample-1 sample-n sample-frequencies]]
            [clojure.test.check.random :refer [make-random rand-double rand-long split split-n]]
            #?(:cljs [kixi.stats.random.macros :refer-macros [defdistribution]]
               :clj [kixi.stats.random.macros :refer [defdistribution]])))

(def ^:no-doc next-rng
  (comp first split))

(defn ^:no-doc swap
  [coll [i1 i2]]
  (assoc coll i2 (coll i1) i1 (coll i2)))

(defn ^:no-doc rand-int
  [a b rng]
  (let [r (* (rand-double rng) (- b a))]
    (int (+ a r))))

(defn ^:no-doc rand-normal
  [rng]
  (let [[r1 r2] (split rng)]
    (* (sqrt (* -2 (log (rand-double r1))))
       (cos (* 2 PI (rand-double r2))))))

(defn ^:no-doc rand-gamma
  [k rng]
  (let [k' (cond-> k (< 1) inc)
        a1 (- k' (/ 1 3))
        a2 (/ 1 (sqrt (* 9 a1)))
        [r1 r2] (split rng)
        [v u] (loop [rng r1]
                (let [[r1 r2] (split rng)
                      [x v] (loop [rng r2]
                              (let [x (rand-normal rng)
                                    v (+ 1 (* a2 x))]
                                (if (<= v 0)
                                  (recur (next-rng rng))
                                  [x v])))
                      v (* v v v)
                      u (rand-double r1)]
                  (if (and (> u (- 1 (* 0.331 (pow x 4))))
                           (> (log u) (+ (* 0.5 x x)
                                         (* a1 (+ 1 (- v) (log v))))))
                    (recur (next-rng r1))
                    [v u])))]
    (if (= k k')
      (* a1 v)
      (* (pow (loop [rng r2]
                (let [r (rand-double rng)]
                  (if (> r 0) r
                      (recur (next-rng rng)))))
              (/ 1 k))
         a1 v))))

(defn ^:no-doc rand-int-tuple
  [a b rng]
  (let [[r1 r2] (split rng)]
    [(rand-int a b r1) (rand-int a b r2)]))

(defn ^:no-doc shuffle
  [coll rng]
  (let [coll (if (vector? coll) coll (vec coll))
        n (count coll)]
    (->> (split-n rng (rand-int 0 (* 2 n) rng))
         (map #(rand-int-tuple 0 n %))
         (reduce swap coll))))

(defdistribution Uniform
  [a b]
  (sample-1 [this rng]
    (+ (* (rand-double rng) (- b a)) a)))

(defdistribution Exponential
  [rate]
  (sample-1 [this rng]
    (/ (- (log (rand-double rng))) rate)))

(defdistribution Binomial
  [n p]
  (sample-1 [this rng]
    (loop [i 0 rng rng result 0]
      (if (< i n)
        (recur (inc i) (next-rng rng)
               (if (< (rand-double rng) p)
                 (inc result)
                 result))
        result)))
  (sample-frequencies [this n' rng]
    (-> (sample-n this n' rng)
        (frequencies))))

(defdistribution Bernoulli
  [p]
  (sample-1 [this rng]
    (< (rand-double rng) p))
  (sample-n [this n rng]
    (let [v (sample-1 (->Binomial n p) rng)]
      (-> (concat (repeat v true)
                  (repeat (- n v) false))
          (shuffle rng))))
  (sample-frequencies [this n rng]
    (let [v (sample-1 (->Binomial n p) rng)]
      {true v false (- n v)})))

(defdistribution Normal
  [mu sd]
  (sample-1 [this rng]
    (+ (* (rand-normal rng) sd) mu)))

(defdistribution Gamma
  [shape scale]
  (sample-1 [this rng]
    (* (rand-gamma shape rng) scale)))

(defdistribution Beta
  [alpha beta]
  (sample-1 [this rng]
    (let [[r1 r2] (split rng)
          u (rand-gamma alpha r1)]
      (/ u (+ u (rand-gamma beta r2))))))

(defdistribution ChiSquared
  [k]
  (sample-1 [this rng]
    (* (rand-gamma (/ k 2) rng) 2)))

(defdistribution F
  [d1 d2]
  (sample-1 [this rng]
    (let [[r1 r2] (split rng)
          x1 (* (rand-gamma (/ d1 2) r1) 2)
          x2 (* (rand-gamma (/ d2 2) r2) 2)]
      (/ (/ x1 d1) (/ x2 d2)))))

(defdistribution Poisson
  [lambda]
  (sample-1 [this rng]
    (let [l (exp (- lambda))]
      (loop [p 1 k 0 rng rng]
        (let [p (* p (rand-double rng))]
          (if (> p l)
            (recur p (inc k) (next-rng rng))
            k))))))

(defn ^:no-doc categorical-sample
  [ks ps n rng]
  (loop [coll '() n n
         rem 1 rng rng
         ks ks ps ps]
    (if (and (seq ks) (> rem 0))
      (let [k (first ks)
            p (first ps)
            x (sample-1 (->Binomial n (/ p rem)) rng)]
        (recur (concat coll (repeat x k)) (- n x)
               (- rem p) (next-rng rng)
               (rest ks) (rest ps)))
      coll)))

(defdistribution Categorical
  [ks ps]
  (sample-1 [this rng]
    (first (categorical-sample ks ps 1 rng)))
  (sample-n [this n rng]
    (shuffle (categorical-sample ks ps n rng) rng))
  (sample-frequencies [this n rng]
    (loop [coll (transient {}) n n
           rem 1 rng rng
           ks ks ps ps]
      (if (and (seq ks) (pos? rem))
        (let [k (first ks)
              p (first ps)
              x (sample-1 (->Binomial n (/ p rem)) rng)]
          (recur (assoc! coll k x) (- n x)
                 (- rem p) (next-rng rng)
                 (rest ks) (rest ps)))
        (if (seq ks)
          (-> (reduce #(assoc! %1 %2 0) coll ks)
              (persistent!))
          (persistent! coll))))))
