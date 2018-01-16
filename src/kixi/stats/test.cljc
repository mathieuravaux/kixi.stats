(ns kixi.stats.test
  (:require [kixi.stats.math :refer [abs sq sqrt lower-regularized-gamma incomplete-beta]]
            [kixi.stats.data]))

(defn chisq-test
  [^kixi.stats.data.ITable table]
  (let [+' (fnil + 0)
        [xs ys total] (reduce (fn [[xs ys total] [[x y] n]]
                                [(update xs x +' n)
                                 (update ys y +' n)
                                 (+ total n)])
                              [{} {} 0]
                              table)
        dof (* (dec (count xs)) (dec (count ys)))
        stat (->> (for [x (keys xs) y (keys ys)] (vector x y))
                  (reduce (fn [acc [x y]]
                            (let [e (/ (* (get xs x) (get ys y)) total)]
                              (+ acc (/ (sq (- e (get table [x y]))) e))))
                          0))]
    {:p-value (- 1 (lower-regularized-gamma (/ dof 2.0) (/ stat 2.0)))
     :X-sq (double stat)
     :dof dof}))

(defn cdf-t
  [x dof]
  (if (zero? x) 0.5
      (let [t  (incomplete-beta (/ dof (+ (sq x) dof))
                                (* 0.5 dof)
                                0.5)]
        (if (pos? x)
          (- 1 (* 0.5 t))
          (* 0.5 t)))))

(defn t-test
  ":paired :one-sided?"
  [{a-mean :mu a-sd :sd a-n :n :as a}
   {b-mean :mu b-sd :sd b-n :n :as b}
   & [{:keys [paired?] :as opts}]]
  (when paired? (assert (= a-n b-n)))
  (let [one-sample? (number? b)
        t (if one-sample?
            (/ (- a-mean b)
               (/ a-sd (sqrt a-n)))
            (/ (- a-mean b-mean)
               (sqrt (+ (/ (sq a-sd) a-n)
                        (/ (sq b-sd) b-n)))))
        dof (cond one-sample? (dec a-n)
                  paired? (dec a-n)
                  :else
                  (/ (sq (+ (/ (sq a-sd) a-n) (/ (sq b-sd) b-n)))
                     (+ (/ (sq (/ (sq a-sd) a-n)) (dec a-n))
                        (/ (sq (/ (sq b-sd) b-n)) (dec b-n)))))]
    {:p-value (* 2 (cdf-t (- (abs t)) dof))
     :t t
     :dof (double dof)}))
