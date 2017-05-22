(ns kixi.stats.random.macros
  (:require [kixi.stats.random.protocols :refer [sample-1 sample-n sample-frequencies ISampleable IDiscrete]]
            [clojure.test.check.random :refer [make-random split split-n]]))

(defn ^:no-doc sampleable->seq
  ([^kixi.stats.random.protocols.ISampleable distribution]
   (sampleable->seq distribution (make-random)))
  ([^kixi.stats.random.protocols.ISampleable distribution rng]
   (lazy-seq
    (let [[r1 r2] (split rng)]
      (cons (sample-1 distribution r1)
            (sampleable->seq distribution r2))))))

(defn ^:no-doc default-sample-n
  [^kixi.stats.random.protocols.ISampleable distribution n rng]
  (map #(sample-1 distribution %) (split-n rng n)))

(defn- if-cljs [env then else]
  (if (:ns env) then else))

(defmacro defdistribution [name params & defs]
  (let [definition (fn [sym defs]
                     (some #(when (= (first %) sym) %) defs))]
    `(deftype ^:no-doc ~name
         ~params
         ~'ISampleable
         ~(definition 'sample-1 defs)
         ~(or (definition 'sample-n defs)
            `(sample-n [this# n# rng#]
                       (default-sample-n this# n# rng#)))
         ~@(when-let [form# (definition 'sample-frequencies defs)]
             `(~'IDiscrete ~form#))
         ~@(if (:ns &env)
             (list 'ISeqable
                   `(~'-seq [this] (sampleable->Seq this)))
             `(clojure.lang.ISeq
               (seq [this] (sampleable->seq this)))))))
