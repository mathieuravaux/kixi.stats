(ns kixi.stats.random.protocols)

(defprotocol ^:no-doc ISampleable
  (sample-1 [this rng])
  (sample-n [this n rng]))

(defprotocol ^:no-doc IDiscrete
  (sample-frequencies [this n rng]))

