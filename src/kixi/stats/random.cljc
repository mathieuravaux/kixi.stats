(ns kixi.stats.random
  (:require[kixi.stats.random.distributions :as dist]
            [kixi.stats.random.protocols :refer [sample-1 sample-n sample-frequencies]]
            [clojure.test.check.random :refer [make-random]]))

(defn uniform
  "Returns a uniform distribution.
  Params: a ∈ ℝ, b ∈ ℝ"
  [a b]
  (dist/->Uniform a b))

(defn exponential
  "Returns an exponential distribution.
  Params: rate ∈ ℝ > 0"
  [rate]
  (dist/->Exponential rate))

(defn bernoulli
  "Returns a Bernoulli distribution.
  Params: p ∈ [0 1]"
  [p]
  (dist/->Bernoulli p))

(defn binomial
  "Return a binomial distribution.
  Params: {:n ∈ ℕ, :p ∈ [0 1]}"
  [{:keys [n p]}]
  (dist/->Binomial n p))

(defn normal
  "Returns a normal distribution.
  Params: {:mu ∈ ℝ, :sd ∈ ℝ}"
  [{:keys [mu sd]}]
  (dist/->Normal mu sd))

(defn gamma
  "Returns a gamma distribution.
  Params: {:shape ∈ ℝ, :scale ∈ ℝ}"
  [{:keys [shape scale] :or {shape 1.0 scale 1.0}}]
  (dist/->Gamma shape scale))

(defn beta
  "Returns a beta distribution.
  Params: {:alpha ∈ ℝ, :beta ∈ ℝ}"
  [{:keys [alpha beta] :or {alpha 1.0 beta 1.0}}]
  (dist/->Beta alpha beta))

(defn chi-squared
  "Returns a chi-squared distribution.
  Params: k ∈ ℕ > 0"
  [k]
  (dist/->ChiSquared k))

(defn f
  "Returns an F distribution.
  Params: d1 ∈ ℕ > 0, d2 ∈ ℕ > 0"
  [d1 d2]
  (dist/->F d1 d2))

(defn poisson
  "Returns a Poisson distribution.
  Params: lambda ∈ ℝ > 0"
  [lambda]
  (dist/->Poisson lambda))

(defn categorical
  "Returns a categorical distribution.
  Params: [k1, ..., kn], [p1, ..., pn]
  where k1...kn are the categories
  and p1...pn are probabilities.
  Probabilities should be >= 0 and sum to 1"
  [ks ps]
  (dist/->Categorical ks ps))

(defn draw
  "Returns a single sample from the distribution.
  An optional seed long will ensure deterministic results"
  ([^kixi.stats.random.protocols.ISampleable distribution]
   (draw distribution {}))
  ([^kixi.stats.random.protocols.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-1 distribution rng))))

(defn sample
  "Returns n samples from the distribution.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.random.protocols.ISampleable distribution]
   (sample n distribution {}))
  ([n ^kixi.stats.random.protocols.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-n distribution n rng))))

(defn sample-summary
  "Returns a summary count of each variate for a sample
  of a given length from a discrete distribution
  such as the Bernoulli, binomial or categorical.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.random.protocols.IDiscrete distribution]
   (sample-summary n distribution {}))
  ([n ^kixi.stats.random.protocols.IDiscrete distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-frequencies distribution n rng))))
