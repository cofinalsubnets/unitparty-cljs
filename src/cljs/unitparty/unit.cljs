(ns unitparty.unit
  (:require [clojure.string :as string]))

;; Unit arithmetic functions
(def uadd (partial merge-with +))
(def dmul uadd)
(def usub (partial merge-with -))
(def ddiv usub)

(defn- umul
  "Unit multiplication"
  [u1 u2]
  (let [multerm (fn [[dim1 coeff1] [dim2 coeff2]]
                  [(dmul dim1 dim2) (* coeff1 coeff2)])]
  (apply hash-map (mapcat multerm u1 u2))))

(defn- uneg [u]
  "Unit inversion"
  (apply hash-map
         (mapcat (fn [[dmap c]]
                   [(apply hash-map
                           (mapcat (fn [[d p]]
                                     [d (- p)])
                                   dmap))
                    (/ 1 c)])
                 u)))

(defn- udiv
  "Unit division"
  [u1 u2]
  (umul u1 (uneg u2)))

(defn- uexp
  "Unit exponentiation"
  [u n]
  (if (= n 0) {{} 1}
    (let [base (nth (iterate (partial umul u) u) (- (Math/abs n) 1))]
      (if (> n 0) base (uneg base)))))

(defn- major-dim [u] (last (sort-by #(reduce + 0 (vals %)) (keys u))))

(defn- major-coeff [u] (u (major-dim u)))

;; Unit stringification functions
(defn- show-dim
  "Get a string representing a dimension map"
  [dim]
  (let [show1 (fn [[d p]]
                (cond (= p 0) ""
                      (= p 1) (name d)
                      :else (str (name d) "<sup>" p "</sup>")))]
    (->> dim (map show1) (filter seq) (string/join "×"))))

(defn ^:export show-unit-dimensions
  "Get a string representation of a unit's dimensions"
  [s]
  (-> s major-dim show-dim))

;; Unit conversion functions
(defn ^:export dim=
  "Check units for commensurability."
  [u1 u2]
  (let [normalize
        #(->> % keys (filter seq)
              (map (comp sort (partial filter (fn [[k v]] (not (= 0 v))))))
              sort)]
  (=  (normalize u1) (normalize u2))))


(defn- round-to-precision [p n]
  (let [fact (Math/pow 10 p)]
    (/ (Math/round (* n fact)) fact)))

(defn ^:export convert
  "Unit conversion: source -> dest -> quantity -> optional precision -> quantity"
  ([u1 u2 q] (convert u1 u2 q nil))
  ([u1 u2 q p]
    (if (dim= u1 u2)
      (let [from-src (* (- q (or (u1 {}) 0)) (major-coeff u1))
            to-targ (+ (or (u2 {}) 0) (/ from-src (major-coeff u2)))]
        (if p (round-to-precision p to-targ) to-targ))
      (throw (new js/Error (str "Incommensurable units: "
                                (show-unit-dimensions u1) " <> "
                                (show-unit-dimensions u2)))))))

