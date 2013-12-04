(ns unitparty.unit
  (:require [clojure.string :as string]))

;; helpers
(def degree #(->> % vals (reduce +)))
(def major-dim #(->> % keys (sort-by degree) last))
(def major-coeff #(% (major-dim %)))

;; Unit arithmetic functions
(def uadd (partial merge-with +))
(def usub (partial merge-with -))
(def dmul uadd)
(def ddiv usub)

;; unit multiplication has to deal somehow with units that have different
;; origins, e.g. fahrenheit and celsius. we do this by ignoring the constant
;; offset and treating the operands as displacements rather than positions,
;; which seems to be how wolfram alpha does it. this invites the question of
;; why we're sticking with a polynomial representation of unit dimensions at
;; all, but w/e, it ain't exactly broke.
;;
;; in the original haskell version we did exactly the opposite of this and
;; refused to compute conversions like celsius^2 => kelvins^2 on the grounds
;; that dimension polynomials with more than one non-constant term are not
;; meaningful. the symmetry between rigor (pedantry?) of language and program
;; is hella pleasing to me.
(defn- umul [u1 u2]
  (let [d1 (major-dim u1) d2 (major-dim u2)]
    {(dmul d1 d2) (* (u1 d1) (u2 d2))}))

(defn- uneg [u]
  (apply hash-map
         (mapcat (fn [[dmap c]]
                   [(apply hash-map
                           (mapcat (fn [[d p]]
                                     [d (- p)])
                                   dmap))
                    (/ 1 c)])
                 u)))

(defn- udiv [u1 u2] (umul u1 (uneg u2)))

(defn- uexp [u n]
  (if (= n 0) {{} 1}
    (let [base (nth (iterate (partial umul u) u) (- (Math/abs n) 1))]
      (if (> n 0) base (uneg base)))))

;; Unit stringification functions
(defn- show-dim [dim]
  (let [show1 (fn [[d p]] (case p 0 ""
                                  1 (name d)
                                  (str (name d) "<sup>" p "</sup>")))
        checkdim #(if (string/blank? %) "dimensionless quantity" %)]
    (->> dim (map show1) (filter seq) (string/join "×") checkdim)))

(defn ^:export show-unit-dimensions [s] (-> s major-dim show-dim))

(def normalize
  #(->> % keys (map (comp sort (partial filter (fn [[k v]] (not (= 0 v))))))
        (filter seq)
        sort))

;; Unit conversion functions
(defn ^:export dim= [u1 u2] (=  (normalize u1) (normalize u2)))

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
      (throw (js/Error. (str "Incommensurable units: "
                             (show-unit-dimensions u1) " <> "
                             (show-unit-dimensions u2)))))))

