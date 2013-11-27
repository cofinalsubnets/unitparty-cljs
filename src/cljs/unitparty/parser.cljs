(ns unitparty.parser
  (:require [unitparty.unit :as unit]
            [unitparty.unit.defs :refer (*units*)]
            [clojure.string :as string]))

(defn- parse-error [s] (throw (js/Error (str "Parse error: " s))))

(defn- lex
  "Return [next-token rest-of-string] or raise an error"
  [s]
  (let [tpat (fn [s] (re-pattern (str "^\\s*(" s ")(.*)"))) 

        pats {:tunit  (tpat "[a-zA-Z]+")
              :texpt  (tpat "\\^")
              :tnum   (tpat "-?[0-9]+")
              :topen  (tpat "\\(")
              :tclose (tpat "\\)")
              :top    (tpat "(/|\\*)")}
        
        gettok (fn [t [pn p]]
                 (or t (let [r (re-find p s)]
                         (when r [pn (nth r 1) (last r)]))))]

    (or (reduce gettok nil pats)
        (throw (js/Error (str "Lexical error: \"" s "\""))))))

(defn- tokens
  "Convert a unit string into a list of tokens or raise an error"
  [s]
  (when (not (string/blank? s))
    (let [[tokt tokc remaining] (lex s)]
      (conj (tokens remaining) [tokt tokc]))))


(defn- group
  "Parse raw token stream into nested sequences"
  ([toks stack]
   (cond
     (= nil (first (first toks)))
     (if (empty? (rest stack))
       (first stack)
       (parse-error "Unmatched '('"))
     (= :topen (first (first toks))) (recur (rest toks) (conj stack []))
     (= :tclose (first (first toks)))
     (recur (rest toks)
            (let [base (rest (rest stack))
                  newtop (first (rest stack))]
              (if (not (nil? newtop))
                (conj base (conj newtop [:tgrp (first stack)]))
                (parse-error "Unmatched ')'"))))
     true (recur (rest toks) (conj (rest stack) (conj (first stack) (first toks))))))
  ([toks] (group toks (list []))))

(def metric-prefixes
  ;; prefixes -> magnitude
  {"yotta" 24
   "zetta" 21
   "exa"   18
   "peta"  15
   "tera"  12
   "giga"  9
   "mega"  6
   "kilo"  3
   "hecto" 2
   "deca"  1
   "deci"  -1
   "centi" -2
   "milli" -3
   "micro" -6
   "nano"  -9
   "pico"  -12
   "femto" -15
   "atto"  -18
   "zepto" -21
   "yocto" -24})

(def prefix-regex
  (re-pattern (str "^(" (string/join "|" (keys metric-prefixes)) ")(.*)")))

(defn- get-prefix-factor
  "Read a sequence of metric prefixes and return a scaling factor and a base unit."
  ([s] (get-prefix-factor s 0))
  ([s fact]
  (let [[m p r] (re-find prefix-regex s)]
    (if m
      (recur r (+ fact (metric-prefixes p)))
      [fact s]))))
(defn- get-unit
  "unit name -> unit"
  [u]
  (or (*units* u)
      (let [[mag s] (get-prefix-factor u)]
        (when mag (unit/umul (get-unit s) {{} (Math/pow 10 mag)})))
      (throw (js/Error (str "Unknown unit: " u)))))


(declare expify)
(defn- parse'
  "Internal parser"
  [t]
  (let [[[tt1 tc1 :as t1] [tt2 tc2 :as t2] [tt3 tc3 :as t3] & ts] (expify t)]
    (cond

      (= tt1 :tgrp)
      (recur (concat (list (parse' tc1) t2 t3) ts))

      (and  (= tt1 :tunit) (= tt2 :top))
      (cond (= tt3 :tunit) (recur (conj ts [:tunit ((if (= tc2 "*") unit/umul unit/udiv) tc1 tc3)]))
            (= tt3 :tgrp)  (recur (concat (list t1 t2 (parse' tc3)) ts))
            true           (parse-error t))

      (and (= tt1 :tunit) (not tt2))
      t1

      true (parse-error t))))

(defn- unify
  "Convert unit tokens into units"
  [[tt tc :as t]]
  (condp = tt
         :tunit [tt (get-unit tc)]
         :tgrp  [tt (map unify tc)]
         t))

(defn- expify
  "Parse unit exponentiation"
  [[[tt1 tc1 :as t1] [tt2 tc2 :as t2] [tt3 tc3 :as t3] & ts :as t]]
  (cond

      (empty? t) t
      (and  (= tt1 :tunit) (= tt2 :texpt) (= tt3 :tnum))
      (conj (expify ts) [:tunit (unit/uexp tc1 (js/Number tc3))])

      (= tt1 :tgrp) (expify (conj (rest t) (parse' tc1)))

      true (conj (expify (rest t)) t1)))

(defn ^:export parse [s]
  "Parse a string into a unit"
  (->> s tokens group (map unify) expify parse' last))

