(ns unitparty.parser
  (:require [unitparty.unit :as unit]
            [unitparty.unit.defs :as defs]
            [clojure.string :as string]))

;; helpers
(defn- parse-error [& s] (throw (js/Error. (apply str "Parse error: " s))))

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
   (if-let [[m p r] (re-find prefix-regex s)]
     (recur r (+ fact (metric-prefixes p)))
     [s fact])))

(defn- get-unit [u]
  (or (and (map? u) u)
      (defs/*units* (string/lower-case u))
      (let [[s mag] (get-prefix-factor u)]
        (when-not (= 0 mag)
          (unit/umul (get-unit s) {{} (Math/pow 10 mag)})))
      (parse-error "unknown unit: " u)))


;; lexer
(let [tpat #(re-pattern (str "^\\s*(" % ")(.*)"))]
  (def lex-patterns
    {::name  (tpat "[a-zA-Z-]+")
     ::expt  (tpat "\\^")
     ::num   (tpat "-?[0-9]+")
     ::open  (tpat "\\(")
     ::close (tpat "\\)")
     ::op    (tpat "(/|\\*)")}))

(defn- lex1 [s]
  (let [gettok (fn [t [tt pat]]
                 (or t (when-let [r (re-find pat s)]
                         [[tt (nth r 1)] (last r)])))]
    (or (reduce gettok nil lex-patterns)
        (throw (js/Error. (str "Lexical error: \"" s "\""))))))

(defn- lex [s]
  (when-not (string/blank? s)
    (let [[tok remaining] (lex1 s)]
      (conj (lex remaining) tok))))


;; Parser
;;
;; the parser uses multimethods to implement a state machine. this is a neat
;; application for multimethods but it's somewhat frustrated by clojure's lack
;; of TCO, as `recur' doesn't re-dispatch on new arguments (at least not in
;; (this version of) clojurescript).
(defmulti parse-tokens (comp first first))

(defmethod parse-tokens ::name [[[_ uname] & toks]]
  (parse-tokens (conj toks [::unit (get-unit uname)])))

(defmethod parse-tokens ::unit [[[_ u :as tok] & toks]]
  (let [[[tt tc :as nxt] & ntoks] toks]
    (case tt

      ::op (let [[[_ nu] nntoks] (trampoline parse-tokens ntoks)
                 op (if (= tc "*") unit/umul unit/udiv)] 
             (recur (conj nntoks [::unit (op u nu)])))

      ::expt (let [[[tt' tc'] & nntoks] ntoks]
               (if (= tt' ::num)
                 (recur (conj nntoks [::unit (unit/uexp u (js/Number tc'))]))
                 (parse-error "illegal exponent: " tc' " (" tt' ")")))

      (::close nil) [tok toks] ; return toks, not ntoks, so the caller knows if
                               ; we hit EOF

      (parse-error "unexpected token: " nxt))))

(defmethod parse-tokens ::open [[_ & toks]]
  (let [[u ntoks] (trampoline parse-tokens toks)]
    (if (empty? ntoks)
      (parse-error "unmatched '('")
      #(parse-tokens (conj (next ntoks) u)))))

(defmethod parse-tokens :default [[t & toks]]
  (parse-error "unexpected token: " t))

(defn ^:export parse [s]
  (let [[[tt u :as t] ts] (trampoline parse-tokens (lex s))]
    (cond (seq ts)            (parse-error "unmatched ')'")
          (not (= ::unit tt)) (parse-error "unexpected token: " t) ; this should never happen
          :else u)))

