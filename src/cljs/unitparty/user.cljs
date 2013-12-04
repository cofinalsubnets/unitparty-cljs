(ns unitparty.user
  (:require [unitparty.unit :as unit]
            [unitparty.parser :as parser]
            [unitparty.unit.defs :refer (*units*)]))

(def id #(.getElementById js/document %))

(def source (atom nil))
(def target (atom nil))
(def amount (atom nil))
(def max-precision 9)

(defn- doupdate []
  (if (and @source @target @amount)
    (try
      (let [[amt prec] @amount
            out (unit/convert @source @target amt prec)]
        (set! (.-innerText (id "output")) out))
      (catch js/Error err
        (set! (.-innerHTML (id "output"))
              (str "can't convert " (unit/show-unit-dimensions @source)
                   " to " (unit/show-unit-dimensions @target)))))
    (set! (.-innerText (id "output")) "")))

(defn- update-listener [update-targ value-fn]
  (fn [e]
    (this-as elem
      (if (seq (.-value elem))
        (try 
          (swap! update-targ (constantly (value-fn (.-value elem))))
          (.remove (.-classList elem) "error")
          (.add    (.-classList elem) "ok")
          (catch js/Error err
            (do (swap! update-targ (constantly nil))
                (.remove (.-classList elem) "ok")
                (.add (.-classList elem) "error"))))
        (do (.remove (.-classList elem) "ok")
            (.remove (.-classList elem) "error")
            (swap! update-targ (constantly nil)))))
    (doupdate)))

(defn- register-updates! [i update-targ value-fn]
  (let [elem (id i) updater (update-listener update-targ value-fn)]
    (doseq [ev ["blur" "input"]] (.addEventListener elem ev updater))))

(defn- unit-list-entry [uname info]
  (let [elem (.createElement js/document "li")]
    (set! (.-innerText elem) uname)
    (set! (.-title elem) info)
    elem))

(defn- populate-unit-list []
  (let [unitlist (id "unitlist")]
    (->> (vals *units*)
         (map (comp (juxt (comp first :names) :info) meta))
         (apply sorted-set)
         (map #(.appendChild unitlist (apply unit-list-entry %)))
         dorun)))

(defn- prefix-list-entry [prefix mag]
  (let [elem (.createElement js/document "li")]
    (set! (.-innerText elem) prefix)
    (set! (.-title elem) (str "10^" mag))
    elem))

(defn- populate-prefix-list []
  (let [prefixlist (id "prefixlist")]
    (doseq [[p m] (sort-by #(* -1 (last %)) parser/metric-prefixes)]
             (.appendChild prefixlist (prefix-list-entry p m)))))

(defn- strict-number [s]
  (let [n (js/Number s)]
    (if (js/isNaN n) ; NaN != NaN D:
      (throw (js/Error. (str "not a number: " s)))
      n)))

(def amt-prec-pair
  (juxt strict-number
        #(->> % (re-find #"\.\d+") count (min max-precision) (max 1))))

(defn ^:export init []
  (register-updates! "source" source parser/parse)
  (register-updates! "target" target parser/parse)
  (register-updates! "amount" amount amt-prec-pair)
  (populate-unit-list)
  (populate-prefix-list)

  (let [blur (new js/Event "blur")]
    (doseq [e (map id ["source" "target" "amount"])]
      (.dispatchEvent e blur))))

