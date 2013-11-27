(ns unitparty.user
  (:require [unitparty.unit :as unit]
            [unitparty.parser :as parser]
            [unitparty.unit.defs :refer (*unique-units*)]))

(defn- di [s] (.getElementById js/document s))
(defn- dc [s] (.getElementsByClassName js/document s))

(def source (atom nil))
(def target (atom nil))
(def amount (atom nil))
(def max-precision 9)


(defn- doupdate []
  (if (and @source @target @amount)
    (try
      (let [[amt prec] @amount
            out (unit/convert @source @target amt prec)]
        (set! (.-innerText (di "output")) out))
      (catch js/Error err
        (set! (.-innerHTML (di "output"))
              (str "can't convert " (unit/show-unit-dimensions @source)
                   " to " (unit/show-unit-dimensions @target)))))
    (set! (.-innerText (di "output")) "")))

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
            (swap! update-targ (constantly nil))))
      (doupdate))))

(defn- register-updates! [id update-targ value-fn]
  (let [elem (di id)
        updater  (update-listener update-targ value-fn)
        keyhandler (fn [ev]
                  (this-as this
                    (if (= (.-keyCode ev) 13)
                      (.call updater this ev)
                      (.call updater this false))))]
    (.addEventListener elem "blur"     updater)
    (.addEventListener elem "input" keyhandler)))

(defn- unit-list-entry [u]
  (let [elem (.createElement js/document "li")
        {[uname & _] :names info :info} (meta u)]
    (set! (.-innerText elem) uname)
    (set! (.-title elem) info)
    (set! (.-className elem) "unit list_entry")
    (set! (.-id elem) (str "unit_list_" uname))
    elem))

(defn- populate-unit-list []
  (let [unitlist (di "unitlist")]
    (dorun (for [u (sort-by (comp first :names meta ) (vals *unique-units*))]
             (.appendChild unitlist (unit-list-entry u))))))

(defn- prefix-list-entry [prefix mag]
  (let [elem (.createElement js/document "li")]
    (set! (.-innerText elem) prefix)
    (set! (.-title elem) (str "10^" mag))
    (set! (.-className elem) "prefix list_entry")
    (set! (.-id elem) (str "prefix_list_" prefix))
    elem))

(defn- populate-prefix-list []
  (let [prefixlist (di "prefixlist")]
    (dorun (for [[p m] (sort-by #(* -1 (last %)) parser/metric-prefixes)]
             (.appendChild prefixlist (prefix-list-entry p m))))))

(defn- amt-prec-pair [txt]
  (let [amt (js/Number txt)
        dml (re-find #"\.\d+" txt)]
    [amt (if dml
           (Math/min max-precision (count dml))
           1)]))

(defn ^:export init []
  (register-updates! "source" source parser/parse)
  (register-updates! "target" target parser/parse)
  (register-updates! "amount" amount amt-prec-pair)
  (populate-unit-list)
  (populate-prefix-list)
  (let [blur (new js/Event "blur")]
    (dorun (for [e (map di ["source" "target" "amount"])]
      (.dispatchEvent e blur)))))

