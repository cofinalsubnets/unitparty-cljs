(ns unitparty.unit.macros)

(defmacro defunits [ureg uniq & us]
  (let [a1 (gensym) a2 (gensym)]
    `(do

       ~@(map (fn [[un md u]] `(def ~un (with-meta ~u ~md))) us)

       (def ^:dynamic ~ureg
         (persistent!
           (reduce (fn [~a1 ~a2]
                     (reduce #(assoc! %1 %2 ~a2) ~a1 (:names (meta ~a2))))
                   (transient {}) ~(vec (map first us)))))

       (def ^:dynamic ~uniq
         (apply hash-map
                (mapcat (fn [~a1] [(-> ~a1 meta :names first) ~a1])
                        (vals ~ureg)))))))

