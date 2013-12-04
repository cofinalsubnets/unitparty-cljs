(ns unitparty.unit.macros)

(defmacro defunits [ureg & us]
  `(do

     ~@(map (fn [[unit names info df]]
              `(def ~unit (with-meta ~df ~{:names names :info info})))
            us)

     (def ^:dynamic ~ureg
       (persistent!
         (reduce (fn [a# b#]
                   (reduce #(assoc! %1 %2 b#) a# (:names (meta b#))))
                 (transient {}) ~(vec (map first us)))))))

