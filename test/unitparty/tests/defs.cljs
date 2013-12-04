(ns unitparty.tests.unit.defs
  (:require
    [unitparty.unit.defs :refer (*units* *unique-units*)]
    [cemerick.cljs.test :as t])
  (:require-macros
    [cemerick.cljs.test
     :refer (deftest testing is with-test run-tests test-var)]))

(deftest units-map-tests
  (is (seq *units*)))

