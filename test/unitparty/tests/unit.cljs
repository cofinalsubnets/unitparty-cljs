(ns unitparty.tests.unit
  (:require
    [unitparty.parser :refer (parse)]
    [unitparty.unit :as unit]
    [cemerick.cljs.test :as t])
  (:require-macros
    [cemerick.cljs.test
     :refer (deftest testing is with-test run-tests test-var)]))

(deftest unit-compatibility
  (let [dim= (fn [a b] (unit/dim= (parse a) (parse b)))]
    (is (dim= "kilogram" "scruple"))
    (is (dim= "shekels/coulomb" "tesla*fortnight"))
    (is (not (dim= "furlong*rods" "millivolts/kilogram")))))

(deftest concrete-unit-conversions
  (let [in-delta (fn [d v1 v2] (< (Math/abs (- v1 v2)) d))
        about    (partial in-delta (Math/pow 10 -6))
        convert  (fn [a b c] (unit/convert (parse a) (parse b) c))]
    (is (about 0        (convert "fahrenheit" "celsius" 32)))
    (is (about 100      (convert "fahrenheit" "celsius" 212)))
    (is (about 0.453592 (convert "pound" "kilograms" 1)))
    (is (about 0.44704  (convert "miles / hour" "meters / second" 1)))

    (let [convert (fn [a b c d] (unit/convert (parse a) (parse b) c d))]
      (testing "with explicit precision"
        (is (= 100 (convert "fahrenheit" "celsius" 212 1)))))
    
    (testing "with metric prefixes"
      (is (about 1000 (convert "kilomile" "mile" 1)))
      (is (about 1000 (convert "megameter" "kilometre" 1)))
      (is (about 1000 (convert "kilokiloinch" "kiloinch" 1))))

    (testing "invalid conversions"
      (is (thrown-with-msg? js/Error #"^Incommensurable units"
                            (convert "milligrams" "megalux" 542))))))


(deftest unit-stringification
  (let [rendered (fn [s u] (= s (unit/show-unit-dimensions (parse u))))]
    (is (rendered "length<sup>2</sup>" "mile*cable"))
    (is (rendered "length<sup>6</sup>×time<sup>-2</sup>" "sverdrups^2"))
    (is (rendered "dimensionless quantity" "hours/second"))))

