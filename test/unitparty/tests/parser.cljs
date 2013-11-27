(ns unitparty.tests.parser
  (:require
    [unitparty.parser :refer (parse lex)]
    [unitparty.unit.defs :refer (*units*)]
    [cemerick.cljs.test :as t])
  (:require-macros
    [cemerick.cljs.test
     :refer (deftest testing is with-test run-tests test-var)]))


(deftest parser-tests
  (let [parsed-as (fn [a b] (= (*units* a) (parse b)))
        parse= (fn [a b] (= (parse a) (parse b)))]

    (is (parsed-as "kilogram" "kilogram"))
    (is (parsed-as "mile" "(((mile)))"))
    (is (parsed-as "angstrom" "   angstrom "))

    (is (parse= "kilogram" "kilogram"))
    (is (parse= "meter^2" "metre*meter"))
    (is (parse= "(meter/second)^2" "meters^2/seconds^2"))
    (is (parse= "newtons" "kilogram * meters / second ^ 2"))
    (is (parse= "megagram" "kilokilogram"))
    (testing "parse errors"
      (is (thrown-with-msg? js/Error #"Parse error" (parse "(meter")))
      (is (thrown-with-msg? js/Error #"Parse error" (parse "meter)")))
      (is (thrown-with-msg? js/Error #"Parse error" (parse "meter(")))
      (is (thrown-with-msg? js/Error #"Parse error" (parse ")meter")))
      (is (thrown-with-msg? js/Error #"Parse error" (parse "(furlong))")))
      (is (thrown-with-msg? js/Error #"Parse error" (parse "()second")))
      
      )))

(deftest lexer-tests
  (is (thrown-with-msg? js/Error #"Lexical error" (lex "~kilogram~~"))))

