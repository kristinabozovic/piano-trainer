(ns piano-trainer.core-test
  (:require [clojure.test :refer :all]
            [piano-trainer.core :refer :all]
            [clojure.string :as str]))

(deftest test-calculate-deviations
         (testing "Testing the calculation of absolute deviations in rhythm")
         (let [expected [0 500 1000 1500]
               played [10 520 980 1550]]
           ;;expected differences [10 20 20 50]
           (is (= [10 20 20 50] (calculate-deviations expected played)))
           ))

(deftest test-evaluate-rhythm
         (testing "Testing evaluation when the rhythm is accurate whithin tolerance"
           (let [result (evaluate-rhythm [0 500] [0 510] 20)]
             (is (str/includes? result "accurate"))))

         (testing "Testing evaluation when the rhythm is inaccurate out og tolerance"
           (let [result (evaluate-rhythm [0 500] [0 570] 20)]
             (is (str/includes? result "inaccurate"))))
         )

;; za pokretanje (run-tests 'piano-trainer.core-test)