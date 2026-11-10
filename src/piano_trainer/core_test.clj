(ns piano-trainer.core-test
  (:require [clojure.test :refer :all]
            [piano-trainer.core :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io]))

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

(deftest test-bytes-to-max-amplitude
  (testing "Calculates max amplitude correctly from 16 bit byte array"
    (is (= 0 (bytes-to-max-amplitude (byte-array [0 0]) 2)))))

(deftest test-dataset-save-and-load
  (testing "Saves session to a file and loads it back correctly"
    (let [test-file "test-dataset.edn"
          expected [0 500 1000]
          played [10 490 1010]]

      (when (.exists (io/file test-file))
        (io/delete-file test-file))
      ;;recording
      (save-session-to-filee expected played test-file)
      ;;loading
      (let [loaded (load-dataset test-file)]
        (is (= 1 (count loaded)))
        (is (= expected (:expected (first loaded))))
        (is (= played (:played (first loaded)))))

      (when (.exists (io/file test-file))
        (io/delete-file test-file)))))

;; za pokretanje (run-tests 'piano-trainer.core-test)