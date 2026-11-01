(ns piano-trainer.core
  (:require [clojure.string :as str])
  (:import (javax.sound.sampled DataLine$Info)))
;; Expected and actual rhythm in milliseconds
(def expected-rhythm [0 500 1000 1500])
(def played-rhythm [10 520 980 1550])

;; Deviation for each note (abs error)
(defn calculate-deviations
      [expected played]
      (map (fn [e p] (abs (- e p)))
           expected
           played))

;;Evaluates the rhythm based on a given tolerance
(defn evaluate-rhythm
      [expected played tolerance]
      (let [differences (calculate-deviations expected played)
            average (double (/ (reduce + differences) (* 1.0 (count differences))))]
           (if (<= average tolerance)
             (str "Rhythm is accurate. Average deviations: "  average " ms")
             (str "Rhythm is inaccurate. Average deviation: " average " ms"))))


(import '[javax.sound.sampled AudioSystem TargetDataLine AudioFormat DataLine$Info])

(defn test-microphone []
  (let [format (AudioFormat. 44100.0 16 1 true true)
        info (DataLine$Info. TargetDataLine format)]
    (if (AudioSystem/isLineSupported info)
      (println "Microphone is supported and ready.")
      (println "Microphone's line is not supported with this format."))))

(defn start-listening []
  (let [format (AudioFormat. 44100.0 16 1 true true)
        info (DataLine$Info. TargetDataLine format)
        ^TargetDataLine line (AudioSystem/getLine info)]
    (.open line format)
    (.start line)
    (println "I'm listening the microfone so play something on the piano!")
    line))
