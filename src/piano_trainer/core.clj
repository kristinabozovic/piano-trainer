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

(defn bytes-to-max-amplitude
  "Evaluating max amplitude from the bytes array (16 bit)"
  [buffer bytes-read]
  (loop [i 0 max-val 0]
    (if (>= i bytes-read)
      max-val
      (let [b1 (aget buffer i)
            b2 (aget buffer (+ i 1))
            sample (bit-or (bit-shift-left (bit-and b2 255) 8)
                           (bit-and b1 255))
            signed-sample (if (> sample 32767) (- sample 65536) sample)
            abs-val (abs (long signed-sample))]
        (recur (+ i 2) (max max-val abs-val))))))


(defn read-audio-stream [^TargetDataLine line]
  (let [buffer-size 1024
        buffer (byte-array buffer-size)]
    (println "Audio stream reading started, so you can play something...")
    (future
      (try
        (while (.isOpen line)
          (let [bytes-read (.read line buffer 0 buffer-size)]
            (when (> bytes-read 0)
              ;;izracunavanje jacine zvuka iz 16bitnog uzorka
              (let [max-amplitude (bytes-to-max-amplitude buffer bytes-read)]
                ;;jacinaaa > prag -> onda je detektovan zvuk
                (when (> max-amplitude 1000)
                  (println "Sound detected. Amplitude: " max-amplitude))
                )
              )))
        (catch Exception e
          (println "ERROR DETAILS:" (.getMessage e))
          (.printStackTrace e)
          )))))

;; (def line (start-listening))  starting microfone and savingline in variable
;; (def stream (read-audio-stream line)) to start reading audio stream in background thread
;; (.close line)