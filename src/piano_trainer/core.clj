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
            average (double (/ (reduce + differences) (count differences)))]
           (if (<= average tolerance)
             (str "Rhythm is accurate. Average deviations: "  average " ms")
             (str "Rhythm is inaccurate. Average deviation: " average " ms"))))

(import '[javax.sound.sampled AudioSystem TargetDataLine AudioFormat DataLine$Info])

(defn test-microphone []
  (let [format (AudioFormat. 48000.0 16 1 true false)
        info (DataLine$Info. TargetDataLine format)]
    (if (AudioSystem/isLineSupported info)
      (println "Microphone is supported and ready.")
      (println "Microphone's line is not supported with this format."))))

(defn start-listening []
  (let [format (AudioFormat. 48000.0 16 1 true false)
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

(def session-start-time (atom nil))
(def recorded-rhythm (atom []))

(defn read-audio-stream [^TargetDataLine line]
  (let [buffer-size 1024
        buffer (byte-array buffer-size)
        cooldown-ms 200                                     ;;min time between 2 impacts u ms
        last-hit-time (atom 0)]

    (reset! session-start-time (System/currentTimeMillis))
    (reset! recorded-rhythm [])

    (println "Audio stream reading started, so you can play something...")
    (future
      (try
        (while (.isOpen line)
          (let [bytes-read (.read line buffer 0 buffer-size)]
            (when (> bytes-read 0)
              ;;izracunavanje jacine zvuka iz 16bitnog uzorka
              (let [max-amplitude (bytes-to-max-amplitude buffer bytes-read)
                    current-time (System/currentTimeMillis)]
                (when (and (> max-amplitude 150)
                           (> (- current-time @last-hit-time) cooldown-ms))
                  ;;noting time of impact relative to the start
                  (reset! last-hit-time current-time)
                  (let [relative-time (- current-time @session-start-time)]
                    (swap! recorded-rhythm conj relative-time)
                    (println "Note detected. Time: " relative-time "ms, Aplitude: " max-amplitude)))
                )
              )))
        (catch Exception e
          (println "ERROR:" (.getMessage e))
          (.printStackTrace e)
          )))))


(require '[clojure.java.io :as io])

(defn save-session-to-filee [expected played filename]
  (let [session-data {:timestamp  (System/currentTimeMillis)
                      :expected   expected
                      :played     played
                      :deviations (calculate-deviations expected played)}]
    ;;write to a file in EDN format and move to a new line
    (spit filename (str (pr-str session-data) "\n") :append true)
    (println "Session is successfully saved to a file: " filename)))


(defn stop-and-evaluate [line expected tolerance filename]
  (.close line)
  (let [result (evaluate-rhythm expected @recorded-rhythm tolerance)]
    (println "RESULT OF PRACTISING:")
    (println result)
    (save-session-to-filee expected @recorded-rhythm filename)
    result))

;;(def line (start-listening))
;;(def stream (read-audio-stream line))
;;(stop-and-evaluate line expected-rhythm 100 "dataset.edn")

(defn load-dataset [filename]
  (try
    (with-open [rdr (io/reader filename)]
      (->> (line-seq rdr)                                   ;;reading line by line, transforming string into map
           (remove clojure.string/blank?)
           (map read-string)
           vec))
    (catch java.io.FileNotFoundException e
           (println  "File is not found: " (.getMessage e))
      [])
    (catch Exception e
      (println "ERROR during reading file: " (.getMessage e))
      [])))
;;(def loaded-data (load-dataSset "dataset.edn"))
;;(map :played loaded-data)
