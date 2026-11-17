(ns piano-trainer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (javax.sound.sampled AudioSystem TargetDataLine AudioFormat DataLine$Info)
           (javax.sound.midi MidiSystem ShortMessage)))

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

(defn normalize-rhythm [rhythm-vector]
  (if (empty? rhythm-vector)
       []
       (let [first-time (first rhythm-vector)]
         (mapv #(- % first-time) rhythm-vector))))          ;;first strike always begins at 0


(defn generate-metronome-rhythm [bpm beats-per-mesure total-measuures]
  (let [interval-ms (/ 60000.0 bpm)
        total-beats (* beats-per-mesure total-measuures)]
    ;;retrun array ms for every beats grouped by measure
    (mapv #(int (* % interval-ms)) (range total-beats))))

(defn load-midi-sequence [filepath]
  (MidiSystem/getSequence (io/file filepath)))

;;extracts all midi events from one track as seq
(defn track-events [track]
  (map #(.get track %) (range (.size track))))

;; is event real note's stroke
(defn note-on-event? [event]
  (let [msg (.getMessage event)]
    (and (instance? ShortMessage msg)
         (= ShortMessage/NOTE_ON (.getCommand ^ShortMessage msg))
         (pos? (.getData2 ^ShortMessage msg)))))

(defn tick-ms [tick resolution tempo]
  (long (/ (* tick tempo) (* resolution 1000))))

(defn parse-midi-to-rhythm
  ([filepath] (parse-midi-to-rhythm filepath 120))
  ([filepath bpm]
   (try
     (let [sequence (load-midi-sequence filepath)
           resolution (.getResolution sequence)
           tempo (long (/ 60000000 bpm))]
       (->> (.getTracks sequence)
            (mapcat track-events)
            (filter note-on-event?)
            (map #(.getTick %))
            (map #(tick-ms % resolution tempo))
            distinct
            sort
            vec
            normalize-rhythm))
     (catch Exception e
       (println "ERROR during reading file: " (.getMessage e))
       []))))

(defn discover-midi-songs [dir-path]
  (let [dir (io/file dir-path)]
    (if (.exists dir)
      (->> (file-seq dir)
           (filter #(and (.isFile %) (.endsWith (.getName %) ".mid")))
           (reduce (fn [acc file]
                     (let [filename (.getName file)
                           composer (.getName (.getParentFile file))
                           song-key (keyword (clojure.string/replace filename #"\.mid$" ""))]
                       (assoc acc song-key {:title filename
                                            :composer composer
                                            :path (.getAbsolutePath file)
                                            :default-bpm 120})))
                   {}))
      (do (println "Folder doesn't exist") {}))))


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
        cooldown-ms 250
        last-hit-time (atom 0)
        previous-amplitude (atom 0)]                        ;;for jump measurement

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
                    current-time (System/currentTimeMillis)
                    amplitude-jump (- max-amplitude @previous-amplitude)]

                (reset! previous-amplitude max-amplitude)
                (when (and (or (> max-amplitude 200) (> amplitude-jump 100))
                           (> (- current-time @last-hit-time) cooldown-ms))
                  ;;noting time of impact relative to the start
                  (let [relative-time (- current-time @session-start-time)]
                    (when (> relative-time 2000)
                      (reset! last-hit-time current-time)   ;;ifnore first 2sec
                      (swap! recorded-rhythm conj relative-time)
                      (println "Note detected. Time: " relative-time "ms, Amplitude: " max-amplitude ", Jump: " amplitude-jump))))))))
        (catch Exception e
          (println "ERROR:" (.getMessage e))
          (.printStackTrace e))))))


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
  (let [played-count (count @recorded-rhythm)
        trimmed-expected (vec (take played-count expected))
        norm-expected (normalize-rhythm trimmed-expected)
        norm-played (normalize-rhythm @recorded-rhythm)
        result (evaluate-rhythm norm-expected norm-played tolerance)]
    (println "RESULT OF PRACTISING:")
    (println result)
    (save-session-to-filee norm-expected norm-played filename)
    result))

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

;;(def all-songs (discover-midi-songs "midi"))
;;(def current-expected-rhythm (parse-midi-to-rhythm (:path (:bach_846 all-songs)) 120))


;;(def loaded-data (load-dataSset "dataset.edn"))
;;(map :played loaded-data)

;;(def current-song (:fur-elize songs))
;;(stop-and-evaluate line current-song 100 "dataset.edn")

;;(def current-expected-rhythm (generate-metronome-rhythm 120 3 10))
;;(def line (start-listening))
;;(def stream (read-audio-stream line))
;;(stop-and-evaluate line current-expected-rhythm 400 "dataset.edn")