(ns piano-trainer.core)
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

