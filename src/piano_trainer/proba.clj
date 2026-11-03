(ns piano-trainer.proba)

(defn myf [e] (abs (- 5 e)))


;;(reduce #(conj (abs (- 5 e))) [] [1 2 3 4])
(reduce conj '() [1 2 3 4])


(defn add-first [rez x] cons (x rez))
(into '() '(1 2 3 4))
(partition 2 [1 2 18 36 73])

(defn myStep [acc e]
  (conj acc [e]))
(reduce myStep [] [1 2 18 26 78])

(defn fja [seq n] (first (drop n seq)))                     ;;nti elem varaca
(defn fja2 [coll] (reduce (fn [acc _] (inc acc)) 0 coll))   ;;broj elem seq
#(loop [c 0 coll %]                                         ;;borj elem kolekcije
   (if (empty? coll)
     c)
   (recur (inc c) (rest coll)))

#(into '() %)                                               ;;pretvara u seq
#(reduce + %)                                               ;;zbir elem seq
(reduce (fn [acc e] (if (= 1 (/ e 2)) e)) 0 [1 2 3 4 5 6])
(defn fja4 [n seq] (take n seq))
#(take % (map first (iterate  (fn [[a b]] [b (+ a b)]) [1 1])))

;;(map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))

(partial [1 2 14 16 69])
(defn podeli-na-dva [coll]
  (loop [s coll acc []]
    (if (empty? s)
      acc
      (recur (drop 2 s)
             (conj acc (vec (take 2 s)))))))

(defn podeli [coll]
  (loop [c coll acc []]
    (if empty? c)
    acc
    (recur (drop 2 c))))