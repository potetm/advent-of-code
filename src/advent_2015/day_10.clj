(ns advent-2015.day-10)


(defn look-and-say [init n]
  (nth (iterate (fn [s]
                  (apply str
                         (map (fn [[n :as s]]
                                (str (count s)
                                     (Character/digit ^char n 10)))
                              (partition-by identity
                                            s))))
                init)
       n))


(defn part-1 [in]
  (count (look-and-say in 40)))


(defn part-2 [in]
  (count (look-and-say in 50)))


(comment
  (def input "3113322113")
  (part-1 input)
  (part-2 input)

  )
