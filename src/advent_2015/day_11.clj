(ns advent-2015.day-11)


(defn next-pwd [^String pwd]
  (let [s (StringBuilder. pwd)]
    (loop [i (dec (count s))]
      (let [c (.charAt s i)]
        (if (= c \z)
          (do (.setCharAt s i \a)
              (recur (dec i)))
          (.setCharAt s
                      i
                      (char (inc (int c)))))))
    (.toString s)))


(defn good-pwd? [pwd]
  (boolean (and (seq (filter (fn [[x :as cs]]
                               (= cs
                                  (range x (+ 3 x))))
                             (partition 3
                                        1
                                        (map int
                                             pwd))))
                (not (re-find #"[iol]" pwd))
                (re-find #"(.)\1.*(.)\2" pwd))))


(defn part-1 [in]
  (first (filter good-pwd?
                 (next (iterate next-pwd in)))))


(defn part-2 [in]
  (part-1 (part-1 in)))

(comment
  (def input "hepxcrrq")
  (good-pwd? "abbcegjk")
  (part-1 input)
  (part-2 input)

  )
