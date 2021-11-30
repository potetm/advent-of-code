(ns advent-2015.day-20
  (:require [advent.util :as util]))


(defn part-1 [n]
  (let [n' (inc (/ n 10))
        ;; pre-calculate all numbers and store in a cache
        cache (reduce (fn [grid i]
                        (reduce (fn [grid j]
                                  (update grid
                                          j
                                          +
                                          (* i 10)))
                                grid
                                (range i n' i)))
                      (vec (repeat n' 0))
                      (next (range n')))]
    (first (keep-indexed (fn [i j]
                           (when (<= n j)
                             i))
                         cache))))


(defn part-2 [n]
  (let [n' (inc (/ n 10))
        ;; pre-calculate all numbers and store in a cache
        cache (reduce (fn [grid i]
                        (reduce (fn [grid j]
                                  (update grid
                                          j
                                          +
                                          (* i 11)))
                                grid
                                (range i
                                       (inc (* i 50))
                                       i)))
                      (vec (repeat (* n' 50) 0))
                      (next (range n')))]
    (first (keep-indexed (fn [i j]
                           (when (<= n j)
                             i))
                         cache))))


(comment

  (time
    (calc 100000000))
  (time
    (part-2 10000000))
  (time
    (part-2 33100000))
  )
