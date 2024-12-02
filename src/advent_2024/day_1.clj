(ns advent-2024.day-1
  (:require [advent.util :as util]
            [clojure.string :as str]))


(defn parse [s]
  (into []
        (map (fn [l]
               (let [[_ g1 g2] (re-find #"(\d+)\s+(\d+)"
                                        l)]
                 {:group1 (parse-long g1)
                  :group2 (parse-long g2)})))
        (str/split-lines s)))


(defn part-1 [in]
  (util/sum (map (fn [g1 g2]
                   (abs (- g1 g2)))
                 (sort (map :group1 in))
                 (sort (map :group2 in)))))


(defn part-2 [in]
  (let [r (frequencies (map :group2 in))]
    (util/sum (map (fn [{g1 :group1}]
                     (* g1 (get r g1 0)))
                   in))))


(comment
  (def t "3   4\n4   3\n2   5\n1   3\n3   9\n3   3")

  (part-1 (parse t))

  (part-2 (parse t))


  (part-2 (parse t))

  )
