(ns advent-2023.day-4-tschady
  (:require [clojure.string :as str]))


(defn parse [in]
  (str/split-lines in))


(defn num-winners [card]
  (count (re-seq #"(?<=:.*)(?=\b(\d+)\b.*\|.*\b\1\b)"
                 card)))


(defn score [n]
  (int (Math/pow 2
                 (dec n))))

(defn part-1 [input]
  (transduce (comp (map num-winners)
                   (map score))
             +
             input))

(defn totals
  ([xs]
   (reduce + xs))
  ([xs x]
   (conj xs
         (reduce +
                 1
                 (take x xs)))))

(defn part-2 [input]
  (transduce (map num-winners)
               totals
               '()
               (rseq input))
  )


(comment
  (def t1 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

  (part-2 (parse t1)))
