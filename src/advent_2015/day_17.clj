(ns advent-2015.day-17
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [advent.util :as util]))


(defn parse [in]
  (map #(Long/parseLong %)
       (str/split-lines in)))


(defn combos [in]
  ;; Use binary integer math to generate all of the
  ;; combinations of indices to keep.
  (let [c (count in)
        fmt (str "%" c "s")]
    (map (fn [i]
           (keep (fn [[v keep?]]
                   (when (= keep? \0)
                     v))
                 (map vector
                      in
                      (str/replace (format fmt (Long/toBinaryString i))
                                   " "
                                   "0"))))
         (range 0 (Math/pow 2 c)))))


(defn part-1 [in n]
  (count (filter (fn [c]
                   (= n (util/sum c)))
                 (combos in))))


(defn part-2 [in n]
  (let [res (filter (fn [c]
                      (= n (util/sum c)))
                    (combos in))
        m (util/min (map count res))]
    (count (filter (fn [c]
                     (= (count c) m))
                   res))))

(comment
  (def tin "20\n15\n10\n5\n5\n")
  (def in "43\n3\n4\n10\n21\n44\n4\n6\n47\n41\n34\n17\n17\n44\n36\n31\n46\n9\n27\n38")

  (util/min-by count (combos (parse tin)))
  (combo/permuted-combinations (parse tin)
                               4)
  (part-2 (parse tin)
          25)

  (part-2 (parse in)
          150)

  )
