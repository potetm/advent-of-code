(ns advent-2024.day-7
  (:require
    [advent.util :as util]
    [clojure.edn :as edn]
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]))


(defn parse [s]
  (into []
        (map (fn [l]
               (let [[tv & nums] (edn/read-string (str "[" (str/replace l #":" "") "]"))]
                 {:test-value tv
                  :nums (vec nums)})))
        (str/split-lines s)))


(defn valid? [{tv :test-value
               nums :nums}]
  (boolean (some (fn [n]
                   (= tv n))
                 (map (fn [[f & r] s]
                        (reduce (fn [acc [op n]]
                                  (op acc n))
                                f
                                (map vector
                                     s
                                     r)))
                      (repeat nums)
                      (combo/selections [+ *]
                                        (dec (count nums)))))))


(defn part-1 [in]
  (util/sum (comp (filter valid?)
                  (map :test-value))
            in))


(defn valid?* [{tv :test-value
                nums :nums}]
  (boolean (some (fn [n]
                   (= tv n))
                 (map (fn [[f & r] s]
                        (reduce (fn [acc [op n]]
                                  (op acc n))
                                f
                                (map vector
                                     s
                                     r)))
                      (repeat nums)
                      (combo/selections [+ * (fn [a b]
                                               (parse-long (str a b)))]
                                        (dec (count nums)))))))


(defn part-2 [in]
  (util/sum (comp (filter valid?*)
                  (map :test-value))
            in))


(comment
  (def t "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20")
  (def in "")


  (part-2 (parse t))
  (part-2 (parse in))

  )

;; Wall of fame solutions?
