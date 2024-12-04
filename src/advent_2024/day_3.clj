(ns advent-2024.day-3
  (:require
    [advent.util :as util]
    [clojure.string :as str]))


(defn parse [s]
  (concat (map (fn [[whole x y]]
                 (cond
                   (str/starts-with? whole "mul") {:instr :mul
                                                   :args [(parse-long x) (parse-long y)]}
                   (str/starts-with? whole "don't") {:instr :don't
                                                     :args []}
                   (str/starts-with? whole "do") {:instr :do
                                                  :args []}))
               (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)"
                       s))))


(defn part-1 [in]
  (util/sum (comp (filter (fn [{i :instr}]
                            (= i :mul)))
                  (map (fn [{[x y] :args}]
                         (* x y))))
            in))


(defn part-2 [in]
  (:sum (reduce (fn [{e? :enabled?
                      s :sum :as acc}
                     {i :instr
                      [x y] :args}]
                  (case i
                    :mul (cond-> acc
                           e? (assoc :sum (+ s (* x y))))
                    :do (assoc acc :enabled? true)
                    :don't (assoc acc :enabled? false)))
                {:enabled? true
                 :sum 0}
                in)))


(comment
  (def t "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (def t' "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)"
          t')

  (def in "")

  (parse t')
  (part-2 (parse ""))


  )
