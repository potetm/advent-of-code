(ns advent-2022.day-2
  (:require
    [advent.util :as util]
    [clojure.set :as set]))


(defn parse [s]
  (map (fn [[_ a b]]
         [a b])
       (re-seq #"(?m)([ABC]) ([XYZ])"
               s)))


(defn decrypt [[oppo me]]
  {:oppo (get {"A" :rock
               "B" :paper
               "C" :scissors}
              oppo)
   :me (get {"X" :rock
             "Y" :paper
             "Z" :scissors}
            me)})


(def lose-vs
  {:rock :paper
   :paper :scissors
   :scissors :rock})


(def win-vs
  (set/map-invert lose-vs))


(defn score [{oppo :oppo
              me :me}]
  (let [base-score {:rock 1
                    :paper 2
                    :scissors 3}
        win-score {:win 6
                   :draw 3
                   :lose 0}
        res (cond
              (= me oppo) :draw
              (= me (lose-vs oppo)) :win
              :else :lose)]
    (+ (base-score me)
       (win-score res))))


(defn part-1 [in]
  (util/sum (comp (map decrypt)
                  (map score))
            in))


(defn decrypt-2 [[oppo res]]
  {:oppo (get {"A" :rock
               "B" :paper
               "C" :scissors}
              oppo)
   :res (get {"X" :lose
              "Y" :draw
              "Z" :win}
             res)})


(defn my-play [{oppo :oppo
                res :res :as s}]
  (assoc s
    :me (case res
          :win (lose-vs oppo)
          :lose (win-vs oppo)
          :draw oppo)))


(defn part-2 [in]
  (util/sum (comp (map decrypt-2)
                  (map my-play)
                  (map score))
            in))

(comment
  (def t "A Y\nB X\nC Z")
  (def in "B Y\nA Z\nC Z\nA Y\nA Y\nB Y\nC Y\nA Y\nB Y\nB Y\nA Y\nB Z\nB Y\nA Y\nC Y\nB X\nB Y\nB Y\nB Y\nC Y\nB Y\nA Y\nB Y\nA Y\nB Y\nC Y\nA Y\nB X\nB Y\nB Y\nB X\nB Y\nC Y\nB Y\nC Z\nA X\nB Y\nB Y\nA Z\nB X\nC Y\nC Z\nB Y\nB Y\nA Y\nB X\nB Y\nB Y\nB X\nB Y\nC Y\nA Y\nB Y\nC Y\nC X\nB X\nB X\nA Z\nC Y\nB Y\nC Z\nB X\nB Y\nB Y\nB X\nB Y\nB Z\nB Y\nB Y\nB X\nB Y\nB X\nC X\nB Y\nB X\nA Y\nB Y\nB X\nB Y\nA Y\nB X\nB X\nB Y\nA Y\nB Y\nB Y\nB Y\nC Z\nB X\nB Y\nB X\nB X\nB Y\nB X\nC Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nA Y\nB Y\nC Y\nC Z\nA Y\nB Y\nA Y\nC Z\nB Y\nB X\nB Y\nC X\nA Y\nB X\nB Y\nB X\nB X\nB X\nA Y\nC Z\nB Y\nA Y\nB X\nA Y\nB Y\nA Y\nB X\nB Y\nB Y\nB Y\nB X\nB X\nC Z\nB Z\nB Y\nB X\nC Y\nC Y\nC Z\nC X\nC Y\nB Y\nB X\nA Y\nA Y\nB X\nC Z\nB X\nC X\nB X\nB Y\nB Y\nA Z\nB Z\nC Y\nC Y\nB Y\nB Y\nC Y\nC Y\nC Z\nB Y\nB X\nB Y\nB Y\nC Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC Y\nA Z\nB X\nB Y\nC Z\nB Y\nC Y\nB Y\nB X\nC Y\nB Y\nB X\nC Y\nB Y\nB Y\nB Y\nA Y\nB X\nB X\nB Y\nB Y\nB X\nC Z\nB Y\nA Y\nA Y\nB X\nC X\nA Y\nB X\nB X\nB Y\nB Y\nA Y\nC Y\nC Z\nC Y\nB Y\nB Y\nA Y\nA Y\nA Y\nB Y\nB Y\nA X\nB Y\nB Y\nB X\nB X\nC Z\nB X\nB X\nA Y\nA Y\nA Y\nB Y\nB Y\nC Y\nB X\nA Y\nB Y\nC Z\nB X\nA Y\nC Z\nA Y\nB X\nC Y\nB Y\nB Y\nC Y\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nA Y\nB X\nB X\nB Y\nA Y\nB Z\nA Y\nB Y\nC Y\nB Y\nB Y\nB X\nC Z\nB Y\nB Y\nB X\nB X\nC Z\nB Y\nA Y\nB Y\nB X\nC Y\nA Z\nC Z\nB Y\nB Y\nB X\nC Y\nC X\nB X\nC Y\nC Z\nB X\nB Y\nB Y\nC Z\nB X\nC X\nA Z\nA Y\nB X\nC X\nB Y\nA Z\nA Z\nA Y\nB X\nA Y\nB Y\nB Y\nC Z\nB X\nA Z\nB Y\nB X\nA Y\nB Y\nC Y\nB Y\nB X\nB X\nA Z\nB Y\nB Y\nC Z\nB X\nB X\nB Y\nC Z\nB X\nB X\nC Z\nB X\nA Y\nB X\nC Z\nA Y\nC X\nB Y\nB X\nB Y\nB Y\nC Y\nB X\nA Z\nA Y\nB X\nA Z\nB X\nB Y\nB Y\nA Z\nC X\nB Y\nB Y\nB Y\nA Z\nB Y\nB X\nC Y\nC Z\nB X\nB Y\nB Y\nB Y\nC Y\nA Z\nB Y\nC Z\nC X\nB Y\nC Y\nB Y\nB X\nB Y\nB Y\nA Y\nB Y\nB Y\nC Z\nA Z\nB X\nC Y\nB Y\nB Y\nB Y\nA Y\nA Y\nB Y\nA Y\nB Y\nA Y\nB X\nC Y\nB Y\nC X\nA X\nB X\nA Y\nC Y\nB Y\nA X\nB X\nA Z\nB X\nB Y\nB Y\nB X\nB Y\nB Y\nB X\nA Y\nA Z\nC Z\nB Y\nB Y\nB X\nC Z\nC Z\nB Y\nA Y\nB X\nC Z\nA Y\nB X\nB Z\nB Y\nB Y\nB Y\nB X\nB Y\nB X\nB X\nB Y\nC Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC Z\nB X\nB Y\nB Y\nB Y\nB X\nB X\nC Y\nB X\nB X\nC Z\nB Y\nB Y\nB Y\nB Y\nB Y\nC Y\nB Y\nB Y\nB Y\nB X\nB Y\nB X\nB Y\nB Y\nC X\nB Y\nC X\nB X\nC Z\nC X\nB Y\nA Y\nA Z\nA Y\nC Y\nB X\nB Y\nB Y\nC Z\nA Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC Y\nB Y\nC Y\nC X\nB X\nB X\nC Z\nA X\nB X\nB Y\nA Y\nB X\nB Y\nA Z\nB Y\nC Y\nB X\nB Y\nB Y\nC Y\nC Y\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nC Z\nB X\nB Y\nB Y\nB X\nB Y\nB X\nB X\nB Y\nB X\nB Y\nB Y\nC Y\nB X\nB Y\nB Y\nB Y\nA Y\nB Y\nA Z\nB Y\nC Y\nC Y\nB Y\nB X\nA X\nB Y\nA Y\nB X\nB Y\nB Y\nB X\nC Y\nC Z\nB X\nB Y\nA Y\nC Y\nA Z\nC Y\nB Y\nB Y\nB Y\nB X\nC Y\nB Y\nB Y\nA Y\nA X\nB Y\nB X\nB Y\nB Y\nA Y\nB X\nC X\nB Y\nA Y\nC Y\nB Y\nB Y\nA Y\nC Y\nA Y\nC Y\nB Y\nB X\nC Y\nC Y\nB Y\nB Y\nA Y\nB Y\nB Y\nB Y\nC Z\nB X\nB Y\nB X\nB X\nA X\nA Y\nB Y\nB X\nA Z\nB X\nA Y\nB Y\nC Y\nC Y\nC Y\nB Y\nA Y\nA Y\nB Y\nB Y\nB Z\nB Y\nB X\nA Y\nB X\nA Z\nB Y\nB Y\nB X\nB Y\nC Y\nA Z\nB Y\nC X\nC Y\nB Y\nB Y\nC X\nB Y\nA Y\nA Y\nC Z\nB X\nB X\nB X\nB X\nB Y\nC Z\nB X\nC Y\nA Y\nB Y\nA Y\nB Y\nA Y\nB Y\nB Y\nC Y\nB Y\nB Y\nB X\nB Y\nB X\nB Y\nB X\nB Y\nA Z\nC X\nB X\nC X\nB X\nA Y\nC Y\nB X\nC Y\nB X\nB X\nC Z\nC Y\nB X\nB X\nB X\nB Y\nC Y\nB X\nB X\nA Z\nB Y\nB Y\nC Y\nB X\nC Z\nC Y\nA Y\nB X\nC Y\nC X\nB Y\nB Y\nA X\nB Y\nB X\nB Y\nB X\nC Y\nB X\nA Y\nB Y\nB X\nB Y\nB Y\nB Y\nC Y\nC Y\nC Y\nC Y\nB X\nB Y\nA Z\nB X\nB X\nB Y\nB Y\nC Z\nB Y\nB X\nB Y\nB X\nC Y\nB Y\nC Z\nA X\nB X\nB Y\nC Y\nC X\nB X\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nB X\nC Z\nB Y\nB Y\nB X\nB X\nB Y\nB X\nC Z\nA Z\nC X\nC X\nB Y\nB X\nA Y\nB Y\nB X\nC Z\nA X\nB X\nB Y\nB X\nB X\nB Y\nB Y\nA Z\nB X\nA Y\nA Y\nB Y\nB X\nB Y\nC Y\nA Y\nA Y\nB X\nB Z\nC Y\nC Y\nB Y\nC X\nB X\nC Z\nC Z\nB Y\nC X\nA Y\nB Y\nB X\nA Y\nB Y\nC X\nB Y\nB X\nB Y\nC Z\nA X\nA Z\nB Y\nB Y\nB X\nB X\nA Y\nB Y\nB X\nB Y\nC Y\nB Y\nB X\nC Z\nC X\nC Y\nA Y\nB Y\nA Y\nB Y\nB Y\nB X\nB X\nA Y\nA Y\nB Y\nB Y\nB Y\nA Y\nB Y\nB Y\nB Y\nC Z\nA Z\nC Y\nB Y\nB X\nC Y\nA Z\nB X\nB Y\nA Z\nC Y\nB Y\nB X\nB Y\nB Y\nA Y\nB X\nB Z\nB X\nB X\nB X\nB X\nB Y\nB Y\nB X\nB Y\nC Y\nC X\nC Z\nB Y\nB Y\nC Y\nA Y\nB X\nB X\nB Y\nB X\nB Y\nB Y\nA Y\nB Y\nB Y\nB Y\nB Y\nA Y\nB X\nC Z\nB Y\nB Y\nB Y\nB X\nB Y\nC Y\nB X\nB X\nA Y\nA X\nA Y\nC X\nC Y\nB Y\nB Y\nC Y\nB X\nB Y\nB Y\nC Y\nB X\nC X\nB X\nC Y\nB Y\nB X\nB X\nB Y\nC X\nB X\nB Y\nB X\nB X\nB Y\nB X\nB Y\nB Y\nB Y\nC Y\nB Y\nB Y\nC Z\nB Y\nB X\nC Y\nB Y\nB Y\nB X\nB Y\nA Y\nB Y\nA Y\nB Y\nB Y\nA Z\nB X\nB Y\nB Y\nC Z\nB X\nB X\nB Y\nB Y\nB Y\nB X\nC Y\nC Y\nB X\nB Z\nA X\nB Z\nC Y\nB X\nB X\nA X\nB Y\nB X\nC Z\nC Y\nB X\nB Y\nB Y\nB Y\nC Z\nB Y\nC Y\nC Y\nA Y\nB X\nB Y\nB Y\nB Y\nA X\nB Y\nB Y\nB X\nB X\nA Y\nB X\nB Y\nA Z\nB Y\nC Z\nB X\nB Y\nB Y\nA Y\nA Y\nB Y\nB Y\nB Y\nB Y\nC Y\nC Z\nB X\nB X\nB Y\nB Y\nB X\nB X\nA X\nA Y\nB Y\nB Y\nB X\nB Y\nA Y\nB Z\nB X\nC Z\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nC Y\nC X\nB X\nC Z\nB Z\nB X\nB X\nC Z\nC Y\nB X\nA Z\nC Y\nA Y\nB X\nB Y\nC Z\nB Y\nB Y\nB X\nB Y\nC Z\nB Y\nA Y\nB X\nB X\nB X\nC Y\nB X\nB Y\nB X\nB Y\nB Y\nA X\nC Y\nA Y\nC Y\nB X\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nB Y\nB Z\nB Y\nA X\nC Z\nB X\nA Y\nC Z\nB Y\nC X\nA Y\nC Y\nC X\nB Y\nB X\nB X\nC Z\nC Y\nA Z\nC Y\nA Y\nB Z\nA Y\nB X\nC X\nA Z\nC X\nB Y\nC X\nB Y\nA Y\nA X\nB X\nB Y\nB X\nB X\nB X\nA Y\nB Y\nB X\nB Y\nA X\nB Y\nC X\nB Y\nB X\nB Y\nA Y\nB X\nB Y\nB X\nB Y\nB X\nB Y\nB Y\nA Z\nA X\nB Y\nC Z\nB Y\nB Y\nB Y\nB Y\nC Y\nB Y\nA Y\nA Y\nC Z\nC Y\nB Y\nA Y\nB Y\nB X\nA Y\nC Y\nB Y\nB Y\nA Y\nB Y\nA X\nC X\nB Y\nB Y\nC X\nB Y\nB X\nA X\nB Y\nB Y\nB Y\nB X\nB X\nA Z\nB Y\nA X\nB X\nA Z\nB X\nC Z\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nC Y\nB Y\nB X\nA Y\nC Z\nB Y\nB X\nA Y\nC Y\nB X\nB Y\nB Y\nC Z\nA Y\nB X\nB Y\nB X\nB X\nC X\nB Z\nC Y\nB Y\nB Y\nB X\nB Y\nC Y\nC Y\nC Y\nB Y\nA Y\nC Z\nB Y\nC X\nC Y\nB Y\nB Y\nA Z\nB X\nA Y\nA Y\nA Z\nB X\nA Y\nC Z\nB Y\nB Y\nA X\nB X\nB Y\nC Z\nB Y\nB Y\nB X\nB Y\nB X\nB X\nA X\nC Y\nC Y\nC Y\nB Y\nB Y\nC Z\nB Y\nB X\nC X\nB Y\nC Z\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nC Z\nB X\nB Y\nC Y\nB X\nB Z\nB Y\nC Z\nC Y\nB Y\nA Z\nA Y\nB X\nB Y\nC Y\nA X\nA Y\nB X\nB Y\nC Z\nB Y\nC X\nA Y\nC Y\nB Y\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nC X\nA Z\nB X\nC Y\nA Y\nB X\nB Y\nB X\nB Y\nC X\nA Y\nA Y\nC Z\nB Y\nC Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC Y\nA Y\nB Y\nB X\nC Y\nB Y\nB Y\nA Z\nB Y\nB Y\nB X\nB Y\nB Y\nB X\nA Y\nA Z\nB X\nB X\nC Y\nB Y\nB X\nC Y\nB Y\nA X\nB X\nB Y\nA Z\nB Y\nB Y\nB Y\nB Y\nB X\nA X\nB X\nC Y\nB Y\nB Y\nB X\nC Y\nB Y\nB X\nB Y\nB X\nA X\nB Y\nA Y\nB X\nC X\nB Y\nC Z\nB Y\nB Y\nC Y\nB Y\nC X\nB Y\nB X\nA Z\nB Y\nB X\nB Y\nA Y\nB Y\nB X\nB X\nB X\nB Y\nB Y\nB Y\nA Y\nC X\nB X\nB X\nC Z\nB Y\nA Y\nA Z\nA Y\nB Y\nB Y\nB X\nC X\nC Y\nC Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nC Y\nB Y\nB Y\nB X\nB X\nB Y\nB Y\nA Y\nA Y\nA Y\nB X\nB X\nC Y\nB Y\nA Z\nB X\nC Y\nB Y\nB X\nB Y\nB Y\nA Y\nA Y\nB Y\nB Y\nB X\nB X\nA Y\nA X\nA Z\nC X\nA Z\nB X\nB Y\nC Y\nA X\nB X\nC Z\nB Y\nC Y\nA Y\nC X\nB Y\nC X\nA Y\nB Y\nB X\nB Y\nA X\nB X\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nB X\nB Y\nA X\nB Y\nB X\nB Y\nB Y\nB Y\nB X\nB X\nA Y\nB Y\nA Z\nB Y\nB X\nC Z\nB X\nB Y\nB Y\nB Y\nB X\nC Y\nB Y\nB Y\nA Y\nB X\nB X\nB Y\nB Y\nA Y\nB Y\nB X\nB Y\nB X\nB Y\nC X\nC Y\nB X\nB X\nB X\nC Z\nB Y\nB Y\nB X\nB X\nB Y\nA Y\nB Y\nC X\nA Y\nB Y\nB Y\nC X\nB X\nA Y\nC X\nB Y\nB X\nC Z\nB X\nB Y\nA Y\nB Y\nB X\nC Y\nA Y\nB Y\nA X\nB Y\nC Y\nC X\nB Y\nB X\nA Z\nB Y\nB Y\nB X\nB Y\nB Y\nB X\nB Y\nC Y\nB X\nB Y\nB X\nB X\nC Y\nB X\nB X\nA Y\nB X\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC X\nB X\nB X\nA Y\nB Y\nA Y\nB X\nB Y\nA Y\nB Y\nA Z\nB X\nA Y\nC Z\nA Y\nA Z\nB X\nB Y\nA Y\nB X\nB X\nB Y\nB Y\nA Z\nA Y\nB X\nC Y\nB Y\nB Z\nC X\nC X\nB X\nA Y\nB X\nB Y\nB Y\nA X\nB Y\nA X\nB Y\nA Y\nB X\nB X\nA Z\nA Y\nA Y\nC Z\nB Y\nB Y\nC Z\nB Y\nB X\nC Y\nB Y\nB X\nA Y\nB Y\nB X\nC X\nC X\nB Y\nA Y\nB Y\nB Y\nB Y\nB X\nA Y\nB Y\nB Y\nA Y\nB Y\nB X\nB X\nC Y\nC X\nB Y\nC X\nB Y\nC Y\nB X\nB Y\nB X\nB Y\nB Y\nB X\nB Y\nB Y\nC Y\nB X\nB Y\nB X\nB Y\nA Y\nB X\nA Y\nB X\nB Y\nB X\nC X\nC Y\nB Y\nB X\nB Y\nB Y\nB X\nB Y\nB Y\nA Y\nB Y\nB X\nB Y\nB X\nC Y\nA Y\nB Y\nB X\nB Z\nB Y\nB X\nC Y\nB Y\nC Y\nC Y\nB X\nC Z\nB X\nC Y\nB Y\nA Y\nB Y\nA X\nB Y\nB Y\nB Y\nB Y\nA Z\nB Y\nB Y\nA Y\nC Y\nB Y\nB X\nB Y\nB X\nB Y\nB Z\nB X\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nB X\nB Y\nB Y\nA Y\nC Z\nB Y\nB Y\nB X\nB X\nB X\nB Y\nA Y\nA Y\nB X\nB Y\nB X\nA X\nB Y\nB Y\nC Y\nA Y\nC Z\nA Z\nA Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC X\nB Y\nA Y\nC X\nC Y\nB X\nA Z\nC Z\nA Z\nC Y\nB X\nB Y\nB Y\nB X\nB Y\nA Z\nC Z\nC X\nB X\nB Z\nA Y\nC Y\nC Y\nC Y\nA Y\nC X\nB X\nC Y\nC Y\nB Y\nB Y\nA Y\nB Y\nA Y\nA Y\nB X\nB X\nC Y\nB Y\nC Y\nB Y\nB Z\nB Y\nA Y\nB X\nB Y\nA X\nB X\nB Y\nB Y\nC Y\nB Y\nC Z\nB Z\nA Y\nA Y\nA Y\nB Y\nB X\nB Y\nA Y\nC X\nB X\nB Y\nB X\nC X\nB Y\nB X\nB Y\nB Y\nB X\nA Y\nA Y\nB Y\nB Y\nB Y\nA Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nB X\nB X\nA X\nC Y\nC X\nC Z\nC Y\nB X\nB Y\nA Z\nB Y\nC Y\nB Y\nB X\nB Y\nA Y\nB X\nB X\nB X\nC Y\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC X\nB Y\nA X\nB X\nC Z\nB X\nB Y\nB X\nB Y\nC Y\nB Y\nB Y\nB X\nA Z\nB Y\nA X\nB Y\nC Z\nC Y\nB Y\nB Y\nB Y\nC X\nB Y\nB Y\nB X\nA Y\nC Z\nB Y\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nB Y\nC X\nB X\nB X\nC Z\nB Y\nB Y\nB X\nA Z\nB Y\nB Y\nC Y\nC Y\nA Y\nC Y\nA Y\nB Y\nB X\nB Y\nB X\nC Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC X\nB X\nB X\nB Y\nC X\nC Z\nB X\nA Y\nC Z\nB X\nB X\nC Y\nC Y\nA Y\nB Y\nB X\nB Y\nB Y\nA Z\nA Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nB Y\nC Y\nB Y\nB X\nA X\nB Y\nA Y\nA Y\nB Y\nB Y\nB X\nB Y\nB Y\nC Y\nA Y\nB X\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nB X\nB X\nA Y\nB X\nB X\nB X\nB X\nB Y\nB X\nA X\nC Y\nB Y\nB Y\nA Y\nB Y\nB X\nC X\nC Y\nA Y\nB Y\nA Z\nC Y\nB Y\nB Y\nB Y\nB X\nB X\nB Y\nC X\nB X\nB Y\nC X\nB X\nA Y\nB Y\nB X\nB Y\nB Y\nB X\nB Y\nB Y\nC Y\nB X\nB X\nB X\nA Y\nB Y\nB X\nC Y\nC Y\nB X\nA Y\nB Y\nC Y\nB X\nB Y\nB X\nB X\nB Y\nC Y\nA Y\nC Y\nB X\nC X\nB Y\nB X\nA Y\nC Y\nB Y\nB Y\nB Y\nC Z\nC Y\nA Z\nA Y\nB Y\nB Y\nB Y\nB Y\nA Y\nB X\nB Y\nB Y\nC Y\nC X\nB Y\nB Y\nB Y\nB Y\nB X\nB X\nB X\nC Z\nB Y\nB Y\nC Y\nB X\nB Y\nB Y\nA Z\nB Y\nC Z\nB X\nA Y\nB X\nB Y\nA Y\nA Z\nA Z\nB Z\nB Y\nB Y\nA Y\nC Z\nB Y\nC Z\nB X\nB Y\nB Y\nB Y\nB Y\nC Y\nB X\nB X\nB X\nB X\nB X\nB Y\nA Z\nA Y\nB Y\nB Y\nB Y\nC X\nB Y\nA Y\nB Y\nB Y\nB X\nB Y\nC Y\nA Z\nA Y\nC Y\nC Z\nB X\nA Z\nB X\nB X\nB Z\nB Y\nC Y\nA Y\nB Y\nB Y\nB X\nC Z\nB Y\nB Y\nA Z\nA X\nB Y\nB X\nB Y\nB X\nB X\nB X\nB X\nA Y\nB Y\nA X\nB X\nB X\nB Y\nB X\nB Y\nB Y\nA Y\nB Y\nC Y\nA Z\nA Y\nB X\nB Y\nA Y\nC X\nB Y\nC Y\nB X\nB Y\nA Y\nB Y\nA Z\nB Y\nB Y\nA Y\nB X\nC Y\nB Y\nB Y\nC Z\nB X\nB Z\nB Y\nA Y\nB Y\nC X\nB Y\nB Y\nB Y\nB Y\nB Y\nC Z\nC Z\nB Y\nC Y\nC Z\nA X\nB Y\nA Y\nB X\nB X\nB Z\nB X\nA Y\nB Y\nB Y\nB Y\nA Z\nC Y\nB Y\nB Y\nC Y\nC Z\nC X\nB Y\nA X\nB Y\nA Y\nC X\nB Y\nB Y\nB Y\nB Z\nB X\nB X\nC Y\nB X\nB X\nC Z\nB Y\nB X\nB X\nB Y\nB Y\nB X\nC X\nB Y\nB Y\nB X\nA Y\nB Y\nC Y\nB Y\nA X\nB Y\nA Y\nA Y\nB Y\nB X\nB X\nC X\nB Y\nB X\nA Y\nC X\nA Y\nB X\nB Y\nB Y\nC Z\nB X\nB Y\nB X\nB Y\nB Y\nA Z\nB Z\nB Y\nB Y\nC Y\nC X\nC Y\nC Y\nB Y\nB Y\nB Y\nC Y\nA Z\nA Z\nB Y\nB X\nA Y\nB X\nB Y\nA Y\nB X\nB Y\nB X\nC X\nB Y\nB Y\nB X\nB X\nA Z\nA Y\nB Y\nA Y\nB Y\nB X\nB X\nA Y\nB X\nB Y\nB Y\nB X\nC X\nC X\nB Y\nB Y\nA Y\nB X\nB Y\nB Y\nB Y\nB Y\nC Y\nB Y\nC Z\nB Y\nB Y\nC X\nB X\nB Y\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nB X\nA Z\nB Y\nB X\nC Z\nB X\nB Y\nA Z\nC Z\nA Z\nB X\nB Y\nB Y\nB Y\nB X\nB Y\nC Y\nC X\nC Y\nA X\nB Y\nB Z\nB Y\nB Y\nB Y\nA Y\nB X\nB Y\nC Y\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nC Z\nB Y\nB X\nB Z\nB X\nB Y\nB Y\nB X\nB X\nA Y\nB X\nB X\nB X\nA Y\nB X\nB X\nB Y\nB X\nA X\nB Y\nB Y\nC Y\nB X\nB Y\nB X\nB X\nC Y\nB Y\nB Y\nB X\nA X\nC Y\nC X\nA Y\nB X\nB Y\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nB Y\nB Y\nC Y\nC Z\nB Y\nC X\nB X\nB Y\nA Z\nC X\nB X\nB Y\nB Y\nB X\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nC Y\nB Y\nC Y\nB X\nA Z\nB Y\nB X\nA Y\nB Y\nB X\nA Y\nB X\nB X\nB X\nB Y\nB Y\nB X\nB Y\nA Z\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB Y\nB X\nB Y\nB Y\nA Y\nB Y\nB X\nB X\nC Y\nC X\nB Y\nA Y\nC Z\nB X\nB X\nB Y\nC Y\nA Y\nB Y\nB Y\nB X\nB Y\nB X\nC Y\nC Y\nB Y\nB X\nB X\nB Y\nC Y\nB Y\nC Y\nB Y\nB Y\nC Y\nA Y\nB Y\nA Z\nC X\nB Y\nB Y\nB X\nB X\nC Z\nB Y\nB Y\nB Y\nB X\nB X\nA X\nB X\nB Z\nB Y\nB X\nB X\nB Y\nB X\nB X\nB Y\nB Y\nB Y\nB Y\nA Y\nB Y\nB X\nB Z\nB Y\nA X\nA X\nA Y\nB Y\nB Y\nC Y\nA Y\nC Y\nB Y\nB X\nB Y\nB Y\nB X\nB Y\nA Y\nB Y\nB Y\nB X\nB Y\nA Y\nB X\nB Y")
  (part-2 (parse in))

  (def beats {:rock :scissors
              :paper :rock
              :scissors :paper})
  (beats :rock))
