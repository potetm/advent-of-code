(ns advent-2022.day-9
  (:require
    [advent.util :as util]
    [clojure.string :as str]))


(defn parse [s]
  (mapv (fn [l]
          (let [[_ [dir] n] (re-find #"(\w)\s(\d+)" l)]
            [dir (parse-long n)]))
        (str/split-lines s)))


(defn expand-cmds [cmds]
  (mapcat (fn [[dir n]]
            (repeat n dir))
          cmds))


(defn touching? [a b]
  (< (util/dist a b)
     2))


(defn mv1 [a b]
  (let [∆ (- b a)]
    (+ a (/ ∆ (abs ∆)))))


(defn step [[h & r] dir]
  (let [mv (get {\R [1 0]
                 \L [-1 0]
                 \U [0 1]
                 \D [0 -1]}
                dir)
        h' (mapv (fn [i di]
                   (+ i di))
                 h
                 mv)]
    (reduce (fn [rope [x1 y1 :as a]]
              (let [[x2 y2 :as b] (peek rope)]
                (conj rope
                      (cond
                        (touching? a b) a
                        (= x1 x2) [x1 (mv1 y1 y2)]
                        (= y1 y2) [(mv1 x1 x2) y1]
                        :else [(mv1 x1 x2)
                               (mv1 y1 y2)]))))
            [h']
            r)))


(defn init [l]
  (vec (repeat l [0 0])))


(defn run [rope in]
  (count (into #{}
               (map peek)
               (reductions step
                           rope
                           (expand-cmds in)))))


(defn part-1 [in]
  (run (init 2)
       in))

(defn part-2 [in]
  (run (init 10)
       in))


(comment
  (def t "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")
  (def t' "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")
  (def in "L 1\nR 1\nL 1\nU 1\nR 2\nU 1\nD 2\nR 2\nU 1\nD 2\nU 2\nL 2\nD 1\nU 1\nD 2\nL 2\nD 1\nL 2\nD 2\nL 1\nU 2\nR 2\nL 1\nD 2\nL 2\nR 2\nD 2\nL 1\nU 1\nR 1\nU 1\nL 1\nD 1\nR 1\nU 2\nD 2\nR 1\nU 1\nR 1\nL 1\nU 1\nR 1\nD 1\nL 1\nU 1\nR 1\nD 1\nU 1\nD 1\nU 1\nR 2\nL 2\nD 2\nL 2\nR 1\nU 2\nL 2\nR 1\nU 1\nR 2\nD 2\nR 2\nL 2\nU 2\nR 2\nD 2\nR 1\nU 1\nL 2\nR 1\nU 1\nD 2\nU 2\nD 1\nR 2\nL 2\nD 2\nL 2\nR 2\nU 1\nR 2\nL 2\nD 2\nL 1\nD 1\nR 2\nL 1\nU 2\nR 1\nU 2\nD 1\nU 2\nD 1\nL 1\nR 2\nD 2\nR 2\nU 2\nR 1\nU 2\nR 2\nU 1\nR 1\nD 2\nU 2\nR 2\nU 1\nR 2\nD 2\nR 1\nL 1\nU 3\nL 2\nU 1\nL 2\nU 1\nD 1\nU 2\nD 1\nU 2\nR 2\nL 1\nU 3\nR 1\nU 1\nR 3\nL 3\nD 3\nL 2\nU 1\nD 3\nU 1\nR 1\nL 1\nR 2\nL 1\nR 3\nL 3\nD 1\nR 2\nL 3\nU 3\nD 1\nL 2\nU 2\nD 2\nL 2\nU 1\nR 1\nL 2\nR 3\nL 3\nD 3\nR 2\nD 3\nU 1\nL 3\nR 1\nU 2\nL 1\nD 3\nL 1\nU 1\nD 3\nL 3\nU 2\nD 1\nU 2\nL 3\nR 1\nU 2\nD 2\nR 3\nD 3\nU 2\nD 3\nL 3\nR 3\nL 1\nU 3\nL 1\nD 1\nL 2\nD 2\nR 1\nL 3\nR 2\nL 3\nR 3\nL 1\nD 1\nU 3\nL 2\nR 2\nL 2\nD 3\nU 3\nL 3\nU 3\nD 1\nU 2\nR 2\nU 1\nD 3\nL 3\nR 1\nL 3\nU 3\nD 1\nL 3\nD 2\nR 1\nU 1\nR 1\nD 2\nL 2\nU 1\nL 2\nR 2\nL 2\nD 3\nU 3\nD 4\nL 3\nD 4\nR 3\nL 2\nU 3\nR 3\nU 1\nR 4\nU 2\nD 1\nR 3\nL 1\nR 1\nD 3\nR 2\nD 1\nR 2\nL 2\nU 2\nD 1\nR 1\nU 2\nR 3\nL 2\nD 1\nU 3\nR 2\nD 4\nL 3\nU 1\nR 1\nD 2\nU 1\nR 3\nL 4\nD 2\nL 1\nU 3\nL 2\nU 3\nR 1\nD 1\nL 2\nR 4\nD 4\nU 1\nR 3\nU 3\nR 1\nU 3\nL 2\nU 3\nL 2\nD 4\nU 1\nR 1\nL 2\nR 2\nD 2\nL 2\nD 2\nU 4\nD 4\nL 2\nU 2\nD 3\nL 4\nU 4\nD 4\nR 1\nD 4\nR 1\nD 1\nL 1\nR 1\nL 1\nD 2\nL 4\nD 2\nR 3\nL 1\nD 2\nU 3\nL 2\nU 3\nR 4\nD 4\nU 1\nL 1\nU 4\nL 4\nU 3\nD 2\nR 3\nL 1\nD 3\nU 3\nR 1\nL 3\nU 1\nD 1\nR 1\nU 3\nR 2\nL 1\nD 1\nL 3\nU 1\nD 1\nL 2\nR 3\nU 4\nL 5\nD 5\nR 5\nD 3\nU 3\nD 1\nL 5\nR 2\nU 1\nL 2\nD 1\nL 1\nD 4\nR 1\nU 1\nL 1\nU 1\nD 3\nL 4\nD 1\nU 5\nL 1\nR 5\nL 2\nR 4\nD 5\nU 2\nD 3\nU 4\nL 1\nR 2\nU 4\nL 4\nD 3\nR 1\nU 1\nR 3\nD 3\nU 5\nL 3\nD 3\nU 5\nR 4\nL 1\nD 4\nU 1\nL 1\nR 4\nD 5\nL 5\nD 3\nR 5\nU 3\nR 2\nD 2\nU 2\nD 5\nR 5\nD 5\nR 2\nD 5\nU 2\nD 2\nR 1\nL 5\nD 2\nU 2\nR 5\nU 1\nD 5\nU 5\nL 2\nD 5\nL 1\nR 4\nU 4\nD 1\nL 5\nU 4\nL 1\nR 3\nL 3\nR 3\nL 3\nD 3\nR 4\nU 5\nL 2\nD 3\nU 2\nL 1\nR 5\nU 1\nL 3\nU 4\nL 4\nD 4\nR 1\nD 4\nR 5\nD 5\nU 5\nR 5\nL 3\nD 3\nU 2\nR 5\nU 5\nR 2\nD 5\nR 1\nU 2\nD 5\nL 3\nR 5\nU 5\nR 4\nD 6\nL 4\nU 4\nL 6\nR 2\nL 4\nU 5\nL 1\nU 6\nD 4\nR 3\nU 2\nR 1\nD 5\nL 4\nU 2\nR 6\nU 1\nL 2\nD 6\nU 1\nR 2\nL 3\nR 2\nL 5\nU 1\nR 3\nD 3\nL 4\nR 5\nU 1\nD 3\nR 6\nD 5\nR 4\nD 3\nU 2\nD 4\nR 5\nU 4\nR 2\nL 2\nU 5\nD 5\nU 6\nD 4\nU 5\nL 5\nR 1\nD 3\nR 3\nL 3\nD 3\nL 3\nU 6\nR 5\nD 6\nU 5\nL 1\nR 4\nL 1\nU 4\nL 5\nD 5\nU 4\nD 3\nR 3\nL 4\nR 4\nU 6\nL 5\nD 2\nU 3\nR 4\nU 5\nL 5\nD 6\nR 5\nD 5\nL 3\nU 6\nL 3\nU 5\nD 2\nU 4\nR 4\nU 3\nD 1\nR 2\nL 6\nR 2\nL 3\nU 5\nL 1\nR 4\nD 2\nU 5\nD 2\nR 4\nU 6\nR 6\nL 6\nU 6\nL 4\nR 5\nD 3\nU 1\nD 6\nL 1\nD 3\nL 4\nD 6\nU 3\nD 2\nL 2\nU 5\nD 2\nU 5\nD 6\nR 4\nD 1\nU 1\nD 5\nU 1\nL 7\nU 3\nR 3\nU 5\nD 2\nR 7\nD 6\nL 7\nR 6\nL 3\nR 3\nU 1\nL 4\nD 4\nL 6\nR 1\nU 4\nL 3\nR 3\nD 3\nL 2\nD 7\nU 7\nD 7\nR 2\nL 6\nD 2\nR 6\nD 6\nU 5\nL 4\nU 3\nD 6\nU 3\nD 1\nL 7\nU 7\nL 4\nR 3\nD 6\nU 2\nL 6\nD 3\nU 7\nD 2\nR 7\nD 4\nL 4\nR 4\nU 2\nD 3\nU 2\nL 4\nR 4\nL 4\nD 6\nR 4\nU 1\nR 7\nD 7\nU 7\nR 6\nD 4\nU 2\nD 4\nL 5\nU 4\nD 6\nR 4\nU 2\nR 1\nU 4\nD 5\nR 1\nD 7\nR 4\nU 1\nD 4\nU 6\nL 6\nR 1\nD 1\nR 1\nD 6\nR 6\nD 3\nU 1\nD 5\nR 4\nL 1\nD 6\nR 6\nL 8\nU 1\nD 7\nR 4\nL 2\nR 6\nU 7\nL 2\nD 5\nR 6\nD 3\nL 4\nR 5\nU 7\nL 3\nU 8\nL 1\nU 6\nL 8\nR 7\nD 2\nU 3\nR 4\nL 8\nD 4\nL 1\nU 1\nD 7\nU 8\nR 7\nD 2\nL 5\nU 6\nD 7\nR 4\nL 1\nR 3\nU 1\nR 2\nU 1\nD 7\nR 6\nU 7\nD 6\nL 2\nD 8\nU 4\nR 8\nL 5\nD 6\nL 7\nU 7\nR 8\nL 5\nD 5\nR 4\nL 5\nU 8\nD 4\nU 6\nD 2\nU 5\nR 7\nL 8\nU 7\nR 3\nL 8\nR 6\nD 4\nR 2\nD 6\nR 5\nD 2\nU 6\nL 4\nU 8\nR 3\nU 1\nL 6\nR 1\nL 3\nR 1\nU 3\nL 8\nD 5\nU 6\nL 8\nD 6\nR 2\nU 6\nR 4\nL 8\nD 1\nR 6\nD 8\nU 6\nD 6\nL 3\nU 4\nD 2\nL 2\nD 7\nL 5\nU 3\nR 8\nD 6\nR 5\nL 8\nU 5\nL 9\nR 7\nD 5\nR 2\nU 5\nL 3\nD 9\nR 4\nU 1\nL 3\nR 5\nL 7\nR 8\nD 1\nU 4\nL 4\nR 3\nU 8\nL 8\nD 8\nU 9\nR 6\nU 3\nR 2\nU 8\nD 9\nR 3\nL 2\nU 9\nR 9\nL 8\nD 2\nR 1\nL 8\nU 7\nR 3\nL 1\nR 9\nL 4\nR 2\nU 3\nD 8\nR 5\nU 9\nL 9\nR 1\nD 5\nR 5\nU 4\nL 9\nR 6\nD 5\nL 2\nR 6\nU 1\nD 5\nL 3\nR 1\nU 5\nR 4\nD 4\nU 8\nD 2\nU 6\nR 6\nU 5\nR 3\nU 5\nR 5\nL 8\nD 9\nL 7\nR 1\nU 7\nR 5\nU 6\nR 8\nU 2\nR 2\nU 8\nR 7\nD 3\nR 6\nL 8\nR 4\nL 8\nR 8\nU 8\nR 3\nL 3\nU 1\nL 9\nD 9\nR 9\nU 4\nR 1\nL 3\nD 4\nU 5\nR 7\nD 6\nL 9\nU 7\nL 4\nR 8\nU 8\nD 4\nL 5\nD 9\nU 2\nD 9\nR 4\nD 8\nR 8\nD 8\nU 9\nL 6\nR 6\nL 2\nU 9\nL 4\nU 10\nL 1\nR 10\nD 3\nU 6\nR 5\nD 3\nR 4\nL 4\nU 7\nD 9\nR 2\nD 10\nU 9\nL 7\nD 4\nR 2\nL 10\nD 5\nR 5\nU 9\nR 4\nL 7\nR 5\nD 3\nR 6\nD 8\nR 6\nD 10\nR 7\nU 4\nD 5\nU 6\nL 3\nD 3\nU 10\nR 10\nU 10\nD 9\nR 6\nD 2\nR 9\nD 8\nR 7\nU 10\nR 4\nU 6\nR 7\nU 2\nR 5\nU 6\nL 4\nU 8\nL 6\nD 8\nR 2\nU 2\nL 4\nU 4\nR 5\nU 10\nD 8\nU 7\nR 5\nD 3\nL 2\nR 9\nU 10\nL 1\nU 5\nL 1\nD 3\nL 9\nR 9\nU 2\nD 3\nU 8\nR 10\nL 2\nU 9\nD 4\nU 4\nD 10\nL 4\nD 3\nR 3\nL 2\nU 8\nL 4\nD 6\nR 2\nL 3\nD 6\nL 4\nD 5\nL 7\nU 9\nL 3\nD 4\nR 6\nL 10\nU 5\nL 3\nR 3\nD 6\nU 1\nL 3\nR 5\nD 8\nR 11\nU 2\nL 11\nR 7\nU 8\nR 4\nD 4\nU 6\nD 4\nU 9\nD 5\nR 9\nD 9\nU 6\nD 1\nU 3\nL 4\nU 3\nD 11\nU 11\nR 4\nL 3\nU 5\nD 1\nR 4\nU 6\nD 4\nU 3\nD 8\nL 7\nR 8\nL 9\nR 3\nD 4\nU 6\nL 2\nU 6\nR 3\nD 8\nR 1\nL 5\nU 8\nD 3\nR 8\nL 8\nU 3\nR 2\nU 11\nL 9\nU 4\nD 10\nL 7\nD 1\nU 2\nD 7\nR 4\nL 2\nU 8\nL 5\nU 5\nD 1\nL 11\nR 4\nL 7\nU 2\nD 9\nL 4\nU 11\nR 8\nL 1\nD 10\nU 5\nR 4\nL 8\nU 8\nD 11\nU 8\nD 3\nR 4\nU 8\nR 2\nU 3\nR 9\nD 4\nL 2\nD 4\nL 4\nU 4\nR 5\nU 4\nL 10\nU 4\nL 4\nR 8\nD 5\nL 2\nU 5\nD 9\nU 10\nD 6\nR 9\nD 4\nR 10\nU 4\nD 12\nR 7\nL 12\nR 8\nL 11\nU 1\nD 4\nL 1\nD 7\nR 1\nL 5\nR 9\nL 3\nD 7\nL 2\nR 6\nU 2\nD 2\nU 9\nL 4\nU 11\nD 4\nR 7\nD 7\nL 11\nU 11\nL 1\nD 9\nU 6\nR 3\nD 3\nL 9\nR 7\nL 6\nD 6\nR 11\nD 10\nU 11\nR 1\nU 2\nR 9\nL 6\nU 7\nD 5\nU 7\nR 4\nU 12\nR 2\nD 1\nL 7\nR 5\nD 6\nR 1\nD 11\nR 9\nL 6\nU 11\nD 7\nL 7\nR 9\nL 7\nD 12\nU 7\nD 12\nR 3\nL 2\nR 10\nD 1\nL 9\nR 3\nD 6\nR 11\nU 2\nL 2\nU 3\nR 3\nL 6\nU 3\nR 1\nL 5\nR 7\nU 2\nD 8\nL 9\nU 7\nL 9\nD 12\nU 3\nD 9\nU 1\nL 10\nD 9\nL 11\nU 6\nR 9\nU 10\nL 5\nR 11\nU 3\nR 9\nD 12\nU 1\nD 11\nR 12\nU 1\nD 7\nL 1\nU 7\nL 5\nU 8\nL 2\nR 6\nU 13\nR 3\nD 8\nU 2\nL 5\nU 7\nL 7\nU 13\nL 3\nR 5\nD 1\nL 9\nU 2\nL 7\nR 9\nU 12\nR 9\nL 13\nR 1\nU 11\nR 4\nU 6\nD 6\nR 13\nD 13\nU 5\nL 9\nU 2\nD 9\nR 12\nD 13\nU 9\nD 10\nU 11\nR 11\nU 12\nD 5\nL 1\nR 2\nU 2\nD 5\nU 1\nD 8\nR 8\nL 1\nU 9\nL 11\nD 10\nR 1\nD 6\nU 5\nD 11\nL 13\nD 2\nR 1\nL 1\nR 7\nL 1\nU 2\nD 6\nU 4\nL 7\nU 8\nD 7\nL 11\nU 5\nD 5\nR 1\nD 6\nR 1\nU 9\nL 11\nD 6\nU 10\nD 10\nR 2\nU 13\nL 5\nU 9\nL 13\nU 7\nL 3\nD 9\nL 13\nR 5\nL 5\nU 5\nL 13\nU 10\nL 1\nR 10\nL 8\nR 13\nU 13\nR 7\nD 6\nR 4\nL 5\nU 3\nD 6\nR 13\nU 9\nL 10\nU 13\nR 3\nD 8\nL 8\nU 5\nL 9\nU 4\nD 11\nL 14\nU 11\nR 1\nD 7\nU 4\nR 11\nU 3\nR 2\nU 11\nL 6\nR 2\nD 5\nL 1\nU 1\nL 11\nR 5\nU 10\nD 12\nR 3\nL 6\nR 3\nD 9\nL 4\nD 11\nL 11\nD 4\nR 10\nD 3\nL 12\nD 8\nU 7\nR 2\nD 1\nL 5\nU 11\nL 9\nR 4\nU 4\nD 11\nR 9\nD 3\nU 6\nL 4\nU 4\nL 8\nU 6\nL 14\nR 13\nD 5\nR 5\nU 5\nR 2\nU 8\nL 12\nR 2\nU 12\nD 4\nR 3\nL 5\nD 11\nR 7\nU 9\nD 10\nL 1\nD 4\nU 4\nD 1\nR 9\nL 10\nR 11\nL 8\nD 10\nR 10\nL 3\nR 1\nL 9\nD 4\nL 8\nR 7\nU 12\nR 5\nL 10\nR 7\nU 6\nR 7\nU 8\nD 1\nU 1\nL 1\nR 2\nD 14\nU 4\nD 5\nR 2\nD 1\nR 5\nD 6\nR 3\nL 12\nU 11\nD 1\nL 7\nR 10\nU 2\nR 6\nU 11\nR 12\nU 2\nR 4\nL 1\nD 7\nU 2\nD 9\nU 12\nD 11\nU 2\nL 4\nR 10\nU 13\nR 2\nL 8\nR 13\nL 11\nR 13\nL 13\nR 5\nD 2\nU 10\nD 14\nL 8\nU 11\nR 1\nL 15\nU 13\nL 4\nD 9\nL 1\nU 14\nR 10\nD 2\nR 15\nL 12\nR 15\nU 15\nL 3\nU 1\nL 1\nR 9\nD 11\nL 9\nD 3\nU 12\nR 1\nD 7\nL 8\nR 10\nD 2\nL 4\nD 2\nU 12\nD 12\nL 15\nU 3\nD 13\nU 13\nD 3\nR 4\nL 7\nD 6\nU 7\nD 7\nR 2\nD 2\nR 10\nL 12\nU 14\nD 1\nU 5\nL 8\nR 1\nU 5\nD 8\nU 1\nD 15\nU 13\nL 15\nR 1\nL 10\nU 14\nD 15\nR 10\nL 13\nD 8\nL 7\nU 11\nR 7\nU 10\nD 15\nL 1\nU 5\nL 2\nR 6\nL 4\nU 15\nR 13\nU 4\nR 7\nU 4\nR 10\nL 3\nU 10\nR 10\nD 13\nR 11\nL 12\nD 12\nU 7\nR 8\nD 14\nL 4\nR 4\nL 8\nD 6\nR 8\nL 5\nU 6\nL 7\nU 14\nD 14\nR 14\nL 9\nD 5\nU 14\nR 16\nL 4\nR 14\nU 3\nD 10\nL 5\nR 9\nD 4\nR 14\nL 16\nD 13\nL 4\nR 13\nD 5\nR 5\nU 1\nR 6\nL 15\nD 16\nR 15\nD 11\nR 11\nD 6\nU 7\nL 10\nD 7\nU 12\nR 6\nL 2\nU 7\nL 8\nU 14\nL 5\nU 6\nL 10\nR 16\nL 13\nD 13\nU 7\nR 9\nL 8\nU 3\nD 6\nR 2\nL 4\nR 15\nD 12\nU 7\nD 4\nU 2\nL 10\nD 4\nL 11\nD 12\nR 5\nL 13\nU 16\nL 12\nR 3\nL 16\nR 14\nL 8\nR 7\nU 7\nL 7\nD 12\nR 2\nD 5\nR 14\nU 9\nD 15\nU 2\nL 10\nD 1\nU 13\nL 2\nR 13\nD 5\nU 1\nD 2\nL 7\nD 14\nU 12\nD 8\nU 10\nL 3\nR 1\nU 14\nR 8\nL 12\nR 8\nU 2\nR 1\nD 14\nU 5\nD 11\nR 16\nU 1\nR 13\nD 2\nU 14\nR 11\nD 12\nU 17\nR 12\nL 6\nU 4\nR 16\nD 4\nR 2\nD 1\nU 6\nD 11\nR 8\nL 7\nR 3\nU 3\nL 14\nR 11\nL 9\nU 9\nL 2\nR 6\nU 11\nR 9\nL 15\nU 7\nR 6\nD 15\nU 11\nL 5\nU 2\nR 8\nD 13\nL 5\nU 6\nR 13\nD 15\nU 11\nL 10\nR 16\nL 14\nU 15\nR 3\nL 9\nU 11\nL 6\nR 7\nL 6\nD 16\nU 8\nR 2\nU 14\nD 14\nR 15\nD 2\nL 16\nD 14\nU 17\nD 6\nL 16\nD 7\nR 2\nU 17\nD 2\nU 2\nD 15\nU 5\nR 9\nL 10\nD 16\nU 1\nL 5\nU 6\nR 10\nD 11\nR 1\nU 7\nR 13\nU 6\nR 2\nL 11\nD 13\nU 8\nD 4\nR 15\nU 8\nR 14\nL 17\nU 5\nL 12\nU 11\nL 14\nR 12\nL 2\nD 10\nL 14\nD 10\nL 11\nU 15\nD 7\nU 5\nR 10\nU 4\nR 14\nU 3\nD 9\nL 6\nD 10\nL 1\nU 4\nL 15\nR 4\nU 13\nD 18\nR 15\nL 3\nU 17\nR 5\nL 3\nD 3\nL 1\nU 10\nL 3\nR 5\nU 13\nD 2\nR 17\nD 3\nU 4\nL 15\nD 5\nL 18\nU 7\nD 7\nU 10\nL 18\nD 14\nU 18\nL 11\nD 3\nL 15\nD 14\nL 16\nR 2\nD 8\nL 12\nU 11\nR 12\nD 2\nL 3\nR 12\nU 7\nL 9\nD 8\nU 7\nL 10\nD 2\nU 10\nD 16\nL 17\nR 13\nL 12\nR 18\nL 6\nU 2\nD 6\nL 15\nD 8\nU 14\nR 12\nL 8\nR 6\nL 17\nU 5\nD 4\nL 8\nU 16\nR 8\nU 12\nL 1\nR 6\nD 11\nL 10\nU 13\nR 4\nL 9\nR 7\nD 1\nL 17\nR 4\nL 13\nR 8\nD 14\nU 3\nR 10\nL 18\nR 11\nU 3\nD 5\nU 2\nL 14\nR 12\nD 12\nR 11\nU 5\nD 17\nR 2\nD 6\nL 13\nD 4\nL 5\nR 7\nU 7\nL 12\nD 16\nU 6\nL 1\nR 18\nD 4\nR 11\nD 10\nL 11\nD 2\nL 1\nD 5\nU 2\nR 19\nD 10\nU 14\nR 1\nU 11\nR 15\nD 2\nL 15\nU 19\nR 3\nL 2\nD 12\nR 2\nL 15\nR 8\nD 3\nR 8\nD 11\nR 17\nD 5\nR 16\nL 8\nR 12\nL 5\nR 14\nD 3\nR 19\nD 4\nR 18\nL 4\nU 12\nD 7\nL 8\nR 11\nU 14\nL 12\nU 17\nL 1\nD 5\nR 8\nL 18\nU 16\nL 15\nD 13\nU 18\nR 11\nL 7\nR 19\nL 15\nU 10\nL 4\nR 9\nU 7\nR 8\nL 16\nR 8\nL 17\nD 8\nL 17\nD 18\nU 9\nD 2\nL 6\nU 6\nL 7\nD 18\nR 1\nU 13\nL 2\nU 1\nR 8\nU 11\nD 14\nR 6\nU 13\nR 2\nD 11\nL 5\nU 12\nD 7\nU 4\nR 4\nD 19\nR 17\nL 10\nU 5\nL 1\nU 4\nL 6\nD 11\nL 16\nD 9\nR 16\nL 3\nR 1\nD 3\nL 5\nU 18\nD 13\nR 10\nU 5\nD 4\nU 17\nR 4\nD 18\nU 1")
  (reductions step
              (init 2)
              (expand-cmds (parse t)))
  (part-1 (parse in))
  (part-2 (parse in))
  (reductions step
              (init 2)
              (expand-cmds (parse t)))

  )
