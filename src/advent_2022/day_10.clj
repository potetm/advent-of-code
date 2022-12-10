(ns advent-2022.day-10
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (map (fn [l]
         (let [[_ instr _ arg] (re-find #"(\w+)( (-?\d+))?"
                                        l)]
           (into [instr]
                 (when arg
                   [(parse-long arg)]))))
       (str/split-lines s)))


(def init
  [{:cycle 1
    :x 1}])


(defn step [prev [instr arg]]
  (let [{c :cycle
         x :x} (peek prev)
        sleep {:cycle (inc c)
               :x x}]
    (case instr
      "addx" [sleep
              {:cycle (+ 2 c)
               :x (+ x arg)}]
      "noop" [sleep])))


(defn run
  ([state instrs]
   (run state
        instrs
        (map identity)))
  ([state instrs xform]
   (sequence (comp cat
                   xform)
             (reductions step
                         state
                         instrs))))


(defn part-1 [in]
  (let [states (run init in)
        cycles (into #{}
                     (range 20 221 40))]
    (util/sum (comp (filter (comp cycles :cycle))
                    (map (fn [{c :cycle
                               x :x}]
                           (* c x))))
              states)))


(defn part-2 [in]
  (doseq [row (run init
                   in
                   (xf/partition 40))]
    (println (str/join (map-indexed (fn [idx {x :x}]
                                      (if (<= (dec x)
                                              idx
                                              (inc x))
                                        "░"
                                        " "))
                                    row)))))

(comment
  (def t "noop\naddx 3\naddx -5")
  (def t' "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop")
  (def in "addx 2\naddx 3\naddx 1\nnoop\naddx 4\naddx 1\nnoop\naddx 28\naddx -24\nnoop\naddx 5\naddx 17\naddx -16\nnoop\naddx 6\nnoop\naddx -7\naddx 11\naddx 4\nnoop\naddx 1\naddx -36\naddx -2\nnoop\nnoop\naddx 10\nnoop\nnoop\naddx -2\naddx 2\naddx 25\naddx -18\naddx 23\naddx -22\naddx 2\naddx 5\naddx -10\naddx -15\naddx 28\naddx 2\naddx 5\naddx 2\naddx -16\naddx 17\naddx -36\nnoop\nnoop\naddx 39\naddx -32\naddx -5\naddx 7\naddx 1\naddx 5\naddx -13\naddx 1\naddx 17\naddx 1\nnoop\naddx 7\nnoop\naddx -2\naddx 2\naddx 5\naddx 2\nnoop\nnoop\nnoop\nnoop\naddx -37\nnoop\nnoop\nnoop\nnoop\naddx 6\naddx 11\naddx -7\naddx 29\naddx -22\naddx 5\nnoop\nnoop\nnoop\naddx 3\nnoop\naddx 7\naddx -28\naddx 24\naddx 3\naddx 2\nnoop\naddx 2\nnoop\naddx 3\naddx -38\nnoop\naddx 7\naddx -2\naddx 1\naddx 6\naddx -10\naddx 38\naddx -25\naddx 5\naddx 2\naddx -10\naddx 11\naddx 2\nnoop\naddx 3\naddx 2\nnoop\naddx 3\naddx 2\naddx 5\naddx -39\naddx 1\naddx 1\naddx 3\naddx 2\naddx 4\naddx 29\naddx -23\nnoop\naddx -1\naddx 5\nnoop\naddx 11\naddx -10\naddx 5\naddx -1\nnoop\naddx 3\nnoop\naddx 3\naddx 4\nnoop\nnoop\nnoop\nnoop\nnoop")

  (parse t)
  (part-1 (parse in))
  (part-2 (parse in))

  )
