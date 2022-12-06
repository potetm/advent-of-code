(ns advent-2022.day-5
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (let [[stacks instrs] (str/split s #"(?m)\n\n")]
    {:state (into {}
                  (comp (filter (fn [col]
                                  (Character/isDigit ^char (peek col))))
                        (map (fn [col]
                               [(peek col) (into []
                                                 (remove #{\space})
                                                 (rseq (pop col)))])))
                  (util/transpose (str/split-lines stacks)))
     :instrs (into []
                   (map (fn [l]
                          (let [[_ c [from] [to]] (re-find #"move (\d+) from (\d+) to (\d+)"
                                                           l)]
                            {:op :move
                             :count (Long/parseLong c)
                             :from from
                             :to to})))
                   (str/split-lines instrs))}))

(defn step [{s :state
             [{op :op :as instr} & r] :instrs}]
  (when instr
    (let [ops {:move (fn [s {c :count f :from t :to}]
                       (reduce (fn [s _]
                                 (let [stack (get s f)]
                                   (-> s
                                       (update f pop)
                                       (update t conj (peek stack)))))
                               s
                               (range c)))
               :mmove (fn [s {c :count f :from t :to}]
                        (let [stack (get s f)]
                          (-> s
                              (update f #(into []
                                               (xf/drop-last c)
                                               %))
                              (update t #(into %
                                               (take-last c stack))))))}]
      {:state ((ops op) s instr)
       :instrs r})))


(defn run-to-complete [s]
  (peek (into []
              (comp (take-while some?)
                    xf/last)
              (iterate step s))))


(defn res [{s :state}]
  (apply str (map peek
                  (vals s))))


(defn part-1 [s]
  (res (run-to-complete s)))


(defn part-2 [{instrs :instrs :as s}]
  (res (run-to-complete (assoc s
                          :instrs (into []
                                        (map (fn [instr]
                                               (assoc instr :op :mmove)))
                                        instrs)))))

(comment
  (def t "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
  (def in "[V]     [B]                     [F]\n[N] [Q] [W]                 [R] [B]\n[F] [D] [S]     [B]         [L] [P]\n[S] [J] [C]     [F] [C]     [D] [G]\n[M] [M] [H] [L] [P] [N]     [P] [V]\n[P] [L] [D] [C] [T] [Q] [R] [S] [J]\n[H] [R] [Q] [S] [V] [R] [V] [Z] [S]\n[J] [S] [N] [R] [M] [T] [G] [C] [D]\n 1   2   3   4   5   6   7   8   9 \n\nmove 1 from 8 to 4\nmove 1 from 7 to 8\nmove 1 from 6 to 3\nmove 2 from 6 to 5\nmove 8 from 5 to 1\nmove 5 from 3 to 8\nmove 1 from 7 to 8\nmove 8 from 1 to 2\nmove 3 from 3 to 9\nmove 13 from 8 to 7\nmove 2 from 1 to 2\nmove 1 from 6 to 2\nmove 2 from 1 to 7\nmove 4 from 4 to 2\nmove 10 from 9 to 4\nmove 7 from 4 to 1\nmove 1 from 6 to 7\nmove 2 from 4 to 5\nmove 1 from 5 to 2\nmove 1 from 5 to 8\nmove 3 from 1 to 5\nmove 2 from 4 to 6\nmove 2 from 6 to 4\nmove 2 from 4 to 5\nmove 5 from 1 to 5\nmove 1 from 9 to 5\nmove 1 from 8 to 5\nmove 14 from 2 to 6\nmove 12 from 7 to 4\nmove 4 from 6 to 7\nmove 3 from 6 to 4\nmove 4 from 4 to 9\nmove 2 from 4 to 6\nmove 2 from 9 to 3\nmove 9 from 4 to 3\nmove 2 from 1 to 6\nmove 5 from 7 to 3\nmove 4 from 7 to 8\nmove 2 from 6 to 7\nmove 3 from 6 to 7\nmove 10 from 5 to 8\nmove 8 from 8 to 9\nmove 10 from 9 to 7\nmove 12 from 7 to 5\nmove 1 from 1 to 5\nmove 3 from 7 to 2\nmove 10 from 3 to 7\nmove 6 from 5 to 7\nmove 2 from 6 to 1\nmove 12 from 2 to 7\nmove 3 from 3 to 1\nmove 1 from 6 to 5\nmove 10 from 5 to 7\nmove 3 from 3 to 4\nmove 34 from 7 to 1\nmove 2 from 6 to 9\nmove 1 from 6 to 3\nmove 3 from 1 to 3\nmove 1 from 7 to 3\nmove 3 from 3 to 6\nmove 1 from 4 to 3\nmove 22 from 1 to 6\nmove 2 from 9 to 7\nmove 2 from 4 to 9\nmove 12 from 6 to 8\nmove 1 from 7 to 6\nmove 4 from 8 to 2\nmove 1 from 7 to 1\nmove 6 from 8 to 9\nmove 1 from 2 to 5\nmove 1 from 2 to 7\nmove 13 from 1 to 2\nmove 2 from 3 to 1\nmove 4 from 7 to 5\nmove 3 from 9 to 4\nmove 1 from 1 to 8\nmove 4 from 5 to 2\nmove 12 from 6 to 2\nmove 3 from 1 to 3\nmove 1 from 4 to 1\nmove 9 from 8 to 5\nmove 6 from 5 to 7\nmove 2 from 4 to 5\nmove 5 from 9 to 6\nmove 5 from 3 to 7\nmove 30 from 2 to 6\nmove 11 from 7 to 9\nmove 36 from 6 to 3\nmove 10 from 9 to 3\nmove 1 from 6 to 5\nmove 3 from 5 to 2\nmove 2 from 5 to 2\nmove 28 from 3 to 4\nmove 6 from 4 to 1\nmove 1 from 2 to 3\nmove 2 from 5 to 2\nmove 6 from 1 to 7\nmove 1 from 1 to 6\nmove 6 from 3 to 5\nmove 6 from 7 to 2\nmove 1 from 6 to 4\nmove 2 from 2 to 6\nmove 8 from 2 to 1\nmove 3 from 2 to 4\nmove 2 from 3 to 4\nmove 4 from 3 to 4\nmove 1 from 6 to 1\nmove 2 from 1 to 8\nmove 1 from 6 to 4\nmove 1 from 9 to 3\nmove 2 from 5 to 4\nmove 1 from 8 to 7\nmove 1 from 7 to 9\nmove 1 from 3 to 5\nmove 1 from 8 to 6\nmove 34 from 4 to 9\nmove 13 from 9 to 8\nmove 1 from 8 to 2\nmove 1 from 2 to 1\nmove 4 from 5 to 1\nmove 9 from 8 to 7\nmove 11 from 1 to 3\nmove 1 from 4 to 2\nmove 1 from 6 to 7\nmove 1 from 9 to 4\nmove 1 from 4 to 1\nmove 1 from 5 to 3\nmove 5 from 7 to 8\nmove 1 from 2 to 5\nmove 1 from 5 to 1\nmove 21 from 9 to 5\nmove 19 from 3 to 4\nmove 17 from 4 to 6\nmove 2 from 8 to 4\nmove 2 from 6 to 8\nmove 2 from 6 to 9\nmove 2 from 7 to 6\nmove 1 from 4 to 9\nmove 6 from 5 to 6\nmove 1 from 9 to 8\nmove 8 from 5 to 7\nmove 15 from 6 to 2\nmove 1 from 9 to 7\nmove 2 from 1 to 6\nmove 3 from 4 to 7\nmove 1 from 1 to 6\nmove 3 from 5 to 4\nmove 2 from 5 to 6\nmove 2 from 4 to 1\nmove 13 from 7 to 8\nmove 2 from 6 to 4\nmove 3 from 2 to 4\nmove 2 from 7 to 6\nmove 5 from 4 to 6\nmove 4 from 2 to 6\nmove 1 from 1 to 9\nmove 18 from 8 to 3\nmove 1 from 4 to 5\nmove 1 from 2 to 7\nmove 15 from 3 to 1\nmove 1 from 5 to 1\nmove 3 from 3 to 4\nmove 1 from 5 to 4\nmove 1 from 5 to 6\nmove 1 from 6 to 8\nmove 2 from 8 to 2\nmove 3 from 1 to 8\nmove 6 from 2 to 8\nmove 1 from 7 to 6\nmove 12 from 8 to 5\nmove 2 from 9 to 6\nmove 6 from 1 to 5\nmove 9 from 5 to 3\nmove 1 from 2 to 8\nmove 20 from 6 to 9\nmove 3 from 6 to 7\nmove 1 from 7 to 1\nmove 7 from 3 to 4\nmove 2 from 7 to 2\nmove 1 from 8 to 7\nmove 8 from 4 to 1\nmove 11 from 1 to 7\nmove 10 from 7 to 6\nmove 2 from 4 to 9\nmove 21 from 9 to 3\nmove 6 from 5 to 9\nmove 6 from 3 to 2\nmove 1 from 4 to 5\nmove 1 from 7 to 9\nmove 8 from 3 to 2\nmove 9 from 2 to 1\nmove 14 from 1 to 6\nmove 1 from 1 to 7\nmove 4 from 3 to 8\nmove 3 from 8 to 7\nmove 5 from 7 to 4\nmove 3 from 6 to 9\nmove 2 from 3 to 7\nmove 3 from 5 to 6\nmove 1 from 5 to 6\nmove 2 from 7 to 9\nmove 1 from 8 to 3\nmove 22 from 6 to 5\nmove 3 from 9 to 4\nmove 3 from 6 to 1\nmove 5 from 4 to 6\nmove 9 from 2 to 8\nmove 4 from 6 to 1\nmove 1 from 3 to 2\nmove 1 from 2 to 3\nmove 6 from 8 to 1\nmove 2 from 4 to 3\nmove 10 from 1 to 7\nmove 2 from 8 to 7\nmove 1 from 9 to 6\nmove 4 from 3 to 5\nmove 1 from 8 to 3\nmove 4 from 9 to 8\nmove 1 from 4 to 3\nmove 1 from 3 to 8\nmove 3 from 7 to 6\nmove 1 from 1 to 5\nmove 10 from 5 to 9\nmove 5 from 6 to 4\nmove 5 from 8 to 5\nmove 4 from 9 to 8\nmove 3 from 3 to 9\nmove 2 from 8 to 6\nmove 5 from 7 to 5\nmove 1 from 4 to 1\nmove 1 from 1 to 2\nmove 2 from 8 to 6\nmove 1 from 2 to 1\nmove 1 from 7 to 2\nmove 1 from 1 to 5\nmove 28 from 5 to 9\nmove 3 from 6 to 1\nmove 1 from 6 to 9\nmove 1 from 2 to 9\nmove 2 from 1 to 2\nmove 2 from 7 to 5\nmove 1 from 7 to 5\nmove 1 from 2 to 5\nmove 3 from 1 to 9\nmove 1 from 5 to 8\nmove 15 from 9 to 2\nmove 11 from 9 to 4\nmove 11 from 4 to 7\nmove 2 from 4 to 1\nmove 7 from 7 to 8\nmove 1 from 1 to 4\nmove 20 from 9 to 1\nmove 2 from 7 to 8\nmove 1 from 4 to 6\nmove 1 from 6 to 2\nmove 2 from 7 to 5\nmove 1 from 9 to 6\nmove 1 from 4 to 9\nmove 4 from 5 to 2\nmove 1 from 6 to 8\nmove 1 from 4 to 9\nmove 11 from 8 to 3\nmove 1 from 1 to 9\nmove 1 from 5 to 9\nmove 1 from 2 to 6\nmove 4 from 9 to 8\nmove 4 from 8 to 7\nmove 10 from 1 to 6\nmove 7 from 1 to 5\nmove 8 from 3 to 4\nmove 2 from 3 to 5\nmove 3 from 7 to 4\nmove 1 from 4 to 5\nmove 2 from 1 to 6\nmove 9 from 2 to 6\nmove 1 from 7 to 9\nmove 1 from 3 to 2\nmove 7 from 4 to 3\nmove 3 from 3 to 7\nmove 5 from 2 to 3\nmove 1 from 1 to 9\nmove 2 from 2 to 7\nmove 1 from 4 to 6\nmove 3 from 5 to 6\nmove 4 from 7 to 6\nmove 1 from 7 to 4\nmove 1 from 4 to 7\nmove 1 from 2 to 8\nmove 1 from 7 to 1\nmove 27 from 6 to 2\nmove 1 from 4 to 1\nmove 7 from 5 to 7\nmove 1 from 4 to 1\nmove 1 from 8 to 3\nmove 3 from 7 to 3\nmove 2 from 1 to 6\nmove 2 from 9 to 1\nmove 18 from 2 to 1\nmove 2 from 7 to 5\nmove 12 from 3 to 4\nmove 1 from 5 to 6\nmove 3 from 6 to 1\nmove 24 from 1 to 8\nmove 9 from 2 to 4\nmove 3 from 2 to 1\nmove 2 from 6 to 3\nmove 1 from 6 to 9\nmove 1 from 5 to 6\nmove 1 from 6 to 2\nmove 1 from 1 to 7\nmove 1 from 2 to 1\nmove 1 from 1 to 2\nmove 3 from 7 to 2\nmove 2 from 1 to 4\nmove 8 from 4 to 5\nmove 22 from 8 to 1\nmove 1 from 8 to 1\nmove 13 from 4 to 1\nmove 1 from 8 to 5\nmove 3 from 3 to 1\nmove 1 from 2 to 7\nmove 38 from 1 to 6\nmove 27 from 6 to 1\nmove 2 from 2 to 9\nmove 3 from 9 to 8\nmove 2 from 8 to 6\nmove 1 from 8 to 3\nmove 1 from 2 to 1\nmove 1 from 3 to 6\nmove 1 from 2 to 3\nmove 1 from 7 to 6\nmove 7 from 6 to 3\nmove 20 from 1 to 4\nmove 6 from 1 to 6\nmove 17 from 4 to 7\nmove 3 from 6 to 5\nmove 14 from 7 to 9\nmove 8 from 5 to 7\nmove 3 from 1 to 6\nmove 3 from 3 to 1\nmove 2 from 4 to 1\nmove 4 from 5 to 1\nmove 9 from 6 to 2\nmove 3 from 6 to 4\nmove 4 from 7 to 8\nmove 4 from 1 to 6\nmove 2 from 3 to 1\nmove 6 from 6 to 7\nmove 4 from 8 to 7\nmove 4 from 2 to 1\nmove 4 from 2 to 3\nmove 4 from 9 to 5\nmove 8 from 9 to 5\nmove 1 from 9 to 5\nmove 1 from 2 to 1\nmove 16 from 7 to 2\nmove 10 from 2 to 9\nmove 11 from 9 to 8\nmove 4 from 3 to 5\nmove 3 from 1 to 4\nmove 13 from 5 to 7\nmove 10 from 8 to 5\nmove 2 from 1 to 5\nmove 11 from 7 to 4\nmove 2 from 3 to 6\nmove 3 from 7 to 6\nmove 1 from 3 to 2\nmove 1 from 1 to 8\nmove 2 from 8 to 4\nmove 3 from 1 to 2\nmove 4 from 6 to 1\nmove 7 from 1 to 9\nmove 1 from 6 to 7\nmove 2 from 5 to 8\nmove 1 from 2 to 9\nmove 1 from 7 to 8\nmove 5 from 5 to 8\nmove 1 from 2 to 3\nmove 4 from 2 to 5\nmove 17 from 4 to 1\nmove 10 from 5 to 9\nmove 2 from 4 to 2\nmove 2 from 4 to 1\nmove 1 from 4 to 9\nmove 1 from 3 to 7\nmove 1 from 7 to 8\nmove 12 from 9 to 2\nmove 1 from 2 to 4\nmove 1 from 4 to 1\nmove 1 from 1 to 9\nmove 1 from 8 to 1\nmove 8 from 8 to 3\nmove 2 from 5 to 1\nmove 3 from 1 to 9\nmove 1 from 2 to 6\nmove 4 from 3 to 7\nmove 1 from 7 to 6\nmove 10 from 9 to 2\nmove 1 from 5 to 9\nmove 1 from 9 to 3\nmove 17 from 1 to 6\nmove 2 from 1 to 2\nmove 11 from 6 to 7\nmove 2 from 2 to 9\nmove 2 from 9 to 5\nmove 12 from 7 to 9\nmove 20 from 2 to 7\nmove 5 from 9 to 5\nmove 21 from 7 to 1\nmove 2 from 6 to 4\nmove 11 from 1 to 4\nmove 5 from 4 to 6\nmove 1 from 7 to 8\nmove 5 from 9 to 3\nmove 5 from 2 to 8\nmove 3 from 9 to 3\nmove 2 from 8 to 7\nmove 2 from 1 to 7\nmove 10 from 6 to 3\nmove 1 from 2 to 6\nmove 2 from 8 to 5\nmove 1 from 6 to 5\nmove 2 from 4 to 9\nmove 1 from 4 to 5\nmove 8 from 1 to 6\nmove 4 from 4 to 8\nmove 6 from 8 to 4\nmove 21 from 3 to 9\nmove 5 from 9 to 2\nmove 4 from 7 to 9\nmove 22 from 9 to 3\nmove 9 from 6 to 4\nmove 2 from 2 to 6\nmove 2 from 2 to 1\nmove 2 from 5 to 7\nmove 7 from 5 to 4\nmove 22 from 4 to 2\nmove 2 from 5 to 4\nmove 16 from 2 to 5\nmove 2 from 6 to 2\nmove 13 from 3 to 4\nmove 5 from 5 to 7\nmove 15 from 4 to 7\nmove 3 from 2 to 3\nmove 3 from 2 to 5\nmove 1 from 1 to 2\nmove 1 from 2 to 4\nmove 6 from 5 to 9\nmove 4 from 3 to 6\nmove 2 from 5 to 9\nmove 1 from 2 to 7\nmove 1 from 1 to 9\nmove 2 from 4 to 5\nmove 19 from 7 to 8\nmove 1 from 6 to 5\nmove 1 from 5 to 1\nmove 1 from 9 to 4\nmove 5 from 8 to 1\nmove 3 from 8 to 1\nmove 7 from 5 to 6\nmove 3 from 7 to 1\nmove 1 from 2 to 5\nmove 4 from 9 to 8\nmove 2 from 5 to 6\nmove 10 from 1 to 4\nmove 1 from 7 to 2\nmove 6 from 3 to 4\nmove 9 from 4 to 3\nmove 2 from 2 to 8\nmove 2 from 9 to 5\nmove 5 from 8 to 3\nmove 1 from 1 to 5\nmove 2 from 5 to 6\nmove 1 from 1 to 7\nmove 2 from 9 to 7\nmove 8 from 4 to 7\nmove 3 from 3 to 9\nmove 4 from 6 to 3\nmove 1 from 5 to 3\nmove 1 from 7 to 2\nmove 1 from 2 to 1\nmove 1 from 6 to 5\nmove 1 from 5 to 2\nmove 10 from 7 to 4\nmove 10 from 4 to 1\nmove 10 from 1 to 8\nmove 1 from 9 to 6\nmove 1 from 1 to 4\nmove 11 from 8 to 1\nmove 2 from 9 to 5\nmove 5 from 6 to 3\nmove 1 from 3 to 8\nmove 4 from 1 to 3\nmove 5 from 3 to 8\nmove 1 from 4 to 7\nmove 1 from 7 to 2\nmove 13 from 3 to 5\nmove 2 from 2 to 1\nmove 4 from 3 to 1\nmove 4 from 5 to 6\nmove 3 from 6 to 2\nmove 4 from 5 to 4\nmove 8 from 8 to 7\nmove 1 from 3 to 9")

  (part-1 (parse in))
  (step (step (parse t)))


  (require '[clj-java-decompiler.core :as dc])

  (dc/decompile (defn step [{s :state
                             [{op :op :as instr} & r] :instrs}]
                  (when instr
                    (let [ops {:move 123}]
                      {:state ((ops op) s instr)
                       :instrs r}))))
  )
