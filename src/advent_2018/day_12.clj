(ns advent-2018.day-12
  (:require [clojure.string :as str]))

(def toy-initial-state "#..#.#..##......###...###")
(def toy-rules "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #")

(def initial-state "#..#####.#.#.##....####..##.#.#.##.##.#####..####.#.##.....#..#.#.#...###..#..###.##.#..##.#.#.....#")
(def rules ".#.## => #\n.###. => #\n#..#. => .\n...## => .\n#.##. => #\n....# => .\n..##. => #\n.##.. => .\n##..# => .\n.#..# => #\n#.#.# => .\n#.... => .\n.#### => #\n.##.# => .\n..#.. => #\n####. => #\n#.#.. => .\n.#... => .\n###.# => .\n..### => .\n#..## => #\n...#. => #\n..... => .\n###.. => #\n#...# => .\n..#.# => #\n##... => #\n##.## => .\n##.#. => .\n##### => .\n.#.#. => #\n#.### => #")

(defn parse-state [s]
  (vec s))

(defn parse-rules [s]
  (into {}
        (map (fn [l]
               [(vec (take 5 l))
                (last l)]))
        (str/split-lines s)))

(defn state-sums [rules state]
  (let [step (fn [rules state]
               (map #(get rules % \.)
                    (partition 5
                               1
                               (concat [\. \. \. \.]
                                       state
                                       [\. \. \. \.]))))
        pot-ids (fn [i]
                  ;; start at neg index adjusted for amount
                  ;; added in `step fn
                  (iterate inc
                           (- (* 2 i))))]
    (map (fn [i state]
           (reduce +
                   (map (fn [i c]
                          (if (= c \#)
                            i
                            0))
                        (pot-ids i)
                        state)))
         (range)
         (iterate (partial step rules)
                  state))))

(defn part-1 [rules state]
  (let [step (fn [rules state]
               (map #(get rules % \.)
                    (partition 5
                               1
                               (concat [\. \. \. \.]
                                       state
                                       [\. \. \. \.]))))
        pot-ids (fn [i]
                  ;; start at neg index adjusted for amount
                  ;; added in `step fn
                  (iterate inc
                           (- (* 2 i))))]
    (nth (map (fn [i state]
                (reduce +
                        (map (fn [i c]
                               (if (= c \#)
                                 i
                                 0))
                             (pot-ids i)
                             state)))
              (range)
              (iterate (partial step rules)
                       state))
         20)))

(defn part-2 [rules state]
  (let [sums (state-sums rules state)
        chunks (take-while (fn [[l m r]]
                             (not= (- m l)
                                   (- r m)))
                           (partition 3 1 sums))
        [a b c] (last chunks)
        cnt (count chunks)]
    (+ b
       (* (- c b)
          (- 50000000000
             cnt)))))

(comment
  (part-1 (parse-rules toy-rules)
          (parse-state toy-initial-state))

  (part-1 (parse-rules rules)
          (parse-state initial-state))

  (part-2 (parse-rules rules)
          (parse-state initial-state)))