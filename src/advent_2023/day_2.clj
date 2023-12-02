(ns advent-2023.day-2
  (:require
    [advent.util :as util]
    [clojure.string :as str]))

(def bag-content
  {"red" 12
   "green" 13
   "blue" 14})


(defn parse [s]
  (into []
        (map (fn [l]
               (let [[_ g] (re-find #"Game (\d+)" l)
                     g (parse-long g)]
                 {:game g
                  :picks (into []
                               (map (fn [s]
                                      (into {}
                                            (map (fn [[_ n c]]
                                                   [c (parse-long n)]))
                                            (re-seq #"(\d+) (red|green|blue)"
                                                    s))))
                               (str/split l #";"))})))
        (str/split-lines s)))


(defn possible? [content picks]
  (every? (fn [[c n]]
            (not (neg? n)))
          (util/map-diff content
                         (apply merge-with max picks))))


(defn part-1 [in]
  (util/sum (comp (filter (fn [{ps :picks}]
                            (possible? bag-content
                                       ps)))
                  (map :game))
            (parse in)))


(defn min-possible [{ps :picks}]
  (apply merge-with max ps))


(defn part-2 [in]
  (util/sum (comp (map min-possible)
                  (map (fn [pick]
                         (util/product (vals pick)))))
            (parse in)))


(comment
  (def t1 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  (parse t1)
  (into []
        (filter (fn [{ps :picks}]
                  (possible? bag-content
                             ps)))
        (parse t1))
  (part-1 t1)
  (parse in)
  (part-1 in)
  (part-2 t1)
  (part-2 in)

  )
