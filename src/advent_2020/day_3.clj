(ns advent-2020.day-3
  (:require [clojure.string :as str]))

(defn parse [s [x y :as _slope]]
  (let [lns (str/split-lines s)
        max-width (* x (count lns))]
    (into []
          (map (fn [l]
                 (into []
                       (take max-width)
                       (cycle l))))
          lns)))

(defn coord [[x y]]
  [y x])

(defn line-points [board [x y :as _slope]]
  (let [max-height (count board)]
    (into []
          (take-while (fn [[x' y']]
                        (<= y' max-height)))
          (iterate (fn [[x' y']]
                     [(+ x' x)
                      (+ y' y)])
                   [0 0]))))

(defn trees-in-path [board slope]
  (count (filter (fn [pnt]
                   (= \# (get-in board
                                 (coord pnt))))
                 (line-points board
                              slope))))

(defn part-1 [in slope]
  (trees-in-path (parse in
                        slope)
                 slope))

(defn part-2 [in slopes]
  (let [max-x (apply max
                     (map first
                          slopes))
        b (parse in
                 [max-x 1])]
    (apply *
           (map (fn [s]
                  (trees-in-path b s))
                slopes))))

(comment
  (part-1 "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"
          [3 1])
  (part-2 "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"
          [[1 1]
           [3 1]
           [5 1]
           [7 1]
           [1 2]])
  )
