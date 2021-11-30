(ns advent-2015.day-21
  (:require [clojure.string :as str]
            [advent.util :as util]))

(def merch* "Weapons:    Cost  Damage  Armor\nDagger        8     4       0\nShortsword   10     5       0\nWarhammer    25     6       0\nLongsword    40     7       0\nGreataxe     74     8       0\n\nArmor:      Cost  Damage  Armor\nLeather      13     0       1\nChainmail    31     0       2\nSplintmail   53     0       3\nBandedmail   75     0       4\nPlatemail   102     0       5\n\nRings:      Cost  Damage  Armor\nDamage +1    25     1       0\nDamage +2    50     2       0\nDamage +3   100     3       0\nDefense +1   20     0       1\nDefense +2   40     0       2\nDefense +3   80     0       3")

(defn parse-merch [s]
  (into {}
        (map (fn [sec]
               (let [[h & rs] (map (fn [l]
                                     (-> l
                                         (str/replace #" \+(\d)" "+$1")
                                         (str/split #"\s+")))
                                   (str/split-lines sec))]
                 [(first h)
                  (map (fn [[n & vs]]
                         (zipmap h
                                 (concat [n]
                                         (map #(Long/parseLong %)
                                              vs))))
                       rs)])))
        (str/split s #"(?m)\s*^$\s*")))


(def merch (parse-merch merch*))


(defn parse-character [s]
  (into {}
        (comp (map (fn [s]
                     (str/split s #":\s+")))
              (map (fn [[k v]]
                     [k (Long/parseLong v)])))
        (str/split-lines s)))


(defn calc-stats [c stuff]
  (apply merge-with
         (fn [l r]
           (cond
             (string? l) [l r]
             (vector? l) (conj l r)
             (number? l) (+ l r)))
         c
         stuff))


(defn step [[{c-d "Damage"
              c-a "Armor" :as c}
             {b-d "Damage"
              b-a "Armor" :as b}]]
  [(update c "Hit Points" - (- b-d c-a))
   (update b "Hit Points" - (- c-d b-a))])


(defn winner [c b]
  (some (fn [[{c-hp "Hit Points"}
              {b-hp "Hit Points"}]]
          (cond
            (not (pos? b-hp)) :player ;; player goes first
            (not (pos? c-hp)) :boss))
        (iterate step [c b])))


(def all-stats
  (let [{a "Armor:"
         w "Weapons:"
         r "Rings:"} merch]
    (for [w w
          a (conj a nil)
          r1 (conj r nil)
          r2 (conj r nil)
          :when (or (and (nil? r1)
                         (nil? r2))
                    (not= r1 r2))]
      (calc-stats {"Hit Points" 100}
                  [w a r1 r2]))))


(defn part-1 [boss]
  (util/min-by #(get % "Cost")
               (filter (fn [p]
                         (= :player (winner p boss)))
                       all-stats)))


(defn part-2 [boss]
  (util/max-by #(get % "Cost")
               (filter (fn [p]
                         (= :boss (winner p boss)))
                       all-stats)))


(comment

  (def boss "Hit Points: 100\nDamage: 8\nArmor: 2")

  (keys merch)

  (def b (parse-character character))
  (clojure.pprint/print-table (sort-by (fn [{c "Cost"
                                             d "Damage"
                                             a "Armor"}]
                                         [c d a])
                                       all-stats))
  (part-1 b)
  (part-2 b)
  (take 10 all-stats)
  (winner {"Hit Points" 100, "Weapons:" "Dagger", "Cost" 58, "Damage" 6, "Armor" 0, "Rings:" "Damage+2"}
          b)
  (winner (calc-stats {"Hit Points" 100}
                      (map (comp first val)
                           m))
          b))
