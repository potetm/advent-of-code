(ns advent-2015.day-13
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]
    [advent.util :as util]))

(defn parse [in]
  (transduce (map (fn [s]
                    (let [[_ p1 gain|lose units p2] (re-find #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)."
                                                             s)
                          units (Long/parseLong units)]
                      {:person p1
                       :partner p2
                       :unit-change (if (= "gain" gain|lose)
                                      units
                                      (- units))})))
             (completing (fn [acc {p1 :person
                                   p2 :partner
                                   uc :unit-change}]
                           (assoc-in acc
                                     [p1 p2]
                                     uc)))
             {}
             (str/split-lines in)))


(defn orientations [in]
  (let [[p1 & r] (keys in)]
    (map #(cons p1 %)
         (combo/permutations r))))


(defn pairs [orientation]
  (take (count orientation)
        (partition 2
                   1
                   (cycle orientation))))


(defn happiness [in orientation]
  (util/sum (map (fn [pair]
                   (+ (get-in in pair)
                      (get-in in (reverse pair))))
                 (pairs orientation))))


(defn part-1 [in]
  (util/max (map (partial happiness in)
                 (orientations in))))


(defn part-2 [in]
  (let [in' (into {"Me" (into {}
                              (map (fn [p]
                                     {p 0}))
                              (keys in))}
                  (map (fn [[k m]]
                         [k (assoc m "Me" 0)]))
                  in)]
    (part-1 in')))

(comment
  (def in "Alice would lose 57 happiness units by sitting next to Bob.\nAlice would lose 62 happiness units by sitting next to Carol.\nAlice would lose 75 happiness units by sitting next to David.\nAlice would gain 71 happiness units by sitting next to Eric.\nAlice would lose 22 happiness units by sitting next to Frank.\nAlice would lose 23 happiness units by sitting next to George.\nAlice would lose 76 happiness units by sitting next to Mallory.\nBob would lose 14 happiness units by sitting next to Alice.\nBob would gain 48 happiness units by sitting next to Carol.\nBob would gain 89 happiness units by sitting next to David.\nBob would gain 86 happiness units by sitting next to Eric.\nBob would lose 2 happiness units by sitting next to Frank.\nBob would gain 27 happiness units by sitting next to George.\nBob would gain 19 happiness units by sitting next to Mallory.\nCarol would gain 37 happiness units by sitting next to Alice.\nCarol would gain 45 happiness units by sitting next to Bob.\nCarol would gain 24 happiness units by sitting next to David.\nCarol would gain 5 happiness units by sitting next to Eric.\nCarol would lose 68 happiness units by sitting next to Frank.\nCarol would lose 25 happiness units by sitting next to George.\nCarol would gain 30 happiness units by sitting next to Mallory.\nDavid would lose 51 happiness units by sitting next to Alice.\nDavid would gain 34 happiness units by sitting next to Bob.\nDavid would gain 99 happiness units by sitting next to Carol.\nDavid would gain 91 happiness units by sitting next to Eric.\nDavid would lose 38 happiness units by sitting next to Frank.\nDavid would gain 60 happiness units by sitting next to George.\nDavid would lose 63 happiness units by sitting next to Mallory.\nEric would gain 23 happiness units by sitting next to Alice.\nEric would lose 69 happiness units by sitting next to Bob.\nEric would lose 33 happiness units by sitting next to Carol.\nEric would lose 47 happiness units by sitting next to David.\nEric would gain 75 happiness units by sitting next to Frank.\nEric would gain 82 happiness units by sitting next to George.\nEric would gain 13 happiness units by sitting next to Mallory.\nFrank would gain 77 happiness units by sitting next to Alice.\nFrank would gain 27 happiness units by sitting next to Bob.\nFrank would lose 87 happiness units by sitting next to Carol.\nFrank would gain 74 happiness units by sitting next to David.\nFrank would lose 41 happiness units by sitting next to Eric.\nFrank would lose 99 happiness units by sitting next to George.\nFrank would gain 26 happiness units by sitting next to Mallory.\nGeorge would lose 63 happiness units by sitting next to Alice.\nGeorge would lose 51 happiness units by sitting next to Bob.\nGeorge would lose 60 happiness units by sitting next to Carol.\nGeorge would gain 30 happiness units by sitting next to David.\nGeorge would lose 100 happiness units by sitting next to Eric.\nGeorge would lose 63 happiness units by sitting next to Frank.\nGeorge would gain 57 happiness units by sitting next to Mallory.\nMallory would lose 71 happiness units by sitting next to Alice.\nMallory would lose 28 happiness units by sitting next to Bob.\nMallory would lose 10 happiness units by sitting next to Carol.\nMallory would gain 44 happiness units by sitting next to David.\nMallory would gain 22 happiness units by sitting next to Eric.\nMallory would gain 79 happiness units by sitting next to Frank.\nMallory would lose 16 happiness units by sitting next to George.")
  (def tin "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol.")
  (part-1 (parse in))
  (part-2 (parse in))

  )
