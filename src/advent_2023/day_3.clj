(ns advent-2023.day-3
  (:refer-clojure :exclude [symbol?])
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (str/split-lines s))


(defn re-locs [re s]
  (let [m (re-matcher re s)]
    (loop [ret []]
      (if (.find m)
        (recur (conj ret [(.start m)
                          (.end m)
                          (.group m)]))
        ret))))


(defn touching? [[s1 e1] [s2 e2]]
  (or (<= s2 s1 e2)
      (<= s2 e1 e2)))


(defn part-1 [in]
  (let [in (parse in)
        empty [(str/join (repeat (count (peek in))
                                 "."))]]
    (util/sum (comp cat
                    (xf/partition 3 1)
                    (mapcat (fn [[_ c :as lns]]
                              (keep (fn [[s e n :as digit]]
                                      (when (some (fn [l]
                                                    (some #(touching? % digit)
                                                          (re-locs #"[^.\d]" l)))
                                                  lns)
                                        (parse-long n)))
                                    (re-locs #"\d+" c)))))
              [empty in empty])))


(defn part-2 [in]
  (let [in (parse in)
        empty [(str/join (repeat (count (peek in))
                                 "."))]]
    (util/sum (comp cat
                    (xf/partition 3 1)
                    (mapcat (fn [[_ c :as lns]]
                              (let [digits (mapcat (partial re-locs #"\d+")
                                                   lns)]
                                (into []
                                      (keep (fn [[s1 e1 _m :as ast]]
                                              (let [touching (filter (partial touching? ast)
                                                                     digits)]
                                                (when (= 2 (count touching))
                                                  (util/product (map (comp parse-long peek))
                                                                touching)))))
                                      (re-locs #"\*" c))))))
              [empty in empty])))

(comment
  (def t1 "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")
  (part-1 t1)
  (part-2 t1)



  (take (drop -1
              (parse t1)))
  (part-number? (parse t1)
                0
                0
                3))
