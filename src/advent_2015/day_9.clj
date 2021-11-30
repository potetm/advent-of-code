(ns advent-2015.day-9
  (:require
    [advent.util :as util]
    [clojure.string :as str]))

(defn parse [in]
  (transduce (mapcat (fn [l]
                       (let [[src _to dest _= weight]
                             (str/split l #"\s+")
                             weight (Long/parseLong weight)]
                         [[src dest {:weight weight}]
                          [dest src {:weight weight}]])))
             (completing (fn [graph [src dest attrs]]
                           (update-in graph [src dest] merge attrs)))
             {}
             (str/split-lines in)))


(defn remove-node [graph node]
  (into {}
        (map (fn [[src paths]]
               [src (dissoc paths node)]))
        (dissoc graph node)))


(defn all-paths
  ([graph]
   (mapcat (partial all-paths graph)
           (keys graph)))
  ([graph src]
   (mapcat (fn [[dest {w :weight}]]
             (if-some [ap (seq (all-paths (remove-node graph src)
                                          dest))]
               (map (fn [{p :path
                          w' :weight}]
                      {:path (into [src] p)
                       :weight (+ w w')})
                    ap)
               [{:path [src dest]
                 :weight w}]))
           (get graph src))))


(defn part-1 [in]
  (util/min (map :weight
                 (all-paths in))))

(defn part-2 [in]
  (util/max (map :weight
                 (all-paths in))))


(comment
  (def input "AlphaCentauri to Snowdin = 66\nAlphaCentauri to Tambi = 28\nAlphaCentauri to Faerun = 60\nAlphaCentauri to Norrath = 34\nAlphaCentauri to Straylight = 34\nAlphaCentauri to Tristram = 3\nAlphaCentauri to Arbre = 108\nSnowdin to Tambi = 22\nSnowdin to Faerun = 12\nSnowdin to Norrath = 91\nSnowdin to Straylight = 121\nSnowdin to Tristram = 111\nSnowdin to Arbre = 71\nTambi to Faerun = 39\nTambi to Norrath = 113\nTambi to Straylight = 130\nTambi to Tristram = 35\nTambi to Arbre = 40\nFaerun to Norrath = 63\nFaerun to Straylight = 21\nFaerun to Tristram = 57\nFaerun to Arbre = 83\nNorrath to Straylight = 9\nNorrath to Tristram = 50\nNorrath to Arbre = 60\nStraylight to Tristram = 27\nStraylight to Arbre = 81\nTristram to Arbre = 90")
  (part-1 (parse "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"))
  (part-1 (parse input))
  (part-2 (parse input)))
