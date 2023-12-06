(ns advent-2023.day-5
  (:require
    [advent.util :as util]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (into []
        (map (fn [l]
               (let [[n & r] (remove str/blank?
                                     (str/split l #": |\n"))]
                 {:transform (re-find #"^[a-z-]+" n)
                  :mappings (into []
                                  (comp (map (fn [l]
                                               (edn/read-string (str "[" l "]"))))
                                        (xf/sort))
                                  r)})))
        (str/split s #"(?m)^$")))


(defn transform [n {mpgs :mappings}]
  (or (some (fn [[drs srs l]]
              (when (and (<= srs n)
                         (< n (+ srs l)))
                (+ drs (- n srs))))
            mpgs)
      n))


(defn part-1 [in]
  (let [[{[seeds] :mappings} & xfs] (parse in)]
    (util/min (map (fn [seed]
                     (reduce transform
                             seed
                             xfs)))
              seeds)))


(defn contiguous-ranges [mpgs]
  (loop [i 0
         ret []
         [[dst src l :as n] & r :as mpgs] mpgs]
    (if-not n
      (conj ret [i i (- Long/MAX_VALUE i)])
      (cond
        (< i dst) (recur dst
                         (conj ret
                               [i i (- dst i)])
                         mpgs)
        (= i dst) (recur (+ dst l)
                         (conj ret n)
                         r)))))


(defn range-contains? [[start end] x]
  (and (<= start x)
       (< x end)))


(defn ranges-overlap? [[s1 e1] [s2 e2]]
  (< (max s1 s2)
     (min e1 e2)))


(defn child-ranges [[stack [start end]]]
  (let [{mpgs :mappings} (peek stack)
        stack' (pop stack)]
    (sequence (comp (drop-while (fn [[dst src rl]]
                                  (< (+ dst rl) start)))
                    (util/take-upto (fn [[dst src rl]]
                                      (range-contains? [dst (+ dst rl)]
                                                       end)))
                    (map (fn [[dst src rl]]
                           (let [[s e] [(max start dst)
                                        (min end (+ dst rl))]]
                             [stack'
                              [(+ src (- s dst))
                               (+ src (- e dst))]]))))
              mpgs)))


(defn parse-2 [s]
  (let [[seeds & r] (parse s)]
    {:seeds (into []
                  (comp cat
                        (xf/partition 2))
                  (:mappings seeds))
     :transforms (into []
                       (map (fn [m]
                              (update m :mappings contiguous-ranges)))
                       r)}))



(defn part-2 [in]
  (let [{xforms :transforms
         seeds :seeds} (parse-2 in)
        init (some (fn [[stack [start :as soil]]]
                     (and (empty? stack)
                          (some (fn [[s l]]
                                  (when (ranges-overlap? soil
                                                         [s (+ s l)])
                                    (max s start)))
                                seeds)))
                   (tree-seq (fn [[stack rng]]
                               (seq stack))
                             child-ranges
                             [xforms [0 Long/MAX_VALUE]]))]
    (reduce transform
            init
            xforms)))

(comment
  (def t1 "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

  (parse t1)

  (transform 13
             {:transform "seed-to-soil", :mappings [[50 98 2] [52 50 48]]}
             )

  (part-1 t1)
  
  (util/time-ms 1000 (part-2 in))

  )
