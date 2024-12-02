(ns advent-2023.day-6
  (:require [advent.util :as util]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse [s]
  (into {}
        (map (fn [l]
               (let [[label nums] (str/split l #":")]
                 [label (edn/read-string (str "[" nums "]"))])))
        (str/split-lines s)))


(def initial
  {:speed 0
   :charge-rate 1})


(defn race-distance [{s :speed
                      cr :charge-rate} charge-time race-time]
  (+ s
     (* cr
        charge-time
        (- race-time charge-time))))


(defn part-1 [in]
  (let [{t "Time"
         d "Distance"} (parse in)
        s initial]
    (util/product (map (fn [[t d]]
                         (count (filter (fn [t']
                                          (< d (race-distance s t' t)))
                                        (range t)))))
                  (map vector
                       t
                       d))))


(defn parse-2 [in]
  (let [{t "Time"
         d "Distance"} (parse in)]
    {"Time" (BigInteger/valueOf (parse-long (apply str t)))
     "Distance" (BigInteger/valueOf (parse-long (apply str d)))}))


(defn quadratic-eq [a b c]
  [(/ (-' (.negate b)
          (.sqrt (.subtract (.pow b 2)
                            (-> (.multiply a c)
                                (.multiply (BigInteger/valueOf 4))))))
      (*' 2 a))
   (/ (+' (.negate b)
          (.sqrt (.subtract (.pow b 2)
                            (-> (.multiply a c)
                                (.multiply (BigInteger/valueOf 4))))))
      (*' 2 a))])


(defn part-2 [in]
  (let [{t "Time"
         d "Distance"} (parse-2 in)
        [s e] (quadratic-eq (BigInteger/valueOf 1)
                            (.negate t)
                            d)]
    (inc (- (Math/floor e)
            (Math/ceil s)))))


(comment
  (def t1 "Time:      7  15   30\nDistance:  9  40  200")

  (into []
        (map double) (quadratic-eq 1
                                   (BigInteger/valueOf 71530)
                                   (BigInteger/valueOf 940200)))

  (parse-2 t1)
  (parse-2 "Time:        48     87     69     81\nDistance:   255   1288   1117   1623")
  (util/sum' (quadratic-eq (BigInteger/valueOf -1)
                           (BigInteger/valueOf 48876981)
                           (BigInteger/valueOf 255128811171623)))
  (parse t1)
  (part-2  "Time:        48     87     69     81\nDistance:   255   1288   1117   1623")
  )
