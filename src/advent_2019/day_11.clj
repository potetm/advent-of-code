(ns advent-2019.day-11
  (:require [advent-2019.day-5 :as d5]
            [advent-2019.day-9 :as d9]))

(def parse d9/parse)

(defn color
  ([{b :board
     p :pos}]
   (color b p))
  ([board pos]
   (get-in board pos 0)))

(def turn
  (let [face [:north :east :south :west]]
    {:right (into {}
                  (map vector
                       face
                       (rest (cycle face))))
     :left (into {}
                 (map vector
                      (reverse face)
                      (rest (cycle (reverse face)))))}))

(defn move [[x y] facing]
  (case facing
    :north [x (dec y)]
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]))

(def init
  {:board {}
   :pos [0 0]
   :facing :north
   :next-action :color
   :pos-changed #{}})

(defn step [{b :board
             p :pos
             f :facing
             pa :next-action
             pchg :pos-changed :as s}
            arg]

  (case pa
    :color (assoc s
             :next-action :turn
             :board (assoc-in b p arg)
             :pos-changed (conj pchg p))
    :turn (let [f (case arg
                    0 (get-in turn [:left f])
                    1 (get-in turn [:right f]))]

            (assoc s
              :next-action :color
              :pos (move p f)
              :facing f))))

(defn run [paint prog]
  (sequence (comp (take-while some?)
                  (map first))
            (iterate (fn [[paint prog]]
                       (let [{[out] :out :as s} (first (filter (comp seq :out)
                                                               prog))]
                         (when out
                           (let [paint' (step paint out)]
                             [paint'
                              (d5/run (assoc s
                                        :out []
                                        :in (repeat (color paint'))))]))))
                     [init
                      (d5/run prog
                              (repeat (color paint)))])))

(defn part-1 [in]
  (count (:pos-changed (last (run init
                                  (parse in))))))

(defn render [{b :board}]
  (let [minx (apply min (keys b))
        miny (apply min (mapcat keys (vals b)))
        maxx (apply max (keys b))
        maxy (apply max (mapcat keys (vals b)))
        xs (range minx (inc maxx))]
    (doseq [y (range miny (inc maxy))]
      (println (apply str
                      (map (fn [x]
                             (get {0 \.
                                   1 \#}
                                  (color b [x y])))
                           xs))))))

(defn part-2 [in]
  (render (last (run (assoc init
                       :board {0 {0 1}})
                     (parse in)))))

(comment

  (part-1 "3,8,1005,8,284,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,72,1006,0,24,1,1106,12,10,1006,0,96,1,1008,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,108,1006,0,54,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,134,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,60,1006,0,64,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,183,1006,0,6,1006,0,62,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,211,1,108,0,10,2,1002,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,242,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,263,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,606,104,0,104,1,21101,0,666526126996,1,21101,301,0,0,1105,1,405,21101,846138811028,0,1,21101,312,0,0,1106,0,405,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,248129978391,1,21101,359,0,0,1105,1,405,21101,97751403560,0,1,21102,1,370,0,1106,0,405,3,10,104,0,104,0,3,10,104,0,104,0,21101,988753585000,0,1,21101,393,0,0,1105,1,405,21102,867961709324,1,1,21102,404,1,0,1106,0,405,99,109,2,22102,1,-1,1,21102,40,1,2,21101,436,0,3,21102,1,426,0,1105,1,469,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,431,432,447,4,0,1001,431,1,431,108,4,431,10,1006,10,463,1102,0,1,431,109,-2,2106,0,0,0,109,4,1202,-1,1,468,1207,-3,0,10,1006,10,486,21102,1,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,505,0,0,1106,0,510,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,533,2207,-4,-2,10,1006,10,533,22101,0,-4,-4,1105,1,601,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,552,0,1105,1,510,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,571,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,593,21202,-1,1,1,21102,1,593,0,106,0,468,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0")
  )
