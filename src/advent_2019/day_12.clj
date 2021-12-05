(ns advent-2019.day-12
  (:require [advent.util :as common]))

(defn parse [s]
  (map (fn [[_ & args]]
         {:vel (zipmap [:x :y :z]
                       (repeat 0))
          :pos (zipmap [:x :y :z]
                       (map #(Long/parseLong %)
                            args))})
       (re-seq #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"
               s)))

(defn velocity [m m' coord]
  (let [p (get-in m [:pos coord])
        p' (get-in m' [:pos coord])
        v (get-in m [:vel coord])]
    (+ v (cond
           (= p p') 0
           (< p p') 1
           :else -1))))

(defn position [{v :vel
                 p :pos :as m}]
  (assoc m
    :pos (merge-with + v p)))

(defn step [ms]
  (into []
        (comp (map (fn [m]
                     (reduce (fn [{v :vel :as m} m']
                               (assoc m
                                 :vel (into {}
                                            (map (fn [c]
                                                   [c (velocity m m' c)]))
                                            (keys v))))
                             m
                             ms)))
              (map position))
        ms))

(defn sum [coll]
  (reduce + 0 coll))

(defn energy [ms]
  (sum (map (fn [{p :pos
                  v :vel}]
              (* (sum (map #(Math/abs ^long %)
                           (vals p)))
                 (sum (map #(Math/abs ^long %)
                           (vals v)))))
            ms)))

(defn part-1 [in n-steps]
  (energy (nth (iterate step (parse in))
               n-steps)))

(defn cycle-length [ms coord]
  (ffirst (into []
                (comp (map-indexed vector)
                      (common/duplicates-by second)
                      (take 1))
                (iterate step
                         (map (fn [m]
                                ;; Only want to run one coord at a time.
                                (-> m
                                    (update :pos select-keys [coord])
                                    (update :vel select-keys [coord])))
                              ms)))))

(defn gcd [x y]
  (loop [x (Math/abs ^long x)
         y (Math/abs ^long y)]
    (if (zero? y)
      x
      (recur y (mod x y)))))

(defn lcm [x y]
  (Math/abs ^long
            (/ (* x y)
               (gcd x y))))

(defn part-2 [in]
  (let [ms (parse in)
        x (cycle-length ms :x)
        y (cycle-length ms :y)
        z (cycle-length ms :z)]
    (reduce lcm [x y z])))

(comment
  (part-1 "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
          10)
  (cycle-length (parse "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
                :z)

  (time (part-2 "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"))

  (part-1 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"
          100)
  (time (part-2 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"))

  (part-1 "<x=-8, y=-9, z=-7>\n<x=-5, y=2, z=-1>\n<x=11, y=8, z=-14>\n<x=1, y=-4, z=-11>"
          1000)

  (time (part-2 "<x=-8, y=-9, z=-7>\n<x=-5, y=2, z=-1>\n<x=11, y=8, z=-14>\n<x=1, y=-4, z=-11>")))
