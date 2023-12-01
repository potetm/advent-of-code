(ns advent-2022.day-15
  (:require [advent.util :as util]
            [clojure.set :as set]
            [net.cgrand.xforms :as xf]))


(defn parse [s]
  (into []
        (comp (map next)
              (map (partial map parse-long))
              (partition-all 2)
              (map (partial zipmap [:sensor :beacon])))
        (re-seq #"x=(-?\d+), y=(-?\d+)"
                s)))


(defn delta [{s :sensor
              b :beacon :as in}]
  (assoc in
    :delta (abs (util/taxicab-distance s b))))


(defn pnts-intersecting-row [row
                             {[x y :as s] :sensor
                              d :delta}]
  (when (<= (- y d)
            row
            (+ y d))
    (for [x (range (- x d)
                   (inc (+ x d)))
          :let [pnt [x row]]
          :when (<= (util/taxicab-distance pnt s)
                    d)]
      pnt)))


(defn part-1 [in row]
  (let [bs (into #{}
                 (mapcat (juxt :beacon :sensor))
                 in)]
    (xf/count (comp (map delta)
                    (mapcat (partial pnts-intersecting-row row))
                    (distinct)
                    (remove bs))
              in)))


(defn perimeter-pnts [{[x y] :sensor
                       d :delta}]
  (let [d (inc d)
        xmx (+ x d)
        xmn (- x d)
        ymx (+ y d)
        ymn (- y d)]
    (eduction (mapcat (fn [[xrng yrng]]
                        (map vector
                             (apply range xrng)
                             (apply range yrng))))
              [[[x xmx 1]
                [ymn y 1]]
               [[xmx x -1]
                [y ymx 1]]
               [[x xmn -1]
                [ymx y -1]]
               [[xmn x 1]
                [y ymn -1]]])))


(defn part-2 [in mx]
  (let [in (map delta in)]
    (first (into []
                 (comp (mapcat perimeter-pnts)
                       (filter (fn [[x y]]
                                 (and (<= 0 x mx)
                                      (<= 0 y mx))))
                       (distinct)
                       (filter (fn [pnt]
                                 (not-any? (fn [{s :sensor
                                                 d :delta}]
                                             (<= (util/taxicab-distance pnt s)
                                                 d))
                                           in)))
                       (map (fn [[x y]]
                              (+ (* x 4000000)
                                 y))))
                 in))))


(comment
  (def t "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")
  (def in "Sensor at x=2300471, y=2016823: closest beacon is at x=2687171, y=2822745\nSensor at x=1315114, y=37295: closest beacon is at x=1671413, y=43557\nSensor at x=1039523, y=3061589: closest beacon is at x=1570410, y=3710085\nSensor at x=214540, y=3768792: closest beacon is at x=-355567, y=3900317\nSensor at x=1641345, y=3524291: closest beacon is at x=1570410, y=3710085\nSensor at x=1016825, y=1450262: closest beacon is at x=745731, y=2000000\nSensor at x=2768110, y=3703050: closest beacon is at x=3133588, y=3984216\nSensor at x=2213658, y=3522463: closest beacon is at x=1570410, y=3710085\nSensor at x=3842967, y=3381135: closest beacon is at x=3839159, y=3421933\nSensor at x=3952516, y=2683159: closest beacon is at x=3213800, y=2708360\nSensor at x=172892, y=369117: closest beacon is at x=-228964, y=1438805\nSensor at x=3999720, y=3498306: closest beacon is at x=3839159, y=3421933\nSensor at x=1596187, y=307084: closest beacon is at x=1671413, y=43557\nSensor at x=3863253, y=3406760: closest beacon is at x=3839159, y=3421933\nSensor at x=3927553, y=3450758: closest beacon is at x=3839159, y=3421933\nSensor at x=2774120, y=3228484: closest beacon is at x=2687171, y=2822745\nSensor at x=3897140, y=3418751: closest beacon is at x=3839159, y=3421933\nSensor at x=1880329, y=2843697: closest beacon is at x=2687171, y=2822745\nSensor at x=33790, y=3243415: closest beacon is at x=-355567, y=3900317\nSensor at x=438583, y=2647769: closest beacon is at x=745731, y=2000000\nSensor at x=1540347, y=3177380: closest beacon is at x=1570410, y=3710085\nSensor at x=3120086, y=3997791: closest beacon is at x=3133588, y=3984216\nSensor at x=3428967, y=3105227: closest beacon is at x=3213800, y=2708360\nSensor at x=2898335, y=1037911: closest beacon is at x=3213800, y=2708360\nSensor at x=3456260, y=3578627: closest beacon is at x=3839159, y=3421933\nSensor at x=1859971, y=3999725: closest beacon is at x=1570410, y=3710085\nSensor at x=3147730, y=3999322: closest beacon is at x=3133588, y=3984216\nSensor at x=3920847, y=71575: closest beacon is at x=3826138, y=-255533\nSensor at x=956723, y=3999438: closest beacon is at x=1570410, y=3710085\nSensor at x=1193760, y=3758205: closest beacon is at x=1570410, y=3710085\nSensor at x=3999446, y=1929369: closest beacon is at x=3213800, y=2708360\nSensor at x=1434466, y=2254087: closest beacon is at x=745731, y=2000000\nSensor at x=200365, y=1856636: closest beacon is at x=745731, y=2000000\nSensor at x=1859710, y=31159: closest beacon is at x=1671413, y=43557\nSensor at x=3712613, y=3930105: closest beacon is at x=3133588, y=3984216\nSensor at x=1660185, y=2900: closest beacon is at x=1671413, y=43557\nSensor at x=1497065, y=93501: closest beacon is at x=1671413, y=43557\nSensor at x=3832823, y=3346266: closest beacon is at x=3839159, y=3421933")


  (part-1 (parse in)
          2000000)

  (part-2 (parse in)
          4000000)
  (count (perimeter-pnts (delta (first (parse in)))))

  (map delta (parse in))

  )
