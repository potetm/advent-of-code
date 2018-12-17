(ns advent-2018.day-11)

(defn hundreds-digit [n]
  (mod (int (/ n
               100))
       10))

(defn power-level [serial-number [x y]]
  (let [rack-id (+ 10 x)]
    (- (hundreds-digit (* rack-id
                          (+ (* y rack-id)
                             serial-number)))
       5)))

(defn grid-power [serial-number [x y :as top-left]]
  (reduce +
          (map #(power-level serial-number %)
               (for [i (range x (+ x 3))
                     j (range y (+ y 3))]
                 [i j]))))

(defn top-lefts [size]
  (for [i (range 1 (- 300 (- size 2)))
        j (range 1 (- 300 (- size 2)))]
    [i j]))

(defn part-1 [serial-number]
  (apply max-key second
         (map (fn [tl]
                [tl (grid-power serial-number
                                tl)])
              (top-lefts 3))))

(defn summed-area-table [serial-number]
  (reduce (fn [g [x y :as pnt]]
            (assoc-in g
                      pnt
                      (- (+ (power-level serial-number pnt)
                            (get-in g [(dec x) y] 0)
                            (get-in g [x (dec y)] 0))
                         (get-in g [(dec x) (dec y)] 0))))
          (vec (repeat 301 (vec (repeat 301 0))))
          (for [i (range 1 301)
                j (range 1 301)]
            [i j])))

(defn grid-power-1 [sat [x y] size]
  (let [size (dec size)
        a (get-in sat [(dec x) (dec y)])
        b (get-in sat [(dec x) (+ y size)])
        c (get-in sat [(+ size x) (dec y)])
        d (get-in sat [(+ size x) (+ size y)])]
    (- (+ a d)
       b
       c)))

(defn part-2 [sn]
  (let [sat (summed-area-table sn)]
    (apply max-key first
           (mapcat (fn [size]
                     (map (fn [pnt]
                            [(grid-power-1 sat
                                           pnt
                                           size)
                             pnt
                             size])
                          (top-lefts size)))
                   (range 1 301)))))

(defn part-2-faster [sn]
  (let [sat (summed-area-table sn)]
    (reduce (partial max-key first)
            (apply concat
                   (pmap (fn [size]
                           (into []
                                 (map (fn [pnt]
                                        [(grid-power-1 sat
                                                       pnt
                                                       size)
                                         pnt
                                         size]))
                                 (top-lefts size)))
                         (range 1 301))))))

(comment
  (power-level 8 [3 5])
  (power-level 57 [122, 79])
  (power-level 39 [217, 196])
  (power-level 71 [101, 153])

  (grid-power 18 [33, 45])
  (grid-power 42 [21, 61])

  (part-1 5791)
  (time (part-2 5791))
  (time (part-2-faster 5791))

  )
