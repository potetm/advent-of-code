(ns advent-2024.day-8
  (:require
    [advent.util :as util]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn parse [s]
  (let [b (into []
                (map vec)
                (str/split-lines s))]
    {:antennas (transduce (keep (fn [p]
                                  (let [v (get-in b p)]
                                    (when (not= \. (get-in b p))
                                      [v p]))))
                          (completing
                            (fn [acc [v p]]
                              (update acc v (fnil conj #{}) p)))
                          {}
                          (util/rect-points b))
     :board b}))


(defn part-1 [{ants :antennas
               b :board}]
  (count (transduce (mapcat (fn [[freq pnts]]
                              (map vector
                                   (repeat freq)
                                   pnts
                                   (repeat pnts))))
                    (completing (fn [antinodes [freq [px py :as p] pnts]]
                                  (if (antinodes p)
                                    antinodes
                                    (into antinodes
                                          (comp (mapcat (fn [[x y]]
                                                          (let [∂x (- px x)
                                                                ∂y (- py y)]
                                                            [[(+ px ∂x)
                                                              (+ py ∂y)]
                                                             [(- x ∂x)
                                                              (- y ∂y)]])))
                                                (filter (partial util/on-board?* b)))
                                          (disj pnts p)))))
                    #{}
                    ants)))


(defn next-antinode [[[i j] [i' j']]]
  (let [∂x (- i i')
        ∂y (- j j')]
    [[i' j']
     [(- i' ∂x)
      (- j' ∂y)]]))


(defn antinodes [b p p']
  (concat (into []
                (comp (map second)
                      (take-while (partial util/on-board?* b)))
                (iterate next-antinode
                         [p p']))
          (into []
                (comp (map second)
                      (take-while (partial util/on-board?* b)))
                (iterate next-antinode
                         [p' p]))))


(defn part-2 [{ants :antennas
               b :board}]
  (count (transduce (mapcat (fn [[freq pnts]]
                              (map vector
                                   (repeat freq)
                                   pnts
                                   (repeat pnts))))
                    (completing (fn [ants [freq p pnts]]
                                  (into ants
                                        (mapcat (fn [p']
                                                  (antinodes b p p')))
                                        (disj pnts p))))
                    #{}
                    ants)))


(comment
  (def t "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............")
  (def in ".E..........m..0N.........f.......................\n........N........P0...............................\n.......j..................................F.......\n........1j............P........................C..\n...........................3..K......f..........E.\n...........V...y...0.....................F........\n1.......j.....P....y.N.......................F....\n....................m...................C.........\n..L......P....p..................w.m..............\n............E......p..AU........8......f..........\n..............C...............w....d..............\nj1...............E..........3.........f........w..\n.................p...A..........3.................\n.................3..p........KU...w..r..F.........\n7.........y........8.......................r......\n........y..u......K...............................\n...1..................8....C...K..................\n...........h.......................6..............\n......................U.........A.r..t........6...\n...........5.........8..c.........................\n.................U................t...............\n.....L...O...................t.............d......\n.........7........................................\n......L..H...c.....9....t.................6.......\n...........................c.M..................4.\n.....R..7...O.....................................\n.......................9......................d...\n..................................................\n.........L..9...R..........................6c.....\n..M.....T.5.................................d.....\n.......5OR...................T....................\n.......D......o.........v...................r.....\n...u....o.........5...............................\n.......WR.....Y...........................e...4...\nT............O......M..................4..a.......\n.Y...................M............................\n........W..D...............oh............e........\n.......7......Do...................A...e.......4..\n.W...Y..D........................h...v..........e.\n..........V.....9.l.......h.......a.........n..v..\n.......................H.....a2...................\n..................................................\n...V............Y....J..H2................vn......\n..............................H2.................n\n................V..........l...........k..........\n.T..u........................J...ak...............\n..................J.....l.........................\n.................l................................\n......u.........................................n.\n......................J..k............2...........")

  (part-1 (parse in))
  (part-2 (parse in))



  (set/difference (part-2 (parse t))
                  (into #{}
                        (mapcat val)
                        (:antennas (parse "##....#....#\n.#.#....0...\n..#.#0....#.\n..##...0....\n....0....#..\n.#...#A....#\n...#..#.....\n#....#.#....\n..#.....A...\n....#....A..\n.#........#.\n...#......##\n")))
                  )
  ;; y = mx + b


  (next-antinode [[3 7] [4 4]])
  (take 10 (iterate next-antinode
                    [[2 5] [1 8]]))
  (antinodes (:board (parse t))
             [3 7] [4 4]

             )
  (let [[x1 y1] [2 5]
        [x2 y2] [1 8]
        ∂x (- x1 x2)
        ∂y (- y1 y2)]
    [[(+ x1 ∂x)
      (+ y1 ∂y)]
     [(- x2 ∂x)
      (- y2 ∂y)]])

  (let [{ants :antennas
         b :board} (parse t)]
    (into []
          (mapcat (fn [[freq pnts]]
                    (map vector
                         (repeat freq)
                         pnts
                         (repeat pnts))))

          ants))

  {\A #{[0 0]
        [1 0]}}

  ;; ellis kenyo solution (complex numbers)
  )