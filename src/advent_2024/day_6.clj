(ns advent-2024.day-6
  (:require [advent.util :as util]
            [clojure.string :as str]
            [net.cgrand.xforms :as xf]))


(defn parse [s]
  (let [b (into []
                (map vec)
                (str/split-lines s))
        {pos :pos :as g} (some (fn [p]
                                 (when (= \^ (get-in b p))
                                   {:facing :north
                                    :pos p}))
                               (util/rect-points b))]
    {:seen #{}
     :guard g
     :starting-pos pos
     :board (assoc-in b pos \.)}))


(defn mv [{f :facing
           [x y] :pos :as g}]
  (assoc g
    :pos (case f
           :north [(dec x) y]
           :east [x (inc y)]
           :south [(inc x) y]
           :west [x (dec y)])))


(defn turn [{f :facing :as g}]
  (assoc g
    :facing (case f
              :north :east
              :east :south
              :south :west
              :west :north)))


(defn step [{g :guard
             b :board
             seen :seen :as s}]
  (let [{p' :pos :as g'} (mv g)
        g' (case (get-in b p')
             \# (turn g)
             \. g'
             ;; off the board. nil signals termination.
             nil)]
    (assoc s
      :seen (conj seen g)
      :guard g')))


(defn prb [{{f :facing
             p :pos} :guard
            b :board}]
  (when b
    (let [b (assoc-in b p
                      (case f
                        :north \^
                        :east \>
                        :south \v
                        :west \<))]
      (doseq [r b]
        (doseq [c r]
          (print c))
        (print \newline)))))


(defn part-1 [s]
  (count (into #{}
               (comp (map #(get-in % [:guard :pos]))
                     (take-while some?))
               (iterate step
                        s))))


(defn cycle? [{s :seen
               g :guard}]
  (contains? s g))


(defn part-2 [{sp :starting-pos
               b :board :as s}]
  (count (into []
               (comp (filter (fn [p]
                               (and (not= \# (get-in b p))
                                    (not= sp p))))
                     (map (fn [[x y :as p]]
                            (peek (into []
                                        (comp (util/take-upto (fn [{g :guard :as s}]
                                                                (or (nil? g)
                                                                    (cycle? s))))
                                              xf/last)
                                        (iterate step
                                                 (-> s
                                                     (assoc-in [:board x y] \#)
                                                     (assoc :obstacle p)))))))
                     (filter cycle?))
               (util/rect-points b))))


(comment
  (def t "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#...")
  (def in )

  (part-1 (parse in))
  (time (part-2 (parse in)))
  (let [{sp :starting-pos
         b :board :as s} (parse t)]
    (into []
          (filter (fn [p]
                    (and (not= \# (get-in b p))
                         (not= sp p))))
          (util/rect-points b)))

  (util/rect-points (:board (parse t)
                      ))

  (prb (step))

  ;; 1704 too high

  )
