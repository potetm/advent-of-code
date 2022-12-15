(ns advent-2022.day-12
  (:require
    [advent.util :as util]
    [clojure.data.priority-map :as pm]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn points-for [b pred]
  (into []
        (filter (fn [p]
                  (pred (get-in b p))))
        (util/rect-points b)))


(defn parse [s]
  (let [b (util/transpose (into []
                                (map vec)
                                (str/split-lines s)))]
    {:start (peek (points-for b #{\S}))
     :dest (peek (points-for b #{\E}))
     :board b}))


(defn pb [board
          path]
  (let [path (set path)]
    (println (xf/str (comp (partition-by second)
                           (map (fn [row]
                                  (map (fn [p]
                                         (if (path p)
                                           \.
                                           (get-in board p)))
                                       row)))
                           (interpose [\newline])
                           cat)
                     (util/rect-points board)))))


(defn priority [path d]
  [(count path)
   (util/taxicab-distance (peek path)
                          d)])


(defn step [b d [pm seen]]
  (let [path (key (peek pm))
        me (peek path)
        v (get-in b me)
        nbrs (into []
                   (comp (remove seen)
                         (filter (fn [pnt]
                                   (let [v (if (= v \S)
                                             \a
                                             v)
                                         v' (get-in b pnt)
                                         v' (if (= v' \E)
                                              \z
                                              v')]
                                     (<= (int v')
                                         (inc (int v)))))))
                   (util/taxi-neighbors b me))]
    (if (= \E v)
      (reduced path)
      [(into (pop pm)
             (map (fn [pnt]
                    (let [path' (conj path pnt)]
                      [path' (priority path' d)])))
             nbrs)
       (into seen nbrs)])))


(defn init-pm [{s :start
                d :dest}]
  (pm/priority-map [s] (priority [s] d)))


(defn shortest-path
  ([{b :board
     d :dest :as state}]
   (loop [pm+seen [(init-pm state) #{}]]
     (let [ret (step b d pm+seen)]
       (if (reduced? ret)
         @ret
         (when (seq (first ret))
           (recur ret)))))))


(defn part-1 [in]
  (dec (count (shortest-path in))))


(defn part-2 [{b :board :as in}]
  (util/min (comp (map #(assoc in :start %))
                  (map part-1)
                  (remove #{-1}))
            (points-for b #{\a \S})))


(comment
  (def t "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
  (def in "abaaaaaccccccccccccccccccaaaaaaaaaaaaaccccaaaaaaaccccccccccccccccccccccccccccaaaaaa\nabaaaaaaccaaaacccccccccccaaaaaaaaacaaaacaaaaaaaaaacccccccccccccccccccccccccccaaaaaa\nabaaaaaacaaaaaccccccccccaaaaaaaaaaaaaaacaaaaaaaaaacccccccccccccaacccccccccccccaaaaa\nabaaaaaacaaaaaacccccccccaaaaaaaaaaaaaaccaaacaaaccccccccccccccccaacccccccccccccccaaa\nabccaaaccaaaaaacccaaaaccaaaaaaaaaaaaaccccaacaaacccccccccaacaccccacccccccccccccccaaa\nabcccccccaaaaaccccaaaacccccaaaaacccaaaccaaaaaaccccccccccaaaaccccccccccccccccccccaac\nabcccccccccaaaccccaaaacccccaaaaacccccccccaaaaaccccccccccklllllccccccccccccccccccccc\nabcccccccccccccccccaaccccccccaaccccccccaaaaaaaccccccccckklllllllcccccddccccaacccccc\nabaccccccccccccccccccccccccccaaccccccccaaaaaaaaccccccckkkklslllllcccddddddaaacccccc\nabacccccccccccccccccccccccccccccccaaaccaaaaaaaaccccccckkkssssslllllcddddddddacccccc\nabaccccccccccccccccccccccccccccccccaaaaccaaacaccccccckkksssssssslllmmmmmdddddaacccc\nabcccccccccccccccaaacccccccccccccaaaaaaccaacccccccccckkkssssusssslmmmmmmmdddddacccc\nabcccccccaaccccaaaaacccccccccccccaaaaaccccccaaaaaccckkkrssuuuussssqmmmmmmmmdddccccc\nabcccccccaaccccaaaaaacccccccaaccccaaaaacccccaaaaacckkkkrruuuuuussqqqqqqmmmmdddccccc\nabccccaaaaaaaacaaaaaacccccccaaaaccaaccaccccaaaaaacjkkkrrruuuxuuusqqqqqqqmmmmeeccccc\nabcaaaaaaaaaaacaaaaaccccccaaaaaacccccaaccccaaaaajjjjrrrrruuuxxuvvvvvvvqqqmmmeeccccc\nabcaacccaaaaccccaaaaaaacccaaaaacccacaaaccccaaaajjjjrrrrruuuxxxxvvvvvvvqqqmmeeeccccc\nabaaaaccaaaaacccccccaaaccccaaaaacaaaaaaaacccaajjjjrrrrtuuuuxxxyvyyyvvvqqqnneeeccccc\nabaaaaaaaaaaacccaaaaaaaccccaacaacaaaaaaaacccccjjjrrrttttuxxxxxyyyyyvvvqqnnneeeccccc\nabaaaaaaaccaacccaaaaaaaaacccccccccaaaaaaccccccjjjrrrtttxxxxxxxyyyyyvvvqqnnneeeccccc\nSbaaaaaacccccccccaaaaaaaaaccccccccaaaaacccccccjjjrrrtttxxxEzzzzyyyvvrrrnnneeecccccc\nabaaaaacccccccccccaaaaaaacccccccccaaaaaaccccccjjjqqqtttxxxxxyyyyyvvvrrrnnneeecccccc\nabaaacccccccccccaaaaaaaccaaccccccccccaaccaaaaajjjqqqttttxxxxyyyyyyvvrrrnnneeecccccc\nabaaacccccccccccaaaaaaaccaaacaaacccccccccaaaaajjjjqqqtttttxxyywyyyywvrrnnnfeecccccc\nabcaaacccccccaaaaaaaaaaacaaaaaaaccccccccaaaaaaciiiiqqqqtttxwyywwyywwwrrrnnfffcccccc\nabcccccccccccaaaaaaaaaaccaaaaaacccccccccaaaaaacciiiiqqqqttwwywwwwwwwwrrrnnfffcccccc\nabccccccccccccaaaaaacccaaaaaaaacccccccccaaaaaaccciiiiqqqttwwwwwswwwwrrrrnnfffcccccc\nabccccccccccccaaaaaacccaaaaaaaaacccccccccaaacccccciiiqqqtswwwwssssrrrrrroofffcccccc\nabccccccaaaaacaaaaaacccaaaaaaaaaaccccccccccccccccciiiqqqssswsssssssrrrrooofffaccccc\nabccccccaaaaacaaccaaccccccaaacaaacccccccccccccccccciiiqqssssssspoorrrooooofffaacccc\nabcccccaaaaaacccccccccccccaaacccccccccccccccccccccciiiqppssssspppooooooooffffaacccc\nabcccccaaaaaacccccccccccccaacccccccccccccccccccccccciipppppppppppoooooooffffaaccccc\nabcccccaaaaaaccccccccccccccccccccccccccccccccccccccciihppppppppgggggggggfffaaaccccc\nabccccccaaacccccccccccccccccccccccaccccccccccccccccchhhhpppppphggggggggggfaaaaccccc\nabaaaccccccccccccccccccccccaccccaaacccccccccccccccccchhhhhhhhhhgggggggggcaacccccccc\nabaaccaaaccaccccccccccccccaaacccaaacaacccaaaaacccccccchhhhhhhhhgaaccccccccccccccccc\nabaaacaaacaacccccccccaaaccaaaacaaaaaaaaccaaaaaccccccccchhhhhhaaaaacccccccccccccccca\nabaaaccaaaaaccccccccccaaacaaaaaaaacaaaaccaaaaaaccccccccccaaacccaaaacccccccccccaccca\nabcccaaaaaaccccccccccaaaaaaaaaaaaacaaaaccaaaaaaccccccccccaaaccccaaaccccccccccaaaaaa\nabcccaaaaaaaacccccccaaaaaaaaaaaaaaaaaccccaaaaaacccccccccccccccccccccccccccccccaaaaa\nabcccaacaaaaaccccccaaaaaaaaaaaaaaaaaaacccccaacccccccccccccccccccccccccccccccccaaaaa")




  (time (part-1 (parse in)))
  (time (part-2 (parse in)))

  )
