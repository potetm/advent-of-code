(ns advent.util
  (:refer-clojure :exclude [min max])
  (:require
    [clojure.core :as cc]
    [clojure.math :as math]
    [clojure.string :as str]))

(defn min
  ([coll]
   (when (seq coll)
     (reduce cc/min
             coll)))
  ([xf coll]
   (when (seq coll)
     (transduce xf
                (completing cc/min)
                Double/POSITIVE_INFINITY
                coll))))


(defn max
  ([coll]
   (when (seq coll)
     (reduce cc/max
             coll)))
  ([xf coll]
   (when (seq coll)
     (transduce xf
                (completing cc/max)
                Double/NEGATIVE_INFINITY
                coll))))


(defn min-by [k coll]
  (when (seq coll)
    (reduce (partial cc/min-key k)
            coll)))


(defn max-by [k coll]
  (when (seq coll)
    (reduce (partial cc/max-key k)
            coll)))


(defn sum
  ([coll]
   (reduce + 0 coll))
  ([xf coll]
   (transduce xf
              +
              0
              coll)))


(defn sum'
  ([coll]
   (reduce +' 0 coll))
  ([xf coll]
   (transduce xf
              +'
              0
              coll)))


(defn product
  ([coll]
   (reduce * 1 coll))
  ([xf coll]
   (transduce xf
              *
              1
              coll)))


(defn product'
  ([coll]
   (reduce *' 1 coll))
  ([xf coll]
   (transduce xf
              *'
              1
              coll)))


(defn transpose [matrix]
  (when (seq matrix)
    (apply mapv
           vector
           matrix)))


(defn row [matrix col]
  (when (seq matrix)
    (into []
          (map #(get % col))
          matrix)))


(defn trim-to-nil [s]
  (let [s' (str/trim s)]
    (when-not (= s' "")
      s')))


(defn assoc-str [^String s idx v]
  (.toString (doto (StringBuilder. s)
               (.setCharAt idx v))))


(defmacro time-ns [n & body]
  `(let [s# (System/nanoTime)]
     (dotimes [_# ~n]
       ~@body)
     (println "Average:"
              (double (/ (- (System/nanoTime)
                            s#)
                         ~n))
              "ns")))


(defmacro time-ms [n & body]
  `(let [s# (System/currentTimeMillis)]
     (dotimes [_# ~n]
       ~@body)
     (println "Average:"
              (double (/ (- (System/currentTimeMillis)
                            s#)
                         ~n))
              "ms")))


(defn duplicates
  ([]
   (fn [rf]
     (let [seen (volatile! {})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (case (get @seen input)
            :seen (do (vswap! seen assoc input :emitted)
                      (rf result input))
            :emitted result
            (do (vswap! seen assoc input :seen)
                result)))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (case (get seen f)
                         :seen (cons f (step (rest s)
                                             (assoc seen f :emitted)))
                         :emitted (recur (rest s) seen)
                         (recur (rest s) (assoc seen f :seen)))))
                   xs seen)))]
     (step coll {}))))


(defn duplicates-by
  ([f]
   (fn [rf]
     (let [seen (volatile! {})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [i (f input)]
            (case (get @seen i)
              :seen (do (vswap! seen assoc i :emitted)
                        (rf result input))
              :emitted result
              (do (vswap! seen assoc i :seen)
                  result))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [v (f x)]
                         (case (get seen v)
                           :seen (cons x (step (rest s)
                                               (assoc seen v :emitted)))
                           :emitted (recur (rest s) seen)
                           (recur (rest s) (assoc seen v :seen))))))
                   xs seen)))]
     (step coll {}))))


;; from https://github.com/weavejester/medley/blob/master/src/medley/core.cljc
(defn take-upto
  "Returns a lazy sequence of successive items from coll up to and including
  the first item for which `(pred item)` returns true."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result x]
        (let [result (rf result x)]
          (if (pred x)
            (ensure-reduced result)
            result))))))
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [x (first s)]
         (cons x (if-not (pred x) (take-upto pred (rest s)))))))))


(defn rect-points
  ([b]
   (rect-points [0 0]
                [(count b) (count (first b))]))
  ([[x1 y1] [length width]]
   (for [y (range y1 width)
         x (range x1 length)]
     [x y])))


(defn taxi-line-points
  ([a b]
   (taxi-line-points [a b]))
  ([pnts]
   (let [[[x1 y1] [x2 y2]] (sort pnts)]
     (map vector
          (if (= x1 x2)
            (repeat x1)
            (range x1 (inc x2)))
          (if (= y1 y2)
            (repeat y1)
            (range y1 (inc y2)))))))


(defn rect-vals [b]
  (into []
        (map #(get-in b %))
        (rect-points b)))


(defn dist [[x1 y1] [x2 y2]]
  (math/sqrt (+ (math/pow (abs (- x2 x1))
                          2)
                (math/pow (abs (- y2 y1))
                          2))))


(defn slope [[x1 y1] [x2 y2]]
  (/ (- y2 y1)
     (- x2 x1)))


(def neighbors*
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (and (zero? x)
                        (zero? y)))]
    [x y]))


(def diag-neighbors*
  [[-1 -1]
   [-1 1]
   [1 -1]
   [1 1]])


(defn move [pnt dir]
  (mapv + pnt dir))


(defn neighbors
  ([pnt]
   (into []
         (map (fn [mv]
                (mapv + mv pnt)))
         neighbors*))
  ;; ASSUMES the board is not transposed to allow x,y addresses
  ([board pnt]
   (into []
         (comp (map (fn [mv]
                      (mapv + mv pnt)))
               (remove (fn [[x y]]
                         (or (neg? x)
                             (neg? y)
                             (< (dec (count board)) x)
                             (< (dec (count (first board))) y)))))
         neighbors*)))




(defn taxi-neighbors [board pnt]
  (let [w (count board)
        h (count (peek board))]
    (into []
          (comp (map (fn [mv]
                       (mapv + pnt mv)))
                (remove (fn [mv]
                          (let [x (first mv)
                                y (peek mv)]
                            (or (neg? x)
                                (neg? y)
                                (<= w x)
                                (<= h y))))))
          [[1 0]
           [-1 0]
           [0 1]
           [0 -1]])))


(defn taxicab-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^long (- x1 x2))
     (Math/abs ^long (- y1 y2))))


(defn board [xmx ymx init]
  (vec (repeat (inc xmx)
               (vec (repeat (inc ymx)
                            init)))))


(defn map-diff [m1 m2]
  (merge-with + m1 (update-vals m2 -)))


;; * means inverted indices
(defn on-board?* [b [x y]]
  (and (< -1
          y
          (count b))
       (< -1
          x
          (count (first b)))))
