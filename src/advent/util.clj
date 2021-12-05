(ns advent.util
  (:refer-clojure :exclude [min max])
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]))

(defn min [coll]
  (when (seq coll)
    (reduce cc/min
            coll)))


(defn max [coll]
  (when (seq coll)
    (reduce cc/max
            coll)))


(defn min-by [k coll]
  (when (seq coll)
    (reduce (partial cc/min-key k)
            coll)))


(defn max-by [k coll]
  (when (seq coll)
    (reduce (partial cc/max-key k)
            coll)))


(defn sum [coll]
  (reduce + 0 coll))


(defn product [coll]
  (reduce * 1 coll))


(defn transpose [coll]
  (when (seq coll)
    (apply mapv
           vector
           coll)))


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
