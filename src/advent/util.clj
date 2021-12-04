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
