(ns advent-2015.day-4
  (:require [clojure.string :as str])
  (:import
    (java.nio.charset StandardCharsets)
    (java.security MessageDigest)))

(defn md5-sum [s]
  (format "%032x"
          (BigInteger. 1
                       (.digest (doto (MessageDigest/getInstance "MD5")
                                  (.update ^bytes
                                           (.getBytes s
                                                      StandardCharsets/UTF_8)))))))


(defn matching-int [secret-key starts-with]
  (first (filter (fn [i]
                   (str/starts-with? (md5-sum (str secret-key i))
                                     starts-with))
                 (next (range)))))


(defn part-1 [secret-key]
  (matching-int secret-key
                "00000"))


(defn part-2 [secret-key]
  (matching-int secret-key
                "000000"))

(comment
  (def input "yzbqklnj")
  (part-1 input)
  (part-2 input))
