(ns advent-2018.day-9
  (:require [criterium.core :as criterium])
  (:import
    (java.util ArrayDeque Deque Iterator)))

(defn rotate! [^Deque dq ^long n]
  (cond
    (zero? n) nil

    (pos? n)
    (dotimes [_ n]
      (let [v (.removeLast dq)]
        (.addFirst dq v)))

    :else
    (dotimes [_ (Math/abs n)]
      (let [v (.removeFirst dq)]
        (.addLast dq v)))))

(defn part-1 [n-players max-marble]
  (let [dq (ArrayDeque.)]
    (.addFirst dq 0)
    (loop [i 1
           scores {}]
      (if (< i max-marble)
        (let [player (mod i n-players)]
          (if (zero? (mod i 23))
            (let [_ (rotate! dq -6)
                  v7l (.pop dq)]
              (recur (inc i)
                     (update scores player (fnil + 0) i v7l)))
            (do (rotate! dq 2)
                (.addLast dq i)
                (recur (inc i)
                       scores))))
        (apply max-key val scores)))))

(defn part-2 [n-players max-marble]
  (part-1 n-players (* 100 max-marble)))

;;;; Linked List

(defprotocol INode
  (insert-left [this v])
  (insert-right [this v])
  (set-left [this n])
  (set-right [this n])
  (delete [this])
  (value [this])
  (left [this])
  (right [node]))

(deftype Node [^:unsynchronized-mutable left
               ^:unsynchronized-mutable right
               ^:unsynchronized-mutable value]
  INode
  (insert-left [this v]
    (let [n (Node. left this v)]
      (set-right left n)
      (set-left this n)
      n))
  (insert-right [this v]
    (let [n (Node. this right v)]
      (set-left right n)
      (set-right this n)
      n))
  (set-left [this n]
    (set! left n))
  (set-right [this n]
    (set! right n))
  (delete [this]
    (set-right left right)
    (set-left right left)
    right)
  (value [this]
    value)
  (left [this]
    left)
  (right [this]
    right))

(defn circular-doubly-linked-list [v]
  (let [n (Node. nil nil v)]
    (set-left n n)
    (set-right n n)
    n))

(defn iterator-left [n]
  (let [n (volatile! n)]
    (reify Iterator
      (hasNext [this]
        true)
      (next [this]
        (let [node @n
              next (left node)]
          (vreset! n next)
          node)))))

(defn iterator-right [n]
  (let [n (volatile! n)]
    (reify Iterator
      (hasNext [this]
        true)
      (next [this]
        (let [node @n
              next (right node)]
          (vreset! n next)
          node)))))

(defn seq-left [n]
  (iterator-seq (iterator-left n)))

(defn seq-right [n]
  (iterator-seq (iterator-right n)))

(defn values [n]
  (into [(value n)]
        (comp (take-while #(not (identical? n %)))
              (map value))
        (next (seq-right n))))

(defn part-1* [n-players max-marble]
  (loop [i 1
         scores {}
         n (circular-doubly-linked-list 0)]
    (if (< i max-marble)
      (let [player (mod i n-players)]
        (if (zero? (mod i 23))
          (let [n (nth (seq-left n)
                       7)
                v7l (value n)]
            (recur (inc i)
                   (update scores player (fnil + 0) i v7l)
                   (delete n)))
          (recur (inc i)
                 scores
                 (insert-right (right n)
                               i))))
      (apply max-key val scores))))

(defn part-2* [n-players max-marble]
  (part-1* n-players (* 100 max-marble)))

(comment
  (criterium/quick-bench (part-1* 473 70904))
  (criterium/quick-bench (part-2* 473 70904))
  (part-2 473 70904)

  (-> (circular-doubly-linked-list 0)
      (right)
      (insert-right 1)
      (right)
      (insert-right 2)
      (values))

  (criterium/quick-bench
    (part-2 473 70904)))
