(ns advent-2018.day-1
  (:require [clojure.string :as str]
            [net.cgrand.xforms :as xforms])
  (:import (java.util HashSet)))

(def toy-data [-1, -2, -3])
(def data "-17\n-20\n-15\n-2\n-7\n-4\n-18\n-7\n-5\n-6\n+16\n+6\n+2\n-7\n+18\n+4\n-9\n-3\n-17\n+9\n+2\n-9\n-5\n-12\n+10\n-13\n-16\n-7\n+5\n+8\n+17\n-3\n-10\n+16\n-1\n+7\n-5\n-9\n-16\n-7\n+12\n-11\n+17\n+1\n-17\n+11\n-7\n-9\n+12\n+8\n+16\n+9\n+1\n-17\n-10\n-12\n-17\n-11\n+12\n-17\n+6\n+14\n-1\n-10\n+9\n-3\n+13\n-4\n-16\n+13\n-8\n-14\n+5\n-6\n-4\n+7\n-6\n-17\n+13\n+20\n+3\n-12\n-5\n-12\n-5\n+16\n-3\n+14\n+15\n-19\n+20\n+14\n+2\n+12\n+27\n-5\n+10\n+19\n+10\n-11\n+10\n-1\n+10\n-1\n-13\n-10\n+22\n-1\n+7\n+8\n+16\n+19\n-15\n+2\n-1\n+16\n+11\n-9\n-5\n-1\n-3\n+6\n-4\n-1\n-12\n+19\n-9\n-3\n-1\n-10\n+9\n+19\n+18\n+4\n-2\n-14\n+7\n-4\n-1\n+4\n-14\n-18\n-15\n-15\n-9\n+10\n+12\n-17\n-18\n-13\n+19\n+18\n+18\n-4\n-8\n+1\n-5\n+6\n+2\n+13\n-1\n+29\n+13\n+14\n+9\n-3\n+5\n+14\n-18\n+5\n+5\n+15\n+13\n+5\n+9\n+13\n+6\n-3\n-13\n-8\n+7\n+20\n+10\n+15\n+9\n+18\n-6\n+19\n+14\n+18\n-16\n+2\n-10\n+18\n-9\n+16\n-8\n+18\n+6\n-1\n+6\n-10\n+12\n-9\n+13\n+11\n-19\n+5\n+18\n-2\n+7\n+10\n+2\n+5\n+5\n-6\n-10\n-7\n-9\n-17\n-11\n-7\n-8\n+19\n-5\n-1\n-9\n-16\n+7\n+8\n-7\n+11\n-6\n-4\n-3\n-14\n-11\n-5\n-7\n+4\n+6\n+10\n+15\n-6\n+25\n-1\n+21\n+3\n+3\n-9\n+11\n-4\n-17\n+22\n+10\n+22\n+12\n+10\n-5\n+4\n-16\n-9\n-11\n-8\n-4\n+10\n-1\n+27\n-14\n+9\n+10\n-3\n+16\n+18\n-14\n+9\n+8\n-14\n+3\n+6\n+18\n-7\n-4\n-18\n-13\n+4\n+10\n-9\n-17\n-18\n-10\n-27\n+12\n+2\n+4\n-1\n+6\n-24\n+9\n+26\n+2\n+2\n+20\n-11\n+21\n-12\n-2\n-5\n+14\n+21\n-8\n+14\n+2\n-5\n+8\n+12\n+8\n+19\n-11\n+1\n+16\n+6\n-1\n-3\n+11\n+17\n-6\n-4\n+5\n-15\n-1\n+10\n+7\n-14\n+9\n-20\n-4\n-7\n-5\n-6\n+10\n-15\n+6\n-9\n+1\n+16\n-12\n+2\n-19\n-13\n+19\n+1\n+7\n+19\n+11\n-4\n+9\n-15\n-12\n+17\n+19\n-6\n-4\n-12\n-9\n-18\n-5\n+14\n+14\n+23\n+34\n+15\n+24\n-6\n+24\n+14\n+11\n+12\n+19\n+13\n-5\n-3\n+16\n-1\n+34\n+17\n+7\n-9\n+12\n+17\n+17\n+16\n+7\n+18\n-6\n-13\n-7\n-7\n+3\n+7\n+9\n-7\n+4\n-11\n-8\n-8\n-8\n-12\n+14\n+3\n-6\n-7\n-20\n+19\n-15\n-5\n-16\n+3\n-13\n+18\n-14\n+3\n-19\n+26\n+21\n+3\n+18\n+8\n-11\n-16\n-7\n-2\n-11\n+34\n+8\n+10\n-17\n-18\n+14\n-24\n+17\n-5\n-34\n-28\n-16\n-16\n-15\n-15\n+3\n+4\n-5\n+29\n-2\n-13\n-11\n+19\n+31\n+94\n+6\n+13\n-12\n+23\n+21\n+13\n-4\n+19\n-6\n-17\n+20\n+7\n+15\n+5\n-13\n+5\n+6\n+10\n-18\n-7\n-12\n-16\n-9\n-4\n+14\n-16\n-2\n+16\n-2\n+13\n+20\n+9\n-3\n+10\n-15\n-9\n-5\n-4\n+20\n-10\n-20\n+6\n+18\n-15\n+14\n+8\n+15\n+20\n-7\n-7\n+10\n-7\n+14\n-13\n-7\n-28\n-10\n-6\n+11\n-23\n-17\n+11\n+4\n-28\n+15\n-4\n+3\n-23\n-6\n+2\n+25\n-24\n-11\n+68\n-7\n+38\n-4\n+23\n+19\n-9\n-1\n-20\n+16\n+33\n-36\n+26\n+7\n+47\n+8\n+21\n+16\n+24\n+21\n-22\n-3\n-14\n-17\n+7\n+21\n-20\n+15\n-11\n+26\n+64\n-13\n-45\n+17\n+9\n+8\n+18\n+28\n-23\n+67\n+39\n-209\n-14\n-8\n-197\n+101\n-73\n+23\n+81274\n+10\n+19\n-8\n-1\n-11\n+7\n-9\n-11\n-17\n+7\n+16\n+11\n+19\n+15\n+13\n-14\n-9\n+17\n-19\n-10\n+13\n+6\n+9\n-2\n+16\n+16\n-12\n+14\n-6\n+7\n+7\n+8\n-20\n-7\n-17\n+11\n+10\n-3\n+13\n-4\n+6\n-14\n+6\n+20\n-4\n+18\n-4\n-7\n-6\n+18\n-6\n-16\n+5\n-11\n+17\n-19\n+23\n+10\n+15\n+10\n+1\n-13\n-11\n+20\n-1\n+10\n+18\n+3\n+6\n+10\n-6\n+12\n-17\n-13\n+3\n-19\n-12\n-4\n-3\n-15\n+11\n-7\n+3\n-6\n+24\n+8\n-9\n+5\n+19\n-1\n-5\n+19\n+16\n+17\n-7\n+17\n-19\n+5\n-11\n+17\n+17\n+2\n-5\n-9\n+17\n-7\n-13\n+10\n+18\n-9\n-10\n-4\n-6\n-25\n+10\n+11\n-1\n-11\n+14\n-19\n-20\n-11\n-15\n+16\n+20\n-9\n+10\n+3\n-16\n-3\n-10\n-1\n+9\n-14\n+1\n+8\n+18\n+3\n+4\n+6\n-16\n+41\n+12\n+13\n+3\n+19\n+18\n-13\n-6\n+7\n+17\n-15\n-15\n-7\n-10\n-4\n-10\n+15\n+13\n+1\n-17\n+6\n+16\n+8\n+7\n+11\n-6\n-9\n-15\n+5\n+5\n+13\n+20\n+20\n+19\n+14\n+6\n+6\n-11\n+1\n+17\n+5\n-15\n-12\n-9\n+11\n-14\n-10\n-3\n+6\n-16\n-16\n+19\n+5\n+4\n+18\n+12\n+7\n-16\n+12\n+3\n+18\n+15\n-17\n+8\n-4\n-15\n-19\n-7\n-16\n+7\n-14\n+17\n+1\n+6\n+13\n+2\n+9\n-1\n+12\n+10\n+7\n+5\n+15\n+3\n+4\n+10\n+10\n-8\n+13\n+11\n+8\n-14\n-7\n-4\n+19\n-17\n+11\n+19\n-12\n-15\n+11\n+12\n-5\n+20\n-4\n+16\n-17\n-2\n+13\n-20\n+13\n-11\n-17\n+20\n-7\n+19\n+10\n-6\n+8\n+21\n+15\n+12\n-13\n+8\n-18\n+5\n-3\n-14\n+4\n+15\n-13\n-4\n+7\n+17\n-11\n-14\n+15\n+17\n-8\n-15\n-13\n+6\n-4\n+14\n+8\n+10\n+16\n+8\n+12\n-4\n-19\n+2\n+6\n+19\n-12\n-6\n-17\n+20\n-2\n-16\n+1\n-15\n-13\n-6\n-15\n-17\n+1\n+6\n-11\n-16\n-16\n-18\n-4\n-6\n+9\n-18\n+13\n-20\n+5\n+14\n+12\n-13\n+17\n+10\n-15\n+16\n+11\n-9\n+23\n-9\n-19\n-19\n-12\n-4\n+3\n-10\n-13\n+18\n-8\n-4\n+15\n-12\n-16\n-4\n+7\n-5\n-9\n+17\n+16\n+1\n-8\n-10\n+13\n+22\n+11\n-4\n-5\n+31\n-3\n+15\n-8\n+25\n+15\n-21\n-4\n+17\n+3\n+3\n+7\n+11\n-15\n-4\n+10\n-21\n-15\n+31\n+1\n-12\n+13\n+25\n+14\n+6\n-3\n-8\n+13\n+3\n+4\n+12\n+4\n-8\n+9\n+12\n+22\n-14\n+1\n+7\n-21\n+2\n+9\n+12\n+17\n-8\n+19\n+12\n-5\n-14\n+3\n-6\n+13\n+1\n+10\n-8\n-15\n-26\n+2\n-1\n+8\n-15\n+12\n-18\n-13\n-21\n+5\n+12\n+10\n-1\n-19\n+12\n-35\n+1\n+4\n+44\n+30\n+2\n+33\n+4\n+21\n+13\n+1\n-16\n-15\n+19\n-5\n-19\n-7\n-3\n+8\n-9\n-12\n-10\n-12\n+4\n-1\n+20\n+13\n+17\n-82073")

(defn parse [s]
  (mapv #(Long/parseLong %)
        (str/split-lines s)))

(defn part-1 []
  (apply + (parse data)))

(def d (parse data))

(defn part-2-imperative []
  ;; DO NOT DO THIS!
  (let [res (atom nil)
        seen (HashSet.)
        sum (atom 0)]
    (while (nil? @res)
      (doseq [inst d]
        (when (and (.contains seen @sum)
                   (nil? @res))
          (reset! res @sum))
        (.add seen @sum)
        (swap! sum + inst)))
    @res))

(defn part-2-recursive []
  (loop [instrs d
         seen #{}
         acc 0]
    (if (empty? instrs)
      (recur d
             seen
             acc)
      (if (contains? seen acc)
        acc
        (recur (next instrs)
               (conj seen acc)
               (+ acc ^long (first instrs)))))))

;; recursion < reduction < sequence operations (map/filter et al)

(defn part-2-reduction []
  (reduce (fn [{:keys [seen
                       acc]} instr]
            (let [v (+ acc instr)]
              (if (contains? seen v)
                (reduced v)
                {:seen (conj seen v)
                 :acc v})))
          {:seen #{0}
           :acc 0}
          (cycle d)))

(defn part-2-reduce+seq []
  (reduce (fn [seen acc]
            (if (contains? seen acc)
              (reduced acc)
              (conj seen acc)))
          #{}
          (reductions + (cycle d))))

(defn part-2-seq []
  (first (common/duplicates (reductions + (cycle d)))))

(defn part-2-xform []
  (first (sequence (comp (xforms/reductions + 0)
                         (common/duplicates)
                         (take 1))
                   (cycle d))))
