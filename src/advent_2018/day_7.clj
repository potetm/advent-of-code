(ns advent-2018.day-7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data "Step P must be finished before step R can begin.\nStep V must be finished before step J can begin.\nStep O must be finished before step K can begin.\nStep S must be finished before step W can begin.\nStep H must be finished before step E can begin.\nStep K must be finished before step Y can begin.\nStep B must be finished before step Z can begin.\nStep N must be finished before step G can begin.\nStep W must be finished before step I can begin.\nStep L must be finished before step Y can begin.\nStep U must be finished before step Q can begin.\nStep R must be finished before step Z can begin.\nStep Z must be finished before step E can begin.\nStep C must be finished before step I can begin.\nStep I must be finished before step Q can begin.\nStep D must be finished before step E can begin.\nStep A must be finished before step J can begin.\nStep G must be finished before step Y can begin.\nStep M must be finished before step T can begin.\nStep E must be finished before step X can begin.\nStep F must be finished before step T can begin.\nStep X must be finished before step J can begin.\nStep Y must be finished before step J can begin.\nStep T must be finished before step Q can begin.\nStep J must be finished before step Q can begin.\nStep E must be finished before step Y can begin.\nStep A must be finished before step T can begin.\nStep P must be finished before step H can begin.\nStep W must be finished before step R can begin.\nStep Y must be finished before step Q can begin.\nStep W must be finished before step M can begin.\nStep O must be finished before step M can begin.\nStep H must be finished before step R can begin.\nStep N must be finished before step L can begin.\nStep V must be finished before step W can begin.\nStep S must be finished before step Q can begin.\nStep D must be finished before step J can begin.\nStep W must be finished before step E can begin.\nStep V must be finished before step Y can begin.\nStep O must be finished before step C can begin.\nStep B must be finished before step T can begin.\nStep W must be finished before step T can begin.\nStep G must be finished before step T can begin.\nStep D must be finished before step T can begin.\nStep P must be finished before step E can begin.\nStep P must be finished before step J can begin.\nStep G must be finished before step E can begin.\nStep Z must be finished before step M can begin.\nStep K must be finished before step T can begin.\nStep H must be finished before step U can begin.\nStep P must be finished before step T can begin.\nStep W must be finished before step A can begin.\nStep A must be finished before step F can begin.\nStep F must be finished before step Y can begin.\nStep H must be finished before step M can begin.\nStep T must be finished before step J can begin.\nStep O must be finished before step S can begin.\nStep P must be finished before step M can begin.\nStep X must be finished before step T can begin.\nStep S must be finished before step J can begin.\nStep H must be finished before step C can begin.\nStep B must be finished before step W can begin.\nStep K must be finished before step N can begin.\nStep E must be finished before step T can begin.\nStep S must be finished before step Y can begin.\nStep C must be finished before step G can begin.\nStep R must be finished before step D can begin.\nStep N must be finished before step U can begin.\nStep O must be finished before step L can begin.\nStep B must be finished before step F can begin.\nStep S must be finished before step F can begin.\nStep X must be finished before step Y can begin.\nStep S must be finished before step D can begin.\nStep R must be finished before step E can begin.\nStep S must be finished before step A can begin.\nStep S must be finished before step X can begin.\nStep A must be finished before step G can begin.\nStep E must be finished before step F can begin.\nStep P must be finished before step A can begin.\nStep A must be finished before step M can begin.\nStep E must be finished before step Q can begin.\nStep H must be finished before step W can begin.\nStep W must be finished before step U can begin.\nStep F must be finished before step Q can begin.\nStep I must be finished before step J can begin.\nStep H must be finished before step G can begin.\nStep I must be finished before step G can begin.\nStep P must be finished before step X can begin.\nStep I must be finished before step D can begin.\nStep R must be finished before step X can begin.\nStep S must be finished before step I can begin.\nStep Y must be finished before step T can begin.\nStep R must be finished before step G can begin.\nStep I must be finished before step X can begin.\nStep B must be finished before step D can begin.\nStep X must be finished before step Q can begin.\nStep F must be finished before step X can begin.\nStep V must be finished before step R can begin.\nStep C must be finished before step J can begin.\nStep L must be finished before step Q can begin.\nStep K must be finished before step B can begin.")
(def toy-data "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")

(defn parse [s]
  (reduce (fn [acc [a b]]
            (update acc a (fnil conj #{}) b))
          {}
          (map (fn [l]
                 (let [[_ a b] (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
                                        l)]
                   [a b]))
               (str/split-lines s))))

(defn immediate-deps [traversal-map]
  (reduce-kv (fn [acc n traverse-set]
               (reduce (fn [acc m]
                         (update acc
                                 m
                                 (fnil conj #{})
                                 n))
                       acc
                       traverse-set))
             {}
             traversal-map))

(defn roots [deps-map]
  (set/difference (apply set/union
                         (vals deps-map))
                  (set (keys deps-map))))

(defn part-1 [go-to]
  (let [imm-deps (immediate-deps go-to)]
    (loop [to-traverse (into (sorted-set)
                             (roots imm-deps))
           res []]
      (if (empty? to-traverse)
        (apply str res)
        (let [n (first (filter (fn [n]
                                 (empty? (set/difference (get imm-deps n)
                                                         (set res))))
                               to-traverse))]
          (recur (into (disj to-traverse n)
                       (get go-to n))
                 (conj res n)))))))

(def s {:in-process {"C" 3}
        :graph {"A" #{"C"}
                "F" #{"C"}}
        :res []})

(defn node-duration [n]
  (- (int (first n)) 4))

(defn step [n-workers {:keys [in-process
                              graph
                              res]}]
  (let [in-proc (into {}
                      (map (fn [[k time]]
                             [k (dec time)]))
                      in-process)
        done (into res
                   (sort (keep (fn [[k time]]
                                 (when (zero? time)
                                   k))
                               in-proc)))
        in-proc' (apply dissoc in-proc done)
        todo (take (- n-workers
                      (count in-proc'))
                   (keep (fn [[n deps]]
                           (when (empty? (set/difference deps
                                                         (set done)))
                             n))
                         (sort-by key graph)))]
    {:in-process (into in-proc'
                       (map (fn [n]
                              [n (node-duration n)]))
                       todo)
     :graph (apply dissoc graph todo)
     :res done}))

(defn part-2 [n-workers graph]
  (let [deps (immediate-deps graph)
        rs (roots deps)
        f (partial step n-workers)]
    (count (take-while (fn [{:keys [in-process]}]
                         (seq in-process))
                       (iterate f {:in-process (into {}
                                                     (map (fn [n]
                                                            [n (node-duration n)]))
                                                     rs)
                                   :graph deps
                                   :res []})))))
;; talk about intellij setup