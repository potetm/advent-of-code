(ns advent-2015.day-14
  (:require
    [clojure.string :as str]
    [advent.util :as util]))


(defn parse [in]
  (map (fn [l]
         (let [[_ r speed secs rest]
               (re-find #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
                        l)]
           {:reindeer r
            :speed (Long/parseLong speed)
            :run-duration (Long/parseLong secs)
            :rest-duration (Long/parseLong rest)}))
       (str/split-lines in)))


(defn part-1 [secs spec]
  (util/max (map (fn [{s :speed
                       run :run-duration
                       rest :rest-duration}]
                   (loop [t 0
                          dist 0]
                     (if (<= t secs)
                       (recur (+ t run rest)
                              (+ dist
                                 (* s run)))
                       dist)))
                 spec)))


(defn init-state [in]
  (into {:time 0
         :reindeer-state (into {}
                               (map (fn [{r :reindeer :as spec}]
                                      [r (assoc spec
                                           :distance 0
                                           :status :running
                                           :status-duration 0
                                           :score 0)]))
                               in)}))


(defn toggle-state? [{s :status
                      sd :status-duration :as state}]
  (= (get state
          (get {:running :run-duration
                :resting :rest-duration}
               s))
     sd))


(defn step-reindeer-state [{speed :speed
                            s :status
                            d :distance
                            sd :status-duration :as state}]
  (let [state' (assoc state
                 :distance (if (= s :running)
                             (+ d speed)
                             d)
                 :status-duration (inc sd))]
    (merge state'
           (when (toggle-state? state')
             {:status-duration 0
              :status (get {:running :resting
                            :resting :running}
                           s)}))))


(defn winners [{rs :reindeer-state}]
  (let [longest (util/max (map :distance
                               (vals rs)))]
    ;; Ties result in multiple winners
    (keep (fn [[r {d :distance}]]
            (when (= d longest)
              r))
          rs)))


(defn step [{rs :reindeer-state
             t :time :as s}]
  (let [rs' (reduce-kv (fn [rs r s]
                         (assoc rs r (step-reindeer-state s)))
                       rs
                       rs)
        rs'' (reduce (fn [rs r]
                       (update-in rs [r :score] inc))
                     rs'
                     (winners rs'))]
    (assoc s
      :time (inc t)
      :reindeer-state rs'')))


(defn part-2 [secs in]
  (let [{rs :reindeer-state} (nth (iterate step
                                           (init-state in))
                                  secs)]
    (util/max (map :score
                   (vals rs)))))


(comment
  (def tin "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

  (part-1 1000 (parse tin))

  (def in "Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.\nRudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.\nDonner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.\nBlitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.\nComet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.\nCupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.\nDasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.\nDancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.\nPrancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.")
  (def secs 2503)
  (part-1 secs (parse in))

  (step (init-state (parse tin)))
  (part-2 secs (parse in)))
