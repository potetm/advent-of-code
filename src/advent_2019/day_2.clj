(ns advent-2019.day-2
  (:require [clojure.string :as str]))

(defn parse [s]
  (into []
        (map #(Long/parseLong %))
        (str/split s #",")))

(defn inputs [intcode noun verb]
  (assoc intcode
    1 noun
    2 verb))

(defn alarm-1202 [intcode]
  (inputs intcode 12 2))

(defmulti intcode-step (fn [[codes pointer]]
                         (get codes pointer)))

(defn apply-op [f n [codes pointer]]
  (let [p (inc pointer)]
    (apply f (map #(get codes %1)
                  (subvec codes
                          p
                          (+ p n))))))

(defmethod intcode-step 1 [[codes pointer :as s]]
  (let [out-p (+ pointer 3)]
    [(assoc codes
       (get codes out-p)
       (apply-op +
                 2
                 s))
     (inc out-p)]))

(defmethod intcode-step 2 [[codes pointer :as s]]
  (let [out-p (+ pointer 3)]
    [(assoc codes
       (get codes out-p)
       (apply-op *
                 2
                 s))
     (inc out-p)]))

(defmethod intcode-step 99 [_]
  ::done!)

(defn intcodes [intcode]
  (sequence (comp (take-while #(not= % ::done!))
                  (map first))
            (iterate intcode-step
                     [intcode 0])))

(defn intcode-out [intcode]
  (get (last (intcodes intcode))
       0))

(defn part-1 [s]
  (intcode-out (alarm-1202 (parse s))))

(defn part-2 [s]
  (let [prog (parse s)]
    (first (for [noun (range 100)
                 verb (range 100)
                 :when (= 19690720
                          (intcode-out (inputs prog noun verb)))]
             (+ (* 100
                   noun)
                verb)))))

(comment
  (apply-op + 2 [[1 0 0 0 99] 0])
  (intcodes (parse "1,9,10,3,2,3,11,0,99,30,40,50"))
  (intcodes (parse "1,0,0,0,99"))
  (intcodes (parse "2,3,0,3,99"))
  (intcodes (parse "2,4,4,5,99,0"))
  (intcodes (parse "1,1,1,4,99,5,6,0,99"))

  (part-2 "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,2,13,23,27,2,27,13,31,2,31,10,35,1,6,35,39,1,5,39,43,1,10,43,47,1,5,47,51,1,13,51,55,2,55,9,59,1,6,59,63,1,13,63,67,1,6,67,71,1,71,10,75,2,13,75,79,1,5,79,83,2,83,6,87,1,6,87,91,1,91,13,95,1,95,13,99,2,99,13,103,1,103,5,107,2,107,10,111,1,5,111,115,1,2,115,119,1,119,6,0,99,2,0,14,0"))
