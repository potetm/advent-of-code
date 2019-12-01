(ns advent-2019.day-1
  (:require [clojure.string :as str]))

(defn parse [s]
  (map #(Long/parseLong %)
       (str/split-lines s)))

(defn fuel-required [n]
  (- (long (/ n 3))
     2))

(defn part-1 [in]
  (transduce (map fuel-required)
             (completing +)
             0
             in))

(defn fuel-required* [n]
  (let [fr (fuel-required n)]
    (if (pos? fr)
      (+ fr (fuel-required* fr))
      0)))

(defn part-2 [in]
  (transduce (map fuel-required*)
             (completing +)
             0
             in))
(comment
  (fuel-required* 100756)
  (part-2 (parse "128167\n65779\n88190\n144176\n109054\n70471\n113510\n81741\n65270\n111217\n51707\n81122\n142720\n65164\n85045\n85776\n51332\n110021\n99706\n50512\n95429\n149220\n102777\n93907\n61769\n66946\n121583\n132351\n53809\n73261\n122964\n120792\n73998\n79590\n140881\n53130\n82498\n72725\n127422\n143777\n55787\n95454\n88293\n107988\n145145\n59562\n142929\n132977\n88825\n104657\n70644\n124614\n66443\n117825\n97016\n79578\n136114\n64975\n113838\n63294\n58466\n76827\n56288\n126977\n63815\n129398\n123017\n118773\n144464\n60620\n79084\n94685\n70854\n148054\n134179\n113832\n113742\n115771\n115543\n73241\n62914\n146134\n128066\n52002\n132377\n100765\n105048\n59936\n131324\n137384\n139352\n127350\n116249\n79847\n53530\n99738\n61969\n118730\n121980\n72977")))