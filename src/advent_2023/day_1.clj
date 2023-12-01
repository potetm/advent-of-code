(ns advent-2023.day-1
  (:require
    [advent.util :as util]
    [clojure.string :as str]))


(defn parse [s]
  (str/split-lines s))


(defn calibration-value [l]
  (let [nums (filter (fn [c]
                       (Character/isDigit ^char c))
                     l)]
    (Long/parseLong (str (first nums)
                         (last nums)))))


(defn part-1 [in]
  (util/sum (map calibration-value)
            in))


(defn part-2 [in]
  (util/sum (comp (map (fn [l]
                         (-> l
                             (str/replace "twone" "21")
                             (str/replace "eightwo" "82")
                             (str/replace "eighthree" "83")
                             (str/replace "oneight" "18")
                             (str/replace "threeight" "38")
                             (str/replace "fiveight" "58")
                             (str/replace "nineight" "98")
                             (str/replace "sevenine" "79")
                             (str/replace "one" "1")
                             (str/replace "two" "2")
                             (str/replace "three" "3")
                             (str/replace "four" "4")
                             (str/replace "five" "5")
                             (str/replace "six" "6")
                             (str/replace "seven" "7")
                             (str/replace "eight" "8")
                             (str/replace "nine" "9"))))
                  (map calibration-value))
            in))

(comment
  (def t "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")

  (part-1 (parse t))


  (def t1 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")
  (into []



        (parse t1))
  (part-2 (parse t1))
  )
