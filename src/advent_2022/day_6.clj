(ns advent-2022.day-6)

(defn solve [l s]
  (first (keep-indexed (fn [idx part]
                         (when (apply distinct? part)
                           [(+ l idx) part]))
                       (partition-all l 1 s))))


(defn part-1 [s]
  (solve 4 s))


(defn part-2 [s]
  (solve 14 s))

(comment
  (def t ["bvwbjplbgvbhsrlpgdmjqwftvncz"
          "nppdvjthqldpwncqszvftbrmjlhg"
          "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
          "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])



  (map part-1 t)

  (def t1 ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
           "bvwbjplbgvbhsrlpgdmjqwftvncz"
           "nppdvjthqldpwncqszvftbrmjlhg"
           "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
           "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

  (map part-2 t1)
  (part-1 in)
  (part-2 in)

  )
