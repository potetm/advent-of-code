(ns advent-2019.day-4)

(def input
  [357253 892942])

(defn password-ish? [i]
  (let [iseq (seq (str i))]
    (boolean (and (apply <= (map int iseq))
                  (some (partial apply =)
                        (partition 2 1 iseq))))))

(comment
  (password-ish? 111111)
  (password-ish? 223450)
  (password-ish? 123789)
  )

(defn part-1 []
  (count (filter password-ish?
                 (apply range input))))

(defn remove-dups [coll]
  (keep (fn [[item c]]
          (when (= 1 c)
            item))
        (frequencies coll)))

(defn password-ish?* [i]
  (let [iseq (seq (str i))]
    (boolean (and (apply <= (map int iseq))
                  (seq (remove-dups (filter (partial apply =)
                                            (partition 2 1 iseq))))))))

(comment
  (password-ish?* 112233)
  (password-ish?* 123444)
  (password-ish?* 111122))

(defn part-2 []
  (count (filter password-ish?*
                 (apply range input))))
(comment
  (filter password-ish?
          (apply range input))
  (part-1)
  (part-2))
