(ns advent-2022.day-11
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  {:round 1
   :current 0
   :monkeys (into []
                  (comp (filter seq)
                        (map (fn [grp]
                               (into {}
                                     (map (fn [[f & r]]
                                            (case f
                                              monkey [:monkey (first r)]
                                              Starting [:items (vec (next r))]
                                              Operation [:op (let [[_new _= _old f v] r
                                                                   f (resolve f)]
                                                               (fn [old]
                                                                 (f old
                                                                    (if (= 'old v)
                                                                      old
                                                                      v))))]
                                              (let [[_test _div _by v] f
                                                    [_if _t _throw _to _mky tm] (first r)
                                                    [_if _f _throw _to _mky fm] (second r)]
                                                [:test [v tm fm]]))))
                                     grp))))
                  (read-string (-> s
                                   (str/replace ":" "")
                                   (str/replace #"(?m)^(.*)$" "($1)")
                                   (str/replace "Monkey" "(monkey")
                                   (str/replace "Test" "(Test")
                                   (str/replace #"(?m)(If false throw to monkey \d+\))$" "$1))")
                                   (str/replace #"^" "(")
                                   (str/replace #"$" ")"))))})


(defn step [{r :round
             cur :current
             mks :monkeys}]
  (let [nxt (mod (inc cur)
                 (count mks))]
    {:round (if (zero? nxt)
              (inc r)
              r)
     :current nxt
     :monkeys (let [{me :monkey
                     is :items
                     op :op
                     [t tm fm] :test} (get mks cur)]
                (reduce (fn [state i]
                          (let [wl (op i)]
                            (-> state
                                (assoc-in [me :items] [])
                                (update-in [(if (zero? (mod wl t))
                                              tm
                                              fm)
                                            :items]
                                           conj
                                           wl))))
                        mks
                        is))}))


(defn run [step-xf in]
  (transduce (comp step-xf
                   (map (fn [{c :current :as s}]
                          [c (get-in s [:monkeys c :items])])))
             (completing (fn [acc [m items]]
                           (update acc
                                   m
                                   (fnil + 0)
                                   (count items))))
             {}
             (iterate step in)))


(defn result [res]
  (util/product (comp (xf/sort >)
                      (take 2))
                (vals res)))


(defn part-1 [{mkys :monkeys :as in}]
  (result (run (take-while (fn [{r :round}]
                             (< r 21)))
               (assoc in
                 :monkeys (mapv (fn [{op :op :as m}]
                                  (assoc m
                                    :op (fn [old]
                                          (long (/ (op old)
                                                   3)))))
                                mkys)))))


(defn part-2 [{mkys :monkeys :as in}]
  (let [lcm (util/product (map (comp first :test))
                          mkys)]
    (result (run (take-while (fn [{r :round}]
                               (< r 10001)))
                 (assoc in
                   :monkeys (mapv (fn [{op :op :as m}]
                                    (assoc m
                                      :op (fn [old]
                                            (mod (op old)
                                                 lcm))))
                                  mkys))))))


(comment
  (def t "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")
  (def in "Monkey 0:\n  Starting items: 65, 58, 93, 57, 66\n  Operation: new = old * 7\n  Test: divisible by 19\n    If true: throw to monkey 6\n    If false: throw to monkey 4\n\nMonkey 1:\n  Starting items: 76, 97, 58, 72, 57, 92, 82\n  Operation: new = old + 4\n  Test: divisible by 3\n    If true: throw to monkey 7\n    If false: throw to monkey 5\n\nMonkey 2:\n  Starting items: 90, 89, 96\n  Operation: new = old * 5\n  Test: divisible by 13\n    If true: throw to monkey 5\n    If false: throw to monkey 1\n\nMonkey 3:\n  Starting items: 72, 63, 72, 99\n  Operation: new = old * old\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 4\n\nMonkey 4:\n  Starting items: 65\n  Operation: new = old + 1\n  Test: divisible by 2\n    If true: throw to monkey 6\n    If false: throw to monkey 2\n\nMonkey 5:\n  Starting items: 97, 71\n  Operation: new = old + 8\n  Test: divisible by 11\n    If true: throw to monkey 7\n    If false: throw to monkey 3\n\nMonkey 6:\n  Starting items: 83, 68, 88, 55, 87, 67\n  Operation: new = old + 2\n  Test: divisible by 5\n    If true: throw to monkey 2\n    If false: throw to monkey 1\n\nMonkey 7:\n  Starting items: 64, 81, 50, 96, 82, 53, 62, 92\n  Operation: new = old + 5\n  Test: divisible by 7\n    If true: throw to monkey 3\n    If false: throw to monkey 0")


  (part-1 (parse in))
  (part-2 (parse in))

  (:op (first (:monkeys (parse t))))

  )
