(ns advent-2015.day-15
  (:require
    [advent.util :as util]))

(def ingredient-attrs
  [:capacity
   :durability
   :flavor
   :texture
   :calories])

(def ingredient-attr-set
  (set ingredient-attrs))

(defn parse [in]
  (map (fn [[_ ing & r]]
         (merge {:ingredient ing}
                (zipmap ingredient-attrs
                        (map #(Long/parseLong %)
                             r))))
       (re-seq #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"
               in)))


(defn int-partitions
  ([n k]
   (filter (fn [ret]
             (= (util/sum ret) n))
           (int-partitions n k [])))
  ([n k ret]
   (cond
     (= (count ret) k) [ret]
     :else
     (mapcat (fn [i]
               (int-partitions (- n i)
                               k
                               (conj ret i)))
             (range n -1 -1)))))


(defn amount-permutations [ingredients]
  (eduction (map (fn [part]
                   (map #(assoc %1 :amount %2)
                        ingredients
                        part)))
            (int-partitions 100
                            (count ingredients))))


(defn score [ingredients]
  (util/product (map #(max 0 %)
                     (vals (apply merge-with +
                                  (map (fn [{a :amount :as ing}]
                                         (-> ing
                                             (select-keys (disj ingredient-attr-set
                                                                :calories))
                                             (update-vals #(* a %))))
                                       ingredients))))))


(defn part-1 [in]
  (transduce (map score)
             (completing max)
             Long/MIN_VALUE
             (amount-permutations in)))



(defn calorie-count [ingredients]
  (util/sum (map (fn [{c :calories
                       a :amount}]
                   (* c a))
                 ingredients)))


(defn part-2 [in]
  (transduce (comp (filter (fn [ing]
                             (= (calorie-count ing)
                                500)))
                   (map score))
             (completing max)
             Long/MIN_VALUE
             (amount-permutations in)))


(comment
  (def tin "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
  (def in "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5\nPeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1\nFrosting: capacity 0, durability -1, flavor 4, texture 0, calories 6\nSugar: capacity -1, durability 0, flavor 0, texture 2, calories 8")

  (part-1 (parse in))
  (part-2 (parse in))

  )
