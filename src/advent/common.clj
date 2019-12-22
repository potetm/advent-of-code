(ns advent.common)

(defn duplicates
  ([]
   (fn [rf]
     (let [seen (volatile! {})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (case (get @seen input)
            :seen (do (vswap! seen assoc input :emitted)
                      (rf result input))
            :emitted result
            (do (vswap! seen assoc input :seen)
                result)))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (case (get seen f)
                         :seen (cons f (step (rest s)
                                             (assoc seen f :emitted)))
                         :emitted (recur (rest s) seen)
                         (recur (rest s) (assoc seen f :seen)))))
                   xs seen)))]
     (step coll {}))))

(defn duplicates-by
  ([f]
   (fn [rf]
     (let [seen (volatile! {})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [i (f input)]
            (case (get @seen i)
              :seen (do (vswap! seen assoc i :emitted)
                        (rf result input))
              :emitted result
              (do (vswap! seen assoc i :seen)
                  result))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [v (f x)]
                         (case (get seen v)
                           :seen (cons x (step (rest s)
                                               (assoc seen v :emitted)))
                           :emitted (recur (rest s) seen)
                           (recur (rest s) (assoc seen v :seen))))))
                   xs seen)))]
     (step coll {}))))
