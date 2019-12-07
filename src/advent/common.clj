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
