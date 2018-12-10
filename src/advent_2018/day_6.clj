(ns advent-2018.day-6
  (:require [clojure.string :as str]))

(def data "264, 340\n308, 156\n252, 127\n65, 75\n102, 291\n47, 67\n83, 44\n313, 307\n159, 48\n84, 59\n263, 248\n188, 258\n312, 240\n59, 173\n191, 130\n155, 266\n252, 119\n108, 299\n50, 84\n172, 227\n226, 159\n262, 177\n233, 137\n140, 211\n108, 175\n278, 255\n259, 209\n233, 62\n44, 341\n58, 175\n252, 74\n232, 63\n176, 119\n209, 334\n103, 112\n155, 94\n253, 255\n169, 87\n135, 342\n55, 187\n313, 338\n210, 63\n237, 321\n171, 143\n63, 238\n79, 132\n135, 113\n310, 294\n289, 184\n56, 259")
(def toy-data "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9")

(defn parse [s]
  (map-indexed (fn [i l]
                 {:id i
                  :pos (mapv #(Long/parseLong %)
                             (rest (re-find #"(\d+), (\d+)" l)))})
               (str/split-lines s)))

(defn taxicab-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^long (- x1 x2))
     (Math/abs ^long (- y1 y2))))

(defn edges [max-x max-y]
  (let [max-x-idx (dec max-x)
        max-y-idx (dec max-y)]
    (concat (mapcat (fn [y]
                      [[0 y]
                       [max-x-idx y]])
                    (range max-y-idx))
            (mapcat (fn [x]
                      [[x 0]
                       [x max-y-idx]])
                    (range 1 max-x)))))

(defn closest-to [pnt coords]
  (let [[[id1 dis1] [id2 dis2]]
        (sort-by second
                 (map (fn [{:keys [id pos]}]
                        [id (taxicab-distance pnt pos)])
                      coords))]
    (when-not (= dis1 dis2)
      id1)))

(defn rect-points [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y]))

(defn part-1 [in]
  (let [blobs (parse in)
        pnts (map :pos blobs)
        max-x (inc (reduce max (map first pnts)))
        max-y (inc (reduce max (map second pnts)))
        inf-blob-ids (into #{}
                           (keep #(closest-to % blobs))
                           (edges max-x max-y))]
    (reduce max
            (vals (frequencies (remove inf-blob-ids
                                       (keep (fn [pos]
                                               (closest-to pos blobs))
                                             (rect-points max-x
                                                          max-y))))))))

(defn in-good-zone? [max-dist-sum pnt blobs]
  (< (reduce +
             (map #(taxicab-distance pnt (:pos %))
                  blobs))
     max-dist-sum))

(defn part-2 [max-dist-sum in]
  (let [blobs (parse in)
        pnts (map :pos blobs)
        max-x (inc (reduce max (map first pnts)))
        max-y (inc (reduce max (map second pnts)))]
    (count (filter #(in-good-zone? max-dist-sum % blobs)
                   (rect-points max-x
                                max-y)))))

