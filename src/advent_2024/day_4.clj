(ns advent-2024.day-4
  (:require
    [advent.util :as util]
    [clojure.string :as str]))


(defn parse [s]
  (into []
        (map vec)
        (str/split-lines s)))


(defn neighbors+dir [board pnt]
  (into []
        (comp (map (fn [mv]
                     [(mapv + mv pnt) mv]))
              (remove (fn [[[x y] _mv]]
                        (or (neg? x)
                            (neg? y)
                            (< (dec (count board)) x)
                            (< (dec (count (first board))) y)))))
        util/neighbors*))


(defn part-1 [board]
  (util/sum (map (fn [pnt]
                   (if-not (= \X (get-in board pnt))
                     0
                     (count (filter (fn [[m-pnt dir]]
                                      (let [a-pnt (util/move m-pnt
                                                             dir)]

                                        (and (= \M (get-in board m-pnt))
                                             (= \A (get-in board a-pnt))
                                             (= \S (get-in board (util/move a-pnt
                                                                            dir))))))
                                    (neighbors+dir board pnt))))))
            (util/rect-points board)))


(defn diag-neighbors+dir [board pnt]
  (into []
        (comp (map (fn [mv]
                     [(mapv + mv pnt) mv]))
              (remove (fn [[[x y] _mv]]
                        (or (neg? x)
                            (neg? y)
                            (< (dec (count board)) x)
                            (< (dec (count (first board))) y)))))
        util/diag-neighbors*))


(defn part-2 [board]
  (util/sum (map (fn [a-pnt]
                   (cond
                     (not= \A (get-in board a-pnt))
                     0
                     (= 2 (count (filter (fn [[m-pnt [x y]]]
                                           (and (= \M (get-in board
                                                              m-pnt))
                                                (= \S (get-in board
                                                              (util/move a-pnt
                                                                         [(- x) (- y)])))))
                                         (diag-neighbors+dir board a-pnt))))
                     1

                     :else 0)))
            (util/rect-points board)))


(comment
  (def t "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX")


  (part-2 (parse "XMXMAXXSXSSMMMXAMXMMMAMXMAMXAMSAMXSAMXMXMMMAXXMASMSMSAMXMSMMMAMXMXAMMAMMMXMAMMMMMAMMMMSAMXMMAMXAXMXMAMXSXSMMSMMMMSMMXXAMSMSASAXMMMSMMMSMASMM\nMSMSMSMXMMAASAMXMASMMXSSMSSMSMMXXSAMXMXMXMSMMXMMMAAXXAAAMXSSSMAAXSXSMSASXMMSMMAAMAMXMASMSASASXMASMMSMSAMMASXAXAASAAMAMSXMAXAMAMSAXAAMASMAMAM\nMAAAAXXAAXXMMMSASASASAAAAAAAAAXSMSAMMMAMAMAAMMMAMMMMMSSXSAMXAMMMMAXSAXAXMMAAASXMMASMMMSXSASAXMSAAXAMAMAXSAMMSMSSSMSMMAAAMMMMMAMMASMSMAXMASAM\nXMSMSMSASXMAAAMMMAMAMMSMMMMXMSMAASXMASASXMSMMAMXXAMXAMXMMAXSMMMSAMXMAMSMAMSSMMAMSMSASAMAMAMAMAMSAMXMAMXMMASAMXMAXXXAXXSSMXASXXXSAMXXMAXSMXAS\nMMAMXXXAAAMMMSSSMMMXMAXXSXXXSAAMMMXMXSAXMAXASMSMXAMXSSXMSSMXXSXMAXSMXMAMSMAMAMMMAXSMMAMAMAMMMSMMXXASASXXSAMMSMMAMXSXMAMAXXXSXMXAAXXXSAMSMSMM\nASMXMSMMMXMAAAMAASMXSXSASXMMSASMMMXSAMXMMMSAMMAMMMMMMAAXAMXSMMASMMXMMSMMAMASXMXMAXXASMMXXXXAMAXSSSMSASMMMASASAMAMXMASMSXSMSMMAXMXSMMMASXAAAX\nMXAASXSXAAXSMSSMXMMAMAMAMMMASAMXXAAMXMAAAXMXMMAMMMAASMMMSSXSSXXMXAMSAAAXSSMSAMASXSSMSAMSSXSASMSAAAAMAMAAMMMASMMASAXXMXAAAMAMMSMMAXXASXMMSMSS\nXMSMSAXMMMMMAMAXAMMAMSMAMAXXSAMXMMXSASMSSSMAMMMSASMMSXSAMMAMXMSMXSAMSMSMMAXSXMASXAAAMMSAAASAMXXMSMMMASMMSMMMMASMSXSMMXMXMXASAMXMAXXASASXXAAM\nSMMMMXMAXMAMXMASMSAMXMMXXMSXMXMSSMMSXMAXXAMXMAAXMXXXMMMASMAMAXXAAAXXAXXASAMXXMASMMMXSMMMSMMMMMXXXXMSASXAAXMAXXMAXAXAXAXXSSMSMSMMASAASAMMMMXS\nXMAAXAASMSSMSAMMXAMSASXSSMSAMAMAAAAXXMMMXMMSSMXMXSMSMAMXMXXSASMMMSMMSSMASMSMXMAMAXAAXAAXAXMXAXSSMAXMASMMMSXAMSMSMAMSMMSAXAMSXAAXAXMMMAMXXAMX\nSSSMSMMMAAAMSASMSMMXASXAASMXXAMSXMMSMSASMSAMXMSMAAAASXSXSAXAMXXAAXMAXAMAMAAAAMMXSAMASMMSASMMSMSASXMSMXASASMMMAAMAMXMAMXMSAMXSSSMMSXMASMXXAAS\nAXAXAMAMMMSMSAMXAXAMSMMSMMMMSSXMSSXXAAAXAMXXMAAASMSMSXAAXMASMMSSSSMXSXMXSSMMMSMSXAMAXXAXAAAAXXSAMMMAMXXMAMAASMMMSMAMAMXXXMMMAMXAAAAMAMSMSSSM\nMSXMASXSMMMAMAMXMAXXXAMMAAXMAMAAMMMSSMSMSMSXMXXAMAMASMMMMSSXMAAAXMMXMMSAMASAMXMAMXMXSMSMASMMSAMXMMSASXSMAMSMMASMAMASMMXSXAXAAXXMASXMAXXXAMAX\nXMAMXMASMAMAMSMXXMMSSXMSMMSMAXMAMAAXMXMAAAXXASXSMXMAMAXAAMMAMMMXMASXAAMMSASMXSAMXMMXSXAMMXMMMMMMSASASAMMAMAASAMSXSASAMAMXMASXMXSXMMSMMAMMSSM\nSMXMAMAMMAXAMAMAMSMAMXMMAMXMSMMASMSSMMSSXMMMXXAAAXMASXMMSXSAMMSAAMXMMMMAMMSASXSMXMAMMAMSMAMMAAAMAAMAMMMMAXSMMMMSMMXSAMASAMMAMMXAMXMAMXMSXAAX\nMASMMMSSSSMMMASAAAMAMSMSAMSAMXSXXAAAMAAMMMXXAMSMMXSXMXXAAXSXSAMMSMAAXAMXSMSMAAXMXXSAMXMAMMXSSSSSMMMMMXMSSXMSAMXMASASAMAXASXAAMAMXXMASAAAMMMX\nMAMAAAAXXMAXSMSMSXSXAAXXAMASXMXAXMXSMMMSAXSMMMXAMAMAMSXASXSXMASXAMASMXSXAAXXMXSXMAMXSSSXSXAMAMAXXAMMAMAAMXSSXMASMMMSMMMSMMMSMSAMXMXAMMSAMXSA\nMXSSMMSSMXSXAAXMXMSMMSSSSMSMMAAMMSMMMXMMXSXAXASXMASAMSAMXAXMSAMXASXMXAXMMMMXSMSAMXMAMAMAXMMMAMAMMXMXAAMAMXMXSAMXXMMXXAAAXXAAXMAXXAMSXMXASAAM\nAAMASAAXXAXXMMMAXAXSXAXAAXAAXMMXAAAAMAMAMXXAMXMASAXASMAMMAMAMASXXSAMSAMXXXMXXAMXMAMXMAMMMSXMSMMSAASXMSMMMMMASXMXSXSMSSSMSMSXSAMXXSAMASXMMMSM\nMMSAMMSSMMSSMMMMMSMMMMSMMSSSMXSAMSSMSSMMMASMASMMMMSMMXAMXXMASXMMXMAMMMAMSAMXMSMMSASXAMSXAMMMAAXXXAXAAXAXSXXAXAXMSAAAMAAXXAAAAMAMAXAMAMXMAMAX\nXXMXMAXAXAAMAAMAAAAAAMAMXXMAXAAMAMAMAXXMAXXMASMAAAAAXSXSXXSXSAAMMSAMXSAMXXMAAAXXMMAMXSAMASASMSMMSSSMMMSMAAASMMMAMSMMMSMMMXXXMMAASXMMSSMSMSAS\nMXAMXXSAMMSSSMSMSXSMMMAXXMSSMSSSMSAMMSMMXSAMASMSMXSSMXMXXAMXMXMMASXSXMASASXMSSSXMXMMMXMAXXMMXXMAAXSAMAMMMMMXAXXSMMMSAMAMSMSMSSXMMAAAXAXAMMAM\nSASASMMXMXMAXXAAXAXAMSXSAMAAMMAAXSAMAAAMASXMAXXAMXMXMMXSAMMXSAXMASXMXMAMXXAAAXXAMSMMMSXMSMSSSMMMSSXMMAMAXAXSAMMMMAAMAMAMAAAAXXMASMMMSAMXMMXM\nMAMASXMMMMMMMMMSMSMAMAMMAMXSMMMMMSMMSSSMASMMMMSMSXSAXAMSASAAMSAMASMSASXSSSMMMSMMMAAAAAAXAXMAXAAMXMAASMSMAMAMXMAAMSSMSMSSMSMSMAAXSAMXXMSSXMAS\nMAMXMASAAASASASAAASXMMSSXMAXXSAMAXMAMXMMMSASMASMMASXMXASAMMMSAMMASAMASAAAXXAXMAMSXSMSXMSMSMMMSXSAMMMSAAXSAMXAASMMAMAMAMXAAXAMXXMSAMXXMAMASAM\nSMSASAMSSXSAMMSSXXXXMMAAAMXSASAMMSMMSXXAAMMMMXSAMAMMMMAMMMXSXMAXMSXMAMMMMMSMSSMMSXMXXAAAAAXXAAASAMAXMMMMMXSSXMAMXAMXMAAMSMSMSXSASAMXXMASAMXS\nXSSXMAXXMMMXMXXMXSXMMMSMMMASXSMMAAAAAASMXMAAMMSAMASAXSAMSSSXMXMXXXAMXSXSAAMMAXXAMASASMMMXMMSMMMMMSXSAMMAMAXAMXXXXXXSAXXMXAMXAAMXXAMAASXSMSXA\nSXMASXMMAAAAXAXMASMASAXAMXXSAXMMSSSMXXMASXSXSAXXMXMAXSAMAXXAMAXSMSAXAMAXMSSMAMMXSAMAAXSXMSXMAMXMAXMMAMSASMMMMSSXMMAXMASMSAMMMSMMSSMXXSMSAXAX\nSAMXSMMMSMSSMSAMXXAXMASXMAMMXMSAMXXXMXXXSAMXMASXMSMSMSAMSSSMMAMSAAAMSMSMAAXMMSAMMMMMMMXMASASAMSMSMASAMMAXMAXAMAXXAMAXXXMSAMXXAAMAMMSXSAMXMAS\nSAMAMAAMAXXMXXASMSSSMMMASAMMSMMMSAMXMMSMMMMXSXMAMAAMASXMAAAAAXSMSMSMXAAMMMSMMMMSXSAMXAMXSMMMXSAAXMAMXXMMMSSMMSMMSSXSSMMXSAMXSMSMASMSAMXMASAX\nSAMXSMMSMSSMASXMASAAMMMXXASAMAAAXMAMSAAAAXMAXAAMSMSMMMMSMSSMMSMMMMMXMSMMSAAAAMSXMSASMSSMMAAXXSMSMMSSSXMAMXAAAXXAXXAAAXXAMXMASAXMASAMXMAMMMSX\nSXMASAMSXAAMMSXMSMMMMMSSMAMASMMSSMAAMXSSMSSMSSMXAMXAMXXXXXAMMMAAXAAMXMAXMAMMMXSAMXMSAMXASXMXXMAMXXAAXAMXASMMMXMMSMXSSMXSXSMASMMSAMXMMSSSMAXS\nMAMASXMXMSSMAMXMAMSMXAAAMMSXMXAAXMMXXAXAAAAXAASAMXSMMMMSSSMSASXMMMXSAMAMSSSMMASXMAMXMXMXMXAMSMXMXMMXMMSMMMSAAXSMMMAMXAMAASMXMXAMXSASXAAMMAMS\nMAMMXMASAMXMXSASASASMMXSMAMXSMMXSXMAMXMAMSSMSXMXXMAMAAMAMAASASXSXMXSASMMMAAASXMSSXSAMXSXSXMASAMXSMSMAAAAXAMMMXSASXXXASXMAMSXAMXXMSAXMXSAMXSS\nXXSXSXMAXMMSASMSXSMAMMMMMMSAMXSAMAMSSMASAMXMXAMXMSASMXSASMMMAMAMASMSMMAAMMMMMAAAXXMASAMXSAMAMSAMXAASMSSSXMSMXMSMMAXSSMMMMAMMXMAMXMSMMAMMSMXM\nSXMXMAMAASAMXMASAXXAAAAMAXMAMAMASMMMAAAMAXMAXXMAMAAXMASAMAXMXMASAMMXXSMMSAXAMMMMMSSMMMSASXMAXMSSMSMSXXAMXMAMAMXXMXMSAAXSMAAAXMMXXAAXMXSAMXAS\nAASMSAMSMMASXMAMMMSSSMXMSMMMMXSMMMXSAMMXXMASXMMSSMSMMXMMMSMSXSAMAXSXAMMSMMSMXAAXAMASAAMXSXSMSSMSXMMMMMMMMSASMSSXSMXSXMMXSASMMSAAMSMSMAXMMSXS\nMXMAMSMXXSAMXMASAAXAAAMSMSAAAMXMASASAMXSSMMXAMAMAXMAMSASMXAAMMASMMSMMAXAAAAMSSMMMSAMMMSXMASMMXAMMSAAAASAAMASAAXAMXXSSSXAMXAAAMMMMAAAMXMSAMMS\nSMMMMXXSAMASXSXSMSSSMMMAAMMMSAAMSMMSXMAAAXASAMXSMMSAMXAMXMAMXSAMAAXAXSSSSMMXAMXSAMAMXXMAMAMAMMAMASMXSMSAXSXMMMMMMSSMAXMXSMMMMSXMSMSMSAAMXSAX\nAXAAMXAMMMASMMASAXAMMXSMSMSXXXSMXXXMXMSSMMXSMMXAMAMASMXMMXAXXMASMMSSMAAAAASMMSAMASMMSSSMMSSSMSAMXMXMXASMMMASXSXSAMXMAMSMMAXSAXAXSAAAMMSMAMMS\nMSXSMMSMXMXMAMAMMMAMMXMAXAMXSAMXMMMXAMMAMXMSMMAXMAMMMXMAMSSSMSMMMAMMAAMMXAMXXMASXMXAXAAMXXAAASXSAMASMAMMAXMMMAAMMXMMSSXAMSAMXSMMSXMXMXMAAXXS\nXMMMAAMMSXSMSMASXXMMXXMAMAMAMAMASXSSSSMMMAXMASMAXXXXAMMAMMASAASXMASXSAXXXSSSXSAMMMMSMSMMSMSMMXASXSAMMAMMAXSASMSMSXMAXAXSMXAXXAMXXASXSXXSXSMM\nXXASMMSMSAMAMMMAMXSAMXMASAMXSASXSMXAAAMSSMMSAMXSMMXMASXSMMAMXMSXXASAMAXSAAAAMMMXAAAMMXXAAXAMXMMMMMMMSASMMXSASXMASAMMSAXMASMMSMMMSMMASAMXMMMX\nXSMSAAAASXMAMASAXMAAMXMASAMXMMXAMXMMMMMAAMAMAMAMAASMXMAMAMXMSSMMSMMXAAMMMMMMMAMMSMSSMMMXSSMSSMAAXXAMXASXMAMMMAMAMMSAAMXMMAMASAAMSAMXMXMSMAAX\nMMAMXSMMMSMXXAMXSASMSAMMMAXSMSXSMXAAASMSSMASAMASMMMMAMAMXSAMSASAAXXSMXSXXSAMMMSAMXMXXASMMMXMXSXXMMMSMSMMMMMASMMXSASMXSAMSAMAMMMXMAMAMAXXMMSM\nXMAMXAMASMSSMSSMSXMASMSSSSMMAAAAASAMXSAXAMXSASXSXASXMMSMMMAMSAMSSSXXAAAMASAMXAMAXSAMXXXAAXASMXMSMSXAXMASAMSASMAXMASAAXAMMAMMSMSMMXSSSMMSMXMM\nXMASMXXMASASAAMAXAMXMAAXXAAMXMMMASXMXMAMMAXSMMXXMASAXAXAAXAMMXMAXAASMMSMASAMMMSAMXAMMMSSMSMSAMXAAXAAMSXMAMMAMMXMMAMMMSXMMAMXAXMASXXMAAAASAAA\nMSXMMMXXMMSMMMXMSSMSMMMSMSSMAMXSAMASXMMAXXMXMXMMAMXMMXXSMSSSSSMSSMAMAAXMASAMXMMXSMMASMAMAMAXMAMMXMMMAASMXMSMMMXSMXSAAAMAXXMMSSXSMXASXMMASMSM\nMAXSAMSMXXAMMSAMMMASXMXXMAXMASAMASAAXMAMXAMMSAASXMMAASMXAAAXAAAAAXASMMSMXSAMXSAXMASAMMAMAMXMXSAMXMSASMMMMXAAAAMSXMMMXSAAMSSMMSXAXSXMXSXMXAXX\nMMMSASAXASXSAXMSMMAMAMSAMSSMMMMMMMMMAMASMMXASAMAAASMMMASXMMMSMMMSSMMXAMXAMASAMMMSAMASXMSMXXXMMXSAAAMXXMAMSSSMSXSASXSAMMASAAAASMSMSAMXMAXSXMM\nAAAXXMMMMSAMMXMAXMASAMXMXAMAAASASXSXSMASMSMXSMMMSMMAXMAMAXXXAXMXAMXSMMXMXSAMXSAAMXXAMXAAAAMMSAASMMMXXXMAXXAMXXASAMAMXSXMMXXMMSAMMMAAASAMMAXX\nSMXXAXXAAXXASASXXSAMASMXMSSSXXXASAMAXXMSAMXXXAMXMXSMMMAMAMXSXSAMMMASASAMXMASXSMSMSMSSMSMSXSAAMMSXSAMSASXXMAMAXMMSMXMASAMXSMSMMXMASXSASAASAMX\nMAXSXSSMSSXMSASAXMASXMMAAAXMXSMMMMMAMAAMAMSMSMSASMXSXXSMMMMSXMXSXMASAMMMMSAMASAXASXMAMXXAMXXXSXMXMAMSMMXASXMXXXAAMAMASAMAXMAAXASASAMAXMMAMSM\nMSMMAMMMMMAMXAMXMMMMMASMMMSMAMAAAMMSSXMSAMAAAAXASAMAMAXAMMMSAMAMXMAMXMSAMMXSAMAMAMXSMMMMMMMXXMASAMAMMAMXAMAMMMMSMXAMXMAMMSMSXMMMMMXMMMXMAXAM\nAXAMAMAAASMMMXMXSASAMXMASXAMMSAMXSAMXAXSMSMMMSMXMMAMMASAMAASAMXMMAMMSAXSAAMXAMXMXMXSSXMASAMXMXXMASXSMASAXSAMAMXMASMSMSAMXSMXASXXMXAMXMXSMSSM\nMSASMXMMXXAASXMAXASXSMXAMSAMXXXXXMMSSSMSAAXSAMXAMXSXMASXMMXMAMASAAASMAXMMMSSSMASAMXMAMSASXSSMSSSMMAMMAMXMSMSXSAMAMXAMSMMMXAMAAAASMSMASAXAAAA\nMMXMMAMSSSSMMAMAMMMMXMMMXAMXXXMSMAMAAAAMSMXMASMMXAMAMASAMSSSMMXAMSMXXMASAMXAASASAMAAMMMXMMMXMAAXAAMSMMSAMXAAASASXXMMMXMAAMMMSMMMSAAAXMMSMSSM\nSSXMMSMAAAMMSXMAMAAXXAXMAMSXSAAASAMMSMMAAMASXMAXMMMAMASAMAAAXMMMXXMMAXMSASMSMMMMXSSSXXXXSAMAMMSMSSXSAAXASMMMMSMMXXAMMAMSSMSAMXXAMXMMMMXAMXAX\nSAASAAXMSMSAMASXSSSMSMXSAMMAMMSMSSSXAMMSXXASASAMXXXAMXXAMMXMAMXXXSASMXMSAMAAMSMMAMXAAMSMASMMSAXMAMASMMMSMMXSMMAAXSXMMMXAAXMASXMMAMXAXXMXSMMM\nMSMMASXXAAMASAMMAMXAAXAMASMAMAXXXXXXMSXMSMXSMMXSAAXSMMSAMXASXSMSASAMXXMMAMMMXAMXSMMMMMAAMAMAXXSMXMXMAMMMAMXMASMMMMMSAMMXSMSAMXXSAXSMSMSMSAXA\nMXMAMMXMMSMAMASXSMMAMMMSAMXMMMSMMMMSMXAAXXMXMSAMMSMMAASMMXMAAAAMASASXSXAXSMSMSSMMASXSXMSSSMMMXXMAMASAMAMAMASAMXSAAAXAMAAXAMXSXMMAXXAAAAASXSS\nXSMSMXXMAXMXSMMMASAXMAMMXSXXSMAXAAAAXXMAMMMAXMASAXAMXMSMSAMMAMXMAMXMASMMMSASAMMASAMAMSXAAAMXXAXAXSASMXXMAMXMASXSMSMSSMMMMAMMMMMXXMMMMSMMMAMX\nMXAAXAMMMSMAMAAAMXXMXAXMASASASASXSSMSMSASASMMSAMASAMXXSASXSXMMAMXSAMMMAMAMMMAMMAMMMAMAXMSMMMMSAMMMXSMASMXMAXSAMXAXMAXMXSMSMAMXSAMXXSAMXSMSMS\nASXXMMMAMAMXSSMSXMAMSASMAMXSAMXSMMAXMAAASMSAAMASXMASXAMXMAMAASXSAXAMXXSMSSXSXMMMSMSXSXMXAMASAMXXASMMMASAMXSMAMMMSSMMSMMSAAXXSAXASAMXASAXAMAA\nMXMAXMAXMMXMMAMMXMSMAAAMSMAMMMAXASAMMXMMMXMXMXMAXSSMXXSMMAMSMMAMMSSMAMMXMMASMSMAAAAAMMSSXSAMMSMSMSASMAXASAXMAAMAMAMXSAAMSMMSMMSXMASMSMMMSMXM\nSASAMSSSXXAXSAMXAAMMMXMAXMAMAMMSXMMAXMSXAXMAXMAAXMAMXMMMMAXAAMSMMAAMMMSAAMSMAAMSMSMSMAAMXMASMSXMASAMMXSAMASMSSMASMMXSMMMAMXXAAMMSMMMXMXAXMAS\nXAXAXMAAASAMSXSXMMSAXASMMSASXSAMMAMSMMMASMSAMMMXSMAMMSAAXSSMSMXAMMMSAASMMXXMXMMAAXAMMMMSAMXMXSAMAMXMAXMXMAMMMAXASXMASAMSXSMXSMMAAAMXSMMMXSAS\nMSMMMMMMMASMSMSXSAMASMMAAXASAMAMXSXMAAAXMAXXMSAMXMXMXMSXSAMXMMSAMAASMMSASMSMMMMMSMAMAAMMASAMASXMASMMMSAAXXAMSMMMSAMXSAMMMAXAXAMSXSMAAXAAXMAS\nMXAAXXMSXMASXASAMXSXMXMMMMSMMSSMAMASXSXSMXMMMAMAMMMSMMAMXXSXMAMMMMMSMAMASAAAASXMXMSMMMMMMMMMAMAXAMAASMMMSSMAAAAASAMMXMMMASMMXAMMAXMASXMSSMAM\nSSXMASXMASMMMMMXMAAAMXSXSXMAMAXMAMXMAAASXSMSAASAXAAAMMMXSSMMMSSSSSSXMASMMXMSMSAAXSXXMAAASAMSSMSMMSSMMAAMAAMSSSMMSAMXMSAMXMAMSAMXAMAMXAAMAMAM\nASMXXXASXMAAAMAXMMSSMSXASXSAMASMMSMMMMMMAMASMMSASMSSSXMAXAASAMXAAXSASASXAXMAXMMMSMMSSMSMMAXAXAXAMAMAMMMMMXXAAAMASAMXASASASMMAMSMMMSSXMSSSXSS\nMXSXXMASMMMSXSXXAAAAXSMMMAXXMASAMSXMASAMAMXMAXSXMAXAMAMSMSMMMSMMMMMXMAMMSSMAMXSXAXAAAXMASXMMXXMAMSSSMSXAMSMMSMMAMXSMASMMASMMMAAAXAAAXXXAAAMX\nXXMMAMXXAMAXXAMSMMSSMSMSASMMSASXMXAXASASMSMSMMXAMXMAMMMXAXXMXAMAAASASXXAAXXAXXMSMSMSSMMAMXAXXSSXMAAAXSXSMXAXAMMMSAXMAMAMAMAMXSSSMMSSMMMMMMMA\nMSMSMAAMMSMSAMXMAMXMAMAAAMXAMAMASXSMXXAMMAMAAMMAMXXSMSMSXMASMMSSXMSAMAMMSSMMMXAAXAMAMXMSSSSMXMAMXMSMMMAXMMMMMXAMMXMMASMMMMSMAXAMAMAMXSMMSSMM\nSAAAMMSSMAASMXXSMSASMMXMMMMSMAMMMAXAMMAMSASXSMSXSMSXAAASAXAXMAXXSAMXMSMMAXAASXMMSMXAXSXMAAXMASAMAXAMAMSMMSASXMXSASXMMMAASXMMSSMSAMAAXMAXAAAA\nSMXMXAAAMMMMXAAMAMASXXXAAXAXSMMSMAMAMSSMSXSAMXSAAXSMSMMSAMSMSMXAAXXAMAAMMSMXMAAXXXXMMSAMMMMSASASXSAMSXXAAMXMAMAMXMAMMSSMMAAXXAXSXSMSMMMMSSMM\nMMSSMMSSMSXSMXSMAMMMAMSXMMXMAXAXMSMAMXAAMASXMASMMMXAAAXMXMAAAXMMMMSMXSSMXXMAXMMMXMASAXAMSXAMAXXXXAXAXAMMMSASAMSSMSSMAMMASXMMXAMXAXMAAAXXXAAX\nMAAASXXXAAAXMAXMXMAMAMXAXASMMMXMSASAXSMMMAMXMXXMXXMSMSMSSMXSMSAXAMXAAMAMXMMSAXSAMXAXASXMMMAMSMSMMXSMMXMAAAAMXSAAMAMMAMMAMMAAXXXMXMMSMMSMAMMM\nMMSXMXMMSMMMSXMSSMASMMSMMSMAAAMXXMXXMSASMXSSSSMSSMAXAMXAASXXMAXSASMMMSAMXSAMXXXSXMMXXMAAXXAAAMAAAAAAASMSSMXXMAXXMASXMMMMSSMASMAMXSAAMXSXXMAX\nSAXXXXXAXASAXAXXASXXMASXMASXMSMSSSXSASAMXAAAMAMAAXAMAMMMSMMAAAAMAMAMASMMXMAMXSAMXMMSSMSXMSMXXSSMMSSSMSAMXXXMASMMSASAMXSXMXXAAXAMSMSXSSMMSSXS\nAAMSMSMSSSMMSMMSMMMSMAMASASMXAAXAXAMMMXMMXMMXAMSSMMSAMMSXMSSMASMSXSMAMASXMSMMXAXAXAAAXAAXXMSAMXSAAAAAXMXXAAMAXSAMASXMAAAXAMSMMMMAXAMSASAXMAM\nMXMAAAAASXAAAAMXXAAAMMSMMMSMSMSMMMXMAXXMAMXXSXXXAXASXSMAXMAMMXAXMAXMMSAMXAAAXSMMMMMSXMMSAAXMMMAAMMMMMMSMSSMMMXMXMMXAMXSSMSXXAXSMSMMXMAMMSXAS\nSSSMSMSMMXMMXMMASMSXSAAAAXXMASXMXXMASMSMSAAMAXMSMMMSSXMASMXSAMMXMXMAXMASXXSAMMXAAAMMXSXMMMMMSMMSXSXASAAAMAAASMSMSMSAMAMXMXAXXAMAXAMAMSMMSMMA\nXAAXXMAMSXSSMXMAXMAXMXSMMXXMXXMXSXMAXAXAXMXSXAMAMAXXMXMASXMAMSXSMMMAMMSMXMXMASMMSSSMAMAXXMAXXAXAAMSXMXSXSXSMMXAAAAMAMMSAXMAMSXMAMMSXMMAMXASM\nMSMMSXMASAAASXMXMMSSMMXAMSXSMASAMXMASAMMMXMMAMXSSSSSMAAAXAXAXXAAAASAMXXAMXXSXMAMXXAMAXMMXSMSMSMMXMSXSAMASAXMSSMMMMSAMXSASXMMXAMXSAMXMXAMXAMX\nMAAXXXMASMSMMXXMXAAAAASAMAAASXMASMMASAMAAXAXAMXAAMAAAMMMSAMMSMSMSMSMSSMAMSAMSMSAAXMMMSMAXSASAAAXAMXMMAMAMXMXAASAXXSMSMXMMMSASXMMMXXMAXSSMMAS\nSSMXMXMASMMASMSMMAMMMMXMMMSMSXMAMXMASASXSSXMASXMMMSMMAAXMXMASAAXXAXAAXMAMMAMASAMXSSSMAMMMMAMSMXSXSAXMMMSMSMMMSSXSASAMXASAAMXSAAXAMMAMSMAMMAA\nAAAXAXMASASAMXAAAAMSSXSAMXXAXMXSAXMASXMXAAASAMASXAAXXMMMSAMXMXMXMMMMMSXXXSXMXSMXAAAASMSSSMMMMXAXASXSMMMXAMMAMAMMMXMASXXSMMMASMMMAXAASAMAMMMM\nSAMSSMMAMMMXMXSMMASAASXMMMMXMMXMAMSAXASAMXMAASAMMSMXMAAAMASXSXSXAXXXAXXMXSXMXXXXSMSMMSXMAXAXMAXMXMMAMAAMAMXAMMSXAAXAMXMMXMSXXAXSSSSMSAMMXASM\nAAMAXAMSSSMMSXAXXXMMMMMAAXASASASMXMMSMMMXAXSAMXSXAAASMMMSAMAMAMXMASMSSMMAMASXMMXXAMAXXMASMSSMAXXAXXAXMMSAMXXMAXMMMMASAXMAMAMSMMMMAXXXMMSSXSX\nSMMASXSAAXAAASMSXSXSXAXXSSXXASASAMMAAAAXSSMXXSXMXMSMMAAMMXMXMXMAXSXAMMAMXMMMAAMXMMMMMMSAMXXAMAXSASMMSSXSASAAASXSXAXSSMXMAXAAAMAMMMMSMSMXAXMX\nMAMAMMMMSMMSAAXMASAMMXSAMXXMAMAMMAMSSSMXMMMAMMMSAMAMXSXSXSAASMSMXAMMMSSMXMASMMMASASXAXMASXSMMMXAXMXSAMXSAMMSMAASXMXMXXASXSMSXXAXAAXXAAAMXMXA\nMAMASXAXMXMAMXAMXMMMAAXASAMXXMXMXSXXXAMXAASAAAAXMSASXMASXMSASAAMXSAMAXAAXSAMAAXXSASXMSMAMAAXMSMMSSXMXMAMAMAAMMAMASMMAMMXAAAXMSSSXSXMMMXMASXM\nSXSASMSMSASMXSSMAMXMSSXXMAXMAMXMXMXXMAMMMMMASMXSXSAMXMAMXMXXMXMSAMAMSSMMMSASMMSAMXMMMAMAASMMMAAXAMXXAMSSXSSSSMMSAXAXMSSMSMMMXAMXMMMSAMASAMAX\nMAMASXMASASXAXMASAXMAMXAMXMXSMMMASAMMSASAAXXMXMAAMXSXMASXMMMXAXMXSXMAXMAAMAXXSAMXXXSSXSXXMAXSSSMAMMMSSMAAMXMAAMMMSMMAAMMXASMMMSAMXAMASXMXSXM\nMSMAMMMXMAMMSSMAMASMASASXXSAXAXSAMAXMAAAMMMMAXXSXSAMASASAMAAXXMSMMASMASMMMXMMSMMMMMMSXAXMSSMMAXXAMMAMAMMMMASXMMAASAAMMSSSXMAAASASMSMAMXAXSAS\nAAAAMMMAMAMMXAMXSXMMMXAXAXMAMMMSXSMMSMAMXMAMAMXMAMXSAMXSAMXSAMXSASXMXMXASMMSAMXMAAMSMSMXMAMAMXMMMSMXSXMXSSXMAMSMMXXMSAAXMASXMMSAMAXMMMMXMSAM\nSSSMMSSSXXMXSSMAMXSXMMSMMMMAMMMSASMMXXSXMSXSAXAMAMAMASAMXSAAMAMSAMMXMAXAMXAMXSASMXMAAXXMMXMSMMMASAMXMASAMXMASXSMSMSMMMSSMASMSMXMMXMSAMAAAMXM\nMAMAXXAMXSMAMAMXMAXAXAAAXAMMSAAMAMXMAMXAMMMXMSMSMSXSAMXMASMMMSAMAMSMSASMAMSMAMMSAASMMMMMSAMMAMXMSASMSAMASMXMXAMAAMAAAAAAMASXAMAMMXMXAMSXSXSX\nSAMSMMAMAXAASMMSASXSMSSMXXXASMSMMMAMMMMMMAMAAXXAMMAMXSXMAMAAXXSSMMAAAXAMAAXMASASXMMAAMMAMAXMAMSXSXMAAMSXMAMSMAMSMSXSMMSSMMMXMSMXSASMSMXAXXMM\nSXMXASMMMSSMXXASAMXXAXMMSSMMMXMMAXASASAXSAMASMSMSXSMAXXMSSSMSAMXMXMSMAMSSXXSAMAMAXSSMSMXSMMASASASAMSMASAMXAXMAMMASAAXXXAAXXSXSAASAMAXMMXMASA\nMASXMMMAMMXSXMAXXSMMXMMAAAAASAMSSSMSASMMSXXMAMXMAXAMXSAAXXMMXASXXMAAXMXMAXMAXMMSAMXXAXMMMMSAMASASXMXMASASASXMXSMAMSMMXMSMMSAAXMMMMMMAMAMSAMS\nSAMXMAMAXMASXMXMSAAMASMMMSMMXAMAXAAMAMXMXMAXSAMXSXMXSAMXXMAMSAMAASMSMXAMXMMAMAXMMXSASXAAAAXXMAMAMXMXMXSAMAMSSXMMXXMMMMAMAMAMAMXXAXMSAMAXMASX\nXMAMSASXSMASMXAMSMMSASAAMMASMXMMSMMMAMAAAAXAAAXAMMXMXXXSMSAMMSMMAMAAMSXMAAMMMXMAMAMMMMXMAXXXMAMMMAXMXXMXMAAMSAMMMXXAMXAMAMMXXAMSXSASASXSAMAA\nXMAMMASAAMAMMMSXMXAMASMMXSASMMMMAAASAMSSXSXMSSMAMSAMAXMSAMASMXXMAMMMXMASMMMSSXSXMAXAAXXMSMSMSMSSSXSXMMMXSMSASXMAMMMSSMSSSMXSXSXSAAXSXMAAASMA\nMMMMMAMMMMXXXAXASXMMMMAMMMAMAAMMSSMSMXMMAXXMAMMSXSASXSMMXMXMMXXMMMSXAXMASAMAMXAXSMSSSMSAXAAXXSAMXSMXMAAAXXMASASASMXMAMMAMMAMMMSSMMMXAMXMAXXM\nMAAXXXMASXSMMASAMMAMXMSMAMAMSXSAXMMMXAAMXMSMASAMASAMASXASMXSAMXMAAASMMSAMXMAMAXAMAAAAAMAMSMSSXXAMXMMMASMSMMMMXSAMMXXAMMAMMAMAXASAASXSMSSMMAS\nSSSSXAMASAMXMAMXMASXSXMSXSAMXAXMXAAXSMXMAMASXMMMAMAMAMMASMASAASXMXMXSAMXMASXSSMSMMMSMXMXAASXAAMSSMXSMXXASAAAXAMMMAMMMMXASXSSSSMSSMXAAAMAASAM\nAAAMMXMXMAMAAXXMAAAAXAXAMAMXMMMXSXMMSAXMMSAXXAMMASMMSXMSSMAMMXXAMXXAMXMAXMSXMXAXXMMMMAMXSMSXMAMAAAAMMXMAMSMSMASAMXSAMMSMMAMAXAXMAXMSMAMSMMAS\nMMMMSSMMSMSSSMMASXMXMMMSMSASAXMAXASMMSMAXMXSMSMMAXAAMXMMMMASMASXMXMAMSSSSSMAMMSMMXXMASMMMXXMXSMSXMMSAMMXMAXXAASAXASASMAAMSMSMMXSAMXXMAMXXXMM\nXXXXAAXASAAAAXAXXASAMXMXAXXSMAXSSXMAAXMXMAMXAAMMSSMMSASMASAXMAMSAASAMXAAXASXMXXAMMMMAAAAXAMSAMXXXAAMMSAXXAMXSASMMXMAMXMMMXAXMXMSAMXAXMASMSMM\nSSSMSXMXSMMSMMSXSAMMXMSMMMASXSXMAMSMMMMMMAMSMMAAAAAASXXSAMAMMAMASMXAXMAMSMMMMMSXMAAMXSMMMMMMASASXMMSXMASMASMMAMAMXMSXSXSAMXMXSAXMMSXMXMASAAM\nMAMAMASMXXAMXMXMMMMMMAAAAMAMAXMMAMXXAAAAXAXXAXMMXSMMSAMMXSASMASMMMSSMSMXAXAXMAAASXMSAMXSXSAXMMASASAXMAAXMAMXXMSMMASAAAASMSAAAXSSXMXSMXMSXSMM\nMMMMSAMAXMAXAMXAXAAAMSSSMMMMAMMMSSXSSSSSSSMSAMXAAMMXMAMAXMAMXAMXXAAMMSASMSSXXMSXMAXMXSXMASXSAMXMAMXSAMSSMXSXMMAMSMSMSMXMAXMMMAXXXMAXMAXXAAMX\nSXSAMAMMAXMSASMASMSMXAAAXXAMXSXAMAAXAAAMMAMXXMMSSMASXMMMSMAMMMSMMMMSAXMXXAMMSMMMSMMMMMMMAMSAXASMAMMAXAAAXMAMAMAXAMSMMXXMAMXAMAMXXMMSSMSMSMXS\nAAXMMMSXMAMSMSMAAMAMMMSMMSMSXMMASMXMMMMSMSASAXMMMXXMAXAMAMASASAMXXSMMXXXMXMXAAAMASAXAMXMXSXXMAXSAMAAXMSSMAASXSSSSXMAMAMSAMXSXASAXSAAAMXAXXAA\nMMMSAAMASXMXAXMXMMAMAMMXXAMAXXMXAMXMSXXSAMASMSMAMSXSXMSSSXXSMSXMAMMAXSMMSASMSSMSASMSMSAMXMMMMMMMMSSMMAAMAMASXAMAMMSAMAASXSAXSXAMAMMSSMMSMMMS\nXMAXMMXAMXMMMMAXSSMSXMAMSMSAMMSMMSMAAAAMAMMMXXMAMMAMXXMAMSMXASMSSXSAMAAXSAMAMXXMASAAXSMMASASAMXSAXAAXMAXSAASMSMAMASMSMMMASAMMMMMMXXAXXXMXSAM\nMMMSASMXMAMAXMAXXAXXAMAMSXMASASAAAXSSSMXXMSXMASXSXSMSMMAMAXMSMAAAXSSSXMMMMMSMSAMXMMMMXASASXXASAMXSSMMXMAMMMMAMSMSMMASMAMMMSMAAXXSMSSMMAMAXAM\nXAXMAMMSXSXSSMSASMMSXMASXASXMAMMXMXMAAMXSXSAMXMMAMAAAASXSMSMAMMMSMSASAMMXMXMASXXAXMXXMAMASMSMMMAXXAAMSMSXAMMMAAXXAMAMMASXAXSSSMMSAAAMSAMSSMM\nMMSMSMXMMMAAXAAAMXMSMSMSMXMXMAMSMSAMSMMXAAMSMAAAMXMSMXMMSAMXXXXXXXMAMMMXAMSMXMMMMSMASMMMMMAAMASMMSMSMAAXSMSSSMSXSAMXXXXXMSMXMAMAMMSMMMASAAMX\nXAAAAXAXASMMMSMSMSMXXAXMAMMXSAMXASMXAXAMMXMXSMMMXXMXMASASMSMMMSAMMMMMAMXXSAMSXSAMAMAXAXAAMSMSASAAXAMMMMMAMSXSXAASXMASMSMAAASMMMXSXMMSMMMMSMX\nXSMSMSSMXSAAMXAAASMMSSSMSMXXAXXMMMXSMSSXAAMMMMSSMMMASAXAMAAAXAMMSAAASASMMMAXAAXMMAXMXSMSSXMAMXSMMMSMAAMSSMMASMMMMASASAAXSSXMAASAMXSASASMSMXX\nMMAXAAMMMSAMAMSMSMAAAXXAAASXMMSAAMXSXAMMSXXAAAMAAMSXSMMSMSMSMXSASMMXSASMASMMMSMSXSMSAMMAMAMAMASAXXMXXMXAMAMMMMAAXMMAMXMMAXXSXMMASMMASAMXAAXX\nAMSMMMSMAMAMMAMXAXMMSSMMMXMAXAXXXSAMMMSMAMMSMSMSMMXAXAAXAXMMXMMASXSXMXMMAMXAXAAXAXAMXXMASAMAMXSXSXXASXMMSAMAASMSSMMAMMMSMMAXAXSAMAMAMXMSMSMS\nXSAMXMAMMSMMXXXSMMSXMAAASXSSMMSSMMASXXAMASAXAMMXXSMMSMMSAMASMMMAMXAXSAMMMSSSMXSMXMASMASASXSASMSASMMXSAAXSASMMMAXAASXMMAAXAMSSMMMSAMXMAXAAAAX\nSXMXMMMSASAMAAMAAASASASMSAMAAXAAMSMMMMXMAMXMSMASAXAMXMAMXSXMAMMMSSMASXXAMXAXAXMMSXAXXXMASXSXSAMAMXMMSXMMSAMMSMMMSMMAMMSSSSXAMAAASASXSSMMSMSM\nXXMASXXMMSAMXXAMMMXAMAXXMAXSSMSMMAAASASMXXAMXMASMSSMASMSASASXMXMAAXMXMXSSMSMSMSAMMMSMMSASMMMMAMAMASXMSMAXASAMAXAXAMXMAAAMMMXSSMXSAMMAAAXXMAX\nXXMAXAASXSMXMASXAXMSMMMXSMMXXAXASMSSMASAMSSMAXXMASMMXSAMASAMASAMXSSXMASMAAMAAAMMSAAAAXMAXXAASMSMMAMAAAMAXAMASMMMSSMAMSXMXSMAMAXXMMMMSMMMAXAX\nSAMASXXMAXSAXMAXXXMMAMXMAMMMMMMAAAXAMAMXMAMMSXSAMXMMMMMMAMXMAMXMAAMAXSASMMMMMSMAMMSXSXMMMSSXSXAMMSSMSMSMMSMMMAAASMSAXMASAAMSSXMXMASAMAXXXMXS\nAXMAMXAMAMSMSSSMSMMSAMMSXSAAASMSMMMMMXSAMXSAMXSAMSSXAXAXSMSSSMSMSXXAMMMMXXAMXAMXSMMAXXMAMAMXSMMMAAAAMAAXXMASXMMMSASXAMAMXMAAXMXAXAXASXMSSMXS\nMSMXSSSMMXSSMAAAMAASASXMASMMSAAAXSAXAMSXSAMXSASAMAASXMMSAAAXAAAAAAMSSSXSXSSSSXSMAASMMMSAMASASASMMXMMMSMSXSAMXAMXMAMMSMAMSMSSMXMMMSXMMXMXAAAM\nXAMXMMASXXMAMMMMMMMSMMAMXMXSMMMMMMXMAMXXSAMXMASXMAMMXMXSMMMSMMXMAXSAAAXMMMXAAAXXMAMAAAXAMMMASAMXMASXMMAMAMAMASASMXMAMXSMXMMAAMAAAMSMAXXXMMMS\nSXSAASAMMXMAMXXMXSAXMSASAMXAMXAASXXXSMMMSASAMMMMXXMAXSAMXAAXASXAXXAMXMMMASMMMSMMXSSSMSSSMAMXMAMASASAAMAMAXAMMAMAAMMASAXXAXXXMAXMXMAMXMMMMAAX\nASMMXMAAMSSMXMSAASASXSASMSAMXSSXSASXMMAXXAMXMMAMXXMMMSAMXMMXXMASXMXMAMASMSXAAAASAMXMMXAMMAMAXMXMMASMMSXMMMSSSMXMXSMAMAMSMSXMASXMSSMMMAAASMSX\nSXXSXSSMMAASASMMMSAMAMAMASASAXXAMAMAMSSSMMMSMSAMASXAAMAMXSXSAMAMAXXSASXSASMSXSSMASAMMMAMMXSSSMAMMMMXASMSMMXAXXAXXXAAMMMASAMXAMAAXAXMSSSXSSXM\nMAXSAMXXMSAMXSASMMAMXMXMASAMASMMMAMSMMAXMAAAAMAXAMMMSSXMXAAAMMSSXMASXSXMAMXMMMAMAMASASMMSMAAXMAXAXXMMSAAXSMMMSMSMMSXXASXSMSSMSSXXMMMAAXMXMXA\nMMMXAMXXAMASXSAMXSSMAMXMAMAMAXAASAXMAMXMSMSSSSSMXSAXMAXSAMXMMAMXMAXMAMXMMSMMASAMXSAMXSAAAMMMMSMSSSMAMMMMXMASAMASAMMMSMSAXMAXMAMAMMAMMSMMSMMM\nAAASXMASXSMMAMMMXAXMASAMASAMXSSMSMXSAMMAXAAAAAAAASAXSAXMAXXSMXSAMSAXAXXSAMASMSXSAMXSAMXSXSMXXXXAAAXSMXXXASMMSAASMMAAAAMMMMASMAMAXSAXSAASAAAX\nSXXAMMAXMAMMXMXMASASMSASMSXSAMAMXAMSAMXSSMASMSMMMXSAMXSSMMAXSXSASXMSAMXXMMMAMXXMXXSAMXMMMXXMASMMSMMMMSAMXSMAXMASXSMSSSMXXAMMXASMXAMXSASXSSMS"))
  (part-2 (parse t))

  (let [b (parse t)]
    (into []
          (map #(get-in b %))
          (util/rect-points b)))
  )


;; transducers

