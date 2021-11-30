(ns advent-2015.day-19
  (:require
    [clojure.string :as str]))


(defn parse-replacements [in]
  (map (fn [l]
         (str/split l #" => "))
       (str/split-lines in)))


(defn replace-str [^String s
                   ^long start
                   ^long end
                   ^String v]
  (.toString (doto (StringBuilder. s)
               (.replace start
                         end
                         v))))

(defn replacements [^String s
                    ^String subs
                    ^String rep]
  (loop [idx 0
         res []]
    (let [idx (.indexOf ^String s
                        ^String subs
                        idx)]
      (if (neg? idx)
        res
        (recur (inc idx)
               (conj res (replace-str s
                                      idx
                                      (+ idx (count subs))
                                      rep)))))))


(defn part-1 [in rep]
  (count (distinct (mapcat (fn [[^String s ^String rep]]
                             (replacements in s rep))
                           rep))))


(defn part-2 [target rep]
  (let [rep (sort-by (comp count second)
                     >
                     rep)]
    (count (take-while (complement #{"e"})
                       (iterate (fn [s]
                                  (first (filter (complement #{s})
                                                 (mapcat (fn [[sub rep]]
                                                           (replacements s
                                                                         rep
                                                                         sub))
                                                         rep))))
                                target)))))


(comment
  (def tin-rep "H => HO\nH => OH\nO => HH\n")
  (def tin-rep-2 "e => H\ne => O\nH => HO\nH => OH\nO => HH")
  (def in-rep "Al => ThF\nAl => ThRnFAr\nB => BCa\nB => TiB\nB => TiRnFAr\nCa => CaCa\nCa => PB\nCa => PRnFAr\nCa => SiRnFYFAr\nCa => SiRnMgAr\nCa => SiTh\nF => CaF\nF => PMg\nF => SiAl\nH => CRnAlAr\nH => CRnFYFYFAr\nH => CRnFYMgAr\nH => CRnMgYFAr\nH => HCa\nH => NRnFYFAr\nH => NRnMgAr\nH => NTh\nH => OB\nH => ORnFAr\nMg => BF\nMg => TiMg\nN => CRnFAr\nN => HSi\nO => CRnFYFAr\nO => CRnMgAr\nO => HP\nO => NRnFAr\nO => OTi\nP => CaP\nP => PTi\nP => SiRnFAr\nSi => CaSi\nTh => ThCa\nTi => BP\nTi => TiTi\ne => HF\ne => NAl\ne => OMg")
  (def in "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")

  (part-1 "HOHOHO" (parse-replacements tin-rep))
  (part-1 in (parse-replacements in-rep))

  (part-2 "HOHOHO" (parse-replacements tin-rep-2))

  (part-2 in (parse-replacements in-rep)))
