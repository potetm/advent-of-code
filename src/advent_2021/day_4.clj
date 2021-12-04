(ns advent-2021.day-4
  (:require
    [advent.util :as util]
    [clojure.string :as str]))


(defn parse [in]
  (let [[drawn _blank & boards] (map util/trim-to-nil
                                     (str/split-lines in))]
    {:drawn (map #(Long/parseLong %)
                 (str/split drawn #","))
     :called []
     :called-set #{}
     :winners []
     :boards (into []
                   (comp (remove #(nil? (first %)))
                         (map (fn [ls]
                                (mapv (fn [l]
                                        (mapv #(Long/parseLong %)
                                              (str/split l #"\s+")))
                                      ls))))
                   (partition-by nil?
                                 boards))}))


(defn winner? [called-set b]
  (let [win? (fn [s]
               (some (partial every? called-set)
                     s))]
    (boolean (or (win? b)
                 (win? (util/transpose b))))))


(defn step [{[d & r] :drawn
             ws :winners
             c :called
             cs :called-set
             bs :boards}]
  (let [c' (conj c d)
        cs' (conj cs d)
        {w true
         l false} (group-by (partial winner? cs')
                            bs)]
    {:drawn r
     :called c'
     :called-set cs'
     :winners (into ws w)
     :boards l}))


(defn score [called called-set b]
  (* (peek called)
     (util/sum (sequence (comp cat
                               (remove called-set))
                         b))))

(defn part-1 [dat]
  (some (fn [{c :called
              cs :called-set
              [w] :winners}]
          (when w
            (score c cs w)))
        (iterate step
                 dat)))


(defn part-2 [dat]
  (some (fn [{c :called
              cs :called-set
              ws :winners
              bs :boards}]
          (when (empty? bs)
            (score c cs (peek ws))))
        (iterate step
                 dat)))

(comment
  (def tin "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
  (def in "4,77,78,12,91,82,48,59,28,26,34,10,71,89,54,63,66,75,15,22,39,55,83,47,81,74,2,46,25,98,29,21,85,96,3,16,60,31,99,86,52,17,69,27,73,49,95,35,9,53,64,88,37,72,92,70,5,65,79,61,38,14,7,44,43,8,42,45,23,41,57,80,51,90,84,11,93,40,50,33,56,67,68,32,6,94,97,13,87,30,18,76,36,24,19,20,1,0,58,62\n\n34 90 18 33 83\n27  7 25 61 15\n43  5 51 32 45\n24 17 72 31 22\n77 46 78 16  9\n\n72 95 37 52 68\n80  1 73 96 63\n16 49  9 42 97\n25 81 20 11 46\n31 24  2 34 18\n\n88 29 95 98 57\n49 36  6 23 83\n18  5 45 40 44\n62 81 74 99 87\n46 56 35 21 52\n\n34 51 57 55 58\n 3 30 35 92 69\n56 53 86 40  4\n46 71 43 29 18\n 6 15  9 60 83\n\n49 11 72 87 56\n40 94 71 70  3\n65  2 90 64 63\n32 79 24 44 55\n58 53 35 77 60\n\n78 77 89 45 67\n32 35 18  1 60\n61 25 71 56  2\n44 27 73 82 79\n40 53 84 23 16\n\n 9 70 11 95 14\n77 54 55 10 97\n88 89 41  8 43\n87 37 20 67 86\n74 24 30 75 25\n\n30 36 41 93 66\n27 23  7 40 92\n73 29  1  2 45\n68 95  0 14 59\n78 70 54 64 25\n\n35 75 54 52 86\n32 60 72  6 79\n45 26 77 83 41\n74 29 58 19 18\n49 63 31 61 88\n\n 0 70 66  5 92\n85 55 65 19 24\n69 73  3 38 79\n58 94 84 22 16\n68 91 59 28 88\n\n45 59 73  3 82\n 7 79 55 62 49\n89 32 99 69 19\n10 26 63 36 27\n28 83 43 64  4\n\n53 28 61 95 63\n39 78 38 50 26\n46 91 70  6 98\n97 87 27  8 25\n 3 36 48 24  7\n\n93 55 34 14 16\n88 92 19 86 53\n37 79 25 21 67\n22 13 10 97  7\n44  8 94 39 64\n\n41  8 70 13 23\n16 33 55 89 12\n94 98 28 91 38\n 3 32 82 71 90\n92 15 76 86 80\n\n34 99 57 29 48\n89 18 67 90 96\n12 95 93 41  9\n25 78 97 59 65\n87 38 91 61 17\n\n31 96 70 24 11\n64 54 52 47  7\n86 27  1 85 44\n77 99 81 97 90\n21 82  6 83 41\n\n98 59 95 45 34\n51 66 49 35 47\n83 13 28 94 32\n10 31 96 22 70\n62 48 42 44 16\n\n61 44 52 12  7\n 5 24 58 53 51\n95 79 83 11 36\n 1 25 94 55 27\n89 59 88 39 70\n\n30 81 65 77 82\n50 53  6 89 85\n12 33  1 72 11\n48 29 23 60 20\n67 27 95 61 28\n\n44  6 72 94 32\n61 81  1 23 78\n67 22 15 24 12\n98  9 71 17  8\n25 50 93 92 56\n\n89 63  3 53 90\n65 66 52 62 92\n55 59 42 71 49\n67  6 22 25 46\n10 94 84  1 77\n\n16 34 96 63 56\n77 32 84 90 49\n52 44  9 95 94\n 3  1 83 39 24\n11 75 17 61 42\n\n12  3  9 55 46\n20 68 26 34  4\n42 49 31 28 87\n51 86  0 15 44\n21 97 98 50 95\n\n21 92 59 50 46\n93 60 95 75 44\n17  1 15 38 25\n96 49 28 76 83\n18 71 48 63 41\n\n57 29 72 50 46\n95 93 92 35 74\n39 73 48 60 89\n80 70 55 76 43\n86 14 75  5 81\n\n78 54 48 69 13\n29 88 23 33 67\n28 16 59 40 14\n79 11 94 68  8\n58 50 72 91 92\n\n80 63  8 58 71\n45 83 52 98 36\n23  7 56 47  0\n99  1 94 76 54\n24 82 92 97 50\n\n95 35 62  5 43\n12 69 85 90 17\n98 20 71  1 34\n13 48 87 78 29\n51 82 28 26 24\n\n84 29 33 85 54\n 8 49 10 24 35\n67 15 37 34 32\n 1 48 81 89 94\n88 25 42 50 74\n\n83 80 22 64 45\n66 68 23 89 30\n75 61 90 50 55\n99 42 17 54 77\n46 72 13 47  1\n\n 7 22 27  6 71\n29 41  9 32 47\n84 67  2 92 53\n36 12 56 68 11\n74 48 38 96 51\n\n85 53  8 73 41\n48 70  7 88 89\n87 63 11 32 12\n33 61 96 65 18\n52 97 20 45 67\n\n70  0 51 30 37\n23 46 55 98 77\n 9 38 19 63 76\n 4 91 33  8 60\n92 67 39 47 85\n\n19  9 17 28 26\n24 99 87 46 93\n16 97 41 96 13\n44 95 56 77 98\n15 61  7  5 58\n\n65 11 68 97 95\n36 19 31 40 76\n12 79 27 34  9\n 6 30 78 96 74\n42 93 49 89 20\n\n47 93 71 33 80\n62 70  4 32 21\n51 72 38 36 48\n40  7 76 16 60\n94 83 69 15 25\n\n85 39 23 25  5\n47 55 75 45 42\n96 18 84 71 92\n 0 19 56 29 14\n58  6 79 21 43\n\n26 28 59 55 49\n48 91 50 33 94\n78 97 43 71 17\n 0 95 93 36 80\n68 18 54 75 53\n\n99 69 93 81 46\n73 66  4 96 24\n45 82  6  0 19\n83 62 58 18 27\n94 52 22 70 37\n\n68 53 35 29 56\n64  8 85 99 41\n86 23 17 83  4\n28 72 50 74 19\n32 11 60 12 39\n\n26  5 42 44 70\n46 86  2 28  1\n33 93 67 50  8\n36  7 57 48 71\n 3 80 45 49 15\n\n66 47 14 37 52\n71 32 95 28 50\n58  1  0 51 30\n44 11 79 74 75\n46 64 26 29 13\n\n25  0 44 52 11\n18 68  9 81  1\n42 41 55 63 91\n10 87 53  7 17\n90 24 49 21 99\n\n12 73 26 13 98\n60  5 17 11 52\n 9 65 33 78 51\n91 99  6  8 55\n29 49 87 21 67\n\n93 25  7 11 96\n22 48 46 75 90\n19 28 77 81 54\n 2 31 16 14 32\n27 36 52  5 64\n\n12 26 90 61 10\n35 75 53 13 51\n50  9  5 71 15\n32 95 55  4 78\n98 48 94 19 27\n\n64 75 20 77 82\n46 63 83 69 41\n44 15 73 35 61\n99 71  4 43 72\n76 81 93 23  0\n\n46 49 42  7 71\n39 82  2 61 11\n87 81 67 57 85\n52  6 92 19 98\n72 76 99 45 96\n\n 4 96 46 42 91\n78  2 52 22 51\n63 65 37 19 45\n 7 77  5 87 36\n 8 55  9 56 97\n\n92 81 27 41 10\n93 35 39 84 57\n19 11 28 97 33\n 4 64 95 40 30\n 9 20 29 82 96\n\n42 21 70 94 18\n66 15 11 79 89\n41 13  6 27 77\n56 37  3 16  8\n 4 28 24 96 10\n\n36 54 66 95 53\n10 92  1 38 44\n 0 26 84 13 48\n99  6 17 34  9\n22 50 33 12  7\n\n45 70 25 99 66\n77 19 28 75 93\n58 95 72 38 37\n 7 98 24 68 15\n61 29  1 55 97\n\n 8  6 61 67 60\n14 53 78  4 66\n54 77 39 79 73\n88 99 41 70 26\n49 86 40 69 16\n\n18 94 52 49 97\n36 38 66  5 34\n48 11 95 92 62\n58 98 33 28 76\n24 25 43 69 10\n\n68 17 51 38 80\n66 85 33  7 40\n24 65 73 29 75\n45 42  0 46  5\n84 54 67 86 19\n\n18 47 72 17 44\n33 66 19 50  2\n39 28 48  5  4\n97 29 30 80 67\n46 55 84 99 59\n\n52 65 63 69 68\n67 25 27 46  4\n42 10 37 99 81\n29  3 93 91 96\n36 19 70 75 88\n\n25 15 63 32 17\n82 61 65 34 45\n92  4 33 80 54\n55 70 20 28  8\n44 52 23 26 11\n\n30 35 52 81 47\n82 33 62 97 34\n90  1 86 88 68\n36 77 18 12 93\n 9 91 43 87 24\n\n93 26 16 30  7\n 9 72 58 74 10\n62 12 80 27 43\n51 40 55  3 83\n28 81 56 52 49\n\n64 56 85  9 70\n93 65 50 42 89\n11 81 52 57 99\n44  8 73 45 47\n35 54 86 12 58\n\n 4 63  6 17 59\n86 81 65 55 36\n16 30 34 79 20\n80 98  2 88 40\n94 23 69 28 27\n\n 0 76 35 44 27\n14 70 87 63  7\n81 80  4  1 69\n33 26 73 25 18\n86 66  3 85 92\n\n21 27 16 80 30\n39 79 12 24 47\n15 46 90 13 33\n28 49 36  8 34\n72  6 87 44 59\n\n82 99 44 76 59\n42 53 39 47 80\n58 78 68 84  9\n97 65 74 95 14\n55 54 32 50 19\n\n50 37  1 67 84\n24 68 34 41 55\n43 66 85 35 88\n58 40 71 10 32\n78 62 94 14 33\n\n89 81 79 16  6\n86 91  7 31  1\n47 84 46 23  8\n66 64 48 27 69\n97 73 22 60 80\n\n25 76 42 48 67\n 0 99 96  5 82\n86  8 24 28 90\n21 92 56  4 53\n74 61 15 12 50\n\n67 16 88 98  0\n10 15 99 96 56\n43 51 13 58 97\n94 57 28 29 23\n40 32  4  9 17\n\n86 30  2 35 46\n88 60 57  9 45\n70  3 92 80 18\n11 32 48  1 23\n12 19 74 39  6\n\n62 78 16 48 74\n88 49 43 44 35\n87 51 99 17 86\n92 28 70 19 18\n25 80 50 52 24\n\n64  9 77 44 31\n11 23 33 62  7\n14 53 84 41 12\n87 48 34  0 63\n71 91 78 27 29\n\n49 42 54 43 98\n 7 32 51 62 40\n31 69 17 89  8\n46  5 64 10  1\n41 73 99 94  6\n\n22 84 32 80 30\n99 74 60 78 10\n83  4 63 77 67\n17 44 54  6 90\n14 91 55 53 18\n\n53 41 12 91 90\n23 66 67 57 11\n55  5 58 16 62\n61 44 21 95  0\n45 59 20 96 29\n\n40 46 68 90 56\n 9 17 13 20 59\n76 91 51 39 99\n72 42 16 69 27\n30 52 71  3 66\n\n 0 76 19 13 21\n26 72 12 27 11\n65 68 30 39 90\n22 59 49 80 52\n66 23 25  6 24\n\n85 64 17 38 66\n 2 99 33 50 52\n58 42 15 86 47\n19 81 16 43 92\n28 30 59 80 36\n\n53 46 30 91  5\n17  8 62 31 12\n45 52 56 41 97\n87 65 26 63 36\n58 42 86 68 92\n\n72 94 25 75 15\n48 61 68 38  9\n 1 81 77 96 51\n86 82 58 89 70\n90 78  8 63  5\n\n45 58 41 27 61\n44 26 18 13 72\n77 39 81 74  5\n59 76 96 42 55\n53 48 79 49 83\n\n19 63  3 71 35\n46 13  1 84 30\n25 81 83 27 49\n 2 74 93 48 98\n78 34 68 29 26\n\n87  3 46 88 19\n14  8 23 34 60\n90 15 75 51 74\n53 35 94 11 84\n49 12 18  1 64\n\n 3 26 34 67 45\n13 85 32 21 51\n73 44 11 15  6\n46 80 35 18 64\n29 38 39 54 94\n\n61 25 82  7 22\n39 87 75 81 18\n37 20 17 11 52\n91 70 35 71 30\n86 43 40 58  0\n\n10 98 82 53 94\n93 29 81 45 80\n88 15  6 24 14\n25 65 84 54 59\n58  7 16 48 22\n\n39 96 24 18 80\n43 29 26 87 41\n35 16  3 28 56\n12 95 42 92 38\n45  0 68  1 10\n\n50 72 88 42 69\n53 37 63 43 34\n26  6  7 49 87\n66 38 28 65 18\n 5 35 80 15 59\n\n30 65 33 48 97\n43 29  0 73 52\n26 35 36 28 39\n58 51 82 16 75\n12 62 55 83 19\n\n 0 88 72  2 19\n91 76 10 16 97\n31 37 39 67 53\n83 54 93  5 18\n63 45 17 78 80\n\n68  9 48 64 70\n61 95 81 50 15\n84 38 42 51 96\n65 29 39 41  7\n 5  2 12 23 56\n\n82 97 57 86 52\n93 45 89  9 23\n41 32 61 16  2\n 0 26 71 51 28\n14 66 40 75 74\n\n53 27  6 92 81\n37 45 91 78 65\n41  4 86 97 69\n40 58 26 14 28\n47 30 46 95 98\n\n39 75 72 69 78\n29 31 36 23 19\n79 57 49 34 22\n87 54 77 11 26\n76  3 83 18 15\n\n58 84 73 91 83\n 6 52 81 92 76\n23 18 62 66 77\n79 89  4 41 72\n59 36 13  8 31\n\n81 32 67 75 44\n96  3 90 11 46\n61  1 14  2 86\n89 24 53  8  7\n26 20 18 59 42\n\n71 77 24 72 43\n41 38 62 21 36\n70 10 37 60 83\n79 94 39  0 51\n73 46 98 34 50\n\n96 26 95 48 17\n97 40 42 53 35\n74 67 47 22 23\n14 70 54  8 66\n52 31 63 34  1\n\n96 55 84 44 10\n57 80 30 52 72\n42 40 36 41 73\n85 38 64 63 13\n47 16  8 76 94")

  (part-1 (parse in))
  (part-2 (parse in))

  )
