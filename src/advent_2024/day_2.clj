(ns advent-2024.day-2
  (:require [clojure.string :as str]))

(defn parse [s]
  (map (fn [l]
         (read-string (str "[" l "]")))
       (str/split-lines s)))


(defn safe? [row]
  (and (or (apply < row)
           (apply > row))
       (every? (fn [diff]
                 (<= 1 diff 3))
               (map (fn [[a b]]
                      (abs (- a b)))
                    (partition 2 1 row)))))


(defn part-1 [in]
  (count (filter safe?
                 in)))


(defn part-2 [in]
  (count (filter (fn [row]
                   (some safe?
                         (map (fn [i]
                                (let [[f s] (split-at i row)]
                                  (concat (butlast f)
                                          s)))
                              (range (inc (count row))))))
                 in)))



(comment
  (def t "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")

  (part-2 (parse t))


  (let [row [1 2 3 4 5]]
    (map (fn [i]

           (let [[f s] (split-at i row)]
             (concat (butlast f)
                     s)))
         (range (inc (count row)))))

  (part-2 (parse "55 56 59 62 61\n68 70 71 74 75 76 78 78\n52 55 56 58 62\n73 76 79 82 84 85 87 94\n1 4 5 6 3 4\n77 80 78 80 83 81\n69 72 73 71 71\n14 15 17 18 17 21\n39 42 43 45 47 46 53\n72 73 73 74 75\n48 49 51 51 53 54 51\n39 40 41 41 43 43\n14 16 19 20 23 23 25 29\n69 70 70 73 80\n79 80 81 84 88 89 92 93\n7 10 13 17 18 16\n67 70 73 75 79 81 84 84\n81 82 84 88 89 93\n76 79 81 83 84 88 94\n71 72 73 80 82 83\n24 26 29 36 33\n69 70 72 73 76 81 81\n6 9 12 17 21\n45 46 52 55 58 59 66\n24 23 24 25 26 29 32\n45 43 45 48 49 51 50\n3 2 5 6 7 9 11 11\n27 25 26 29 31 32 33 37\n17 15 18 19 22 24 30\n69 68 71 70 72\n38 36 38 41 44 41 38\n16 14 12 15 15\n37 34 36 33 36 39 40 44\n80 77 78 81 79 86\n57 55 57 57 60\n31 29 30 30 28\n77 76 79 79 79\n75 73 74 76 76 80\n14 11 11 12 15 22\n72 69 72 74 78 80 81\n80 77 79 83 85 87 86\n32 29 33 36 37 39 41 41\n7 5 9 10 14\n18 17 20 23 24 26 30 35\n86 84 87 89 94 95 98\n32 29 30 37 39 38\n27 26 28 30 31 38 41 41\n20 18 25 27 31\n84 81 83 88 89 94\n21 21 24 27 28 31 32\n15 15 18 21 24 22\n42 42 43 46 47 47\n35 35 38 39 43\n61 61 63 65 67 74\n23 23 21 24 27 29\n8 8 6 9 10 9\n84 84 83 86 86\n67 67 69 71 73 70 74\n85 85 84 87 93\n9 9 11 14 17 17 20\n33 33 34 34 31\n65 65 68 68 71 71\n60 60 62 62 63 67\n11 11 14 14 20\n18 18 21 24 26 28 32 34\n43 43 44 47 51 52 55 52\n58 58 61 65 67 68 68\n55 55 57 61 63 67\n72 72 76 78 79 80 85\n1 1 2 9 12 13\n80 80 83 89 90 93 92\n24 24 26 33 36 38 38\n17 17 18 19 26 30\n21 21 23 26 33 34 37 44\n30 34 37 39 41 44\n24 28 30 31 32 35 38 36\n84 88 90 91 92 95 95\n60 64 67 70 73 76 79 83\n9 13 15 16 19 21 22 27\n46 50 51 52 49 50 51 53\n68 72 71 73 74 72\n14 18 16 19 21 23 24 24\n50 54 55 58 55 58 62\n76 80 82 81 82 85 88 93\n66 70 73 73 76 78 81 83\n37 41 42 42 43 45 43\n7 11 14 14 14\n5 9 11 11 12 16\n10 14 17 17 18 21 22 28\n32 36 38 42 43\n51 55 59 62 63 64 65 63\n14 18 19 20 24 27 29 29\n33 37 38 39 41 45 46 50\n74 78 82 85 88 89 96\n56 60 65 67 69\n30 34 36 38 43 42\n74 78 79 80 85 85\n75 79 86 87 89 91 93 97\n78 82 84 90 92 93 99\n65 72 75 77 80 82\n82 89 91 93 94 95 93\n49 56 59 62 62\n18 25 27 28 30 34\n46 52 54 56 57 62\n13 19 21 22 23 21 23\n26 32 29 31 32 31\n91 96 98 95 95\n47 52 54 51 53 55 58 62\n79 84 81 82 83 90\n75 82 82 83 86\n76 83 83 84 86 84\n9 14 17 17 17\n26 32 35 37 39 39 40 44\n40 45 45 46 53\n58 65 69 71 72 74\n34 40 44 46 48 45\n46 53 55 56 60 63 63\n18 25 29 30 34\n4 9 10 11 15 22\n51 56 61 63 64 67 70\n13 18 23 25 28 31 32 29\n16 21 27 28 28\n25 32 34 41 43 47\n29 34 37 44 47 50 55\n36 34 33 31 28 25 22 23\n27 25 22 21 21\n15 12 9 7 6 5 1\n47 46 45 44 37\n80 78 79 77 74 73 70\n16 13 11 8 5 2 4 6\n51 50 48 49 47 46 43 43\n21 18 19 17 15 13 9\n41 40 38 40 39 33\n9 8 6 6 5\n44 41 41 40 38 41\n99 96 93 93 93\n40 38 36 36 33 30 27 23\n27 25 24 24 22 17\n93 92 88 86 85 83 81\n49 46 45 41 38 41\n71 69 65 63 61 59 58 58\n51 50 48 44 41 40 38 34\n44 41 37 34 32 31 30 25\n14 13 8 5 4 2\n32 31 28 21 20 19 16 18\n39 36 35 32 27 26 26\n49 48 46 39 35\n56 54 53 47 40\n54 55 54 52 49 48 47\n78 79 78 75 74 73 76\n95 96 94 91 89 86 85 85\n73 74 72 70 69 65\n91 93 92 90 88 83\n81 84 83 86 84 83\n48 50 49 47 49 48 45 46\n88 90 88 87 89 87 87\n47 49 48 46 43 46 42\n47 48 47 46 48 42\n23 24 21 18 18 17\n66 67 67 65 68\n50 53 52 51 51 48 48\n89 92 92 90 89 85\n92 93 91 88 87 85 85 80\n88 91 88 86 82 80\n74 77 73 72 73\n30 32 29 26 24 20 19 19\n88 89 88 84 80\n42 44 40 38 35 34 27\n94 95 94 88 86\n18 19 17 14 7 4 5\n70 72 69 63 62 62\n28 30 28 27 26 19 17 13\n57 60 58 56 55 50 44\n68 68 66 63 61\n45 45 42 41 42\n76 76 74 71 68 68\n81 81 79 78 77 73\n98 98 96 95 92 87\n58 58 57 60 57 54 53\n92 92 89 87 89 87 86 89\n83 83 80 79 82 80 80\n52 52 50 47 45 46 43 39\n41 41 43 40 38 33\n57 57 56 53 52 52 50 49\n88 88 85 84 83 83 84\n72 72 70 67 65 65 65\n61 61 61 60 56\n61 61 58 57 55 53 53 48\n59 59 58 54 53 52 51 50\n45 45 44 43 41 37 39\n78 78 76 74 73 69 69\n86 86 82 81 79 76 73 69\n36 36 32 30 23\n24 24 23 22 21 18 11 8\n15 15 13 6 4 5\n41 41 38 33 32 32\n13 13 7 5 1\n74 74 71 68 65 60 54\n88 84 82 79 78 75 72 70\n91 87 84 81 80 79 82\n74 70 68 67 67\n58 54 53 52 48\n95 91 89 86 79\n51 47 50 49 48 45 44\n36 32 31 29 31 29 26 29\n86 82 80 81 80 77 77\n43 39 36 39 36 32\n77 73 76 74 71 68 66 60\n95 91 91 88 85\n12 8 8 6 5 7\n53 49 48 48 47 46 46\n30 26 26 25 21\n68 64 62 60 58 58 57 51\n45 41 40 36 35 34\n23 19 15 12 15\n38 34 30 27 27\n54 50 46 45 43 40 36\n37 33 29 27 24 19\n56 52 51 49 43 42\n66 62 56 55 52 49 51\n65 61 58 56 54 51 44 44\n70 66 65 62 59 56 50 46\n98 94 92 85 83 82 81 75\n52 45 43 41 39\n43 38 37 34 37\n57 51 50 49 48 47 44 44\n38 31 29 27 26 23 19\n23 16 14 12 7\n36 30 31 29 28\n12 6 8 5 8\n20 13 12 11 12 9 9\n93 86 84 83 84 82 80 76\n55 48 49 47 42\n17 11 9 6 6 3\n32 26 26 23 20 23\n89 84 83 83 83\n79 72 70 70 66\n56 49 48 48 45 40\n61 54 50 48 45 44\n24 18 16 12 11 9 10\n69 62 61 57 57\n34 28 27 23 20 19 15\n85 78 74 72 70 69 66 61\n59 53 52 51 46 44 43 42\n65 58 51 48 49\n98 91 86 83 83\n48 42 41 35 33 29\n47 41 40 37 31 28 21\n52 53 56 59 60 58\n62 63 65 66 67 67\n62 65 66 68 71 75\n42 44 46 47 48 51 53 59\n89 91 94 93 96 99\n29 32 34 35 36 34 31\n28 29 26 28 31 31\n74 75 73 75 77 79 81 85\n46 47 44 47 48 54\n4 6 8 8 11 12 15 18\n80 81 81 82 79\n51 52 54 56 56 59 59\n49 51 51 53 56 60\n47 50 53 55 55 62\n6 8 12 15 17 19 21\n86 89 90 92 96 98 99 96\n77 78 80 83 87 87\n55 58 60 63 66 70 71 75\n55 57 61 64 69\n83 84 85 92 93 95\n77 78 83 84 83\n26 29 34 37 37\n20 21 23 26 27 33 35 39\n28 29 30 35 36 37 39 45\n62 61 64 67 68 70\n20 18 21 23 26 28 31 29\n72 70 73 76 79 80 80\n48 46 49 52 53 55 56 60\n9 6 7 8 9 10 13 19\n69 68 65 66 69 72 75\n40 38 39 40 39 40 38\n50 48 49 48 48\n81 79 80 79 83\n15 14 13 16 18 23\n10 9 12 14 14 16 18 20\n96 95 98 98 99 96\n79 77 79 79 79\n61 59 61 62 63 63 66 70\n17 16 17 18 19 19 21 28\n22 21 25 27 28\n35 33 34 38 41 39\n62 61 64 65 69 69\n46 44 47 51 53 55 59\n46 44 46 49 50 51 55 62\n41 39 46 48 50\n39 38 45 46 45\n78 75 82 85 85\n22 21 23 26 28 34 38\n6 5 12 13 15 18 24\n58 58 59 62 65\n21 21 23 24 26 29 32 31\n96 96 98 99 99\n84 84 85 87 91\n68 68 71 74 81\n6 6 7 10 11 10 11 14\n31 31 33 36 38 36 34\n54 54 55 57 59 61 60 60\n31 31 33 35 34 37 41\n25 25 27 24 25 30\n82 82 85 85 86 88 90\n39 39 41 43 43 46 45\n39 39 42 43 46 46 46\n88 88 91 91 95\n81 81 83 85 85 86 87 93\n36 36 40 41 43 44 46\n2 2 5 9 8\n8 8 11 12 16 18 18\n79 79 81 85 89\n47 47 50 54 61\n77 77 79 81 88 89\n60 60 63 66 68 74 75 74\n76 76 83 85 87 87\n34 34 41 43 47\n65 65 68 71 76 81\n3 7 8 11 12 14 17\n53 57 59 60 61 60\n28 32 33 34 36 37 37\n45 49 51 53 54 57 61\n55 59 61 63 68\n43 47 50 51 50 53 55 57\n1 5 7 10 13 12 11\n43 47 48 51 48 51 51\n80 84 82 83 85 89\n38 42 44 45 46 44 49\n41 45 47 48 50 53 53 56\n2 6 6 8 11 12 14 12\n10 14 17 20 20 21 24 24\n12 16 18 21 23 23 26 30\n30 34 37 39 42 44 44 49\n53 57 59 62 64 68 70\n68 72 76 77 75\n9 13 16 20 22 22\n78 82 83 87 89 91 92 96\n12 16 17 19 23 25 28 35\n76 80 86 89 90 93\n86 90 91 92 98 99 98\n61 65 68 71 76 76\n24 28 30 36 40\n31 35 37 40 41 46 49 56\n43 50 51 52 55 58 60\n43 48 51 54 56 59 58\n55 62 65 68 70 71 74 74\n10 16 19 20 21 23 25 29\n80 87 89 92 97\n91 96 94 97 99\n82 87 88 85 87 86\n16 23 20 21 24 24\n20 25 22 25 28 32\n27 34 37 36 37 38 45\n80 85 88 91 93 96 96 99\n47 52 54 54 57 60 58\n62 68 70 71 73 73 73\n21 28 29 29 31 34 35 39\n40 45 46 46 47 52\n61 68 70 74 77 79 80 81\n76 81 84 85 87 88 92 91\n79 85 86 90 90\n57 62 65 69 73\n11 18 21 25 28 29 31 37\n74 79 86 87 89 90 91 93\n5 12 14 19 21 23 24 22\n36 43 45 47 48 53 53\n27 34 39 40 44\n64 69 75 78 81 82 88\n55 54 52 51 52\n73 70 68 67 67\n83 82 81 80 79 76 73 69\n24 23 22 19 18 16 14 8\n99 97 95 96 95 92\n23 20 21 20 19 17 14 15\n25 23 24 23 21 18 18\n53 51 49 47 48 46 42\n87 84 82 85 80\n95 93 91 88 88 86 83\n90 87 85 84 83 83 85\n69 66 63 63 61 60 60\n13 11 11 10 6\n59 57 57 56 51\n43 40 37 35 33 29 26 25\n14 13 9 8 6 5 8\n77 74 72 70 66 64 64\n46 44 41 37 33\n59 58 54 51 46\n55 52 45 43 42 41 40\n39 36 35 34 33 28 29\n47 45 40 39 39\n47 45 43 37 35 33 32 28\n96 94 92 87 84 81 79 72\n43 46 45 44 42 40 37\n79 82 80 77 74 73 71 74\n42 44 43 41 40 37 37\n57 58 57 56 53 52 48\n78 79 78 75 74 71 66\n31 32 31 34 31\n25 28 27 29 30\n37 38 37 35 36 34 34\n68 71 70 73 71 67\n20 22 25 22 21 14\n98 99 98 95 95 94\n84 86 83 82 82 80 77 80\n57 59 57 57 56 55 55\n24 27 25 25 22 19 16 12\n53 54 51 48 48 45 38\n28 29 27 26 22 19\n42 44 43 39 36 38\n88 91 90 89 86 82 82\n91 94 92 88 86 82\n27 29 27 23 20 18 11\n16 17 16 10 9\n69 70 67 60 58 57 56 58\n28 31 30 27 26 19 18 18\n58 61 58 57 50 47 43\n69 72 71 66 64 63 61 55\n49 49 47 45 43\n38 38 35 32 29 32\n62 62 59 58 57 56 53 53\n79 79 78 77 73\n28 28 27 25 24 22 16\n49 49 51 50 47 46\n37 37 36 34 31 33 36\n36 36 37 35 35\n50 50 47 45 46 43 42 38\n39 39 38 36 39 34\n28 28 28 27 24 22 21 19\n21 21 19 17 15 15 13 14\n30 30 30 28 25 23 20 20\n79 79 79 78 75 73 70 66\n47 47 47 46 40\n72 72 70 66 64 62\n49 49 45 44 45\n82 82 80 76 74 72 72\n26 26 22 20 16\n71 71 67 64 63 60 54\n95 95 93 87 86\n15 15 14 13 12 6 8\n69 69 66 64 58 58\n36 36 34 33 26 23 22 18\n22 22 17 15 10\n33 29 27 26 25 22\n25 21 18 15 14 17\n71 67 64 61 61\n71 67 65 62 59 58 54\n89 85 82 79 77 74 72 67\n52 48 47 50 47 46 44 42\n55 51 49 46 49 46 48\n8 4 2 5 5\n86 82 84 83 79\n38 34 32 31 28 29 24\n49 45 42 40 40 38 35 32\n24 20 17 15 15 12 15\n72 68 65 65 65\n67 63 61 58 56 53 53 49\n45 41 40 38 36 36 29\n44 40 37 33 30 28\n24 20 17 13 11 8 10\n60 56 54 50 49 48 48\n30 26 25 21 17\n26 22 20 17 13 12 5\n32 28 26 20 17\n43 39 38 33 30 28 26 29\n94 90 85 82 82\n81 77 71 68 66 62\n99 95 94 89 88 81\n21 16 15 14 13 12\n83 76 75 74 73 70 69 71\n73 68 66 65 63 62 59 59\n44 37 35 32 28\n91 86 83 82 81 79 78 72\n96 90 92 89 86\n33 27 25 28 25 23 24\n32 27 24 22 20 23 20 20\n64 57 60 57 53\n43 36 33 30 28 31 26\n10 5 5 3 2\n65 60 58 58 55 58\n69 63 63 61 61\n70 65 62 61 61 57\n56 51 50 48 45 45 42 36\n75 70 66 65 62\n17 11 7 5 6\n36 29 25 22 20 19 18 18\n26 20 18 14 13 9\n43 38 35 34 32 30 26 19\n66 60 54 52 49\n54 47 41 38 37 35 36\n40 33 26 25 25\n31 26 24 22 19 13 12 8\n80 73 71 68 62 57\n11 8 9 16 17 19 22 22\n25 25 27 26 25 24 22 20\n71 75 78 82 85 88 91\n24 26 29 33 34 37 40 42\n39 45 45 46 47 49 51\n42 42 44 45 48 52\n92 95 92 92 92\n53 56 55 53 53 50 46\n79 75 72 71 70 67 67\n26 26 23 19 18 14\n86 85 88 91 92 95 96 98\n35 35 38 37 38\n12 12 13 11 6\n27 23 26 24 23 23\n35 39 42 43 46 49 52 54\n30 24 24 21 18 15 14 14\n61 55 52 50 48 46 43 46\n9 7 9 10 8\n67 68 66 59 55\n52 56 59 60 63 64 68 65\n11 14 16 19 22 29 30 35\n52 58 59 65 66\n40 45 48 53 55 59\n47 47 50 52 56 58 62\n37 41 41 44 47 50\n22 19 16 15 18\n21 21 23 27 30\n68 72 73 74 76 77 80 84\n61 66 67 71 72 75 73\n60 63 62 55 54\n46 50 53 55 55 59\n60 58 59 58 55 51\n72 68 61 58 56 55 58\n25 24 27 28 30 34 38\n14 17 18 20 22 25 23 23\n47 40 37 35 34\n57 58 59 65 67 66\n74 69 62 61 60 57 58\n4 10 14 16 17 20\n13 18 19 20 19 22 29\n54 54 51 50 45\n72 69 71 73 70 74\n84 79 77 80 79 74\n25 25 25 24 22\n86 86 85 85 82 80 77 79\n68 65 67 72 73 74\n29 29 27 27 24 21 21\n59 61 63 62 65 69\n45 42 41 38 36 32 31\n45 38 39 37 33\n79 76 76 73 72\n31 32 31 27 21\n9 9 8 9 10 12 14 17\n47 51 54 57 57\n26 25 26 29 30 37\n91 90 88 85 81 78 74\n53 56 52 50 50\n51 47 46 43 42 39 33\n84 77 76 72 69 63\n41 40 39 41 39 38 37 37\n74 78 76 79 79\n64 64 59 57 56 56\n69 72 69 70 72 75 82\n27 32 35 36 34\n31 26 25 21 20 19 15\n67 67 70 76 79 84\n33 35 36 37 39 41 42 39\n35 32 32 29 29\n57 54 52 49 47 46 46 47\n1 5 8 10 12 13 10 13\n37 38 42 45 46 49 55\n86 91 92 94 94 92\n88 86 83 80 78 77 73\n3 5 6 10 10\n24 24 27 30 30 34\n17 15 13 16 18 15\n57 54 52 52 51 50 46\n46 43 45 46 49 51 51 52\n70 68 65 63 58\n49 55 59 60 60\n53 46 45 45 42 41 40 35\n47 51 52 58 65\n27 29 26 23 25\n23 19 16 10 8 5 1\n98 91 91 89 92\n59 59 58 55 54 53 51\n56 63 60 62 64 67 69\n3 9 9 11 14 14\n86 83 84 82 80 74\n74 77 80 80 83 85 88 88\n24 20 20 17 10\n56 62 64 65 66 69 73\n1 5 7 9 15 12\n60 63 67 70 69\n63 66 69 70 76 79 82\n38 38 36 34 32 30 31\n44 47 46 45 44 44 41 40\n66 66 67 64 71\n1 7 8 9 11 14\n13 16 15 14 12 11 11\n75 72 66 65 64 64\n56 59 56 59 57 56 52\n27 20 17 16 10 6\n19 23 27 30 30\n95 98 97 93 90\n33 37 37 40 42 40\n36 42 44 42 43 46 50\n94 88 82 79 78 71\n17 13 11 9 5 4 4\n88 88 89 91 92 95 95 92\n97 93 90 88 85 88 86 83\n15 12 12 14 17 24\n7 10 9 8 11 8 2\n59 55 54 51 47\n60 57 59 57 58\n74 79 82 85 89 94\n24 19 15 12 10 7\n12 7 5 2 2\n38 36 33 34 37 38 38\n36 32 32 31 27\n15 20 21 28 31 37\n79 79 80 82 80\n95 91 90 89 86 83 80 81\n89 86 86 87 89 93\n3 6 4 6 3 4\n65 61 59 56 55 53 52 51\n27 26 28 31 38 39 45\n81 84 87 91 95\n71 71 68 66 65 62 59 59\n28 28 27 25 25 19\n90 94 96 97 94\n63 63 67 68 69 66\n68 68 66 65 60 59 58 51\n92 92 86 85 82 80 76\n26 27 24 25 26 25\n73 78 84 86 88 91 91\n42 44 43 40 42 40 40\n73 80 82 85 85\n44 44 46 53 56\n29 25 22 20 16 15 18\n33 30 35 38 39 42 44 48\n87 87 85 80 79 78\n37 44 45 43 42\n87 87 89 91 95 97 97\n82 82 83 83 84 86 88 88\n90 90 88 90 92 95 97 97\n82 85 88 87 84 83 82 81\n70 70 73 75 77 83\n57 55 54 56 57 59\n26 19 16 11 9\n64 65 63 61 57 54 57\n81 85 86 87 92 93 95\n10 14 18 21 22 25 32\n70 66 63 61 58 52 49 46\n35 30 27 25 22 25 23\n71 77 77 79 81 82 83 90\n17 16 17 18 17 19 21 28\n31 30 23 21 24\n47 47 50 51 54 56 59 59\n74 74 71 73 70 67 64 60\n6 11 15 17 18 22\n59 55 55 53 51 51\n51 45 42 39 42 41 41\n22 29 32 30 32 32\n58 56 57 61 62 63 66 68\n16 16 12 9 7 6 8\n61 57 53 52 50 44\n60 58 55 58 55\n45 45 50 52 54 57 60 64\n31 28 29 31 35 37 40 40\n86 86 89 91 94\n32 25 23 16 13 13\n83 82 81 78 75 72 69 69\n52 56 59 61 62 63 61 66\n56 50 49 46 40\n15 18 18 17 20\n18 15 11 8 6 8\n72 72 71 67 62\n85 87 85 79 81\n36 39 37 35 33 31 31 24\n41 40 37 33 30 29 22\n63 67 67 69 71 72 79\n41 39 41 45 44\n20 23 25 26 28 29 32 36\n45 49 52 49 51 52 54 58\n48 46 40 37 30\n52 54 54 57 63\n8 11 14 17 19 18 19\n99 99 92 90 93\n14 12 13 14 20 21 24 22\n56 60 61 62 63 65 70\n38 34 36 33 31 30 28 24\n35 33 32 30 28 26 19 15\n66 68 65 62 55 48\n15 9 11 10 7 5 7\n82 82 85 92 95 97 97\n80 80 81 80 82 83 84 88\n40 36 38 35 33 28\n97 95 92 90 87 85 84\n76 77 80 83 84 87 88 90\n81 80 78 77 75 74\n94 93 91 89 88 86\n85 82 79 76 74 73 72 71\n73 71 69 66 64 63\n17 18 21 24 26 29 30\n72 73 76 79 82 84\n16 14 11 10 9 8 6\n25 27 30 33 35\n30 31 34 37 39\n82 84 87 90 93 95\n21 19 16 14 13 11 9\n43 46 49 50 52 55 57\n70 69 67 66 65 63 61 60\n29 26 23 22 20 17 14\n74 73 71 68 66 65 64\n85 83 80 79 77 76 73 71\n10 11 14 16 17 18 20\n47 44 42 39 38 35 32 30\n67 68 70 71 74 76 77 79\n35 37 40 43 45 46 48 51\n66 64 61 58 55 53 52 49\n76 79 80 81 82 83 86\n56 54 51 49 47\n67 64 63 60 58 57 54 52\n10 12 14 16 18 21 24\n79 82 83 84 86 87 89 90\n79 76 75 74 73 70 67\n48 51 52 53 54 57\n42 41 38 35 34 32 29 28\n2 5 6 8 10 11 14 15\n77 75 73 72 69 67\n81 84 87 90 92 93 94\n29 32 35 36 39 42 43 44\n25 24 23 20 19 17 16\n64 67 69 70 73 74\n17 19 20 21 22 24 25\n42 43 44 45 48 50 52 54\n45 42 41 38 37\n80 78 75 72 69 66 63 62\n8 9 11 14 15 18 20 22\n52 51 50 47 44\n23 20 19 18 16 15 14\n18 21 24 25 26\n86 88 89 91 93 94 95 97\n5 6 9 11 14 17 18\n74 73 72 71 70 68\n70 73 75 77 80\n56 59 60 61 62 64 67\n21 24 26 29 32 34 35 37\n31 34 36 37 38\n18 15 13 10 8 7 4\n36 39 42 44 45 47\n71 74 76 77 78 79 82\n31 34 36 38 39 42\n20 22 23 24 26 28 29\n72 71 70 67 64 62 61 58\n71 74 75 76 77 78 80 82\n49 50 53 56 57 58 61\n15 14 13 10 7 6 3\n28 26 24 21 19 16 14\n50 47 46 45 43\n37 36 35 32 30\n71 69 66 63 62 61 60\n62 64 66 68 69 72 74 76\n40 41 42 45 48 50\n20 17 16 15 12 11 10\n9 10 13 15 17 20 23 24\n38 39 42 43 44 45\n18 19 20 23 24 27 28 29\n5 8 9 12 14 16 17\n58 55 54 53 51 49\n15 18 21 23 25\n20 19 18 16 15 13\n69 67 66 63 60 58 57\n64 61 60 57 54 52 50 49\n20 22 25 26 27 28\n42 45 48 50 52 55 57\n18 17 15 13 12 10\n22 19 16 15 14 11 9\n67 64 61 58 55 53 51\n50 52 53 56 59 62 65 68\n52 55 57 58 60 62\n40 38 37 35 32 31 30\n69 68 66 65 63 61 58\n49 48 45 42 40 37 34\n37 36 34 31 28\n52 53 56 59 60\n39 41 42 43 45 48 51 52\n15 16 19 22 23\n84 86 89 91 92 95 97 99\n83 86 88 90 92 95\n63 64 67 68 71\n90 87 85 83 82 79\n56 58 60 62 63 64 66 69\n14 12 10 9 6\n43 42 41 40 37\n18 16 15 14 11\n57 58 59 61 64 66\n60 59 56 55 52 49 48\n19 21 22 23 26 29\n49 47 46 43 40\n84 85 87 89 91 94 96\n61 60 58 56 53 50 47 44\n66 67 70 73 75 78 80 81\n27 25 22 19 18 15 13 10\n44 47 48 49 52 53 54 56\n17 14 11 9 6 4\n94 91 88 86 85\n58 57 55 52 51\n90 89 87 85 84 81 80 78\n35 37 38 41 44 45\n38 39 40 41 43\n89 90 92 93 94 97 98\n62 59 58 55 54 52 51 49\n9 12 14 16 17 19 22 24\n94 92 89 86 85 84 81 78\n28 30 31 34 35 36 38 41\n59 61 64 65 66 69 70 72\n77 76 75 72 71 70 68 65\n55 54 52 49 48 47 44\n96 94 93 92 89\n40 41 42 45 46 48 50 53\n52 53 54 56 58 60 62\n98 97 94 92 89 88 87 86\n68 69 72 75 77 80\n49 48 47 44 41 39 36\n57 56 53 51 50 48 47\n11 14 15 17 20 23 26 28\n21 19 18 16 14\n49 51 54 57 60 63 66 69\n50 47 46 44 42 41 39\n45 42 41 39 36 33 32 30\n47 48 49 52 53 55 56\n79 80 82 83 85 86 87 90\n31 28 27 26 23\n3 6 7 10 13\n84 85 88 91 94 95 96\n59 57 55 52 50 49\n55 52 49 48 45 44\n13 14 16 18 19 20\n70 71 74 77 78 80 83\n36 38 39 42 45 46 49 50\n95 93 90 87 86 85 83\n14 17 20 22 25\n20 18 16 15 14 13 12 9\n32 34 35 36 39 40 41\n31 34 36 37 39 40 43\n73 72 70 67 64\n47 48 50 53 54\n91 88 85 84 83 81 79 77\n49 46 44 41 39 37 36\n20 19 18 17 16 15 12\n86 88 89 92 93 96\n78 80 81 84 87 88 90 91\n22 20 19 18 16 13\n2 3 6 7 10 11\n41 38 37 34 33 30 28\n28 27 24 23 21 20 17 14\n74 77 80 81 82 84\n69 67 65 64 63 62\n81 79 78 76 73 72\n54 53 52 50 47\n17 16 15 13 11\n57 59 60 62 64\n23 25 27 30 32 34\n29 26 25 24 23\n30 28 26 25 22 19 18\n70 67 64 61 58 57\n62 64 65 68 71\n83 86 89 92 93 94 96 99\n47 45 44 43 40 37 35 34\n47 48 49 52 55 57 59 60\n3 4 7 10 13 15 18\n64 66 68 69 70 73\n18 21 24 25 27 29\n73 70 68 65 62 59 58 56\n68 65 62 60 59 57 54 52\n11 8 6 3 2\n28 29 31 32 34 35 36\n37 38 41 44 46 48 49\n60 61 64 67 70 73 75 76\n22 23 25 28 29\n84 87 90 92 95 96 99\n31 33 34 35 37 40 41 42\n70 68 67 65 63\n82 84 87 88 90 92 94 95\n86 88 89 92 94\n72 69 68 67 65 64 63\n79 82 85 86 87 90\n29 30 32 33 36 37 39\n36 38 39 42 43 45 47 50\n70 67 66 63 62 60 58 55\n25 26 27 28 31 34\n82 80 77 75 74 73 72 70\n3 6 8 11 12 15\n29 30 33 35 38 41 42 43\n44 45 48 49 52 55 57\n47 49 51 53 56 58\n60 59 57 55 52\n22 25 28 29 31 33\n22 21 19 16 14 13\n19 21 24 26 29 32 34 37\n49 46 43 42 40 37 34\n40 37 35 33 31 29 27 25\n84 82 80 77 74 71 70\n75 73 70 67 64 61 59\n55 52 51 49 48\n71 74 76 77 79 81\n57 59 61 63 64 67 70\n55 54 53 52 50 49 46 45\n44 47 49 51 52 53 55 57\n84 87 88 91 94\n79 77 76 73 70 67 65 64\n48 47 44 42 39 38 36 35\n38 36 35 34 32 29 27 26\n58 61 64 67 70\n83 85 87 89 90 91 93\n70 69 68 66 64 62 60 58\n25 22 20 17 16\n59 62 64 67 70 73 74\n56 55 54 51 50\n36 37 38 41 43 44 47\n48 46 45 42 41 40 37\n41 40 39 37 35 33 31 29\n52 49 46 44 43 42\n72 73 75 76 78 79 81 84\n75 77 80 81 84 87 88 90\n28 29 31 32 34 37\n20 22 24 27 28 31 33 34\n39 37 34 31 29 26 23 20\n15 16 18 19 21 24\n36 38 40 41 44 47 48 49\n10 12 14 17 18 19 22\n27 24 23 22 19\n90 87 86 85 82 79 76\n34 35 36 37 40 42 45\n66 64 62 59 56 55 54\n60 62 64 65 66 67 70\n37 38 40 41 42 43\n79 78 77 74 73 70 68 65\n22 21 19 18 15\n95 94 93 92 89 88\n75 72 69 66 65\n25 22 19 16 14 13 12 9\n30 31 32 33 35\n5 6 7 9 10 13 14 17\n82 83 85 87 89 92 93 94\n54 52 51 48 46 45\n35 32 29 28 27 25 23 20\n51 49 46 43 42 40\n39 38 35 32 30 27\n48 45 42 40 38 37 35\n59 58 56 53 51 48 45\n76 75 73 70 68 66 63\n68 71 74 77 80 81 82\n28 29 31 34 35\n88 90 91 92 94 95 98 99\n69 68 65 64 62\n88 86 83 81 80\n85 88 89 92 93 94 96\n38 36 34 33 30 27 26 25\n69 70 71 73 74 77\n22 24 27 29 32 33 35\n80 81 83 85 87 89 91 92\n47 49 51 53 54 56 58\n45 44 41 38 36 33 30\n16 18 19 21 24 25 26 29\n69 70 73 76 77 80 82\n86 85 83 82 80 77\n7 8 10 11 12 13\n68 65 62 60 57\n20 17 16 14 13 12\n90 87 86 85 83 82 80 79\n20 18 16 14 13 10\n36 38 41 44 47 48\n64 66 69 70 72\n94 91 88 85 83 80\n25 26 29 32 33 36\n26 24 21 18 17\n63 60 59 56 54\n40 39 38 35 32 30\n60 63 65 67 69 71 72\n16 15 12 10 9\n49 51 54 57 58 59 62\n48 49 52 54 55 58 61\n36 35 32 31 29\n73 70 69 66 63 61 59\n16 18 19 22 25\n32 30 28 26 25 22 20\n39 42 45 48 51 54\n70 72 73 74 76 79 80 81\n37 34 31 30 29\n15 16 17 19 21 24\n31 30 28 25 23 20 18 17\n81 82 83 86 89 91 92 94\n78 76 74 72 71\n24 25 28 30 31 32\n80 79 76 75 73 72 71\n71 72 74 76 78 81 84 86\n8 11 14 15 18\n46 43 42 41 38 35 34\n20 18 17 15 14\n84 82 80 79 77 76\n43 44 47 48 50"))
  )
