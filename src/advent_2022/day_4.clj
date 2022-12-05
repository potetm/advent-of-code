(ns advent-2022.day-4
  (:require [clojure.string :as str]))

(defn parse [s]
  (map (fn [l]
         (into []
               (comp (map parse-long)
                     (partition-all 2))
               (next (re-find #"(\d+)-(\d+),(\d+)-(\d+)"
                              l))))
       (str/split-lines s)))


(defn part-1 [in]
  (count (filter (fn [[[a1 a2] [b1 b2]]]
                   (or (<= a1 b1 b2 a2)
                       (<= b1 a1 a2 b2)))
                 in)))


(defn part-2 [in]
  (count (filter (fn [[[a1 a2] [b1 b2]]]
                   (or (<= a1 b1 a2)
                       (<= b1 a1 b2)))
                 in)))


(comment
  (def t "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")
  (def in "13-53,17-82\n32-32,32-42\n85-85,8-86\n78-80,79-91\n60-71,59-70\n91-92,4-90\n90-90,1-90\n35-49,38-50\n31-51,31-52\n78-80,81-81\n85-86,86-94\n52-95,96-97\n1-39,1-40\n8-68,7-24\n83-89,58-78\n96-98,98-99\n26-27,26-74\n9-43,8-44\n3-8,9-89\n3-77,7-26\n1-88,2-2\n30-53,31-52\n3-47,46-86\n8-78,7-9\n53-92,6-52\n84-97,49-97\n38-67,38-68\n13-13,12-95\n7-13,52-87\n18-18,17-79\n65-65,64-64\n12-92,11-91\n67-67,2-68\n14-89,13-92\n13-44,12-14\n20-42,21-24\n9-47,7-11\n99-99,32-97\n22-37,21-36\n24-98,24-99\n62-76,62-77\n49-50,49-98\n92-98,26-79\n5-10,9-95\n24-92,24-91\n13-13,14-30\n56-97,5-97\n8-33,9-34\n18-76,18-75\n16-80,12-17\n8-99,8-95\n4-97,3-96\n11-12,11-86\n5-90,1-89\n32-71,31-84\n12-96,14-91\n9-9,12-71\n13-93,8-92\n4-77,78-93\n4-6,5-89\n18-52,17-51\n35-65,11-64\n76-77,1-76\n24-24,26-43\n4-8,7-48\n85-85,5-86\n1-86,4-85\n10-44,10-45\n19-91,36-79\n63-95,75-98\n59-86,59-87\n87-91,42-90\n49-98,48-96\n2-28,2-27\n4-91,4-90\n93-98,28-91\n5-91,90-90\n67-77,82-85\n98-98,66-94\n19-36,33-46\n6-54,6-53\n52-82,87-90\n94-99,1-94\n2-2,1-96\n43-46,42-45\n20-23,25-30\n7-93,6-92\n18-56,17-19\n40-95,39-96\n35-90,91-99\n15-17,15-19\n29-31,37-99\n2-91,6-92\n11-28,7-7\n10-50,9-11\n27-89,10-26\n33-60,33-61\n66-68,55-67\n11-11,10-90\n10-20,9-11\n73-96,20-98\n6-85,6-86\n7-82,6-8\n37-54,10-57\n36-87,35-86\n46-86,86-93\n4-68,7-69\n25-26,20-26\n99-99,2-89\n1-10,3-17\n6-92,5-5\n29-87,1-30\n7-74,12-74\n26-36,25-97\n29-83,16-84\n13-97,13-98\n34-72,35-72\n55-62,61-61\n24-95,59-95\n4-38,4-37\n62-73,8-77\n5-35,2-2\n79-79,4-78\n18-51,18-50\n10-12,9-9\n44-83,6-44\n71-81,95-98\n36-98,26-97\n67-74,19-74\n81-89,40-80\n26-81,53-94\n64-64,26-63\n34-67,11-75\n13-56,14-56\n55-55,44-56\n3-51,3-97\n76-80,75-77\n5-66,77-89\n49-70,18-49\n32-53,14-33\n30-30,30-31\n3-7,14-47\n33-33,1-33\n7-9,11-95\n92-92,37-92\n40-90,4-90\n33-79,7-46\n50-64,26-49\n3-94,37-94\n32-68,68-77\n5-17,1-6\n6-86,3-6\n26-46,21-76\n14-74,38-75\n3-86,20-85\n60-70,60-69\n10-32,11-33\n43-43,11-43\n17-96,9-95\n36-96,37-95\n11-94,87-95\n9-94,8-94\n60-61,45-60\n25-58,24-58\n46-81,45-92\n37-77,38-80\n95-95,2-96\n57-71,56-70\n28-93,59-94\n16-88,15-90\n12-38,12-22\n13-88,87-99\n7-38,7-37\n3-70,4-69\n44-77,45-76\n4-69,2-98\n45-79,44-78\n39-91,2-33\n33-78,33-77\n91-91,16-90\n63-94,95-97\n1-97,1-97\n19-38,3-19\n95-95,91-96\n53-77,15-78\n8-53,7-53\n20-41,28-42\n5-7,7-90\n55-86,85-87\n2-92,9-80\n17-79,79-89\n43-77,44-78\n61-66,60-60\n8-76,5-8\n40-83,39-83\n69-79,79-80\n51-51,50-92\n29-99,18-99\n18-75,17-18\n17-39,8-59\n9-35,34-79\n8-52,7-53\n51-95,75-94\n94-94,1-94\n37-55,32-56\n6-84,1-6\n19-19,18-40\n95-97,28-95\n89-99,82-92\n11-47,46-47\n4-98,4-99\n12-16,23-94\n18-50,17-49\n37-72,35-57\n23-77,22-24\n54-65,6-54\n61-74,39-73\n83-94,52-90\n13-99,2-14\n10-98,11-99\n30-98,29-99\n38-90,37-90\n63-66,64-68\n49-93,49-95\n84-88,79-88\n11-35,35-83\n2-59,3-59\n42-64,57-64\n14-62,15-88\n52-54,53-71\n37-66,38-67\n20-96,21-92\n5-82,4-6\n13-52,13-52\n10-69,4-21\n5-69,26-98\n41-57,40-42\n8-94,9-93\n6-27,26-61\n27-65,27-66\n97-98,1-97\n46-85,45-86\n43-48,38-43\n5-49,48-48\n14-59,12-58\n30-97,7-96\n62-70,64-69\n31-79,12-78\n10-18,9-17\n74-76,72-76\n29-69,29-70\n15-93,93-95\n66-68,67-73\n2-89,1-2\n2-95,9-96\n4-89,6-45\n98-98,28-99\n16-77,4-77\n16-86,87-87\n17-17,18-83\n18-82,29-81\n21-99,7-98\n36-85,1-85\n9-70,70-90\n38-95,60-94\n94-94,24-95\n4-63,3-63\n40-72,73-88\n3-4,3-99\n1-27,3-93\n15-26,16-27\n10-89,20-34\n16-18,6-39\n34-48,28-34\n5-83,6-6\n37-49,38-50\n39-86,9-38\n12-74,74-75\n16-80,15-81\n26-67,24-85\n5-7,12-19\n40-68,69-69\n17-79,80-95\n17-63,16-18\n69-72,16-75\n15-24,48-89\n12-12,13-96\n4-4,6-89\n79-84,5-39\n23-25,24-61\n26-97,6-96\n4-97,5-97\n55-91,56-91\n25-80,80-81\n52-76,53-75\n45-61,44-61\n65-81,64-82\n65-74,3-64\n4-73,72-72\n54-54,53-57\n51-51,29-50\n1-14,3-36\n93-95,57-92\n85-86,6-85\n3-98,3-15\n21-21,20-54\n18-72,19-73\n27-49,47-53\n20-77,21-78\n52-76,38-76\n82-87,14-87\n3-79,1-4\n1-97,1-1\n39-40,40-91\n6-11,5-10\n5-98,4-6\n36-36,9-37\n6-69,7-7\n77-93,76-95\n41-94,26-93\n70-81,7-70\n1-72,71-72\n30-90,29-98\n68-76,12-76\n28-42,15-19\n76-96,39-77\n30-32,46-90\n83-98,83-92\n1-98,9-98\n17-93,16-92\n17-89,3-8\n61-67,62-67\n19-63,18-20\n51-53,52-98\n20-21,20-58\n5-86,6-87\n23-85,24-84\n34-54,22-53\n20-67,20-68\n26-88,26-26\n26-94,27-94\n27-29,29-84\n42-73,2-42\n68-72,56-71\n13-85,16-84\n54-87,88-95\n26-29,32-35\n2-21,20-45\n18-18,19-48\n91-98,31-88\n5-59,41-60\n94-96,70-94\n8-85,86-89\n3-97,98-99\n89-90,16-92\n13-44,43-74\n6-87,25-99\n5-5,6-53\n3-85,1-84\n25-94,35-87\n17-95,17-64\n12-85,13-85\n2-4,4-75\n7-25,8-24\n47-96,24-96\n95-96,94-94\n94-94,93-93\n22-77,3-90\n17-77,78-83\n35-42,48-75\n12-12,13-97\n13-19,18-77\n32-83,26-83\n31-78,3-32\n3-17,17-95\n45-61,44-61\n73-81,13-80\n2-99,6-93\n80-88,33-74\n45-86,3-44\n39-75,39-93\n2-96,95-96\n10-64,9-65\n4-76,5-5\n72-96,72-73\n78-85,77-79\n20-94,19-98\n14-87,3-15\n21-35,35-53\n2-87,88-93\n15-90,15-87\n3-99,2-99\n9-95,9-96\n5-89,1-90\n6-73,3-74\n52-76,6-51\n5-52,66-69\n17-19,18-36\n8-43,2-9\n56-72,63-73\n73-91,66-92\n4-89,5-89\n95-99,9-93\n91-98,92-99\n18-78,6-7\n11-71,71-94\n1-99,3-97\n7-90,1-8\n5-87,5-88\n46-48,47-97\n3-92,7-91\n33-33,32-73\n39-95,38-98\n35-82,36-81\n41-90,86-95\n21-45,44-44\n5-5,6-49\n49-67,50-66\n8-76,8-77\n15-17,29-73\n35-95,20-34\n29-64,30-64\n11-45,11-46\n14-87,4-13\n38-69,37-37\n11-76,6-9\n70-98,62-97\n17-99,16-18\n2-82,5-81\n15-93,4-94\n56-57,56-57\n1-6,6-88\n48-93,48-49\n7-75,39-74\n1-3,2-90\n27-31,28-31\n10-10,9-63\n11-64,65-79\n57-57,56-69\n50-50,49-88\n43-43,2-44\n15-59,11-59\n28-44,7-28\n5-39,37-42\n88-95,95-95\n2-63,63-63\n5-91,14-98\n7-89,6-98\n16-36,36-56\n40-80,81-83\n4-99,2-98\n70-70,2-71\n6-45,7-46\n9-97,10-96\n34-79,33-34\n51-55,50-55\n27-72,58-79\n9-65,8-64\n3-5,4-99\n17-43,11-16\n9-29,10-28\n23-91,24-90\n13-80,10-14\n47-85,86-86\n9-25,4-8\n43-92,43-92\n89-90,76-89\n32-88,4-31\n33-52,32-51\n3-97,1-99\n39-91,40-51\n19-92,22-92\n4-89,23-92\n5-6,6-66\n6-99,7-78\n8-93,92-93\n5-95,58-97\n2-96,99-99\n34-39,35-35\n92-92,3-93\n38-54,11-53\n45-59,59-87\n55-78,54-54\n42-66,42-66\n19-91,22-92\n39-46,38-91\n42-89,41-41\n33-66,34-65\n7-71,6-70\n7-80,30-61\n58-91,57-90\n30-95,88-95\n4-97,96-99\n12-99,12-95\n39-41,40-50\n34-87,35-87\n14-86,2-12\n14-87,15-86\n7-97,96-96\n7-36,31-46\n6-75,7-49\n29-29,28-81\n58-67,68-68\n18-97,18-91\n17-31,27-30\n39-46,51-96\n68-89,69-69\n15-57,15-57\n24-24,16-77\n40-92,91-93\n43-61,41-76\n49-49,42-48\n34-76,15-75\n2-73,74-74\n16-98,16-97\n7-80,6-80\n23-66,24-65\n39-81,40-82\n74-99,33-37\n81-93,64-93\n19-86,86-95\n97-97,9-98\n45-70,62-71\n54-67,20-53\n85-85,18-85\n11-79,28-76\n40-96,12-40\n19-79,35-80\n3-91,1-92\n5-90,5-89\n42-93,43-92\n40-70,34-38\n12-43,11-12\n22-22,3-21\n53-90,7-65\n22-90,11-15\n24-24,24-36\n3-3,5-55\n53-54,53-53\n53-53,26-54\n3-9,9-97\n43-58,58-59\n31-64,17-64\n34-36,35-35\n4-11,10-48\n12-39,15-40\n44-44,43-92\n9-68,68-68\n20-95,20-81\n6-27,45-62\n7-91,7-91\n2-3,5-88\n11-77,22-61\n8-95,30-77\n85-97,86-97\n3-60,8-18\n14-70,13-69\n34-81,61-82\n42-82,43-82\n9-99,10-10\n2-75,19-53\n26-77,69-86\n82-97,27-81\n17-92,9-93\n5-6,5-5\n46-46,45-47\n21-67,21-68\n36-95,17-36\n41-88,41-89\n6-81,11-80\n26-45,45-46\n4-75,2-4\n27-36,24-35\n49-64,50-97\n86-86,40-85\n5-5,5-92\n81-82,6-82\n14-92,93-93\n28-36,35-65\n12-17,18-27\n28-68,6-68\n47-93,48-48\n20-99,11-97\n32-75,9-18\n53-96,96-96\n33-64,32-63\n79-83,3-78\n38-46,47-79\n3-88,4-98\n6-97,96-96\n68-68,42-69\n24-99,23-23\n27-98,65-99\n9-94,5-5\n22-30,23-23\n71-72,23-71\n2-97,2-96\n6-58,7-57\n44-88,45-74\n2-97,98-99\n27-30,9-27\n10-89,48-89\n19-99,18-88\n95-95,15-78\n10-30,10-31\n10-97,11-79\n5-97,1-98\n17-99,1-99\n27-29,28-95\n90-90,33-90\n64-65,3-64\n34-98,35-97\n23-62,22-63\n76-85,85-85\n56-97,57-98\n43-43,43-43\n17-83,16-18\n4-15,6-15\n18-70,66-70\n62-63,9-62\n81-85,32-80\n35-95,24-90\n6-62,7-62\n17-19,18-29\n24-38,37-37\n2-42,2-43\n11-50,10-12\n20-50,19-51\n22-22,2-22\n54-54,8-53\n27-47,46-46\n11-97,5-6\n47-99,1-99\n31-46,32-45\n8-56,2-39\n57-95,10-96\n17-17,25-75\n44-46,10-45\n30-33,29-32\n1-94,24-95\n31-32,8-32\n19-21,20-49\n34-34,19-35\n33-79,86-96\n12-98,12-97\n27-35,26-36\n37-89,90-90\n2-5,4-81\n29-38,38-39\n3-88,58-82\n5-8,7-65\n6-14,13-73\n57-98,90-99\n29-53,30-54\n1-78,59-60\n34-34,24-35\n4-12,11-11\n41-84,9-83\n5-88,6-6\n1-78,8-75\n80-80,19-81\n95-97,72-96\n13-85,84-84\n31-81,28-32\n53-58,37-45\n13-69,62-82\n11-69,10-69\n46-46,45-74\n60-93,77-98\n14-58,14-59\n73-73,11-72\n3-91,32-91\n4-6,5-87\n11-85,16-78\n2-99,89-97\n21-88,20-87\n4-52,51-51\n22-22,1-21\n21-76,8-94\n4-6,5-40\n97-97,5-93\n22-85,11-23\n6-74,5-5\n67-91,66-92\n24-99,16-98\n2-58,1-1\n21-52,20-52\n71-82,76-96\n5-65,5-64\n4-6,9-70\n73-75,68-88\n39-52,38-40\n29-32,29-31\n65-65,37-66\n3-69,2-68\n36-97,36-85\n52-68,15-69\n3-42,42-43\n19-25,25-58\n9-28,8-27\n23-99,24-99\n21-98,22-95\n6-12,12-13\n42-93,41-99\n57-83,58-84\n91-91,90-94\n71-72,71-88\n2-97,1-3\n9-12,11-49\n75-81,38-82\n38-75,16-52\n16-18,17-91\n29-72,30-71\n83-83,45-82\n17-56,19-57\n50-52,51-78\n17-67,66-66\n3-81,3-80\n20-48,49-65\n17-51,10-18\n1-9,2-10\n49-49,46-48\n95-96,55-96\n27-89,28-74\n47-76,28-82\n9-95,10-96\n87-88,5-88\n27-74,28-47\n74-93,46-92\n3-95,98-98\n49-50,26-50\n28-79,27-79\n15-53,14-93\n17-36,17-18\n7-72,64-75\n7-96,32-99\n87-99,21-77\n12-98,13-97\n14-79,2-79\n2-2,2-3\n38-73,73-74\n11-82,4-7\n67-67,68-71\n86-87,13-84\n52-80,51-52\n71-97,71-96\n4-5,5-87\n14-31,31-73\n28-28,27-55\n3-3,2-28\n37-67,65-73\n79-80,73-79\n16-95,17-94\n7-77,8-76\n16-82,18-82\n34-34,33-93\n13-84,88-88\n37-98,9-97\n16-40,7-59\n3-96,1-3\n3-95,30-99\n1-84,2-85\n2-3,4-56\n5-54,6-55\n69-93,36-93\n31-89,44-88\n5-88,23-87\n5-87,4-88\n85-90,42-84\n44-72,33-37\n7-11,21-99\n32-91,31-91\n99-99,61-99\n5-79,10-44\n59-94,58-84\n4-6,5-99\n24-29,28-82\n23-41,23-40\n6-95,95-96\n18-99,17-53\n87-95,32-85\n28-66,28-67\n4-94,99-99\n22-97,22-98\n30-96,14-31\n30-70,31-69\n56-74,3-89\n4-85,86-86\n57-97,58-97\n41-95,41-96\n78-79,37-78\n25-70,70-75\n92-92,4-93\n46-65,66-73\n18-20,19-23\n32-90,32-89\n38-73,38-95\n30-69,31-68\n49-68,1-78\n18-98,10-97\n25-65,14-22\n14-46,12-45\n19-48,20-32\n52-83,29-52\n57-95,57-94\n4-97,4-96\n35-91,90-92\n5-99,4-99\n4-26,4-27\n54-73,6-54\n6-98,7-99\n63-81,7-80\n58-85,59-59\n1-2,1-88\n71-85,51-93\n5-65,6-6\n3-49,33-93\n44-84,5-84\n49-62,48-61\n35-98,36-99\n2-3,10-64\n95-95,43-95\n2-55,3-56\n20-53,20-52\n15-97,11-13\n81-81,3-81\n40-45,1-44\n1-3,5-73\n4-97,97-98\n91-92,12-91\n31-64,30-63\n18-55,17-54\n35-40,34-35\n3-6,5-95\n26-70,79-94\n4-97,8-97\n6-63,28-63\n28-95,11-21\n5-81,1-6\n38-41,39-40\n49-96,48-88\n59-77,58-77\n29-40,28-40\n45-75,46-46\n3-87,4-67\n58-99,4-92\n47-57,46-47\n24-84,15-80\n85-92,38-85\n55-89,88-90\n32-68,67-67\n7-89,8-89\n14-92,13-92\n29-41,30-41\n5-92,4-92\n9-29,19-41\n65-89,90-99\n91-91,60-91\n5-70,21-68\n11-95,35-92\n25-65,14-26\n52-93,88-94\n52-89,51-90\n74-87,86-86\n32-96,99-99\n5-97,4-6\n19-91,20-91\n65-76,64-77\n36-90,37-90\n37-82,25-92\n76-78,77-84\n5-66,5-66\n4-9,12-50\n2-52,10-52\n15-99,14-98\n18-96,6-18\n3-81,4-33\n21-66,15-71\n49-49,27-67\n4-4,5-94\n50-50,9-51\n31-95,30-94\n82-91,87-93\n61-96,17-95\n2-95,17-95\n34-37,7-35\n2-94,7-85\n96-96,49-95\n41-41,42-59\n2-97,3-97\n22-97,22-98\n33-95,34-82\n33-47,47-72\n32-93,33-93\n36-58,35-35\n10-58,52-58\n77-89,60-67\n3-13,12-81\n14-98,40-98\n11-94,7-12\n51-63,43-63\n22-82,2-81\n31-34,35-60\n8-79,8-42\n67-67,66-88\n64-81,4-64\n16-96,15-97\n22-65,23-65\n59-97,60-96\n18-89,90-99\n2-2,3-97\n12-63,11-63\n12-42,41-59\n36-37,31-36\n45-91,2-59\n7-89,6-8\n21-49,21-50\n13-13,12-87\n48-89,47-90\n3-22,2-55\n19-19,20-41\n48-67,47-94\n11-13,12-83\n16-62,16-61\n11-35,12-41\n71-78,5-71\n66-67,67-87\n22-71,14-71\n19-72,15-72\n20-42,21-41\n8-94,7-93\n97-98,19-96\n3-98,2-98\n33-71,72-73\n33-35,34-81\n18-98,18-97\n85-85,24-86\n7-97,26-96\n47-47,46-49\n5-14,3-3\n1-83,14-83\n10-91,3-75\n62-62,32-63\n16-84,83-84\n52-53,52-70\n2-47,2-47\n8-93,8-24\n56-97,97-97\n8-84,27-84\n71-89,89-89\n96-98,30-96\n11-92,49-92\n66-66,19-67\n55-57,54-56\n3-69,3-70\n5-99,4-6\n1-40,1-39\n1-87,88-89\n11-98,20-96\n22-92,21-94\n5-88,5-86\n79-98,79-97\n34-88,82-88\n67-80,66-79\n16-87,9-17\n78-91,92-92\n46-57,35-57\n45-64,44-44\n77-82,12-81\n3-98,1-3\n42-62,62-89\n2-70,95-95\n62-63,61-63\n39-97,38-98\n3-99,6-98\n26-84,83-83\n18-27,17-28\n52-58,7-52\n25-82,1-82\n29-66,67-67\n43-72,21-71\n23-29,5-28\n43-90,43-44\n42-68,42-67\n67-69,66-70\n19-40,18-57\n1-1,8-9\n56-89,97-99\n87-98,25-86\n88-98,10-88\n2-93,2-96\n3-82,82-89\n7-14,8-14\n3-37,36-56\n51-52,4-51\n38-88,37-96\n74-81,22-81\n69-83,68-70\n2-91,38-91\n2-90,91-96")

  (re-find #"(\d+)-(\d+),(\d+)-(\d+)"
           "2-4,6-8")
  (part-1 (parse in))
  (part-2 (parse in))
  )
