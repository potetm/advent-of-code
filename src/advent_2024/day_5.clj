(ns advent-2024.day-5
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (let [[rules [_ & updates]] (split-with (complement str/blank?)
                                          (str/split-lines s))]
    {:rules (transduce (map (fn [l]
                              (let [[_ x y] (re-find #"(\d+)\|(\d+)" l)]
                                [(parse-long x)
                                 (parse-long y)])))
                       (completing
                         (fn [acc [x y]]
                           (update acc
                                   x
                                   (fnil conj #{})
                                   y)))
                       {}
                       rules)
     :updates (into []
                    (map (fn [l]
                           (into []
                                 (map parse-long)
                                 (str/split l #","))))
                    updates)}))


(defn valid? [rules update]
  (let [rev (rseq update)]
    (every? (fn [[f & r]]
              (not-any? (fn [i]
                          (contains? (rules f) i))
                        r))
            (map (fn [i]
                   (drop i rev))
                 (range (dec (count rev)))))))


(defn part-1 [{rs :rules
               us :updates}]
  (util/sum (comp (filter (fn [u]
                            (valid? rs u)))
                  (map (fn [u]
                         (get u
                              (int (/ (count u)
                                      2))))))
            us))


(defn part-2 [{rs :rules
               us :updates}]
  (util/sum (comp (remove (fn [u]
                            (valid? rs u)))
                  (map (fn [u]
                         (into []
                               (xf/sort (fn [x y]
                                          (cond
                                            (contains? (get rs x)
                                                       y)
                                            -1

                                            (contains? (get rs y)
                                                       x)
                                            1

                                            :else 0)))
                               u)))
                  (map (fn [u]
                         (get u
                              (int (/ (count u)
                                      2))))))
            us))


(comment
  (def t "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")
  (def in "32|75\n18|19\n18|82\n41|56\n41|28\n41|47\n85|67\n85|66\n85|35\n85|93\n87|31\n87|38\n87|44\n87|94\n87|55\n66|96\n66|39\n66|94\n66|95\n66|29\n66|36\n16|14\n16|74\n16|47\n16|17\n16|18\n16|28\n16|93\n31|77\n31|19\n31|34\n31|29\n31|52\n31|66\n31|39\n31|75\n62|89\n62|18\n62|63\n62|85\n62|76\n62|37\n62|93\n62|68\n62|14\n46|44\n46|66\n46|75\n46|67\n46|87\n46|31\n46|19\n46|18\n46|63\n46|15\n74|27\n74|67\n74|84\n74|66\n74|63\n74|29\n74|34\n74|18\n74|44\n74|28\n74|46\n56|67\n56|68\n56|47\n56|59\n56|18\n56|87\n56|46\n56|27\n56|32\n56|43\n56|28\n56|13\n68|89\n68|66\n68|52\n68|28\n68|98\n68|36\n68|84\n68|19\n68|34\n68|27\n68|44\n68|95\n68|13\n28|94\n28|63\n28|31\n28|66\n28|87\n28|29\n28|34\n28|15\n28|98\n28|13\n28|75\n28|95\n28|18\n28|44\n13|98\n13|21\n13|75\n13|77\n13|62\n13|38\n13|34\n13|83\n13|84\n13|29\n13|96\n13|86\n13|19\n13|44\n13|76\n55|38\n55|14\n55|35\n55|96\n55|74\n55|62\n55|76\n55|17\n55|82\n55|59\n55|56\n55|21\n55|46\n55|93\n55|43\n55|85\n96|56\n96|68\n96|35\n96|74\n96|16\n96|62\n96|59\n96|87\n96|47\n96|86\n96|85\n96|14\n96|37\n96|28\n96|46\n96|67\n96|93\n93|75\n93|29\n93|74\n93|32\n93|98\n93|67\n93|52\n93|66\n93|15\n93|46\n93|31\n93|87\n93|34\n93|18\n93|77\n93|68\n93|13\n93|89\n89|77\n89|82\n89|55\n89|38\n89|34\n89|75\n89|95\n89|19\n89|98\n89|52\n89|29\n89|94\n89|84\n89|13\n89|83\n89|41\n89|66\n89|27\n89|36\n98|59\n98|96\n98|56\n98|85\n98|77\n98|34\n98|62\n98|38\n98|21\n98|84\n98|39\n98|17\n98|94\n98|86\n98|95\n98|83\n98|36\n98|44\n98|82\n98|19\n37|28\n37|87\n37|14\n37|74\n37|75\n37|29\n37|63\n37|31\n37|32\n37|93\n37|46\n37|15\n37|27\n37|95\n37|18\n37|68\n37|67\n37|13\n37|43\n37|98\n37|52\n34|35\n34|41\n34|43\n34|85\n34|94\n34|84\n34|59\n34|56\n34|37\n34|96\n34|86\n34|14\n34|38\n34|17\n34|82\n34|21\n34|76\n34|47\n34|39\n34|55\n34|83\n34|16\n38|47\n38|96\n38|17\n38|46\n38|32\n38|16\n38|39\n38|37\n38|56\n38|18\n38|86\n38|76\n38|43\n38|68\n38|21\n38|93\n38|59\n38|67\n38|74\n38|85\n38|35\n38|14\n38|28\n35|18\n35|28\n35|29\n35|89\n35|63\n35|74\n35|43\n35|93\n35|98\n35|52\n35|37\n35|87\n35|14\n35|68\n35|46\n35|27\n35|13\n35|67\n35|95\n35|66\n35|15\n35|31\n35|75\n35|32\n44|94\n44|17\n44|76\n44|41\n44|16\n44|85\n44|36\n44|38\n44|47\n44|59\n44|37\n44|39\n44|77\n44|35\n44|86\n44|55\n44|83\n44|84\n44|96\n44|21\n44|34\n44|82\n44|62\n44|56\n67|77\n67|75\n67|94\n67|19\n67|32\n67|15\n67|66\n67|36\n67|18\n67|31\n67|52\n67|44\n67|27\n67|84\n67|89\n67|28\n67|13\n67|29\n67|63\n67|34\n67|98\n67|87\n67|68\n67|95\n29|34\n29|21\n29|16\n29|56\n29|41\n29|84\n29|38\n29|76\n29|17\n29|62\n29|82\n29|19\n29|96\n29|36\n29|83\n29|55\n29|98\n29|85\n29|94\n29|95\n29|86\n29|77\n29|39\n29|44\n14|13\n14|29\n14|74\n14|31\n14|75\n14|18\n14|89\n14|15\n14|52\n14|67\n14|98\n14|66\n14|95\n14|93\n14|87\n14|28\n14|27\n14|32\n14|44\n14|68\n14|19\n14|63\n14|43\n14|46\n59|28\n59|29\n59|46\n59|52\n59|68\n59|15\n59|32\n59|18\n59|87\n59|37\n59|67\n59|13\n59|47\n59|63\n59|93\n59|75\n59|66\n59|35\n59|43\n59|31\n59|89\n59|74\n59|27\n59|14\n82|85\n82|14\n82|93\n82|56\n82|37\n82|59\n82|62\n82|35\n82|28\n82|43\n82|41\n82|76\n82|47\n82|96\n82|68\n82|46\n82|74\n82|16\n82|17\n82|67\n82|86\n82|21\n82|38\n82|39\n17|89\n17|15\n17|13\n17|59\n17|68\n17|43\n17|63\n17|52\n17|28\n17|74\n17|32\n17|27\n17|14\n17|18\n17|67\n17|37\n17|31\n17|93\n17|87\n17|47\n17|85\n17|56\n17|46\n17|35\n19|41\n19|38\n19|84\n19|39\n19|44\n19|76\n19|83\n19|94\n19|96\n19|16\n19|17\n19|82\n19|34\n19|21\n19|36\n19|55\n19|62\n19|47\n19|35\n19|59\n19|56\n19|77\n19|86\n19|85\n77|34\n77|96\n77|94\n77|62\n77|16\n77|47\n77|84\n77|85\n77|35\n77|86\n77|17\n77|36\n77|56\n77|83\n77|14\n77|76\n77|37\n77|38\n77|55\n77|21\n77|41\n77|82\n77|59\n77|39\n43|87\n43|46\n43|27\n43|93\n43|18\n43|66\n43|52\n43|77\n43|44\n43|19\n43|13\n43|29\n43|15\n43|89\n43|31\n43|67\n43|74\n43|63\n43|75\n43|95\n43|32\n43|28\n43|98\n43|68\n15|95\n15|31\n15|84\n15|19\n15|82\n15|27\n15|21\n15|38\n15|98\n15|75\n15|52\n15|55\n15|66\n15|13\n15|34\n15|29\n15|39\n15|94\n15|77\n15|36\n15|89\n15|41\n15|83\n15|44\n76|46\n76|17\n76|27\n76|43\n76|89\n76|74\n76|35\n76|28\n76|68\n76|67\n76|56\n76|52\n76|15\n76|87\n76|85\n76|59\n76|37\n76|31\n76|18\n76|47\n76|32\n76|93\n76|63\n76|14\n95|59\n95|82\n95|47\n95|17\n95|34\n95|94\n95|77\n95|55\n95|86\n95|84\n95|21\n95|96\n95|39\n95|83\n95|44\n95|56\n95|41\n95|36\n95|19\n95|38\n95|76\n95|62\n95|16\n95|85\n83|76\n83|96\n83|14\n83|56\n83|68\n83|16\n83|93\n83|46\n83|17\n83|37\n83|59\n83|86\n83|62\n83|82\n83|74\n83|85\n83|21\n83|35\n83|47\n83|38\n83|43\n83|67\n83|39\n83|41\n75|34\n75|84\n75|82\n75|36\n75|98\n75|19\n75|83\n75|17\n75|44\n75|55\n75|76\n75|39\n75|77\n75|66\n75|62\n75|21\n75|94\n75|86\n75|41\n75|96\n75|29\n75|16\n75|95\n75|38\n52|29\n52|66\n52|83\n52|19\n52|84\n52|95\n52|13\n52|98\n52|16\n52|77\n52|75\n52|41\n52|96\n52|39\n52|21\n52|82\n52|36\n52|86\n52|94\n52|38\n52|62\n52|44\n52|55\n52|34\n21|59\n21|76\n21|18\n21|86\n21|39\n21|46\n21|37\n21|16\n21|62\n21|96\n21|14\n21|35\n21|56\n21|68\n21|43\n21|67\n21|32\n21|47\n21|17\n21|85\n21|74\n21|93\n21|87\n21|28\n84|83\n84|85\n84|41\n84|14\n84|62\n84|59\n84|16\n84|93\n84|39\n84|38\n84|36\n84|55\n84|43\n84|86\n84|17\n84|47\n84|82\n84|21\n84|56\n84|94\n84|96\n84|76\n84|35\n84|37\n47|68\n47|35\n47|74\n47|67\n47|89\n47|31\n47|63\n47|27\n47|52\n47|87\n47|15\n47|28\n47|14\n47|66\n47|46\n47|32\n47|29\n47|37\n47|98\n47|43\n47|13\n47|75\n47|93\n47|18\n39|43\n39|56\n39|32\n39|14\n39|96\n39|85\n39|93\n39|74\n39|28\n39|18\n39|17\n39|86\n39|47\n39|16\n39|46\n39|59\n39|62\n39|35\n39|63\n39|67\n39|68\n39|87\n39|76\n39|37\n27|29\n27|21\n27|77\n27|94\n27|66\n27|39\n27|82\n27|86\n27|13\n27|31\n27|75\n27|41\n27|19\n27|98\n27|34\n27|36\n27|84\n27|44\n27|55\n27|83\n27|96\n27|52\n27|95\n27|38\n36|74\n36|59\n36|55\n36|86\n36|43\n36|93\n36|38\n36|16\n36|85\n36|37\n36|96\n36|39\n36|56\n36|14\n36|17\n36|46\n36|83\n36|82\n36|21\n36|35\n36|47\n36|41\n36|62\n36|76\n94|56\n94|86\n94|85\n94|17\n94|37\n94|36\n94|59\n94|74\n94|93\n94|39\n94|38\n94|76\n94|55\n94|82\n94|62\n94|21\n94|83\n94|47\n94|43\n94|41\n94|16\n94|35\n94|96\n94|14\n86|85\n86|74\n86|47\n86|32\n86|16\n86|28\n86|35\n86|46\n86|14\n86|56\n86|15\n86|76\n86|63\n86|93\n86|59\n86|89\n86|87\n86|17\n86|37\n86|43\n86|67\n86|18\n86|68\n86|62\n63|15\n63|77\n63|38\n63|44\n63|19\n63|27\n63|83\n63|41\n63|21\n63|82\n63|34\n63|94\n63|95\n63|36\n63|89\n63|31\n63|52\n63|84\n63|13\n63|75\n63|98\n63|29\n63|55\n63|66\n32|98\n32|29\n32|41\n32|34\n32|36\n32|82\n32|44\n32|63\n32|27\n32|55\n32|83\n32|95\n32|19\n32|77\n32|52\n32|66\n32|15\n32|94\n32|89\n32|84\n32|13\n32|87\n32|31\n18|55\n18|29\n18|77\n18|34\n18|27\n18|52\n18|94\n18|89\n18|95\n18|13\n18|15\n18|63\n18|66\n18|36\n18|98\n18|87\n18|84\n18|75\n18|44\n18|31\n18|32\n18|83\n41|86\n41|59\n41|38\n41|85\n41|18\n41|74\n41|46\n41|17\n41|96\n41|62\n41|93\n41|67\n41|76\n41|14\n41|43\n41|37\n41|39\n41|35\n41|21\n41|68\n41|16\n85|68\n85|37\n85|43\n85|27\n85|13\n85|89\n85|75\n85|46\n85|52\n85|59\n85|31\n85|87\n85|32\n85|15\n85|18\n85|14\n85|28\n85|74\n85|63\n85|47\n87|15\n87|41\n87|13\n87|52\n87|36\n87|29\n87|95\n87|34\n87|27\n87|63\n87|77\n87|89\n87|19\n87|83\n87|84\n87|75\n87|98\n87|82\n87|66\n66|62\n66|38\n66|98\n66|82\n66|16\n66|84\n66|44\n66|41\n66|76\n66|77\n66|86\n66|83\n66|21\n66|55\n66|56\n66|19\n66|34\n66|17\n16|63\n16|76\n16|59\n16|56\n16|15\n16|85\n16|89\n16|35\n16|27\n16|67\n16|32\n16|43\n16|62\n16|87\n16|68\n16|46\n16|37\n31|98\n31|96\n31|95\n31|41\n31|16\n31|82\n31|84\n31|21\n31|83\n31|44\n31|13\n31|38\n31|36\n31|55\n31|86\n31|94\n62|43\n62|56\n62|17\n62|67\n62|32\n62|87\n62|46\n62|59\n62|27\n62|74\n62|31\n62|47\n62|35\n62|15\n62|28\n46|28\n46|95\n46|32\n46|77\n46|29\n46|13\n46|89\n46|34\n46|27\n46|52\n46|94\n46|68\n46|84\n46|98\n74|31\n74|95\n74|89\n74|68\n74|98\n74|19\n74|32\n74|15\n74|77\n74|75\n74|13\n74|87\n74|52\n56|93\n56|85\n56|75\n56|63\n56|35\n56|37\n56|89\n56|52\n56|15\n56|14\n56|31\n56|74\n68|75\n68|31\n68|94\n68|29\n68|18\n68|77\n68|15\n68|63\n68|55\n68|87\n68|32\n28|36\n28|83\n28|52\n28|27\n28|19\n28|55\n28|32\n28|84\n28|89\n28|77\n13|95\n13|55\n13|66\n13|94\n13|39\n13|36\n13|16\n13|82\n13|41\n55|37\n55|83\n55|47\n55|39\n55|16\n55|67\n55|86\n55|41\n96|18\n96|76\n96|32\n96|17\n96|15\n96|63\n96|43\n93|27\n93|28\n93|95\n93|63\n93|19\n93|44\n89|21\n89|96\n89|44\n89|31\n89|39\n98|16\n98|55\n98|76\n98|41\n37|19\n37|66\n37|89\n34|36\n34|62\n38|62\n\n52,77,83,31,94,75,34,95,29,38,82,19,41,39,27,98,84,13,55,21,66\n76,17,43,93,46,67,68,28,18,32,87,15,89,27,31\n28,63,89,27,13,66,98,95,19,36,55\n38,21,39,86,16,62,76,85,47,35,93,67,68,28,18\n46,67,68,28,18,87,15,89,27,31,52,13,75,66,29,98,95,19,44,77,84\n41,34,98,29,95,96,55,86,52,13,16,38,82\n55,83,21,16,62,76,56,43,46\n37,14,56,17,18,16,35,67,21,38,59,85,93\n63,18,55,27,98,32,84,19,89,87,52,44,31,95,77,36,66,13,28,29,15,94,75\n55,76,39,16,82,19,34,66,29,96,75\n77,34,84,94,36,55,83,82,41,38,21,39,96,86,16,62,76,56,85,59,47,35,37\n62,17,47,39,37,86,85,74,38,14,93,16,67\n37,67,56,46,85,62,43,68,96,17,86,39,14,76,41,82,16\n89,44,67,98,28,13,75,31,63,87,52,84,34,68,77,32,46\n41,38,39,96,62,17,56,47,35,37,93\n44,84,94,36,21,16,62,76,85,47,35\n34,13,32,83,18,89,94,31,84,27,29,95,19,75,55,63,44,52,66,98,77\n75,68,67,89,74,15,28,63,44,98,29,93,52,18,95,43,13\n84,83,41,16,62,17,85,47,35,14,43\n66,75,29,39,36,82,19,13,95,44,84,96,38,16,34,52,55,86,98,83,21\n38,39,37,93,17,59,68,82,85,46,47,16,67,86,62,14,35\n75,34,27,98,89,28,84,63,36,32,87,55,31\n43,93,74,46,67,68,28,87,63,15,89,31,52,13,75,66,29,98,95,19,44\n28,87,63,15,31,52,13,29,95,19,34,84,94\n32,87,63,15,89,31,13,75,66,95,19,77,34,94,36,83,82\n55,21,17,56,83,98,76,62,39,38,19,29,44,41,84,34,95,86,16\n17,56,59,35,14,93,74,46,67,27,52\n89,27,52,19,77,34,94,21,39\n85,14,74,46,68,28,18,32,87,63,15,27,52,13,75\n83,82,41,38,21,39,96,86,16,62,76,17,56,85,59,47,35,37,14,43,93,74,67\n85,14,74,46,68,32,87,15,31,52,75\n95,27,19,77,98,31,84,94,15,63,34,75,55,83,52,87,13\n13,66,19,77,34,84,55,86,62\n34,84,87,19,13,83,31,29,44,95,15,77,55,36,52,27,66,98,75,89,41\n16,62,76,56,47,37,43,93,87,63,89\n15,13,75,66,34,84,36,83,82,38,21\n14,27,46,18,75,59,63,47,85,67,68,37,43\n63,15,89,27,31,13,66,98,95,19,38\n18,32,63,15,27,31,52,13,75,98,95,19,44,77,34,84,36,55,83\n44,75,27,63,98,18,34,66,68,31,19,95,52,32,28,87,29\n19,44,77,34,84,94,36,55,83,82,41,38,21,96,86,16,62,76,17,56,85,59,47\n67,63,18,93,37,46,17,87,89,43,52,56,35\n98,18,29,89,31\n52,13,66,98,95,19,34,84,94,55,82,41,39,86,16\n67,28,87,75,66,29,19,77,34,84,94\n31,13,55,41,98,38,15,83,27,95,29,89,84,77,21,34,75\n18,31,13,75,95\n27,14,87,63,18,32,29,67,31,89,15,37,13,43,74,47,68,28,35\n93,96,82,41,94,16,17\n17,56,85,37,43,93,74,67,68,28,18,32,63,15,89,31,52\n96,16,76,17,85,59,47,37,46,68,18,87,63\n17,16,77,59,95,44,34\n77,44,75,68,32,29,67,74,15,52,46,87,13,31,34,63,28,66,95,98,27\n68,63,15,13,66,19,84,94,36\n13,75,66,29,98,95,19,44,77,34,84,94,36,55,83,41,38,21,39,96,86,16,62\n15,27,52,13,29,19,44,77,94,36,55,83,41\n43,93,67,68,28,18,32,87,63,15,89,27,52,13,75,66,98,19,44\n55,83,38,39,86,16,76,14,93\n29,84,87,27,34,94,18,55,13,63,77,98,89,66,95\n41,21,96,59,35,46,28\n83,82,41,38,21,39,96,86,62,76,17,56,85,59,47,35,14,43,67\n82,94,39,76,98,75,86\n14,43,74,46,68,18,32,63,15,89,13,75,66,98,95\n94,39,82,41,75,55,98,89,27,66,19,34,21,84,95,77,29\n85,59,35,37,43,93,74,46,67,68,28,18,32,87,15,89,27,31,52,13,75\n59,47,35,37,14,43,93,74,46,67,68,28,18,32,87,63,15,89,27,31,52,13,66\n16,76,17,56,47,35,14,93,74,46,68,18,63,15,89\n39,96,16,62,76,56,85,59,47,35,14,93,67,28,18,32,87\n56,85,59,47,35,37,14,43,93,74,46,67,68,28,18,32,87,63,15,89,27,52,13\n36,82,38,39,62,37,74\n66,59,75,63,15,35,74,28,67,93,37,27,18\n66,84,41,36,87,44,31\n15,77,34,44,67,32,66,29,18,68,13,28,31,27,19,63,89,95,84,94,98\n68,35,86,46,14,93,85,37,96\n27,31,52,75,66,29,98,95,19,77,84,36,55,82,41,38,96\n98,38,85,55,76,84,44,83,95,36,96,82,86,34,17\n55,89,66,29,63,77,87,75,84,31,94,19,34,36,52,32,95,98,27,18,44\n66,29,98,95,19,44,77,94,36,38,96,86,16,76,17\n16,46,85,17,59,74,18,39,47,37,67,93,76,56,32,96,14,28,62\n34,86,59,83,56,94,76,84,17,55,36,77,35,39,38,96,16\n34,84,36,98,95,39,44,66,75,76,55\n41,56,21,82,86,16,47,59,19\n59,47,35,37,14,93,74,46,67,68,28,18,32,87,63,15,89,27,31,52,13,75,66\n41,39,38,13,82,34,83,98,31,95,55,19,86\n29,52,84,31,98,95,77,63,94,83,15,66,38,19,41,55,89\n86,16,62,56,85,47,35,37,14,46,68,87,15\n35,93,37,89,15,46,29,13,18,75,68,14,66,87,52,27,67,74,31,43,32,28,47\n13,82,94,75,55,19,63,66,77,34,98,41,31,95,89,44,87\n41,38,21,39,96,86,16,62,76,17,56,85,59,47,35,37,14,43,74,46,67,68,28\n14,43,93,74,46,67,68,28,18,32,87,63,15,89,27,52,13,75,66,29,98,95,19\n21,39,86,76,17,56,59,37,43,46,28\n67,32,63,93,37,31,29,18,74,15,66,47,52,43,13,75,89\n56,85,59,47,35,37,14,43,93,74,46,68,18,32,87,63,15,89,27,31,52\n87,18,75,27,63,28,19,67,29,43,74,95,31,98,52\n85,38,56,84,98,86,95,34,62,36,16,44,77\n37,67,87,63,27,52,75,98,95\n37,62,21,86,84,83,82,14,47,55,59,34,96,35,36\n96,86,16,62,76,56,85,59,47,35,37,14,43,74,46,67,28,18,32,87,63\n75,66,29,19,94,55,83,82,38,21,16,62,76\n35,43,74,75,66,29,98\n85,47,35,17,28,87,32,59,76,37,39,56,93\n18,14,68,67,63,62,27,32,89,43,46,28,15,93,87\n77,34,94,55,83,82,41,38,21,39,86,16,76,17,59,35,37\n74,21,18,43,14,96,32,62,67,56,59,76,85,39,46\n34,94,36,82,41,38,76,17,85,35,14\n66,86,21,36,84,82,44,34,55,83,52,13,94,75,29,98,16,19,39,77,96,38,41\n16,34,86,76,59,41,39,94,21,47,83,84,62,38,82,36,96,17,56,55,35,77,85\n86,62,76,56,85,59,47,35,37,43,93,74,67,32,15\n35,14,43,93,74,46,67,68,28,18,32,87,63,15,89,27,31,52,13,75,66,29,98\n85,27,59,47,67,35,37,15,62,56,46,89,76\n37,14,43,93,74,46,67,68,28,32,63,31,52,13,75,66,29,98,95\n77,27,83,98,32,31,18,63,19,66,94,55,29,36,34\n77,84,94,55,83,38,76,17,56,59,47,35,37\n44,87,32,13,98,28,94,66,52,89,77,67,75,95,34\n59,35,37,14,93,46,18,32,87,15,89,27,31,52,13,75,66\n41,47,28,74,17,68,43\n93,74,46,67,68,28,18,32,87,15,89,27,31,52,13,75,66,29,98,95,19,44,77\n85,59,35,68,28,18,89,31,75\n32,87,15,89,44,84,94,36,83\n84,34,29,41,94,66,75,27,38,63,52\n98,19,77,34,84,36,55,41,21,39,96,86,16,62,76,56,85\n27,52,75,66,19,44,77,34,94,55,82,38,39\n62,76,17,85,59,47,35,37,14,43,93,74,67,68,28,63,27\n82,39,16,62,17,59,47,14,46\n89,27,52,13,75,66,98,95,19,44,77,34,84,94,36,55,41,38,21\n17,74,46,15,56,43,37,32,14,68,76,31,47\n63,38,27,31,44,98,55\n21,41,39,59,86,82,36,94,55,38,47,93,17,14,85,43,16,76,37\n75,98,95,44,77,55,38,16,76\n39,96,16,62,76,56,59,37,43,93,74,67,28,18,87\n86,77,59,94,76,83,44,96,39,35,85\n55,83,96,59,93,74,46\n86,17,93,67,28\n29,98,19,44,77,34,84,94,36,55,83,39,86,16,62,76,56\n31,52,75,66,95,77,34,84,36,83,21,39,86\n46,67,68,85,15,18,17,59,63,28,56,87,31\n89,27,31,52,75,29,19,44,34,36,82,38,21\n31,52,67,19,87,29,44,77,63,28,94\n62,16,41,14,17,86,35,59,21\n44,86,21,34,59,94,85,96,76,35,41\n21,95,75,15,19,77,66,94,36,13,52,82,98,44,29,31,84,38,83,27,41\n38,39,96,16,62,56,85,59,35,37,14,74,46,67,68,28,18\n55,21,96,86,37,14,46\n98,63,27,44,15\n39,96,86,62,76,56,85,35,37,74,46,67,68,18,87\n83,59,37,38,82\n98,19,34,83,82\n39,19,59,84,47,77,16,34,56,44,83\n56,47,83,62,55,44,21,59,35,76,39,16,86,17,82,77,41,36,38,96,34\n14,28,18,32,27,31,29\n83,21,89,52,39,34,41,77,31,44,29,55,84,38,98\n68,31,13,98,44,84,36\n34,84,82,86,76\n29,32,19,63,66,44,89,52,46,98,77,18,75,67,84,13,95,27,68,15,28\n63,89,27,31,75,29,98,19,84,94,36,55,83,82,41\n35,86,67,28,37,76,93,63,68,59,18,47,32,96,17,43,16,14,85,62,56\n29,52,55,19,13,15,89,83,31,77,75,63,82\n77,34,36,55,83,82,41,96,16,62,56,85,37\n35,59,86,37,74,47,62,46,38,76,14,83,39,43,82,41,55,16,17,93,96\n56,59,47,37,74,18,32,89,27,52,13\n31,93,89,63,35,15,52,87,74,68,32,98,67,37,28,18,46,14,75\n74,47,15,43,63,35,86,62,67,56,93,68,46,16,32,87,17,37,18,59,14,28,76\n93,74,63,89,31\n47,35,37,14,43,93,74,46,67,68,28,32,87,63,15,89,27,31,52,13,75,66,29\n68,63,15,31,98,95,19,44,77\n93,46,68,28,18,87,63,15,89,31,52,13,66,29,95\n77,85,95,21,94,39,41,86,98,17,34,56,83,84,82\n63,87,14,46,43,47,56,13,67\n62,76,17,43,93,74,68,63,27\n85,47,43,93,74,68,63,15,89,27,52,13,75\n17,35,37,14,93,46,68,87,15,27,52\n27,34,77,98,19,32,75,89,13,18,28,74,68,46,95,52,44,67,29\n95,13,66,27,84,44,36,89,63,55,28\n41,38,21,39,96,86,16,62,76,56,85,59,47,35,37,14,43,93,46,67,28\n52,13,75,66,29,98,95,19,44,77,34,84,94,55,83,82,41,38,21,39,96,86,16\n86,56,35,59,83,37,82,55,96\n89,47,63,18,74,62,28,43,59,27,14,76,67,15,93,68,85\n86,87,67,43,47,39,14\n18,55,89,77,19,13,66,29,28,32,52,27,94,95,44,15,36,87,34,31,84\n35,37,14,43,93,74,46,67,68,28,18,32,63,15,89,27,31,52,13,75,66,29,98\n77,34,84,36,41,38,39,96,86,76,56,85,59\n31,66,84,94,82,21,86\n93,32,87,52,75,66,29,19,77\n74,46,27,18,37,28,14,87,93,66,35,67,52,15,29,75,98\n98,66,29,75,15,68,27,67,31,13,63,93,89,46,52,87,95,18,32,19,44,74,43")


  (valid? {47 #{13 61 29 53}, 97 #{75 13 61 29 47 53}, 75 #{13 61 29 47 53}, 61 #{13 29 53}, 29 #{13}, 53 #{13 29}},
          [75 97 47 61 53]
          )

  (part-1 (parse in))
  (part-2 (parse in))




  (str/blank? "47|53")
  )

