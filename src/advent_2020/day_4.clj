(ns advent-2020.day-4
  (:require [clojure.string :as str]
            [clojure.set :as set]))
(def specs
  {"byr" {:valid? (fn [v]
                    (<= 1920 v 2002))
          :parse #(Long/parseLong %)}
   "iyr" {:valid? (fn [v]
                    (<= 2010 v 2020))
          :parse #(Long/parseLong %)}
   "eyr" {:valid? (fn [v]
                    (<= 2020 v 2030))
          :parse #(Long/parseLong %)}
   "hgt" {:valid? (fn [v]
                    (let [[_ v u] (re-find #"(\d+)(cm|in)" v)]
                      (boolean (when-let [v (and v (Long/parseLong v))]
                                 (if (= u "cm")
                                   (<= 150 v 193)
                                   (<= 59 v 76))))))
          :parse identity}
   "hcl" {:valid? (fn [v]
                    (boolean (re-find #"^#[a-f0-9]{6}$"
                                      v)))
          :parse identity}
   "ecl" {:valid? (comp boolean
                        #{"amb"
                          "blu"
                          "brn"
                          "gry"
                          "grn"
                          "hzl"
                          "oth"})
          :parse identity}
   "pid" {:valid? (fn [v]
                    (boolean (re-find #"^\d{9}$"
                                      v)))
          :parse identity}
   "cid" {:valid? (constantly true)
          :parse identity}})

(defn parse [in]
  (into []
        (map (fn [d]
               (into {}
                     (map (fn [[_ k v]]
                            [k ((get-in specs [k :parse]) v)]))
                     (re-seq #"([^:\s]+):([^:\s]+)"
                             d))))
        (str/split in #"\n\n")))



(def passport-fields
  (set (keys specs)))

(defn valid? [passport]
  (let [d (set/difference passport-fields
                          (set (keys passport)))]
    (or (= d #{"cid"})
        (= d #{}))))

(defn part-1 [in]
  (count (filter valid?
                 (parse in))))


(defn valid?* [passport]
  (and (valid? passport)
       (every? (fn [[k {v? :valid?}]]
                 (v? (get passport k)))
               specs)))


(defn part-2 [in]
  (count (filter valid?*
                 (parse in))))


(comment
  (part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

  ;; invalid
  (part-2 "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007")

  ;; valid
  (part-2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

  ;; 128 - wrong
  (part-2 "eyr:2033\nhgt:177cm pid:173cm\necl:utc byr:2029 hcl:#efcc98 iyr:2023\n\npid:337605855 cid:249 byr:1952 hgt:155cm\necl:grn iyr:2017 eyr:2026 hcl:#866857\n\ncid:242 iyr:2011 pid:953198122 eyr:2029 ecl:blu hcl:#888785\n\nhgt:173cm hcl:#341e13\ncid:341\npid:112086592\niyr:2012 byr:2011 ecl:amb\neyr:2030\n\npid:790332032\niyr:2019\neyr:2023 byr:1969 ecl:brn\nhgt:163cm\nhcl:#623a2f\n\nbyr:1920 eyr:2023 cid:146 pid:890112986 hgt:171cm hcl:#b6652a iyr:2017 ecl:hzl\n\nhcl:#c0946f byr:1967 cid:199 ecl:gry\niyr:2012 pid:987409259 hgt:157cm eyr:2021\n\npid:316587303 iyr:2016 eyr:2023 ecl:blu byr:1959 hgt:186cm hcl:#733820\n\nhcl:#fffffd hgt:152cm byr:1996 ecl:gry eyr:2024\n\necl:brn hgt:185cm\npid:648491325 byr:1967\nhcl:#172f67 iyr:2014\neyr:2028\n\npid:328737320 iyr:2017 hcl:#fffffd hgt:178\necl:#35fad5\nbyr:1959\n\niyr:2010 byr:1943 eyr:2028\nhgt:178cm hcl:#888785 pid:572750267\n\ncid:175 ecl:brn eyr:2026 iyr:2017\nhcl:#5d69b9 byr:1998 pid:289515215\nhgt:151cm\n\nhgt:182cm\necl:blu eyr:2028 iyr:2011 hcl:#a97842 pid:758494126\n\niyr:2023\nhgt:174cm hcl:cafc2f\necl:utc\ncid:299 eyr:2026 pid:57963956\n\npid:853993893 ecl:blu hgt:188cm cid:294 hcl:#341e13 byr:1975\neyr:2027 iyr:2015\n\necl:amb hgt:70 iyr:2018 pid:241872490\nbyr:1962\neyr:2024 hcl:c5f0c3\n\npid:994754974 eyr:2029 iyr:2017\ncid:137 byr:1966 hcl:#733820 ecl:blu\n\necl:utc\npid:937481632 hgt:190\neyr:2025 iyr:2027 byr:1949 cid:84 hcl:d3f4f5\n\ncid:129 ecl:brn hgt:91 eyr:1932\niyr:2020 pid:298540404 hcl:#888785 byr:1986\n\npid:416051368 eyr:2020 iyr:2011\nhgt:93 byr:2023 hcl:#efcc98\necl:amb\n\nbyr:2010 hgt:181cm\niyr:2020\neyr:2040 pid:785862801 ecl:#f84ccd\n\nhgt:152cm iyr:2013 pid:932020343 eyr:2023 ecl:hzl byr:1920\nhcl:#fffffd\n\nhgt:152cm\niyr:2020 byr:1993 hcl:#18171d pid:120354938\neyr:2021 ecl:gry\n\neyr:2025 byr:1996\nhcl:#623a2f\niyr:2018 ecl:oth cid:177 pid:904738945 hgt:164cm\n\npid:45042993\nhgt:193 iyr:2018 byr:2026 eyr:2026\nhcl:#623a2f cid:175 ecl:brn\n\nbyr:1956 ecl:hzl iyr:2020 pid:892810672 hgt:164cm eyr:2021 cid:186 hcl:#efcc98\n\ncid:314 hgt:180cm\necl:amb hcl:#602927 byr:2027\niyr:1958\neyr:2020 pid:#b2b732\n\ncid:50\nhcl:#a97842\nhgt:155cm pid:667716485 ecl:gry iyr:2014 byr:1995 eyr:2029\n\neyr:2030\npid:783631610\nhgt:74\nbyr:2014 hcl:z iyr:2003 ecl:grt\n\necl:#d4d852 iyr:2029 hcl:z hgt:185in\neyr:2034 pid:#526166\n\niyr:1946 eyr:1957 byr:1941 pid:632690435 ecl:oth hcl:18a37c hgt:59cm\n\niyr:2013\neyr:2026 pid:002380966 cid:95\nhcl:#623a2f byr:1965 ecl:oth\n\necl:gry pid:479214778 hcl:#18171d\nbyr:1928 cid:98 eyr:2020 iyr:2019 hgt:163cm\n\nhcl:#bd0f54 cid:225 eyr:2024 hgt:153cm iyr:2020\nbyr:1956 pid:048565668 ecl:hzl\n\necl:#5d8b3b hgt:156cm eyr:2029 pid:#3df0cc byr:1967 cid:118 hcl:e23f0f\n\npid:437721309\nhcl:#cfa07d hgt:181cm iyr:2019 cid:224 eyr:2028 byr:1946\necl:gry\n\nbyr:1962\neyr:2023\nhgt:182cm\npid:733248003 ecl:blu\niyr:2014 hcl:#efcc98\n\nbyr:1966 hgt:158cm\neyr:2029 hcl:#602927 iyr:2013\npid:963663665\n\npid:529595074 byr:1940 eyr:2020 hcl:#c0946f cid:113 iyr:2015 ecl:oth\nhgt:191cm\n\necl:hzl\nhcl:#a97842\nhgt:157cm eyr:2025 iyr:2015 byr:1978 pid:579525362\n\necl:oth hcl:#18171d cid:76 iyr:2011 eyr:2021\nbyr:1953 hgt:173cm pid:976483712\n\necl:brn hcl:#fffffd cid:242 pid:588299934\nhgt:69in byr:1947\niyr:2010 eyr:2030\n\nhcl:#fffffd ecl:gry cid:93\npid:731904973 byr:1939 eyr:2029 iyr:2016\n\npid:96716037 iyr:1938 eyr:2032\nhgt:192 ecl:gmt byr:2029 hcl:02edc5\n\nbyr:1958\niyr:2011 eyr:2029 ecl:gry\npid:526931024\nhcl:z\nhgt:59cm\n\nbyr:1966 ecl:hzl\npid:378066668 hcl:#c0946f\niyr:2017 eyr:2026 cid:142 hgt:184cm\n\npid:1134356373\niyr:2019\necl:grn\nhcl:#ceb3a1 byr:1950\nhgt:154in\n\nhgt:157cm ecl:zzz byr:1976\npid:5047305958\niyr:2013 hcl:#341e13\n\neyr:2024\nhcl:#a97842 hgt:179cm pid:543943316 cid:214 ecl:brn\nbyr:1999\niyr:2017\n\nbyr:1983 eyr:2024 hgt:177cm\necl:hzl\niyr:2013 pid:328883228 hcl:#ceb3a1\n\ncid:226 pid:262286178 iyr:2010 ecl:grn byr:1962 eyr:2025 hcl:#efcc98\nhgt:60in\n\niyr:2029 ecl:#559ffe hcl:z\ncid:156\nbyr:2003\nhgt:178\npid:93994500\n\niyr:2026 hcl:#eda7f3 ecl:amb\nbyr:1921 eyr:2021 pid:153cm\n\nbyr:1988\necl:amb hgt:178cm hcl:#2aea45\npid:70722502 eyr:2027 iyr:2015\n\npid:555636800 eyr:2020 hgt:182cm iyr:2019 byr:1948 cid:325\nhcl:#733820\n\neyr:1973\niyr:2024 hcl:z byr:2028 ecl:dne cid:169\npid:43596015 hgt:170in\n\nhcl:#b6652a ecl:gry\ncid:264\nhgt:178cm iyr:2027 pid:23042405 byr:1947 eyr:2024\n\nbyr:1989\npid:266274644 ecl:blu eyr:2023 hcl:#733820\nhgt:192cm iyr:2018 cid:272\n\niyr:2013 hcl:z hgt:73cm byr:2014 pid:192cm ecl:#1627a7\n\npid:816749378 hgt:178cm\nhcl:#733820 eyr:2029\nbyr:1993\n\nhcl:0cacc5 byr:1944 iyr:2028 eyr:2024 hgt:163in pid:74169361\necl:dne\n\nhcl:#ceb3a1\necl:grn\neyr:2027 pid:481186415 hgt:176cm cid:325\nbyr:1986\n\neyr:2028 hgt:174cm ecl:brn\nhcl:#888785 iyr:2015 pid:064161451 byr:1969\n\necl:brn cid:323 hgt:167\nbyr:1993 iyr:1953\nhcl:z eyr:2023 pid:161542750\n\necl:#895336 eyr:2033 hgt:67cm cid:254 iyr:1967 hcl:z byr:1954\n\necl:#9dbea3 iyr:2018 eyr:2035\nhgt:183\npid:747501524 hcl:#fd8515 cid:110\nbyr:1997\n\necl:utc\niyr:2014 hgt:183in byr:2007 eyr:2026 hcl:#cfa07d\ncid:334\n\necl:gry hcl:#b6652a pid:250420128\neyr:2024 byr:1939 hgt:170cm\n\nhcl:#18171d eyr:2030 byr:1925 pid:204206116\niyr:2014 hgt:174cm ecl:hzl\n\niyr:2020 hcl:#efcc98\npid:393444692\necl:oth hgt:152cm byr:1957 eyr:2021\n\nbyr:1973 eyr:2024 iyr:2014 hcl:#602927\npid:832320186 hgt:166cm ecl:grn\n\npid:192524664 hgt:182cm\nhcl:#18171d\necl:oth eyr:2030 byr:1942 iyr:2013\n\nhgt:129 byr:2016 hcl:6734a1 ecl:#915282 iyr:1949 cid:130 pid:677408864 eyr:2030\n\neyr:2028 ecl:gry hgt:171cm byr:1960 iyr:2020 pid:688526729 cid:262 hcl:#733820\n\necl:grn iyr:2019 pid:775867641 hcl:#bf1e29\nbyr:1920 hgt:163cm\n\neyr:2029 hcl:#866857\niyr:2024\npid:170cm byr:1931\nhgt:177cm ecl:hzl cid:312\n\niyr:2019 ecl:#19fef5 pid:2080468234\neyr:2008 hgt:72\nhcl:e14dfe\nbyr:1980 cid:272\n\nbyr:2025\ncid:163\niyr:2020 pid:758946748 hgt:161cm\necl:amb eyr:2023 hcl:026d4d\n\niyr:2021 cid:109\neyr:2032 byr:2010\nhcl:#fffffd pid:874586711 ecl:hzl hgt:142\n\neyr:2023\nhcl:z iyr:2019 byr:2013 ecl:#b42611 pid:164cm hgt:60cm\n\nbyr:1935 cid:226 hcl:#602927 ecl:blu pid:700452129 eyr:2029\niyr:2016 hgt:180cm\n\nhgt:179cm\npid:181cm\necl:gry cid:309\neyr:2029\nhcl:#6b5442\nbyr:1987\niyr:2010\n\necl:gry byr:1986 hcl:#cce4b8\npid:101583943 iyr:2010 hgt:65cm eyr:2021\n\ncid:168\npid:#8556c9 hcl:413944 eyr:2036 byr:2013 iyr:2012\necl:#66dc1c hgt:59cm\n\neyr:1984 byr:2017 pid:#cbc84e hcl:z\ncid:189 iyr:2021 ecl:blu\nhgt:152cm\n\nbyr:1993 hcl:#004c11 eyr:2026\niyr:2010\necl:brn hgt:188cm\npid:889959941\n\nhgt:172 byr:2008 eyr:2030 iyr:1959\necl:oth hcl:#a8ebbb\n\niyr:2013 hcl:#8f97b1 hgt:182cm ecl:grn cid:89\neyr:2029\nbyr:1974\n\neyr:2025 hcl:#6b5442 pid:222418968 byr:1951 cid:105 ecl:hzl iyr:2011 hgt:181cm\n\niyr:2017\necl:brn pid:481721303 hgt:190cm\neyr:2037 byr:1990 hcl:#ceb3a1\n\nhcl:z\npid:85905429\nbyr:1923 cid:260\neyr:2031 ecl:gry\niyr:2022 hgt:180cm\n\nhcl:#fffffd hgt:76cm\necl:grn\npid:39254112 byr:2010 iyr:1961 eyr:2028\n\necl:brn iyr:2016\neyr:2027 hcl:#efcc98 pid:753268957 hgt:60in byr:1943\n\necl:oth\npid:087762106 hgt:190cm\nbyr:1974 cid:171 hcl:#c63f21\neyr:2020\n\necl:#8c1b6c\nbyr:1962 iyr:2007 pid:106672731\nhgt:172in cid:239 eyr:2026 hcl:#b6652a\n\nhgt:170cm eyr:2021\ncid:219 hcl:#a97842 pid:040224991 byr:1950 iyr:2018\n\nbyr:2029\neyr:2036 cid:309\niyr:2016 hgt:167cm hcl:#fffffd\necl:#1ab23b\n\niyr:2013 ecl:gry eyr:2020 pid:947828194\nhcl:#18171d hgt:163cm byr:1971\n\nhcl:#fffffd iyr:2011\npid:150105713 eyr:2029\nhgt:168cm byr:1925 ecl:hzl\n\ncid:253\nhcl:#341e13\neyr:2025 hgt:184cm\npid:651786830 byr:1936 iyr:2013\necl:hzl\n\npid:7328393469\nhgt:175cm ecl:gry iyr:2012 byr:1963 hcl:#623a2f eyr:2026\n\neyr:2029 pid:669044398 hgt:161cm hcl:#cfa07d ecl:gry iyr:2018\n\npid:920006222 byr:1941 ecl:grn eyr:2027 cid:87 hcl:#733820\niyr:2018 hgt:171cm\n\nbyr:1964 hgt:157cm hcl:#a97842\npid:756972774 eyr:2024 iyr:2013 ecl:gry\n\niyr:2010 cid:279\nhgt:189cm byr:1959\necl:brn eyr:2022 pid:937686753 hcl:#602927\n\niyr:2013\neyr:2027 cid:223 pid:145547438\nhcl:#6e6f47 hgt:165cm ecl:amb\n\nbyr:2009\necl:oth hcl:#623a2f\nhgt:166cm eyr:2034 pid:120339592\niyr:2012\n\neyr:2026 hcl:z byr:2018 pid:7809314464 iyr:2012 hgt:158cm ecl:hzl\n\ncid:291 hgt:168\necl:#7734de iyr:2021 hcl:5b4ef1 pid:3381158334 eyr:1956 byr:2003\n\niyr:2015 hcl:#cfa07d\nbyr:1971 eyr:2023\necl:oth pid:560419063 cid:155 hgt:170cm\n\neyr:2021 hgt:189cm iyr:2014 hcl:#6b5442 ecl:brn cid:287\nbyr:1951 pid:936881112\n\niyr:2013\neyr:2020 ecl:hzl hgt:150cm cid:210\npid:032458640 byr:1920 hcl:#6b5442\n\nhgt:180cm hcl:#cfa07d ecl:grn eyr:2027\npid:140859202 iyr:2014 cid:232\nbyr:1932\n\npid:68300657 byr:1988 hgt:181cm\necl:hzl iyr:1951 hcl:e18469\neyr:2013\n\npid:157572693 hgt:185cm\nhcl:#065fe8 ecl:gry eyr:2027\niyr:2014\n\neyr:2029 hgt:164cm hcl:z ecl:grn cid:270 iyr:2019 byr:1993 pid:338068138\n\niyr:2016 cid:131 byr:1990 hcl:#7d3b0c ecl:grn pid:066023454 hgt:154cm\neyr:2025\n\niyr:2019 eyr:2024 hgt:174cm\npid:855792798 byr:1920 hcl:#cfa07d\n\neyr:2020\nhgt:74in iyr:2013 ecl:amb\nbyr:1920 hcl:3f6214\npid:957164804\n\npid:756767000 hcl:#220540 byr:1922 hgt:172cm\neyr:2023 cid:305 ecl:hzl iyr:2019\n\nhgt:193in eyr:2025 pid:117240526 iyr:2017 hcl:#888785 byr:1928 ecl:blu\n\nbyr:1942\necl:blu\ncid:347\nhcl:#fffffd eyr:2023 iyr:2017\nhgt:154cm pid:836554235\n\neyr:2023 hcl:#efcc98\npid:364475403 byr:1962 iyr:2015\necl:brn hgt:59in cid:289\n\nbyr:2021 hcl:ca4bcf hgt:88 iyr:2017 ecl:gmt pid:181cm eyr:2032\n\nbyr:1937 iyr:2014 hgt:154cm ecl:brn\nhcl:#866857\neyr:2022 pid:234591437\n\niyr:2002 cid:139 byr:1982\nhcl:#c0946f\npid:#62721b hgt:159in eyr:1966 ecl:brn\n\neyr:2036 ecl:oth byr:2026 hgt:96\npid:137651094\nhcl:z\n\npid:373485985\niyr:2030 ecl:gry byr:2011 hgt:65cm\nhcl:#733820\n\npid:390979357\necl:gry hgt:164cm\nhcl:#ceb3a1 eyr:2029 byr:1932 iyr:2015\n\necl:hzl hgt:68in eyr:2023 pid:829734763 iyr:2016 hcl:#733820 byr:1997\n\nhgt:150cm byr:1926 iyr:2019 pid:521908229 eyr:2029 ecl:brn hcl:#623a2f\n\neyr:2023 byr:1974\niyr:2018 cid:58\necl:grn\npid:310883188 hcl:#866857\nhgt:164cm\n\nbyr:1963 iyr:2019\nhgt:162cm\neyr:2021\nhcl:#fffffd ecl:oth pid:104734523\n\nhcl:#888785 cid:150\neyr:2020 byr:1988\niyr:2018\necl:oth\nhgt:179cm pid:635972018\n\niyr:2014 hcl:#7d3b0c ecl:hzl pid:717760687\nbyr:1929 eyr:2027 hgt:183cm\n\niyr:2019 byr:2000\neyr:2025 pid:506581828 hcl:#602927 ecl:oth\nhgt:162cm\n\npid:#fd3377 ecl:#618bce hcl:#ceb3a1\niyr:1944 hgt:182cm cid:57\nbyr:2022 eyr:1995\n\npid:4790730010\nhgt:192cm\ncid:222 byr:2022\nhcl:4798e7 ecl:#5126d5 iyr:1954 eyr:2040\n\nhcl:#b6652a\necl:brn cid:181 pid:983890186 hgt:189cm byr:1998\neyr:2022 iyr:2011\n\niyr:2018 eyr:2022\ncid:58 byr:1994 hgt:169cm ecl:hzl pid:036522894\nhcl:#2c9ee8\n\niyr:1979\nhcl:e09b9c byr:2016\nhgt:121 eyr:1962 pid:#fb14be cid:265 ecl:lzr\n\neyr:2024 hgt:63in hcl:#efcc98 iyr:2018\nbyr:1953 pid:881102827 ecl:amb\n\necl:oth hgt:177cm eyr:2028 iyr:2011\nhcl:#efcc98 pid:113579849 byr:1957\n\necl:#fe1b74 iyr:1926 hgt:70cm\npid:70807766 hcl:556dca byr:2030\neyr:2032\n\nbyr:1997 ecl:blu hgt:105\npid:178655906 iyr:2025\nhcl:#6b5442\neyr:2021\n\neyr:2030 ecl:grt hgt:161cm hcl:#ceb3a1 iyr:2016\npid:318930966 cid:59 byr:1924\n\ncid:200 hgt:67in\npid:229395752 byr:1936 ecl:oth iyr:2013 eyr:2020 hcl:#c0946f\n\nbyr:1990 iyr:2018\ncid:99 hcl:#6b5442 hgt:155 pid:350832537 ecl:blu eyr:2021\n\neyr:2039 byr:2025 pid:247367429 hcl:z iyr:2013 ecl:amb\ncid:118\n\neyr:2027 hcl:z ecl:utc\ncid:274 hgt:175in iyr:2016 byr:1977 pid:478855994\n\neyr:2020 pid:636743032\nhcl:#64a8b8 iyr:2018 ecl:grn hgt:68in byr:1969\n\npid:515635081 iyr:2013 byr:1980 eyr:2024 hgt:173cm ecl:gry\nhcl:#b6652a\n\necl:utc byr:2026 iyr:1999 eyr:1937\nhgt:66cm\nhcl:z\npid:2247643960\n\niyr:2013 byr:1942 hgt:154cm eyr:2020 hcl:#18171d cid:323 pid:302753381 ecl:oth\n\necl:xry pid:346719476\niyr:1999\neyr:2020 hgt:154cm hcl:z\nbyr:2027\n\nhgt:160cm eyr:2025 hcl:#fffffd byr:1998\npid:678119271 ecl:blu iyr:2014\n\nhgt:161cm iyr:2011\necl:blu\nbyr:1921 pid:236833613 eyr:2021\nhcl:#623a2f\n\necl:hzl hcl:#18171d hgt:151cm pid:541887993\nbyr:1995 iyr:2019 eyr:2021\n\npid:496474711 byr:1966 ecl:gry eyr:2025 hgt:176cm\nhcl:#b6652a iyr:2018\n\niyr:2010 hcl:#efcc98 pid:351846405\neyr:2024 hgt:150cm\nbyr:1941\n\nhgt:151cm ecl:gry hcl:#a97842 pid:586789406\neyr:2022 iyr:2013 byr:1982\n\nbyr:1994 eyr:2028 ecl:gry\nhcl:#888785 iyr:2010\nhgt:165cm cid:183\n\niyr:2015\nbyr:1933 hcl:#733820 hgt:167cm\necl:blu pid:914665208 eyr:2027\n\neyr:2031 hcl:6804ef\necl:amb\nbyr:2024\nhgt:157cm iyr:1938\npid:#0418fb\n\nbyr:1936\necl:oth hgt:190cm cid:91\npid:711544430 iyr:2020\neyr:2025 hcl:#888785\n\npid:381452527\neyr:2027 hcl:#efcc98 ecl:brn\nbyr:1956 hgt:63in\n\necl:oth\niyr:2014 hcl:#ceb3a1\ncid:254\npid:544612871 byr:1985 eyr:2023 hgt:172cm\n\nhcl:#efcc98\nhgt:191cm byr:1948\necl:blu eyr:2028\npid:953894279 iyr:2017\n\nbyr:1968 pid:875469219\nhcl:#efcc98 hgt:176cm cid:141 iyr:2017\n\neyr:2022 hcl:#733820 ecl:hzl\npid:870733357 iyr:2013\nbyr:1949 hgt:150cm cid:252\n\necl:gry\nhcl:#602927 pid:632246684 byr:1986\neyr:2030 hgt:152cm iyr:2013\n\neyr:2029\niyr:2016\nbyr:1969 pid:595125675 ecl:gry hcl:#cfa07d hgt:184cm\n\nbyr:1947 hcl:z\ncid:188 eyr:2038 pid:177cm iyr:2011 hgt:166cm ecl:#c1376b\n\necl:hzl hgt:170cm cid:307 eyr:2022\nbyr:1971\nhcl:#b6652a pid:047040501\n\nhgt:126 ecl:zzz\nbyr:2019\npid:170207910 eyr:2035 hcl:23df48\niyr:1932\n\nhgt:152cm cid:270 eyr:2036 ecl:#408f6e iyr:1952 pid:5808880830 byr:2022\nhcl:0b1ba6\n\neyr:2021 hgt:179cm\nbyr:1938 pid:140937061 iyr:2030 hcl:#a97842 ecl:oth\n\nhgt:67cm eyr:2028 pid:816355657\niyr:2019 byr:2008 hcl:z ecl:#5b4f31\n\ncid:192\niyr:2018 eyr:2020 byr:1983 pid:873720366\necl:grn hgt:187cm hcl:#6b5442\n\nbyr:1955 hgt:71in iyr:2018 pid:320019385 hcl:#6b5442\ncid:324 eyr:2027\n\npid:957860464\nhcl:#602927\niyr:2011\nbyr:2026 cid:261 eyr:2006\n\nbyr:1989 ecl:gry cid:143 pid:258434299 eyr:2027 hgt:192cm iyr:2017 hcl:#7d3b0c\n\npid:#1742ae\necl:#a61090\niyr:2028 hcl:717dd0 hgt:139 cid:183\neyr:2035\n\neyr:2028 pid:039325804 hgt:167cm hcl:#888785 ecl:oth cid:155 iyr:2013 byr:1923\n\nbyr:1956 iyr:2010\nhcl:#d683bf\neyr:2023\nhgt:70in\ncid:197 pid:143320690\necl:hzl\n\necl:#4004e3 cid:278\niyr:1950 pid:745107377\nbyr:2007 eyr:2036\nhcl:8447eb hgt:74cm\n\nhcl:#ceb3a1 hgt:177cm iyr:2010 pid:640032134\necl:gry\neyr:2027 byr:1958\n\nhgt:187cm\niyr:1921 ecl:#1c7d96\neyr:1987\nbyr:2028 pid:#28e5a1 cid:144 hcl:9fc25d\n\niyr:2012 byr:1996 cid:289 hgt:177cm hcl:#fffffd pid:687240168 eyr:2030 ecl:gry\n\npid:860410143 ecl:dne eyr:2031 cid:206 hgt:187in byr:1927\nhcl:8c2149 iyr:2012\n\niyr:2010\nbyr:1963 cid:139 pid:160019759\neyr:2030 hgt:172cm hcl:#602927\n\npid:309851270\niyr:2014\necl:hzl byr:1939\ncid:71\neyr:2030\nhcl:#b216fb\nhgt:161cm\n\necl:gry\ncid:138\niyr:2014\nhgt:177cm byr:1942\npid:900269082 eyr:2024 hcl:#fffffd\n\niyr:2019 hgt:158cm\nhcl:#18171d pid:941939350 eyr:2024 ecl:brn byr:1944\n\nbyr:2023 ecl:brn\ncid:101 eyr:2016 pid:190078757 hgt:188in\n\ncid:188\necl:blu pid:075499609\nbyr:1970\nhcl:#fffffd hgt:164cm eyr:2028 iyr:2015\n\nbyr:2011 hcl:z\necl:gry\npid:408316491 hgt:64cm iyr:2017 eyr:1968\n\necl:oth hcl:#6b5442\npid:623099801\nhgt:163cm\nbyr:1928\n\npid:165230004\necl:grn byr:1935 hcl:#c0946f iyr:2012\nhgt:185cm\n\nhgt:162cm pid:069876432 byr:1960 cid:326 iyr:2013\nhcl:#cfa07d eyr:2021\necl:grn\n\necl:#f3d8ba hgt:182cm eyr:2020 byr:2007\nhcl:z iyr:2014 pid:6141297559\n\npid:867747198\nhcl:#efcc98\neyr:2030 byr:1989\nhgt:181cm\n\nbyr:2000 eyr:2021 hgt:166cm\nhcl:#fffffd iyr:2019 pid:546346187 cid:111 ecl:grn\n\neyr:2034 hcl:#623a2f\nbyr:1958\npid:60553207 ecl:#76b538 hgt:59 cid:75\n\nhcl:#623a2f\neyr:2023 pid:251940892 byr:1998\niyr:2012\nhgt:181cm ecl:gry\n\niyr:2020 cid:83\nbyr:1938 eyr:2024 ecl:amb pid:046668488 hgt:181cm hcl:#341e13\n\necl:grn\neyr:2036 iyr:1951 byr:2029 hcl:z hgt:177in\npid:135470038\n\niyr:2015 eyr:2023\nbyr:1961\ncid:81 hcl:#a97842 pid:710065884\nhgt:152cm\necl:#1f9801\n\nbyr:2014\npid:25253929 hcl:z\necl:#f3fb41 eyr:2025 cid:255 iyr:1998\nhgt:155cm\n\necl:gry pid:919070381 hcl:#efcc98 iyr:2019 eyr:2021 byr:1995\n\nbyr:1942\neyr:2029\nhgt:191cm hcl:#18171d\npid:649719423 iyr:2018 ecl:brn\n\necl:gry\nbyr:1963 iyr:2016 hgt:188cm pid:024539026 eyr:2022\n\nhgt:176cm\necl:hzl eyr:1923 pid:176188310 hcl:#b6652a\nbyr:1939\niyr:2011\n\niyr:2011 hcl:#888785 eyr:2030 ecl:gry byr:1920\n\npid:#0468a7 hcl:851fe0 eyr:2036 hgt:60cm\nbyr:2030\niyr:1995\necl:utc\n\nhcl:#866857 iyr:2016 ecl:oth\npid:414233531 eyr:2022 byr:1957\nhgt:169cm cid:229\n\ncid:185 ecl:#5f6f53\npid:#20f317 byr:2024 eyr:1988 hcl:z iyr:2023 hgt:158in\n\npid:015894427 eyr:2027 hgt:177cm ecl:blu\ncid:222\nhcl:#c0946f iyr:2010 byr:1993\n\ncid:101 hgt:162cm hcl:#c0946f pid:666662343 ecl:grn\nbyr:1974\niyr:2019\neyr:2029\n\npid:782547454 hcl:z ecl:#b0805f\niyr:2013 eyr:2023\nhgt:159cm\nbyr:1935\ncid:230\n\npid:298008321 hcl:231e1b hgt:166cm ecl:oth\niyr:2026 eyr:2020\n\npid:230201309 iyr:2010 eyr:2025 hcl:#6b5442\ncid:238\necl:grn\nhgt:174cm\n\ncid:287 eyr:2026 hcl:#733820\npid:201750712 iyr:2010\necl:oth byr:1985\nhgt:185cm\n\nhcl:#a97842 hgt:70in eyr:2029\npid:419407059 ecl:grn byr:1987\niyr:2016\n\nhgt:191cm byr:1951\neyr:2027 hcl:#8a9477 iyr:2015 ecl:amb pid:769071985\n\nhcl:#6b5442\niyr:2012 ecl:blu\ncid:336 pid:391608810 byr:1995\neyr:2022\nhgt:161cm\n\niyr:2020\nbyr:1938\npid:927067439 eyr:2027 hgt:173cm\nhcl:306963 ecl:xry\n\nbyr:1991\niyr:2021 hgt:175cm hcl:68b4f3 ecl:utc\npid:037777327 eyr:2026\n\nhgt:64in\neyr:2025 hcl:#da6977 cid:137 byr:1990 iyr:2013 pid:918997697 ecl:amb\n\niyr:2011 ecl:gry hgt:173cm eyr:2023 pid:802831612\nhcl:#733820 byr:1976\n\nbyr:1938 eyr:2021 pid:575395401 cid:234\nhcl:#866857 ecl:hzl hgt:176cm\n\nhcl:#ceb3a1 ecl:hzl\neyr:2035\niyr:2014\nbyr:2019\n\necl:hzl pid:961361236\nhgt:193cm hcl:#efcc98\niyr:2011 eyr:2030 byr:1967\n\neyr:1936 ecl:blu\nhgt:153cm hcl:98d3f0 pid:7296832671\nbyr:1931 iyr:1962\n\niyr:2016 eyr:2024 hcl:#6b5442 ecl:grn\npid:265815316 byr:1966\nhgt:165cm\n\npid:203025149\neyr:2029\niyr:2010 cid:124 byr:1999 ecl:blu\n\niyr:2011 eyr:2028 pid:#7e0612 byr:1924 hcl:#7d3b0c ecl:oth\nhgt:82\n\necl:hzl\nbyr:1941\nhcl:#b6652a eyr:2020 pid:409573276\niyr:1976 hgt:166cm\n\necl:grn eyr:2030\nhgt:163cm iyr:2011 pid:121609314 byr:1961 hcl:#426e1a\n\npid:#49ea2c\neyr:2029 hcl:#6b5442 iyr:1931\nhgt:62cm ecl:brn\nbyr:2012\n\neyr:2038 hcl:8d1f49 ecl:#6d4ea1 pid:0853660207 byr:2020 hgt:71cm\n\necl:hzl hgt:170cm\niyr:2011\nbyr:1966 eyr:2028\npid:609548717 hcl:#c0946f\n\nbyr:1921 hcl:#c0946f ecl:blu iyr:2019 eyr:2024 pid:643387204\n\ncid:324 hgt:162cm ecl:amb\nhcl:#18171d byr:1961\neyr:2027 iyr:2010 pid:939720354\n\nbyr:1933 hcl:#fffffd\npid:353343882\neyr:2025 hgt:171cm ecl:amb cid:329\niyr:2017\n\nbyr:2004 iyr:2022\npid:157cm eyr:2035\necl:#eafe47 hgt:129\nhcl:z\n\ncid:55 iyr:2025 hgt:177in pid:493884348\nhcl:#888785 byr:1925 ecl:#b11d27 eyr:2036\n\necl:hzl\nhgt:171cm\niyr:2012 pid:479669573 cid:335 hcl:#fffffd byr:1953\neyr:2029\n\nbyr:1930 hcl:5bdf31\npid:#b21f8a hgt:164cm\ncid:134\niyr:2023 ecl:lzr\n\niyr:2018 eyr:2026 ecl:grn\npid:541667478 hcl:#6b5442 byr:1992 hgt:155cm\n\nhcl:2a1c4f iyr:2011\nhgt:192cm eyr:2028 byr:2029 cid:270 ecl:dne pid:7995627426\n\nbyr:1929 ecl:oth\npid:954905104 iyr:2016\nhgt:68in hcl:#7d3b0c eyr:2020\n\ncid:167 byr:2000 hgt:186cm iyr:2013 hcl:#ff4019 pid:384287209\neyr:2024 ecl:amb\n\neyr:2022\niyr:2018 byr:1972 cid:290\nhgt:170cm ecl:grn pid:127269636\n\nbyr:1997\necl:amb hgt:150cm\npid:056368047 hcl:#fffffd eyr:2020 iyr:2020\n\necl:gry hgt:167in byr:2020 cid:131 pid:651833067 hcl:#623a2f iyr:2027 eyr:2038\n\nhcl:#56c370\niyr:2014\nbyr:1941\npid:654258425 hgt:184cm\neyr:2025 ecl:hzl\n\npid:571765355\nbyr:2021\nhcl:z\neyr:1921 cid:106 iyr:1978\necl:#1162c5\nhgt:184in\n\niyr:2015 hcl:#18171d cid:237 pid:348578306 ecl:blu\nbyr:1988 eyr:2025 hgt:155cm\n\nbyr:1963 hcl:#733820 cid:145 eyr:2030 ecl:oth pid:964094037 hgt:164cm iyr:2018\n\npid:595618708 ecl:amb\nhcl:#866857\nhgt:186cm eyr:2024 byr:1924 iyr:2014\n\necl:hzl pid:484466493\nhgt:176cm iyr:2016 byr:1983 hcl:#ceb3a1\n\necl:gry hcl:#6b5442\nhgt:185cm\neyr:2029\npid:045583320 byr:1974 iyr:2020\n\necl:brn hcl:352cf1 cid:149\nhgt:184cm byr:2011\neyr:2031\npid:21942403 iyr:2028\n\necl:brn eyr:2029 pid:083295950 byr:1995 hgt:176cm hcl:#c0946f\ncid:68 iyr:2014\n\nhgt:170cm\nbyr:1945 hcl:#623a2f\niyr:2013 pid:912213595 ecl:gry eyr:2020\n\necl:gry\nhcl:#18171d iyr:2015 hgt:185cm eyr:2023\nbyr:1950\n\nbyr:1997 hgt:68in pid:368643584 hcl:#623a2f ecl:hzl eyr:2029 iyr:2012\ncid:239\n\niyr:2003 eyr:2020 cid:99 byr:2027 hcl:2c10a6 hgt:74cm\necl:brn\n\npid:151cm hcl:46a5fd eyr:2031\niyr:2014\nbyr:2005 ecl:xry hgt:176cm\n\nbyr:2011 ecl:oth pid:821123244\niyr:2022\nhcl:839b47 eyr:2039 hgt:150in\n\npid:604669618 hgt:152cm iyr:2013\nbyr:1954\neyr:2021 ecl:amb hcl:#623a2f\n\nhgt:182cm\nbyr:1993\ncid:177 hcl:#b6652a ecl:gry iyr:2011 pid:441649857 eyr:2027\n\ncid:296 hgt:98\necl:grt iyr:2028 hcl:#a97842 byr:2022\npid:69736889 eyr:1935\n\niyr:2016 hcl:#cfa07d\nbyr:1941\nhgt:182cm\npid:720595987 ecl:gry\neyr:2022\n\niyr:2018 hgt:164cm hcl:#650d28 byr:1973 cid:108 pid:#b0df80 ecl:blu eyr:2020\n\nhcl:z\npid:315901778\niyr:2013\nbyr:1999 ecl:#49f691 eyr:2026 hgt:179cm\n\nbyr:1925\npid:555786686 hgt:189cm hcl:#cfa07d iyr:2012 ecl:gry eyr:2028\n\niyr:2016\nhgt:168cm\neyr:2027 cid:60 ecl:gry hcl:#cfa07d\npid:322944081 byr:1993\n\npid:163cm\nhgt:189cm iyr:1997 hcl:03db25 eyr:1970\nbyr:2016 ecl:#6c59eb\n\npid:766719295 iyr:2017\nhgt:168cm\nhcl:z ecl:grt\neyr:2022 byr:2010\n\nhgt:173cm pid:247156751 cid:109 eyr:2022 iyr:2012 ecl:gry byr:1989\n\ncid:288\nhcl:77241f\nhgt:157cm byr:1956 pid:587115461 iyr:2016 ecl:lzr\neyr:2034\n\nhcl:5307c9 ecl:#cc4aff\npid:#d80d30\ncid:224 hgt:72cm byr:2025 eyr:2039 iyr:2025\n\neyr:2027 byr:2015\nhgt:184 hcl:98fb9d pid:58151347\niyr:2029\n\nhgt:183cm cid:187 byr:2019 ecl:xry iyr:2013 pid:164cm hcl:#18171d eyr:2021")

  (defn v? [k v]
    ((get-in specs [k :valid?]) v))

  (v? "pid" "012345678"))
