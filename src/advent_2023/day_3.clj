(ns advent-2023.day-3
  (:refer-clojure :exclude [symbol?])
  (:require
    [advent.util :as util]
    [clojure.string :as str]
    [net.cgrand.xforms :as xf]))


(defn parse [s]
  (str/split-lines s))


(defn re-locs [re s]
  (let [m (re-matcher re s)]
    (loop [ret []]
      (if (.find m)
        (recur (conj ret [(.start m)
                          (.end m)
                          (.group m)]))
        ret))))


(defn touching? [[s1 e1] [s2 e2]]
  (or (<= s2 s1 e2)
      (<= s2 e1 e2)))


(defn part-1 [in]
  (let [in (parse in)
        empty [(str/join (repeat (count (peek in))
                                 "."))]]
    (util/sum (comp cat
                    (xf/partition 3 1)
                    (mapcat (fn [[_ c :as lns]]
                              (keep (fn [[s e n :as digit]]
                                      (when (some (fn [l]
                                                    (some #(touching? % digit)
                                                          (re-locs #"[^.\d]" l)))
                                                  lns)
                                        (parse-long n)))
                                    (re-locs #"\d+" c)))))
              [empty in empty])))


(defn part-2 [in]
  (let [in (parse in)
        empty [(str/join (repeat (count (peek in))
                                 "."))]]
    (util/sum (comp cat
                    (xf/partition 3 1)
                    (mapcat (fn [[_ c :as lns]]
                              (let [digits (mapcat (partial re-locs #"\d+")
                                                   lns)]
                                (into []
                                      (keep (fn [[s1 e1 _m :as ast]]
                                              (let [touching (filter (partial touching? ast)
                                                                     digits)]
                                                (when (= 2 (count touching))
                                                  (util/product (map (comp parse-long peek))
                                                                touching)))))
                                      (re-locs #"\*" c))))))
              [empty in empty])))

(comment
  (def t1 "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")
  (part-1 t1)
  (part-2 ".479........155..............944.....622..............31.........264.......................532..........................254.........528.....\n..............-...............%.....+...................=....111*.................495.......+.......558..................../..........*.....\n....................791*..62.....$.............847........&........-..........618.*...........818....&..642.........................789.....\n....520.58......405......#....542.../587.............*....198.......846.........*..............*.......*....................647.............\n.........*........./.964..........................474.302.....................786...43..............505..436...................*.....#51....\n......832....@..........*.951*....984*111..801................../.....................-.......@............%.198......322.186...262.........\n..........490........690......346.........................702&.566.%....................192...190.87............*.....-....=..%.........344.\n....*.........................................816*588..............152..535................*.......*...........425...........53.............\n..36.290.831....374................579.536.....................408.......*..733....998....169...146.....%179..........658...............260.\n...........+..../...........795/.....*.*.....................%........776......*..............................790.871./.............281*....\n....................78.............716..400....319........167.................399..@.............................*.........$...599..........\n............719.......*........640................%..376...............800........211......#478......326*93........889..684....*.....285....\n.....852.......-.......462..................374/....%..................*........$..............................603..*..........369..&.......\n........@.........960..................................*...........966.321...925............926...................*.947..&.............574..\n..............%.....$.........$......*.......479....909.339..........*..............803........*17......284$...657.......587......*.........\n...........772............&....345..93...465*................419......676...............-.@521.....-...........................399.662......\n.................17+..2..531.......................79........*...589......198*734....534.........614..................109...................\n.......301............=............................&..321..895..*..........................344.................694............717...511*....\n..........%...707*370.....................473.428........*.....509......=889.....353%.........*................*.......299.......*..........\n........................973.....................*.......877............................&855.955.670.@682.150....958.............197....555..\n................314....*....504*352........602...468..............688.....10-....................#.......*...........306*.............*.....\n.....................987.......................5.............811..*...../.......515..217...........705....462..880.......374......16...42...\n....#......402.$............804..295...406.....&..150........*....22..429...268............324........-.....................................\n.....270..*.....982...644.../.....+....*.........%.............................-..........#.............-...87.......................505....\n938.................=...*.....=98.......370................19@.@867.............................396...272...*......760.......627.#...*......\n..........593.....793....503.........34...............................406.....456...............+.........303........*...........142.432....\n...........*..........&........707..*................563.....837.........+......*.....230..169.......................138....420.............\n..572....689..........503.......#...................*...........*449..........39.........*./......77.......%....404......#..$............739\n...............137...................#.....624.....883..../............................891.......@..........310.*.....404...................\n...287.......*..%............*961.488.........@...........544..........130$................$.......531.72.......424............766..........\n...*......476......316....722............780........613........../533...............96......553.91*......*.835*........*690...*.............\n.350...........%...............470.950...*.............*......................%.....*...728............359.....141..326.......658..832...330\n..........772...127..................-...335......./...539........362......101.....959.............221..................512.........*.......\n..........*..........798.......138..............207.....................................999...574.....*..........484....&............364....\n.......@...919.........*......*.....202.971...............488.................@349........*......*..........404...=........&..448..$........\n.....246........211...426......206...*.....*557..........*....27..659@....588.............367...........961...*..........583.....*..280.....\n...........724.....*......324*.....788..........685....788......@........*....532.................85.......*.139....75.........196..........\n.......377....@..521..........391................./...........@.........987...*.....................*810.214........*.....-........490......\n......................................&....679.........776....447...457......25..............................467..173....241................\n................43........898.412...742......%....*540.*.............*..............825..259...997.514.........*............................\n.......%775..52*...........*..*.........809....871.....384.....295..470............$.......-.....*....*..&.....114.....................=....\n...147...........69=...........914..144*....=..............%.....*......$875.....+............=......278.441.........859.346.281........40..\n......*....................-...............89...........578....519.............676..........473..361..................*....*..+.............\n..78+..42......$...750..465.....218...833.......137...=.............538.783...........*962.........*............*...421.502....../..42......\n..............457.....*.........+........*.....*....825.....26*....-.........238...205.....539.109.348........837..............842...*......\n........#..............175..............925....399.............560.......88.*..............*.....*.................................636......\n......693.......................447.................137............-679......479........619...283...............$458.544.-802.848@..........\n...%.........................&...$....&.+39............*.........................%..618..................*141........+......................\n....471...502....252....663..986...633..............530..117............598.....220...................542......568.......#219..532..15......\n.........*.....-........-...........................................840.$.............717..$...................../............#......*......\n.......351......993......................................573...865...*....$......848.......239.....134....826........409..338...$64..231....\n...........................................=...809.925..*.........*...43..277.....@............571.=........................................\n..........+698..355.....-...594....%...#.55.......*......847..409...............@...............*.....*78..........................#363.497.\n...................#..261.......591..695....-....................=.....678.......714.......364.804.156...................676...605..........\n587.881.....356............192.............957...963.447.................#...63.............*................344.....373........*...........\n..................524..568....&...691...........*.......*...169*218..10....................10.........399.46*............../488.491.16......\n.....824@.....772..$.....&..........*.265...............964............#...............................*........359...556............=.897..\n.............*................293.345.*...161*.....................671.............%414.726.347.....564....................420..............\n.............155.......483....................546........-.....794....*......968.........*...............591.......................$........\n.....806.../.......120.*.......813....................481...........593.........*....667.815.....682........*.%579.......#298....668.188....\n.......*.718.......*....469...*.........251...52*919.......846..................887....*......../........637...........................*....\n.....81.......132.51..........236........-................*.......$.167....338......963..258......844.........884.......*980........#...816.\n..........475.........150..........316........389......590......291..-.....*....662......*...........................143..........284.......\n.......................&.......*..*....390.......+.559..................116.............926........779..................................233.\n500...%......................821..594....*.........*................220...........830*...............*..........89...915.......230.363......\n...*...623....337.......................40..........827..............*................828....$294....392....*....*.....%..............*.....\n.993............*....565........................638...............307.............95.......#..............535.105.........632..938.166..$939\n.....$..444@...378...*.......4...283...971@.......*...................689..937...*.......736......@...................991..@....*...........\n....639....../.....886...........*..............668...88........472...+...*....742...493.........674....*......#........*.....*.78..-350....\n...........290.........*.....%...110........447.......#....-.......*....838..........................167..911.487.....880..493..............\n199...745......189=.389.....676......+........*..=........442....810........................................*.......................795.....\n...................................337........76..728..=......61.....769.............................386.....627..604............$.....*623.\n....=925................................=...............282.............*...597.....851..841...............2......*..........991..139.......\n.......................443....=..........829....................524...816......*......*.*.....752....=468..*...805.....388@...@.......387...\n....14...........*........*...325.................676..............-.........192...859..712....+..........585...............................\n....*.............892...513..........286.984...................301....859.....................................900.................788%......\n...19................................@............424/....155..*...........124.....*......844.......693.......*.............................\n......469..................706.786.............67........*.............115*.....631.164..#............*..852..277.960.................11....\n.....@.......796...850..........*..................354...471.......................................152.........................870...*...824\n................*....*....832..822..............................=114.....881....-.........357*.....................................606..*...\n..574............956.124........................943....278..................&.163...................=199................................761.\n........329.623............308.....210..........*.........*....#...3........................792...........11...676..........................\n.795.68....*........................@.......280..........965.943...............814.....182.*.....454..202*..............971.47.704.....444..\n....*.........407..............558.............#.........................913.....%....%.....801.................652......*....*.....-..*....\n..........552*.................*.........487......321.......852.217.......+..........................=.943..628.......303..........350.288..\n.......@.....................80..................*.....$......*....*..............806*.......&....190...&.................806.235...........\n.......264.538......729.................997....688..435....510......82.285............316.639....................246.......%...*.......#....\n....@.........+.964...............795..+.......................862...............................945.726-....../..../...........210.....700.\n..452.............*...201.....891*.............49.............*........182......483.399.847.......*.............868....204.220.......-......\n.................466..*.............819...=.......184....$...114........+..........*........619....974....603................*.....854......\n.......................624.........*.....42...499*......743.........586.................950....-.............................927............\n........$........379&.......855..252..#.....................120%......*..$...618.......*..................619..........636..................\n....408.462.............528*...........869....145.15.606............581..297...*.....35..-...........218...*.......4*....%.......175........\n.....*........................$20................*..............................901......388...........$.............25...........*......157\n..904........659............$.....762........808...%..........*18.............................#.878..........44..693............611..566*...\n......977.......*.=.......597..-.*..........*.....538.209..............+.....$703......#....347....*652........*..........504...............\n.............668...462........59.2...370...587........*.............819..............152......................919.......+..*................\n.....817................688&.......................848..................639...347...........*671.......240........900..122.951..............\n....*..........268/..........*821...........449..........817........86...*...............431......404..&...660....*..................691....\n....611.............661....59........256.........$.......*...........+.548.........746.............+...........975...669.577...353....*.....\n580.........307......&................*...........634.100....339......................*.................................*..........387......\n.....594.......*........&445...........675....+.............*......857......*........192.251..........504...................../.........534.\n.923..........954.............329.150......727....80.......474........$..385.............=...515*97..#......876.573...199.....921...........\n...*.....236...............74........*..............*..............................919.........................*.......*..803....../460.....\n.94......*.........133.......@........894.826........316............&.........%........349..752/.%.....735...........838.....*..............\n...............425.*............985*........%......&........77....653....622...659.964..*.........147.-....................-.671............\n......252...........898.....640.....841.........988..........*.............*.......*...805....................=...&.....410..........$743...\n.........*.....................*985.........407........$756..506.........-.332....934..................253..972...903........887.199........\n......#...548........608*471.........142.....*...685...............729.172......-............................................&....#.....-558\n.....855..........%...........82.......*..729.......*.509.....850..@......./.910.......202........447..#........481......908........*.......\n.............$.641...758..846.*.....225...............*...374...........158........231.......775....*..837...............$...........549....\n.......587.615.......*.........844.......59......135..88.....*................437...*...........*.374................186....%829.167........\n...691*............573..............293.*...........*.....940....105..38..........98........%.64.........432*737........*...........*.......\n..........727..........@.682........=....50......324................*.......446...........51.....360....................175....227..426.....\n566..186./......*277..18....*779..*.........+...........226..........184.....=......696..........+............344...550...........*.....65..\n.......*..................&........728....521.....277.........&.123........+........*....+......................*....@...20.....385....*....\n.....85........37....=....438.442.............522...*...757.248....*577.27..466..862...503.406*767........*736...164.......*...........233..\n.................#....640.....*........834...........23...*...............*............................404...........-......785.............\n.........*..................89..877*....#..572..........22............891..........295.354*864...875............=..&..706.........-.........\n.......307............59............510...*....187.*247............+........#..741*..............$.......$608.316.355......*.....916....858.\n..................745......705*590.......815..@...........296.....540...=..742........843.*44.......718.................309.............*...\n....................*............................*888.......%..........278..............@.......878*.........797..$366..................95..\n......890....*96.894......765......170/.......260.......149.......................759........................*.................940..........\n.......*....................*..#...................57*....-.........................&..858................312......881.........$.......627..\n....149......392..633.....581.947.#...955...151*......44.................942..............+..........14........210*.....................*...\n............/......*..889.........792...........201............#.....450*............................*...*................127.........921...\n....567#........253...*......................................817...........85*106......239*.......920..16.969......%......*...............76\n.........*537..........583.512*...............*393.43...............513#...........906..............................562.681....*713.........\n......260.....48..560..........376.........140............................151......*........823*.....921..........&.........224.............\n...............*...%......=.................................716.........*..=...769.....@........65........13....29.................509......\n.............61..........836..&29......357............106......*199...996.......*....545.................=...........@106......420*.........\n....#...........#487.433...............#....81........*.......................488........382.....&.............#............................\n..33............................908$........*........133....470&......-...........894...%......70..............626........253.*915..........\n..................139*134.185...........49..142..../...............685..312...95..+..................985...#.......831.....@................\n........*..................&.....................487..........822*........@....*.....................*....919......*...................*....\n98......931......*98..................................@..710$...................522...915.583.72......592..........169.......353....365.678.\n..............559......570...........................485..............#....582.......*.......*............*..................*....=.........\n.......+...38..........*...506.........811.....+188......766...623..363....*......*.914............#.@..92.365.........../...694..312..156..\n........59...*.......405...*..........*......%..........&.........*.........515.586.......239@...571.80..................852...........*....\n....737.....608..........362...336....642....606..................262......................................209.........................617..")

  (part-2 t1)



  (take (drop -1
              (parse t1)))
  (part-number? (parse t1)
                0
                0
                3))
