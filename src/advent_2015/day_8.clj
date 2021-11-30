(ns advent-2015.day-8
  (:require
    [advent.util :as util]
    [clojure.string :as str]))

(defn parse [in]
  (map str/trim
       (str/split-lines in)))


(defn in-mem-count [s]
  (reduce -
          (count s)
          (map (fn [s]
                 (cond
                   (= s "\"") 1
                   (= s "\"") 1
                   (str/starts-with? s "\\x") 3
                   (str/starts-with? s "\\") 1))
               (re-seq #"^\"|\\x[0-9a-f]{2}|\\[\\\"]|\"$"
                       s))))

(defn in-mem [s]
  (-> s
      (str/replace #"^\"|\"$"
                   "")
      (str/replace #"\\([\\\"]|x([0-9a-f]{2}))"
                   (fn [[_ c hex]]
                     (if (str/starts-with? c "x")
                       (Character/toString (Long/parseLong hex 16))
                       c)))))

(defn part-1 [in]
  (- (util/sum (map count in))
     (util/sum (map (comp count in-mem)
                    in))))


(defn escape [s]
  (str \"
       (str/join (map (fn [c]
                        (get {\" "\\\""
                              \\ "\\\\"}
                             c
                             c))
                      s))
       \"))


(defn part-2 [in]
  (- (util/sum (map (comp count escape)
                    in))
     (util/sum (map count
                    in))))

(escape "\"\\\\\\x27\"")

(comment
  (def input "\"azlgxdbljwygyttzkfwuxv\"\n\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\"\n\"merxdhj\"\n\"dwz\"\n\"d\\\\gkbqo\\\\fwukyxab\\\"u\"\n\"k\\xd4cfixejvkicryipucwurq\\x7eq\"\n\"nvtidemacj\\\"hppfopvpr\"\n\"kbngyfvvsdismznhar\\\\p\\\"\\\"gpryt\\\"jaeh\"\n\"khre\\\"o\\x0elqfrbktzn\"\n\"nugkdmqwdq\\x50amallrskmrxoyo\"\n\"jcrkptrsasjp\\\\\\\"cwigzynjgspxxv\\\\vyb\"\n\"ramf\\\"skhcmenhbpujbqwkltmplxygfcy\"\n\"aqjqgbfqaxga\\\\fkdcahlfi\\\"pvods\"\n\"pcrtfb\"\n\"\\x83qg\\\"nwgugfmfpzlrvty\\\"ryoxm\"\n\"fvhvvokdnl\\\\eap\"\n\"kugdkrat\"\n\"seuxwc\"\n\"vhioftcosshaqtnz\"\n\"gzkxqrdq\\\\uko\\\"mrtst\"\n\"znjcomvy\\x16hhsenmroswr\"\n\"clowmtra\"\n\"\\xc4\"\n\"jpavsevmziklydtqqm\"\n\"egxjqytcttr\\\\ecfedmmovkyn\\\"m\"\n\"mjulrvqgmsvmwf\"\n\"o\\\\prxtlfbatxerhev\\xf9hcl\\x44rzmvklviv\"\n\"lregjexqaqgwloydxdsc\\\\o\\\"dnjfmjcu\"\n\"lnxluajtk\\x8desue\\\\k\\x7abhwokfhh\"\n\"wrssfvzzn\\\"llrysjgiu\\\"npjtdli\"\n\"\\x67lwkks\"\n\"bifw\\\"ybvmwiyi\\\"vhol\\\"vol\\xd4\"\n\"aywdqhvtvcpvbewtwuyxrix\"\n\"gc\\xd3\\\"caukdgfdywj\"\n\"uczy\\\\fk\"\n\"bnlxkjvl\\x7docehufkj\\\\\\\"qoyhag\"\n\"bidsptalmoicyorbv\\\\\"\n\"jorscv\\\"mufcvvfmcv\\\"ga\"\n\"sofpwfal\\\\a\"\n\"kcuqtbboaly\\\"uj\\\"k\"\n\"n\\\\c\"\n\"x\\\"\\xcaj\\\\xwwvpdldz\"\n\"eyukphh\"\n\"wcyjq\"\n\"vjx\\\"\\\"hjroj\\\"l\\x4cjwbr\"\n\"xcodsxzfqw\\\\rowqtuwvjnxupjnrh\"\n\"yc\"\n\"fpvzldgbdtca\\\"hqwa\"\n\"ymjq\\x8ahohvafubra\\\"hgqoknkuyph\"\n\"kx\\\\mkaaklvcup\"\n\"belddrzegcsxsyfhzyz\"\n\"fuyswi\"\n\"\\\\hubzebo\\\"ha\\\\qyr\\\"dv\\\\\"\n\"mxvlz\\\"fwuvx\\\"cyk\\\"\"\n\"ftbh\\\"ro\\\\tmcpnpvh\\\"xx\"\n\"ygi\"\n\"rw\\\"\\\"wwn\\\\fgbjumq\\\"vgvoh\\xd0\\\"mm\"\n\"\\\"pat\\\"\\x63kpfc\\\"\\x2ckhfvxk\\\"uwqzlx\"\n\"o\"\n\"d\\\"hqtsfp\\xceaswe\\\"\\xc0lw\"\n\"zajpvfawqntvoveal\\\"\\\"trcdarjua\"\n\"xzapq\"\n\"rkmhm\"\n\"byuq\"\n\"rwwmt\\xe8jg\\xc2\\\"omt\"\n\"nfljgdmgefvlh\\\"x\"\n\"rpjxcexisualz\"\n\"doxcycmgaiptvd\"\n\"rq\\\\\\\"mohnjdf\\\\xv\\\\hrnosdtmvxot\"\n\"oqvbcenib\\\"uhy\\\\npjxg\"\n\"pkvgnm\\\\ruayuvpbpd\"\n\"kknmzpxqfbcdgng\"\n\"piduhbmaympxdexz\"\n\"vapczawekhoa\\\\or\"\n\"tlwn\\\"avc\\\"bycg\\\"\\\"xuxea\"\n\"\\xcdvryveteqzxrgopmdmihkcgsuozips\"\n\"kpzziqt\"\n\"sdy\\\\s\\\"cjq\"\n\"yujs\"\n\"qte\\\"q\"\n\"qyvpnkhjcqjv\\\"cclvv\\\"pclgtg\\xeak\\\"tno\"\n\"xwx\"\n\"vibuvv\"\n\"qq\\\"\"\n\"wwjduomtbkbdtorhpyalxswisq\\\"r\"\n\"afuw\\\\mfjzctcivwesutxbk\\\"lk\"\n\"e\\xcef\\\\hkiu\"\n\"ftdrgzvygcw\\\"jwsrcmgxj\"\n\"zrddqfkx\\x21dr\\\"ju\\\"elybk\\\"powj\\\"\\\"kpryz\"\n\"dttdkfvbodkma\\\"\"\n\"lzygktugpqw\"\n\"qu\\x83tes\\\\u\\\"tnid\\\"ryuz\"\n\"\\\\o\\\"pe\\\\vqwlsizjklwrjofg\\xe2oau\\\\rd\"\n\"mikevjzhnwgx\\\"fozrj\\\"h\\\"\"\n\"ligxmxznzvtachvvbahnff\"\n\"d\\\\kq\"\n\"tnbkxpzmcakqhaa\"\n\"g\\\\yeakebeyv\"\n\"cqkcnd\\\"sxjxfnawy\\x31zax\\x6ceha\"\n\"m\\x0dtqotffzdnetujtsgjqgwddc\"\n\"masnugb\\\"etgmxul\\x3bqd\\\\tmtddnvcy\"\n\"floediikodfgre\\x23wyoxlswxflwecdjpt\"\n\"zu\"\n\"r\"\n\"\\\"ashzdbd\\\"pdvba\\xeeumkr\\\\amnj\"\n\"ckslmuwbtfouwpfwtuiqmeozgspwnhx\"\n\"t\\\\qjsjek\\xf9gjcxsyco\\\"r\"\n\"hoed\\x1b\\\\tcmaqch\\\"epdy\"\n\"mgjiojwzc\\\\ypqcn\\xb1njmp\\\"aeeblxt\"\n\"\\xdf\\\"h\\x5enfracj\"\n\"\\x6fpbpocrb\"\n\"jbmhrswyyq\\\\\"\n\"wtyqtenfwatji\\\"ls\\\\\"\n\"voy\"\n\"awj\"\n\"rtbj\\\"j\"\n\"hynl\"\n\"orqqeuaat\\\\xu\\\\havsgr\\xc5qdk\"\n\"g\\\"npyzjfq\\\"rjefwsk\"\n\"rk\\\\kkcirjbixr\\\\zelndx\\\"bsnqvqj\\\"\"\n\"tecoz\"\n\"dn\\\"uswngbdk\\\"\"\n\"qb\\\\\"\n\"wpyis\\\\ebq\"\n\"ppwue\\\\airoxzjjdqbvyurhaabetv\"\n\"fxlvt\"\n\"ql\\\"oqsmsvpxcg\\\"k\"\n\"vqlhuec\\\\adw\"\n\"qzmi\\xffberakqqkk\"\n\"tisjqff\\\"wf\"\n\"yhnpudoaybwucvppj\"\n\"xhfuf\\\\ehsrhsnfxcwtibd\\\"ubfpz\"\n\"ihgjquzhf\\\"\"\n\"ff\\x66dsupesrnusrtqnywoqcn\\\\\"\n\"z\\x77zpubbjmd\"\n\"\\\"vhzlbwq\\\"xeimjt\\\\xe\\x85umho\\\"m\\\"\\\"bmy\"\n\"mmuvkioocmzjjysi\\\"mkfbec\\\"\"\n\"rpgghowbduw\\x2fayslubajinoik\\xd0hcfy\"\n\"xrkyjqul\\xdexlojgdphczp\\\"jfk\"\n\"mg\\x07cnr\\x8b\\x67xdgszmgiktpjhawho\"\n\"kdgufhaoab\"\n\"rlhela\\\"nldr\"\n\"wzye\\x87u\"\n\"yif\\x75bjhnitgoarmfgqwpmopu\"\n\"pvlbyez\\\"wyy\\x3dpgr\"\n\"ezdm\\\"ovkruthkvdwtqwr\\\"ibdoawzgu\"\n\"qubp\"\n\"b\\\\kcpegcn\\\\zgdemgorjnk\"\n\"gjsva\\\\kzaor\\\"\\\"gtpd\"\n\"\\\"kt\"\n\"rlymwlcodix\"\n\"qqtmswowxca\\\"jvv\"\n\"jni\\xebwhozb\"\n\"zhino\\\"kzjtmgxpi\\\"zzexijg\"\n\"tyrbat\\\\mejgzplufxixkyg\"\n\"lhmopxiao\\x09\\\"p\\xebl\"\n\"xefioorxvate\"\n\"nmcgd\\x46xfujt\\\"w\"\n\"\\xe3wnwpat\\\"gtimrb\"\n\"wpq\\\"xkjuw\\xebbohgcagppb\"\n\"fmvpwaca\"\n\"mlsw\"\n\"fdan\\\\\\x9e\"\n\"\\\"f\\\"fmdlzc\"\n\"nyuj\\\\jnnfzdnrqmhvjrahlvzl\"\n\"zn\\\"f\\xcfsshcdaukkimfwk\"\n\"uayugezzo\\\\\\\"e\\\"blnrgjaupqhik\"\n\"efd\\\"apkndelkuvfvwyyatyttkehc\"\n\"ufxq\\\\\\\"m\\\"bwkh\\x93kapbqrvxxzbzp\\\\\"\n\"fgypsbgjak\\x79qblbeidavqtddfacq\\\\i\\\"h\"\n\"kcfgpiysdxlgejjvgndb\\\\dovfpqodw\"\n\"\\\"onpqnssmighipuqgwx\\\"nrokzgvg\"\n\"vhjrrhfrba\\\"jebdanzsrdusut\\\\wbs\"\n\"o\\xdakymbaxakys\"\n\"uwxhhzz\\\\mtmhghjn\\\\\\\\tnhzbejj\"\n\"yd\\\\\"\n\"bpgztp\\\\lzwpdqju\\\"it\\x35qjhihjv\"\n\"\\\\my\\\\b\\\"klnnto\\\\\\xb3mbtsh\"\n\"ezyvknv\\\"l\\x2bdhhfjcvwzhjgmhwbqd\\\"\\\\\"\n\"ftkz\\\"amoncbsohtaumhl\\\"wsodemopodq\"\n\"ifv\"\n\"dmzfxvzq\"\n\"sped\\\"bvmf\\\"mmevl\\\"zydannpfny\"\n\"fjxcjwlv\\\"pnqyrzatsjwsqfidb\"\n\"muc\\xfdqouwwnmuixru\\\\zlhjintplvtee\"\n\"mraqgvmj\"\n\"njopq\\\"ftcsryo\"\n\"enoh\\\"n\"\n\"t\\\"ntjhjc\\\"nzqh\\xf7dcohhlsja\\x7dtr\"\n\"flbqcmcoun\"\n\"dxkiysrn\\\\dyuqoaig\"\n\"nehkzi\\\"h\\\"syktzfufotng\\xdafqo\"\n\"dzkjg\\\\hqjk\\\\\\\"zfegssjhn\"\n\"sadlsjv\"\n\"vmfnrdb\\\"\"\n\"ac\\\\bdp\\\"n\"\n\"qt\\x89h\"\n\"lsndeugwvijwde\\\\vjapbm\\\\k\\\\nljuva\"\n\"twpmltdzyynqt\\\\z\\\\tnund\\x64hm\"\n\"hpcyata\\\"ocylbkzdnhujh\"\n\"hskzq\\\"knntuhscex\\\"q\\\\y\\\\vqj\\x3an\"\n\"eekwyufvji\\\\mqgeroekxeyrmymq\"\n\"hl\\\"durthetvri\\xebw\\\\jxu\\\"rcmiuy\"\n\"\\\"fxdnmvnftxwesmvvq\\\"sjnf\\xaabpg\\\"iary\"\n\"\\\"\\\"nksqso\"\n\"ruq\\xbezugge\\\"d\\\"hwvoxmy\\\"iawikddxn\\\"x\"\n\"rxxnlfay\"\n\"stcu\\\"mv\\xabcqts\\\\fasff\"\n\"yrnvwfkfuzuoysfdzl\\x02bk\"\n\"qbdsmlwdbfknivtwijbwtatqfe\"\n\"\\\"erqh\\\\csjph\"\n\"ikfv\"\n\"\\xd2cuhowmtsxepzsivsvnvsb\"\n\"vj\"\n\"d\"\n\"\\\\g\"\n\"porvg\\x62qghorthnc\\\"\\\\\"\n\"tiks\\\\kr\\\"\\x0fuejvuxzswnwdjscrk\"\n\"xmgfel\\\"atma\\\\zaxmlgfjx\\\"ajmqf\"\n\"oz\\\\rnxwljc\\\\\\\"umhymtwh\"\n\"wlsxxhm\\x7fqx\\\\gjoyrvccfiner\\\\qloluqv\"\n\"k\\\\ieq\"\n\"xidjj\\\"ksnlgnwxlddf\\\\s\\\\kuuleb\"\n\"wjpnzgprzv\\\\maub\\x0cj\"\n\"r\"\n\"y\"\n\"\\\"yecqiei\\\"ire\\\\jdhlnnlde\\xc5u\"\n\"drvdiycqib\"\n\"egnrbefezcrhgldrtb\"\n\"plqodxv\\\\zm\\\"uodwjdocri\\x55ucaezutm\"\n\"f\\\"wexcw\\x02ekewx\\\"alyzn\"\n\"pqajwuk\\\\\\\\oatkfqdyspnrupo\"\n\"rkczj\\\"fzntabpnygrhamk\\\\km\\x68xfkmr\"\n\"wejam\\xbac\\x37kns\"\n\"qqmlwjk\\\"gh\"\n\"fdcjsxlgx\"\n\"\\\\cxvxy\\\"kb\\\"\\\"unubvrsq\\\\y\\\\awfhbmarj\\\\\"\n\"geunceaqr\"\n\"tpkg\\\"svvngk\\\\sizlsyaqwf\"\n\"\\\"pa\\\\x\\x18od\\\\emgje\\\\\"\n\"ffiizogjjptubzqfuh\\\"cctieqcdh\"\n\"yikhiyyrpgglpos\"\n\"h\\\\\"\n\"jotqojodcv\"\n\"ervsz\\x87ade\\\"fevq\\\\tcqowt\"\n\"\\\\y\\\"fgrxtppkcseeg\\\\onxjarx\\\\hyhfn\\x5fi\"\n\"kxndlabn\\\\wwumctuzdcfiitrbnn\"\n\"eoosynwhwm\"\n\"\\\"c\\x04\"\n\"ny\\xf6vuwlec\"\n\"ubgxxcvnltzaucrzg\\\\xcez\"\n\"pnocjvo\\\\yt\"\n\"fcabrtqog\\\"a\\\"zj\"\n\"o\\\\bha\\\\mzxmrfltnflv\\xea\"\n\"tbfvzwhexsdxjmxejwqqngzixcx\"\n\"wdptrakok\\\"rgymturdmwfiwu\"\n\"reffmj\"\n\"lqm\"\n\"\\\\oc\"\n\"p\\\"\"\n\"ygkdnhcuehlx\"\n\"vsqmv\\\"bqay\\\"olimtkewedzm\"\n\"isos\\x6azbnkojhxoopzetbj\\xe1yd\"\n\"yo\\\\pgayjcyhshztnbdv\"\n\"fg\\\"h\"\n\"vcmcojolfcf\\\\\\\\oxveua\"\n\"w\\\"vyszhbrr\\\"jpeddpnrjlca\\x69bdbopd\\\\z\"\n\"jikeqv\"\n\"\\\"dkjdfrtj\"\n\"is\"\n\"hgzx\"\n\"z\\\"\"\n\"woubquq\\\\ag\\\"\"\n\"xvclriqa\\xe6ltt\"\n\"tfxinifmd\"\n\"mvywzf\\\"jz\"\n\"vlle\"\n\"c\\\"rf\\\"wynhye\\x25vccvb\\\"\"\n\"zvuxm\"\n\"\\xf2\\\"jdstiwqer\\\"h\"\n\"kyogyogcknbzv\\x9f\\\\\\\\e\"\n\"kspodj\\\"edpeqgypc\"\n\"oh\\\\x\\\\h\"\n\"julb\"\n\"bmcfkidxyilgoy\\\\xmu\\\"ig\\\\qg\"\n\"veqww\\\"ea\"\n\"fkdbemtgtkpqisrwlxutllxc\\\"mbelhs\"\n\"e\"\n\"ecn\\x50ooprbstnq\"\n\"\\\"\\xe8\\\"ec\\xeah\\\"qo\\\\g\\\"iuqxy\\\"e\\\"y\\xe7xk\\xc6d\"\n\"lwj\\\"aftrcqj\"\n\"jduij\\x97zk\\\"rftjrixzgscxxllpqx\\\"bwwb\"\n\"fqcditz\"\n\"f\\x19azclj\\\"rsvaokgvty\\\"aeq\"\n\"erse\\x9etmzhlmhy\\x67yftoti\"\n\"lsdw\\xb3dmiy\\\\od\"\n\"x\\x6fxbljsjdgd\\xaau\"\n\"hjg\\\\w\\\"\\x78uoqbsdikbjxpip\\\"w\\\"jnhzec\"\n\"gk\"\n\"\\\\zrs\\\\syur\"")

  (part-2 ["\"\""
           "\"abc\""
           "\"aaa\\\"aaa\""
           "\"\\x27\""])
  (in-mem-count "\"\"")
  (part-1 (parse input))
  (part-2 (parse input))

  (escape "\"aaa\\\"aaa\"")
  (filter (fn [l]
            (not= l
                  (in-mem (escape l))))
          (parse input))
  )

