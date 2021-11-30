(ns advent-2020.day-6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [in]
  (into []
        (map str/split-lines)
        (str/split in #"\n\n")))

(defn part-1 [in]
  (transduce (map (fn [g]
                    (count (into #{}
                                 cat
                                 g))))
             +
             0
             (parse in)))


(defn part-2 [in]
  (transduce (map (fn [answers]
                    (count (apply set/intersection
                                  (map (comp set seq)
                                       answers)))))
             +
             0
             (parse in)))


(comment
  (part-1 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")
  (part-2 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

  (part-2 "lqhksfnerg\nnegsc\nsnage\nengs\nsneg\n\nctfzrdbsapql\nsrldfatzqcpb\nqsntlprfdbza\nldptswouqbxzafr\n\nncjolhqfbp\ngphonqbflxj\njepdmfhsqtonz\ncqpihonjbgf\n\nabytu\nayu\nayu\nuhodlay\nuay\n\nkzx\nxjwk\nkxfhsdc\nxjk\njkx\n\nrwxckmfn\nxpcrkft\n\nntaf\nlacuserbk\n\nsxwdfmnpl\nxpsmnwlfd\nlpxmwsndf\nxfdmlwnps\npwlnmdsxf\n\ngjvcfamuty\nuqfmjgzatpvyc\nyfcjtavghm\namqfjkpgvyct\nsvgcenmylotjafbx\n\nyepqgs\nnsfqwghce\nsegvq\n\nmi\nl\n\neirgvuadbphcfsmyqnzotxjlk\nqjeintsxubmfchoavpyzglrkd\nfdjeyhbxgqclsnaimoptkvruz\niexuntjgmvfhqykspzbdclrao\nhkjgpqlyaxdtcnvobmrzifuse\n\ni\nnl\non\nzbp\n\naqpjnkeowivfbdhgr\npgiojefhvrqdawknc\n\nugmsibf\nmsbigfu\ncsgfimubj\nmfisbgyu\nnsmvuigfbe\n\ndemjnxcwbsri\naypesftqhzluko\n\necoxhl\nlochex\nchelox\nxhcoel\ncholxe\n\nxntubrdemhylsoi\ngfjihrutbaewzpoxlys\n\niuqkewnyv\nuyptkn\nnyupsk\n\ne\nf\n\nbxrsntkjdwivceapq\ncqbakpimsvetnjxwrd\nknvxqwpasurcodietbj\nxvrqbdjiwsacykpent\nnpsavtbqxijkewrczld\n\npk\nqpwsk\nzpk\n\nqdchnb\nqwxbcdhyir\njhbdcq\nhnecdbq\n\ntdryep\nth\nvxg\nbre\ne\n\nf\nd\nd\n\nrspjhlqocixnw\nxicoqjpnlwsh\nlxinqphjoscw\njhlowspincxq\n\nqptb\nbtpq\nguqhbtpd\n\nnzloqtcxmfgdbj\ntcfdnbmzgqlxo\nbqkgnxazecltidsoymf\nwtzmxcpflbongdhvq\nzfroctngqdbmxl\n\nkcyhtoendvzwafbmuj\neltymcnhaowbkdzvuf\nsfkmvobdnteuychzaw\n\nwetrpdnqab\nqrpkenabgwtdl\nnahwtbrpdsef\n\nhqbtefp\nxslnrykmvau\nzghcqtojidw\n\ngqsyhx\nvcent\n\npazioqhkbd\nqnopbmrahdyk\ndxbmkhaqotp\nvkhaofqcdp\nkqpzhaod\n\nytljngqdmxfck\ntcjmkfnqlgydx\nmfctkldxqpjuynvg\nldtfnycxqkmjg\nlndxtfkymgjcq\n\nelrfwpobjxa\nmtpfoarbsdj\n\nqigtfkwhobpvrcyzjuelnd\nzcawdbogrhqnlieyufpkt\nhqlzcnrikfypswutoxbegd\nbutncgpyovzrlfwideqhk\n\nozdnivyjlwkp\nvenpbsuzlodyw\nxfdqnzhwyopvltcr\naizdgmlywponv\n\ntqg\nqt\nrqs\n\nwhm\nvadomyl\nwmnqh\ngwmps\nmcnu\n\nloav\nzdl\n\nvsleinyhgo\nkshrxvaymptw\ncqsubvf\n\nofd\nfod\n\npdjrszhgkfe\ndfshzegk\nedhzfkgs\n\nwti\nftcwib\niwtoz\nwiut\n\njoebqrhgxvst\nalkgprwetohvznqdmcb\nvbrgeoqhyjtix\nthrfequgbxvo\n\nejx\nezfmnx\niderjhxc\n\nhfrqe\nrjambh\n\nytsdznehwg\ndwrohqyvmscgjz\n\nxdatzefv\nkcxwhypoanlr\n\ntbvdcol\ntblcodv\n\nkg\nkyg\n\nwxtezyuomkfdhspqjcn\nxuoqwtphyckfjndzsme\nouzwykhqfcndmspejxt\nncemfpxsquzwyhdtkvoj\n\ndyfvmobrspnlguqaw\nvhalnuwbypod\nxjuawvbptnyohld\n\nbmytkawjszl\njaystwzmkl\nmsatlkwzjy\n\nvghcafoer\ngraecvhzo\nuavrcoheg\nrhgvcaoe\n\nnvjga\navgni\n\ng\na\ng\ng\n\notxj\nxtjqo\nxtoj\njxntoy\nltqjxoa\n\ni\nji\ni\nai\n\nw\npw\nqutdebyio\nw\n\ncamyzohlbrgd\nygcalmzbrdoh\nhrbycdzmalgo\n\nhgsltxvwmc\nloxsntghvdc\nsgxclthv\nctvxlshg\nyxcvsgthlf\n\nekizadqujopgx\nzuagvjsirokwpx\nuoykijgpqaxzc\ngqkoixpyabdjuz\n\nqjygvhmlkdbzsifcwapuonxrte\ndesvfncpryjamkzhgutwoi\nnifhzpertsgmycjwauokdv\nswohafgetuyvicpzjrnmkd\n\ndybpeckxz\nzucgeyipqdnl\nwspyefczlnhd\nprevcytzod\npheyjdwzc\n\nvbpfnedqyoawhcr\nqrhcewvabfopdnmy\nafrvebhdocniykqpw\nfoqeawrbvyhpdcn\nncwaphbvoqdrfey\n\ndwj\nwdzj\n\nmr\nrm\n\nidnkmufx\ndemjykixo\nkjdmxiov\n\nukbflqhomzrwastyjgpex\nvoqkyihjufmxswgrztbc\n\ntjn\nnjv\n\nruika\nirkua\nwairlkszu\n\nrfgphcezl\nzcreg\n\nmce\nmec\nqsme\nmpeyf\n\nwjcxoftvnr\nzomvfjnyw\nnywjofvtm\nkvwhfunajolsigq\n\ny\nn\nn\ny\noe\n\nnvjkryifxace\nziehtgnjfbyrlvka\nxnpvjismakeyfr\n\ng\ng\ng\ng\ng\n\nvwgnj\nivoj\nugjv\n\nmyzrbf\nymczfr\nvmwroxhfj\nmlfdr\n\najqerszpbdyvoufwlmgcnitxk\nwmzuftgdkavjrlibenpoqxcys\ngnpmcsxlwzvfateqdirkyujob\n\nbnfudi\ninay\niuncv\n\ntrqlgxaeouy\nuvoqxktwejyr\notqeuxyr\nxogreiyqut\n\nficdymxhuboq\ndvwiuexfc\n\nzbhndug\nugsdwphnv\nnugdwh\ndughnjio\njgsyuhnd\n\nf\nqrcvkfu\nf\n\nyvf\nyfv\nyfv\n\nqtnc\ncbqn\nqnc\ntcqn\n\nrxyhizgjklumv\nfidxgjlkyhvr\navncxqrjbkgplhiy\nzrdkxtlgjvyhi\n\nzjq\nf\nu\nprgot\n\nhrgktyczsvnmiolewbf\ngbowmtzvynferksichl\nsvgbrzmcolkieynfhtw\nvehwtyrmgsnickfozbl\n\nlbucxdo\njlcgdxiu\n\nhvtafpnwzex\nvnxzwftehp\nhezxvnpfwt\ntnehfxvpzw\n\nwbmyejlv\n\ngevqi\neigv\n\nunogtlfwvcyjdr\njcuvlftdgnwroy\n\nkdlm\npklw\nklfay\nlk\n\nsomjqeuvdk\nrzaqodkmvxsj\n\ng\ng\ng\ng\n\nceihqtrjpbxso\nxohipcersbqj\npibhoxsrjeftcq\npbcqvosehrjix\nicbqhosrpejx\n\njvldsruex\noktrdvslcujx\nxdurvpjsl\nmxesdiujvrl\n\nrgcdsax\nalqsbd\nfdavzs\nijsutahdpye\nbaqdsz\n\njxcabgpuimtekqsy\niduqmetsognaybxp\nyqwaburlxps\n\nulesvzoijdnpfhkacwmxbrty\nzvxrdjskefbthimwnpayoul\nihdnlkxrpgwjbyevozuatmfs\nyixowledpbvrthjkzufsmna\n\nhxibvuzdowysmflpkqjgacnr\nbhctimdakyqlsvxwjprounfg\n\nxolza\naxzl\nxazld\nlzoax\n\nvjpdirbzwmcyeqtfgoulhsk\nshzpugkqlobmridvjyewfc\nybufwljhrcdegsiozqvkmp\nucnbokilmgvqwjzsfphaeryxd\n\nfzwiyxutvmdpbqkgrj\nvmqludbfywzkxpijtg\nqwkuzbfxyjmdptigv\npmgifdkjtbuzywvqx\n\njvxwlqr\nrqfjmsveoaw\nptbyhvjqw\nqwkvjxg\n\nqnaxrduhpfvoyjbksgw\nqjnwieaoxrsfpzdhk\n\nnrcmyksodhaztijuwblv\nksxrufaqhwbcnme\n\nsqx\nsjbql\ngsqnpwk\nsq\n\ncpgkrjvqx\nvgkpqcxjr\nqjxckapvorg\nxjvqckprg\ngkqrcpvxj\n\nojyziuqkbe\nfvuceart\n\nqpgyknd\nkfvyjtd\n\nphfmqdwxnuvgo\nouxdblhnmpgq\namxnpihkdusoqr\n\nrun\ndnx\nvnc\nponklq\nnur\n\nixoytfqugrvjzcdwlhkbesmp\nqalryztjxumhevifnobpcgdw\nbothiuxqjzefcglpwymdvar\nlgfjctrixhzudaqmebpvoyw\nzlwfoituxvdpqbejhycgrm\n\nlrwvkj\njqwc\n\nawvyst\nvtysa\n\nugxys\nx\nx\nx\nx\n\nismv\nmfi\nihofm\n\nlijwuxcbrfotekpangzmsvyd\nonmsdklutybrcqavegwfxijp\nvnyarmslcetoxjkwuqgdipfb\njtrsdmfcnwiveyxlobhukgpa\nmsatlerkovjnfgdicuywpxb\n\nxmyen\nenymx\nmyxen\nxyemn\nxymen\n\noaustqdifyxepkrwh\nkbcvmrjn\n\nuqv\nqrvw\nqv\nvqtmu\nqv\n\nlo\nl\nl\nt\nl\n\naoqujemitxrszgnywvlc\nlauzwhvr\nakulrbdzwv\nlrwdpvhauz\n\njhwycgso\ntrshda\nhs\nsh\n\noydxjrhetainw\nnbwdxloetyiar\nentcioxrwadyh\ntonyexrpdawi\n\nbkdrqlnjwshi\nnkidbwshrqlj\nhrqvfldjinsbkw\nkhdbsnwrjilq\nwjnipadhqkrlbs\n\niusqovtzgajpndcwl\nicstgzpurhbwqnvjoka\n\nhvwcue\nchwnejv\ncxwvath\n\nyroksb\ntbs\nswb\nsb\nsb\n\nvgixnfosrpetb\nrwpndfevylg\n\nalytpjgxn\nstyjnkmegil\nnpfyakjgxlt\nvzdhtlonwygbjq\n\ndp\ndp\npd\ndp\npd\n\ncav\ni\nac\nchzu\n\nkdv\nfld\ndwq\nqds\n\nx\nwanxl\n\ncfborlazjq\npfcloajrwqbz\nbtonaqzidlfrgjc\nlcfjzqsroba\n\no\nn\nkwl\ns\nf\n\noylasgwtbnu\nwstxylzug\nikjvuqwltyhgs\nultbomgwnsyax\n\niotqspcwuhr\nuplinrgcvhews\ntduspchwrim\nsihrcbquotwp\n\nqjdnkl\nldnqa\nqdiknl\nqdmnlv\nsltxwbqnyd\n\nrnzmybp\nzfnwc\nnkazdqg\nuzesvmjn\nvzn\n\nemp\nepm\npem\nmep\nmpe\n\nzxhde\ndemhz\njadu\nckrpvglsw\n\npegn\ngnep\npgne\nengp\n\nhjkeoyrglpcitbva\nubtzsmvakoly\n\nu\nb\n\nksanb\nsbk\nsbk\nskvxdb\nbkfs\n\nz\nz\nzo\nwuqmz\ngz\n\njwbyaugknetqrmoxh\naxwnmobetrjqkdyuhg\nomvjbyhngureqxkatw\n\nlqdiban\nifbazln\nbafzinl\nifsbanl\n\ndovkxjylabqmtizw\npawvoxdmbyizqtj\nvwubiayopgxcjzqdtm\nqbjtvowdnzsixamy\n\nykpusjaqclowb\nkjsclynwqapobu\n\nzasypunbwitorjvmeglqxhkdcf\nbvlirhqwgskftxedonymuapcjz\n\nezxghwr\nrzgxnewh\nerwxzhgv\n\nnkfvplstgaybmhorewzu\nlakohrfuznjdsqvgmywp\ntuyshcfwkpabzomglrnv\nclnsrgvowhufamykzp\n\ngbrln\npb\nb\nbsw\n\nugcnskomzepjvxifrtwq\nvpmkutefjzgwrbsoncxiq\ncmkesufoxvqprwidzgntj\n\nrpsheibkg\nbokwgjeh\n\nsfwgbm\njmfgbws\nsgfumwb\nbfmgws\n\nqfxderyt\nikwyln\n\nnsrjpbtxuvcfgl\nputvcjfrdlsonxegb\nvgbjtrlsfxcpun\nlnbgctvmsjrxiufp\n\ncnawzexsdlubpm\nbufnvdiaxptlcmse\nlbyugexmdncspa\n\nxtvpcfsdzewmgaqb\nfelcxvdpiqnuymow\n\nxoqt\ntqxp\ngtfqjkvbu\ntqa\nqta\n\njplacdti\napfliojtcb\nilcapjtk\natcjilp\ntuipnjleyasc\n\nby\nby\nyob\nby\n\nm\nmp\n\nkenjrtgyvxiqwmazchfbup\ndezplgiwyxrq\n\nra\nwa\naw\n\nqldpihtceyfoksa\ncfsopeatkilynqrdv\nxctlfjsoapiguyqdke\nsyiupwbjlacefokqdt\n\nsc\ntwsq\n\newc\nce\nce\nceiaf\n\nwnqfklygehirdxazup\nvqgepxnsziakrw\neznxgqwpriak\nirnqvzwkpaxbeg\n\ngu\nau\n\nujvidwoe\njekiubdov\nidunjveo\neidvunjo\nevinoyduj\n\nirytzs\njhnr\nr\nmnrjdb\nrnv\n\nug\ngu\nug\nug\nug\n\nifndua\nrafyduin\nufhvnadi\ndyguamifn\n\naihwljuqkezp\nwzaiqlp\niplagzwq\nqwizmpla\n\npvedlxntwjf\nveldpnftwj\n\nxvglbzusemwqihn\nvkdbefxrtimp\n\nspcwi\npjewi\nidpyfb\nivp\npwzioj\n\ne\nv\nza\nz\ng\n\nmjvxyaguoc\nqlhypbgw\nqegylr\ngklyrniw\n\ntv\nl\nl\n\ntmrzoldvsiu\nowzpglsidvtmu\nhmtjvlodxsubyzi\ntuvilzdoksma\n\nyenvdmsagq\nnihfbxaymg\n\nhcylanqdjmwrpixbo\nqmwxybpcdalohn\nmxwoazyphndcblq\nwzbydamnoqxhcpl\nmnlcadybwxpqoh\n\nmiupnhbvj\nuwpnbhxjqiv\n\nrqgta\nawkxo\nhzjsdb\nawfu\n\ngskncrox\nnpulc\n\niugzcseolh\ngshioluezc\n\njqbi\npjqi\nbqj\nnjoqv\n\neitjbcr\nlpskbhyfrgxuzoiawv\n\nfejdyanx\ncyfjekxdon\n\ngqzniubejkx\nzjingbqhuexk\ngknzxqcuijeb\nuxjnzvergikybq\n\nwub\nw\ntarx\n\nghpi\nhiasg\nahip\nyicd\n\nbrysq\nhkgfbnmacr\nwprtbyqls\n\nlzfqot\nqolfz\nlqfzkor\nqopmzdeflcx\ntoqflz\n\nvhremdpywtabokuilx\noryhfcztaixlvmdwup\n\nmwvqzngluxpcoeidk\nufcgnxjvmdzqikhlpw\nlwvkxmciuepndgqz\nglmzpqknieudvxcws\n\nibrduwhlcxtsnpyvamekzjf\nzmuevtynhpfskralcwjxibd\n\nrvdpebnwsiugmaqzxj\nsidlbyakwrhvgmjoef\n\nq\nq\nq\n\njofkmbcunaxet\nelqngabhskmy\nwkmxacenib\n\nezfpogadrbulys\nsbamgenyopfzu\n\nutagkdzvwoqemchb\nzthluvqinacgb\nqcjhvutilznabgy\n\ngduwcpj\nbcxgdswpurj\npujdwmcg\n\nzxcagirye\nuzrljgq\ngwtzhfrb\ndxrgze\n\nm\nm\nm\n\nmzf\nzfm\nzfm\nmfz\nzfmo\n\novdqzm\ndmqvoz\nzmdqvo\n\nbisprlndef\nabvltrnkxd\nrndbyl\n\nzmglywve\nlyvegwz\n\ngtmrhpujzfqskoxavydie\ntypwiahqfnkdumvgoxresz\nqzevtxauogipcksyrhfdml\n\nrcova\npjcab\nupacjb\nalc\n\nkzp\nkpz\nyz\n\naivymnlux\nmyuxvlias\nlmyauixv\nviumlxay\niyavulrdexm\n\nsmgjbqifah\nbixgqsfmha\ngsfqpnmhbilae\n\nyb\nfy\n\nhfvcgd\nhfkxcugdo\nfhemcdgo\nhgqfxdcy\nbegthcfrdqi\n\ngcwtjyluzvbsnrkopmeai\nuzbtyinrqmofsjdkcvwega\n\ns\ns\ns\n\ndsvpg\ngdp\nndegr\ndigp\n\njiylzgcomkurtvqpa\nyvmjtapingcruo\n\nauijcbwfqkxyshl\nxwhcyqukbislfaj\nyiqcljkxfasruhbw\ncjlfkxyqwuhiasb\niayklqhucsjwxbf\n\nz\ns\ns\nsi\n\nbrayc\ncykwbra\nrxbcva\nckdabrh\n\nrlfvawcqemhub\nctuqfsbaehm\n\nwbrzpyacqhi\nbmpalyhzgrc\nvbazpnyshk\nhzyijfacbp\n\nwgjohx\nsxhjgwo\n\nalhkqy\ndahfmjlcy\n\ng\ng\ng\ng\n\nlpsbynokvrudcgzqmtijhx\nsnuhqjlokypfrbvcmidxg\nyhwbjmxopvirguqscnldk\n\nmwzsclu\nyuwslzm\nalsumwz\n\nmaj\nmjxal\n\nasgryznlwqjdfcpkehbm\nknehsopgjdrqa\ndnapshekjgvqr\najendtksguqphr\n\npuznxmqsrtfdckvyijbalg\nkcgytbmsxvrunqjzlfaip\nyuikxpqtafvbnszgjmcrl\nqkvagxcjslbytnifpumzr\n\npglvfdihqa\npntwkbzeco\nep\n\nfpjmzitnlkvbd\nqdbknvtjfilrzm\nbijznpmlkdtfv\notbjfznlmvdhsik\n\nx\nx\ngx\nxk\n\nb\nqge\nkycnzjw\npixlb\n\ncpzgaufikyltmbqvesoj\nfusibtgoayvjqkpe\novtfbasiekjugryqp\niubyfkjqvsoepgat\nakofugibyqetpdsjv\n\nyirboahn\nozvha\noah\nhoav\naehco\n\nhspqcxmjrbfyktngi\nsptqigfklmadncxrvybh\n\nwdksztumeroaplhxqbyvjfin\ntadflskombxyhqewjrvipzn\nafrejdkobnhstqyilpmxvwz\nvzynhwtafpdqlkjoirxmebs\nkqjpwxrdvtizfmlhenysbao\n\nwonxesaihurqkymd\nsxyqkmwdbnljrgueiao\nwhenxordaumqksyi\nkrouemnawsidyqx\nrmsukxeyqonwida\n\nckywportslfhdaqixvmjn\ngkvlapqwfscjzruoy\n\ndtevjz\nfeqndzgxlvrtboc\nzteuvkd\ndszvuheti\ntvdewz\n\nehaqfwylskdbzvmpgt\nvagbfeqwptszykhmld\narpzbsvmweytfugjdchqlk\ngpmvqkawhdstzlyebf\nbtaesiwkvpfdhzymlqg\n\nhozuack\nyuaznf\n\nozvmpady\npaydovmz\n\ngtmbeikr\nbreigtmk\ngebkritm\nkmibgert\n\njm\nm\nml\n\nfrexcnogyv\nnrvgyzilodxufeh\n\nbtvzjpu\nzjvtubap\nbtzujpv\njdeptzvub\nbvzpjqut\n\nfkdcirhqt\ncidhrkvfq\nicqdhrfvk\nvktrchidfq\nldqascokxrhfig\n\ntlgjdsoapmciey\ngitdwojaeslmynpc\ndtycalegpmosji\nmydoitvespcjlga\ngladijvyoctfsemp\n\nhwieujqgsxc\nsgxebjhnuqwc\nkqgcxwuhtvesj\nqgecmsjxwhu\ncwjqmugeshx\n\nqjwldfsri\niqgbedvlsfrh\nujnldiksfqmpyrw\n\na\na\na\n\nspotgrheckwqan\nshwvnatgpkqcyoerz\nhcgasrpewnoktq\ntkewponscaqhrg\n\nbjrsxkeiucnpoz\nxrkczspbfeu\nkqcuytdsrhvgplbxa\n\nat\nae\n\nx\nx\nv\nx\n\nds\ndr\nxemdno\n\nbvpdiocu\npzvhuyxci\n\na\nu\n\nd\nd\n\nrplowaxqj\nrqvowa\nyrombawnsteq\nacujvqowr\nwrxloaupq\n\ns\nk\nk\nk\n\nzmn\npfgseobimq\nmdkza\nmzyvd\nkwhmr\n\nuqmtreg\ntwleuoqm\n\nhm\nmh\nwhm\nhm\n\nydegnjfmkr\nkvglzrfs\n\nkovgzemjf\nkfyighomzelj\nfkzxtueogjm\nfxkjozgmue\nzofbvejmgk\n\nzatrhwfjiopulke\nsatohknewfuipjlzrv\nekioxurwfljpztah\nlgqzopirjkhwaufte\n\nxgbozwltnevidfrjuhcysk\ngwsmtbkroyfinadpjhevcxq\n\nx\ndx\nmxzr\nx\n\nkaof\nkodf\nofk\nfko\n\nrkuoazney\nazyrqhtdo\nyalobrzcw\nxioayfrze\n\ncusyzmjhqloftdpakegin\nlmnjipdyogkfahuqstcze\nmneocjwgilpfxzthsayqudk\n\nm\nhwyug\nymna\ndrptzijlc\nwm\n\npg\npg\n\nlrobg\norqfnsv\nnqao\nojmwdecz\n\njmxlkan\nmlauxtj\nolcwmeaxqjnk\njxbpmltar\nmljxdiszyagfv\n\nsgriuf\nugmpoxvyq\ndgzcuqwnh\n\nm\ne\nm\ny\n\ndqkh\nqdhk\nhkdq\n\nznfwuiodcryqsmbpvejg\nxnqwcjrdebpzmgovysi\npihmyewrcgnjzqvdabsuo\ntqdsnjwzovrbcgeiamfyp\n\nmeztpcwrsnojquxkg\nmekurwxtncojsqg\nktjwqocsnugxerm\n\neplzc\nezpcl\nlzypec\n\njmlrkseozxdunqf\ndknseojuqzmlx\nnsxqljdeukbmiz\nsqmvnxubokizlejd\nctqsjmlkpwxzdunghe\n\nklqgxyrdt\ntoykrdqgl\n\nfwkzcuvmogxjrbelthi\nzfhtgbqnomvwxku\nfuozbthvwkqgmx\n\ns\nx\nx\n\npefqgwzlimyjncouh\nqaduwhmysxjopieftrl\nojfphequiwmnykl\n\nksfb\nsbkf\nsfbk\nfbqvks\n\neomabw\nwemarv\n\nghldr\niw\nyj\npo\n\nlpihfvqersujkzyb\nvjlfrebkiq\njblqifrevk\n\nzgr\nzg\nzg\nzg\ngz\n\nhfvydluaxgipsq\notqcbfurygsvempk\nljnqdswvgpufy\n\ngdxbjivufyolq\nusmbflyxiqdv\n\nkpfy\nyrfpk\nfpyk\n\noencuyp\nuceypoi\ncypueo\n\norcfsjhvxaqynibuk\ncbixuyokaqvrnhsjf\nqcoyfjxukrsnhavbi\nfkyuhbvojnsxaqrci\nvrcshuqxyfniakojb\n\nmvgbupxhfesodnzwjcq\ndcjxzqyuepnvhfbsmgo\nkpmzrsfebgaovnhcxu\nnpxgivuohmwsjefcbz\n\nxaujvbesdqgnz\naugndvqbcjfzs\n\neoyhpnrcldjbqfvtiua\nlaoyrgqzbuhndxwpvf\n\npxfnblvuzijhdq\nhdkljpbz\n\njphqxmkwgsycurietbadzfv\nvweygbzashxqcdji\nwcbjdniysvhqeaxgz\ndqxwhvbozjaiysgce\nzwxghisaqbycjevd\n\nbzqgcuerktfsavwhnoydi\nrbvyqdsiokcuganewhtzm\n\ntom\nto\nto\nto\nto\n\nrkfmtd\nvkcunbq\nrk\nkf\n\npkmifxjtwnz\nkmsfanxteihjp\nnmaftdji\nybqimuvojntf\n\nns\nr\nr\ncvhm\nwnias\n\nmfsxucvboyj\ncxomsbyvfj\nfsbvomjyxc\nmjhcvysbofx\nxmfjcybovs\n\nquxomeysk\nuqtokyxsm\nyqohxbkm\nuyoqkaxm\n\nbr\nrbi\nbrj\n\npaqhkzmcwfrsiubtvjnx\nzktiqbucarpwnxmyhj\naikmgtyucrjnpqwhxzb\njkabpxztcrwhuqnmi\nmnxzwbaitqrkujhpc\n\nszbanq\noegiflu\nas\nqpt\nt\n\nqsu\nfaqiu\nqsui\nuroqckjv\n\nufkcrahgitoyj\nxknlcfujagd\ngjcklauf\njkwafgcu\n\ngdmxn\nboiygrca\nhgs\n\nybwclvjfkumxso\nnvmkyowedlijuxcfsb\nlzmqscybvxfukro\n\nfiprvjsgcenhtmalxzqo\nphcgntoqmeviajfxlszr\nzsxqctnevliohrpagmjf\nsqgipcuanmtxlrvjehozf\nvlczqjgoipafetmrnhxs\n\nypwbf\npfwytb\npyfbw\nbtfwpy\nybpgwf\n\nwrzhafdpmx\ntzspfhxnd\ndkfhzwst\nbgelqjdcyfizvhou\n\npeubvwgaqiclkfdxmsyjtr\nkuwpsvlxbjrditgmqeacyf\nbtxejuiqcrpgslmadvfywk\ngwalifqpdusrvmtjybcekx\n\nqvmhulrt\nwcpzdsifex\n\npohq\nhqop\nopqh\noqhp\n\nen\niws\ntpr\nqs\n\nlvfinmpcbzx\nhkcsblgz\ntcgzbkl\ncblzw\n\nrsjie\newjribms\nijers\n\nknizwxycoqe\ndnqocbk\npkgsanof\n\nhravnysejobwi\nnlfusgdxtpqmaki\n\nkuvboax\nkjvohxa\n\nqwm\nmwq\n\nxowbafsitjz\nlawm\naylmw\nwanur\n\nqcdsao\ndqaos\nopaksqd\n\np\np\np\np\np\n\nwl\nd\nl\nn\nrwa\n\nrdi\ndri\ndir\nird\nrdi\n\nei\ne\ne\ne\ne\n\nfavclpbni\nicbfalnpv\n\nuokmvxgafizceprnydsht\npkeaonhzruqxgvdfmscytil\nfpxhsgcrutovzykaenimd\naszthkimoyxdufvcregpn\n\nfgibsklo\ndsokalnriegjzbf\nkvwlocuys\n\nicnortpmfwy\nvnqrofpmycit\n\nyaiempzjbvkgnrfco\ntozadmejkbgqfcsiw\n\ncrgtkhbajzdmlp\nepnsvfhib\npvbhn\n\nfglxtqrb\nuigxvbft\nwxflbyg\nxbpzndhfegj\n\njiolrqxfapuwdngybhktzcv\nbnerupsdkflaqjzvitcmxo\n\nznjus\nqzsjna\nznjs\nuzjsn\nzjxnks\n\nsnwj\nw\nw\npw\nw\n\ni\np\ncd\nqs\n\nf\nofm\nf\nf\n\ndv\nav\ncxkuo\n\nrbxieoyczljtuqvm\ngnphsrckqifzeljw\n\nh\nh\nh\nh\nh\n\nexghrdkbivqsmcwal\nvhicaqdmlbrywxsgek\nmrvcbskheadwliqxg\nyrmckldaqeghisvwxb\nalgoshwxzmbrqjdevcik\n\nomtswbxujpvfzd\nyntcbxwl\naqtxyewb\ntiybhgrkxw\n\nsrkxfjlgtpedai\ndigtrkjsxyapq\ndjxrgapiktse\n\nfzlagmcnjwbd\nchlzbgunwfj\n\nqszghewkntxaomu\nxekowrasunz\nxnksoeawzu\nwjxaoduensbzfk\n\noacfnvhugekystwp\nftncaykejwhbomz\n\nabrcsi\nxctrvokbsg\nyaescfbr\n\nvjwbpsleug\nghlwjekpbusv\njpuelswvfbg\nyjwpvbeslgu\nuwjegbpsvl\n\njkvqesnto\nkjot\njkot\njotk\notjk\n\nelf\nnlg\nvzl\nyl\nyl\n\nrgbf\nyrzmhbn\nrbv\nbtrp\nfzrb\n\nuyetaqw\nwheyaut\nateuyw\nawyeut\n\nswuaon\nusnwao\n\nudcyfksltzeqb\nbkuycfstdqzel\n\ngjmbfxieklohpc\noglhiuyxjekcmfpr\nokelfixjpgmch\n\nzixq\nziq\nqgiz\niezq\n\nsdbjl\nvyjsdb\nglkdnjb\nhdefnbj\nopjbmd\n\nid\nd\n\nl\nl\n\ndsnze\nsenzd\ndzesn\n\ncegqnbzyhtm\novaksmjwrdbpicxul\n\nfzjxbck\nndjp\nheyrsgaqm\ntzof\nxvdn\n\nkxtimcjayh\nsyavicktmxh\nmpxcfatihky\nkyxitmcah\ntaxhmyickv\n\nxlwjibshfgontek\nnjeixkslbogfhwt\nhiwlbojxnktgfes\n\nudsygjltikqrmv\ntulkrgijvm\nteumkjvlgir\nvgjkulrimt\nrghkmlvpejtiu\n\noegptdlksbixhcwqavjun\njacqedxslunipgh\nenjgslzcmxupaqdih\nzcndhlaqsepxugij\nnqajehxpilcdgsu\n\nshdieqwxflurjzby\nwmtajifoqcupgnkb\n\nxwck\nao\n\nzkpfgvda\nkdgya\nqwnbiheos\njr\nmuxfjcl\n\nwamxpysnl\nqpydmrnj\nnmypsl\n\nk\nxtvweuplk\naykc\nkyf\n\nuryfiabjvelptkmnsxow\npfcaoerluxdtnwjqbskm\n\nwqenhkxgypc\ndnltwxzaofs\n\nsrxmeabuodzq\noaeqdrubmzk\nkdeaurobzqm\n\nrfungq\ngfunqr\nrqgufn\nnftougqr\n\njqthdmlfacp\nliqumfdcj\nqtjmglcskaf\nlrbzwqyxoenmfjc\n\ndgz\nfj\nx\ng\n\nqnhwlzjoac\nwlzoajhcqn\njwqzcanohl\nmwlzqonhjac\n\nasqtlpwnrm\nnparsmd\nsarnmpf\nnspamxr\n\not\nozt\n\ncuokwgjmhxzidteslq\nytfqwpieraosbjn\n\nczoxdyvhasnit\nzaxiovcynhts\njuoyhvxatbnsiz\n\nowsjger\noerwgjs\nrwsojeg\nojswgre\newjsocrgv\n\ncmexibr\nyxbir\nirbx\n\nnbgpodvsxmzfqiteuj\nmnvtqbpxuszjod\njtsqpovbxndmzu\nspxbzqmodvjtun\noumjnpsvztdbqx\n\nvrwytk\nkglvfrw\nzxwusdonikph\nmwgkj\nwarykt\n\nrkdijofslupwqgtvaxhy\njzdsuprykoiwfvlxaq\nwfboijqpurvslkxdya\nskylwcmauoexpijdvqfr\n\npe\nep\ne\neb\ne\n\nuzrmjehfktblxsivqpo\nrovklpubzdsfiheqxmt\nekzuprvbtisxoflhqm\nimuhkftvbrepzslqxo\nfmrtxblpzeosuvikhq\n\noauifsgwqbtvkzxjycpml\nkqmgzxjbwcoyiuvlatfps\npwjmqczoytaxgksvbiful\nyqwsazxilvutcmbjopkfg\nqtfdpxkmbzwacsgoiluyjv\n\nykcjafmxlbehuqtiwvs\nzqxudnmysc\nngrpcymsqxu\n\nqlyp\nplqy\npqly\nlyqp\nyqlp\n\ndtszxekb\nsxrgbztedm\nwsdnyxobqtl\n\noqmxabwknfy\nmxyofkbqwaj\n\nxfqrjeh\nhfqrjex\nqbfxrhje\nxhjeqfr\noqhrixfjcse\n\npjaoerbtkqslx\nfxkelnithrspa\n\ntdrpcbksa\ndaksbcptr\nabskdptrc\n\nclidq\nfqkmil\nbioje\n\nwjetbyvxhoumkp\nbfyenwakpvotxm\n\nadlqkrzvg\nvlzaqgdr\nzqdvryagl\nlzarqgvd\narvlfqgdz\n\nxfsb\nxbfs\nxgbzsjf\n\npbtrqilmkfwv\nmkwflp\npkwflm\nwfklsmup\nwpkflm\n\nqkjhnsczmueftdpbrgvixw\nkdejtqhfswnbmupcgxvzri\nghxwkqupdtbvscrmnzfeij\nfbmrgtkxjqevhywsdzcnupi\nenskiwxgdtpbquchvrjfzm\n\nu\nu\nu\nu\n\nfitcw\ndfvzabyu\n\nsaewhufodzlxbjcrqivygmpk\njqxiopdmyluwkctsfzegbavr\n\nzsdfpajrnvqk\nbkaxzpsrndvqj\ndanpsvrqjykz\nujspgrndaovkzq\ntljnarpzvekwqsdmhi\n\nwyuemtvkdixl\ndirevlmytpnowauk\neigvlkwumyctd\n\njcrpnzalft\njacrutndpeixz\njczvantrepk\nergcajnptzv\n\niagonws\nawings\nwgiasn\nanwsig\nsngaiw\n\nqogitsbfzmxk\nkefoxbiqtzs\ntisfzvxobqk\ncastjxbqpifozk\nokbfsnidqtzxe\n\ngocwh\notmhge\nogsh\nogh\nsgho\n\ne\ny\nixmds\n\nrtkcgwfx\ntjxzckfhgbiw\naenyvmcfg\n\nngspdqwv\nhziq\nqx\nyqtf\nqjz\n\nexgirhuwyjt\nrxtuepwhgjisy\nxtwyghrjeui\ngetuyijwcxrh\nughtwyxiejr\n\nvdh\ndevh\n\ny\ny\n\nm\nm\nmpv\n\nptvlsonebiqjwraxckumhzgf\nlqtvbihsowmgeunckzjfapxr\nxglbifczovtwrpqnmajeuksh\neoivpuqfcbhtnagjwkrzsxml\nnubpeofjwmqvxgircahksztl\n\nuyltcxrzjegmfwbkio\ntgpxmuvsobicfej\nesvgjxcfmuotib\nxifognqmbtdujeca\nbxhfmiejcgout\n\ndjaz\ndajn\nedja\ntawdh\n\nhyp\nyhvp\nydph\n\nz\nz\nz\nz\n\njeixykzcrfd\nzexmdjk\nxkzdje\n\nauih\nuial\nuia\n\nucwkeiadhgsr\nihswgkrudea\ngwezhskryadnuoi\n\nlvhmuranicfqpbt\nbqhrlotfnzgacmu\nfhdcslqwymbrutan\n\nu\nbj\npkb\npj\ni\n\nyg\nmlyxo\n\nq\np\nu\n\nyibeaxjpkcwsfm\nixpekjcaswmyrfbl\necmbikjptafwyzsxq\nsxfacbmiekjwyp\n\nvyzhmwginr\nscjl\n\ns\nc\nh\ns\n\nqxhngcyijptvazme\ngdiwlszqfbtvcpxnejhy\neghzaniykvpqotmjuxc\n\nyasrbcwju\nrzbacjug\nabuzkxjcr\n\nyjdgutwfreslm\njlutewfdgsmry\ntglyjcefrswmud\n\ncf\nm\nmn\n\nzbwlnqrc\nzwrpc"))