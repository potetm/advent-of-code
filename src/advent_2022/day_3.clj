(ns advent-2022.day-3
  (:require
    [advent.util :as util]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn parse [s]
  (str/split-lines s))


(defn split-compartments [^String rs]
  (let [mid (/ (count rs)
               2)]
    [(subs rs 0 mid)
     (subs rs mid)]))


(def priorities
  (into {}
        (map (fn [c p]
               [(char c) p])
             (concat (range 97 123)
                     (range 65 91))
             (next (range)))))


(defn part-1 [in]
  (util/sum (comp (map split-compartments)
                  (mapcat (fn [[l r]]
                            (set/intersection (set l)
                                              (set r))))
                  (map priorities))
            in))


(defn part-2 [in]
  (util/sum (comp (partition-all 3)
                  (mapcat (fn [[a b c]]
                            (set/intersection (set a)
                                              (set b)
                                              (set c))))
                  (map priorities))
            in))

(comment
  (def t "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
  (def in "GwrhJPDJCZFRcwfZWV\nLjnQlqNpjjmpmQlLlqNfZRvQcTWcTSTTZcSQcZ\nnNqjdspspngnmjmslqmjjjCDGrHPHMGddGCMCGPPPJWC\nGwmVZmPWWFFmBbVbZVwmbPsTCnlgQgnQfhlffffZnlQh\nDqVDSqqSMzLLDDNSHHLdqSdSllCQjsTlClhlflnTlhjgfgfM\nVHJztNLHGtcbvvPG\nbjrPrNCtNrjdcCPpptfpTVspDtfTtB\nJGQJMJQMmmmZMnnLpLBTpHCD\nWJJqWRgWlCJZhZRCQZwdPScdrPNbvzPzwvqz\nQNSQNBWQNLjZBNNhLhSNRsTcsrTTVzcwZZZsfrrbwb\ntCFtHpppppMldpvpqnMFmMVGrbPcrwbzswrzcccfvTfw\npdmCpgqCdmHHdJVWgSRNJDRVVj\nsNrFnvNSzrjQtQjQTj\nlcPmcJDLdPDbJPVLljdGGBBThBQTGwTtBw\nPDLqmJmpJQfFqfqsCM\nBnhctqdnqnRcBnslCJJCMrJrsG\nwNDMZpbQwMpCvCGVjlss\nWfzNwZFbwZzZmFZbLzNwzzzzcdqgRMTTPdHPTTPMRdcWgRPt\ngrsrVSFSSdFSDFVFjZZWwpWpZWZplgZZ\nmcBPPPBLBfNdLlvvWljWJC\ndMcmcRdbRzdVhFthSsTShM\nbzvJZMTzTZSHLCCdDzmDcc\nhqBqWPFssvshWvvssNqtsHftmfpHfdcdDGHmcpfctL\nWvBQgNNNhghTJbJQlJTZlT\nchcdwNwdbCbQctCjnnQGHsQspMHMjG\nnSSSJqJZzJgWWRfZDJSnqvTTsVvvHVPpHVfpjHMTjP\nBZRDRmmrDWSrZWWzWSRNhdnCFwChclFtwbNdtr\nlNgmssCtqLwqCCtfsCLHPFhhhmMhVzBDbVzMDMVz\nZnRlQTlJzFQFQFVV\nnpZJvRRGZSnWvSvrSLglsClfpfcLgNgpHf\ntVtqcVqFVtZhcfFtqlgSpmpDSDNPzSzZmNpw\nLRGTHqbrHdnGHrTCSSwNDzMDwPMzNwbp\nTqWGJrGHCHnTWnhsWcFthFjtfQch\nqNnTbwtctvffFcqfrHjMrFjVHRjSjZDZ\ndLLzWWPmCmCzGdsLgBLGGBDRMVMHRlrrrZDDZsNMrNNS\nPJQWggCzWNWJzGWfchvfTbJvfnnwtf\nghzdgzzdQsdqzzhMNqQzvhgQnRRBWTjWWGTRGWwGTZhwGnBT\nfsrfJHbFfDFLVLVFHrWCWrBRZZTGCCjwWZ\nHLLllcDPbLPQdPspMNgvMt\nfNDJqdPNbtHpCbwpCCCp\nRTMRLrzGrMRMRPWnnvSmgCHFCCFmmT\nWQsWQjzGWMsGQzWclQtVBJfBftNdtqVPfP\ngbTCVVmDVFdsgmgrrcfwlwfTfPlcRR\nqhQZqQvnQhLQhJnvfPcSwSwlfjGcqjqj\ntLNZLZZJJZthpzhMZDCdFmFsmWWmtDDgsw\nbqCvLvLppzPzPPvPbFztFtttBNGdGsRggSgGSHDdggHSzNgH\nrMQpWfMfrcTjWJhwWHHsSBsRBdSTTNBgSR\nVwfmWjwMWwccrWcWpQQFnFtlCqmltFnFLbbZmn\ncWqsMWJMzqJJMHsJcqsJqTqjSbLBdfdSbtzLbbLfbSfShfhd\ngplGvQmRrCrgZSZtSGZZjhbj\nCQmmmmNQRPvjgRClCvmmcVHPqMFMFsWJVqFFcnTJ\nQHHqvGwjjWNqvGTQGvTFcGwJRJbszcPtDbJVbtPzVbDptp\nMLdrgmSgZZdhdfbLVRpszlRDstRL\ngdSgMCSfdMnrghCWGRQvHwvNHjnjvv\nRDBZwvZBrMlsvnlb\nWdFQqdjWWcHHPrwSPnnSWnSS\nmLdqgqHmcjHHjqLHjLppmhfBfgtDtBJZJfVtBZwGZB\nCCWRJQnZlHtHtNZRFDcBhrcvhDrJVVDv\ndPPSqLzfsqGLSTzfLzLGdLMVVgvBcmgMVwmmDFrVgmBBBr\nSFjdTGzqpjdRbNRNnjtnQR\nhjNcwBDDwDFcjdFfjtFhtcRsGGgTsGRRRTsGGqZGRq\ngbmrLnbzLmvQJnQVVpqZTqzWSCRpqRTsSR\nMQMvVMbPQQHrQMnMPldtwNNfgHtlwBhdwj\nzwzwpzMfzrBMWfCCZrwzrMJDGGGnNmGNZvgNZsDDsGsG\nFbFqSbcSbSHqTjmgGFnJglllsDJm\nTbhVdVjqdtqTjVHqjPdthPBBWpCnRfwRPRCfBCCnWR\nhlpmbfJJpCSChmJMmrSjTjcSdjTtQQTtTtjF\ngqrgsqLzgnBgZGzHBnnsQNNQtjjcNNjjtNFQNcNH\nLVRzgGGzzzPCVrJMbPJb\nVHrmqFnVdvlzzNrr\nPMtwBJPBcPwfbwBJndplLvLdLlgMMzLL\nbBZnTwbtnScfQJPJwPTjqGZFsVFjDHHGhhHhVj\ncftqScHJrfVfrrRZ\nDTTsDvvlBbTGrWBwwsWDBbWdVpZjjZjpVPPGhRRVjVZNRPNN\nlsWdWDbrTLBsbdrmdwbMJtmHMQJccFHFnJFqFt\nSWNPTPVSWChCSmQQhpppJdFJLpDpgLJmLd\nNGGtNtGfHtDpdJdqLB\nNcsNGNjHZsZGnzZfnGhQnhPClrVlQPhTVVhl\nQDdgMBsNhhMgcWbZdzmWLzFzWH\nfRqRJJqGCvrJGjCRRrSJlfPtHzzPmfFbtPtLZZLnmt\nVjvwwjlwVGGqJSSqJFccshpgNhQNQTsVgBgT\nwvDLDwCbFgSTfTSJJgfB\nqsRhmhqchmVhPdfTHJSzpCtJpfPf\nhmdhrWrddmhlqCRcwQjDLMQnMFDZnlLl\ntrMWtlwwMplMZMCZWltDpzBLBnflVLBbHzbBSGlVlL\nghhqJTfmjQjfqqznznnHnBRzBLmn\nsQhPQsjjQcQcTsPqZWwwZcFfWrWcrZww\nMRVpVCZZTHWVMCHvgNvVvbQSqgQSlg\nNFmnrNDDfnjFnndfssmcStvjvQQlvzvllqvwQllj\nGGPNmBrFNdcfcGrsGcdmDFhJHMMhHLZJMhpLHCMMMMPJ\nDSvDGdGFlGGnDZFdVSZvfPqwnfhpnrqpPNpLPrrh\nsWcTjtHCsTmsCNfgMPjpfPhqhP\nBtHzBzChzBBvFSDJvVzFJJ\nsfsNrsFFBTfjwwtNNWHPVCVWtSCDDCDmmS\nzMdhMMZnSccMmmWVWmCPlC\ncLSScJZQbcvLhZvnzBwfTjrpNwNrBFffpb\nTBrCBgrTngVQBVbhrCtgJJrGssGsMGRGcjMcNjfN\nLZdSLvHMFdzFRWsLjcGRWWNJ\npHpzlqPqFPvdBthgMbVPDhgh\nSZlnZZvBvvMrcBnllBMZSvhGMtQwFMGztthfwQtMwwPf\nHLqsDgNsDLDDDjggHDHszthzFbQGTghPGQPbTfFT\ndmLqDqCmFNjJsjHdssFNHDVWZccnRllnVZvRSBZrZlCc\nSccnnSGGftShfHSHHhnvbMjvVlCjzbVzzbMMTbCB\ngRpppNNQLWqZgPZwNWwwBMBbDlZCTzVTjHMMbBjV\ndqNQPQRqrqpPcGtchhdfhHSF\nmfDzgnNMMszBtJCpHlrjnFppCdHj\nLLRThGGZcbClBQpdWFGl\nbSqVTbBbMVMsNmNM\nBTTbbLVpfchmjbsj\nJSQJHDMHqdNZTZlhFFhCFFrNhNcsrr\ntMwJQlwMMlQwDDJtWGLGPpWLLGnTPn\nLcVQQCPPLqTzqQTcllTzhnHHfFJRcGHcFfwRGHwJjJ\nstdWDDBtVgbpWgZbsNgDNdWFGMnnwHfjHFpfwwMGMMGRjJ\nZWSDtgNdWNBdgsdsNDDsdbDlTzCVSTCqQmSqTQSvhqLVQq\ndZbgdZbNtmqttFJtHHzcczMcFszHnsvH\nwwpQplQQwqVVjqwPjCGCSMCMcHSHvvzHMzvcsrMc\npfjlQRpPRRLQWtmLNdWdmqqJ\nCPTPPmbjmVjVGCvzbjjPrGsnnMpttdtGdncdMccDRd\nlhlHzQSHwzhJLwgWgpMDMMsDdcDQMDMMns\nBHZghLWwSFBJJBFvzmbfjNZvZmCvmb\nPBGcvvcRwpwNcZcNPpPNcTHGdMtrCWrCCtCLWMtWgbVdMV\nfmsJjnqmmfsjQJnjFzSFSqsqgWrtMttZgMWVMbbVMdbSrLtr\nqQjjZFmfjZhZmwcvPhNpTNBTwN\nHHlVVmmsbbqMsJmVzGSBMSrQQrRrGvvnDn\nPZcphZPPZPhjcpdWgPZhRPfcDSrtDBSGNvtggrQtnvQNGNDn\ndcWwFjpcPhRcCpjwdCPLzHblJbLbzmsmbTwzqH\nhRfzTTfRrTGzhGWTrRrbfcQZQSttWtwddJtvdJJvWSHq\nnpjnDjFlpDnFFNMjljCnFMQtHHtqNHNQJwwZZqstNwJJ\nDCjpLjjpVLDMDpVLDLQbbhzBhVrcVgVGQQcz\nLncLBLjCSNrNrNpCLQBBBGwqQwzlzmggvqRqgllmzwtv\nfMZPHhhHfthMdbRgHJzmVqlvwlwg\nhfsPbZFPPDsfGLcBtSFNBSjL\nMlZmszBMJBHrMBMbShwSFpbZSZfwwb\nTCLCcPNGTgTPNGWtCtcWtPcSsRfRjRwjFbfpNFDjwsFspw\nnVtqqsWsdHzJHqmM\nRCrhSmWrmrvmrvhMvRNrRCzCJcQQbPtsMZVGJJtsZssPcQcZ\njLFBGqLFpqBLgZVbPbsLJQcbsV\nHjDljGFwrRHRRTrS\nGZZhnrwZBwNjRPRCbCbn\nfJtJJpsVfpgNTbVNFTRP\nJJcpLJfLdcWLdplwRdQMBvSqwRhvrG\nwmZDPlRlCDwglgsHtsBvdBHLFLSddr\nVbVMnMftfVjQWFFHdMBdBFMFHr\nzfjtnGqqnjGqfjPcDPlZPlRDzccw\nBRjhfhvRgnTMlFDDJfZzZFFQDZ\nqLdqcNttwwcwwSPSpqLNmrwmrZsGzzDFZGZFzVssrzJGnsQG\nwSNdHScScdmwHSpdNcmmtLMvChRHbvBMTBnCBBvhvlCh\nJgWTPfFPgCPPlCntQSGghHvQnSdQ\nBzvMZvLVQpdQpSZh\nRwVVjRDVcRDNDTlJPqTv\nSGHSrBBRPhPPHQcTccQTRRQjTN\nvvWvspCbzWVWVrWdjj\nwZpDzCDgDbCZJZzJGlrlqPqnqPllmH\nFCncCrDWMLCbjMCcFpLdzZfmZzwwWzdzNRZdWB\nsqsgTqHSqllNldMwlZzJ\nMtHPTgQhvhhqcrDrrDpjLCQc\npPPvmPWSClqqPvqCmSwqmgGBWDjhGLHfjhDLJGjBBhNj\nzrbdcdMndcRdTrsMcbTRdzRFVHjLjDjNLNHsfDhNGjhJNhDj\nRdFFcnTdZcTrRRdFFbZtwQCPQglvPlwJwQPZSqqP\nwlmbvwmvQvWQsvmbsSsQbswlRCNPfCTcTRVCffPtTSCPNRVP\nFhJJJFgFqJGBtDpJhTTcVcVhdcCdCdTV\nGDFtgLFnqqDGqGZsQvsllrjbLjbrvw\nlnFSnJvmgvLlfnJpgnsjnjgfDQWqCJqZdDtDCtCtCdDrtDDQ\nVTBBMPFcNNtMZDMW\nVTGbzGGhTbTGHwVPvvFnfpvjgHnfjppp\nJJwHqvlvDjljDwJFlZjZDwHNNsMqhNpphNpmNVzpsnsnRV\nmTLgrLLcLSTTTdmPPfrrrnssNhRNWhgngzMWzgzVnM\nSmTfdSBbBJbtjJvljl\nbPNLwTCLLQQqtJsf\nzdnnZVlWWGGRWGWdgdSStQMqJSMRptftbsMf\nFWbvgvZZZZgnTmwrrhrFPCrP\nHcGzzszFGllHWHbZspHbHGsHTwwrTrLLCNjSZwNjNjjCCNLj\nPBJMJQJDDDnDggRhMdRSLmjTmTwwVjVQSvvwvC\nRqfdhgDPDJDqJJnBdfzWWHcstslcbtStfHzl\nzvRRlCqrdNdZcZpjBpVwjsmjsm\nfgbTDqbhGfDnLDnLLqLhFmsHpTPHjHppppBwpwws\nnhnnnDDngDtDbfSbDnGhhgRlNvQdQqNvQvtcQQNJRNJN\ncZbCcbbScCbcmPGjPfSBQQSq\nlnMnnVsMVvmzzGMDzPDf\nLhrTsTTglrnsrrWWVvlwTnNtcpZRCmhtbCZFdttZbRCp\nNWrFPZVWNVrvvrhtnNdddtpldmjm\nDcBQBDsJbCwQnbtdzmjjjljbpjbz\nqCDcGsDJGCcBDBcswJnBJQDfWfqgvZSvgZPfrVSWvPvZZZ\nvcsdHdGtHtMHMFtVsddsWCcbppZwjScLpWhbjRWR\nNTwrnzJrgTPrDwnlphRpjSpWbJJLLZWj\nTlDPfPnzzlzTBzzvQFFBHMtVtqBqqw\nNHnqqfZvZBNHHvgfrSlJrJCSllJRVrCn\nTDTdhLMWjFcddMJPSSPJRmlCPz\nbljWFdLLTDLtdFtLlwZvqfbgwwHfwqHNvw\nBRRjhRQndRNVqBjRVhFLccjpwMmLmjHmgFHH\nfZJfJvzPPWtWWlltZzZPpcgFMsFFwwFdpHdgwtdw\nPCrdrzzfWCPdvSlqTqNSDnnQVVQQGT\nDjbfBMDSfBljBsLSjSZbzrGtPtMCPtVPvvqrzqzG\nmWdJWcppcNTdpppjzjRRVrPRpq\nQncmnHwmdTmwQcmjNTfgfhlBShshhsffnfbB\nWGDsMJsrjHCWtDMGDDVQqSvZqfSJzSnvnvvv\nLgLFLFBFLVVzfBzMqZ\nlgmFcwLhNcwdwwMLwhmcRDjNpCWRsWRspGGssHCp\nPnPzNccnjFfvCvhbSBVcWqdhSVhV\npsGMDQJDDDJgQNDHHJbwqwBsVqqZVWBBhBdd\nDlDJDQGptpgpGDfTRnrTrFPnNTlf\nMSSSMLLmFHcDScSq\nppZnCsbjPZpnnJcbRDmzHJqRRD\npmNmnGnQNnClZGMVMdBGrMgVWg\nlsTTGcQzBcljCcQzGcGjGptttpmvSJtmggtwwswwtS\nqZRnrhMbRVdhZRhhdnnVRPbmwSNwNNHtmJBvwpvtwNSvSb\nVnMrqrrdqhZrnrBLLlzzlQjQjLfTcGfFDF\ndJJTlHvhZqZlQTJnSgQDzgsSbScsSBzc\nRRNtGjCCpRPPpRtjfrttRzmbscLsLZLgcsbmLzSGLB\nwfNttfNrtWwPNNFfRtpfrdJMTTTZTMZTTVTlVwTlvM\nPQTGLmdNTgPmGgNNdCPLQlrMqBrDzMCMFqDqFqjVCBCD\nhhRwwvpSFmzDrmFh\nvwwZfSfsmvtSspnZLLLdLGWPTGTQtTWG\npMcWzWFvWhFpPMWzvvhpdprHTZTQrHrQdZTJdfTgQTnJ\nCGbjBbNjjDmRHJDgrTVVZg\nNNttGlGqNLsbtlhMFMFcMLwMvvZz\nCGSCBNCQBtBCQttBwCGtGtQrqrLrJqZHLHbqHvLDHLrq\nnVVhPMfVdfVPbfqLLqgDDqPvgZsv\ncpVncbfnhFcBltTplpmTBC\nMrdcdStbMnddtRBdqMnFmbqGCwqCVHVsNHwPfGVPqsCsCs\nDBLllzWWQQzlZVVVCsGWHfsH\nJQphjTgBjlLgjjpTpLgvTjQnnnSJJRRFmdbRRSdMRtmdMc\nQbRZMSWMblwLsgpwZzqZ\nBFncBrfcdNrrnVrNjsFzFTJpJLGJsGqLTp\nVjhDDBdrfdhQMllzHmPQMh\nLdVVjFVFbpVGRQGllG\ncNMcJNHzJWJtCWHNJHcHczWpGmmhMQmBBqrlRhBmpGpGBQ\nJZzTTtCZtHCJnNnNwPfbFpnfdDdLdnvP\nTpMlrWTTddjmlmDmgQgRtw\nMNNVMSsVSNSnNVMFLDqwtGgRRtGbgFRwtR\nCCLSCPSCZZHVCfZscBJJhPphpdpprdhjJM\ngSMSHJHsMMpzRgHzsRMPPSzsPhtZtZdqdDqQDhdCdZmQldht\nFCcCnrGcNTfvvtqqfvlflQ\nTrTrWNWwrTJLMzJCzWLL\nTpTzwMrfbrpFpMbFrrrzbPSdZmtSZRTlTZRlmdCVlCtJ\nvqvWgqDJQJsQCVtZgdZdRRGd\nvsvLJLchWBcqnvczwjLfzPjfrjzPrz\nzqzbqCFZgmzzmNmf\nvpRWSbRVbVWddVpwvwdRSwnSNgLHsnfNgMmgMLMmnrns\nDwWVpJRlpdbpRDWdGJGcGlhFtPPCqCCBFqZPQttlqFBq\nwQRlwtBJBDwttJdGvLfBvHLLfTLz\nMMmNZcMrcMFnRHzfjjvvHfvc\nFggpbFnhrNNrrMrMbMbnhQVJVhstJwqWCVCRsQJQ\nDQbCGblQlpQFQlHjCbjwDQQMggNmJmgnnpRBngfZmNgJMf\nzvhWccWVdWBchdssPrrWZZZfmsmmmgsnZZJRsRTf\nzBdtqPccWPHFCqCCqljq\nttrbRMmgtHgfmHSfBpLfnBBZBppB\nCVTJDCCNPwCPDwcqzmddQZdTQdnLBQThWp\nzwFDjwDJJPzjzVNcVJwCcbRHGmbbMrFHgHvrsgbblG\ngZjjwHqHCzrMZVVR\nhhzcdTzPrVhVCGMb\nfPcmLPNffsccJDdNDjBnpwzmHqgWjHwwvg\nSJQFSvQBlzbSCgdPPddPPPSN\npcrjcWLwwcHcgPNgTPLMNTCB\npRsjsWRnrpHRmrBrHrjlbJFvvzQFnzQblQDDbJ\nVjQVMQPVMfVPPbGPHHbGJD\npcqSttltsbDGddsCJG\nTSchqLtTLFhgQbMMQMrr\ntrqzMRwNTtDzLPJQgWmjmjrf\nlbBQdpZbsmhGmZhmmG\nllVbpCplvvHBBHpnRDcDRRqnRRQnFRzT\nSLSSFFmzLShsVSSHnLnrJdbnRdZZbrRw\nqCfWBftpNWNNlqvTpwrRbGGCnwGmgRJGZn\nNcTBNpvWvBWpMftNffpqWlTpmzPDQPSzFVMsFQVhHsjHszss\nVtJtNBRBGDpdpNbC\nQgLncnttvFcwwhLvFjSGsSbmmQCSDdpCmpdG\nvLgjLhhrctMvLFFjLtMTLMgfPZqBZPZzJBBfWZZPRZZTRV\nmJzDJJpJBvfsGMQnBM\nCwPWCLRRWwRqwPqhPsrZrnrlhhQrMTrvZl\ndCdLLSPRLSqWqVSLqLjgJDzDmtbngFVtJtzz\nmtgWtMWrqjzQTTjghwwfczlNJdlcJnlc\nFvRsDPPFGRBFvvslwDnTlcTTdwndlh\nSGBZRBTsFGBRvLpvSCmgQWQjgggMrQjmmSmW\nGcsRrQhrVVjhRcWlnDFGGmvntDWZ\nTPbSgJJgBSCbCTbLHMCMTTZdFHvtZlWZDZFzmzZHZmmF\ngBCMCSpbPMMPjcjqQQpqQprv\nnZJcnZwvwzvTTTVtpDFnHH\nDQPBqGGGdMdTRHRBpNgFNR\ndCGPfhPWQdWWWCWShWPqrChWLLwLswjcvSJbvbLjJLbzJbJD\nQrBQtdtrQBrdtFHPrdQBDvGhLGnPnCWnmpDmLpmD\nNjlRJRlNzJJVbSSRVZwwJcmpWDGCWnbchnLCCmnWCG\nllSJzsZzMMlsSZjSjZwJNQqtHHdBFsqdfTHhqFftQB\nzdTJFHTdDBzrNdMnhNnNdM\nZlLZZcLtVtcWtGjtzLjLZjCrnVNrnRbrQQbQSRVrRnSNqS\nlZtGtCvjZPCGCctPpsDDBzTHFmPmFszD\nmQSMvdMQtQdZhQrPWCPqPQrN\nRwjwnZGzJFTZgzggzJDDwJnCPPhNNqPrLhrGNcWcWNPqCq\nZTzDfnwFzTngTwJvfSlMtMMlmsHmHt\nlZlmFRVZWmgQWhRsRpJsCJpJct\nPTbPTGTGwwGrbdfjNNZJvcCsCZtvpTsh\nbGdBBqGrdBPjDMzzVFZgqQzFFL\nszvsmLvppPPtzGLGWpVdTSHTNgjHQRmHTgSH\nFnBMBNZwZNcnDZMcnZlZgwgdQTTHjVJjHHVRQHJj\nDnZrFCMZMNffrLPbLsfW\nrJvmnBgnrCrGRSGNQR\nhthjNfhwctwpjTLtVLjTGSpldSCGSPdlPSRzSqSz\nTVcTfHNFcwtjMhTvgbHZsBbWmmZbnH\nWsQgstQmvQJnssWsWPzhRzhBjZBSBRZSnj\nqwCNqFwDrrlDrFPvRhTSPPzLRz\nbppqwppCddlvfbDNVgmMmtMfVVmfmVWW")

  (part-1 (parse in))
  (part-2 (parse in))

  )