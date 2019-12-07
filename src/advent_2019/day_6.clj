(ns advent-2019.day-6
  (:require [clojure.string :as str]
            [ubergraph.alg :as uga]
            [ubergraph.core :as ug]))

(defn parse [s]
  (into []
        (map (fn [l]
               (vec (str/split l #"\)"))))
        (str/split-lines s)))

(defn graph [in]
  (apply ug/graph
         in))

(defn part-1 [s]
  (let [g (graph (parse s))]
    (reduce (fn [acc n]
              (+ acc (uga/cost-of-path (uga/shortest-path g n "COM"))))
            0
            (ug/nodes g))))

(defn part-2 [s]
  (let [g (graph (parse s))]
    (- (uga/cost-of-path (uga/shortest-path g "YOU" "SAN"))
       2)))

(comment
  (part-1 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
  (part-2 "XV5)LZ5\n6JC)7ZG\nNCW)VDZ\nCPM)N4Z\n3PZ)4D6\n3TT)QPK\nG1T)FD5\nS43)3G7\n26J)ZMX\nGTS)G9S\n1GM)9ZN\nWCW)XTJ\n9B7)NTZ\nQDH)P9B\nFN2)6NB\n8XB)H8X\n638)2KM\nR42)M9X\nGX7)TGJ\nHB4)6SC\n9RP)62K\nW69)787\nT5M)H81\nM1C)KF5\nZ8D)F1Y\nF53)N8J\nBKJ)KYJ\nN9B)SHB\nQNJ)BST\nCD1)RVG\nTBD)Q68\nXL4)VSV\nY84)L1C\nQ26)HGL\nJWN)42N\nMXF)XT6\nT5L)JCX\n2ZH)PQJ\nQJR)GWG\nKDY)5Y3\nWCS)BRL\n3K7)JX3\nCHC)MCZ\nWFT)GBX\n7NN)QVT\nJJY)RRG\nC9W)CS6\nCR2)J1Y\nGQR)X9Q\nWSX)3KX\nDCV)GQR\n1Z8)MC2\nXJP)WQC\nN1C)MY4\nJ75)4CW\nY35)RR1\nK3D)XWG\nVWV)LXN\nP18)RSW\nRX1)5P9\nDMP)XNR\nFNG)H83\nDVW)B2Q\nJJ4)BL4\n428)TLP\nN74)6TQ\nRSX)45D\nNM2)1JD\nRSX)2LL\nM9X)PWW\nW2J)DQ9\nDCW)WKP\nQ3B)FMY\nRPD)Y7X\nNZ6)4PV\nNM4)SGJ\nBBD)WWV\nXN6)9QQ\nWPZ)WJ9\nST4)T72\nNDQ)XJ9\nP5G)G3N\n5VY)CNC\nGQG)SSB\nM7R)8QN\nNR8)DQJ\nKSW)Z8S\n42B)JBM\nBTV)985\nN7N)SGC\nGW9)6Q8\n73H)FSQ\n8GR)176\nRWL)XCH\nM3Z)RZ1\n1ZV)6BV\n6MW)TCW\n2HF)6JJ\n47B)8R6\n1TX)DSQ\n55J)GL3\n99Y)D27\nCD6)PCP\nVW8)JNV\nFNK)Q3B\nHWB)948\n3CT)XP3\n684)RK3\nTDG)4GH\nDZZ)NCW\nGR2)BJX\nN2M)9GR\n331)FGR\nHDQ)K74\nQC2)12V\n5TP)344\nQPN)4WD\nQJG)TWS\n15P)9NX\n4CV)WLD\nXGN)C88\n1Y6)7LB\nZ32)TNV\nG2V)QNQ\n673)YPW\n22J)628\nG46)7PK\nJN8)KZK\n4CW)PJS\nDMY)QXC\n7LB)L7S\nLQ9)X8N\nC3T)VWL\nBZ8)M2F\nXQ3)QKV\n89K)4YJ\n9BB)KV4\nX8N)1PJ\nPZT)98P\nC88)YSP\nLDV)T39\n1ZQ)SND\nTJJ)YKM\n7N4)8V8\nBBD)S66\n5MF)2X2\nGWW)LTH\nKQK)KPG\nFNV)HWD\nSMJ)HWZ\nM41)2W1\nW1G)C5D\nX79)P3C\n4KT)N5Q\n7BS)BWR\nCLX)DCW\nC1T)Q7C\n4K5)Y84\n63K)H8L\nPRZ)8JW\nLNM)8HG\nN4Z)GKB\n7DX)C1F\nBJX)LX3\nNWL)LK5\nLGW)ZXN\nXV7)W6H\nXBH)44C\n376)1LQ\nHQL)9BB\nPX2)3KS\nVHC)XP9\n4YZ)3XD\nSVN)2T8\nG8Z)ZP3\nYXP)H6D\n4C6)MLH\nBST)QV3\nRXP)CBM\nM9X)WCW\n5PJ)W3R\n98P)PXC\nMB3)TDG\nJ4L)GJM\n6BV)59Y\nCNC)7P1\nQ7V)1H5\n3WF)65H\nMK1)SM4\n6JJ)YSV\nJFL)HC5\n6G7)1LD\n4GY)G9V\nB1Q)WCV\n3QY)K7W\nTSC)KDY\nSQS)1WB\nLV2)2SC\nFNV)4QX\nM2W)47Y\nZ8S)PQ9\nVSY)S88\n6TD)CP7\nDCD)4CT\n3NQ)R39\n6D5)YZC\nQFC)DDG\nXTJ)9SX\nSYH)Y8J\nKZ9)V3D\n32H)DD7\nW9V)YP3\nHQ1)RYK\nXHR)1TD\n7VD)4S5\nH81)PQR\n838)D9H\nJGT)R7V\n2MX)FTS\n1TD)CKB\nV2Z)F53\n7XL)563\nSM4)J3M\nDMP)Z9S\nJ3G)6JC\nMWB)VPM\nNTS)V21\n2LG)97S\n9W9)CS9\nHLB)M2L\nWJ3)DTH\nG3S)5BD\n2YJ)PPH\nZXK)HXX\nVFR)G3L\n1JK)L36\nTVV)4GY\n4HY)F1F\nJBM)XZ1\nBL4)X1D\nNF8)9KV\nSXX)PYS\nHPD)HBH\nRMF)HHZ\n4HK)5G8\nXZX)1ZV\nMCB)3YW\nLP4)JMQ\nWDL)JWB\n757)B3C\nMQX)8TY\nHMN)HV1\nJ18)SKS\nLXJ)B41\nZGV)JVM\nDC1)3K5\nQMC)SVQ\nVB9)2Z4\nWHT)BKM\nCDK)MS5\nSLQ)ZRZ\n7FL)GR2\n9T2)K81\n2T8)RX1\nDGR)PMG\nDGP)R3M\nNC8)WTV\nLK5)4XF\n132)QBZ\nD79)19N\nB4D)1FM\n53B)HLN\nHS1)RCM\nLQQ)CV3\nHJW)RB8\nLC7)BFM\n28Q)QLG\nCS9)G22\nQ98)GX7\nP9B)N1C\nJNR)VHW\nNVK)W4N\nV5R)HB4\nD89)5P6\nYQ3)LB9\nRSW)Q6W\nQM5)9XS\nTQM)3TT\nBV8)WCS\n9HZ)HMP\n4LX)XN6\nFGR)7Q1\nV9F)G1Z\nGBX)718\n2HR)SLL\nPZB)LGP\nPBC)32W\n5JZ)N3Y\nTZ9)2S1\nCNF)4YS\nT21)PKL\n4H2)XGN\n6SB)2HW\nR1V)LQB\nDCT)NKZ\nXTB)98Z\nCRZ)4LX\n8W7)JQQ\nC8M)92K\n3YW)YBY\nC76)42B\nR3M)FXF\n2VF)C12\nP1T)B4H\nZP3)NM6\nKJB)JTN\nDTB)WMD\nRS8)2BX\nGCB)5HK\n4WD)F2T\nBZB)8NK\nNBK)SGD\nQV3)KXN\nWVG)4HY\nKRN)3VW\nM4Q)J75\nSTH)T8Q\nGNV)5VY\n366)ZMV\n41V)PBC\nMHQ)3N4\nKVB)312\nK7Z)YYJ\nHBF)11M\nF2Q)17H\n2YZ)QV6\nTHG)5WF\nS8B)V5H\nLY5)MQM\nTVK)FD6\n72F)QMQ\nLCT)HBZ\nLGZ)DLC\nRQ6)CPW\nV9J)Y5Y\n4LK)WN1\nF2Y)F8D\nWQR)RMZ\nZ42)JZ3\nQNQ)29B\nF1W)CFT\nT3J)13Q\n6NB)HVY\nG64)88J\nDD7)W3G\nCXY)CNF\nFD5)9C2\n8QH)FCS\n4K5)KZ9\nC12)NTS\nPJJ)298\n9G7)5BF\nHYT)GHC\nCVT)QKK\nPX2)LC7\nVN8)8DZ\nLT5)TYL\n12Z)RMY\nGRY)DRD\nG6R)RDJ\n864)ZT9\nDKD)N74\nL74)SWF\n2B6)4LK\n8V8)CLB\n45K)LL9\nYBY)JF4\nQ9Q)55M\n78F)6RM\nHYN)7TH\n7QN)HP9\nTB2)ZZM\n39D)T5W\nC5P)LGW\nDLD)RSX\n9RZ)HPC\n7TL)9T1\nY3S)CRZ\nDTH)35N\nFD7)K3D\nYNK)FNB\nG1P)758\n68M)BQ8\nYMP)VW8\nPJZ)PJQ\nJJY)Y6B\nMD4)XNT\n4CS)SS5\n2X2)B52\n2TR)XL4\nH8X)C4N\nSV5)TL5\nCGN)QM5\nMXZ)GSX\nZ8K)JT2\n738)MR5\n2ZS)4F6\nQSD)NRM\nQ48)N1T\n4SH)99Y\n3VW)M3Z\n62K)HYN\nXTQ)9VX\n1FS)KF2\n9VL)K71\nXVB)7FL\nQHN)DSL\nY6B)KRC\nSNJ)X8C\nZDL)DPL\n2TR)3FH\nCP7)TG1\n3K7)L7K\nKQK)MK1\n53H)MY9\n7FW)S5B\nQL9)LF8\n4FG)MJY\nFMY)6VK\nG3Z)HDQ\nT39)BD9\nR6B)YFS\nHCX)72F\nQKP)RF9\nN1C)462\n9RM)B1Q\nTLP)7QN\nQSW)Q72\nNH3)253\n4TG)DCT\nZ4K)GV9\n4YS)689\nKSQ)P2S\nSV7)Z81\nD2J)73K\nRSY)FG7\n5PR)66Q\nQY8)HYT\nZ52)VBB\n3P3)7CQ\nGWD)3PZ\nGH6)RRF\nYQ2)YSL\nTQM)F6N\nTWT)R8N\nYYJ)X86\nLDV)362\nTV5)S99\nJTN)VHC\n42N)BX1\nLMV)XQ3\nZ27)Y1V\nDWK)RTT\n64J)3CT\nVWV)RF3\nNLV)1L3\nM4Y)JB3\nJGL)LR3\nF4T)LK7\nCD6)CR2\n167)7LW\nZMV)4D3\nZCY)X62\nG3N)VSY\n3SC)W6C\nN3Y)4KT\nV3D)HRV\nQGH)KLP\nXV7)JZ8\nYR4)CBG\nL6B)N1V\n628)STY\nYQX)FLG\n4RR)NKX\nL7K)SVN\nVBQ)SLK\nH3Z)BGN\n5P6)NY3\nG7X)2NG\nM1C)7JS\n658)2HR\nQVN)4HK\n6D4)NGS\nXNV)GH6\nDKJ)1CT\nPCP)JSF\nDRN)YOU\n88J)QVN\nQ4W)658\nLGP)FQG\nSWF)DYZ\nDRD)PSB\nG8Z)HTG\nLVD)HWB\nD3D)63H\nS94)FKM\n26M)RQ6\nY7X)89R\nCPY)BZB\n4WQ)1ZQ\nXNR)XGT\n221)8W7\nT53)KP7\n3K3)RLH\nFR7)8ZB\nQG8)KRN\nG1Z)2ZS\nGD9)455\n9SX)3NC\nWTV)9T2\nD61)F1L\nVSJ)2SB\nM8D)7VD\nCV3)TVK\nTVX)VJV\n7P1)28Q\n69P)DLZ\nVYR)83M\nVQS)3Y8\nW34)LV2\nZ9S)6KV\nPFV)GHF\nYKB)CXY\n8NK)GL1\nNWF)KJB\nSXP)JNL\nJCY)DFT\nSKS)W7D\nP19)BJC\nMW3)WPW\nSPV)HVT\nS5T)KVN\n3HW)C92\n6HZ)W74\n1JD)K41\nCFT)WSS\n61T)27F\n4GH)Z8K\nD27)7H9\n78F)TV5\nHFX)NWP\nRHS)DQL\n819)4V3\nRGW)738\nQ4Y)T21\nRRF)248\nMK7)D9X\n55F)QFS\n8M8)NM2\nJ4X)LFH\nB1J)T6K\n4F6)PZT\nWN1)9WR\nVXM)3K7\n7RC)NW1\nTF2)9CM\n59Y)6WQ\nG7T)BF5\nZ4Q)JKJ\nZCD)LT5\n2QP)MRX\n6TM)D79\n14T)KBG\nRHS)9KD\n7JS)Z4Q\nXVN)B1D\nY7X)17K\nDRD)Q9Q\n3MH)BXK\nTCW)9TC\n9R4)NBY\n7ZG)B4M\nPPH)3LC\n9KD)VRQ\n91T)KSX\nFS2)3S9\n2WF)TWT\nPQJ)FQT\n8D8)ZGV\n1ZV)FN2\n4TB)LDQ\nJDY)6N8\n35N)756\nWPZ)V5R\nHYH)QK8\nJCS)JL9\n1Z8)MPR\nJPN)8W4\n34C)85G\nFNG)F35\nB3T)819\nPLQ)138\nVBJ)JTC\nJB3)FS1\n62X)LM8\nFZP)4SH\n1P7)LNM\nJZP)56P\n1QR)SQS\nVRQ)LMV\nWZG)7B1\nKPG)N3V\n4C6)VQS\nJQQ)MDR\nYCZ)69P\nRB8)9H5\n23R)9JW\n7XH)WRC\nJZ3)TYF\n5SG)1CH\nJNV)HCX\n312)QJG\n4T1)NS1\n64L)N1L\n6NK)LC1\n4WQ)FN6\nBKF)68D\nGB4)K95\nHX7)JPN\nW2M)6QS\nJSG)91T\nTP2)CWM\nNP1)S94\n3Y8)KWK\n2NM)ZJ5\nCBG)JC6\nKJB)ZPJ\nXPM)TF2\nBR7)L7C\nSHB)PY2\nPMG)F33\n2F1)Z36\nSS5)9K7\n73F)PL9\n6TH)9LP\n9MY)WHF\n63W)Q98\nYSP)DY7\n4WD)Z3C\nVCV)FNF\nQSW)PJZ\nPV4)9VL\nNY1)H37\nD81)NRC\nXYM)GP4\nJX4)LKF\nK81)FG6\nHMD)W34\nDYZ)8KY\nDY7)6WF\n8GC)ZDL\nWSS)X88\nDVV)R1B\nBB7)1DN\n3NC)7S4\nC12)TNS\nL2H)14T\nY3P)92P\nW58)44W\nMCB)YPZ\nXQZ)DJH\nKQZ)MN2\n9WR)KCD\n2JM)D5L\n5Z7)9B7\nQKY)MV1\nK41)QKY\nVTF)BQ9\nNDZ)QNJ\nBGN)PJJ\nP77)6YV\nHT2)YWK\nPPJ)CQX\nH83)QSD\nVSV)Z8D\nLXN)QG8\nR87)3DM\nW6H)ZGN\nMKN)GD9\n3J7)1V9\n7FW)GQZ\n63H)WV5\n1PJ)5HM\nS88)SQ6\nXCH)PXL\nMSH)ZMH\nRR1)XJ8\nNTH)67R\nBWD)F1S\nJF8)9G7\nDYL)TNX\nW34)NVK\n8K3)53B\n75S)DVW\n4QX)3DS\nRSW)HQ1\n3HS)F2Q\nB4H)DX6\n6JX)HQL\nSV5)ZFX\nKJ1)WJ6\n89R)84D\nFJB)QYY\nK5S)55J\nF3G)WY8\n1CP)JTF\nWT9)LF9\n9TC)N8L\nFBP)R5D\n57B)D61\nXSG)3WX\n675)WHT\nB33)VYR\nK2H)3PN\n8ZB)SLB\nC1F)LLG\n3PN)5FH\nQKP)HL9\nG9S)3QY\nZQF)F4T\nFXM)517\nL74)2HK\nV31)QJ3\nDDG)PY6\n3YW)6XG\nCXD)TBD\nK8W)J6H\nBX1)VS9\nLVJ)331\nB3J)4TY\n3S9)FRD\nCZ9)QD7\n718)SMJ\n53G)S8B\nLR3)W2M\nFRG)32P\nVBB)23R\nJM7)JY4\n3GM)HLB\n8HG)BCY\nCLV)WKS\nCFB)MW3\n8KY)FQM\n3N4)SD9\nC3T)DZZ\n75P)Z8Q\nMP2)VTS\nCW7)PZD\nGSV)TJJ\n5G8)WPZ\n45D)RGW\nVXC)2YG\nGLD)Y35\nL7C)P18\nPSB)PFS\nQPM)6R2\n1NB)BKF\n787)754\nKRC)B4D\nN5Q)DGP\n718)B5P\n44C)ST4\nXVY)45K\nWPB)VSB\nPKL)1WV\n7DG)37F\nQF5)QRB\nQC2)MK7\nKPZ)3SC\n8M2)LXX\nVPZ)45M\nV5M)34C\nJMQ)ZB6\nB4M)G18\nD6Y)DYT\nLFD)B9S\nR84)72H\nXP4)L74\nH37)BDG\nZTS)NBK\nQKC)G3K\n7W1)FV4\n16P)M37\nZSC)XJL\nB1D)VBJ\nRW8)5X5\n4D2)54L\n212)WZG\nNY3)TB2\nLR3)6D4\nKYJ)Z52\n9CM)98M\nRDB)LHB\nW48)V9J\n69X)ZFL\n45M)NV5\n54Q)QK1\nP18)B38\nQPD)1WG\n4DX)9HZ\nJC6)598\nLHT)8XB\nWKY)VMF\nRDV)PMX\nXLJ)LRG\nMPR)N15\nC9Z)FKC\nLZ5)C2K\nMW5)TFF\nFV4)9BW\n2RJ)K8W\n9GR)18Y\n1LD)JGG\n82Z)HKD\n7JS)4HT\n7NC)CXB\n2VZ)YL1\nPQR)JWN\nHZP)SV5\n212)42H\nQGD)J6P\nD5L)6NK\nXP7)6TM\n18Y)D48\n4TX)4NG\nNZJ)ZSC\nWGD)QWY\nNX4)66X\n2MG)BCF\nPSV)FVJ\nGD7)QC2\nDLZ)542\n2W1)DRN\n1WB)P6H\nKTF)H5W\nY8J)9ZH\nJWB)PYW\n1DN)ZG6\n4MX)TSC\n5BF)PYB\nSLL)KSW\nRGR)4CS\n9T1)T26\n7H9)3SR\nBRP)TD9\nB73)C5P\nBXK)VN8\n4KT)6X5\nB65)BTV\nN88)2V5\nP2F)6SB\nPYW)QTV\nMJY)684\nJSF)P77\nLP4)P4J\nLKF)RNH\nSSB)3VB\nD75)F2D\nSD9)HDZ\nS46)1L7\nQYY)638\nXJJ)HJT\nR5D)Y72\nTXH)VN5\nWLD)HT2\nDLC)36P\nWS3)XLF\nHKD)V9F\nCKB)2YZ\n57X)RX9\nYKM)DTB\n4PR)G27\nT5W)55F\nPZD)JGT\n4CT)8FR\nHL9)W5B\nHWD)MW5\nXT6)2ZH\nBQH)9C8\nMY9)HV8\nX5S)RDN\nVSB)16C\nX86)3HW\n84D)26M\nMS5)M1C\n6X5)NWF\n9XS)L8X\n8R5)DXZ\n3KX)6B7\nS99)VWV\n5PV)D89\nFTS)8GR\nSTY)9QL\nZ4L)6KM\nZN3)Z27\n9G5)9F4\n7YP)167\nKF2)PX2\n72L)M4Y\nBSL)424\nQ7C)M4Q\n9GP)FJB\nSG6)NP1\nSVQ)QKC\nJ3D)X5P\nKFR)RR8\n216)HMD\nKXN)93W\nY8V)QGD\nY8V)PV4\n248)LVD\nWFP)G1T\nNKZ)SNJ\nCX5)PSV\nQWD)THG\nJ6P)C1T\nHV8)PMQ\nSGJ)MD4\nK3D)4Q2\n6R2)Z32\nKH9)72L\n9QQ)JR3\nB9P)4T7\nRNH)154\n6HQ)BRP\nHP9)WPB\nHGG)7DG\n27F)VSJ\nLW1)NWL\nGD7)LW1\nLWN)6TH\n9L1)TLR\nV7Q)3G5\n5P9)J3D\nB3N)XQT\nS59)5QP\nJR3)BW2\n2KM)R2V\n6KM)FNG\n6Q8)H2B\nCHD)RRQ\n4S5)7FY\n289)MWB\n73H)Y3P\nHYW)TXH\nX6L)XZX\n6YV)2YJ\nZFL)4T6\nMM1)JX4\n63K)6D5\nP8G)Z9J\nNBY)Z4L\nDCT)LPH\nSTQ)ZPT\nMCZ)VPZ\nKSX)YNK\nT6K)26J\nWWV)216\nLJW)Y3S\nW7D)VWC\n4SH)8MM\nNBY)6TD\n5HM)KQ9\nXP9)MQX\nYSL)3Y4\nNL2)DYL\n19L)8K3\nRZ1)9VC\nQVX)WVR\n9F4)Q3F\n2K5)N4X\nRF3)NHN\nWRC)G1G\nP8R)7TL\nXJ8)K6F\n5CZ)281\nB2Q)DG5\n5YC)XK3\n92K)DWK\n9ZH)YTR\nVWC)FQP\nBFM)B65\nZ9F)PDP\n8FR)6G7\nRDN)CHD\nXP3)RGR\nWVR)MMB\n7LB)5MB\n789)XCW\nV3L)19X\n689)2NM\n7DR)GLL\nRDV)Z4K\nHBH)F2Y\nHW4)B4W\nZ81)P8G\nXZ1)VYN\nT26)KDJ\nRCM)P2F\nF1Y)TCT\nT8D)V2B\nHV1)QPD\nKL3)CRS\nWY8)R42\n9VX)675\n6KN)LQQ\nLHF)NY1\n98Z)2VN\n3KG)JZP\nHHZ)2T6\nQ44)5JZ\n4HT)QF5\n9C2)FH4\nKFR)W9V\nL7G)4Y2\nGHC)L2H\n26J)YR4\nD9Z)XP4\nCRS)8MG\nJ9B)P91\nJ1C)8KQ\nCCR)M6M\nB52)PQC\nHLN)RSY\nQHM)G64\nBWR)47B\nG1G)8GC\nX6L)3TF\nFSG)7NN\nLF8)7FW\nWMD)WQQ\n45K)2MG\nMMB)JJ4\n3DS)XVN\nM2L)2X4\nZYF)4JR\nMV2)Y4C\nPDP)JGN\nGWG)M2W\nXQT)DC1\n6QS)C28\nKRT)DMY\nNS1)L5X\nFY4)SK4\nZPJ)HWH\nR39)PPQ\nJ67)KNT\nDZ6)W48\n626)DGK\n4T7)VLZ\nHTJ)GLD\nPXL)PRZ\nDCW)WHP\nTDG)RDV\nGF6)F7V\n49C)DZ6\n3YR)5PR\nM8N)64L\nWHK)62X\n1WV)788\nZPT)4MX\nNV5)58L\n9WY)8K9\nZT9)D6Y\nMKN)S43\nQ65)Z73\nQPL)XVY\nX7B)NDQ\n153)BDX\nR2V)395\nYWK)N34\nZMX)GX4\n3DJ)X11\nRHM)2VZ\nP3N)366\nZRZ)6JX\n559)662\nWZ4)C8M\nKC4)VQC\nFS1)B3J\n72H)YWP\nH8L)TP2\nQK9)XXQ\nPL6)6PM\n32W)P8R\nYSV)4TX\nPY2)VD5\nLHB)4PR\nR1B)J4L\n36B)9NC\nFXF)4K5\nQPK)C3T\nVYN)VSW\n4NG)J65\nBZ8)LWN\nP5G)HJW\n2Z7)BX4\nBSK)16V\nS7S)9YQ\nTNS)5SG\nB3C)QK9\nKQ9)9L1\nVLZ)7Q7\n4PP)F1W\n2NG)F8R\nJH3)63W\n87R)KMF\nYPW)YGC\nF1L)BBD\nQLG)NH3\nS1H)K7Z\nF2D)DN4\nWJ9)BMS\nP7C)5YC\n428)HBF\n2HK)75P\n7S4)RT8\nD48)PL4\nB73)B5R\n9L7)1JK\n3G5)P7C\nB8P)BB7\n6G7)K2H\n598)V3L\nQBZ)BR7\nGQ5)HYW\nRLH)DCD\nBD9)CMV\nN34)J18\n3V7)KFR\n6N8)4J3\nJ6H)QHN\nJGG)F6G\nWJ6)X5S\nKSJ)D81\n662)VCV\n4XT)B1J\nJWB)SXX\nXN6)XLJ\nLFH)TFP\nRT8)Y5D\nQKY)LY5\nPYB)FD7\nF1F)WDL\nCWM)RL5\nRR8)C8B\n18B)PWN\nF35)3HS\nB9S)7D4\n5BD)X79\nTGJ)9HF\nNNJ)235\nDTH)NDS\nGLL)CPY\n138)RXP\nX9Q)LHF\nWNV)DF5\n19N)46N\nQKV)BYV\n7TH)JNR\nGL3)288\n66Q)YYR\nSK4)4VH\nQNJ)XQZ\n7CC)278\nBDG)T8D\nWBM)YMW\nNCW)RHM\n2V7)YGW\nPYB)XSG\nRX9)YMP\nBCY)J1N\nJZ8)5CZ\n362)QVX\n517)QPM\nK74)3WF\nG9V)MHQ\nZ8Q)CJ2\n69M)54X\nSTB)CHC\nVSW)GWD\nCMV)XTB\n17H)XNF\nCXG)HNZ\nHNR)LVJ\nS5B)QJR\nZ73)6MM\nWQT)7QR\nP6H)VHY\nBZ4)B33\nNM6)DKD\n9KV)419\n235)G2V\nK71)6MW\nT8Q)Y62\n68B)Q3S\n8MG)68B\nFSQ)3GM\n4XF)WJD\nFG6)36B\nDYZ)XDF\nBX4)XV7\n77F)WQR\nJTC)HTJ\nYZC)MVQ\nLM8)2B6\nZB6)SXP\nPXC)5TP\nF6G)5FM\nG18)4WQ\nR4M)HFX\nHMP)2QP\nQ1L)2RJ\nQRB)9G5\nYJZ)STB\n3FH)3K3\n356)DKJ\n5GR)27Y\nBNQ)NC3\n36P)G7T\nYN2)HYX\nP76)39D\nPJS)YQX\nC76)C1P\n27Y)65Z\nHVY)37H\n462)16P\nN8L)Q44\nFQM)BWD\n54L)DQH\n455)P1T\n5GF)6JL\nJWR)WFT\nFG7)H2M\nDBT)J9B\nQ72)6KN\n756)BNQ\nMQG)HNR\nHDV)VGH\nFKM)MXF\n6PM)V7Q\nHPC)QWD\nG5M)Z1T\nFRD)8ZS\nZPH)7TK\nKV4)NLS\n7QR)7NF\nRL5)JH3\nCPS)1HR\nBDX)B8P\nZB6)XVB\nCS6)YXP\nW6C)C42\nSTY)ZXK\nWXD)2TR\nKNT)J76\nXQ8)GQG\n1HR)D75\nMLH)56K\nPFS)X2B\nFVB)W7K\n6YQ)82Z\nRK3)XFT\nNRM)SG6\n9QL)VTF\n9TC)61T\n6RM)5GG\n8MM)H3Z\nSGC)KZ3\n7Q1)Z71\nHGL)FXD\nB69)4XT\nN1L)YQ2\nQV6)YCH\nK3N)KNV\n68D)BSL\nY4C)HPL\nVDZ)WXD\nC5B)6SR\nPMQ)NVZ\nM35)NSK\nB4W)QCQ\n6FC)1GM\n7TS)N9B\nFN6)BKJ\n9H5)QL9\nKDJ)DCV\nLPH)W1G\nBQ9)SD1\n4Q2)KSQ\n6WF)4YZ\nM1W)5GF\nHYT)WVG\nHDZ)VBQ\nDZ6)VSS\nYTR)428\nT72)R8V\nF1S)C8R\n1FD)4ZK\n41J)626\n658)6CN\nHZ6)FNK\nFKM)1Z8\n6KV)NZJ\nF33)DJ1\nXTB)TSF\nNXV)RYS\nF1F)5MF\nYGW)2Z7\nMC2)CCR\n3YR)GSV\nXQK)XP7\nM97)PL6\n1CP)9ZP\nN8J)C9W\nGQZ)L6B\nHV1)SYH\n9VC)VZP\nQD7)5GC\nX2B)VT1\n92P)T5L\nXCW)WKY\nMY4)3J7\nMGN)3VL\nK7W)8R5\nHWH)BT3\n4PV)9R4\nFVJ)5BJ\n5MB)18B\nF8R)7RC\nMGN)N88\n1CH)ZPC\nX88)YN2\nY8J)7C2\n1H5)R53\nBMJ)SV7\nLLG)G7X\n9LP)M97\nZDL)B6H\n5WZ)MV2\nRVG)442\nW3R)CCX\n73K)L33\n17K)HGG\nLC7)GNV\nQ27)Q1L\n2HC)V2Z\nHXX)KC4\n6WQ)JCL\nT92)M8D\nKNV)6FC\nT9B)559\nRKY)DBT\nRRG)LRM\nDSQ)PCQ\nKF5)6YQ\nR8N)FXM\nJL9)KQK\nRCK)3P3\n8W4)XBP\nTNL)XYM\n4NG)Q26\nJX3)Q27\nGX4)8QH\nGTC)GD7\nLMV)XQK\nYZ8)2MX\nYL1)PFV\nVHW)WT9\nS99)58S\n13Q)NDZ\nLHB)LHT\nFQT)S59\nDX6)RWL\n288)MP2\n2LL)4H2\nZPC)GTC\nHLN)4RR\n22J)9MY\nFH4)19L\nNF3)KRT\nJCL)DGR\nGSX)FSG\nTG1)X7B\nNHN)S46\nFZP)P5G\nS66)XTQ\nWQC)G1P\nXNT)QSX\nV14)673\nTLR)B9P\n2YP)ZPH\nFKC)VFR\n56K)QHM\nB9W)133\nTNV)P2L\n9NX)F98\n8KQ)5VP\n5PH)FY4\n6VK)S1H\nZZM)M7R\nPLQ)MXZ\nGFY)RW8\n758)78F\nFQ1)JCY\nWV5)W58\nF3G)XPM\nGLX)GL5\nLQB)7BS\nZ55)GQ5\nTHN)KH9\n5GG)V31\nG1P)FR3\nXFC)K5S\nHQ5)XBH\nHJT)FRG\nVSJ)MB3\nVHY)FQ1\nVL1)RDB\nL33)1NB\nM6M)LXJ\nBCF)J67\n6WT)B69\nFQT)R6B\nL3S)HS1\nVSS)C5B\n9FD)G5M\nP2L)GCB\nL5X)1VY\n26X)5Z7\n4D6)LWG\n8DZ)WT6\nXWG)V72\nQJ7)M1Z\n58S)HMN\nHJR)QY8\nL7S)ZYF\nMR5)5PV\nC8B)WFP\nQK8)1TX\nJ12)2V7\nJ76)RS8\n6MV)WJ3\n16C)HLY\n4TY)73H\nH6D)XQ8\n4JR)MX5\nM37)Q65\nW74)XX9\nTK5)49S\nB6H)BZ4\nF6N)BKV\nJF8)ZTS\nWBS)4LS\nLTH)J42\nJNL)6WT\nGL1)5Q3\nXJL)KVB\nYMW)MM1\n9HF)356\n5QP)7XH\nK95)KJ1\nQWY)8W6\nKPN)J1C\nRRQ)XNV\n419)XMQ\nYYR)9RZ\n1CT)KLZ\n8XQ)V5M\nXGT)ZN3\nWZ4)G8Z\nYWP)3MS\n93W)18V\n7FY)M8N\nTFP)NM4\nFR3)7N4\nJGN)NF3\nJDY)1P7\nM17)WS3\nSLQ)RVW\nDQJ)XR9\nFNF)89K\nYKH)KKH\nK6F)RPD\nJ75)9GP\n2X9)DLD\n6CN)7W1\n7TK)MGN\n2SC)3KG\nBMS)MQG\nZFX)53H\nZ3C)LT3\nVDN)GRY\n6JL)9Z9\n7NF)PML\n14T)WSX\n1V9)N7N\nN3V)HPD\nF53)PPJ\nDQ9)TNL\nJPH)1FL\nL1C)WBS\nRMY)BMJ\nMV1)1CP\n4GY)8M8\nCGN)4GC\n7B1)26V\n8QN)JSG\nVQC)XRT\nJ1N)MKN\nPYS)289\n7Z9)HDV\nSJB)4C6\nLX3)HX7\n94G)R1P\nDY7)D6W\n3FH)2RV\nXKS)WHK\nFNB)6L2\nK6Y)NK7\nBYV)Z9F\nQBZ)FVB\nC5B)73F\n6SC)DMP\nB5R)K3N\nBKV)YKB\nVW8)1FD\nPWN)QPL\nDG5)BQH\n1MD)YCZ\nKWK)CZ9\nJY5)NX4\nF2T)8QR\nRYS)KF3\nLRM)M1W\nLXX)CD1\n98M)J4X\nD6Y)M17\n97S)BSK\nX11)2X9\nWKP)57B\nCCX)221\nGJM)T5M\nLT9)7YP\nRVW)FZP\n62K)CLV\n3VL)QQK\n3XD)HQ5\nW3R)QFC\n8JW)32H\nKCD)2HC\nSND)9FD\n4PR)D3D\nQ68)77F\n56P)87R\nSD1)RHS\nCOM)2WF\nQ53)HBT\nZJ5)68M\n4VH)VDN\nZ36)2QN\nFD6)R96\nNWP)HZP\n4T6)LGZ\nQTV)GF6\nS2P)HZ6\nXFC)S8R\n2V5)QDH\nPPQ)CD6\nZ71)YKH\nRYK)864\nVMF)MZQ\nKZK)KTF\nKZK)XKS\nSLK)WBM\nHWZ)CX5\nN1L)2HF\n54X)JWR\nTYF)YY5\n3DM)264\nFLG)FBP\nD2J)376\n2RV)JPH\nPMX)12Z\nCLB)7TS\nCNC)RCK\n281)8M2\n2SB)R4M\nKF3)1RN\nB38)4FG\nGV9)XV5\n33M)676\nQGN)HS2\n7D4)GFY\n4CT)STK\nN15)R1V\n29B)Q4W\nDXZ)BV8\n563)GLX\nXDF)GRS\n1LQ)TK5\nBPW)C93\nKP7)GLH\n2X2)F24\nYTR)LT9\nMN2)NNP\nW7K)TF6\nDGK)KPN\nD6W)PVB\n67R)3DJ\nTB6)NLV\nMDR)PLQ\nC92)3NQ\nF8D)1P3\n4ZK)T9B\n347)JJY\nXNF)6HQ\nQKK)63K\nB4H)HYH\nTCT)GFD\nCPW)5MK\nBQ8)789\nSGD)R84\nCBM)7XL\nNKX)4TB\n9K7)GTS\nVWL)3V7\nM1Z)7DX\nXXQ)W69\n676)Z55\n1NP)GW9\nHYX)Q7V\nYY5)5WZ\n7PK)Q12\n2VN)GWW\nNC3)SXB\nH5W)V14\nPQC)CPM\nJVM)RZL\nWQQ)R87\nYQ2)JCS\n3SR)MF9\nRPG)BZ8\nL36)CDK\n6WT)KSJ\nQSX)9RM\nSTK)JM7\nDPL)CXG\n8DZ)FNV\nR96)7NC\nC5D)SAN\n1FM)4T1\nFXD)KCT\nK7W)347\nC1P)FS2\nC8R)Y4D\nF35)Q4Y\n3Y4)Q53\nWHF)212\nRMZ)TVV\nDKM)Y8V\n395)C76\n65Z)JF8\n4LS)CFB\nBJC)132\nP4J)WDH\nQQK)LCT\nTWS)LP4\nQ6W)XJJ\nGRS)JDY\n1WG)G46\nPCQ)X9P\n12Z)41J\nJF4)LXM\nF98)2LG\nDQH)15P\nDTB)9L7\n6XG)8D8\nTKW)2JM\nX5P)M35\n154)CGN\n7LW)KRQ\nKCT)57X\n9BW)QGN\nGHF)D2J\nY72)HW4\n4V3)LFD\nZXN)T3J\nW5B)L7G\nNP1)JGL\n6L2)J3G\n6B7)NTH\nDFT)DKM\nGP4)HXH\nFQP)THN\nXQ3)QPN\nH2M)ZCD\n37H)1RM\nMQM)BDT\nY5Y)PZB\nWT6)B3N\nR7V)MSH\n2T8)T53\n3WX)WZ4\nWHP)69M\nYFS)NF8\nG3K)4TG\nV2B)3YR\n5HM)CLX\n5HK)75S\nHC5)SLQ\n55M)KPZ\nKKH)4CV\nB65)SJB\nTFF)1Y6\n5Q3)HJR\nMVQ)MCB\n12V)757\nYQX)V73\n9C8)JN8\nDYT)5PH\nC8M)69X\nVGH)S5T\nTNX)CXD\nXK3)TVX\n1L3)7RX\n662)B3T\nRTT)KQZ\n21H)RMF\nZQF)2YP\n8ZS)7DR\nX8C)GY8\nHNZ)JPZ\n133)1NP\nPJJ)CPS\n18V)RPG\n9NC)STQ\nJ1Y)3MH\n6SR)B73\nHPL)41V\nYMW)BPW\nRDJ)KN3\nLXM)P76\n42H)9W9\nCCD)W8T\n176)S7S\nB41)G6R\nKLZ)P19\nSQ6)XFC\nTF6)H89\n2FV)8DB\n4Y2)4D2\nJTF)TKW\nLDQ)X6L\nLT3)T3K\nQK1)GB4\nW4N)7Z9\nBKM)7CC\n65H)S2P\n8K9)YJZ\n253)9WY\nKVN)4PP\n2Z4)F3G\nC93)KTW\n5FM)QSW\nF24)4DX\nT3K)NR8\n3G7)RKY\nV73)6MV\nBT3)2F1\nHLY)CCD\nGLH)94G\n2CZ)W2J\nNSK)NC8\nV5H)M41\n19X)NXV\nQMQ)54Q\nLRG)Q48\n7CQ)YB2\nTSF)8XQ\nFCS)2VF\n16V)VXC\nHBZ)WGD\nVJV)JY5\nXRT)J2X\n424)5GR\nSXB)XJP\nBJC)5PJ\nSND)49C\nLC1)8Y7\nFR7)L6J\nH2M)XHR\nBW2)C9Z\n2HW)185\nTYL)2K5\n15P)6HZ\nLK7)YZ8\nJPZ)NNJ\nNGS)G3S\nGL5)QVR\nDJH)NZ6\nGKB)9S3\nJCX)Z42\nP2S)T92\n9ZN)ZCY\nPY6)DVV\nYP3)W5Z\n11M)WQT\nPQ9)G3Z\n2S1)SPV\nCQX)ZQF\nYPZ)LQ9\nVRQ)TZ9\nGY8)26X\n985)YQ3\n9ZP)9RP\n9JW)VL1\n4MX)33M\nL6J)L3S\nKBG)P3N\nG22)RPQ\n948)1FS\n3KS)CW7\nWCV)WNV\nLB9)KL3\nBDT)21H\nQ53)VB9\nHCV)2CZ\n7RX)TQM\n4GC)LDV\nPL9)N2M\n1VY)NL2\nR96)153\n37F)53G\nVT1)KF4\n61T)838\nHBT)R4N\nH81)TB6\n1FL)FR7\nJ65)D9Z\n8Y7)22J\nKZ3)JFL\nW5B)B9W\nG27)QJ7\nV72)1MD\n5G8)J12\nT39)STH\n6JL)HCV\nSK4)K6Y\nZ1T)2FV\nPPJ)LJW\nJKJ)64J\n47Y)QGH\nN1V)CVT\nPJQ)TWQ\n26V)QMC\n5Y3)1QR\nF1W)VXM\n49S)QKP")
  (part-2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"))