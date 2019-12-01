(ns advent-2018.day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix :as matrix]))

(def toy-data "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>")
(def data "position=< 41660,  20869> velocity=<-4, -2>\nposition=< 41617,  10491> velocity=<-4, -1>\nposition=< 41669, -10244> velocity=<-4,  1>\nposition=< 10509, -41348> velocity=<-1,  4>\nposition=<-41324, -51717> velocity=< 4,  5>\nposition=< 41616, -30982> velocity=<-4,  3>\nposition=< 31242, -51716> velocity=<-3,  5>\nposition=<-20570, -30976> velocity=< 2,  3>\nposition=< 20882,  51971> velocity=<-2, -5>\nposition=<-10205, -30981> velocity=< 1,  3>\nposition=< 31299, -20612> velocity=<-3,  2>\nposition=<-41320,  51974> velocity=< 4, -5>\nposition=<-10205, -51723> velocity=< 1,  5>\nposition=<-10237,  10492> velocity=< 1, -1>\nposition=<-10181, -10244> velocity=< 1,  1>\nposition=< 31275, -10243> velocity=<-3,  1>\nposition=< 52025,  51974> velocity=<-5, -5>\nposition=< 41634, -20612> velocity=<-4,  2>\nposition=< 41632, -20610> velocity=<-4,  2>\nposition=< 52028, -10247> velocity=<-5,  1>\nposition=<-30972,  51969> velocity=< 3, -5>\nposition=<-30946, -20609> velocity=< 3,  2>\nposition=<-30918,  31229> velocity=< 3, -3>\nposition=< 41618, -51714> velocity=<-4,  5>\nposition=< 31284,  31231> velocity=<-3, -3>\nposition=<-30931, -20616> velocity=< 3,  2>\nposition=<-30964,  31238> velocity=< 3, -3>\nposition=<-20577,  10495> velocity=< 2, -1>\nposition=< 20899, -51720> velocity=<-2,  5>\nposition=< 31284, -41354> velocity=<-3,  4>\nposition=<-20570,  10491> velocity=< 2, -1>\nposition=<-20605,  20865> velocity=< 2, -2>\nposition=<-41320, -41345> velocity=< 4,  4>\nposition=<-41328, -10240> velocity=< 4,  1>\nposition=< 20926,  41601> velocity=<-2, -4>\nposition=<-30967,  10491> velocity=< 3, -1>\nposition=< 41637, -10242> velocity=<-4,  1>\nposition=<-10179,  20864> velocity=< 1, -2>\nposition=<-51665, -10240> velocity=< 5,  1>\nposition=<-30951,  41600> velocity=< 3, -4>\nposition=<-51709, -10246> velocity=< 5,  1>\nposition=< 20923, -10239> velocity=<-2,  1>\nposition=<-41296,  31237> velocity=< 4, -3>\nposition=< 10521,  41601> velocity=<-1, -4>\nposition=<-41296, -30983> velocity=< 4,  3>\nposition=<-20586,  20861> velocity=< 2, -2>\nposition=<-10176,  10499> velocity=< 1, -1>\nposition=< 20894,  10493> velocity=<-2, -1>\nposition=< 51977, -20613> velocity=<-5,  2>\nposition=<-30965,  51967> velocity=< 3, -5>\nposition=<-41296,  20866> velocity=< 4, -2>\nposition=<-41334, -51714> velocity=< 4,  5>\nposition=< 41648,  31238> velocity=<-4, -3>\nposition=< 20896,  10495> velocity=<-2, -1>\nposition=<-20566, -30978> velocity=< 2,  3>\nposition=< 20910, -20609> velocity=<-2,  2>\nposition=<-41315,  10491> velocity=< 4, -1>\nposition=< 51995, -41345> velocity=<-5,  4>\nposition=< 10558, -51714> velocity=<-1,  5>\nposition=< 20870, -41345> velocity=<-2,  4>\nposition=<-41303,  10497> velocity=< 4, -1>\nposition=< 41621,  10498> velocity=<-4, -1>\nposition=< 41621, -51716> velocity=<-4,  5>\nposition=<-51689,  31230> velocity=< 5, -3>\nposition=<-30927,  20864> velocity=< 3, -2>\nposition=< 10559,  10500> velocity=<-1, -1>\nposition=< 10557,  20867> velocity=<-1, -2>\nposition=<-51672,  31238> velocity=< 5, -3>\nposition=< 20890, -51720> velocity=<-2,  5>\nposition=<-10236,  10495> velocity=< 1, -1>\nposition=<-30963, -10238> velocity=< 3,  1>\nposition=<-20603,  41605> velocity=< 2, -4>\nposition=<-20563,  20869> velocity=< 2, -2>\nposition=<-20553, -20608> velocity=< 2,  2>\nposition=< 31267, -51719> velocity=<-3,  5>\nposition=<-41285,  31229> velocity=< 4, -3>\nposition=< 41608,  20864> velocity=<-4, -2>\nposition=<-30914, -10241> velocity=< 3,  1>\nposition=<-41315,  20865> velocity=< 4, -2>\nposition=< 52021, -20613> velocity=<-5,  2>\nposition=< 52029, -41345> velocity=<-5,  4>\nposition=<-30919, -51717> velocity=< 3,  5>\nposition=<-20570, -41345> velocity=< 2,  4>\nposition=< 52012, -51714> velocity=<-5,  5>\nposition=<-41301, -41354> velocity=< 4,  4>\nposition=< 41666,  10495> velocity=<-4, -1>\nposition=< 52009, -30984> velocity=<-5,  3>\nposition=< 52017, -41354> velocity=<-5,  4>\nposition=<-30941, -30981> velocity=< 3,  3>\nposition=< 20872, -30982> velocity=<-2,  3>\nposition=< 31263,  41603> velocity=<-3, -4>\nposition=< 10530, -30984> velocity=<-1,  3>\nposition=< 41653,  10492> velocity=<-4, -1>\nposition=<-41284,  20864> velocity=< 4, -2>\nposition=< 31268,  51971> velocity=<-3, -5>\nposition=<-41288, -10240> velocity=< 4,  1>\nposition=<-20602,  51975> velocity=< 2, -5>\nposition=<-51701,  31238> velocity=< 5, -3>\nposition=< 31284, -51722> velocity=<-3,  5>\nposition=< 52006,  20866> velocity=<-5, -2>\nposition=< 10557, -51721> velocity=<-1,  5>\nposition=<-51700,  10497> velocity=< 5, -1>\nposition=< 10501,  31234> velocity=<-1, -3>\nposition=< 20927,  31233> velocity=<-2, -3>\nposition=< 31268, -10239> velocity=<-3,  1>\nposition=<-30927,  20866> velocity=< 3, -2>\nposition=< 20926,  41604> velocity=<-2, -4>\nposition=<-20574,  41602> velocity=< 2, -4>\nposition=< 41632, -10247> velocity=<-4,  1>\nposition=<-41317,  20864> velocity=< 4, -2>\nposition=< 10501, -20608> velocity=<-1,  2>\nposition=<-41283, -20609> velocity=< 4,  2>\nposition=<-20606, -20615> velocity=< 2,  2>\nposition=< 10551, -51714> velocity=<-1,  5>\nposition=<-20550,  51976> velocity=< 2, -5>\nposition=< 51997,  20860> velocity=<-5, -2>\nposition=<-30965, -20612> velocity=< 3,  2>\nposition=< 41664, -30984> velocity=<-4,  3>\nposition=< 41640, -10241> velocity=<-4,  1>\nposition=< 31283,  51970> velocity=<-3, -5>\nposition=< 10536,  10495> velocity=<-1, -1>\nposition=< 31300, -51715> velocity=<-3,  5>\nposition=< 41644, -41354> velocity=<-4,  4>\nposition=< 52006,  31234> velocity=<-5, -3>\nposition=< 52002, -10243> velocity=<-5,  1>\nposition=< 31292,  41599> velocity=<-3, -4>\nposition=<-51657,  10498> velocity=< 5, -1>\nposition=<-20586,  51974> velocity=< 2, -5>\nposition=< 20903, -41350> velocity=<-2,  4>\nposition=< 41621, -10242> velocity=<-4,  1>\nposition=< 51985, -10242> velocity=<-5,  1>\nposition=< 20899, -10245> velocity=<-2,  1>\nposition=< 41645,  10500> velocity=<-4, -1>\nposition=< 10542, -41354> velocity=<-1,  4>\nposition=< 10533, -41351> velocity=<-1,  4>\nposition=<-41300, -51723> velocity=< 4,  5>\nposition=< 41668, -41345> velocity=<-4,  4>\nposition=<-20606,  20868> velocity=< 2, -2>\nposition=< 41608, -10239> velocity=<-4,  1>\nposition=< 31273, -51723> velocity=<-3,  5>\nposition=<-10225, -30985> velocity=< 1,  3>\nposition=<-10193,  20869> velocity=< 1, -2>\nposition=< 41617,  31238> velocity=<-4, -3>\nposition=<-30938,  31229> velocity=< 3, -3>\nposition=< 10557,  41607> velocity=<-1, -4>\nposition=<-20588, -20607> velocity=< 2,  2>\nposition=< 51993, -41346> velocity=<-5,  4>\nposition=< 20890, -20613> velocity=<-2,  2>\nposition=<-51713,  51971> velocity=< 5, -5>\nposition=<-41288, -10247> velocity=< 4,  1>\nposition=< 10559, -30985> velocity=<-1,  3>\nposition=< 20905, -51714> velocity=<-2,  5>\nposition=< 41637,  41607> velocity=<-4, -4>\nposition=<-20602, -30984> velocity=< 2,  3>\nposition=<-20582, -20608> velocity=< 2,  2>\nposition=<-20595, -41345> velocity=< 2,  4>\nposition=< 10506, -30976> velocity=<-1,  3>\nposition=<-20587, -51714> velocity=< 2,  5>\nposition=< 41616, -30983> velocity=<-4,  3>\nposition=<-51678,  20860> velocity=< 5, -2>\nposition=<-51670,  31233> velocity=< 5, -3>\nposition=< 20902, -51718> velocity=<-2,  5>\nposition=<-30950, -41350> velocity=< 3,  4>\nposition=< 20912,  10491> velocity=<-2, -1>\nposition=< 10543, -10238> velocity=<-1,  1>\nposition=< 31239,  10491> velocity=<-3, -1>\nposition=< 10530, -51715> velocity=<-1,  5>\nposition=< 52006, -10247> velocity=<-5,  1>\nposition=<-20587, -51714> velocity=< 2,  5>\nposition=<-20561,  41598> velocity=< 2, -4>\nposition=<-10229,  51969> velocity=< 1, -5>\nposition=< 31247,  51971> velocity=<-3, -5>\nposition=<-41333, -10243> velocity=< 4,  1>\nposition=< 41632, -10243> velocity=<-4,  1>\nposition=< 41652,  41598> velocity=<-4, -4>\nposition=<-20593, -51718> velocity=< 2,  5>\nposition=<-51677, -10238> velocity=< 5,  1>\nposition=<-51654, -30976> velocity=< 5,  3>\nposition=< 41656, -10242> velocity=<-4,  1>\nposition=< 20930,  20869> velocity=<-2, -2>\nposition=< 20929, -30981> velocity=<-2,  3>\nposition=<-41312,  31235> velocity=< 4, -3>\nposition=< 31288,  41598> velocity=<-3, -4>\nposition=< 41649, -10241> velocity=<-4,  1>\nposition=<-10194, -30976> velocity=< 1,  3>\nposition=<-51670, -30985> velocity=< 5,  3>\nposition=< 20903,  20860> velocity=<-2, -2>\nposition=<-20566, -51714> velocity=< 2,  5>\nposition=<-41335,  41602> velocity=< 4, -4>\nposition=<-30967,  41602> velocity=< 3, -4>\nposition=< 20902,  41599> velocity=<-2, -4>\nposition=< 41628, -20615> velocity=<-4,  2>\nposition=<-41301, -20616> velocity=< 4,  2>\nposition=< 41632,  31238> velocity=<-4, -3>\nposition=< 20899,  20863> velocity=<-2, -2>\nposition=< 10546,  51976> velocity=<-1, -5>\nposition=<-51681,  41607> velocity=< 5, -4>\nposition=<-10216,  41598> velocity=< 1, -4>\nposition=< 41648, -10238> velocity=<-4,  1>\nposition=< 52033, -20612> velocity=<-5,  2>\nposition=< 31248, -41345> velocity=<-3,  4>\nposition=< 41616, -20615> velocity=<-4,  2>\nposition=<-51662,  51976> velocity=< 5, -5>\nposition=<-30962,  51970> velocity=< 3, -5>\nposition=< 51986,  20860> velocity=<-5, -2>\nposition=< 10512,  41598> velocity=<-1, -4>\nposition=<-30926,  41607> velocity=< 3, -4>\nposition=< 31271, -10238> velocity=<-3,  1>\nposition=< 10536, -51714> velocity=<-1,  5>\nposition=< 31287,  31230> velocity=<-3, -3>\nposition=< 10525, -41346> velocity=<-1,  4>\nposition=< 41640, -10242> velocity=<-4,  1>\nposition=<-51680, -41354> velocity=< 5,  4>\nposition=< 41632, -10241> velocity=<-4,  1>\nposition=< 20926,  31234> velocity=<-2, -3>\nposition=<-10196, -30979> velocity=< 1,  3>\nposition=< 31273, -30981> velocity=<-3,  3>\nposition=< 20918, -30977> velocity=<-2,  3>\nposition=<-30933, -30980> velocity=< 3,  3>\nposition=< 31239, -41354> velocity=<-3,  4>\nposition=< 20905,  20864> velocity=<-2, -2>\nposition=<-10226,  41598> velocity=< 1, -4>\nposition=< 31265, -20612> velocity=<-3,  2>\nposition=<-41312, -51723> velocity=< 4,  5>\nposition=< 20906, -41354> velocity=<-2,  4>\nposition=< 52027, -41354> velocity=<-5,  4>\nposition=<-30955, -30983> velocity=< 3,  3>\nposition=<-51688,  41602> velocity=< 5, -4>\nposition=<-30967, -30985> velocity=< 3,  3>\nposition=<-41315,  51969> velocity=< 4, -5>\nposition=< 20931, -41346> velocity=<-2,  4>\nposition=< 31268, -41353> velocity=<-3,  4>\nposition=<-51657,  31238> velocity=< 5, -3>\nposition=< 31240, -51719> velocity=<-3,  5>\nposition=<-20606,  31238> velocity=< 2, -3>\nposition=< 31243, -51715> velocity=<-3,  5>\nposition=<-20569, -41354> velocity=< 2,  4>\nposition=< 20899,  31232> velocity=<-2, -3>\nposition=<-10224,  20861> velocity=< 1, -2>\nposition=< 10521,  10495> velocity=<-1, -1>\nposition=< 10552, -51714> velocity=<-1,  5>\nposition=<-20601, -51723> velocity=< 2,  5>\nposition=< 10545,  41601> velocity=<-1, -4>\nposition=< 20921,  31238> velocity=<-2, -3>\nposition=<-51689,  51971> velocity=< 5, -5>\nposition=< 20902, -41346> velocity=<-2,  4>\nposition=<-10181, -10239> velocity=< 1,  1>\nposition=<-20574,  10498> velocity=< 2, -1>\nposition=<-41288, -10241> velocity=< 4,  1>\nposition=< 10514,  20865> velocity=<-1, -2>\nposition=< 31295,  31229> velocity=<-3, -3>\nposition=<-41303, -30976> velocity=< 4,  3>\nposition=< 51985,  31232> velocity=<-5, -3>\nposition=<-20593,  10499> velocity=< 2, -1>\nposition=<-51681,  41601> velocity=< 5, -4>\nposition=<-30958, -10238> velocity=< 3,  1>\nposition=< 20870,  51968> velocity=<-2, -5>\nposition=< 41632,  10494> velocity=<-4, -1>\nposition=<-20566,  10499> velocity=< 2, -1>\nposition=<-30919, -41352> velocity=< 3,  4>\nposition=< 52006, -41350> velocity=<-5,  4>\nposition=< 10549,  41599> velocity=<-1, -4>\nposition=< 51996, -10247> velocity=<-5,  1>\nposition=<-41344,  31235> velocity=< 4, -3>\nposition=<-20598, -10245> velocity=< 2,  1>\nposition=< 52030,  51975> velocity=<-5, -5>\nposition=<-41331,  41606> velocity=< 4, -4>\nposition=< 20930, -41350> velocity=<-2,  4>\nposition=<-20558,  10495> velocity=< 2, -1>\nposition=< 20931,  31235> velocity=<-2, -3>\nposition=<-30916, -51723> velocity=< 3,  5>\nposition=<-20558, -10239> velocity=< 2,  1>\nposition=< 31239,  41600> velocity=<-3, -4>\nposition=<-41324,  20862> velocity=< 4, -2>\nposition=<-10177,  10491> velocity=< 1, -1>\nposition=< 31300, -51720> velocity=<-3,  5>\nposition=< 31268, -10246> velocity=<-3,  1>\nposition=< 10503,  31235> velocity=<-1, -3>\nposition=<-20586, -20614> velocity=< 2,  2>\nposition=<-30927,  10492> velocity=< 3, -1>\nposition=< 20912,  20865> velocity=<-2, -2>\nposition=<-41283,  20862> velocity=< 4, -2>\nposition=< 52033,  10492> velocity=<-5, -1>\nposition=<-51700, -10239> velocity=< 5,  1>\nposition=<-51705,  41604> velocity=< 5, -4>\nposition=< 10502, -20612> velocity=<-1,  2>\nposition=< 10559, -51714> velocity=<-1,  5>\nposition=<-20547, -51719> velocity=< 2,  5>\nposition=< 20931, -20614> velocity=<-2,  2>\nposition=< 41611, -20614> velocity=<-4,  2>\nposition=< 52033, -30977> velocity=<-5,  3>\nposition=<-10237, -20612> velocity=< 1,  2>\nposition=<-30941, -20607> velocity=< 3,  2>\nposition=< 31263,  10496> velocity=<-3, -1>\nposition=< 31247,  31235> velocity=<-3, -3>\nposition=< 20922, -20616> velocity=<-2,  2>\nposition=< 52021,  51976> velocity=<-5, -5>\nposition=<-51713, -51716> velocity=< 5,  5>\nposition=<-41336,  31234> velocity=< 4, -3>\nposition=< 41640,  51972> velocity=<-4, -5>\nposition=< 31259, -51719> velocity=<-3,  5>\nposition=<-20579,  51971> velocity=< 2, -5>\nposition=<-10208, -10239> velocity=< 1,  1>\nposition=<-51665, -30978> velocity=< 5,  3>\nposition=<-30934,  31229> velocity=< 3, -3>\nposition=< 20894,  41602> velocity=<-2, -4>\nposition=<-20597, -20612> velocity=< 2,  2>\nposition=< 41666,  20860> velocity=<-4, -2>\nposition=< 52001, -51716> velocity=<-5,  5>\nposition=<-10203, -51714> velocity=< 1,  5>\nposition=<-10229, -30982> velocity=< 1,  3>\nposition=<-10181,  41603> velocity=< 1, -4>\nposition=< 52011, -20616> velocity=<-5,  2>\nposition=< 31300, -10246> velocity=<-3,  1>\nposition=<-20550, -20616> velocity=< 2,  2>\nposition=< 10520,  31238> velocity=<-1, -3>\nposition=<-10200, -20607> velocity=< 1,  2>\nposition=<-20598,  51975> velocity=< 2, -5>\nposition=< 31247, -41345> velocity=<-3,  4>\nposition=<-10189, -41351> velocity=< 1,  4>\nposition=< 31288, -41345> velocity=<-3,  4>\nposition=< 31296, -41354> velocity=<-3,  4>\nposition=<-51689, -51721> velocity=< 5,  5>\nposition=< 10533, -20608> velocity=<-1,  2>\nposition=< 10521,  41604> velocity=<-1, -4>\nposition=<-10218,  51967> velocity=< 1, -5>\nposition=< 20918,  51969> velocity=<-2, -5>\nposition=<-10229,  10498> velocity=< 1, -1>\nposition=< 51986, -20616> velocity=<-5,  2>\nposition=< 20913, -51714> velocity=<-2,  5>\nposition=<-51681, -41345> velocity=< 5,  4>\nposition=< 31297, -10243> velocity=<-3,  1>\nposition=<-30939, -20612> velocity=< 3,  2>\nposition=<-30954, -10247> velocity=< 3,  1>\nposition=< 51990,  20867> velocity=<-5, -2>\nposition=<-51654, -41354> velocity=< 5,  4>\nposition=<-20573, -51714> velocity=< 2,  5>\nposition=< 20905,  41598> velocity=<-2, -4>\nposition=< 51977, -51717> velocity=<-5,  5>\nposition=< 31300,  31234> velocity=<-3, -3>\nposition=< 41640, -10245> velocity=<-4,  1>\nposition=<-41296,  51971> velocity=< 4, -5>\nposition=< 10551, -30976> velocity=<-1,  3>\nposition=<-20554, -41345> velocity=< 2,  4>\nposition=< 51986,  20864> velocity=<-5, -2>\nposition=< 41621, -41352> velocity=<-4,  4>\nposition=<-30943, -41350> velocity=< 3,  4>\nposition=< 52010, -41354> velocity=<-5,  4>\nposition=<-51693, -30980> velocity=< 5,  3>\nposition=< 41636,  51971> velocity=<-4, -5>\nposition=< 52006,  41607> velocity=<-5, -4>\nposition=< 52012,  31233> velocity=<-5, -3>\nposition=< 31258, -30985> velocity=<-3,  3>\nposition=<-41303,  51976> velocity=< 4, -5>\nposition=<-30943,  10494> velocity=< 3, -1>\nposition=< 52009,  41598> velocity=<-5, -4>\nposition=<-41332, -30981> velocity=< 4,  3>\nposition=< 20894,  31235> velocity=<-2, -3>\nposition=<-10237,  31235> velocity=< 1, -3>\nposition=< 52038,  31232> velocity=<-5, -3>\nposition=< 10513,  20869> velocity=<-1, -2>\nposition=< 31257, -30976> velocity=<-3,  3>\nposition=< 52035,  51967> velocity=<-5, -5>\nposition=<-20577, -51716> velocity=< 2,  5>\nposition=<-10237,  41601> velocity=< 1, -4>\nposition=< 41637, -20607> velocity=<-4,  2>\nposition=<-20561, -30983> velocity=< 2,  3>\nposition=<-41336,  10495> velocity=< 4, -1>\nposition=< 31296,  51971> velocity=<-3, -5>\nposition=< 31297, -10238> velocity=<-3,  1>\nposition=< 51997,  51975> velocity=<-5, -5>\nposition=< 10533,  31237> velocity=<-1, -3>")

(defn parse [s]
  (mapv (fn [l]
          (let [[x y xv yv] (map #(Long/parseLong %)
                                 (re-seq #"-?\d+" l))]
            {:pos [x y]
             :vel [xv yv]}))
        (str/split-lines s)))

(defn bounds [pnts]
  (reduce (fn [{[minx miny] :min
                [maxx maxy] :max} [x y]]
            {:min [(min minx x) (min miny y)]
             :max [(max maxx x) (max maxy y)]})
          {:min [Long/MAX_VALUE Long/MAX_VALUE]
           :max [Long/MIN_VALUE Long/MIN_VALUE]}
          pnts))

(defn draw [{[minx miny] :min
             [maxx maxy] :max} star-positions]
  (let [has-star? (into #{} star-positions)]
    (doseq [j (range miny (inc maxy))]
      (doseq [i (range minx (inc maxx))]
        (if (has-star? [i j])
          (print "#")
          (print ".")))
      (print "\n"))))

(defn move [{[x y] :pos
             [vx vy] :vel} t]
  [(+ x (* t vx))
   (+ y (* t vy))])

;; -8 <=  (y1 + (t * vy1)) - (y2 + (t * vy2)) <= 8

(comment )

(defn centralize [pnts]
  (matrix/sub pnts
              (matrix/div (reduce matrix/add
                                  pnts)
                          (count pnts))))

(defn sum-of-dots [xs ys]
  (reduce +
          (map matrix/dot
               xs
               ys)))

(defn the-t [stars]
  (let [xs (centralize (map :pos stars))
        vs (centralize (map :vel stars))]
    (long (- (/ (sum-of-dots xs vs)
                (sum-of-dots vs vs))))))

(comment
  ;; method a
  (let [[{[x1 y1] :pos
          [vx1 vy1] :vel}
         {[x2 y2] :pos
          [vx2 vy2] :vel}] (parse data)]
    (Math/abs ^long (/ (- y2 y1 8)
                       (- vy2 vy1))))

  (time (the-t (parse data)))
  ;; method b
  (let [w (io/writer System/out)]
           (binding [*out* *out*]
             (let [poss (map #(move % 10369)
                             (parse data))]
               (draw (bounds poss)
                     poss)))))

