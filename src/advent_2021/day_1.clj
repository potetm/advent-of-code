(ns advent-2021.day-1
  (:require
    [clojure.string :as str]
    [advent.util :as util]
    [net.cgrand.xforms :as xforms]))


(defn parse [in]
  (map #(Long/parseLong %)
       (str/split-lines in)))


(def xf
  (comp (xforms/partition 2 1)
        (filter (fn [[l r]]
                  (< l r)))))


(defn part-1 [d]
  (xforms/count xf d))


(defn part-2 [d]
  (xforms/count (comp (xforms/partition 3 1)
                      (map util/sum)
                      xf)
                d))

(comment
  (def tin "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n")
  (def in "148\n167\n168\n169\n182\n188\n193\n209\n195\n206\n214\n219\n225\n219\n211\n215\n216\n195\n200\n197\n226\n231\n234\n248\n249\n265\n262\n275\n271\n283\n297\n302\n306\n307\n309\n313\n306\n307\n310\n307\n306\n284\n291\n296\n297\n298\n302\n297\n295\n296\n297\n293\n294\n295\n303\n302\n304\n305\n303\n288\n289\n290\n291\n290\n288\n302\n304\n282\n283\n290\n301\n304\n301\n330\n337\n341\n340\n337\n341\n342\n343\n301\n311\n312\n302\n317\n318\n319\n345\n347\n354\n358\n356\n357\n362\n361\n357\n375\n376\n383\n384\n382\n383\n384\n391\n400\n410\n413\n419\n421\n424\n426\n425\n427\n429\n432\n433\n434\n440\n446\n444\n453\n461\n463\n461\n464\n463\n489\n486\n485\n486\n499\n498\n493\n494\n485\n490\n499\n498\n506\n510\n519\n524\n520\n521\n543\n550\n551\n553\n564\n567\n568\n572\n597\n579\n592\n597\n618\n620\n628\n651\n674\n671\n684\n719\n718\n730\n727\n729\n724\n720\n737\n740\n753\n763\n764\n762\n772\n774\n773\n771\n777\n774\n783\n791\n792\n791\n801\n822\n821\n814\n811\n812\n817\n835\n844\n837\n839\n841\n855\n872\n881\n895\n910\n900\n902\n913\n914\n911\n917\n918\n923\n933\n932\n931\n915\n912\n923\n939\n940\n941\n943\n947\n948\n931\n933\n944\n933\n947\n948\n949\n956\n949\n948\n969\n973\n989\n993\n994\n993\n1007\n1010\n1020\n1024\n1026\n1031\n1032\n1028\n1045\n1015\n1018\n1024\n998\n1000\n1006\n994\n995\n993\n1010\n1011\n1010\n1015\n1021\n1039\n1045\n1041\n1049\n1044\n1050\n1063\n1066\n1051\n1056\n1059\n1060\n1059\n1064\n1067\n1066\n1067\n1069\n1072\n1074\n1077\n1078\n1080\n1086\n1079\n1089\n1093\n1099\n1102\n1103\n1108\n1119\n1138\n1139\n1144\n1148\n1151\n1152\n1151\n1155\n1154\n1153\n1157\n1159\n1164\n1168\n1172\n1171\n1182\n1189\n1190\n1192\n1194\n1196\n1197\n1222\n1216\n1220\n1206\n1205\n1196\n1200\n1210\n1203\n1200\n1212\n1218\n1219\n1205\n1211\n1212\n1213\n1176\n1187\n1174\n1175\n1180\n1181\n1187\n1185\n1211\n1212\n1199\n1231\n1234\n1249\n1250\n1247\n1235\n1234\n1236\n1237\n1266\n1265\n1286\n1289\n1282\n1283\n1273\n1282\n1267\n1271\n1278\n1291\n1307\n1294\n1292\n1320\n1323\n1314\n1317\n1342\n1344\n1319\n1326\n1330\n1329\n1331\n1335\n1338\n1360\n1364\n1381\n1389\n1386\n1387\n1388\n1389\n1391\n1395\n1398\n1404\n1405\n1408\n1412\n1427\n1410\n1425\n1409\n1410\n1412\n1411\n1394\n1406\n1407\n1404\n1413\n1418\n1417\n1442\n1443\n1444\n1445\n1458\n1452\n1453\n1466\n1465\n1466\n1465\n1476\n1484\n1487\n1490\n1494\n1495\n1502\n1498\n1501\n1496\n1498\n1492\n1501\n1499\n1477\n1480\n1483\n1484\n1483\n1488\n1494\n1491\n1489\n1503\n1535\n1544\n1547\n1549\n1550\n1557\n1542\n1540\n1542\n1543\n1544\n1542\n1541\n1546\n1542\n1540\n1526\n1534\n1536\n1549\n1542\n1552\n1553\n1554\n1569\n1568\n1569\n1563\n1560\n1572\n1568\n1544\n1552\n1559\n1563\n1556\n1577\n1594\n1596\n1581\n1580\n1570\n1552\n1560\n1561\n1558\n1559\n1560\n1561\n1560\n1554\n1562\n1565\n1561\n1570\n1571\n1572\n1568\n1565\n1574\n1616\n1607\n1604\n1606\n1609\n1607\n1608\n1609\n1618\n1621\n1622\n1623\n1628\n1637\n1648\n1656\n1653\n1649\n1651\n1654\n1693\n1697\n1705\n1706\n1705\n1712\n1709\n1708\n1715\n1717\n1721\n1723\n1730\n1729\n1740\n1732\n1733\n1740\n1746\n1747\n1744\n1748\n1746\n1745\n1747\n1749\n1750\n1744\n1747\n1749\n1751\n1749\n1751\n1782\n1783\n1767\n1769\n1799\n1777\n1783\n1784\n1786\n1790\n1791\n1802\n1804\n1807\n1824\n1825\n1840\n1852\n1872\n1874\n1871\n1866\n1861\n1860\n1861\n1842\n1840\n1841\n1842\n1841\n1832\n1848\n1851\n1834\n1846\n1844\n1826\n1829\n1840\n1841\n1844\n1848\n1820\n1831\n1833\n1822\n1814\n1819\n1833\n1832\n1824\n1814\n1815\n1818\n1829\n1832\n1833\n1847\n1846\n1848\n1850\n1880\n1889\n1887\n1888\n1881\n1880\n1883\n1885\n1882\n1881\n1868\n1870\n1855\n1862\n1863\n1870\n1866\n1875\n1894\n1896\n1880\n1898\n1917\n1918\n1914\n1919\n1907\n1905\n1893\n1901\n1899\n1909\n1910\n1918\n1921\n1930\n1929\n1935\n1917\n1919\n1917\n1907\n1911\n1910\n1911\n1930\n1958\n1964\n1971\n1972\n1950\n1963\n1967\n1968\n1965\n1973\n1975\n1965\n1963\n1949\n1950\n1953\n1952\n1957\n1974\n1992\n2022\n2021\n2022\n2025\n2024\n2032\n2048\n2074\n2068\n2072\n2068\n2085\n2091\n2111\n2095\n2091\n2089\n2103\n2107\n2117\n2123\n2128\n2136\n2134\n2140\n2141\n2140\n2141\n2122\n2125\n2129\n2142\n2106\n2101\n2104\n2122\n2131\n2156\n2178\n2187\n2182\n2180\n2177\n2178\n2179\n2197\n2198\n2197\n2198\n2202\n2204\n2210\n2211\n2212\n2207\n2206\n2211\n2216\n2209\n2210\n2202\n2207\n2206\n2207\n2210\n2213\n2185\n2187\n2188\n2203\n2206\n2213\n2233\n2238\n2249\n2250\n2252\n2253\n2240\n2246\n2248\n2235\n2237\n2255\n2257\n2235\n2245\n2240\n2246\n2237\n2255\n2282\n2283\n2291\n2304\n2305\n2306\n2309\n2319\n2321\n2323\n2318\n2317\n2316\n2317\n2323\n2314\n2326\n2313\n2314\n2316\n2317\n2320\n2317\n2326\n2322\n2348\n2349\n2335\n2331\n2338\n2343\n2344\n2345\n2335\n2332\n2339\n2340\n2335\n2346\n2347\n2343\n2342\n2344\n2333\n2326\n2351\n2355\n2363\n2366\n2364\n2393\n2410\n2411\n2433\n2440\n2413\n2407\n2403\n2404\n2407\n2427\n2425\n2430\n2446\n2449\n2450\n2448\n2453\n2454\n2456\n2455\n2456\n2455\n2460\n2451\n2464\n2463\n2468\n2471\n2472\n2456\n2467\n2471\n2481\n2505\n2524\n2534\n2533\n2531\n2526\n2527\n2522\n2534\n2535\n2538\n2537\n2538\n2541\n2542\n2544\n2543\n2529\n2519\n2508\n2522\n2526\n2554\n2560\n2554\n2558\n2562\n2576\n2580\n2581\n2589\n2573\n2596\n2600\n2601\n2606\n2607\n2611\n2606\n2624\n2631\n2634\n2663\n2661\n2663\n2676\n2680\n2678\n2680\n2677\n2672\n2685\n2716\n2713\n2690\n2698\n2699\n2716\n2720\n2719\n2720\n2724\n2722\n2724\n2725\n2727\n2733\n2736\n2739\n2740\n2753\n2762\n2763\n2793\n2808\n2787\n2778\n2782\n2771\n2783\n2795\n2803\n2808\n2832\n2833\n2842\n2843\n2839\n2840\n2807\n2808\n2806\n2825\n2836\n2838\n2842\n2848\n2817\n2818\n2821\n2820\n2844\n2859\n2830\n2824\n2825\n2850\n2853\n2852\n2830\n2835\n2839\n2828\n2827\n2828\n2826\n2827\n2815\n2816\n2819\n2818\n2792\n2789\n2793\n2801\n2778\n2779\n2780\n2783\n2808\n2824\n2792\n2811\n2810\n2777\n2774\n2773\n2782\n2781\n2790\n2785\n2789\n2785\n2786\n2785\n2782\n2783\n2785\n2788\n2785\n2790\n2816\n2834\n2835\n2836\n2842\n2846\n2839\n2861\n2863\n2868\n2867\n2864\n2849\n2850\n2851\n2852\n2826\n2815\n2814\n2820\n2821\n2837\n2865\n2866\n2851\n2856\n2861\n2867\n2874\n2871\n2872\n2861\n2842\n2843\n2840\n2872\n2879\n2873\n2881\n2877\n2880\n2873\n2872\n2902\n2906\n2900\n2899\n2911\n2914\n2924\n2925\n2920\n2909\n2901\n2924\n2936\n2938\n2947\n2950\n2962\n2963\n2955\n2953\n2970\n2967\n2947\n2958\n2961\n2962\n2952\n2953\n2951\n2947\n2945\n2952\n2956\n2959\n2961\n2960\n2965\n2966\n2962\n2964\n2972\n2975\n2985\n2982\n2996\n2997\n2991\n2994\n2995\n2994\n2978\n2987\n2994\n3003\n2994\n2993\n2998\n3000\n3004\n3005\n3006\n3024\n3046\n3049\n3053\n3052\n3064\n3067\n3065\n3067\n3068\n3063\n3065\n3067\n3063\n3066\n3068\n3079\n3085\n3086\n3088\n3090\n3093\n3119\n3144\n3143\n3144\n3146\n3159\n3160\n3162\n3164\n3160\n3158\n3143\n3138\n3135\n3140\n3145\n3137\n3134\n3140\n3141\n3135\n3134\n3133\n3123\n3124\n3123\n3124\n3126\n3125\n3109\n3105\n3106\n3120\n3100\n3112\n3114\n3122\n3121\n3120\n3131\n3140\n3141\n3152\n3153\n3154\n3155\n3162\n3160\n3161\n3177\n3190\n3191\n3192\n3194\n3196\n3190\n3187\n3199\n3193\n3200\n3199\n3214\n3212\n3175\n3150\n3153\n3177\n3181\n3165\n3183\n3185\n3187\n3188\n3187\n3201\n3203\n3202\n3200\n3209\n3218\n3217\n3216\n3208\n3204\n3203\n3179\n3185\n3188\n3195\n3196\n3193\n3194\n3211\n3216\n3208\n3210\n3211\n3212\n3204\n3205\n3209\n3232\n3234\n3255\n3254\n3264\n3267\n3269\n3274\n3278\n3290\n3301\n3303\n3305\n3306\n3304\n3305\n3302\n3307\n3308\n3297\n3302\n3307\n3306\n3307\n3306\n3274\n3275\n3276\n3278\n3281\n3280\n3274\n3299\n3298\n3276\n3275\n3278\n3281\n3284\n3285\n3281\n3282\n3244\n3239\n3240\n3217\n3236\n3238\n3240\n3245\n3248\n3255\n3256\n3260\n3261\n3271\n3289\n3302\n3303\n3304\n3305\n3304\n3306\n3307\n3334\n3338\n3341\n3340\n3360\n3354\n3353\n3366\n3369\n3374\n3377\n3368\n3361\n3378\n3385\n3384\n3382\n3386\n3384\n3388\n3381\n3358\n3359\n3363\n3364\n3365\n3366\n3368\n3392\n3419\n3436\n3438\n3452\n3479\n3480\n3483\n3494\n3500\n3511\n3524\n3535\n3560\n3571\n3584\n3600\n3598\n3591\n3593\n3598\n3599\n3598\n3599\n3610\n3608\n3610\n3611\n3615\n3616\n3620\n3606\n3622\n3625\n3629\n3636\n3635\n3640\n3635\n3636\n3634\n3635\n3652\n3654\n3655\n3657\n3642\n3647\n3654\n3661\n3621\n3620\n3621\n3628\n3604\n3597\n3598\n3587\n3588\n3591\n3595\n3594\n3596\n3597\n3598\n3603\n3604\n3605\n3632\n3627\n3630\n3631\n3632\n3642\n3643\n3644\n3649\n3653\n3652\n3653\n3654\n3653\n3669\n3688\n3696\n3697\n3704\n3707\n3710\n3704\n3718\n3721\n3725\n3751\n3757\n3760\n3776\n3775\n3782\n3778\n3792\n3804\n3818\n3819\n3806\n3810\n3795\n3806\n3821\n3823\n3858\n3860\n3858\n3865\n3868\n3875\n3876\n3878\n3888\n3891\n3893\n3884\n3889\n3897\n3891\n3894\n3896\n3924\n3925\n3926\n3927\n3934\n3938\n3966\n3972\n3973\n3977\n3976\n4003\n4017\n4023\n4019\n4017\n3988\n3997\n4010\n4017\n4018\n3994\n3995\n3985\n3987\n3997\n4004\n4003\n4005\n4009\n4007\n4021\n4023\n3998\n3991\n4021\n4020\n4027\n4026\n4030\n4043\n4047\n4048\n4046\n4045\n4046\n4059\n4050\n4064\n4065\n4077\n4083\n4087\n4079\n4087\n4082\n4083\n4111\n4112\n4122\n4123\n4126\n4129\n4138\n4139\n4172\n4160\n4135\n4136\n4147\n4182\n4181\n4180\n4184\n4188\n4191\n4194\n4195\n4176\n4185\n4180\n4187\n4188\n4189\n4190\n4211\n4205\n4208\n4220\n4236\n4239\n4240\n4243\n4240\n4269\n4275\n4274\n4272\n4282\n4281\n4286\n4285\n4248\n4247\n4250\n4249\n4244\n4272\n4265\n4269\n4295\n4280\n4279\n4273\n4256\n4245\n4247\n4248\n4228\n4229\n4231\n4245\n4259\n4261\n4268\n4270\n4272\n4276\n4305\n4322\n4325\n4333\n4329\n4330\n4331\n4328\n4332\n4340\n4367\n4363\n4375\n4399\n4408\n4434\n4412\n4420\n4421\n4422\n4433\n4416\n4430\n4434\n4437\n4438\n4445\n4446\n4448\n4450\n4465\n4468\n4482\n4484\n4461\n4462\n4461\n4462\n4464\n4461\n4464\n4465\n4470\n4478\n4480\n4461\n4471\n4464\n4462\n4463\n4464\n4465\n4454\n4457\n4455\n4456\n4467\n4480\n4490\n4491\n4499\n4500\n4517\n4524\n4540\n4551\n4552\n4559\n4560\n4559\n4565\n4558\n4557\n4562\n4568\n4573\n4578\n4557\n4573\n4574\n4572\n4588\n4613\n4614\n4600\n4602\n4601\n4598\n4597\n4601\n4611\n4612\n4613\n4621\n4622\n4645\n4651\n4650\n4663\n4660\n4657\n4660\n4667\n4666\n4669\n4666\n4654\n4657\n4669\n4672\n4651\n4652\n4655\n4653\n4654\n4669\n4670\n4671\n4672\n4673\n4675\n4681\n4682\n4687\n4691\n4692\n4688\n4687\n4713\n4725\n4719\n4720\n4730\n4728\n4725\n4720\n4721\n4722\n4714\n4716\n4724\n4707\n4716\n4707\n4706\n4707\n4708\n4709\n4710\n4711\n4726\n4694\n4701\n4697\n4698\n4678\n4679\n4680\n4670\n4661\n4677\n4678\n4660\n4649\n4650\n4653\n4655\n4650\n4657\n4658\n4660\n4663\n4686\n4691\n4686\n4676\n4658\n4656\n4655\n4657\n4656\n4655\n4670\n4661\n4658\n4659\n4663\n4664\n4673\n4691\n4694\n4696\n4698\n4702\n4706\n4714\n4709\n4707\n4708\n4719\n4725\n4744\n4754\n4782\n4778\n4808\n4826\n4830\n4838\n4841\n4848\n4855\n4866\n4862\n4865\n4852\n4881\n4873\n4875\n4874\n4876\n4869\n4866\n4870\n4867\n4866\n4883\n4912\n4918\n4938\n4960\n4948\n4956\n4961\n4978\n4993\n4994\n4998\n5000\n5001\n5007\n5009\n5015\n5002\n5005\n5039\n5037\n5043\n5044\n5051\n5053\n5057\n5058\n5059\n5058\n5065\n5067\n5066\n5069\n5070\n5077\n5076\n5077\n5078\n5079\n5085\n5081\n5087\n5107\n5122\n5123\n5099\n5066\n5083\n5109\n5105\n5106\n5090\n5112\n5102\n5109\n5108\n5109\n5108\n5109\n5118\n5114\n5117\n5119\n5139\n5144\n5142\n5148\n5151\n5147\n5148\n5160\n5172\n5154\n5156\n5155\n5164\n5155\n5143\n5122\n5147\n5162\n5163\n5151\n5152\n5157\n5163\n5203\n5211\n5222\n5224\n5241\n5239\n5250\n5254\n5250\n5251\n5252\n5256\n5276\n5274\n5246\n5248\n5251\n5250\n5254\n5267\n5285\n5286\n5292\n5290\n5289\n5293\n5294\n5299\n5303\n5305\n5306\n5316\n5317\n5319\n5320\n5326\n5325\n5330\n5329\n5332\n5336\n5338\n5339\n5349\n5353\n5354\n5346\n5348\n5349\n5358\n5359\n5360\n5361\n5377\n5380\n5390\n5391\n5403\n5402\n5405\n5404\n5406\n5405\n5406\n5405\n5406\n5434\n5443\n5444\n5458\n5478\n5484\n5491\n5492\n5504\n5507\n5508\n5514\n5512\n5523\n5529\n5534\n5535\n5558\n5560\n5561\n5534\n5543\n5538\n5537\n5533\n5554\n5558\n5557\n5554\n5590\n5601\n5604\n5623\n5626\n5625")
  (part-1 (parse in))
  (part-2 (parse in))

  )
