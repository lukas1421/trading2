#FIRST BREAK
latest[, firstBreak:=which(c(log(H1300/pmOpen),log(H1301/pmOpen),log(H1302/pmOpen),log(H1303/pmOpen),log(H1304/pmOpen),log(H1305/pmOpen),log(H1306/pmOpen),log(H1307/pmOpen),log(H1308/pmOpen),log(H1309/pmOpen),log(H1310/pmOpen),log(H1311/pmOpen),log(H1312/pmOpen),log(H1313/pmOpen),log(H1314/pmOpen),log(H1315/pmOpen),log(H1316/pmOpen),log(H1317/pmOpen),log(H1318/pmOpen),log(H1319/pmOpen),log(H1320/pmOpen),log(H1321/pmOpen),log(H1322/pmOpen),log(H1323/pmOpen),log(H1324/pmOpen),log(H1325/pmOpen),log(H1326/pmOpen),log(H1327/pmOpen),log(H1328/pmOpen),log(H1329/pmOpen),log(H1330/pmOpen),log(H1331/pmOpen),log(H1332/pmOpen),log(H1333/pmOpen),log(H1334/pmOpen),log(H1335/pmOpen),log(H1336/pmOpen),log(H1337/pmOpen),log(H1338/pmOpen),log(H1339/pmOpen),log(H1340/pmOpen),log(H1341/pmOpen),log(H1342/pmOpen),log(H1343/pmOpen),log(H1344/pmOpen),log(H1345/pmOpen),log(H1346/pmOpen),log(H1347/pmOpen),log(H1348/pmOpen),log(H1349/pmOpen),log(H1350/pmOpen),log(H1351/pmOpen),log(H1352/pmOpen),log(H1353/pmOpen),log(H1354/pmOpen),log(H1355/pmOpen),log(H1356/pmOpen),log(H1357/pmOpen),log(H1358/pmOpen),log(H1359/pmOpen)
                             ,log(H1400/pmOpen),log(H1401/pmOpen),log(H1402/pmOpen),log(H1403/pmOpen),log(H1404/pmOpen),log(H1405/pmOpen),log(H1406/pmOpen),log(H1407/pmOpen),log(H1408/pmOpen),log(H1409/pmOpen),log(H1410/pmOpen),log(H1411/pmOpen),log(H1412/pmOpen),log(H1413/pmOpen),log(H1414/pmOpen),log(H1415/pmOpen),log(H1416/pmOpen),log(H1417/pmOpen),log(H1418/pmOpen),log(H1419/pmOpen),log(H1420/pmOpen),log(H1421/pmOpen),log(H1422/pmOpen),log(H1423/pmOpen),log(H1424/pmOpen),log(H1425/pmOpen),log(H1426/pmOpen),log(H1427/pmOpen),log(H1428/pmOpen),log(H1429/pmOpen),log(H1430/pmOpen),log(H1431/pmOpen),log(H1432/pmOpen),log(H1433/pmOpen),log(H1434/pmOpen),log(H1435/pmOpen),log(H1436/pmOpen),log(H1437/pmOpen),log(H1438/pmOpen),log(H1439/pmOpen),log(H1440/pmOpen),log(H1441/pmOpen),log(H1442/pmOpen),log(H1443/pmOpen),log(H1444/pmOpen),log(H1445/pmOpen),log(H1446/pmOpen),log(H1447/pmOpen),log(H1448/pmOpen),log(H1449/pmOpen),log(H1450/pmOpen),log(H1451/pmOpen),log(H1452/pmOpen),log(H1453/pmOpen),log(H1454/pmOpen),log(H1455/pmOpen),log(H1456/pmOpen),log(H1457/pmOpen),log(H1458/pmOpen),log(H1459/pmOpen),log(H1500/pmOpen)
                             ) > amRange*1.11)[1],keyby=list(Date)]

#FIRST BREAK TIME
latest[, firstBreakTime:=c("H1300","H1301","H1302","H1303","H1304","H1305","H1306","H1307","H1308","H1309","H1310","H1311","H1312","H1313","H1314","H1315","H1316","H1317","H1318","H1319","H1320","H1321","H1322","H1323","H1324","H1325","H1326","H1327","H1328","H1329","H1330","H1331","H1332","H1333","H1334","H1335","H1336","H1337","H1338","H1339","H1340","H1341","H1342","H1343","H1344","H1345","H1346","H1347","H1348","H1349","H1350","H1351","H1352","H1353","H1354","H1355","H1356","H1357","H1358","H1359","H1400","H1401","H1402","H1403","H1404","H1405","H1406","H1407","H1408","H1409","H1410","H1411","H1412","H1413","H1414","H1415","H1416","H1417","H1418","H1419","H1420","H1421","H1422","H1423","H1424","H1425","H1426","H1427","H1428","H1429","H1430","H1431","H1432","H1433","H1434","H1435","H1436","H1437","H1438","H1439","H1440","H1441","H1442","H1443","H1444","H1445","H1446","H1447","H1448","H1449","H1450","H1451","H1452","H1453","H1454","H1455","H1456","H1457","H1458","H1459","H1500"
)[firstBreak], keyby=list(Date)]


#first break cat
latest[, firstBreakCat:=cut(firstBreak,quantile(firstBreak,na.rm = T,probs = seq(0,1,0.1)),include.lowest = T)]

#checks
latest[retAMCO>0 & retPMHO>amRange*1.11, list(retAMCO,amMaxT1,amMinT1,amRange,firstBreak,firstBreakTime,get(firstBreakTime),pmOpen,log(get(firstBreakTime)/pmOpen),log(get(firstBreakTime)/pmOpen)/amRange,log(get(firstBreakTimeMinus1)/pmOpen)/amRange, firstBreakCat  ), keyby=list(Date)]


latest[retAMCO>0 & retPMHO>amRange*1.11, list(retAMCO,firstBreak,retPMHO,retPMCO-amRange*1.11,weekday, amRange) ,keyby=list(Date)][, calcSharp(V4), keyby=list(year(Date))]

latest[,first10Max:=c("H931","H932","H933","H934","H935","H936","H937","H938","H939","H940")[which.max(c(H931,H932,H933,H934,H935,H936,H937,H938,H939,H940))] ,keyby=list(Date)]
latest[,first10Min:=c("L931","L932","L933","L934","L935","L936","L937","L938","L939","L940")[which.min(c(L931,L932,L933,L934,L935,L936,L937,L938,L939,L940))] ,keyby=list(Date)]

latest[,pmFirst10Max:=c("H1301","H1302","H1303","H1304","H1305","H1306","H1307","H1308","H1309","H1310"
)[which.max(c(H1301,H1302,H1303,H1304,H1305,H1306,H1307,H1308,H1309,H1310
))] ,keyby=list(Date)]

latest[,pmFirst10Min:=c("L1301","L1302","L1303","L1304","L1305","L1306","L1307","L1308","L1309","L1310"
)[which.min(c(L1301,L1302,L1303,L1304,L1305,L1306,L1307,L1308,L1309,L1310
))] ,keyby=list(Date)]

latest[, pmFirst10MaxNum := as.numeric(str_sub(pmFirst10Max,2,5)), keyby=list(Date)]
latest[, pmFirst10MinNum := as.numeric(str_sub(pmFirst10Min,2,5)), keyby=list(Date)]
latest[, pmFirst10MaxMinDiff:=pmFirst10MaxNum-pmFirst10MinNum]
latest[, pmFirst10MaxMinDiffCat:=cut(pmFirst10MaxMinDiff, quantile(pmFirst10MaxMinDiff),include.lowest = T)]


