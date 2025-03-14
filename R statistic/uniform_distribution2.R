#?????????????? ?????????????????? ???? ????????:
# sqnc - ?????????????????????? ????????????????????????????????????,
# pckts - ???????????????????? ???????????????? ????????????????,
# lBound - ???????????? ?????????????? ?????????????????? ??????????????,
# rBound - ?????????? ???????????????????? ?????????????????? ??????????????,
# xAxisName - ?????? ?????? ?? ("??" ???? ??????????????????),
# yAxisName - ?????? ?????? Y ("Y" ???? ??????????????????),
# mainName - ?????? ?????????????????????? ("Histogram" ???? ??????????????????),
# color - ????????, ?????????????? ?????????????????????? ?????????????????????? ("Red" ???? ??????????????????);
#?????????????????? ???????????? ????????????????, ???? ?????????????? ?? ?????????????? ?????????????????? ?? ?????? ?????????????????? sequence;
#?????? ???????????? ?????????????????? ???? ?????????? ?? ???????? ??????????????, ?????????? ???????????????? ??????????????????????;
#?? ?????????????????????? ?? ?????????????? ???????????????????????? ?????? ???????????????????????????? ??????????????:
#?? ???????????? - ???????????????????? ??????????????????, ?????????????? lBound, ?? ?????????????????? - ???????????????????? ??????????????????, ?????????????????????????? rBound;
#?????????????? ????????????????????:
# frequency - ???????????? ???????????? ?????????????????? ?????????????????? sequence ??????????????.
histAndInfo <- function(sqnc, pckts, lBound, rBound, xAxisName = "X", yAxisName = "Y", mainName = "Histogram", color = "Red") {
  #????????????????
  step = (rBound-lBound)/ pckts
  pocketBounds = seq(lBound,rBound + step, by = step)
  pocketCentres = seq(lBound - step/2, rBound + step/2, by = step)
  frequency = seq(0,0,length.out=pckts + 2)
  frequency[1] = length(sqnc[sqnc < pocketBounds[1]])
  for(i in 2:(pckts + 1))
    frequency[i] = length(sqnc[sqnc >= pocketBounds[i-1] & sqnc < pocketBounds[i]])
  frequency[pckts + 2] = length(sqnc[sqnc >= pocketBounds[pckts + 1]])
  #?????????????? ???????????? ?????? ??????????????????
  dataframe=data.frame(pocketCentres, pocketBounds, frequency)
  print(dataframe)
  cat("?????????? ????????????????????: ", sum(frequency), "\n")
  #???????????? ??????????????????????
  barplot(frequency, col = color,
          names.arg = round(pocketCentres, 2),
          xlab = xAxisName,
          ylab = yAxisName,     
          main = mainName 
  )
  return (frequency)
}
#???????????? ??????????????
n <- 1000
#???????????????? ???????????????????? ???????????????? ???????????????????? ???????? 1
pockets <- 12
N <- rnorm(n, 100, 30)
N1 <- rnorm(n, 130, 30)
N2 <- rnorm(n, 100, 30)
N3 <- rnorm(n, 70, 30)
N4 <- rnorm(n, 40, 30)
N5 <- rnorm(n, 10, 30)
#?????????????? ??????????????????????????:
print(N)
#?????????????? ??????????????????????????:
print(N1)
#?????????????? ??????????????????????????:
print(N2)
#?????????????? ??????????????????????????:
print(N3)
#?????????????? ??????????????????????????:
print(N4)
#?????????????? ??????????????????????????:
print(N5)
#?????????????????? ??????????:
#V1 = N + N1
#V2 = N + N2
#V3 = N + N3
#V4 = N + N5
#V5 = N + N5
V1 <- rep(0, n)
V2 <- rep(0, n)
V3 <- rep(0, n)
V4 <- rep(0, n)
V5 <- rep(0, n)
for (i in 1:n)
  V1[i] = N[i]+N1[i]
for (i in 1:n)
  V2[i] = N[i]+N2[i]
for (i in 1:n)
  V3[i] = N[i]+N3[i]
for (i in 1:n)
  V4[i] = N[i]+N4[i]
for (i in 1:n)
  V5[i] = N[i]+N5[i]
#?????????????????? ???????????????? 1:
print(V1)
#?????????????????? ???????????????? 2:
print(V2)
#?????????????????? ???????????????? 3:
print(V3)
#?????????????????? ???????????????? 4:
print(V4)
#?????????????????? ???????????????? 5:
print(V5)
#?????????????? ?????????????????????????? ???????????????????????????? V1:
mn <- mean(V1)
chars <- c(mn, var(V1), sd(V1), median(V1), paste(c(round(mn, digits = 2), " +- ", round(sqrt(var(V1) / n)* 1.96, digits = 2), ", P = 95%"), collapse = ""))
rowNames <- c("??????????????:", "??????????????????:", "??????:", "??????????????:", "??????. ????????????????:")
dataframe=data.frame(chars)
row.names(dataframe) <- rowNames
colnames(dataframe) <- c("?????????????????????????? ???????????????????????????? V1")
dataframe
#?????????????? ?????????????????????????? ???????????????????????????? V2:
mn <- mean(V2)
chars <- c(mn, var(V2), sd(V2), median(V2), paste(c(round(mn, digits = 2), " +- ", round(sqrt(var(V2) / n)* 1.96, digits = 2), ", P = 95%"), collapse = ""))
rowNames <- c("??????????????:", "??????????????????:", "??????:", "??????????????:", "??????. ????????????????:")
dataframe=data.frame(chars)
row.names(dataframe) <- rowNames
colnames(dataframe) <- c("?????????????????????????? ???????????????????????????? V2")
dataframe
#?????????????? ?????????????????????????? ???????????????????????????? V2:
mn <- mean(V3)
chars <- c(mn, var(V3), sd(V3), median(V3), paste(c(round(mn, digits = 2), " +- ", round(sqrt(var(V3) / n)* 1.96, digits = 2), ", P = 95%"), collapse = ""))
rowNames <- c("??????????????:", "??????????????????:", "??????:", "??????????????:", "??????. ????????????????:")
dataframe=data.frame(chars)
row.names(dataframe) <- rowNames
colnames(dataframe) <- c("?????????????????????????? ???????????????????????????? V3")
dataframe
#?????????????? ?????????????????????????? ???????????????????????????? V4:
mn <- mean(V4)
chars <- c(mn, var(V4), sd(V4), median(V4), paste(c(round(mn, digits = 2), " +- ", round(sqrt(var(V4) / n)* 1.96, digits = 2), ", P = 95%"), collapse = ""))
rowNames <- c("??????????????:", "??????????????????:", "??????:", "??????????????:", "??????. ????????????????:")
dataframe=data.frame(chars)
row.names(dataframe) <- rowNames
colnames(dataframe) <- c("?????????????????????????? ???????????????????????????? V4")
dataframe
#?????????????? ?????????????????????????? ???????????????????????????? V5:
mn <- mean(V5)
chars <- c(mn, var(V5), sd(V5), median(V5), paste(c(round(mn, digits = 2), " +- ", round(sqrt(var(V5) / n)* 1.96, digits = 2), ", P = 95%"), collapse = ""))
rowNames <- c("??????????????:", "??????????????????:", "??????:", "??????????????:", "??????. ????????????????:")
dataframe=data.frame(chars)
row.names(dataframe) <- rowNames
colnames(dataframe) <- c("?????????????????????????? ???????????????????????????? V5")
dataframe
#??????????????????????:
freqs1 = histAndInfo(V1, pockets, 75, 405,
                     xAxisName = "???????????? ????????????????",
                     yAxisName = "?????????????? ??????????????????",
                     mainName = "?????????????????????? ?????????????????????????? ???????????? V1",
                     color = "Green"
)
freqs2 = histAndInfo(V2, pockets, 45, 375,
                     xAxisName = "???????????? ????????????????",
                     yAxisName = "?????????????? ??????????????????",
                     mainName = "?????????????????????? ?????????????????????????? ???????????? V2",
                     color = "Red"
)
freqs3 = histAndInfo(V3, pockets, 15, 345,
                     xAxisName = "???????????? ????????????????",
                     yAxisName = "?????????????? ??????????????????",
                     mainName = "?????????????????????? ?????????????????????????? ???????????? V3",
                     color = "Blue"
)
freqs4 = histAndInfo(V4, pockets, -15, 315,
                     xAxisName = "???????????? ????????????????",
                     yAxisName = "?????????????? ??????????????????",
                     mainName = "?????????????????????? ?????????????????????????? ???????????? V4",
                     color = "Brown"
)
freqs5 = histAndInfo(V5, pockets, -45, 275,
                     xAxisName = "???????????? ????????????????",
                     yAxisName = "?????????????? ??????????????????",
                     mainName = "?????????????????????? ?????????????????????????? ???????????? V5",
                     color = "Pink"
)
#???????????????????? ????????????????
means <-c(mean(V1),mean(V2),mean(V3),mean(V4),mean(V5))
vars<- c(var(V1),var(V2),var(V3),var(V4),var(V5))
sds<- c(sd(V1),sd(V2),sd(V3),sd(V4),sd(V5))
medians <-c(median(V1),median(V2),median(V3),median(V4),median(V5))
numbs <-c(1,2,3,4,5)
plot(numbs,means,col="blue",xlab = "?????????? ??????????????????",ylab = "??????????????")
lines(numbs,means)
plot(numbs,vars,col="red",xlab = "?????????? ??????????????????",ylab = "??????????????????")
lines(numbs,vars)
plot(numbs,sds,col="green",xlab = "?????????? ??????????????????",ylab = "??????")
lines(numbs,sds)
plot(numbs,medians,col="brown",xlab = "?????????? ??????????????????",ylab = "??????????????")
lines(numbs,medians)
