#очистка экрана
cat("\014")
options(max.print=1000000)

#функция принимает на вход:
#   sqnc - исследуемую последовательность, 
#   pckts - количество значимых карманов,
#   lBound - начало первого значимого кармана,
#   rBound - конец последнего значимого кармана,
#   xAxisName - имя оси Х ("Х" по умолчанию),
#   yAxisName - имя оси Y ("Y" по умолчанию),
#   mainName - имя гистограммы ("Histogram" по умолчанию),
#   color - цвет, которым закрашивать гистограмму ("Red" по умолчанию);
#вычисляет центры карманов, их границы и частоты попадания в них элементов sequence;
#эти данные выводятся на экран в виде таблицы, затем строится гистограмма;
#в гистограмме и таблице присутствуют два дополнительных кармана:
#в первои - количество элементов, меньших lBound, в последнем - количество элементов, превосходящих rBound;
#функция возвращает:
#   frequency - вектор частот попадания элементов sequence карманы.

histAndInfo <- function(sqnc, pckts, lBound, rBound, xAxisName = "X", yAxisName = "Y", mainName = "Histogram", color = "Red") {
  #юстируем
  step = (rBound-lBound)/ pckts
  pocketBounds = seq(lBound,rBound + step, by = step)
  pocketCentres = seq(lBound - step/2, rBound + step/2, by = step)
  frequency = seq(0,0,length.out=pckts + 2)
  
  frequency[1] = length(sqnc[sqnc < pocketBounds[1]])
  for(i in 2:(pckts + 1))
    frequency[i] = length(sqnc[sqnc >= pocketBounds[i-1] & sqnc < pocketBounds[i]])
  frequency[pckts + 2] = length(sqnc[sqnc >= pocketBounds[pckts + 1]])
  
  #выводим данные для юстировки
  dataframe=data.frame(pocketCentres, pocketBounds, frequency)
  print(dataframe)
  cat("Всего подсчитано: ", sum(frequency), "\n")
  
  #строим гистограмму
  barplot(frequency, col = color,
          names.arg = round(pocketCentres, 2),
          xlab = xAxisName,
          ylab = yAxisName,
          main = mainName
  )
  
  return (frequency)
}

#размер выброки
n <- 1500
#количество значимых карманов гистограмм
pockets <- 12

R1 <- runif(n, 9, 11)
R2 <- rnorm(n, 30, 5)
R3 <- runif(n, 40, 60)

#Выведем R1:
print(R1)
#Выведем R2:
print(R2)
#Выведем R3:
print(R3)


#Вычисляем поверхности:
#R = (R1*R3 + R1*R2 + R2*R3)/(R1*R2*R3)
R <- rep(0, n)
for (i in 1:n)
  R[i] = (R1[i] * R3[i] + R1[i] * R2[i] + R2[i] * R3[i]) / (R1[i] * R2[i] * R3[i])

#Выведем поверхности:
print(R)

#Высчитаем интервал и вероятность попадания в него
mean(R)
sd(R)

RMinus <- min(c(median(R), mean(R))) - sqrt(var(R))
RPlus <- max(c(median(R), mean(R))) + sqrt(var(R))
belowMinus <- length(R[R < VMinus])
abovePlus <- length(R[R > RPlus])
p <- (n - belowMinus - abovePlus) / n
cat("Интервал: [", RMinus, "; ", RPlus, "]")
cat("Меньше нижней границы: ", belowMinus, " чисел;")
cat("Больше верхней границы: ", abovePlus, "чисел;")
cat("Вероятность попадания в интервал: ", p)

#Гистограммы:
par(mfrow=c(2,2))
freqs = histAndInfo(R1, pockets, 9, 11,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот R1",
                    color = "Blue"
)
freqs = histAndInfo(R2, pockets, 30 - 3*5 + 5, 30 + 3*5 - 5,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот R2",
                    color = "Red"
)
freqs = histAndInfo(R3, pockets, 40, 60,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот R3",
                    color = "Yellow"
)
freqs = histAndInfo(R, pockets, 0.12, 0.21,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот R",
                    color = "Green"
)
