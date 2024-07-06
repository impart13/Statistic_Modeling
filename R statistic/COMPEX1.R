#очистка экрана
cat("\014")
options(max.print=1000000)

#SP – полная поверхность тора моделируется для n = 2000
#r= 20 +/- 10%, равномерно распределен,
#R – распределен нормально со средним = 80 и СКО = 10
#Требуется:
#1) рассчитать и построить отъюстированные гистограммы для R и r и SP,
#2)   вычислить вероятность P(SP) попадания SP в интервал от SP- до SP+,
#где SP- = МИН (СРЕДНЕЕ SP; МЕДИАНА SP) – СКО SP,
#SP+ = МАКС (МЕДИАНА SP; СРЕДНЕЕ SP) + СКО SP
#В описании работы привести в явной форме моделируемую формулу

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
n <- 2000
#количество значимых карманов гистограмм
pockets <- 10

r <- runif(n, 18, 22)
R <- rnorm(n, 80, 10)

#Выведем r:
print(r)
#Выведем R:
print(R)

#Вычисляем поверхности:
#SP=4*Pi^2*R*r
SP <- rep(0, n)
for (i in 1:n)
  SP[i] = 4 * pi^2*R[i]*r[i]

#Выведем поверхности:
print(SP)

#Высчитаем интервал и вероятность попадания в него
mean(SP)
sd(SP)

sMinus <- min(c(median(SP), mean(SP))) - sqrt(var(SP))
sPlus <- max(c(median(SP), mean(SP))) + sqrt(var(SP))
belowMinus <- length(SP[SP < sMinus])
abovePlus <- length(SP[SP > sPlus])
p <- (n - belowMinus - abovePlus) / n
cat("Интервал: [", sMinus, "; ", sPlus, "]")
cat("Меньше нижней границы: ", belowMinus, " чисел;")
cat("Больше верхней границы: ", abovePlus, "чисел;")
cat("Вероятность попадания в интервал: ", p)

#Гистограммы:
par(mfrow=c(2,2))
freqs = histAndInfo(r, pockets, 18, 22,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот r",
                    color = "Red"
)
freqs = histAndInfo(R, pockets, 80 - 3*10 + 5, 80 + 3*10 - 5,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот R",
                    color = "Yellow"
)
freqs = histAndInfo(SP, pockets, 40500, 86500,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот SP",
                    color = "Green"
)
