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
n <- 750
#количество значимых карманов гистограмм
pockets <- 10

d <- runif(n, 27, 33)
D <- runif(n, 32, 48)
h <- rnorm(n, 50, 10)

#Выведем d:
print(d)
#Выведем D:
print(D)
#Выведем h:
print(h)

#Вычисляем поверхности:
#V=(Pi*h/60)*(8*D^2+4*D*d+3*r^2)
V <- rep(0, n)
for (i in 1:n)
  V[i] = (pi * h[i] /60) * (8 * ((D[i]) ^ 2) + 4 * D[i] * d[i] + 3 * ((r[i]) ^ 2))

#Выведем поверхности:
print(V)

#Высчитаем интервал и вероятность попадания в него
mean(V)
sd(V)

VMinus <- min(c(median(V), mean(V))) - sqrt(var(V))
VPlus <- max(c(median(V), mean(V))) + sqrt(var(V))
belowMinus <- length(V[V < VMinus])
abovePlus <- length(V[V > VPlus])
p <- (n - belowMinus - abovePlus) / n
cat("Интервал: [", VMinus, "; ", VPlus, "]")
cat("Меньше нижней границы: ", belowMinus, " чисел;")
cat("Больше верхней границы: ", abovePlus, "чисел;")
cat("Вероятность попадания в интервал: ", p)

#Гистограммы:
par(mfrow=c(2,2))
freqs = histAndInfo(d, pockets, 27, 33,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот d",
                    color = "Blue"
)
freqs = histAndInfo(D, pockets, 32, 48,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот D",
                    color = "Red"
)
freqs = histAndInfo(h, pockets, 50 - 3*10 + 5, 50 + 3*10 - 5,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот h",
                    color = "Yellow"
)
freqs = histAndInfo(V, pockets, 12100, 100000,
                    xAxisName = "Центры карманов",
                    yAxisName = "Частоты попадания",
                    mainName = "Гистограмма распределения частот V",
                    color = "Green"
)
