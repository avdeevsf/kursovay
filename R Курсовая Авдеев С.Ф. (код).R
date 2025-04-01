install.packages("lmtest")
library("lmtest")
install.packages("pastecs")
library("pastecs")
install.packages("psych")
library(psych)
install.packages("moments")
library(moments)


#основные описательные статистики
summary(x)
skewness(Base1$y)
skewness(Base1$x1)
skewness(Base1$x2)
skewness(Base1$x3)
skewness(Base1$x4)
skewness(Base1$x5)
kurtosis(Base1$y)
kurtosis(Base1$x1)
kurtosis(Base1$x2)
kurtosis(Base1$x3)
kurtosis(Base1$x4)
kurtosis(Base1$x5)
sd(Base1$y)/mean(Base1$y) * 100
sd(Base1$x1)/mean(Base1$x1) * 100
sd(Base1$x2)/mean(Base1$x2) * 100
sd(Base1$x3)/mean(Base1$x3) * 100
sd(Base1$x4)/mean(Base1$x4) * 100
sd(Base1$x5)/mean(Base1$x5) * 100
shapiro.test(Base1$y)
shapiro.test(Base1$x1)
shapiro.test(Base1$x2)
shapiro.test(Base1$x3)
shapiro.test(Base1$x4)
shapiro.test(Base1$x5)

# Гистограммы распределения показателей

hist(Base1$y, xlab = "Валютные вклады", main = "Гистограмма распределения переменной Валютные вклады")

hist(Base1$x1, xlab = "Среднедушевые доходы", main = "Гистограмма распределения переменной Среднедушевые доходы")

hist(Base1$x2, xlab = "Безработица", main = "Гистограмма распределения переменной Безработица")

hist(Base1$x3, xlab = "Выпуск бакалавров, специалистов, Магистров 2019 год", main = "Гистограмма распределения переменной Выпуск студентов")

hist(Base1$x4, xlab = "Продажа алкогольной продукции населению", main = "Гистограмма распределения переменной Продажа алкогольной продукции населению")

hist(Base1$x5, xlab = "Выдача патентов", main = "Гистограмма распределения переменной Выдача патентов")

#построение корреляционной матрицы

X <- Base1[ ,2:7]

View(round(x = cor(X), digits = 2))

#построение графиков регрессии

plot(Base2$x3,Base1$y, xlab = "Выпуск бакалавров, специалистов, магистров за 2019 год", ylab =
       "Стоимость валютных вкладов", main = "График разброса стоимости валютных вкладов от 
     выпуска бакалавров, специалистов, магистров за 2019 год",
     abline(lm(Base2$y ~ Base2$x3), col = 'blue'))

plot(Base2$x5,Base1$y, xlab = "Выдача патентов", ylab =
       "Стоимость валютных вкладов", main = "График разброса стоимости валютных вкладов от 
     выдачи патентов",
     abline(lm(Base2$y ~ Base2$x5), col = 'blue'))

plot(Base2$x2,Base1$y, xlab = "Безработица, в %", ylab =
       "Стоимость валютных вкладов", main = "График разброса стоимости валютных вкладов от 
     безработицы",
     abline(lm(Base2$y ~ Base2$x2), col = 'blue'))

#Построение модели 1
Base1 <- read.csv(file = file.choose(), header = TRUE, sep = ";", dec = ",")
m1 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data=Base1)
summary(m1)

#Построение модели 2
Base2 <- read.csv(file = file.choose(), header = TRUE, sep = ";", dec = ",")
m2 <- lm(y ~ x1 + x3 + x4 + x5, data=Base2)
summary(m2)

#модель с фиктивной перменной в аддитивном виде
m3 <- lm(y ~ x1 + x3 + x4 + x5 + dummy, data = Base2)
summary(m3)

#модели с фиктивной перменной в мультипликативном виде
m5 <- lm(y ~ x1 + x3 + x4 + x5 + dummy*x1, data = Base2)
m6 <- lm(y ~ x1 + x3 + x4 + x5 + dummy*x3, data = Base2)
m7 <- lm(y ~ x1 + x3 + x4 + x5 + dummy*x4, data = Base2)
m8 <- lm(y ~ x1 + x3 + x4 + x5 + dummy*x5, data = Base2)

summary(m6)
summary(m7)
summary(m8)

waldtest(m2, m3)
waldtest(m2, m5)
waldtest(m2, m6)
waldtest(m2, m7)
waldtest(m2, m8)

#построение модели с лог-линейной формой
m4 <- lm(log(y) ~ x2 + x3 + x4 + x5, data = Base2)
stargazer::stargazer(m4, type = "text")
summary(m4)

#Проверка теоремы Гаусса-Маркова и сравнение моделей

#первое условие
mean(m1$residuals)
mean(m2$residuals)
mean(m3$residuals)
mean(m4$residuals)

#второе условие
bptest(m1, studentize = FALSE)
bptest(m2, studentize = FALSE)
bptest(m3, studentize = FALSE)
bptest(m4, studentize = FALSE)

#третье условие
bgtest(m1)
bgtest(m2)
bgtest(m3)
bgtest(m4)

#четвертое условие
shapiro.test(m1$residuals)
shapiro.test(m2$residuals)
shapiro.test(m3$residuals)
shapiro.test(m4$residuals)

#критерий Шварца и Акайка
BIC(m1)
BIC(m2)
BIC(m3)
BIC(m4)

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)

#Тест Песарано
petest(m3, m4)


