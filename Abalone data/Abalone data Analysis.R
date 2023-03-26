### CS910 Assessed Exercise 3

aba<-read.csv(file.choose(), header=FALSE)
head(aba)
str(aba)
summary(aba)
class(aba)
attach(aba)

## Question 1
fit1 <- lm(aba$V3 ~ aba$V2, data = aba)
print(fit1)
summary(fit1)
cor(aba$V3,aba$V2)
cor(aba$V3,aba$V2)**2
plot(aba$V2, aba$V3, xlab = "Length", ylab = "Diameter", pch = 1, cex = .5, main = "Linear model: Diameter against Length") 
abline(fit1, col = "red", lwd = 3)

## Question 2
fit2 <- lm(aba$V5 ~ aba$V6 + aba$V7 + aba$V8, data = aba)
print(fit2)
summary(fit2)
pairs(aba$V5 ~ aba$V6 + aba$V7 + aba$V8, pch = 1, cex = .5, main = "Whole weight against weight of pieces") 
cor(aba$V5,aba$V6+aba$V7+aba$V8)
cor(aba$V5,aba$V6+aba$V7+aba$V8)**2

## Question 3

par(mfrow=c(2,2))
fit3 <- lm(aba$V5 ~ aba$V3)
plot(aba$V3, aba$V5, xlab = "Diameter", ylab = "Whole weight", pch = 1, cex = .5, main = "Linear model") 
print(fit3)
cor(aba$V5, aba$V3)
cor(aba$V5, aba$V3)**2
abline(fit3, col = "red", lwd = 3)
summary(fit3)

fit4 <- lm(aba$V5 ~ aba$V3 + I(aba$V3^2), data = aba)
plot(aba$V3, aba$V5, xlab = "Diameter", ylab = "Whole weight", pch = 1, cex = .5, main = "Quadratic model") 
print(fit4)
summary(fit4)
cor(aba$V5, aba$V3 + I(aba$V3^2))
cor(aba$V5, aba$V3 + I(aba$V3^2))**2
curve(-3.3555*x + 10.4968*(x**2)+0.3477, col = "red", lwd = 3, add = TRUE)

fit5 <- lm(aba$V5 ~ I(aba$V3^3)-1, data = aba)
plot(aba$V3, aba$V5, xlab = "Diameter", ylab = "Whole weight", pch = 1, cex = .5, main = "Cubic model") 
print(fit5)
summary(fit5)
cor(aba$V5, I(aba$V3^3))
cor(aba$V5, I(aba$V3^3))^2
curve(10.34*(x**3), col = "red", lwd = 3, add = TRUE)

fit6 <- lm(log(aba$V5) ~ aba$V3, data = aba)
plot(aba$V3, aba$V5, xlab = "Diameter", ylab = "Whole weight", pch = 1, cex = .5, main = "Exponential model") 
print(fit6)
summary(fit6)
cor(log(aba$V5), aba$V3)
cor(log(aba$V5), aba$V3)**2
curve(exp(8.117*x-3.751), col = "red", lwd = 3, add = TRUE)

## Question 4
library(dummies)
abasex <- data.frame(dummy(aba$V1), data = aba)
aba$A <- abasex$V1.F + abasex$V1.M

fit7 <- glm(aba$A ~ aba$V2, family="binomial")
summary(fit7)
print(fit7)
pairs <- paste(round(predict(fit7, type = "response")), aba$A)
table(pairs) 
summary(aba$A)

fit8 <- glm(aba$A ~ aba$V5, family="binomial")
summary(fit8)
print(fit8)
pairs <- paste(round(predict(fit8, type = "response")), aba$A)
table(pairs) 
summary(aba$A)

fit9 <- glm(aba$A ~ aba$V9, family="binomial")
summary(fit9)
print(fit9)
pairs <- paste(round(predict(fit9, type = "response")), aba$A)
table(pairs) 
summary(aba$A)

fit10 <- glm(aba$A ~ aba$V2 + aba$V5 + aba$V9, family="binomial")
summary(fit10)
print(fit10)
pairs <- paste(round(predict(fit10, type = "response")), aba$A)
table(pairs) 
summary(aba$A)

