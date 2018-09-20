scrip.data <- read.csv("C:/Users/00008020/Desktop/R_projects/data2.csv")

x <- data.matrix(scrip.data[, c(5,7,10)])
y <- data.matrix(scrip.data[, "High.Price"])

fit_lin_glm <- lm(y ~ x)
summary(fit_lin_glm)

pred_fit_ln_glm <- predict(fit_lin_glm, newdata = scrip.data, interval='confidence')

p1 <- data.frame(pred_fit_ln_glm)

p1 <- cbind(p1, Date=scrip.data$Date)
p1 <- cbind(p1, High.Price=scrip.data$High.Price)
p1 <- cbind(p1, Open.Price=scrip.data$Open.Price)
p1 <- cbind(p1, Average.Price=scrip.data$Average.Price)
p1 <- cbind(p1, Low.Price=scrip.data$Low.Price)
View(p1)
