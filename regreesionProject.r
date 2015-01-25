##### In this report data gathered by Moto Trends is explored and analyzed in order to determine the relationship between a set of variables and miles per gallon of automobiles. In particlular, the following two issues will be addressed:
##### Is an automatic or manual transmission better for MPG?
##### What is the MPG difference between automatic and manual transmissions?

data(mtcars)
head(mtcars)

# covert qualitative data into factors

mtcars$am <- factor(mtcars$am, labels = c("automatic", "manual"))
#am Transmission (0 = automatic, 1 = manual)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

### plot pairwise graph of mtcars
png("plot2.png", width = 480, height = 480)
plot <- pairs(mtcars, panel = panel.smooth, main ="Pairwise plot of mtcars data")
dev.off()

### draw plot for MPG vs Transmission
png("plot1.png", width = 480, height = 480)
boxplot(mpg~am, data = mtcars, main = "MPG vs Transmission",
        xlab = "Transmission Type", ylab = "MPG",
        names = c("Automatic", "Manual"),
        col = c("lightblue", "pink"))
dev.off()

### check the correlation of variance between every factors
cov2cor(cov(sapply(mtcars, as.numeric)))
head(cov2cor(cov(sapply(mtcars, as.numeric))), 1)

### basic model using tramsmission as predictors
basicModel <- lm(mpg ~ am, data = mtcars)
summary(basicModel)
t_test <- t.test(mpg ~ am, data = mtcars)


### regression model using all data as predictors
allModel <- lm(mpg ~ ., data = mtcars)
summary(allModel)

### stepwise-selected model
stepwiseModel <- step(lm(mpg ~ ., data = mtcars), trace = 0)
summary(stepwiseModel)$coef
stepwiseModel$anova

### compare basic model with stepwise-selected model

compare <- anova(basicModel, stepwiseModel)
compare$Pr

png("plot3.png", width = 480 , height = 480)
par(mfrow=c(2,2))
plot(stepwiseModel)
dev.off()


### Plot4 mpg comparision with respect to weight

png("plot4.png", width =480 , height = 480)
plot(mpg ~ wt, mtcars, col = am, xlab = 'Weight')
abline(lm(mpg ~ wt, mtcars))
legend('topright', lty = c(1,1), col = 1:2, legend = c('Automatic', 'Manual'))
dev.off()

### Plot5 mpg comparision with respect to qsec.
png("plot5.png", width = 480 , height = 480)
plot(mpg ~ qsec, mtcars, col = am, xlab = "qsec(1/4 mile time)", ylab = "mpg")
abline(lm(mpg ~ qsec, mtcars))
legend('topright', lty = c(1,1), col = 1:2, legend = c('Automatic', 'Manual'))
dev.off()


