library(plyr)
cars <- mutate(mtcars,
               cyl=factor(cyl))

# Question 1
data(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, mtcars)
summary(fit)

# Question 2
fit2 <- lm(mpg ~ factor(cyl), mtcars)
summary(fit2)

# Question 3
library(plyr)
cars <- mutate(mtcars,cyl=factor(cyl))
fit <- lm(mpg~cyl+wt,data=cars)
fit2 <- update(fit,mpg~cyl + wt + cyl*wt)
summary(fit)
summary(fit2)
anova(fit,fit2)

# Question 4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# Question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
lm.influence(fit)$hat

# Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y~x)
lm.influence(fit)$hat
dfbetas(fit)