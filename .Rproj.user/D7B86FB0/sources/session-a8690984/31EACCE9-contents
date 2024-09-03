# Leave one out cross validation -- LOOCV

# We are making some plots before we move to the real part
# scatter plot
plot(mpg ~ horsepower, data = Auto, col = "blue")

# simple linear regression
lm_hp_lin <- lm(mpg ~ horsepower, data=Auto)
lm_hp_quadr <- lm(mpg ~ horsepower + I(horsepower ^ 2), data=Auto)

# model summary
summary(lm_hp_quadr)

# Visualization: for a simple linear model you can use abline
abline(lm_hp_lin, col = "red", lwd = 2)

# for the quadratic model we build our own predictor function
f_q <- function(x){
  coefs <- coef(lm_hp_quadr)
  coefs[1] + coefs[2] * x + coefs[3] * x ^2
}
curve(f_q, 40, 230, add = TRUE, col = "green", lwd=2)




# load the libraries
library(boot)

# fit the auto data to a linear model
glm.fit <- glm(mpg ~ horsepower, data = Auto)

cv.err <- cv.glm(data = Auto, glm.fit)
cv.err$delta


# let's repeat the above for for increasingly complex polynomial fits
# creating a vector containing 10 entries --  all are initially 0
cv.error <- rep(0, 10)

# Using a for loop we fit increasingly complex polynomials
for (i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

# let's display all the cross validation errors
cv.error
