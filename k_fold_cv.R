# K-fold cross validation

# we are using cv.glm() function to perform k-fold CV

# set the seed
set.seed(17)

# create a vector to hold the errors
cv.error.10 <- rep(0, 10)

for(i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10
