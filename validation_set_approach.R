# Validation Set approach on Auto Dataset

# required library 
library(ISLR2)

# for the purpose of reproducibility we set seed
set.seed(1)

train <- sample(392, 196)
train

# we are using a linear regression using the training set
# mpg (miles per gallon) is dependent on horsepower variable
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
Auto[-train]

# having the above linear model, we can make predictions
# further for calculating MSE, we use mean function
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

# fit the train data to a quadratic function
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

# calculate the test error -- MSE
mean ((mpg - predict(lm.fit2 , Auto))[-train ]^2)

# fit the training data to a cubic regression
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

# Calculate the MSE for the cubic regression
mean ((mpg - predict(lm.fit3 , Auto))[-train ]^2)

# The test rates obtained are: 23.38, 17.15, 17.11

# let's now choose a different training set
# as a result we will have different error on the validation set
set.seed(2)

train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# having the above linear model, we can make predictions
# further for calculating MSE, we use mean function
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

# fit the train data to a quadratic function
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

# calculate the test error -- MSE
mean ((mpg - predict(lm.fit2 , Auto))[-train ]^2)

# fit the training data to a cubic regression
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

# Calculate the MSE for the cubic regression
mean ((mpg - predict(lm.fit3 , Auto))[-train ]^2)

# this time the errors are: 25.72, 20.43, 20.38
# these results are quiet consistent with the previous ones
# The Model that uses quadratic function performs much better than the model
# that is using linear function, but there is not enough evidence to say that
# cubic regression is any better than quadratic.


# let's do the above one more time to see how varied is the error
set.seed(3)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# having the above linear model, we can make predictions
# further for calculating MSE, we use mean function
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

# fit the train data to a quadratic function
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)

# calculate the test error -- MSE
mean ((mpg - predict(lm.fit2 , Auto))[-train ]^2)

# fit the training data to a cubic regression
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

# Calculate the MSE for the cubic regression
mean ((mpg - predict(lm.fit3 , Auto))[-train ]^2)

# let's check if we have any attached dataset
search()

# we see that Auto us attached, let's detach that 
detach(Auto)

