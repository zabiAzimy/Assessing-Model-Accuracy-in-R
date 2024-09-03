## Programming Cross validation

set.seed(1234567890)

x <- matrix(1:30, ncol = 1)
x

y_true <- cut(x, breaks = c(0, 10, 20, 30), labels = c(10, 15, 8)) |>
  as.character() |> as.integer()
y_true


epsilon_train <- rnorm(length(x))
epsilon_train

y_obs <- y_true + epsilon_train
y_obs


plot(x[,1], y_true, type = "p", ylim = range(c(y_obs, y_true)),
     xlab = "x", ylab = "y")
lines(x[,1]-0.5, y_true, type = "s", lty = 2, col = "grey")
points(x[,1], y_obs, lty = 2, col = "red", pch = 20)

legend("topright", legend = c("true values", "observed data"),
       col = c("black", "red"), pch = c(1, 20))

# First run: fit linear regression and leave just the first observation out
x_loo <- x[-1]
y_loo <- y_obs[-1]

x_loo
y_loo

# fit the linear model
lm_loo <- lm(y_loo ~ x_loo)
summary(lm_loo)

# plot the resulting regression to the existing plot
abline(lm_loo, col = "grey")

# finding the sum of squared error using the missing point
(predict(lm_loo, newdata = data.frame(x_loo = x[1])) - y_obs[1]) ^ 2

predict(lm_loo, newdata = data.frame(x_loo = x[1])) - y_obs[1]

# jsut manually predicting for the value of x[1] then minus the observed value of y
(12.31218 - 0.06681 * 1) - y_obs[1]


# we are going to write a loop which leaves one observation out at a time
loo_sse <- NULL

# loop over each of n observations
for(i in seq_along(x)){
  
  x_loo <- x[-i]
  y_loo <- y_obs[-i]
  
  # fit the linear regression model with the loo data
  lm_loo <- lm(y_loo ~ x_loo)
  
  # plot the regression line
  abline(lm_loo, col = "orange")
  
  # in each iteration compute the sse and store in the loo_sse vector
  loo_sse <- c(loo_sse,
               (predict(lm_loo, newdata = data.frame(x_loo = x[i])) - y_obs[i]) ^ 2)
}

# let's check the populated vector containing the errors
loo_sse

# calculate the mean of sse
mse_loocv <- mean(loo_sse)
mse_loocv



# we are now fitting a polynomial regression with degree from 1 - k
# initialize the result vector
mse_loocv <- rep(NA, 10)


# an outer loop that changes the polynomial degree
for(k in 1:10){
  
  loo_sse <- NULL
  
  # inner loop for each of the n observation
  for(i in seq_along(x)) {
    
    # create the new data by leaving one observation out
    x_loo <- x[-i]
    y_loo <- y_obs[-i]
    
    # fit the linear regression model with the loo data
    lm_loo <- lm(y_loo ~ poly(x_loo, degree = k, raw = T))
    
    # store the current sse to result vector
    # sum of squared errors
    loo_sse <- c(loo_sse,
                 (predict(lm_loo, newdata = data.frame(x_loo = x[i])) - y_obs[i]) ^ 2)
  }
  
  # store MSE for the current polynomial degree
  mse_loocv[k] <- mean(loo_sse)
}

# plot the different values of MSE_LOOCV
plot(mse_loocv, type = "b", col = "darkgreen", lwd = 2)

# Which degree minimized the mean squared error : I think that is 9
# at the degree of the polynomial as 9 we get the lowest MSE




# using the result to obtain the final fitted values 
lm_best_mse <- lm(y_obs ~ poly(x, degree = which.min(mse_loocv)))
summary(lm_best_mse)

# plot the above regression which the resulted best degree
plot(x, lm_best_mse$fitted.values, col = "red", type = "l", lwd = 2)
points(x, y_obs, col = "darkgreen")
lines(x, y_true, col = "grey", lwd = 1.5)

















