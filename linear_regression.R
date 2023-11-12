# StatQuest youtube

## SIMPLE(ONE VARIABLE) LINEAR REGRESSION MODEL

# A new type of washing machine was recently introduced in 11 department stores. 
# These stores are of roughly equal size and are located in similar types of communities. 
# The manufacturer varied the price charged in each store, and the following data, 
# giving the number of units sold in 1 month for each of the different prices, resulted.
unit.data <- data.frame(price = c(280,290,300,310,320,330,340,350,360,370,380),
                        numb_sold = c(44,41,34,38,33,30,32,26,28,23,20))

plot(unit.data$price, unit.data$numb_sold)
unit.regression <- lm(numb_sold ~ price, data = unit.data)
summary(unit.regression)
abline(unit.regression)

# A plot of the number of units sold y versus the price x for these 11 data pairs.
# The resulting scatter diagram indicates that, sub- ject to random error,
# the assumption of a straight-line relationship between the number of units sold and the price appears to be reasonable.
# That is, a simple linear regression model appears to be appropriate.

#############

# Suppose that the responses Y_i corresponding to the input values x_i, i = 1,2,...,n
# are to be observed and used to estimate the parameters alpha and beta in a simpe linear regression model
# Y = alpha + beta*x + e

# beta_hat = sum_i=1_n (x_i - x_bar)(Y_i - Y_bar) / sum_i=1_n (x_i - x_bar)^2
# alpha_hat = Y_bar - beta_hat*x_bar

# x_bar = sum_i=1_n x_i / n
# Y_bar = sum_i=1_n Y_i / n

# y = a_har + b_hat*x
# b_hat == slope, a_hat == intercept

#A large midwestern bank is planning on introducing a new word processing system to its secretarial staff.
# To learn about the amount of training that is needed to effectively implement the new system, 
# the bank chose eight employees of roughly equal skill. 
# These workers were trained for different amounts of time and were then individually put to work on a given project. 
# The following data indicate the training times and the resulting times (both in hours) 
# that it took each worker to complete the project.


worker.data <- data.frame(training_time = c(22,18,30,16,25,20,10,14),
                          project_compl_time = c(18.4,19.2,14.5,19.0,16.6,17.7,24.4,21.0))

plot(worker.data$training_time, worker.data$project_compl_time)
worker.regression <- lm(project_compl_time ~ training_time, data = worker.data)
summary(worker.regression)
attributes((worker.regression))
abline(worker.regression, col = "red")

worker.regression$coefficients[1]
worker.regression$coefficients[2]

# a) What is the estimated regression line?
# Y = worker,regression$coefficients[[1]] + worker.coefficients[[2]] * x
# Y = 27.46608 - 0.444700*x
#.    alpha.      beta.  variable. 
# Precict the amount of time it would take a worker who receives 28 hours of training to complete the project?

hours_28 <- data.frame(training_time = c(28)) # data.frame with new independent variable for training time
predict(worker.regression, newdata = hours_28) # prediction args: linear model made before, new data is the points intrested
# 15.01446 (hours)

# OR by hand
# alpha + (training_time)*beta = 27.46608 - 28*(0.444700) = 15.01448

# Predict the amount of time it would take a worker who receives 50 hours of training to complete the project.

# Question asks for prediction of value 50 which is far greater than all the input values in our data set.
# As a result, even though the scatter diagram indicates a straigth-line fit,
# WE SHOULD BE NOT ESTIMATING VALUES THIS FAR OF OUR VALUES!!!

# ** Warning: Do not use the estimated regression line to predict responses at input values **
# ** that are far outside the range of the ones used to obtain this line. **

## TESTING THE HYPOTHESIS THAT BETA = 0

#H_0 beta = 0, H_1 beta != 0

# sqrt( ((n-2)*S_xx) / SS_R) * (beta_hat - beta)
# has a t distribtion with n - 2 degrees of freedom
# it follows that if H_0 == TRUE beta = 0 then,
# sqrt( ((n-2)*S_xx) / SS_R) * beta_hat
# has a t distribution with n - 2 degrees of freedom

# A significance-level-gamma test oh H_0 is to
#
# REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
# Not reject: otherwise
# WHERE:
# TS = sqrt( ((n-2)*S_xx) / SS_r ) * beta_hat

# p-value = 2P( T_(n-2 >= abs(v)))

## EX
# An individual claims that the fuel consumption of his automobile does not depend
#on how fast the car is driven. To test the plausibility of this hypothesis,
# the car was tested at various speeds between 45 and 75 miles per hour.
# The miles per gallon attained at each of these speeds were determined,
# with the following data resulting.

car_consumption.data <- data.frame(
  speed = c(45,50,55,60,65,70,75),
  mpg = c(24.2,25.0,23.3,22.0,21.5,20.6,19.8))
plot(car_consumption.data$speed, car_consumption.data$mpg)
cc_model <- lm(mpg ~ speed, data = car_consumption.data)
summary(cc_model)
abline(cc_model, col = "blue")

cars_n <- length(car_consumption.data$speed)
cars_x_bar <- mean(car_consumption.data$speed)
cars_y_bar <- mean(car_consumption.data$mpg)

#(x-x_bar)
cars_x_simple <- lapply(car_consumption.data$speed, function(x) (x - cars_x_bar))
#(x-x_bar)^2
cars_x_squared <- lapply(cars_x_simple, function(x) (x)^2)
#(y-y_bar)
cars_y_simple <- lapply(car_consumption.data$mpg, function(y) (y - cars_y_bar))
#(y-y_bar)^2
cars_y_squared <- lapply(cars_y_simple, function(y) (y)^2)
#(x-x_bar)*(y-y_bar)
cars_x_y_prod <- Map("*", cars_x_simple, cars_y_simple)

#SUMS
#(x-x_bar)
cars_x_summed <- sum(unlist(cars_x_simple))
#(x-x_bar)^2
#Sxx
cars_Sxx <- sum(unlist(cars_x_squared))

#(y-y_bar)
cars_y_summed <- sum(unlist(cars_y_simple))
#(y-y_bar)^2
#Syy
cars_SYY <- sum(unlist(cars_y_squared))

#Sxy
cars_SxY <- sum(unlist(cars_x_y_prod))

#SS_R = Sxx*Syy - (SxY)^2 / Sxx
cars_SS_R <- ((cars_Sxx * cars_SYY) - (cars_SxY)^2) / cars_Sxx

#beta_hat = SxY / Sxx  
cars_beta_hat <- cars_SxY / cars_Sxx

#Test statistic = sqrt( ((n-2)*Sxx) / SS_R) * beta_hat
cars_test_statistic <- sqrt( ((cars_n - 2) * cars_Sxx) / cars_SS_R) * cars_beta_hat
# Simple linear regression model
# H_0 beta = 0 against beta != 0
# REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
# WHERE:
# TS = sqrt( ((n-2)*S_xx) / SS_r ) * beta_hat

# T_CRIT = T_(n-2),gammapercentage(0.01)/2
cars_T_crit <- qt(1-0.005, cars_n - 2)
# REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
cars_H_0_rejected <- ifelse(abs(cars_test_statistic) >= cars_T_crit, TRUE, FALSE)


# NOT FROM BOOK!
attitude.data <- data.frame(
  correct = c(17,13,12,15,16,14,16,16,18,19),
  attitude = c(94,73,59,80,93,85,66,79,77,91)
)

att_m <- length(attitude.data$correct)
att_n <- length(attitude.data$attitude)

#means
att_x_bar <- mean(attitude.data$correct)
att_y_bar <- mean(attitude.data$attitude)
#(x - x_bar)
att_x_simple <- lapply(attitude.data$correct, function(x) (x - att_x_bar))
att_x_simple # <-- correct values
#(y - y_bar)
att_y_simple <- lapply(attitude.data$attitude, function(y) (y - att_y_bar))
att_y_simple # <-- correct values
#(x - x_bar)^2
att_x_squared <- lapply(att_x_simple, function(x) (x)^2)
att_x_squared # <-- correct values
#(y - y_bar)^2
att_y_squared <- lapply(att_y_simple, function(y) (y)^2)

#MAP <-- mapply
att_x_times_y <- Map("*", att_x_simple, att_y_simple)

# SUM (x-x_bar)*(y-y_bar)
att_sum_x_times_y <- sum(unlist(att_x_times_y))
# SUM (x-x_bar)^2
att_sum_x_bar_squared <- sum(unlist(att_x_squared))
# SUM (y-y_bar)^2
att_sum_y_bar_squared <- sum(unlist(att_y_squared))

# PEARSON CORRELATION COEFFICIENT (r)
# = ( SUM (x-x_bar)*(y-y_bar) / SQRT(SUM (x-x_bar)^2 * SUM (y-y_bar)^2))
att_pearson_r <- (att_sum_x_times_y / sqrt(att_sum_x_bar_squared * att_sum_y_bar_squared))

# STANDARD DEVIATIONs
#S_YY sqrt(SUM (y-y_bar)^2 / n-1)
att_S_yy <- sqrt(att_sum_y_bar_squared / (att_n - 1)) 
#S_XX sqrt(SUM (x-x_bar)^2 / m-1)
att_S_xx <- sqrt(att_sum_x_bar_squared / (att_n - 1))

# SLOPE
# BETA = r * S_YY / S_XX
att_beta <- att_pearson_r * (att_S_yy / att_S_xx)

#INTERCEPT
# ALPHA = y_bar - beta*x_bar
att_alpha <- att_y_bar - (att_beta * att_x_bar)

# SIMPLE LINEAR REGRESSION FORMULA
# Y = a + bx, suppose x = 15, remember to stay in bounds!
att_15 <- 15
att_big_Y_at_15 <- att_alpha + att_beta*att_15

# REGRESSION TO THE MEAN

reg.data <- data.frame(
  father = c(60,62,64,65,66,67,68,70,72,74),
  son = c(63.6,65.2,66,65.5,66.9,67.1,67.4,68.3,70.1,70.0)
)
plot(reg.data$father, reg.data$son)
reg_height_model <- lm(son ~ father, data = reg.data)
summary(reg_height_model)
abline(reg_height_model)
reg_n <- length(reg.data$father)

#means
reg_x_bar <- mean(reg.data$father)
reg_y_bar <- mean(reg.data$son)
#(x-x_bar)
reg_x_simple <- lapply(reg.data$father, function(x) (x- reg_x_bar))
#(x-x_bar)^2
reg_x_squared <- lapply(reg_x_simple, function(x) x^2)
#(y-y_bar)
reg_y_simple <- lapply(reg.data$son, function(y) (y - reg_y_bar))
#(y-y_bar)^2
reg_y_squared <- lapply(reg_y_simple, function(y) y^2)
#(x-x_bar)*(y-y_bar)
reg_x_times_y <- Map("*",reg_x_simple, reg_y_simple)

#SUMS
#(x-x_bar)
reg_x_sum <- sum(unlist(reg_x_simple))
#(x-x_bar)^2
#Sxx
reg_sxx <- sum(unlist(reg_x_squared))

#(y-y_bar)
reg_y_sum <- sum(unlist(reg_y_simple))
#(y-y_bar)^2
#Syy
reg_syy <- sum(unlist(reg_y_squared))

#Sxy
reg_sxy <- sum(unlist(reg_x_times_y))

# PEARSON CORRELATION COEFFICIENT (r)
# = ( SUM (x-x_bar)*(y-y_bar) / SQRT(SUM (x-x_bar)^2 * SUM (y-y_bar)^2))
reg_pearson_R <- reg_syx / sqrt((reg_sxx * reg_syy))

#SS_R = (Sxx*Syy - (SxY)^2) / Sxx
reg_ss_r <- (reg_sxx * reg_syy - (reg_sxy)^2) / reg_sxx

#BETA = SxY / Sxx  
reg_beta <- reg_sxy / reg_sxx

# ALPHA = y_bar - beta*x_bar
reg_alpha <- reg_y_bar - reg_beta * reg_x_bar

# H_0 beta = 1 against beta < 1
# REJECT H_0: TS <= -T_8,alpha
# WHERE:
# TS = sqrt( ((n-2)*S_xx) / SS_r ) * (beta_hat - 1)
reg_test_statistic <- sqrt( ((reg_n - 2)* reg_sxx) / reg_ss_r) * (reg_beta - 1)  

# T_CRIT = T_(n-2),gammapercentage(0.01)/2
reg_t_crit <- qt(0.01,df = reg_n - 2)
# REJECT H_0: TS <= -T_8,alpha
reg_rejected <- ifelse(reg_test_statistic <= reg_t_crit, TRUE, FALSE)

# The following data relate the number of motor vehicle deaths occurring in 12
# counties in the northwestern United States in the years 1988 and 1989.

