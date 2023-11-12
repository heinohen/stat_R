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









