#Suppose that if a signal having intensity μ originates at location A, 
#then the intensity recorded at location B is normally distributed with mean μ 
#and standard deviation 3. That is, due to “noise,” 
#the intensity recorded differs from the actual intensity of 
#the signal by an amount that is normal with mean 0 and standard deviation 3. 
#To reduce the error, the same signal is independently recorded 10 times. 
#If the successive recorded values are

results <- c(17, 21, 20, 18, 19, 22, 20, 21, 16, 19)
sample_mean <- mean(results)
#print sample mean X-bar
sample_mean
#95% conf int z-score is qnorm(0.975) 2 times both tails
#lowerbound
lowerbound <- sample_mean-qnorm(0.975)*(3/sqrt(length(results)))
#upperbound
upperbound <- sample_mean+qnorm(0.975)*(3/sqrt(length(results)))
lowerbound
upperbound
# Z = √n*(XBAR-μ/σ) has a standard normal distribution. Now, since z0.025 = 1.96,
#it follows that 95 percent of the time the absolute value of Z is less than or equal to 1.96 (see Fig. 8.1)#
#99
lowerbound_ninetynine <- sample_mean-qnorm(0.995)*(3/sqrt(length(results)))
upperbound_ninetynine <- sample_mean+qnorm(0.995)*(3/sqrt(length(results)))
lowerbound_ninetynine
upperbound_ninetynine


#From past experience it is known that the weights of salmon grown at a commercial hatchery 
#are normal with a mean that varies from season to season 
#but with a standard deviation that remains fixed at 0.3 pounds. 
#If we want to be 90 percent certain that our estimate of 
#the mean weight of a salmon is correct to within ±0.1 pounds, 
#how large a sample is needed? What if we want to be 99 percent certain?
#((2*Z(alpha/2)*sigma)/b)^2
#90 percent
salmon_samplesize <- ((2*qnorm(1-(0.1/2))*0.3)/0.2)^2
salmon_samplesize
#99 percent
salmon_samplesize <- ((2*qnorm(1-(0.01/2))*0.3)/0.2)^2
salmon_samplesize

#Suppose in ^^^^^^^ that we want to specify a value that, 
#with 95 percent confidence, is less than the average weight of a salmon. 
#If a sample of 50 salmon yields an average weight of 5.6 pounds, 
#determine this value.

Z_0.05 <- (qnorm(1-0.05))
salmonSD <- 0.3
sample_size50 <- 50
sample_mean50 <-  5.6

#lower mean > XBAR - Z_alpha*sigma/sqrt(n)
salmon50lower <- sample_mean50 - Z_0.05*(salmonSD/sqrt(sample_size50))
round(salmon50lower,digits = 4)

#To estimate μ, the average nicotine content of a newly marketed cigarette, 
#44 of these cigarettes are randomly chosen, 
#and their nicotine contents are determined
#Assume that it is known from past experience that the standard deviation of 
#the nicotine content of a cigarette is equal to 0.7 milligrams.

#(a) If the average nicotine finding is 1.74 milligrams,
#what is a 95 percent confidence interval estimator of μ?
cigs_mean <- 1.74
cigs_samplesize <- 44
cigs_var <- 0.7
bounds <- c()
#lowerbound
bounds[1] <- cigs_mean-qnorm(0.975)*(0.7/sqrt(cigs_samplesize))
#upperbound
bounds[2] <- cigs_mean+qnorm(0.975)*(0.7/sqrt(cigs_samplesize))
bounds

#(b) How large a sample is necessary for the length of 
#the 95percent confidence interval to be less than or equal to 0.3 milligrams?
#((2*Z(alpha/2)*sigma)/b)^2
#95 percent
cigssize95 <- ((2*qnorm(1-(0.05/2))*0.7)/0.3)^2
cigssize95

#find 95 conf upper bound

cigs_upper95 <- cigs_mean + qnorm(0.95)*(0.7/sqrt(cigs_samplesize))
cigs_upper95


# T-Test

#P(T_n > T_n,a) == alpha, P(T_n < T_n,a) = 1-alpha == t_n,a == 100(1-alpha) percentile of the t-distribution

#A 100(1 − alpha ) percent confidence interval estimator for the population mean μ is given by the interval

#XBAR +/- t_n-1,alpha/2*S/sqrt(n)


#The Environmental Protection Agency (EPA) is concerned about the amounts of PCB, 
#a toxic chemical, in the milk of nursing mothers. 
#In a sample of 20 women, the amounts (in parts per million) of PCB were as follows:
#16,0,0,2,3,6,8,2,5,0,12,10,5,7,2,3,8,17,9,1

#(a) 95 percent confidence interval
milk_data <- c(16,0,0,2,3,6,8,2,5,0,12,10,5,7,2,3,8,17,9,1)
milk_sample_mean <- mean(milk_data)
milk_sample_var <- var(milk_data)
milk_sample_SD <- sqrt(milk_sample_var)

milk_ttest_95 <- t.test(milk_data,alternative = "two.sided",mu = milk_sample_mean)
milk_ttest_95

#alpha / 2, df= data - 1
milk_95_alpha <- 1-0.95
milk_95_alpha_per_2 <- (1-0.95)/2
milk_95_tscore <- qt((1 - milk_95_alpha_per_2), df = length(milk_data)-1)
milk_95_bound_tscore <- qt(1 - milk_95_alpha, df = length(milk_data) - 1)
milk_95_bounds <- c((milk_sample_mean - milk_95_tscore * milk_sample_SD/sqrt(length(milk_data))), (milk_sample_mean + milk_95_tscore * milk_sample_SD/sqrt(length(milk_data))))

#(b) 99 percent confidence interval
milk_99_alpha <- 1-0.99
milk_99_alpha_per_2 <- (1-0.99)/2
milk_99_tscore <- qt((1 - milk_99_alpha_per_2), df = length(milk_data)-1)
milk_99_bound_tscore <- qt((1 - milk_99_alpha), df = length(milk_data)-1)
milk_99_bounds <- c((milk_sample_mean - milk_99_tscore * milk_sample_SD/sqrt(length(milk_data))),
                    (milk_sample_mean + milk_99_tscore * milk_sample_SD/sqrt(length(milk_data))))


#(a) 95 percent confidence bounds

milk_95_lower_confbound <- round(milk_sample_mean - milk_95_bound_tscore * milk_sample_SD/sqrt(length(milk_data)), digits = 2)
milk_95_upper_confbound <- round(milk_sample_mean + milk_95_bound_tscore * milk_sample_SD/sqrt(length(milk_data)), digits = 2)

#(b) 99percent confidence bounds

milk_99_lower_confbound <- round(milk_sample_mean - milk_99_bound_tscore * milk_sample_SD/sqrt(length(milk_data)), digits = 2)
milk_99_uppwer_confbound <- round(milk_sample_mean + milk_99_bound_tscore * milk_sample_SD/sqrt(length(milk_data)), digits = 2)