
#------------------------------------------------------------------------------------

#An approximate 100(1 - alpha) percent confidence interval estimator of p is given by
# P_hat +/- Z_alpha/2*SD(P_hat)

#Out of a random sample of 100 students at a university, 82 stated that they
#were nonsmokers. Based on this, construct a 99 percent confidence interval
#estimate of p, the proportion of all the students at the university who are
#nonsmokers.

all_smoke_question <- 100
non_smokers <- 82
smokers_alpha <- 0.01
smokers_99 <- 100*(1 - smokers_alpha)
smokers_Z <- 2.576
smokers_p_hat <- non_smokers/all_smoke_question

# P_hat +/- Z_alpha/2*SD(P_hat)
smokers_calc <- smokers_Z*sqrt(smokers_p_hat*(1 - smokers_p_hat)/all_smoke_question)
smokers_lower <- round(smokers_p_hat - smokers_calc, digits = 3)*100
smokers_upper <- round(smokers_p_hat + smokers_calc, digits = 3)*100

#------------------------------------------------------------------------------------

# On December 24, 1991, The New York Times reported that a poll indicated
# that 46 percent of the population was in favor of the way that President Bush
# was handling the economy, with a margin of error of ±3 percent. What does
# this mean? Can we infer how many people were questioned?
# It has become common practice for the news media to present 95 percent
# confidence intervals. That is, unless it is specifically mentioned otherwise,
# it is almost always the case that the interval quoted represents a 95 percent
# confidence interval. Since Z0.025 = 1.96, a 95 percent confidence interval for
# p is given by

#P_hat +/- Z_score*sqrt(P_hat(1-P_hat)/n)

in_favor_z <- 1.96 # from table D.2
in_favor_error <- 0.03

in_favor_phat <- 0.46

#1.96*sqrt((in_favor_phat*(1- in_favor_phat)/n)) = 0.03
#square both sides
#(1.96)^2* ((0.46)(1-0.46)/n) = (0.03)^2 || *n
#(1.96)^2* ((0.46)(1-0.46)) = (0.03)^2*n || : (0.03)^2
#(1.96)^2* ((0.46)(1-0.46)/(0.03)^2) = n
in_favor_left_side <- 1.96^2
in_favor_right_side <- 0.46*(1-0.46)/0.03^2
in_favor_result <- round(in_favor_left_side * in_favor_right_side,digits = 0)

#------------------------------------------------------------------------------------

# How large a sample is needed to ensure that the length of the 90 percent
# confidence interval estimate of p is less than 0.01?

# n > (Z_alpha/2 / b)^2
conf_int_Z <- 1.645
conf_int_p <- 0.01
conf_int_estimate <- (conf_int_Z/conf_int_p)^2 # n > 27060

#------------------------------------------------------------------------------------

# A random sample of 125 individuals working in a large city indicated that
# 42 are dissatisfied with their working conditions. Construct a 95 percent
# lower confidence bound on the percentage of all workers in that city who
# are dissatisfied with their working conditions.

#A 100(1 − α) percent lower conﬁdence bound for p is given by

workers_all <- 125
workers_dissatisfied <- 42
workers_Z <- 1.645
workers_Phat <- workers_dissatisfied / workers_all

#P_hat - Z_alpha * sqrt(P_hat*(1-P_hat)/n)

workers_lower_confbound <- workers_Phat - workers_Z * sqrt(workers_Phat*(1 - workers_Phat)/ workers_all) #0.26650

# That is, we can be 95 percent certain that over 26.6 percent of all workers
# are dissatisfied with their working conditions.


