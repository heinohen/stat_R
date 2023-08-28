#--------------------------------------------------------------------------------

# Suppose that if a signal of intensity μ is emitted from a particular star, then
# the value received at an observatory on earth is a normal random variable with mean μ and standard deviation 4.
# In other words, the value of the signal emitted is altered by random noise, which is normally distributed with
# mean 0 and standard deviation 4. It is suspected that the intensity of the
# signal is equal to 10. Test whether this hypothesis is plausible if the same
# signal is independently received 20 times and the average of the 20 values
# received is 11.6. Use the 5 percent level of significance.

#H_0 mu = 10
h_zero <- 10
#H_1 mu != 10

#sqrt(n)/sigma * abs(X_bar - mu)

signal_Z <- 1.960 #95% ==  0.05 / 2 ===> alpha @ inf Z_0.025
signal_sample_size <- 20
signal_mean <- 11.6
signal_SD <- 4

signal_value_statistic <- sqrt(signal_sample_size)/signal_SD * abs(signal_mean - h_zero)
signal_boolean <- signal_value_statistic >= signal_Z # FALSE

#REJECT H_0 if sqrt(n)/sigma * abs(X_bar - mu) >= Z_alpha/2
# ====> CALCULATED STATISTIC IS NOT EQUAL OR GREATER THAN Z_alpha/2 ====> H_0 is not rejected!
# Since this value is less than z0.025 = 1.96, the null hypothesis is not rejected.
# In other words, we conclude that the data are not inconsistent with the null
# hypothesis that the value of the signal is equal to 10.

signal_Z_90 <- 1.645
signal_boolean_99 <- signal_value_statistic >= signal_Z_90 # TRUE WITH 90% CONF INT!!!!

#--------------------------------------------------------------------------------

# Suppose that the average of the 20 values in ^^^^^^^^^ is equal to 10.8. In
# this case the absolute value of the test statistic is:

signal_new_mean <- 10.8
signal_new_value_statistic <- sqrt(signal_sample_size)/signal_SD * abs(signal_new_mean - h_zero) # = 0.894

# P-value P{ abs(Z) >= 0.894} = 2*P{Z >= 0.894} = 1.788 (LOOK FROM TABLE D.1 VALUE, GIVEN IN P (Z <= x) ==> must use 1 - P(P <= x)
# 1.79 ==> 0.9633 <=> 1 - 0.9633 == 0.371

# R:llä p-arvo olisi saatu komennolla 2*(1-pnorm(q=3,mean=0,sd=1)) tai komennolla
# 2*pnorm(q=3,mean=0,sd=1,lower.tail=FALSE). Saatua p-arvoa voi tulkita siten, että olisi
# 0.003 todennäköisyys saada näin poikkeava tai vielä poikkeavampi aineisto, mikäli pätisi
# μ = 105. Siis on hyvin epätodennäköistä, että tällainen aineisto olisi saatu ”vain sattumalta”.
# Johtopäätös olisi voitu tehdä myös p-arvon perusteella. Nyt kun p-arvo on pienempää kuin
# merkitsevyystaso α = 0.05, niin nollahypoteesi hylätään.

signal_new_p_value <- 2*(1-pnorm(0.894))

#--------------------------------------------------------------------------------

# assuming a 0.05 significance level, what is the probability
# that the null hypothesis (that the signal intensity is equal to 10) will not be
# rejected when the actual signal value is 9.2?

# h_zero still == 10

#REJECT H_0 if sqrt(n)/sigma * abs(X_bar - h_zero) >= Z_alpha/2

# sqrt(signal_sample_size)/signal_SD * abs(X_bar - h_zero) >= signal_Z <===> abs(X_bar - h_zero) >= 4*signal_Z/sqrt(signal_sample_size)

signal_distance <- signal_SD * signal_Z / sqrt(signal_sample_size) # 1.753
 
# this means that H0 is to be rejected if the distance between X and 10 is at least 1.753. That is, H0 will be
# rejected if either:

# X_bar >= 10 + 1.753 = 11.753
signal_XBAR_upper_limit <- h_zero + signal_distance
# OR
# X_bar <= 10 - 1.753 = 8.247
signal_XBAR_lower_limit <- h_zero - signal_distance

# Now, if the population mean is 9.2 then X will be normal with mean 9.2
# and standard deviation 4/sqrt(20) = 0.894; and so the standardized variable

# Z = XBAR - 9.2 / 0.894
signal_supposed_mean <- 9.2
signal_supposed_SD <- round(signal_SD / sqrt(signal_sample_size),digits = 3)

# REJECTION OF H_zero ==> P(XBAR >= 11.753) + P(XBAR <= 8.247)

#P(Z > XBAR-9.2 / SD)

signal_standardized_upper_prob <- round((signal_XBAR_upper_limit - signal_supposed_mean)/signal_supposed_SD,digits =3)
signal_standardized_lower_prob <- round((signal_XBAR_lower_limit - signal_supposed_mean)/signal_supposed_SD, digits = 3)
signal_calculated_upper <- 1 - pnorm(signal_standardized_upper_prob)
signal_calculated_lower <- pnorm(signal_standardized_lower_prob)
signal_total_prob <- signal_calculated_upper + signal_calculated_lower

# SO LASTLY

signal_h_zero_not_rejected <- 1- signal_total_prob # ~0.8547

# That is, when the true signal value is 9.2, there is an 85.47 percent chance
# that the 0.05 significance level test will not reject the null hypothesis that
# the signal value is equal to 10.







