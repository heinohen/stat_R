#---------------------------------------------------------------------------------

# if you bet $1 on a number at a roulette table in a U.S. casino, then either you
# will win $35 if your number appears on the roulette wheel or you will lose
# $1 if it does not. Since the wheel has 38 slots–numbered 0, 00, and each of
# the integers from 1 to 36 — it follows that the probability that your number
# appears is 1/38. As a result, your expected gain on the bet is:

#ESTIMATED GAIN = E[gain]

casino_gain <- 35*(1/38)-1*(37/38)

# That is, your expected loss on each spin of the wheel is approximately 5.3 cents.

#---------------------------------------------------------------------------------

# Let us check the preceding formulas for the expected value and variance
# of the sample mean by considering a sample of size 2 from a population
# whose values are equally likely to be either 1 or 2. That is, if X is the value
# of a member of the population, then:

#P(X = 1) = 1/2, P(X = 2) = 1/2

#Mean of x is Expected value of x or E[x] 
mean_ex7_1 <- 1*(1/2) + 2*(1/2) # = 1.5
#Variance is Var(X) = E[(X-mu)^2]
var_ex7_1 <- ((1-1.5)^2)*(1/2) + ((2-1.5)^2)*(1/2) # = 1/4 = 0.25

# To obtain the probability distribution of the sample mean (X1 + X2 )/2, note
# that the pair of values X1 , X2 can assume any of four possible pairs of values
# (1, 1), (1, 2), (2, 1), (2, 2)
# where the pair (2, 1) means, for instance, that X1 = 2, X2 = 1. By the independence of X1 
# and X2 it follows that the probability of any given pair is 1/4. Therefore we see that the possible values of X_bar = (X_1 + X_2)/2,
# along with their respective probabilities are as follows:

#P(X_bar = 1) = P(1,1) = 1/4
#P(X_bar = 1.5) = P{(1,2) OR (2,1)} = 2/4 = 1/2
#P(X_bar = 2) = P(2,2) = 1/4

#E(X_bar)
mean_xbar_7_1 <- 1*(1/4) + 1.5*(2/4) + 2*(1/4) # = 1.5
#Var(X_bar) = E[(Xbar - mu)^2]
var_xbar_7_1 <- ((1-1.5)^2)*(1/2) + ((1.5-1.5)^2)*(2/4) + ((2-1.5)^2)*(1/4) # = 1/8 = 0.1875

# since mean(mu) = 1.5 and sigma^2 = 1/4, Expected valueE(X_bar) = mu,
#Variance Var(X_bar) = (sigma^2)/2,
#Standard diviation SD(X_bar) = sqrt(Var(X_bar))

#---------------------------------------------------------------------------------

#CENTRAL LIMIT THEOREM - CLT

#---------------------------------------------------------------------------------
 
# An insurance company has 10,000 (=10^4 ) automobile policyholders. If the
# expected yearly claim per policyholder is $260 with a standard deviation of
# $800
sd_7_2 <- 800
n_7_2 <- 10^4
# X_i = yearly claim of policyholder i = 1,2....,10^4


# BY CLT
# X = SUM_{i=1}_{10^4} X_i ~N(10^4 x 260, 800*sqrt(10^4))
#EX
expected_value_7_2 <- 10^4 * 260
#SD = SD*sqrt(n)
normalized_SD_7_2 <- 800*sqrt(n_7_2) 

# approximate the probability that the total yearly claim exceeds $2.8
# million (=$2.8 × 10^6 ).
asked_amount_7_2 <- 2.8 * 10^6

#P(X > 2800000) = 1 - P(X <= 2800000)
zscore_7_2 <- (asked_amount_7_2 - expected_value_7_2) / normalized_SD_7_2 
prob_7_2 <- 1 - pnorm(zscore_7_2) # 0.0062

#---------------------------------------------------------------------------------

# Let X be the sample mean of a sample of size n from a population having mean μ and variance σ^2 . By the central limit theorem,
# P(X_bar <= a) = P( (X_bar - mu)/ (sigma/sqrt(n)) <= (a - mu)/(sigma/sqrt(n)) )
# P(Z <= (a-mu)/(sigma/sqrt(n)))

ex_7_3_workers_mean <- 202
ex_7_3_workers_SD <- 14

ex_7_3_X_bar <- 36

#P(198 <= X <= 206)
O_lower_limit <- 198
O_upper_limit <- 206

#EX(X_bar)
O_X_bar <- 202
O_SD <- 14/sqrt(ex_7_3_X_bar)

#LOWERLIMIT P(X >= 198)
O_Lower <- (O_lower_limit - O_X_bar)/O_SD
#UPPERLIMIT P(X <= 206)
O_Upper <- (O_upper_limit - O_X_bar)/O_SD

O_prob <- pnorm(O_Upper) - pnorm(O_Lower) # 0.9134

#Repeat (a) for a sample size of 64.

Bex_7_3_X_bar <- 64

#P(198 <= X <= 206)
OB_lower_limit <- 198
OB_upper_limit <- 206

#EX(X_bar)
OB_X_bar <- 202
OB_SD <- 14/sqrt(Bex_7_3_X_bar)

#LOWERLIMIT P(X >= 198)
OB_Lower <- (OB_lower_limit - OB_X_bar)/OB_SD
#UPPERLIMIT P(X <= 206)
OB_Upper <- (OB_upper_limit - OB_X_bar)/OB_SD

OB_prob <- pnorm(OB_Upper) - pnorm(OB_Lower) # 0.9777

#---------------------------------------------------------------------------------

# An astronomer is interested in measuring, in units of light-years, the distance from her observatory to a distant star.
# However, the astronomer knows that due to differing atmospheric conditions and normal errors, each time a
# measurement is made, it will yield not the exact distance, but an estimate of
# it. As a result, she is planning on making a series of 10 measurements and
# using the average of these measurements as her estimated value for the actual distance.
# If the values of the measurements constitute a sample from a
# population having mean d (the actual distance) and a standard deviation of
# 3 light-years, approximate the probability that the astronomer’s estimated
# value of the distance will be within 0.5 light-years of the actual distance.

# so asked is +/- 0.5 lightyears, mean is unknown but thats ok, use -0.5 and 0.5 in the nominator
lightyears_n_of_obs <- 10
lightyears_SD <- 3/sqrt(lightyears_n_of_obs)

#P(-0.5 <= X_bar - d <= 0.5)
lightyears_lower_limit <- -0.5
lightyears_upper_limit <- 0.5

#LOWERLIMIT P(X >= -0.5)
lightyears_lower = -0.5/lightyears_SD
#UPPERLIMIT P(X <= 0.5)
lightyears_upper = 0.5/lightyears_SD

lightyears_prob <- pnorm(lightyears_upper) - pnorm(lightyears_lower) # 0.402

#---------------------------------------------------------------------------------

# SAMPLING PROPORTIONS FROM A FINITE POPULATION

# Consider a population of size N in which certain elements have a particular
# characteristic of interest. Let p denote the proportion of the population having
# this characteristic. So Np elements of the population have it and N(1 − p) do not.

# Suppose that 60 out of a total of 900 students of a particular school are
# left-handed. If left-handedness is the characteristic of interest, then N = 900
# and p = 60/900 = 1/15.
left_handers <- 60
left_all_n <- 900
left_hand_p <- left_handers/left_all_n # 0.0666666666 = 1/15 

#---------------------------------------------------------------------------------

# Suppose that 50 percent of the population is planning on voting for candidate A in an upcoming election. 
# If a random sample of size 100 is chosen,
# then the proportion of those in the sample who favor candidate A has expected value:

#BINOMIAL DISTRIBUTION 

#E(X) = np, SD(X) = sqrt(np(1-p))

#E(X_bar) = E(X)/n = p = 50/100 = 0.5 
#SD(X_bar) = SD(X)/n = sqrt(p(1-p)/n) = sqrt((0.5(1-0.5))/100) = sqrt(1/400) = 0.05

#---------------------------------------------------------------------------------

# Probabilities Associated with Sample Proportions: The
# Normal Approximation to the Binomial Distribution

# it follows that X/n can be regarded as the sample mean of a sample of
# size n from a population having mean p and standard deviation sqrt(p(1 − p)).
# Thus, from the central limit theorem, we see that for n large:

# (X / (n - p)/sqrt(p(1-p)/n)) == (X-np)/sqrt(np(1-p)) ~

#---------------------------------------------------------------------------------
# 
# Suppose that exactly 46 percent of the population favors a particular candidate.

# X = number of who is in favor X ~ Bin(n,p) = X ~ Bin(200, 0.46)

Candidate_n <- 200
Candidate_p <- 0.46

# If a random sample of size 200 is chosen what is the probability that
# at least 100 favor this candidate?

#Expected value = np = 200*0.46
Candidate_expected <- 200*0.46

#P(X >= 100) =(continuity correction) = P(X >= 99.5)
Candidate_corrected <- 99.5
Candidate_SD <- sqrt(Candidate_n*Candidate_p*(1 - Candidate_p))
Candidate_Zscore <- (Candidate_corrected - Candidate_expected)/ Candidate_SD

#P(X >= x) = 1 - P(X < x)
Candidate_prob <- 1 - pnorm(Candidate_Zscore)

#oOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoO

#THIS CAN ALSO BE DONE STRAIGHT WITH R:

#pbinom(q = 99.5, size = 200, prob = 0.46, lower.tail = FALSE)

#oOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoOoO











