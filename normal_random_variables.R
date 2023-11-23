#-------------------------------------------------------------------------------

# Normal Random Variables

#-------------------------------------------------------------------------------

# mu = E[X], sigma = SD(X)

# Test scores on the Scholastic Aptitude Test (SAT) verbal portion are normally
# distributed with a mean score of 504. If the standard deviation of a score is
# 84, then we can conclude that approximately 68 percent of all scores are
# between 504 − 84 and 504 + 84. That is, approximately 68 percent of the
# scores are between 420 and 588. Also, approximately 95 percent of them
# are between 504 − 168 = 336 and 504 + 168 = 672; and approximately 99.7
# percent are between 252 and 756.

#-------------------------------------------------------------------------------

# Z is greater than x.
# For instance, suppose we want to determine the probability that Z is greater
# than 2. To accomplish this, we note that either Z is less than or equal to 2 or
# Z is greater than 2, and so

#P(Z <= 2) + P(Z > 2) = 1
#P(Z > 2) = 1 - P(Z <= 2)
N_a <- 1 - pnorm(2)

N_TESTA <- pnorm(2)
N_TESTAA <- 1 - pnorm(2)

#-------------------------------------------------------------------------------

# Find 
# (a) P {Z < 1.5}
N_aa <- pnorm(1.5)

# (b) P {Z ≥ 0.8}
N_ab <- 1 - pnorm(0.8)

#-------------------------------------------------------------------------------

# While Table 6.1 specifies P {Z < x} for only non-negative values of x, it can be
# used even when x is negative. Probabilities for negative x are obtained from
# the table by making use of the symmetry about zero of the standard normal
# curve. For instance, suppose we want to calculate the probability that Z is less
# than −2. By symmetry (see Fig. 6.7), this is the same as the probability that Z
# is greater than 2; and so

#P(Z < -2) == 1 - P(Z > 2) == 1 - P(Z < 2)
N_ba <- 1 - pnorm(2) # 0.0028

#-------------------------------------------------------------------------------

# Find

#(a) P {1 < Z < 2} = P(Z < 2) - P(Z < 1)
N_caa <- pnorm(2) - pnorm(1)

#(b) P {−1.5 < Z < 2.5} = P(Z < 2.5) - P(Z < - 1.5) = P(Z < 2.5) - (1 - P(Z > 1.5))
N_cbb <- pnorm(2.5) - (1 - pnorm(1.5))

#-------------------------------------------------------------------------------

# Find P{ abs(Z) > 1.8 } = 2P{Z > 1.8} = 2*(1 - P(Z <= 1.8))
N_dabs <- 2*(1 - pnorm(1.8))

# Another easily established result is that for any positive value of a
# P {−a < Z < a} = 2P {Z < a} − 1

#-------------------------------------------------------------------------------

#FINDING NORMAL PROBABILITIES: CONVERSION TO THE STANDARD NORMAL

# Let X be normal random variable with mean = mu, standard deviation = sigma.

# Z = (X - mu)/sigma

#-------------------------------------------------------------------------------

# IQ examination scores for sixth-graders are normally distributed with mean
# value 100 and standard deviation 14.2.

IQ_mean <- 100
IQ_SD <- 14.2


# a) What is the probability a randomly chosen sixth-grader has a score greater than 130?
# P(X > 130) = 1 - P(X <= 130)
IQ_a <- 1 - pnorm((130-100)/IQ_SD) # 0.017

# b) What is the probability a randomly chosen sixth-grader has a score between 90 and 115?
# P(90 < X < 115) = P(X < 115) - P(X < 90)
IQ_b <- pnorm((115-100)/IQ_SD) - pnorm((90 - 100)/IQ_SD) # 0.614 --- if rounded before # 0.612 which is the book's answer.

#-------------------------------------------------------------------------------

# Let X be normal with mean μ and standard deviation σ . Find
# (a) P {|X − μ| > σ } = 2P{abs(Z) > 1}
UNKN_a <- 2*(1 - pnorm(1))

# (b) P {|X − μ| > 2σ } = 2P(abs(Z) > 2}
UNKN_b <- 2*(1 - pnorm(2))

# (c) P {|X − μ| > 3σ } = 2P{abs(Z) > 3}
UNKN_c <- 2*(1 - pnorm(3))

#-------------------------------------------------------------------------------

# LET X and Y be independent normal random variables with mu_x and mu_y and SD sigma_x and sigma_y
# Then X + Y is normal random variable with mean mu_x + mu_y and SD sqrt(sigma^2_x + sigma^2_y)
 
# Suppose the amount of time a light bulb works before burning out is a
# normal random variable with mean 400 hours and standard deviation 40
# hours. If an individual purchases two such bulbs, one of which will be used
# as a spare to replace the other when it burns out, what is the probability
# that the total life of the bulbs will exceed 750 hours?
single_bulb_mean <- 400

#P(X+Y > 750) == 1 - P(X+Y < 750), mean = mu_x + mu_y = 400 + 400, SD = sqrt(40^2 + 40^2)
bulbs_mean <- 400 + 400
bulbs_sd <- sqrt(40^2 + 40^2)
bulbs_zscore <- (750 - bulbs_mean)/bulbs_sd
bulbs_prob <- 1 - pnorm(bulbs_zscore) # 81.1%

#-------------------------------------------------------------------------------

# Data from the U.S. Department of Agriculture indicate that the annual
# amount of apples eaten by a randomly chosen woman is normally distributed with a mean of 19.9 pounds
# and a standard deviation of 3.2
# pounds, whereas the amount eaten by a randomly chosen man is normally
# distributed with a mean of 20.7 pounds and a standard deviation of 3.4
# pounds. Suppose a man and a woman are randomly chosen. 
woman_mean <- 19.9
woman_sd <- 3.2
man_mean <- 20.7
man_sd <- 3.4

# X = eaten by woman, Y = eaten by man
# What is the probability that the woman(X) ate a greater amount of apples than the man(Y)?

# E(X-Y) = E(X) + (-1)E(Y) = 19.9 + (-20.7)
woman_man_expvalue <- 19.9 + (-1)*20.7
# SD(X-Y) = SD(X) + (-1)SD(Y) = sqrt(3.2^2 + 3.4^2)
woman_man_sd <- sqrt((3.2^2) + (3.4)^2)

# P(X > Y) = P(X - Y > 0)
# P( (X+Y) - (-0.8) / woman_man_sd) ==>( X+Y + 0.8 / woman_man_sd ) > 0.8/woman_man_sd

woman_man_zscore <- 0.8/woman_man_sd
#P(Z > 0.17) = 1 - P(Z < 0.17)
woman_man_prob <- 1 - pnorm(0.17)
# SAME AS vvvvvvvvvv ^^^^^^^^^^
woman_man_straight_prob <- pnorm(0.17, lower.tail = FALSE) 

#-------------------------------------------------------------------------------

# PERCENTILES OF NORMAL RANDOM VARIABLES

# For any α between 0 and 1, we define z_alpha to be that value for which
# P {Z > z_alpha } = alpha

# We can determine the value of z_alpha by using Table 6.1. For instance, suppose we
# want to find z_0.025 . Since
# P {Z < z_0.025 } = 1 − P {Z > z_0.025 } = 0.975
# we search in Table 6.1 for the entry 0.975, and then we find the value x that
# corresponds to this entry. Since the value 0.975 is found in the row labeled 1.9
# and the column labeled 0.06, we see that
# z0.025 = 1.96

# CAN ALSO BE FOUND BY qnorm(1-0.025) = 1.959964

#-------------------------------------------------------------------------------

# Find

# (a) z0.25 = P(Z > Z_0.25)
N_fa <- qnorm(1-0.25)

# (b) z0.80 = P(Z > Z_0.8) = P(Z < -z_0.8)
N_fb <- qnorm(0.8)

#-------------------------------------------------------------------------------

# An IQ test produces scores that are normally distributed with mean value
# 100 and standard deviation 14.2. The top 1 percent of all scores is in what
# range?

QI_mean <- 100
QI_sd <- 14.2

#P(X > x) = 0.01 == 1%
QI_zscore = qnorm(1-0.01) # 2.33

# x-100 / 14.2 = 2.33 || * 14.2
# x-100 = 33.086 || + 100
# X = 133.086

QI_TEST_MEAN <- 100
QI_TEST_SD <- 14.8

#P(X > x) = 0.04 == 4%

QI_TEST_ZCORE = qnorm(1-0.04)

# x - 100 / 14.8 = 1.75 || * 14.8
# x - 100  = 25.9 || + 100
# x = 125.9

#-------------------------------------------------------------------------------

# CALCULATING NORMAL PROBABILITIES WITH R
# Let Z be a standard normal. Then
# pnorm(x) returns P (Z ≤ x)290 CHAPTER 6: Normal Random Variables
# More generally, if X is normal with mean μ and variance σ 2 , then
# pnorm(x, μ, σ ) returns P (X ≤ x)
# Of course, we could also have obtained P (X ≤ x) by
# y = (x − μ)/σ
# pnorm(y)
# For instance, if X is normal with mean 10 and variance 9, then P (X ≤ 12) can
# be obtained either by
# >
#   [1]
pnorm(12, 10, 3)
# 0.7475075
# or by
# >
#   >
#   [1]
y = (12 - 10)/3
pnorm(y)
# 0.7475075
# or by
# >
#   [1]
pnorm((12 - 10)/3)
# 0.7475075
# To determine the value zα such that
# P (Z > zα ) = α
# when Z is a standard normal random variable, just type qnorm(1 − α). That
# is,
# qnorm(1 - α) returns zα
# For instance, z.01 is obtained as follows:
#   >
qnorm(1 - .01)
2.326348


