tehtava_2_data <- data.frame(
  tyontekijat = c(9,11,12,13,15,18,16,14,12,10),
  havikki = c(420,350,360,300,225,200,230,280,315,410)
)

#plot(havikki ~ tyontekijat, data = tehtava_2_data)
#t2_model <- lm(havikki ~ tyontekijat, data = tehtava_2_data)
#abline(t2_model)


nelja_tehtava_data <- data.frame(
  pituus = c(183,160,196,186,195,182,208,169,166,198,145,168),
  palkka = c(100,92,106,92,109,101,116,96,94,101,93,96)
)

nelja_tehtava_model <- lm(palkka ~ pituus, data = nelja_tehtava_data)
plot(palkka ~ pituus, data = nelja_tehtava_data)
abline(nelja_tehtava_model)
summary(nelja_tehtava_model)



##### LASKETAAN TÄHÄN NYT OIKEASTI


nelja_n <- length(nelja_tehtava_data$pituus)
nelja_x_bar <- signif(mean(nelja_tehtava_data$pituus),6)
nelja_y_bar <- signif(mean(nelja_tehtava_data$palkka),6)
###### Y #######
NELJA_jotain_Y <- lapply(nelja_tehtava_data$palkka, function(y) (y)^2)
NELJA_ytoiseen_summa <- sum(unlist(NELJA_jotain_Y))
NELJA_oikea_SYY <- signif(NELJA_ytoiseen_summa - (nelja_n * (nelja_y_bar)^2),8)
###### X ######
NELJA_jotain_x <- lapply(nelja_tehtava_data$pituus, function(x) (x)^2)
NELJA_xtoiseen_summa <- sum(unlist(NELJA_jotain_x))
NELJA_oikea_Sxx <- signif(NELJA_xtoiseen_summa - (nelja_n * (nelja_x_bar)^2),8)
###### XY ######
NELJA_x_kertaa_y <- Map("*", nelja_tehtava_data$pituus, nelja_tehtava_data$palkka)
NELJA_xy_summa <- sum(unlist(NELJA_x_kertaa_y))
NELJA_oikea_SxY <- signif(NELJA_xy_summa - (nelja_n * nelja_x_bar * nelja_y_bar),8)
##### BETA HAT #######
beta_hat_NELJA <- NELJA_oikea_SxY / NELJA_oikea_Sxx
##### ALPHA HAT ######
alpha_hat_NELJA <- nelja_y_bar - (beta_hat_NELJA * nelja_x_bar)
#### SSR ######
ssr_nelja <- ((NELJA_oikea_Sxx * NELJA_oikea_SYY)-(NELJA_oikea_SxY)^2) / NELJA_oikea_Sxx

#Test statistic = sqrt( ((n-2)*Sxx) / SS_R) * beta_hat
nelja_test_statistic <- (sqrt( ((nelja_n - 2) * NELJA_oikea_Sxx) / ssr_nelja)) * beta_hat_NELJA
# Simple linear regression model
# H_0 beta = 0 against beta != 0
# REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
# WHERE:
# TS = sqrt( ((n-2)*S_xx) / SS_r ) * beta_hat

# T_CRIT = T_(n-2),gammapercentage(0.05)/2
nelja_T_viisi_crit <- qt(1-0.025, nelja_n - 2)
nelja_T_yksi_crit <- qt(1-0.005, nelja_n - 2)
# REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
nelja_H_0_viisi_rejected <- ifelse(abs(nelja_test_statistic) >= nelja_T_viisi_crit,TRUE,FALSE)
nelja_H_0_yksi_rejected <- ifelse(abs(nelja_test_statistic) >= nelja_T_yksi_crit, TRUE, FALSE)
#####P-VALUE######
nelja_viisi_alpha <- 0.05
nelja_yksi_alpha <- 0.01
nelja_p_value <- 2 * pt(4.3809, df=10, lower.tail = FALSE)
nelja_pvalue_viisi_reject <- ifelse(nelja_p_value<nelja_viisi_alpha,TRUE,FALSE)
nelja_pvalue_yksi_reject <- ifelse(nelja_p_value<nelja_yksi_alpha,TRUE,FALSE)
