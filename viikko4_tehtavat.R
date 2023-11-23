# (Sivu 554, tehtävä 4) Seuraava aineisto kertoo 10 yliopisto-opiskelijan pääsykokeen pis-
#   teiden määrän ja heidän kurssiarvosanojensa keskiarvon ensimmäisen opiskeluvuoden
# lopuksi.
pk <- c(88, 74, 70, 77, 83, 94, 92, 81, 85, 92) #Pääsykokeen pistemäärä 
ka <- c(3.2, 2.7, 2.3, 2.9, 2.8, 3.6, 3.0, 2.8, 3.3, 3.1) #Kurssiarvosanojen keskiarvo 
t1_data <- data.frame(paasykoe = pk,
                      keskiarvo = ka)
t1_fitted <- lm(keskiarvo ~ paasykoe, data = t1_data)
summary(t1_fitted)

# ##### LASKETAAN TÄHÄN NYT OIKEASTI
# 
# 
t1_n <- length(t1_data$paasykoe)
t1_x_bar <- signif(mean(t1_data$paasykoe),6)
t1_y_bar <- signif(mean(t1_data$keskiarvo),6)
# ###### Y #######
# NELJA_jotain_Y <- lapply(nelja_tehtava_data$palkka, function(y) (y)^2)
t1_jotain_y <- lapply(t1_data$keskiarvo, function(y) (y)^2)
# NELJA_ytoiseen_summa <- sum(unlist(NELJA_jotain_Y))
t1_ytoiseen_summa <- sum(unlist(t1_jotain_y))
# NELJA_oikea_SYY <- signif(NELJA_ytoiseen_summa - (nelja_n * (nelja_y_bar)^2),8)
t1_Syy <- signif(t1_ytoiseen_summa - (t1_n * (t1_y_bar)^2),6)
# ###### X ######
# NELJA_jotain_x <- lapply(nelja_tehtava_data$pituus, function(x) (x)^2)
t1_jotain_x <- lapply(t1_data$paasykoe, function(x) (x)^2)
# NELJA_xtoiseen_summa <- sum(unlist(NELJA_jotain_x))
t1_xtoiseen_summa <- sum(unlist(t1_jotain_x))
# NELJA_oikea_Sxx <- signif(NELJA_xtoiseen_summa - (nelja_n * (nelja_x_bar)^2),8)
t1_Sxx <- signif(t1_xtoiseen_summa - (t1_n * (t1_x_bar)^2),6)
# ###### XY ######
# NELJA_x_kertaa_y <- Map("*", nelja_tehtava_data$pituus, nelja_tehtava_data$palkka)
t1_x_kertaa_y <- Map("*", t1_data$paasykoe, t1_data$keskiarvo)
# NELJA_xy_summa <- sum(unlist(NELJA_x_kertaa_y))
t1_xy_summa <- sum(unlist(t1_x_kertaa_y))
# NELJA_oikea_SxY <- signif(NELJA_xy_summa - (nelja_n * nelja_x_bar * nelja_y_bar),8)
t1_SxY <- signif(t1_xy_summa - (t1_n * t1_x_bar * t1_y_bar), 6)
# #### SSR ######
# ssr_nelja <- ((NELJA_oikea_Sxx * NELJA_oikea_SYY)-(NELJA_oikea_SxY)^2) / NELJA_oikea_Sxx
t1_SSR <- signif(((t1_Sxx*t1_Syy-(t1_SxY)^2)/t1_Sxx),6)
# ##### BETA HAT #######
# beta_hat_NELJA <- NELJA_oikea_SxY / NELJA_oikea_Sxx
t1_beta_hat <- signif((t1_SxY / t1_Sxx),6)
# ##### ALPHA HAT ######
# alpha_hat_NELJA <- nelja_y_bar - (beta_hat_NELJA * nelja_x_bar)
t1_alpha_hat <- signif(t1_y_bar - (t1_beta_hat * t1_x_bar),6)


# (a) Ennusta kurssiarvosanojen keskiarvo opiskelijalla
# (ei mukana alkuperäisessä aineistossa), jonka pääsykokeen pistemäärä oli 88.
t1_ennuste_ka <- predict(t1_fitted, data.frame(paasykoe = 88), interval = "conf")
t1_ennuste_ka
# (b) Laske 90% ennusteväli (a)-kohdan opiskelijan keskiarvolle.

t1_ennustevali <- predict(t1_fitted, data.frame(paasykoe = 88), interval = "pred", level = 0.9)
t1_ennustevali
# (c) Testaa 5% merkitsevyystasolla onko opiskelijan kurssiarvosanojen keskiarvo riip-
#   pumaton opiskelijan pääsykokeen pisteistä


# 
# #Test statistic = sqrt( ((n-2)*Sxx) / SS_R) * beta_hat
# nelja_test_statistic <- (sqrt( ((nelja_n - 2) * NELJA_oikea_Sxx) / ssr_nelja)) * beta_hat_NELJA
t1_test_statistic <- (sqrt( ((t1_n - 2) * t1_Sxx) / t1_SSR)) * t1_beta_hat
# # Simple linear regression model
# # H_0 beta = 0 against beta != 0
# # REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
# # WHERE:
# # TS = sqrt( ((n-2)*S_xx) / SS_r ) * beta_hat
# 
# # T_CRIT = T_(n-2),gammapercentage(0.05)/2
# nelja_T_viisi_crit <- qt(1-0.025, nelja_n - 2)
t1_T_viisi_crit <- qt(1-0.025, t1_n - 2)
# nelja_T_yksi_crit <- qt(1-0.005, nelja_n - 2)
# # REJECT H_0: abs(TS) >= t_{n-2,gamma/2}
# nelja_H_0_viisi_rejected <- ifelse(abs(nelja_test_statistic) >= nelja_T_viisi_crit,TRUE,FALSE)
t1_H_0_viisi_rejected <- ifelse(abs(t1_test_statistic) >= t1_T_viisi_crit, TRUE, FALSE)
# nelja_H_0_yksi_rejected <- ifelse(abs(nelja_test_statistic) >= nelja_T_yksi_crit, TRUE, FALSE)
# #####P-VALUE######
# nelja_viisi_alpha <- 0.05
t1_viisi_alpha <- 0.05
# nelja_yksi_alpha <- 0.01
# nelja_p_value <- 2 * pt(4.3809, df=10, lower.tail = FALSE)
t1_p_value <- 2 * pt(t1_test_statistic, df=(t1_n - 2), lower.tail = FALSE)

t1_pvalue_viisi_reject <- ifelse(t1_p_value<t1_viisi_alpha,TRUE,FALSE)
# nelja_pvalue_yksi_reject <- ifelse(nelja_p_value<nelja_yksi_alpha,TRUE,FALSE)


# tarkastellaan allaolevaa aineistoa, jossa on listattuna kahdeksan sa-
#   tunnaisesti valitun, ei verenpainelääkettä käyttävän miehen painoindeksit (BMI) sekä systoliset ve-
#   renpaineet.
t2_data <- data.frame(BMI = c(20.3, 22.0, 26.4, 28.2, 31.0, 32.6, 17.6, 19.4),
                      verenpaine = c(116, 110, 131, 136, 144, 138, 122, 115))
t2_fitted <- lm(verenpaine ~ BMI, data = t2_data)
summary(t2_fitted)


# t1_n <- length(t1_data$paasykoe)
t2_n <- length(t2_data$BMI)
# t1_x_bar <- signif(mean(t1_data$paasykoe),6)
t2_x_bar <- signif(mean(t2_data$BMI),6)
# t1_y_bar <- signif(mean(t1_data$keskiarvo),6)
t2_y_bar <- signif(mean(t2_data$verenpaine),6)
###### X ######
# t1_jotain_x <- lapply(t1_data$paasykoe, function(x) (x)^2)
t2_jotain_x <- lapply(t2_data$BMI, function(x) (x)^2)
# t1_xtoiseen_summa <- sum(unlist(t1_jotain_x))
t2_xtoiseen_summa <- sum(unlist(t2_jotain_x))
# t1_Sxx <- signif(t1_xtoiseen_summa - (t1_n * (t1_x_bar)^2),6)
t2_Sxx <- signif(t2_xtoiseen_summa - (t2_n * (t2_x_bar)^2), 6)
# # ###### Y #######
# t1_jotain_y <- lapply(t1_data$keskiarvo, function(y) (y)^2)
t2_jotain_y <- lapply(t2_data$verenpaine, function(y) (y)^2)
# t1_ytoiseen_summa <- sum(unlist(t1_jotain_y))
t2_ytoiseen_summa <- sum(unlist(t2_jotain_y))
# t1_Syy <- signif(t1_ytoiseen_summa - (t1_n * (t1_y_bar)^2),6)
t2_Syy <- signif(t2_ytoiseen_summa - (t2_n * (t2_y_bar)^2), 6)
# # ###### XY ######
# t1_x_kertaa_y <- Map("*", t1_data$paasykoe, t1_data$keskiarvo)
t2_x_kertaa_y <- Map("*", t2_data$BMI, t2_data$verenpaine)
# t1_xy_summa <- sum(unlist(t1_x_kertaa_y))
t2_xy_summa <- sum(unlist(t2_x_kertaa_y))
# t1_SxY <- signif(t1_xy_summa - (t1_n * t1_x_bar * t1_y_bar), 6)
t2_SxY <- signif(t2_xy_summa - (t2_n * t2_x_bar * t2_y_bar), 6)
# # #### SSR ######
# t1_SSR <- signif(((t1_Sxx*t1_Syy-(t1_SxY)^2)/t1_Sxx),6)
t2_SSR <- signif(((t2_Sxx*t2_Syy-(t2_SxY)^2)/t2_Sxx), 6)
# # ##### BETA HAT #######
# t1_beta_hat <- signif((t1_SxY / t1_Sxx),6)
t2_beta_hat <- signif((t2_SxY / t2_Sxx), 6)
# # ##### ALPHA HAT ######
# t1_alpha_hat <- signif(t1_y_bar - (t1_beta_hat * t1_x_bar),6)
t2_alpha_hat <- signif(t2_y_bar - (t2_beta_hat * t2_x_bar), 6)

# Laske 95 prosentin ennusteväli verenpaineelle sellaiselle miehelle, joka ei käytä verenpainelääkettä
# ja jonka painoindeksi on 26.0.

# ensin Y ennuste
t2_ennuste_verenpaine <- predict(t2_fitted, data.frame(BMI = 26.0), interval = "conf")
t2_ennuste_verenpaine
# ennuste käsin == sama
# W #
t2_w <- round(sqrt((1 + (1/(t2_n)) + ((26.0 - t2_x_bar)^2)/t2_Sxx) * (t2_SSR / (t2_n - 2))),6)

# X nolla #
t2_xennuste <- 26.0
t2_kasin_ennuste <- t2_alpha_hat + t2_beta_hat*t2_xennuste
t2_ennustevali <- predict(t2_fitted, data.frame(BMI = 26.0), interval = "pred", level = 0.95)
t2_ennustevali
t2_T_viisi_crit <- qt(1-(0.05/2), t2_n - 2)

