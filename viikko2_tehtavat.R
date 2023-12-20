## VIIKKO 2

# TEHTÄVÄ 2
# Jaakko sai käsiinsä kolmea eri taikapapulajiketta.
# Hän haluaa selvittää, kasvaako jokaisesta lajikkeesta yhtä korkeita pavunvarsia.
# Jaakko istutti illalla 5 kappaletta jokaista taikapapulajiketta
# ja aamulla mittasi yön aikana kasvaneiden
# pavunvarsien korkeudet saaden seuraavat havainnot:
# Testaa kasvaako taikapapulajikkeista keskimäärin yhtä korkeita pavunvarsia. Minkä
# johtopäätöksen voit 

# tehdä 5% merkitsevyystasolla? 
t2_data <- data.frame(papu1 = c(2.3,2.9,2.3,1.9,2.5),
                      papu2 = c(1.1,2.1,0.9,1.8,1.5),
                      papu3 = c(3.2,2.9,2.5,3.7,3.1))


##### SAMA TESTI vvvvvvv
t2_data_testi <- data.frame(sample = c(2.3,2.9,2.3,1.9,2.5,
                                       1.1,2.1,0.9,1.8,1.5,
                                       3.2,2.9,2.5,3.7,3.1),
                            group = factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
                            labels=c('papu1','papu2','papu3')))

t2_oneway <- oneway.test(sample ~ group, data = t2_data_testi, var.equal = TRUE)
t2_oneway

##### SAMA TESTI ^^^^^^ 


# otoskeskiarvot
t2_papu1_xbar <- mean(t2_data$papu1)
t2_papu2_xbar <- mean(t2_data$papu2)
t2_papu3_xbar <- mean(t2_data$papu3)
# otosvarianssit
t2_papu1_var <- var(t2_data$papu1)
t2_papu2_var <- var(t2_data$papu2)
t2_papu3_var <- var(t2_data$papu3)
#arvoja
t2_m <- length(t2_data)
t2_n <- length(t2_data$papu1)
## x barbar
t2_xbarbar <- (t2_papu1_xbar + t2_papu2_xbar + t2_papu3_xbar) /3
#S_bar^2
t2_s_bar_2 <- ((t2_papu1_xbar - t2_xbarbar)^2 + (t2_papu2_xbar - t2_xbarbar)^2 + (t2_papu3_xbar - t2_xbarbar)^2 ) / (t2_m - 1)
# sum( (mean(S_i^2))
t2_s_i_2 <- (t2_papu1_var + t2_papu2_var + t2_papu3_var) / t2_m

# TS = (n* s_bar_2) / sum( (mean(S_i^2))
t2_ts <- (t2_n * t2_s_bar_2) / t2_s_i_2
# CRIT
t2_alpha <- 0.05
t2_crit <- qf((1- t2_alpha), (t2_m - 1), (t2_m*(t2_n - 1)))
# suuret arvot kriittisiä
t2_viisi_rejected <- ifelse(t2_ts > t2_crit, TRUE, FALSE)
# tai p-arvolla
t2_p_value <- pf(t2_ts, df1 = (t2_m - 1), df2 = (t2_m*(t2_n - 1)), lower.tail = FALSE)
t2_viisi_rejected_pvalue <- ifelse(t2_p_value < t2_alpha, TRUE, FALSE)
# Entä 1% merkitsevyystasolla?
t2_alpha_yksi <- 0.01
t2_crit_yksi <- qf((1-t2_alpha_yksi), (t2_m-1), (t2_m * (t2_n-1)))
t2_yksi_rejected <- ifelse(t2_ts > t2_crit_yksi, TRUE, FALSE)
t2_yksi_rejected_pvalue <- ifelse(t2_p_value < t2_alpha_yksi, TRUE, FALSE)

# TEHTÄVÄ 3

# Viittä eri kauralajiketta istu-
#   tettiin jokaiselle neljästä pellosta. Saatiin seuraavat sadot
# Pelto
# Kauralajike 1 2 3 4
# 1 296 357 340 348
# 2 402 390 420 335
# 3 345 342 358 308
# 4 360 322 336 270
# 5 324 339 357 308
# Estimoi mallin E(Xij ) = μ + αi + βj tuntemattomat parametrit.

t3_data <- matrix(c(296,357,340,348,
                    402,390,420,335,
                    345,342,358,308,
                    360,322,336,270,
                    324,339,357,308), nrow = 5, ncol = 4, byrow = TRUE)
###############################

# kauran mukaan alkaa 11112222333344445555
# 
# t2_data_testi <- data.frame(sample = c(2.3,2.9,2.3,1.9,2.5,
#                                        1.1,2.1,0.9,1.8,1.5,
#                                        3.2,2.9,2.5,3.7,3.1),
#                             group = factor(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
#                                            labels=c('papu1','papu2','papu3')))



t3_owt_testi <- data.frame(sample = c(296,357,340,348,
                                      402,390,420,335,
                                      345,342,358,308,
                                      360,322,336,270,
                                      324,339,357,308),
                           kaura = factor(c(1,1,1,1,
                                            2,2,2,2,
                                            3,3,3,3,
                                            4,4,4,4,
                                            5,5,5,5),
                                          labels=c('k1','k2','k3','k4','k5')),
                           pelto = factor(c(1,2,3,4,
                                            1,2,3,4,
                                            1,2,3,4,
                                            1,2,3,4,
                                            1,2,3,4),
                                          labels=c('p1','p2','p3','p4')))

                            
kaura <- oneway.test(t3_owt_testi$sample ~ t3_owt_testi$kaura, var.equal = TRUE)              
kaura$statistic

pelto <- oneway.test(t3_owt_testi$sample ~ t3_owt_testi$pelto, var.equal = TRUE)
pelto$statistic
##################################


# yleiskeskiarvo
# (kaikki arvot) / rivit * sarakkeet
t3_yleis_ka <- mean(t3_data)

# rivikeskiarvot X_i. i = 1,2,3,4,5
t3_rivi1_ka <- mean(t3_data[1,])
t3_rivi2_ka <- mean(t3_data[2,])
t3_rivi3_ka <- mean(t3_data[3,])
t3_rivi4_ka <- mean(t3_data[4,])
t3_rivi5_ka <- mean(t3_data[5,])

# rivipoikkeamat
t3_rpoik_1 <- t3_rivi1_ka - t3_yleis_ka
t3_rpoik_2 <- t3_rivi2_ka - t3_yleis_ka
t3_rpoik_3 <- t3_rivi3_ka - t3_yleis_ka
t3_rpoik_4 <- t3_rivi4_ka - t3_yleis_ka
t3_rpoik_5 <- t3_rivi5_ka - t3_yleis_ka

# sarakekeskiarvot X.i i = 1,2,3,4
t3_sar1_ka <- mean(t3_data[,1])
t3_sar2_ka <- mean(t3_data[,2])
t3_sar3_ka <- mean(t3_data[,3])
t3_sar4_ka <- mean(t3_data[,4])

# sarakepoikkeamat
t3_spoik_1 <- t3_sar1_ka - t3_yleis_ka
t3_spoik_2 <- t3_sar2_ka - t3_yleis_ka
t3_spoik_3 <- t3_sar3_ka - t3_yleis_ka
t3_spoik_4 <- t3_sar4_ka - t3_yleis_ka

# TEHTÄVÄ 4
# (Sivu 511, tehtävä 3, jatkoa tehtävään 3) 
# Viittä eri kauralajiketta istutettiin jokaiselle neljästä pellosta
# ja saatiin sadot kuten tehtävässä 3. Testaa (tavanomaisilla merkitse-
# vyystasoilla) onko aineiston perusteella sadon määrällä eroa

t4_n <- length(t3_data[1,])
t4_m <- length(t3_data[,1])
# rivineliösumma
# SSr = n * rivipoikkeamat^2
t4_SSr <- t4_n * ((t3_rpoik_1)^2 + (t3_rpoik_2)^2 + (t3_rpoik_3)^2 + (t3_rpoik_4)^2 + (t3_rpoik_5)^2)
# sarakeneliösumma
# SSc = m * sarakepoikkeamat^2
t4_SSc <- t4_m * ((t3_spoik_1)^2 + (t3_spoik_2)^2 + (t3_spoik_3)^2 + (t3_spoik_4)^2)

#jäännösneliösumma
# SSe 

jaannosnelio <- function(mat) {
  kaikki <- 0
  for (i in 1:nrow(mat))   
    for (j in 1:ncol(mat))
      kaikki <- kaikki +  (mat[i,j] - mean(mat[i,]) - mean(mat[,j]) + t3_yleis_ka)^2 
  return(kaikki)    
  }
    
t4_SSe <- jaannosnelio(t3_data)

#testisuureet

# crit
t4_viis_alpha <- 0.05
t4_yksi_alpha <- 0.01

# N = (n − 1)(m − 1)
t4_N <- (length(t3_data[,1])-1) * (length(t3_data[1,]) - 1)

# TS_ROW
t4_ts_row <- (t4_SSc / ( t4_n - 1)) / (t4_SSe / t4_N)
# F_(m-1, N, alpha) ROW
# VIISI 
t4_crit_row <- qf((1 - t4_viis_alpha), (t4_n - 1), t4_N)
# YKSI
t4_crit_row_yksi <- qf((1- t4_yksi_alpha), (t4_m - 1), t4_N)
# REJECT ?
t4_rejected_row <- ifelse(t4_ts_row >= t4_crit_row, TRUE, FALSE)
t4_rejected_row_yksi <- ifelse(t4_ts_row >= t4_crit_row_yksi, TRUE, FALSE)
# P-VALUE ROW == TS, m-1, N, lowertail FALSE
t4_pvalue_row <- pf(t4_ts_row, (t4_m-1), t4_N, lower.tail = FALSE)
t4_rejected_row_pvalue <- ifelse(t4_pvalue_row<t4_viis_alpha, TRUE, FALSE)
t4_rejected_row_pvalue_yksi <- ifelse(t4_pvalue_row<t4_yksi_alpha, TRUE, FALSE)

# TS COL
t4_ts_col <- (t4_SSr / ( t4_m - 1)) / (t4_SSe / t4_N)
# F_(n-1, N, alpha) COL
# VIISI
t4_crit_col <- qf((1- t4_viis_alpha), (t4_m -1), t4_N)
# YKSI
t4_crit_col_yksi <- qf((1 - t4_yksi_alpha), (t4_n -1), t4_N)
# REJECT ?
t4_rejected_col <- ifelse(t4_ts_col >= t4_crit_col, TRUE, FALSE)
t4_rejected_col_yksi <- ifelse(t4_ts_col>= t4_crit_col_yksi, TRUE, FALSE)
# P-VALUE COL TS, n-1, N, lowertail FALSE
t4_pvalue_col <- pf(t4_ts_col, (t4_n -1) , t4_N, lower.tail = FALSE)
t4_rejected_col_pvalue <- ifelse(t4_pvalue_col<t4_viis_alpha, TRUE, FALSE)
t4_rejected_col_pvalue_yksi <- ifelse(t4_pvalue_col<t4_yksi_alpha, TRUE, FALSE)

# TEHTÄVÄ 5
# 
# Ravitsemusterapeutti väittää, että määrällä, jonka henkilö juoksee,
# on yhteys kyseisen henkilön veren kolesteroliarvoihin.
# Valittiin satunnaisesti 6 juoksijaa kolmesta eri kategoriasta
# ja heidän veren kolesteroliarvonsa testattiin.
# Saatiin seuraavat otoskeskiarvot ja otosvarianssit:
                                # x_bar_i s_i^2
t5_data <- data.frame(alle_viis = c(188,190),
                      viis_kolme = c(181,211),
                      yli_kolme = c(174,202))
t5_m <- length(t5_data)
t5_n <- length((t5_data$alle_viis)) *length(t5_data[1,])

t5_xbarbar <- (sum(t5_data[1,]) / length(t5_data[1,]))
t5_s_bar_2 <- ((t5_data$alle_viis[1] - t5_xbarbar)^2 + (t5_data$viis_kolme[1] - t5_xbarbar)^2 + (t5_data$yli_kolme[1]- t5_xbarbar)^2) / (t5_m -1)
t5_s_i_2 <- (sum(t5_data[2,]) / length(t5_data[2,]))

# TS = n*s_bar_2 / sum 1/m s_i_^2
t5_ts <- (t5_n * t5_s_bar_2) / t5_s_i_2

# CRIT 5% m-1, m*(n-1)
t5_alpha <- 0.05
t5_crit <- qf((1-t5_alpha), 2, 15)
t5_rejected <- ifelse(t5_ts >= t5_crit, TRUE, FALSE)
# P-value TS, m-1, m*(n-1)
t5_pvalue <- pf(t5_ts, (t5_m - 1), t5_m * (t5_n -1))


### TENTTI

## banaani / lannaoite

tehtava_N <- 12
lannoite <- 3
tehtava_SSr <- 3.4
tehtava_SSc <- 1.3
tehtava_SSe <- 1.2


# # TS COL
# t4_ts_col <- (t4_SSr / ( t4_m - 1)) / (t4_SSe / t4_N)
lannoite_ts <- (tehtava_SSc / (lannoite - 1)) / (tehtava_SSe / tehtava_N)
# # F_(n-1, N, alpha) COL
# # VIISI
# t4_crit_col <- qf((1- t4_viis_alpha), (t4_m -1), t4_N)
# # YKSI
# t4_crit_col_yksi <- qf((1 - t4_yksi_alpha), (t4_n -1), t4_N)
# # REJECT ?
# t4_rejected_col <- ifelse(t4_ts_col >= t4_crit_col, TRUE, FALSE)
# t4_rejected_col_yksi <- ifelse(t4_ts_col>= t4_crit_col_yksi, TRUE, FALSE)
# # P-VALUE COL TS, n-1, N, lowertail FALSE
# t4_pvalue_col <- pf(t4_ts_col, (t4_n -1) , t4_N, lower.tail = FALSE)
# t4_rejected_col_pvalue <- ifelse(t4_pvalue_col<t4_viis_alpha, TRUE, FALSE)
# t4_rejected_col_pvalue_yksi <- ifelse(t4_pvalue_col<t4_yksi_alpha, TRUE, FALSE)


pf(lannoite_ts,2,12, lower.tail = FALSE)


