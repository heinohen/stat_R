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
# t2_crit <- qf((1- t2_alpha), (t2_m - 1), (t2_m*(t2_n - 1)))
# N = (n − 1)(m − 1)
t4_N <- (length(t3_data[,1])-1) * (length(t3_data[1,]) - 1)