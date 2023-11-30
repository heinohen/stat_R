# TEHTÄVÄ 1
# 
# Eteläisen Kalifornian alueella haluttiin tutkia,
# esiintyykö maan- järistyksiä (voimakkuus vähintään 4.4 Richterin asteikolla mitattuna)
# todennäköisemmin joinakin tiettyinä viikonpäivinä.
# Tutkimusta varten kerättiin seuraavat tiedot koskien 1100 maanjäristystä:
# Viikonpäivä ma ti ke to pe la su Järistystenlkm 
t1_P_i <- 1/7
t1_jaristykset <- c(144, 170, 158, 172, 148, 152, 156)
t1_probz <- c(t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i)
t1_alpha <- 0.05
t1_n <- 1100

# Testaa hypoteesia, jonka mukaan maanjäristys tapahtuu yhtä todennäköisesti
# minä tahansa viikonpäivänä. Käytä viiden prosentin merkitsevyystasoa.
t1_chisq <- chisq.test(t1_jaristykset)
t1_chisq
t1_e_i <- signif((t1_n * t1_P_i),5)

# (Ni - ei)^2 / ei
t1_per_paiva <- lapply(t1_jaristykset, function(x) (x - t1_e_i)^2 / t1_e_i)
t1_per_paiva
t1_ts <- sum(unlist(t1_per_paiva))

# crit qchisq(Pi, k-1)
t1_crit <- qchisq(1-(t1_alpha), length(t1_jaristykset)-1)
t1_rejected_ts <- ifelse(t1_ts >= t1_crit, TRUE, FALSE)

# pvalue P(X^2_k-1 >= TS)
t1_pvalue <- 1 - pchisq(t1_ts, length(t1_jaristykset)-1)
t1_rejected_pvalue <- ifelse(t1_ts < t1_alpha, TRUE, FALSE)


# TEHTÄVÄ 2

# On ollut näyttöä, että koiran omistaminen saattaa ennustaa selviääkö
# henkilö sydänkohtauksesta hengissä.
# Seuraava aineisto on 95 sydänkohtauksen saaneen henkilön satunnaisotos.
# Satunnaisotos on jaoteltu luokkiin sen perusteella, onko henkilöllä koira ja
# selvisikö hän sydänkohtauksesta hengissä.
# Koira Ei koiraa
# Selvisi 28 44
# Ei selvinnyt 8 15
# Todistaako tämä aineisto 5% merkitsevyystasolla,
# että koiran omistaminen ja sydän- kohtauksesta selviäminen ovat riippuvaisia?
# Kerro huolellisesti mitä nollahypoteesia testaat
# (kerro myös vastahypoteesi) ja mitä testisuuretta käytät.

#b_matriisi <- matrix(c(4,10,6,8,6,6), nrow = 3, ncol = 2, byrow = FALSE)

t2_matriisi <- matrix(c(28,44,8,15), nrow=2, ncol=2,byrow = TRUE)
t2_matriisi[1,1] # <--- 28

#my_matrix <- cbind(my_matrix, c(2, 7, 7, 8))
t2_r1_sum <- sum(t2_matriisi[1,])
t2_r2_sum <- sum(t2_matriisi[2,])
t2_s1_sum <- sum(t2_matriisi[,1])
t2_s2_sum <- sum(t2_matriisi[,2])

t2_m_sum <- t2_matriisi[1,1] + t2_matriisi[1,2] + t2_matriisi[2,1] + t2_matriisi[2,2]

t2_matriisi <- cbind(t2_matriisi, c(t2_r1_sum, t2_r2_sum))
t2_matriisi <- rbind(t2_matriisi, c(t2_s1_sum, t2_s2_sum, t2_m_sum))


# selvinneet ----> rivi 1 Eij = (n_r * n_s) / n
#ODOTUSARVOT
t2_selvisi_expected_koira <- (t2_r1_sum * t2_s1_sum) / t2_m_sum
t2_selvisi_expected_ei_koiraa <- (t2_r1_sum * t2_s2_sum) / t2_m_sum
t2_ei_selvinnyt_expected_koira <- (t2_r2_sum * t2_s1_sum) / t2_m_sum
t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum

# SELVISI / KOIRA
t2_s_k <- (((t2_matriisi[1,1] - t2_selvisi_expected_koira)^2) / t2_selvisi_expected_koira)
# SELVISI / EI KOIRAA
t2_s_ek <- (((t2_matriisi[1,2] - t2_selvisi_expected_ei_koiraa)^2) / t2_selvisi_expected_ei_koiraa)
# EI SELVINNYT / KOIRA
t2_es_k <- (((t2_matriisi[2,1] - t2_ei_selvinnyt_expected_koira)^2) / t2_ei_selvinnyt_expected_koira)
# EI SELVINNYT / EI KOIRAA
t2_es_ek <- (((t2_matriisi[2,2] - t2_ei_selvinnyt_expected_ei_koiraa)^2) / t2_ei_selvinnyt_expected_ei_koiraa)

# CHI^2
t2_chi <- t2_s_k + t2_s_ek + t2_es_k + t2_es_ek

# CRIT
t2_alpha <- 0.05
t2_df <- (2-1) * (2-1)
# qchisq(1-alpha,(r-1)*(s-1))
t2_crit <- qchisq(1-t2_alpha, t2_df)

# REJECT TS >= x^2_(r-1)(s-1),alpha
t2_rejected_ts <- ifelse(t2_chi >= t2_crit, TRUE, FALSE)

# P - ARVO
t2_pvalue <- 1 - pchisq(t2_chi, t2_df)
# REJECT Pvalue < alpha
t2_rejected_pvalue <- ifelse(t2_pvalue < t2_alpha, TRUE, FALSE)

# TEHTÄVÄ 3 
# 
# (Sivu 612, tehtävä 6) Eräällä kahdensadan opiskelijan tilastotieteen kurssilla sataa satunnaisesti
# valittua opiskelijaa pyydettiin seuraamaan kurssin luennot videoituna. Loput sata opiskelijaa seura-
#   sivat luennot paikan päällä luentosalissa. Allaolevassa taulukossa on listattuna opiskelijoiden saamat
# arvosanat kyseiseltä kurssilta.
#                                   5     4      3  Huonompi
# Paikan päällä luentoja seuranneet 22    38    35    5
# Videoituja luentoja seuranneet    18    32    40   10
# Testaa hypoteesia, jonka mukaan opiskelijan saama arvosana on riippumaton siitä, seuraako hän
# kurssin luennot paikan päällä vai videoituna. Voidaanko hypoteesi hylätä viiden prosentin merkit-
#   sevyystasolla? Entä yhden prosentin merkitsevyystasolla?
#   
# 
# 
# t2_matriisi <- matrix(c(28,44,8,15), nrow=2, ncol=2,byrow = TRUE)
t3_matriisi <- matrix(c(22,38,35,5,18,32,40,10),nrow = 2, ncol = 4, byrow = TRUE)
# t2_matriisi[1,1] # <--- 28
t3_chsiq_test <- chisq.test(t3_matriisi)
t3_chsiq_test$expected
# #my_matrix <- cbind(my_matrix, c(2, 7, 7, 8))

# t2_m_sum <- t2_matriisi[1,1] + t2_matriisi[1,2] + t2_matriisi[2,1] + t2_matriisi[2,2]
# 
# t2_matriisi <- cbind(t2_matriisi, c(t2_r1_sum, t2_r2_sum))
t3_matriisi <- cbind(t3_matriisi, c(sum(t3_matriisi[1,]), sum(t3_matriisi[2,])))
# t2_matriisi <- rbind(t2_matriisi, c(t2_s1_sum, t2_s2_sum, t2_m_sum))
t3_matriisi <- rbind(t3_matriisi, c(sum(t3_matriisi[,1]),
                                    sum(t3_matriisi[,2]),
                                    sum(t3_matriisi[,3]),
                                    sum(t3_matriisi[,4]),
                                    sum(t3_matriisi[,5])))
# 
# # paikanpäällä ----> rivi 1 Eij = (n_r * n_s) / n
# #ODOTUSARVOT
t3_pp_expected_viisi <- (t3_matriisi[1,5] * t3_matriisi[3,1]) / t3_matriisi[3,5]
# t2_selvisi_expected_ei_koiraa <- (t2_r1_sum * t2_s2_sum) / t2_m_sum
t3_pp_expected_nelja <- (t3_matriisi[1,5] * t3_matriisi[3,2]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_koira <- (t2_r2_sum * t2_s1_sum) / t2_m_sum
t3_pp_expected_kolme <- (t3_matriisi[1,5] * t3_matriisi[3,3]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum
t3_pp_expected_huono <- (t3_matriisi[1,5] * t3_matriisi[3,4]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum
t3_ei_expected_viisi <- (t3_matriisi[2,5] * t3_matriisi[3,1]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum
t3_ei_expected_nelja <- (t3_matriisi[2,5] * t3_matriisi[3,2]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum
t3_ei_expected_kolme <- (t3_matriisi[2,5] * t3_matriisi[3,3]) / t3_matriisi[3,5]
# t2_ei_selvinnyt_expected_ei_koiraa <- (t2_r2_sum * t2_s2_sum) / t2_m_sum
t3_ei_expected_huono <- (t3_matriisi[2,5] * t3_matriisi[3,4]) / t3_matriisi[3,5]

# 
# # PP / VIISI
# t2_s_k <- (((t2_matriisi[1,1] - t2_selvisi_expected_koira)^2) / t2_selvisi_expected_koira)
t3_pp_viisi <- (((t3_matriisi[1,1] - t3_pp_expected_viisi)^2) / t3_pp_expected_viisi)
# # PP / NELJA
# t2_s_ek <- (((t2_matriisi[1,2] - t2_selvisi_expected_ei_koiraa)^2) / t2_selvisi_expected_ei_koiraa)
t3_pp_nelja <- (((t3_matriisi[1,2] - t3_pp_expected_nelja)^2) / t3_pp_expected_nelja)
# # PP / KOLME
t3_pp_kolme <- (((t3_matriisi[1,3] - t3_pp_expected_kolme)^2) / t3_pp_expected_kolme)
# # PP / HUONO
t3_pp_huono <- (((t3_matriisi[1,4] - t3_pp_expected_huono)^2) / t3_pp_expected_huono)
# # EI / VIISI
t3_ei_viisi <- (((t3_matriisi[2,1] - t3_ei_expected_viisi)^2) / t3_ei_expected_viisi)
# # EI / NELJA
t3_ei_nelja <- (((t3_matriisi[2,2] - t3_ei_expected_nelja)^2) / t3_ei_expected_nelja)
# # EI / KOLME
t3_ei_kolme <- (((t3_matriisi[2,3] - t3_ei_expected_kolme)^2) / t3_ei_expected_kolme)
# # EI / HUONO
t3_ei_huono<- (((t3_matriisi[2,4] - t3_ei_expected_huono)^2) / t3_ei_expected_huono)


# # CHI^2
# t2_chi <- t2_s_k + t2_s_ek + t2_es_k + t2_es_ek
t3_chi <- (t3_pp_viisi + t3_pp_nelja + t3_pp_kolme + t3_pp_huono) +
          (t3_ei_viisi + t3_ei_nelja + t3_ei_kolme + t3_ei_huono)
# 
# # CRIT
# t2_alpha <- 0.05
t3_alpha_viisi <- 0.05
t3_alpha_yksi <- 0.01
# t2_df <- (r-1) * (s-1)
t3_df <- (2-1) * (4-1)
# # qchisq(1-alpha,(r-1)*(s-1))
# t2_crit <- qchisq(1-t2_alpha, t2_df)
t3_crit_viisi <- qchisq(1 - t3_alpha_viisi, t3_df)
t3_crit_yksi <- qchisq(1 - t3_alpha_yksi, t3_df)
# 
# # REJECT TS >= x^2_(r-1)(s-1),alpha
# t2_rejected_ts <- ifelse(t2_chi >= t2_crit, TRUE, FALSE)
t3_rejected_ts_viisi <- ifelse(t3_chi >= t3_crit_viisi, TRUE, FALSE)
t3_rejected_ts_yksi <- ifelse(t3_chi >= t3_crit_yksi, TRUE, FALSE)

# 
# # P - ARVO
# t2_pvalue <- 1 - pchisq(t2_chi, t2_df)
t3_pvalue <- 1 - pchisq(t3_chi, t3_df)
# # REJECT Pvalue < alpha
# t2_rejected_pvalue <- ifelse(t2_pvalue < t2_alpha, TRUE, FALSE)
t3_rejected_pvalue_viisi <- ifelse(t3_pvalue < t3_alpha_viisi, TRUE, FALSE)
t3_rejected_pvalue_yksi <- ifelse(t3_pvalue < t3_alpha_yksi, TRUE, FALSE)
# 
# 

# TEHTÄVÄ 4

# Sivu 596, tehtävä 6) Erään alueen autoilijoista 84 prosenttia ei ole ollut osallisena
# liikenneonnettomuudessa viimeisen vuoden aikana,
# 14 prosenttia on ollut osallisena tasan yhdessä onnettomuudessa ja
# 2 prosenttia on ollut osallisena ainakin kahdessa on- nettomuudessa.
# Poimitaan neljänsadan lakimiehen satunnaisotos kyseiseltä alueelta.
# Otoksen lakimiehistä 308 ei ollut ollut osallisena liikenneonnettomuudessa
# viimeisen vuoden aikana, 66 oli ollut yhdessä onnettomuudessa ja 26
# ainakin kahdessa onnetto- muudessa. Voidaanko tästä päätellä,
# että lakimiesten yhteys liikenneonnettomuuksiin poikkeaa alueen muista autoilijoista?
                        #0  1     2   kolaria 
t4_expected_probs <- c(0.84,0.14,0.02)
t4_expected_freq <- lapply(t4_expected_probs, function(x) x*400)
t4_vec_exp_frq <- unlist(t4_expected_freq)

# 0   1   2   kolaria
t4_lakimiehet_observed <- c(308,66,26)
t4_n <- 400

t4_freqs <- lapply(t4_lakimiehet_observed, function(x) x / t4_n)
t4_vec_freqs <- unlist(t4_freqs)
t4_vec_freqs
t4_eka <- ((t4_lakimiehet_observed[1] - t4_expected_freq[[1]])^2 / t4_expected_freq[[1]])
t4_toka <- ((t4_lakimiehet_observed[2] - t4_expected_freq[[2]])^2 / t4_expected_freq[[2]])
t4_kolmas <- ((t4_lakimiehet_observed[3] - t4_expected_freq[[3]])^2 / t4_expected_freq[[3]])
t4_summat <- t4_eka + t4_toka + t4_kolmas

t4_otos <- c(308,66,26) # lakimiehet
t4_osuudet <- c(0.84,0.14,0.02) # odotetut osuudet
chisq.test(otos,p=osuudet)


# H_0 lakimiesten yhteys liikenneonnettomuuksiin ei poikkea
# H_1 lakimiesten yhteys liikenneonnettomuuksiin poikkeaa
t4_crit <- qchisq(1-0.05,2)
t4_crit_yksi <- qchisq(1-0.01,2)
t4_pvalue <- 1 - pchisq(t4_summat, (length(t4_otos)-1))
#0.000000000204

# Hylätään H_0
