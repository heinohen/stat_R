
# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.


# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████       ██ 
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██     ███ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      ██ 
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██      ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██      ██ 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# Eteläisen Kalifornian alueella haluttiin tutkia,
# esiintyykö maan- järistyksiä (voimakkuus vähintään 4.4 Richterin asteikolla mitattuna)
# todennäköisemmin joinakin tiettyinä viikonpäivinä.
# Tutkimusta varten kerättiin seuraavat tiedot koskien 1100 maanjäristystä:
# Viikonpäivä ma ti ke to pe la su järistysten lkm 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

t1_P_i <- 1/7
t1_jaristykset <- c(144, 170, 158, 172, 148, 152, 156)
t1_probz <- c(t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i, t1_P_i)
t1_alpha <- 0.05
t1_n <- 1100

# JOS EI TODENNÄKÖISYYDET SAMAT SAADAAN LASKETTUA

# > otos <- c(92,20,4,84)
# > osuudet <- c(0.41,0.09,0.04,0.46)
# > chisq.test(otos,p=osuudet)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# Testaa hypoteesia, jonka mukaan maanjäristys tapahtuu yhtä todennäköisesti
# minä tahansa viikonpäivänä. Käytä viiden prosentin merkitsevyystasoa.

# H_0 : tapahtuu jokaisena päivänä yhtä todennäköisesti
# H_1 : tapahtuu jonain päivänä todennäköisemmin kuin muina

#   ██████ ██   ██ ██ ███████  ██████      ████████ ███████ ███████ ████████ 
# ██       ██   ██ ██ ██      ██    ██        ██    ██      ██         ██    
# ██       ███████ ██ ███████ ██    ██        ██    █████   ███████    ██    
# ██       ██   ██ ██      ██ ██ ▄▄ ██        ██    ██           ██    ██    
#   ██████ ██   ██ ██ ███████  ██████         ██    ███████ ███████    ██    
#    ▀▀                                         
 
#Tällä saa suoraan oikeat arvot.
t1_chisq <- chisq.test(t1_jaristykset)


# Odotettu frekvenssi kaikille päiville, sama tn niin yksi arvo riittää
t1_e_i <- signif((t1_n * t1_P_i),5)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.


# ████████ ███████ 
#    ██    ██      
#    ██    ███████ 
#    ██         ██ 
#    ██    ███████ 

# Odotettu frekvenssi kaikille päiville, sama tn niin yksi arvo riittää
t1_e_i <- signif((t1_n * t1_P_i),5)

# (Ni - ei)^2 / ei
t1_per_paiva <- lapply(t1_jaristykset, function(x) (x - t1_e_i)^2 / t1_e_i)
t1_ts <- sum(unlist(t1_per_paiva))
t1_ts

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.


#   ██████ ██████  ██ ████████ 
# ██       ██   ██ ██    ██    
# ██       ██████  ██    ██    
# ██       ██   ██ ██    ██    
#   ██████ ██   ██ ██    ██    


# crit qchisq(Pi, k-1)
t1_crit <- qchisq(1-(t1_alpha), length(t1_jaristykset)-1)
t1_rejected_ts <- ifelse(t1_ts >= t1_crit, TRUE, FALSE)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████         █████  ██████  ██    ██  ██████  
# ██   ██       ██   ██ ██   ██ ██    ██ ██    ██ 
# ██████  █████ ███████ ██████  ██    ██ ██    ██ 
# ██            ██   ██ ██   ██  ██  ██  ██    ██ 
# ██            ██   ██ ██   ██   ████    ██████  

# pvalue P(X^2_k-1 >= TS)
t1_pvalue <- 1 - pchisq(t1_ts, length(t1_jaristykset)-1)
t1_rejected_pvalue <- ifelse(t1_ts < t1_alpha, TRUE, FALSE)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.



# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██     ██      
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ███████ 
# TEHTÄVÄ 2

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

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

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# H_0: Selvityminen on riippumatonta koiran omistamisen kanssa
# H_1: Selviytyminen on riippuvaista koiran omistamisen kanssa

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

t2_mat <- matrix(c(28,8,44,15), ncol=2,byrow = TRUE)
t2_mat
#       [,1] [,2]
# [1,]   28   44 
# [2,]    8   15 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

#   ██████ ██   ██ ██ ███████  ██████      ████████ ███████ ███████ ████████ 
# ██       ██   ██ ██ ██      ██    ██        ██    ██      ██         ██    
# ██       ███████ ██ ███████ ██    ██        ██    █████   ███████    ██    
# ██       ██   ██ ██      ██ ██ ▄▄ ██        ██    ██           ██    ██    
#   ██████ ██   ██ ██ ███████  ██████         ██    ███████ ███████    ██    
#    ▀▀                                         

# Tällä saa suoraan oikeat arvot.
t2_chisq <- chisq.test(t2_mat, corr = F)
t2_chisq$p.value

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# Tällä saa tarkastettua
# Lasketaan odotetut frekvenssit
# (apply(O, 1, sum) %*% t(apply(O,2,sum))) / sum(O) > # Lasketaan testisuure
t2_matriisilasku  <- (apply(t2_mat, 1, sum) %*% t(apply(t2_mat,2,sum))) / sum(t2_mat)
# > X <- sum((O - E)ˆ2 / E)
t2_matriisitulos <- sum(( t2_mat - t2_matriisilasku)^2 / t2_matriisilasku)
#> pchisq(B, df=length(row)-1, lower=F)
t2_chisq_laskettu <- pchisq(t2_matriisitulos, df = length(t2_mat[2,]) - 1, lower.tail = F)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# Tai sitten käsin
# rivisummat
t2_r1_sum <- sum(t2_mat[1,])
t2_r2_sum <- sum(t2_mat[2,])
# sarakesummat
t2_s1_sum <- sum(t2_mat[,1])
t2_s2_sum <- sum(t2_mat[,2])
# kaikkien summa
t2_m_sum <- t2_mat[1,1] + t2_mat[1,2] + t2_mat[2,1] + t2_mat[2,2]
# lisätään sarake jossa rivisummat
t2_matriisi <- cbind(t2_mat, c(t2_r1_sum, t2_r2_sum))
# lisätään rivi jossa sarakesummat
t2_matriisi <- rbind(t2_matriisi, c(t2_s1_sum, t2_s2_sum, t2_m_sum))

#       [,1] [,2] [,3]
# [1,]   28   44   72
# [2,]    8   15   23
# [3,]   36   59   95

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

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ██████  
# TEHTÄVÄ 3 

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


# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

t3_matriisi <- matrix(c(22,38,35,5,18,32,40,10),nrow = 2, ncol = 4, byrow = TRUE)
t3_matriisi

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

#   ██████ ██   ██ ██ ███████  ██████      ████████ ███████ ███████ ████████ 
# ██       ██   ██ ██ ██      ██    ██        ██    ██      ██         ██    
# ██       ███████ ██ ███████ ██    ██        ██    █████   ███████    ██    
# ██       ██   ██ ██      ██ ██ ▄▄  █        ██    ██           ██    ██    
#   ██████ ██   ██ ██ ███████  ██████         ██    ███████ ███████    ██    
#    ▀▀                                         

#Tällä saa suoraan oikeat arvot.
t3_chsiq_test <- chisq.test(t3_matriisi)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# Tai sitten käsin
t3_matriisi_sarakkeet <- cbind(t3_matriisi, c(sum(t3_matriisi[1,]), sum(t3_matriisi[2,])))

t3_matriisi_valmis <- rbind(t3_matriisi_sarakkeet, c(sum(t3_matriisi_sarakkeet[,1]),
                                                    sum(t3_matriisi_sarakkeet[,2]),
                                                    sum(t3_matriisi_sarakkeet[,3]),
                                                    sum(t3_matriisi_sarakkeet[,4]),
                                                    sum(t3_matriisi_sarakkeet[,5])))

# 
# # paikanpäällä ----> rivi 1 Eij = (n_r * n_s) / n
# #ODOTUSARVOT
t3_pp_expected_viisi <- (t3_matriisi_valmis[1,5] * t3_matriisi_valmis[3,1]) / t3_matriisi_valmis[3,5]
t3_pp_expected_nelja <- (t3_matriisi_valmis[1,5] * t3_matriisi_valmis[3,2]) / t3_matriisi_valmis[3,5]
t3_pp_expected_kolme <- (t3_matriisi_valmis[1,5] * t3_matriisi_valmis[3,3]) / t3_matriisi_valmis[3,5]
t3_pp_expected_huono <- (t3_matriisi_valmis[1,5] * t3_matriisi_valmis[3,4]) / t3_matriisi_valmis[3,5]
t3_ei_expected_viisi <- (t3_matriisi_valmis[2,5] * t3_matriisi_valmis[3,1]) / t3_matriisi_valmis[3,5]
t3_ei_expected_nelja <- (t3_matriisi_valmis[2,5] * t3_matriisi_valmis[3,2]) / t3_matriisi_valmis[3,5]
t3_ei_expected_kolme <- (t3_matriisi_valmis[2,5] * t3_matriisi_valmis[3,3]) / t3_matriisi_valmis[3,5]
t3_ei_expected_huono <- (t3_matriisi_valmis[2,5] * t3_matriisi_valmis[3,4]) / t3_matriisi_valmis[3,5]

# # PP / VIISI
t3_pp_viisi <- (((t3_matriisi[1,1] - t3_pp_expected_viisi)^2) / t3_pp_expected_viisi)
# # PP / NELJA
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

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.


# ████████ ███████ 
#    ██    ██      
#    ██    ███████ 
#    ██         ██ 
#    ██    ███████ 

# # CHI^2
t3_chi <- (t3_pp_viisi + t3_pp_nelja + t3_pp_kolme + t3_pp_huono) +
          (t3_ei_viisi + t3_ei_nelja + t3_ei_kolme + t3_ei_huono)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

#   ██████ ██████  ██ ████████ 
# ██       ██   ██ ██    ██    
# ██       ██████  ██    ██    
# ██       ██   ██ ██    ██    
#   ██████ ██   ██ ██    ██    

t3_alpha_viisi <- 0.05
t3_alpha_yksi <- 0.01
t3_df <- (2-1) * (4-1)
t3_crit_viisi <- qchisq(1 - t3_alpha_viisi, t3_df)
t3_crit_yksi <- qchisq(1 - t3_alpha_yksi, t3_df)
# 
# # REJECT TS >= x^2_(r-1)(s-1),alpha
# t2_rejected_ts <- ifelse(t2_chi >= t2_crit, TRUE, FALSE)
t3_rejected_ts_viisi <- ifelse(t3_chi >= t3_crit_viisi, TRUE, FALSE)
t3_rejected_ts_yksi <- ifelse(t3_chi >= t3_crit_yksi, TRUE, FALSE)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████         █████  ██████  ██    ██  ██████  
# ██   ██       ██   ██ ██   ██ ██    ██ ██    ██ 
# ██████  █████ ███████ ██████  ██    ██ ██    ██ 
# ██            ██   ██ ██   ██  ██  ██  ██    ██ 
# ██            ██   ██ ██   ██   ████    ██████  

t3_pvalue <- 1 - pchisq(t3_chi, t3_df)
# # REJECT Pvalue < alpha
# t2_rejected_pvalue <- ifelse(t2_pvalue < t2_alpha, TRUE, FALSE)
t3_rejected_pvalue_viisi <- ifelse(t3_pvalue < t3_alpha_viisi, TRUE, FALSE)
t3_rejected_pvalue_yksi <- ifelse(t3_pvalue < t3_alpha_yksi, TRUE, FALSE)
# 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██   ██ 
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██     ██   ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████     ███████ 
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██          ██ 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

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

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

#   ██████ ██   ██ ██ ███████  ██████      ████████ ███████ ███████ ████████ 
# ██       ██   ██ ██ ██      ██    ██        ██    ██      ██         ██    
# ██       ███████ ██ ███████ ██    ██        ██    █████   ███████    ██    
# ██       ██   ██ ██      ██ ██ ▄▄  █        ██    ██           ██    ██    
#   ██████ ██   ██ ██ ███████  ██████         ██    ███████ ███████    ██    
#    ▀▀                                         

#Tällä saa suoraan oikeat arvot.
t4_otos <- c(308,66,26) # lakimiehet
t4_osuudet <- c(0.84,0.14,0.02) # odotetut osuudet
t4_chiqqqq <- chisq.test(t4_otos, p=t4_osuudet)
t4_chiqqqq$p.value


# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

# 0   1   2   kolaria
t4_lakimiehet_observed <- c(308,66,26)
t4_n <- 400

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

# H_0 lakimiesten yhteys liikenneonnettomuuksiin ei poikkea
# H_1 lakimiesten yhteys liikenneonnettomuuksiin poikkeaa
# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

#   ██████ ██████  ██ ████████ 
# ██       ██   ██ ██    ██    
# ██       ██████  ██    ██    
# ██       ██   ██ ██    ██    
#   ██████ ██   ██ ██    ██    


t4_crit <- qchisq(1-0.05,2)
t4_crit_yksi <- qchisq(1-0.01,2)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████         █████  ██████  ██    ██  ██████  
# ██   ██       ██   ██ ██   ██ ██    ██ ██    ██ 
# ██████  █████ ███████ ██████  ██    ██ ██    ██ 
# ██            ██   ██ ██   ██  ██  ██  ██    ██ 
# ██            ██   ██ ██   ██   ████    ██████  
t4_pvalue <- 1 - pchisq(t4_summat, (length(t4_otos)-1))
t4_pvalue
#0.000000000204

# Hylätään H_0

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.




## TENTTI


# 
# # ██████   █████  ████████  █████  
# # ██   ██ ██   ██    ██    ██   ██ 
# # ██   ██ ███████    ██    ███████ 
# # ██   ██ ██   ██    ██    ██   ██ 
# # ██████  ██   ██    ██    ██   ██ 
# 
# t2_mat <- matrix(c(28,8,44,15), ncol=2,byrow = TRUE)
# t2_mat

tentti14_matriisi <- matrix(c(127,156,24,29,35,29),ncol=3)

rownames(tentti14_matriisi) <- c("aamuvirkku", "ei-aamuvirkku")
colnames(tentti14_matriisi) <- c("kylki", "vatsa", "selka")
tentti14_matriisi
sum(tentti14_matriisi[1,])
sum(tentti14_matriisi[2,])

# # .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.
# 
# #   ██████ ██   ██ ██ ███████  ██████      ████████ ███████ ███████ ████████ 
# # ██       ██   ██ ██ ██      ██    ██        ██    ██      ██         ██    
# # ██       ███████ ██ ███████ ██    ██        ██    █████   ███████    ██    
# # ██       ██   ██ ██      ██ ██ ▄▄ ██        ██    ██           ██    ██    
# #   ██████ ██   ██ ██ ███████  ██████         ██    ███████ ███████    ██    
# #    ▀▀                                         
# 
# # Tällä saa suoraan oikeat arvot.
# t2_chisq <- chisq.test(t2_mat, corr = F)
tentti14_chisq <- chisq.test(tentti14_matriisi, corr = F)
# t2_chisq$p.value
tentti14_chisq$p.value
