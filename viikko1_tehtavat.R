# TEHTÄVÄ 1
# Satunnaisotannalla toteutetussa kyselyssä haastateltiin amerikkalaisia
# kahvinjuojia, joista
# 220 oli naisia ja
# 210 miehiä.
# Tulosten perusteella
# naisista 71 ja 
# miehistä 58 kertoivat juovansa mieluummin kofeiinitonta kahvia.
# Voidaanko tulosten perusteella päätellä, että kofeiinitonta kahvia mieluummin juovien naisten osuus
# eroaa vastaavasta miesten osuudesta viiden prosentin merkitsevyystasolla mitattuna?
# lukumäärät
t1_n_miehet <- 210
t1_n_naiset <- 220
# kofeenittomia
t1_kof_miehet <- 58
t1_kof_naiset <- 71
# otoskeskiarvot
t1_p_hat_m <- t1_kof_miehet / t1_n_miehet
t1_p_hat_n <- t1_kof_naiset / t1_n_naiset
t1_p_hat <- (t1_kof_miehet + t1_kof_naiset) / (t1_n_miehet+t1_n_naiset)
# TS = ( p_hat_1 - p_hat_2) / sqrt( (1/n_1 + 1/n_2) * p_hat * (1-p_hat))
t1_ts_ylakerta <- t1_p_hat_n - t1_p_hat_m
t1_ts_alakerta <- ((1 / t1_n_naiset) + (1 / t1_n_miehet)) * t1_p_hat *(1 - t1_p_hat)                                                             
t1_ts_alakerta_sqrt <- sqrt(t1_ts_alakerta)
t1_test_statistic <- t1_ts_ylakerta / t1_ts_alakerta_sqrt
# Z-score
t1_zeta <- 0.05
t1_crit <- qnorm(1-(t1_zeta/2))
t1_rejected <- ifelse(abs(t1_test_statistic)>t1_crit, TRUE, FALSE)
# p-value
# Mikä on testin p-arvo?
t1_pvalue <- 2 * pnorm(-1.05)
t1_rejected_p_value <- ifelse(t1_pvalue <= t1_zeta, TRUE, FALSE)

# # TEHTÄVÄ 2
# Sivu 439, tehtävä 2)
# Eräs liuoksen pH-arvon mittaamiseen käytetty menetelmä antaa
# tuloksen, joka noudattaa normaalijakaumaa odotusarvolla,
# joka on liuoksen todellinen pH-arvo,
# ja keskihajonnalla 0.05. Ympäristön saastumisen erikoistunut tutkija
# väittää, että kaksi eri liuosta tulevat samasta lähteestä.
# Jos väite pätee, näiden liuoksien pH-arvot ovat samat. Väitteen uskottavuuden testaamiseksi molempien liuoksista
# kerättiin 10 riippumatonta pH-arvomittausta ja saatiin seuraavat tulokset

t2_liuos_A <- c(6.24,6.31,6.28,6.30,6.25,6.26,6.24,6.29,6.22,6.28)
t2_liuos_B <- c(6.27,6.25,6.33,6.27,6.24,6.31,6.28,6.29,6.34,6.27)
t2_n <- length(t2_liuos_A)
t2_keskihajonta <- 0.05
t2_varianssi <- t2_keskihajonta^2
# (a) Todistaako aineisto tutkijan väitettä vastaan? Käytä 5% merkitsevyystasoa.
t2_liuosA_keskiarvo <- mean(t2_liuos_A)
t2_liuosB_keskiarvo <- mean(t2_liuos_B)

# TS = (XBAR - YBAR) / SQRT( sigma^2/n + sigma^2/m)
t2_ts_ylakerta <- t2_liuosA_keskiarvo - t2_liuosB_keskiarvo
t2_ts_alakerta <- sqrt(
  (t2_varianssi / t2_n) + (t2_varianssi / t2_n)
)
t2_ts <- t2_ts_ylakerta / t2_ts_alakerta
t2_zeta <- 0.05
t2_crit <- qnorm(1-(t2_zeta / 2))
# reject h0 if abs(TS) >= z / 2
t2_rejected <- ifelse(abs(t2_ts) >= t2_crit, TRUE, FALSE)
# (b) Laske p-arvo.
t2_pvalue <- 2*pnorm(t2_ts)
t2_rejected_pvalue <- ifelse(t2_pvalue <= t2_zeta, TRUE, FALSE)

# # TEHTÄVÄ 3
# Sairaalasta A poimitaan 480 kappaleen satunnaisotos sepelvaltimon ohitusleikkauksista.
# Otoksen potilaista 72 ei selvinnyt. Sairaalasta B poimitaan
# vastaava 360 kappaleen satunnaisotos. Tämän otoksen potilaista 30 ei selvinnyt.
# Sairaala A
t3_A_n <- 480
t3_A_kuolleet <- 72
t3_A_osuus <- t3_A_n - t3_A_kuolleet
t3_p_hat_A <- t3_A_osuus / t3_A_n
# Sairaala B
t3_B_n <- 360
t3_B_kuolleet <- 30
t3_B_osuus <- t3_B_n - t3_B_kuolleet
t3_p_hat_B <- t3_B_osuus / t3_B_n
# Yhteisosuus
t3_p_hat <- (t3_A_osuus + t3_B_osuus) / (t3_A_n + t3_B_n)
# Testisuure
t3_ts_ylakerta <- (t3_p_hat_A - t3_p_hat_B)
t3_ts_alakerta <- ((1 / t3_A_n) + (1 / t3_B_n)) * t3_p_hat * (1 - t3_p_hat)
t3_ts_alakerta_sqrt <- sqrt((t3_ts_alakerta))
t3_ts <- t3_ts_ylakerta / t3_ts_alakerta_sqrt
# Testataan hypoteesia, että selviytymistodennäköisyys sepelvaltimon ohitusleikkauksessa
# on sama kummassakin sairaalassa.
#Mikä on testin p-arvo ja johtopäätös?

# # Z-score
# t1_zeta <- 0.05
t3_zeta_yksi <- 0.01
t3_zeta_viisi <- 0.05
# t1_crit <- qnorm(1-(t1_zeta/2))
t3_crit_yksi <- qnorm(1-(t3_zeta_yksi/2))
t3_crit_viisi <- qnorm(1-(t3_zeta_viisi/2))
# t1_rejected <- ifelse(abs(t1_test_statistic)>t1_crit, TRUE, FALSE)
t3_rejected_yksi <- ifelse(abs(t3_ts)>t3_crit_yksi, TRUE, FALSE)
t3_rejected_viisi <- ifelse(abs(t3_ts)>t3_crit_viisi, TRUE, FALSE)
# # p-value
# # Mikä on testin p-arvo?
# t1_pvalue <- 2 * pnorm(-1.05)
t3_pvalue <- 2 * pnorm(t3_ts)
# t1_rejected_p_value <- ifelse(t1_pvalue <= t1_zeta, TRUE, FALSE)
t3_rejected_p_value_yksi <- ifelse(t3_pvalue <= t3_zeta_yksi, TRUE, FALSE)
t3_rejected_p_value_viisi <- ifelse(t3_pvalue <= t3_zeta_viisi, TRUE, FALSE)
# # TEHTÄVÄ 4
# Toteutettiin koe, jonka avulla selvitettiin muutoksia naisten
# ruokavaliossa talvella ja kesällä. Heinäkuussa tarkkailtiin
# 12 naisen satunnaisotosta
# ja laskettiin kuinka paljon kukin nainen sai kaloreita rasvasta. Sama koe toistettiin
# samoille naisille joulukuussa. Saatiin seuraavat tulokset:

t4_heinakuu <- c(32.2, 27.4, 28.6, 32.4, 40.5, 26.2, 29.4, 25.8, 36.6, 30.3, 28.5, 32.0)
t4_joulukuu <- c(30.5, 28.4, 40.2, 37.6, 36.5, 38.8, 34.7, 29.5, 29.7, 37.2, 41.5, 37.0)

t4_n <- length(t4_heinakuu)
# havaintoparien erotusten otoskeskiarvo
t4_D_hat <- mean(t4_heinakuu - t4_joulukuu)
# havaintoparien erotusten otoskeskiarvo
t4_sd <- sd(t4_heinakuu - t4_joulukuu)

t4_ts <- sqrt(t4_n) * ((t4_D_hat)/(t4_sd))
# Testaa hypoteesia, että rasvan saannissa ei ole eroa kesällä ja talvella.
# (a) Käytä 5% merkitsevyystasoa.
t4_alpha_viisi <- 0.05
t4_crit_viisi <- qt(1-(t4_alpha_viisi/2), df = (t4_n - 1))
# t1_rejected <- ifelse(abs(t1_test_statistic)>t1_crit, TRUE, FALSE)
t4_rejected_viisi <- ifelse(abs(t4_ts)>t4_crit_viisi, TRUE, FALSE)
# (b) Käytä 1% merkitsevyystasoa.
t4_alpha_yksi <- 0.01
t4_crit_yksi <- qt(1-(t4_alpha_yksi/2), df = (t4_n -1))
t4_rejected_yksi <- ifelse(abs(t4_ts)>t4_crit_yksi, TRUE, FALSE)

# TEHTÄVÄ 5
# Vastasyntyneiden vauvojen painoista (paunoissa)
# kahdessa vierekkäisessä läänissä saatiin seuraavat arvot:
#   n = 53 m = 44
t5_laani1_n <- 53
t5_laani2_m <- 44
#   X = 6.8 Y = 7.2
t5_laani1_ka <- 6.8
t5_laani2_ka <- 7.2
#   S2
#   x = 5.2 S2
t5_Sx2 <- 5.2
t5_Sy2 <- 4.9
#   y = 4.9
#   Testaa hypoteesia, että vastasyntyneiden vauvojen keskimääräinen paino
# on sama molemmissa lääneissä.
# Oleta, että perusjoukkojen varianssit ovat yhtä suuret ja otoskoot
# tulkitaan pieniksi.
#   (a) Hylätäänkö nollahypoteesi 5% merkitsevyystasolla?
#Sp^2 = (n-1)*Sx2 + (m-1)*Sy2 / n + m - 2
t5_Sp2 <- ((t5_laani1_n - 1)*t5_Sx2 + (t5_laani2_m - 1)*t5_Sy2) / (t5_laani1_n + t5_laani2_m -2)
# oletetaan pieneksi ja varianssi yhtäsuurta
# TS = XBAR - YBAR / SQRT ( SP2( 1/N + 1/M))
t5_ts_ylakerta <- t5_laani1_ka - t5_laani2_ka
t5_ts_alakerta_eka <- t5_Sp2 * ((1 / t5_laani1_n) + (1 / t5_laani2_m))
t5_ts_alakerta_sqrt <- sqrt(t5_ts_alakerta_eka)
t5_ts <- t5_ts_ylakerta / t5_ts_alakerta_sqrt
t5_zeta <- 0.05
t5_crit_viisi <- qt(1-(t5_zeta/2), df = (t5_laani1_n + t5_laani2_m -2))
t5_rejected <- ifelse(abs(t5_ts) >= t5_crit_viisi, TRUE, FALSE)
#   (b) Laske p-arvo.
t5_pvalue <- 2 * pnorm(t5_ts)
#   (c) Laske myös p-arvo testille, missä oletatkin otoskoot suuriksi ja et oleta, että pe-
#     rusjoukkojen varianssit ovat yhtäsuuret. Kuinka nämä kaksi p-arvoa vertautuvat
#   keskenään?
t5_c_ts_ylakerta <- t5_laani1_ka - t5_laani2_ka
t5_c_ts_alakerta <- (t5_Sx2 / t5_laani1_n) + (t5_Sy2 / t5_laani2_m)
t5_c_ts_alakerta_sqrt <- sqrt(t5_c_ts_alakerta)
t5_c_ts <- t5_c_ts_ylakerta / t5_c_ts_alakerta_sqrt
t5_c_pvalue <- 2 * pnorm(t5_c_ts)
t5_c_rejected <- ifelse(t5_c_pvalue <= t5_zeta, TRUE, FALSE)


t6_n <- 7
t6_m <- 8
t6_xbar <- 30.7
t6_ybar <- 35.1
t6_sp2 <- 12.9

# t5_ts_ylakerta <- t5_laani1_ka - t5_laani2_ka
t6_ylakerta <- t6_xbar - t6_ybar
# t5_ts_alakerta_eka <- t5_Sp2 * ((1 / t5_laani1_n) + (1 / t5_laani2_m))
t6_alakerta_eka <- t6_sp2 * (( 1 / t6_n ) + ( 1 / t6_m))
# t5_ts_alakerta_sqrt <- sqrt(t5_ts_alakerta_eka)
t6_alakerta <- sqrt(t6_alakerta_eka)
# t5_ts <- t5_ts_ylakerta / t5_ts_alakerta_sqrt
t6_ts <- t6_ylakerta / t6_alakerta
t6_pvalue <- 2 * pnorm(t6_ts)
