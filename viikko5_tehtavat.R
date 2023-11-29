# TEHTÄVÄ 1
# 
# Eteläisen Kalifornian alueella haluttiin tutkia,
# esiintyykö maan- järistyksiä (voimakkuus vähintään 4.4 Richterin asteikolla mitattuna)
# todennäköisemmin joinakin tiettyinä viikonpäivinä.
# Tutkimusta varten kerättiin seuraavat tiedot koskien 1100 maanjäristystä:
# Viikonpäivä ma ti ke to pe la su Järistystenlkm 
t1_jaristykset <- c(144, 170, 158, 172, 148, 152, 156)
t1_alpha <- 0.05
t1_n <- 1100
t1_P_i <- 1/7
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
