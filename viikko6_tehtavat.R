

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.


# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████       ██ 
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██     ███ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      ██ 
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██      ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██      ██ 

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.
print("TEHTÄVÄ 1")
# (Sivu 628, tehtävä 2) Vuonna 2001 Connecticutin kotitalouksien vuositulojen mediaani
# 52 758 dollaria. Äskettäin tehtyyn tutkimukseen kerättiin 250 kotitalouden satunnaisotos
# ja saatiin selville, että 42% tulot olivat alle vuoden 2001 mediaanin ja 58% tulot
# olivat yli vuoden 2001 mediaanin. Todistaako tämä, että Connecticutin kotitalouksien
# vuositulojen mediaani ei ole enää sama kuin vuonna 2001? Laske p-arvo.
# 

# Käytetään merkkitestiä (sign test)

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 


t1_n <- 250
t1_success <- 0.42 * t1_n
t1_p <- 1/2

# ██████  ██ ███    ██  ██████  ███    ███     ████████ ███████ ███████ ████████ 
# ██   ██ ██ ████   ██ ██    ██ ████  ████        ██    ██      ██         ██    
# ██████  ██ ██ ██  ██ ██    ██ ██ ████ ██        ██    █████   ███████    ██    
# ██   ██ ██ ██  ██ ██ ██    ██ ██  ██  ██        ██    ██           ██    ██    
# ██████  ██ ██   ████  ██████  ██      ██        ██    ███████ ███████    ██    

t1_binom_test <- binom.test(t1_success, t1_n, t1_p, alternative = "two.sided")
t1_binom_test$p.value
# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ 
#    ██    ██      
#    ██    ███████ 
#    ██         ██ 
#    ██    ███████ 

# Testisuure on onnistumisten lukumäärä binomikokeessa
# TS = havaittu prosentuaalinen osuus * koko populaatio
t1_TS <- 0.42 * 250  
# 105




# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████         █████  ██████  ██    ██  ██████  
# ██   ██       ██   ██ ██   ██ ██    ██ ██    ██ 
# ██████  █████ ███████ ██████  ██    ██ ██    ██ 
# ██            ██   ██ ██   ██  ██  ██  ██    ██ 
# ██            ██   ██ ██   ██   ████    ██████  

# args:
# q = onnistumiset, size = n, p = odotettu tn
t1_ans <- 2 * pbinom(105,250,1/2)


# > t1_binom_test$p.value
# [1] 0.01348145
# > 2 * pbinom(105,250,1/2)
# [1] 0.01348145

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██     ██      
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ███████ 
# TEHTÄVÄ 2

# Oletetaan, että vitamiinivalmisteiden on todettu kasvattavan opiskelu-
# motivaatiota. 14 vapaaehtoista opiskelijaa vastasivat opiskelumotivaatiota mittaavaan
# kyselyyn. Tämän jälkeen opiskelijat jaettiin kahteen yhtä suureen ryhmään. Ensim-
# mäistä ryhmää ohjeistettiin syömään vitamiinivalmistetta A kahden kuukauden ajan
# ja toista ryhmää ohjeistettiin syömään vitamiinivalmistetta B. Kahden kuukauden
# jälkeen opiskelijat vastasivat uudestaan opiskelumotivaatiota mittaavaan kyselyyn ja
# tuloksia verrattiin ennen koetta saatuihin tuloksiin. Saatiin seuraavat tulokset opiske-
# lumotivaation muuttumisesta:
print("TEHTÄVÄ 2")
# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

t2_data <- data.frame(vitA = c(1,7,-6,5,-3,6,6),
                      vitB = c(2,-1,0,2,3,4,-2))
t2_data

##### SAMA TESTI vvvvvvv TÄMÄ TOIMII #######
t2_data_testi <- data.frame(sample = c(1,7,-6,5,-3,6,6,2,-1,0,2,3,4,-2),
                            group = factor(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2),
                                           labels=c('vitA','vitB')))
t2_data_testi
t2_pairwise <- pairwise.wilcox.test(t2_data_testi$sample, t2_data_testi$group)
t2_pairwise$p.value

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ██████  
# TEHTÄVÄ 3 
print("TEHTÄVÄ 3")
#Alla on aurinkoisten päivien lukumäärä erään vuoden jokaiselta syys-marraskuun viikolta.

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████   █████  ████████  █████  
# ██   ██ ██   ██    ██    ██   ██ 
# ██   ██ ███████    ██    ███████ 
# ██   ██ ██   ██    ██    ██   ██ 
# ██████  ██   ██    ██    ██   ██ 

t3_paivat <- c(5, 7, 4, 6, 5, 3, 4, 5, 3, 1, 2, 1, 0)

# ██████  ███████ ██████  ███    ███     ████████ ███████ ███████ ████████ 
# ██   ██ ██      ██   ██ ████  ████        ██    ██      ██         ██    
# ██████  █████   ██████  ██ ████ ██        ██    █████   ███████    ██    
# ██      ██      ██   ██ ██  ██  ██        ██    ██           ██    ██    
# ██      ███████ ██   ██ ██      ██        ██    ███████ ███████    ██    

#Todistaako aineisto 5 prosentin merkitsevyystasolla aurinkoisten päivien luku-
# määrän vähentyneen viikkojen edetessä?

# ( ei löydy suoraan kirjastokoodia, lasketaan käsin)

# N
t3_N <- length(t3_paivat)
# xbar
t3_xbar <- mean(t3_paivat)
# xbar^2
t3_xbar_toiseen <- t3_xbar^2
# ss = sum(i=1)(N)Xi^2
t3_ss <- sum(sapply(t3_paivat, function(x) x^2))

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ 
#    ██    ██      
#    ██    ███████ 
#    ██         ██ 
#    ██    ███████ 

# TS = sum(j=1)(N) jXj

t3_ts_func <- function(vec) {
  summa = 0
  for (i in 1:length(vec)) {
    tama <- i * vec[i]
    summa <- summa + tama
  }
  return (summa)
}

t3_ts <- t3_ts_func(t3_paivat)
# E(TS) = N(N+1)/2 * xbar
t3_ETS <- ((t3_N*(t3_N + 1)) / 2) * t3_xbar

# tämä varianssilasku toimii, voi käyttää, aseta muuttujat yllä
              #a                          b                       c                     d
# VAR(TS) = N(N+1)(2N+1) / 6(N-1) * (ss - N*xbar^2) + N^2*(N + 1)^2 / 4*(N-1) * (xbar^2 - ss/N)
t3_Var_a <- (t3_N * (t3_N + 1) * (2*t3_N + 1)) / (6*(t3_N-1))
t3_Var_b <- t3_ss - t3_N * t3_xbar_toiseen
t3_Var_c <- (t3_N^2 * (t3_N + 1)^2) / (4 * (t3_N - 1))
t3_Var_d <- t3_xbar_toiseen - (t3_ss / t3_N)

t3_var <- t3_Var_a * t3_Var_b + t3_Var_c * t3_Var_d

# Vastahypoteesi on, että aurinkoisten päivien lukumäärä on vähentynyt, joten
# pienet testisuureen arvot ovat kriittisiä nollahypoteesin kannalta. Siten p-arvo
# on P (X <= 237)
# 
# missä X ∼ N(321.958, 807.333) ja se voidaan laskea 
# R:llä komennolla pnorm(237, mean = 321.958, sd = sqrt(807.333))

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ██████         █████  ██████  ██    ██  ██████  
# ██   ██       ██   ██ ██   ██ ██    ██ ██    ██ 
# ██████  █████ ███████ ██████  ██    ██ ██    ██ 
# ██            ██   ██ ██   ██  ██  ██  ██    ██ 
# ██            ██   ██ ██   ██   ████    ██████  

# args:
# pnorm(Havaittu_TS, mean, sd = sqrt(var))
t3_pvalue <- pnorm(t3_ts, t3_ETS, sqrt(t3_var))


# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██   ██ 
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██     ██   ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████     ███████ 
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██          ██ 

# Kurssin lopputyön voi kirjoittaa käsin tai tietokoneella.
# Professori mietti, onko tällä vaikutusta arvosanaan, jonka opiskelija saa lopputyöstä.
# Professori päätti testata tätä jakamalla 28 opiskelijaa pareiksi niin, että parien taidot olivat
# suurin piirtein samalla tasolla. Tämän jälkeen toinen parista palautti käsinkirjoitetun
# projektin ja toinen tietokoneella kirjoitetun projektin. Professori päätti sen, kumpi
# palautti käsin ja kumpi koneella kirjoitetun projektin, heittämällä kolikkoa. Projekteille
# annetut pistemäärät olivat seuraavat
print("TEHTÄVÄ 4")

# TÄMÄ TOIMII

kasin = c(83,75,75,60,72,55,94,85,78,96,80,75,66,55)
koneella = c(88,91,72,70,80,65,90,89,85,93,86,79,64,68)
srtest <- function(x, y) {
  z <- (x - y)[x - y != 0]
  TS <- sum(rank(abs(z))[z < 0])
  n <- length(z)
  mu <- n*(n + 1)/4
  sigma2 <- mu*(2*n + 1)/6
  normval <- pnorm(TS, mean=mu, sd=sqrt(sigma2))
  return(2*min(normval, 1 - normval))
}
t4_test <- srtest(kasin,koneella)
t4_test

# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.._.:*~*:._.:*~*:._._.:*~*:._.:*~*:._

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ███████ 
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██     ██      
#    ██    █████   ███████    ██    ███████ ██    ██ ███████     ███████ 
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ███████ 

print("TEHTÄVÄ 5")
# 
# Sivu 652, tehtävä 8) Seuraava aineisto listaa 10 peräkkäisen vuoden Dow Jones
# Industrial Average -indeksit jokaisen vuoden lopusta:

t5_data <- c(910, 890, 1010, 1033, 1080, 1275, 1288, 1553, 1980, 2702)
 
# Testaa 5% merkitsevyystasolla hypoteesia, jonka mukaan tämän aineiston voidaan
# ajatella muodostavan satunnaisotoksen.

t5_med <- median(t5_data)

## TÄMÄ TOIMII

# Data vektorina
t5_data <- c(910, 890, 1010, 1033, 1080, 1275, 1288, 1553, 1980, 2702)
# Aineiston lukumäärä
t5_n <- length((t5_data))
# Mediaani
t5 <- median(t5_data)
# Tämä generoi 0/1 vektorin
t5_generate_vec <- function(x, med) {
  vec0 <- c()
  for (i in 1:length(x)){
    if (x[i] <= med ){
      new_val <- 0
    }
    else{
      new_val <- 1
    }
    vec0 <- append(vec0,new_val) 
  }
  return(vec0)
}
# Tällä kutsutaan
t5_test <- t5_generate_vec(t5_data, t5_med)

runs <- function(x) {
  # Code the desired functionality here
  r <- rle(x)
  onesum <- sum(r$lengths[r$values == 1])
  zerosum <- sum(r$lengths[r$values == 0])
  mu <- (2 * onesum * zerosum) / (zerosum + onesum) +1
  var <- ((2*onesum * zerosum) * (2 * onesum * zerosum - onesum - zerosum)) / ((zerosum + onesum)^2 * (onesum + zerosum -1))
  x_ts <- length(r$lengths)
  
  calculated_pval <- 0
  if (x_ts >= mu - 0.5) {
    pee <- (x_ts - mu - 0.5) / sqrt(var)
    pval <- 2 * pnorm(pee, lower.tail = FALSE)
    calculated_pval <- pval
  } else {
    pee <- (x_ts - mu + 0.5) / sqrt(var)
    pval <- 2 * pnorm(pee)
    calculated_pval <- pval
  }
  print(calculated_pval)
  return(min(c(1,calculated_pval)))
}
t5_runs <- runs(t5_test)


# EXTRA

# 
# ## TÄMÄ TOIMII
# 
# # Data vektorina
# e_data <- c(24,32,47,36,27,28,38,45,34,21,17,29,41,35,29,23,30,49,38,29)
# # Aineiston lukumäärä
# e_n <- length((e_data))
# # Mediaani
# e_med <- median(e_data)
# # Tämä generoi 0/1 vektorin
# e_generate_vec <- function(x, med) {
#   vec0 <- c()
#   for (i in 1:length(x)){
#     if (x[i] <= med ){
#       new_val <- 0
#     }
#     else{
#       new_val <- 1
#     }
#     vec0 <- append(vec0,new_val) 
#   }
#   return(vec0)
# }
# # Tällä kutsutaan
# e_test <- e_generate_vec(e_data, e_med)
# 
# # Tämä tekee rle testin binäärivektorille
# e_r <- rle(e_test)
# # Tällä saadaan runien lukumäärä
# e_runs <- length(e_r$lengths)
# # Nollien lukumäärä
# e_zeroes <- sum(e_r$lengths[e_r$values == 0])
# # Ykkösten lukumäärä
# e_ones <- sum(e_r$lengths[e_r$values == 1])
# # P-arvo
# 2 * pruns(e_runs, e_zeroes, e_ones)
