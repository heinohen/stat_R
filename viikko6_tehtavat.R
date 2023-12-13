# # # 8 henkiöä osallistui kuntokuurille. Heidän painonsa mitattiin ennen
# # # kuntokuurin alkamista ja kuntokuurin päätyttyä
# # # Onko painojen jakauma muuttunut kuntokuurin aikana?
# # 
# # ennen <- c(64.6,64.3,53.3,88.2,85.9,102.8,69.0,82.5)
# # jalkeen <- c(65.7,67.5,58.1,85.0,82.7,97.9,70.1,82.5)
# # 
# # bsumma <- sapply(B, function(x) prank[x])
# # srtest <- function(x, y) {
# #   # Code the desired functionality here
# #   if (length(x) != length(y)) {
# #     return(NULL)
# #   }
# #   
# #   erotus <- x - y
# #   
# #   # H_0 painojen jakauma on sama
# #   # H_1 painojen jakauma ei ole sama
# #   
# #   # kaikki erisuuret kuin nolla kiinnostaa
# #   erotustarkastus <- erotus[erotus != 0]
# #   anna <- length(erotustarkastus)
# #   erotussortti <- erotustarkastus[order(abs(erotustarkastus))]
# #   erotusrankki <- rank(abs(erotussortti), ties.method = 'average')
# #   
# #   ts <- 0
# #   for (i in 1:length(erotussortti)) {
# #     if (erotussortti[i] < 0) {
# #       ts <- ts + erotusrankki[i]
# #     }
# #   }
# #   # E(TS) = n*(n+1) / 4
# #   odotusarvo <- (anna * (anna + 1)) / 4
# #   # Var(TS) = n*(n+1)*(2 * n + 1) / 24
# #   varianssi <- ((anna * (anna + 1)) * (2 * anna +1)) / 24
# #   
# #   # p-arvo = 2 * min{P(TS <= 13), P(TS >= 13)}
# #   
# #   if (ts >= odotusarvo) {
# #     # koska P TS <= 13 < E(TS)=14
# #     # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
# #     pee <- (ts - odotusarvo) / sqrt(varianssi)
# #     pval <- 2 * pnorm(pee, lower.tail = FALSE)
# #     return(pval)
# #   } else {
# #     # koska P TS <= 13 < E(TS)=14
# #     # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
# #     pee <- (ts - odotusarvo) / sqrt(varianssi)
# #     pval <- 2 * pnorm(pee)
# #     return(pval)
# #   }
# # }
# # 
# # 
# # # # Luo funktio runs, joka ottaa argumenttinaan vektorin x, jossa voit olettaa olevan vain
# # # nollia ja ykkösiä. Funktion tulee ensin laskea Rossin kirjan kappaleessa 14.5 esitetyn
# # # ”Runs” -testin testisuureen arvo r, joka on vektorin x käsittämien jonojen määrä
# # # (number of runs). Tämän jälkeen funktion tulee laskea testisuureelle nollahypoteesin
# # # mukainen odotusarvo ja varianssi
# # 
# # # W,L,L,L,W,L,L,W,L,L,W,L,L,W,L,W,L,LL,L,W,L,W,L
# # #EX 14.12 
# # # W = 1
# # # L = 0
# # 
# # t14_12 <- c(1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,1,0,1,0)
# # 
# # length(t14_12)
# # 
# # r <- rle(t14_12)
# # onesum <- sum(r$lengths[r$values == 1])
# # zerosum <- sum(r$lengths[r$values == 0])
# # length(r$lengths)
# # zerosum
# # onesum
# # 
# # mu <- (2 * onesum * zerosum) / (zerosum + onesum) +1
# # var <- ((2*onesum * zerosum) * (2 * onesum * zerosum - onesum - zerosum)) / ((zerosum + onesum)^2 * (onesum + zerosum -1))
# # 
# # t14_12_ts <- length(r$lengths)
# # 
# # parvo <- 0
# # if (t14_12_ts >= mu - 0.5) {
# #   # koska P TS <= 13 < E(TS)=14
# #   # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
# #   pee <- (t14_12_ts - mu - 0.5) / sqrt(var)
# #   pval <- 2 * pnorm(pee, lower.tail = FALSE)
# #   parvo <- pval
# # } else {
# #   # koska P TS <= 13 < E(TS)=14
# #   # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
# #   pee <- (t14_12_ts - mu + 0.5) / sqrt(var)
# #   pval <- 2 * pnorm(pee)
# #   parvo <- pval
# # }
# # 
# # ## EX 14.17
# # Suppose that 10 experts are to analyze three different wines, giving numer-
# #   ical scores ranging from 0 (terrible wine) to 10 (exceptional wine) to each
# # wine. Use the resulting scores given below to test, at the 5 percent level
# # of significance, the null hypothesis that the wines are of identical qual-
# #   ity.
# # Numerical Scores given to Wines A, B,
# # C by Ten Experts
# # 
# # x1 <- c(7.21,6.60,6.22,7.38,8.20,7.07,6.72,5.89,9.02,6.88)
# # x2 <- c(6.04,6.26,7.44,8.02,6.91,6.65,7.11,7.15,6.61,7.29)
# # x3 <- c(6.42,4.80,7.05,5.84,7.13,6.54,7.04,5.22,6.83,7.08)
# 
# # erotussortti <- erotustarkastus[order(abs(erotustarkastus))]
# # erotusrankki <- rank(abs(erotussortti), ties.method = 'average')
# 
# 
# 
# # x_1 <- c(4.25,5.72,5.21)
# # x_2 <- c(9.98,8.48,9.59)
# # x_3 <- c(6.66,9.29,6.40)
# # x_4 <- c(5.24,7.09,6.56)
# 
# # x1 <- 1:5
# # x2 <- rep(2, 5)
# # x3 <- c(-1, 2, 0.5, 3.2, 0.1)
# #  
# 
# # 
# # 
# # xt <- sort(c(x1,x2,x3))
# # xt_ranked <- rank(xt)
# # 
# # a_ranks <- c()
# # for (i in 1:length(x1)) {
# #     for (j in 1:length(xt)) {
# #     if (x1[i] == xt[j]) {
# #             a_ranks <- append(a_ranks, xt_ranked[j])
# #             break
# #     }
# #   }
# # }
# # 
# # b_ranks <- c()
# # for (i in 1:length(x2)) {
# #   for (j in 1:length(xt)) {
# #     if (x2[i] == xt[j]) {
# #       b_ranks <- append(b_ranks, xt_ranked[j])
# #       break
# #     }
# #   }
# # }
# # 
# # c_ranks <- c()
# # for (i in 1:length(x3)) {
# #   for (j in 1:length(xt)) {
# #     if (x3[i] == xt[j]) {
# #       c_ranks <- append(c_ranks, xt_ranked[j])
# #       break
# #     }
# #   }
# # }
# # 
# # a_sum <- sum(a_ranks)
# # b_sum <- sum(b_ranks)
# # c_sum <- sum(c_ranks)
# # 
# # x_sum <- sum(a_sum^2, b_sum^2, c_sum^2)
# # x_N <- length(x1) + length(x2) + length(x3)
# # 
# # x_H_first <- 12 / (x_N * (x_N + 1))
# # x_H_middle <- (1/length(x1)) * x_sum 
# # x_H_end <- 3 * (x_N + 1)
# # x_TS <- x_H_first * x_H_middle - x_H_end
# # x_pval <- 1 - pchisq(x_TS , df = 3-1)
# # 
# # 
# # ajat <- c(24,32,47,36,27,28,38,45,34,21,17,29,41,35,29,23,30,49,38,29)
# # 
# # median(ajat)
# 
# # indexit <- c(910,890,1010,1033,1080,1275,1288,1553,1980,2702)
# # median(indexit)
# # 
# # r <- rle(indexit)
# # r$lengths
# # 
# # paivia <- c(5,7,4,6,5,3,4,5,3,1,2,1,0)
# # paivia_2 <- paivia^2
# # sum(paivia_2)
# # 
# # ekaosa <- (13*(13+1)*(2*13+1)) / (6 * (13-1))
# # tokaosa <- 216 - 13 * 3.54^2
# # kolmasosa <- ((13^2) * (14)^2) / (4 * (14 - 1))
# # neljasosa <- 3.54^2 - (216/13)
# # (ekaosa * tokaosa) + (kolmasosa * neljasosa)
# 
# 
# # Week 6 Exercise 1
# 
# 
# 
# srtest <- function(x, y) {
#   # Code the desired functionality here
#   if (length(x) != length(y)) {
#     return(NULL)
#   }
#   
#   erotus <- x - y
#   
#   # H_0 painojen jakauma on sama
#   # H_1 painojen jakauma ei ole sama
#   
#   # kaikki erisuuret kuin nolla kiinnostaa
#   erotustarkastus <- erotus[erotus != 0]
#   anna <- length(erotustarkastus)
#   erotussortti <- erotustarkastus[order(abs(erotustarkastus))]
#   erotusrankki <- rank(abs(erotussortti), ties.method = 'average')
#   
#   ts <- 0
#   for (i in 1:length(erotussortti)) {
#     if (erotussortti[i] < 0) {
#       ts <- ts + erotusrankki[i]
#     }
#   }
#   
#   
#   # E(TS) = n*(n+1) / 4
#   odotusarvo <- (anna * (anna + 1)) / 4
#   # Var(TS) = n*(n+1)*(2 * n + 1) / 24
#   varianssi <- ((anna * (anna + 1)) * (2 * anna +1)) / 24
#   
#   # p-arvo = 2 * min{P(TS <= 13), P(TS >= 13)}
#   
#   if (ts >= odotusarvo) {
#     # koska P TS <= 13 < E(TS)=14
#     # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
#     pee <- (ts - odotusarvo) / sqrt(varianssi)
#     pval <- 2 * pnorm(pee, lower.tail = FALSE)
#     return(pval)
#   } else {
#     # koska P TS <= 13 < E(TS)=14
#     # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
#     pee <- (ts - odotusarvo) / sqrt(varianssi)
#     pval <- 2 * pnorm(pee)
#     return(pval)
#   }
#   
# }
# 
# 
# 
# # Week 6 Exercise 2
# 
# runs <- function(x) {
#   # Code the desired functionality here
#   r <- rle(x)
#   onesum <- sum(r$lengths[r$values == 1])
#   zerosum <- sum(r$lengths[r$values == 0])
#   mu <- (2 * onesum * zerosum) / (zerosum + onesum) +1
#   var <- ((2*onesum * zerosum) * (2 * onesum * zerosum - onesum - zerosum)) / ((zerosum + onesum)^2 * (onesum + zerosum -1))
#   x_ts <- length(r$lengths)
#   
#   calculated_pval <- 0
#   if (x_ts >= mu - 0.5) {
#     pee <- (x_ts - mu - 0.5) / sqrt(var)
#     pval <- 2 * pnorm(pee, lower.tail = FALSE)
#     calculated_pval <- pval
#   } else {
#     pee <- (x_ts - mu + 0.5) / sqrt(var)
#     pval <- 2 * pnorm(pee)
#     calculated_pval <- pval
#   }
#   return(min(c(1,calculated_pval)))
# }
# 
# 
# 
# 
# 
# # Week 6 Exercise 3
# 
# # You are NOT allowed to use the function kruskal.test in your solution!
# 
# mytest <- function(x1, x2, x3) {
#   # Code the desired functionality here
#   
#   xt <- sort(c(x1,x2,x3))
#   xt_ranked <- rank(xt,ties.method = "average")
#   
#   a_ranks <- c()
#   for (i in 1:length(x1)) {
#     for (j in 1:length(xt)) {
#       if (x1[i] == xt[j]) {
#         a_ranks <- append(a_ranks, xt_ranked[j])
#         break
#       }
#     }
#   }
#   
#   print(paste0("panini1 R_1: ", a_ranks))
#   
#   
#   
#   b_ranks <- c()
#   for (i in 1:length(x2)) {
#     for (j in 1:length(xt)) {
#       if (x2[i] == xt[j]) {
#         b_ranks <- append(b_ranks, xt_ranked[j])
#         break
#       }
#     }
#   }
#   
#   print(paste0("panini2 R_2: ", b_ranks))
#   
#   c_ranks <- c()
#   for (i in 1:length(x3)) {
#     for (j in 1:length(xt)) {
#       if (x3[i] == xt[j]) {
#         c_ranks <- append(c_ranks, xt_ranked[j])
#         break
#       }
#     }
#   }
#   
#   print(paste0("panini3 R_3: ", c_ranks))
#   
#   
#   
#   
#   a_sum <- sum(a_ranks)
#   b_sum <- sum(b_ranks)
#   c_sum <- sum(c_ranks)
#   
#   print(paste0("panini1 ranksum: ", a_sum))
#   print(paste0("panini2 ranksum: ", b_sum))
#   print(paste0("panini3 ranksum: ", c_sum))
#   
#   a_sum2 <- a_sum^2
#   b_sum2 <- b_sum^2
#   c_sum2 <- c_sum^2
#   
#   print(paste0("panini1 R_i^2: ", a_sum2))
#   print(paste0("panini2 R_i^2:: ", b_sum2))
#   print(paste0("panini3 R_i^2:: ", c_sum2))
#   
#   
#   
#   
#   
#   
#   x_N <- length(x1) + length(x2) + length(x3)
#   
#   print(paste0("n_i ", x_N))
#   
#   H_start <- 12 / (x_N * (x_N+1))
#   print(paste0("H_start ", H_start))
#   
#   
#   
#   H_x1 <- (a_sum2 / length(x1))
#   H_x2 <- (b_sum2 / length(x2))
#   H_x3 <- (c_sum2 / length(x3))
#   
#   
#   H_mid <- sum(H_x1, H_x2, H_x3)
#   
#   
#   
#   H_end <- 3 * (x_N + 1)
#   print(paste0("H_end ", H_end))
#   H <- H_start * H_mid - H_end
#   print(paste0("H: ", H))
#   pval <- 1 - pchisq(H, 3-1)
#   
#   return(pval)
#   
# }
# 
# 
# 
