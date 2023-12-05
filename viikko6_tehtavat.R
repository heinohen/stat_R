# 8 henkiöä osallistui kuntokuurille. Heidän painonsa mitattiin ennen
# kuntokuurin alkamista ja kuntokuurin päätyttyä
# Onko painojen jakauma muuttunut kuntokuurin aikana?

ennen <- c(64.6,64.3,53.3,88.2,85.9,102.8,69.0,82.5)
jalkeen <- c(65.7,67.5,58.1,85.0,82.7,97.9,70.1,82.5)

srtest <- function(x, y) {
  # Code the desired functionality here
  if (length(x) != length(y)) {
    return(NULL)
  }
  
  erotus <- x - y
  
  # H_0 painojen jakauma on sama
  # H_1 painojen jakauma ei ole sama
  
  # kaikki erisuuret kuin nolla kiinnostaa
  erotustarkastus <- erotus[erotus != 0]
  anna <- length(erotustarkastus)
  erotussortti <- erotustarkastus[order(abs(erotustarkastus))]
  erotusrankki <- rank(abs(erotussortti), ties.method = 'average')
  
  ts <- 0
  for (i in 1:length(erotussortti)) {
    if (erotussortti[i] < 0) {
      ts <- ts + erotusrankki[i]
    }
  }
  # E(TS) = n*(n+1) / 4
  odotusarvo <- (anna * (anna + 1)) / 4
  # Var(TS) = n*(n+1)*(2 * n + 1) / 24
  varianssi <- ((anna * (anna + 1)) * (2 * anna +1)) / 24
  
  # p-arvo = 2 * min{P(TS <= 13), P(TS >= 13)}
  
  if (ts >= odotusarvo) {
    # koska P TS <= 13 < E(TS)=14
    # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
    pee <- (ts - odotusarvo) / sqrt(varianssi)
    pval <- 2 * pnorm(pee, lower.tail = FALSE)
    return(pval)
  } else {
    # koska P TS <= 13 < E(TS)=14
    # 2P(TS <= 13) ~ 2P(TS <= 13.5)  JATKUVUUSKORJAUS
    pee <- (ts - odotusarvo) / sqrt(varianssi)
    pval <- 2 * pnorm(pee)
    return(pval)
  }
}
