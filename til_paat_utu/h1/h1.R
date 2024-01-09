# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██     ██      
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ███████ 
# TEHTÄVÄ 2


# Kevään 2023 tilastollisen päättelyn peruskurssin toisen tenttikerran arvosanat olivat:
t2_arvosanat <- c(1, 0, 0, 0, 5, 2, 2, 0, 1, 1, 0, 0, 2, 0, 2, 3, 0, 0, 1, 2, 1, 0, 0, 0, 0, 1, 1, 4, 0, 1)
# Muodosta näistä frekvenssijakauma

t2_frekvenssitaulu <- table(t2_arvosanat)
t2_frekvenssitaulu
#ja piirrä vastaava pylväskuvio.
barplot(t2_frekvenssitaulu)



#Mikä on arvosanojen moodi?
library("DescTools")
Mode(t2_arvosanat)
# .:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:._.:*~*:.

# ████████ ███████ ██   ██ ████████  █████  ██    ██  █████      ██████  
#    ██    ██      ██   ██    ██    ██   ██ ██    ██ ██   ██          ██ 
#    ██    █████   ███████    ██    ███████ ██    ██ ███████      █████  
#    ██    ██      ██   ██    ██    ██   ██  ██  ██  ██   ██          ██ 
#    ██    ███████ ██   ██    ██    ██   ██   ████   ██   ██     ██████  
# TEHTÄVÄ 3 

# Jatkoa edelliseen tehtävään. Määritä arvosanojen (otos)keskiarvo, mediaani ja kvartiilit. Voit tehdä
# tämänkin tehtävän joko käsin tai R:llä.
print("TEHTÄVÄ 3")
t3_arvosanat <- c(1, 0, 0, 0, 5, 2, 2, 0, 1, 1, 0, 0, 2, 0, 2, 3, 0, 0, 1, 2, 1, 0, 0, 0, 0, 1, 1, 4, 0, 1)

#(otos)keskiarvo
mean(t3_arvosanat)

#mediaani
median(t3_arvosanat)

#kvartaalit
quantile(t3_arvosanat)
quantile(t3_arvosanat, type=2)

x <- c(3,1,4,0,1,2,6,6)
y <- c(1:8)

x+y
