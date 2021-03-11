library(haven)
p6 <- read_dta("G:/My Drive/FGV EESP/5o Semestre/Econo III/Problem6.dta")

library(plm)

# Arrumando a base 

p6df <- pdata.frame(p6,index = c("state","year"))

# Modelo first differences

fd <- plm(polpc ~ lcriv + lcrip + black + metro + unem + incpc + ag0_14 + ag15_17 + ag18_24 + ag25_34, data = p6df, model = "fd") # Adiciono lag polpc em X? Problema de autocorrelação dos erros (p. 245 A&K)
summary(fd) 

# Modelo em nível com lag em X

p6df$polpc_l <- lag(p6df$polpc, k = 1, shift = "time")

lagm <- plm(polpc ~ polpc_l + lcriv + lcrip + black + metro + unem + incpc + ag0_14 + ag15_17 + ag18_24 + ag25_34, data = p6df, model = "within") # Falta alguma correção na inferência? p valor mt baixo
summary(lagm)
