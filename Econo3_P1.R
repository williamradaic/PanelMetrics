library(haven)
library(plm)

df <- read_dta("G:/My Drive/FGV EESP/5o Semestre/Econo III/Problemas 1-4, 9 , 10 e 13.dta")
df = df[,c(2,1,3:ncol(df))]

pooled_model <- plm(EARNINGS ~ UNION, data = df, model = "pooling")
summary(pooled_model)

# Adicionar mais regressores como controles
# O que exatamente estamos fazendo com uma pooled regression? Modelo mais próximo da regressão cross-section tradicional em que assumimos pra fora do modelo autocorrelação, correlação serial dos erros, nenhuma dinâmica? Ganhamos apenas n maior com o painel?
# Estimativas incluem cluster indexada nos indivíduos? iid é em toda a amostra; apenas entre indivíduos diferentes. e a estrutura temporal está sendo ignorada? 

