library(plm)
data("Wages", package = "plm")
Wage_painel = pdata.frame(Wages, index=595)

# b: pooled OLS ###################

pooledm <- plm(lwage ~ bluecol + exp + ed + black + sex, data = Wage_painel, model = "pooling") # Modelo proposto no enunciado (b)
summary(pooledm, vcov=vcovHC(pooledm, method = "white1")) # summary com correção para erros robustos a heteroscedasticidade


# c: #############################

# A estimativa encontrada para o prêmio salarial de bluecol não é crível, pois o sinal negativo contraria a correlação intuitiva entre empregos com altos salários e o "setor" de colarinho branco. Assim, há fortíssimas evidências de má especificação do modelo. Para corrigir esse problema, podemos implementar um modelo que incorpora efeitos fixos, de modo controlar todas as características relevantes específicas do indivíduo e constantes no tempo.

# Para isso, utilizaremos o estimador within (desenvolvido na questão 3b,c), que expressa as variáveis em termos de variações da média temporal.

withinm <- plm(lwage ~ bluecol + exp + ed + black + sex, data = Wage_painel, model = "within") 
summary(withinm, vcov=vcovHC(withinm, method = "white1")) # summary com correção para erros robustos a heteroscedasticidade

# Notamos que o summary reporta os coeficientes apenas das variáveis bluecol e exp. As demais dummies não variam ao longo do tempo e são, portanto, excluídas para que a transformação within atenda à hipótese de full rank. 

# Quanto ao coeficiente reportado para bluecol, novamente observamos um sinal negativo -- porém, a estimativa não é significativa. O resultado, apesar de não seguir a razoável expectativa de um sinal positivo, ao menos não a contraria explicitamente com um sinal negativo significante. Em que pese o resultado insignificante estatisticamente, observamos também uma diminuição do valor de beta com relação à estimativa anterior, o que indica endogeneidade do pooled OLS. 


# d: ###################################

# É interessante controlar o modelo por efeitos de tempo pois é razoável dizer que há uma tendência de aumento dos salários -- seja por fatores como a inflação ou o aumento da produtividade. Assim, podemos incluir dummies de tempo no modelo: 

withinm_t <- plm(lwage ~ bluecol + exp + ed + black + sex + as.factor(time), data = Wage_painel, model = "within") 
summary(withinm_t, vcov=vcovHC(withinm_t, method = "white1")) # summary com correção para erros robustos a heteroscedasticidade

# Seguimos com o mesmo resultado com relação ao parâmetro de interesse (bluecol) -- i.e., sem evidências significativas do prêmio de trabalhos de colarinho branco. Entretanto, agora temos algumas dummies de ano com efeito significante (e positivo), o que corrobora nossa suposição de tendência dos salários.


# e: ###################################

# Modelo de efeitos aleatórios:

randomm <- plm(lwage ~ bluecol + exp + ed + black + sex, data = Wage_painel, model = "random") # Modelo proposto no enunciado (b), com efeitos aleatórios
summary(randomm, vcov=vcovHC(randomm, method = "white1")) # summary com correção para erros robustos a heteroscedasticidade

# O modelo de efeitos aleatórios considera que as características não observadas (c_i) fazem parte do termo de erro (Wooldridge, p. 291). Isso implica que, para haver consistência, é necessário impor a hipótese de ortogonalidade também entre c_i e x_it. Isso implica que: 
# Hipótese RE.1 (Wooldridge): E(u_it | x_i, c_i) = 0, t = 1, ..., T; E(c_i | x_i) = E(c_i) = 0.
# A hipótese acima impõe exogeneidade estrita entre x_i e c_i, para além da já tradicional exogeneidade entre u_i e x_i. 
# Uma maneira alternativa de se escrever essa hipótese é: E(v_it | x_i) = 0, t = 1, 2, ..., T, em que v_it = c_i + u_it.

# Além disso, ainda precisamos da hipótese tradicional de full rank: 
# rank E(X_i' \Omega^-1 X_i) = K
# Note que, aqui, consideramos uma estrutura de GLS devido à estrutura do erro.

# As hipóteses acima se referem à identificação. Podemos também impor uma hipótese de inferência que garante a eficiência do FGLS:
# E(u_i u_i' | x_i, c_i) = \sigma_u^2 I_T; E(c_i^2 | X_i) = \sigma_c^2

