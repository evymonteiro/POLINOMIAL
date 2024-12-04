####Os dados referem-se a produções de milho (Y), em kg/parcela, de um experimento
####casualizado em blocos de adubação de milho com diferentes doses (X) de P2O5.

#dados <- read.csv("D:\\ARQUIVOS_POS_ESTATISTICA\\EXPERIMENTAL\\REGRESSAO_POLINOMIAL\\dados.txt", header = TRUE)
#dados

dados <- data.frame(
  Bloco = c("I", "II", "III", "I", "II", "III", "I", "II", "III", "I", "II", "III", "I", "II", "III"),
  Tratamento = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  Dose = c(0, 0, 0, 25, 25, 25, 50, 50, 50, 75, 75, 75, 100, 100, 100),
  Producao = c(85.0, 86.0, 84.0, 94.5, 96.0, 95.8, 99.5, 98.0, 104.0, 93.0, 96.0, 90.5, 83.0, 80.0, 78.5)
)

attach(dados)

#1:

#a) Existe diferença significativa entre as doses de P2O5? a = 5% 

#O experimento proposto é do tipo delineamento em blocos casualizados, com 
#tratamentos quantitativos, dessa forma, para verificar se existe diferença
#entre os níveis de dosagem, deve-se proceder com a ANOVA. 


#Inicialmente, foi proposto o modelo aov, para estudar a variabilidade entre
#as doses:

modelo <- aov(Producao ~ as.factor(Dose) + as.factor(Bloco)) 
# factor transforma valores categóricos em níveis distintos.

#Posteriomente, a análise de variância foi executada:

anova(modelo)

#As hipóteses a serem testadas por meio da ANOVA:

#H0: As médias das produções de milho são iguais para todas as doses de P2O5.
#H1: Pelo menos uma média é diferente. 

#Ao nível de significância de 5%, com 4 graus liberdade, a hipótese nula foi rejeitada
#uma vez que o p-value foi significativo (5,087^-5), concluindo que existe 
#pelo menos uma média de produção de milho com valor estatisticamente diferente.

##Pressupostos: 

#Para validar a inferência obtida via ANOVA, é necessária a validação de
#pressupostos estatisticos de normalidade, homocedasticidade e independência 
#de resíduos. Dessa forma:

#Para normalidade de resíduos foi aplicado o teste Shapiro.Wilk, sob as
#hipóteses:
#H0: Os resíduos seguem distribuição normal vs.
#H1: Os resíduos não seguem distribuição normal. 

shapiro.test(residuals(modelo))  

#O p-value obtido foi de 0,6136 indicando que não há evidências estatísticas
#contra H0 ao nível de significância de 5%, ou seja, não há evidências para rejeitar
#a hipótese de que os resíduos apresentam normalidade. 

#Para corroborar com o teste, foi aplicado o gráfico Q-Q: 

qqnorm(residuals(modelo))
qqline(residuals(modelo))

#O gráfico obtido indica a normalidade dos resíduos, uma vez que os pontos
#estão próximos da reta, caso houvesse uma dispersão ou distanciamento da reta
#seria um indicativo de não-normalidade.

#Homocedasticidade: 

# Teste de Bartlett sob as hipóteses:

#H0: Os resíduos são homocedásticos vs.
#H1: Os resíduos não são homocedásticos.

bartlett.test(Producao ~ Tratamento, data = dados)

#O p-value obtido foi de 0,419 indicando que não há evidências estatísticas
#contra H0 ao nível de significância de 5%, ou seja, não há evidências para rejeitar
#a hipótese de que os resíduos são homocedásticos. 


#De forma visual, foi realizado o boxplot dos dados, afim de verificar
#visualmente o comportamento dos resíduos:

boxplot(residuals(modelo) ~ dados$Tratamento, 
        col = "lightblue",
        main = "Resíduos por Tratamento",
        xlab = "Tratamento", 
        ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2, lwd = 2)

#É possível identificar que os tratamentos/dosagens 3, 4 e 5 apresentam uma maior variabilidade
#em relação aos tratamentos/dosagens 1 e 2, devido ao achatamento das caixas, porém, sem presença
#de outliers. 

#Independência de resíduos:

#Para verificar se os resíduos estão autocorrelacionados, foi criado o gráfico
#acf: 

acf(residuals(modelo), main = "ACF dos Resíduos")

#Por meio do gráfico, é possível identificar que os resíduos são independentes,
#pois nenhuma linha ultrapassou a linha pontilhada azul. Entretanto, para análise
#ser ainda mais robusta, também foi utilizado o teste de Durbin-Watson:

library(car)
dwtest(modelo)

#As hipóteses: 
#H0: Os resíduos são independentes vs.
#H1: Os resíduos não são independentes.

#Ao nível de significância de 5% e com p-value = 0,7882, não há
#evidências para rejeitar H0, sendo assim, conclui-se com 95% de confiança
#que os resíduos são independentes. 

##Com todos os pressupostos atendidos, foi proposto um modelo que representasse
#a relação entre a dosagem e a produção de milho:

#b) Equação de regressão polinomial mais adequada,  a = 5%:
#Após concluir que existe diferença entre as dosagens, o modelo de regressão
#busca modelar como os valores de dosagens influenciam os diferentes níveis de produção:

#Modelo polinomial de primeiro grau: 

Lm1 <- lm(Producao ~ (Dose) + (Bloco)) #Linear Model 1
Lm2 <- lm(Producao ~ as.factor(Dose) + as.factor(Bloco)) #Linear Model 2 (com função factor)

anova(Lm1,Lm2)

#As hipóteses testadas:

#H0: o modelo é adequado vs H1: O modelo não é adequado. 

##Com p-value = 3,089^-5, ao nível de significância de 5%, os resíduos
#de regressão foram significativos, indicando que existe um modelo de grau
#superior mais adequado.

#Modelo polinomial de segundo grau: 

Lm3 <- lm(Producao ~ (Dose) + I(Dose^2) + Bloco) #Modelo 3
Lm4 <- lm(Producao ~ as.factor(Dose) + as.factor(Dose^2) +as.factor(Bloco)) # Modelo 4

#As hipóteses testadas:
#H0: o modelo é adequado vs H1: O modelo não é adequado. 

anova(Lm3,Lm4)

##Com p-value = 0,5163, ao nível de significância de 5%, os resíduos
#de regressão não foram significativos, indicando que o modelo polinomial de grau 2 é adequado,
#e que, modelos com graus superiores não são estatisticamente significativos
#quanto sua capacidade de melhor descrever os dados, sendo assim, cabíbel escolher
#o modelo mais parcimonioso.

plot(Lm3)

## Gráfico associado ao modelo ajustado, com valores observados e a curva ajustada: 
anova(Lm4)

library(ExpDes.pt)
reg.poly(Producao, Dose, glres=8, SQres= 47.42, gltrat=4, SQtrat=782.45)

Lm3

par(mfrow=c(1,1))
x<-seq(0,100,1)
plot(Dose, Producao,xlab="Dose", ylab="Produção",main="Curva ajustada e valores observados"
)
curve(84.881905 + 0.624648*x  -0.006697*x^2, 0, 100, col=2, add=TRUE)

##O gráfico obtido é uma parábola com concavidade negativa, o que indica que existe um
#valor máximo de produção, associado a uma dosagem de P2O5, visualmente, pode-se
#dizer que a dosagem que oferece o valor máximo de produção está entre 40 e 60 doses,
#porém, para identificar esse valor, é necessário calcular a derivada do modelo ajustado.


##Pressupostos para o modelo linear: 

#Normalidade:
#H0: Os resíduos seguem distribuição normal vs.
#H1: Os resíduos não seguem distribuição normal. 

shapiro.test(residuals(Lm3))  

#O p-value obtido foi de 0,2277 indicando que não há evidências estatísticas
#contra H0 ao nível de significância de 5%, ou seja, não há evidências para rejeitar
#a hipótese de que os resíduos apresentam normalidade. 

#Gráfico Q-Q: 

qqnorm(residuals(Lm3))
qqline(residuals(Lm3))

#O gráfico obtido indica a normalidade dos resíduos.

#Homocedasticidade: 

# Teste de Breusch-Pagan sob as hipóteses:

#H0: Os resíduos são homocedásticos vs.
#H1: Os resíduos não são homocedásticos.

library(lmtest)
bptest(Lm3)

#O p-value obtido foi de 0,3049 indicando que não há evidências estatísticas
#contra H0 ao nível de significância de 5%, ou seja, não há evidências para rejeitar
#a hipótese de que os resíduos são homocedásticos. 


#Independência de resíduos:

#Para verificar se os resíduos estão autocorrelacionados, foi criado o gráfico
#acf: 

acf(residuals(Lm3), main = "ACF dos Resíduos")

#Por meio do gráfico, é possível identificar que os resíduos são independentes,
#pois nenhuma linha ultrapassou a linha pontilhada azul. Entretanto, para análise
#ser ainda mais robusta, também foi utilizado o teste de Durbin-Watson:

dwtest(Lm3)

#As hipóteses: 
#H0: Os resíduos são independentes vs.
#H1: Os resíduos não são independentes.

#Ao nível de significância de 5% e com p-value = 0,9334, não há
#evidências para rejeitar H0.

##Com todos os pressupostos atendidos, é possível realizar inferências a partir
#do modelo proposto, incluindo valores de máxima produção e dosagem mínima que otimize
#a produção. 