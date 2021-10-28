# TÉCNICAS DE MODELAGEM
################################################################################

# FUNÇÕES AUXILIARES
################################################################################
increment = function(x) {
  eval.parent(substitute(x <- x + 1))
}

inflacao = list()
inflacao[['2010']] = 0.0591
inflacao[['2011']] = 0.0650
inflacao[['2012']] = 0.0584
inflacao[['2013']] = 0.0591
inflacao[['2014']] = 0.0641
inflacao[['2015']] = 0.1067
inflacao[['2016']] = 0.0629
inflacao[['2017']] = 0.0295
inflacao[['2018']] = 0.0375
inflacao[['2019']] = 0.0431
inflacao[['2020']] = 0.0452

fixInflation = function (acc_value, year) {
  if (year >= 2020) {
    return(acc_value)
  } else {
    new_acc_value = as.double(acc_value * (1 + inflacao[[as.character(year)]]))
    new_year = year + 1
    fixInflation(new_acc_value, new_year)
  }
}

# AMBIENTE E BIBLIOTECAS
################################################################################
library(fpp2)
library(urca)
library(tseries)
library(ggplot2)
library(patchwork)
# library(lmtest)

setwd("/home/irwinarruda/Documents/modelagem/modelagem-receitas-de-pedágio/")
data = read.csv(
  file = "receita_de_pedagio_antt.csv", 
  header = T, 
  sep = ";", 
  dec= ",", 
  fileEncoding = "ISO-8859-1"
)
summary(data)
str(data)

# TRATAMENTO DE DADOS
################################################################################
nomesConcess = levels(factor(data[['concessionaria']]))
nomesConcess

dadosPorConcess = list()
nomesConcessTratados = levels(factor(data[['concessionaria']]))

for (concess in nomesConcess) {
  buffer = 0L
  concessDf = data[data['concessionaria'] == concess,]
  
  for (valor in concessDf[['valor']]) {
    if (is.na(valor)) {
      increment(buffer)
    }
  }

  if (buffer == 0 & length(concessDf[['valor']]) == 11) {
    rownames(concessDf) = 1:nrow(concessDf)
    for (i in 1:nrow(concessDf)) {
      concessDf[i, 'valor'] = fixInflation(
        acc_value = concessDf[i, 'valor'], 
        year = aconcessDf[i, 'ano']
      )
    }
    dadosPorConcess[[concess]] = ts(
      data = concessDf[['valor']], 
      start = 2010, frequency = 1
    )
  }
}

# Variável principal contendo todas as tseries
# chave                  -> valor
# dadosPorConcess$CONCER -> time series
dadosPorConcess$CRT

# PLOTAGEM
################################################################################

times = c(2010:2020)
ggplot() +
  geom_line(aes(x = times, y = dadosPorConcess$`AUTOPISTA FERNÃO DIAS`, color = "AUTOPISTA FERNÃO DIAS")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`AUTOPISTA FLUMINENSE`, color = "AUTOPISTA FLUMINENSE")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`AUTOPISTA LITORAL SUL`, color = "AUTOPISTA LITORAL SUL")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`AUTOPISTA PLANALTO SUL`, color = "AUTOPISTA PLANALTO SUL")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`AUTOPISTA REGIS BITTENCOURT`, color = "AUTOPISTA REGIS BITTENCOURT")) + 
  geom_line(aes(x = times, y = dadosPorConcess$CONCER, color = "CONCER")) + 
  geom_line(aes(x = times, y = dadosPorConcess$CRT, color = "CRT")) + 
  geom_line(aes(x = times, y = dadosPorConcess$ECOSUL, color = "ECOSUL")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`RODOVIA DO AÇO`, color = "RODOVIA DO AÇO")) + 
  geom_line(aes(x = times, y = dadosPorConcess$TRANSBRASILIANA, color = "TRANSBRASILIANA")) + 
  geom_line(aes(x = times, y = dadosPorConcess$`VIA BAHIA`, color = "VIA BAHIA")) + 
  xlab("Tempo (ano)") +
  ylab("Valor (reais)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +   
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))+
  scale_y_continuous(labels = scales::comma)

# Concessionárias Escolhidas
# AUTOPISTA LITORAL SUL, AUTOPISTA FLUMINENCE
# CONCER, CRT

nomesConcessTratados = names(dadosPorConcess)
nomesConcessTratados

# K_TEST - Kwiatkowski-Phillips-Schmidt-Shin
################################################################################
k_tests = list()
for (concess in nomesConcessTratados) { 
  k_tests[[concess]] = kpss.test(dadosPorConcess[[concess]], null="Level") # Level ou Trend
}
k_tests
k_tests$CONCER
k_tests$CRT
k_tests$`AUTOPISTA LITORAL SUL`
k_tests$`AUTOPISTA FLUMINENSE`

# QUANTIDADE DE ITERAÇÕES PARA SE TORNAR ESTACIONÁRIA
################################################################################
n_iter = list()
for (concess in nomesConcessTratados) { 
  n_iter[[concess]] = ndiffs(dadosPorConcess[[concess]], alpha = 0.05)
}
n_iter

# ANALISANDO AS DIFERENÇAS
################################################################################
diff_dadosPorConcess = list()
for (concess in nomesConcessTratados) { 
  diff_dadosPorConcess[[concess]] = diff(dadosPorConcess[[concess]])
}
diff_dadosPorConcess

# K_TEST DA DIFF
################################################################################
diff_k_test = list()
for (concess in nomesConcessTratados) { 
  diff_k_test[[concess]] = kpss.test(diff_dadosPorConcess[[concess]], type = "tau")
}
summary(diff_k_test$CONCER)

# PLOTAGEM DA DIFF
################################################################################
diff_times = c(2011:2020)
ggplot() +
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`AUTOPISTA FERNÃO DIAS`[10], x =  "Fernão Dias", color="FERNÃO DIAS")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`AUTOPISTA FLUMINENSE`[10], x = "Fluminense", color = "FLUMINENSE")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`AUTOPISTA LITORAL SUL`[10], x = "Litoral Sul", color = "LITORAL SUL")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`AUTOPISTA PLANALTO SUL`[10], x = "Planalto Sul", color = "PLANALTO SUL")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`AUTOPISTA REGIS BITTENCOURT`[10], x = "Regis..", color = "REGIS BITTENCOURT")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$CONCER[10], x = "Concer", color = "CONCER")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$CRT[10], x = "Crt", color = "CRT")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$ECOSUL[10], x = "Ecosul", color = "ECOSUL")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`RODOVIA DO AÇO`[10], x = "Rod do Aço", color = "RODOVIA DO AÇO")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$TRANSBRASILIANA[10], x = "Transbra...", color = "TRANSBRASILIANA")) + 
  geom_bar(stat="identity", aes(y = diff_dadosPorConcess$`VIA BAHIA`[10], x = "Via BA", color = "VIA BAHIA")) + 
  xlab("Concessionárias") +
  ylab("Valor (reais)") + 
  theme(axis.text.x=element_text(size=rel(0.7), angle=90)) +
  scale_y_continuous(labels = scales::comma)

# FIT ARIMA
################################################################################
fit_arima_concer = auto.arima(dadosPorConcess$CONCER,d=1,D=1, stepwise = FALSE, approximation=FALSE, trace=TRUE) # xreg = 
fit_arima_crt = auto.arima(dadosPorConcess$CRT,d=1,D=1, stepwise = FALSE, approximation=FALSE, trace=TRUE) # xreg = 
fit_arima_fluminense = auto.arima(dadosPorConcess$`AUTOPISTA FLUMINENSE`,d=1,D=1, stepwise = FALSE, approximation=FALSE, trace=TRUE) # xreg = 
fit_arima_litoral = auto.arima(dadosPorConcess$`AUTOPISTA LITORAL SUL`,d=1,D=1, stepwise = FALSE, approximation=FALSE, trace=TRUE) # xreg = 
print(summary(fit_arima_concer))

checkresiduals(fit_arima_concer)
checkresiduals(fit_arima_crt)
checkresiduals(fit_arima_fluminense)
checkresiduals(fit_arima_litoral)

################################################################################

# DF - Dickey-Fuller Aumentado
DF_tests = list()
for (concess in nomesConcessTratados) { 
  DF_tests[[concess]] = ur.df(dadosPorConcess[[concess]], type="none", lags=1)
}
DF_tests
summary(DF_tests$CONCER)

