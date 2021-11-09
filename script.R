########################## TÉCNICAS DE MODELAGEM ###############################


# AMBIENTE E BIBLIOTECAS
################################################################################
library(fpp2)
library(urca)
library(tseries)
library(ggplot2)
library(patchwork)
library(lmtest)
library(forecast)
library(vars)
setwd("C:/Users/arrud/Desktop/projetos/modelagem-rodovias-federais")

# DADOS PRINCIPAIS

receita_pedagio_df = read.csv(
  file = "receita_de_pedagio_antt.csv",
  header = T,
  sep = ";",
  dec = ",",
  fileEncoding = "ISO-8859-1"
)
str(receita_pedagio_df)

pib_df = read.csv(
  file = "evolucao_do_pib.csv",
  header = T,
  sep = ",",
  dec = ".",
  fileEncoding = "UTF-8"
)
str(pib_df)

pib_df_desde_2010 = pib_df[pib_df[['Ano']] > 2009,]
# Esse dado pertence a essa sessão pois é apenas utilizado para auxiliar no tratamento dos dados
inflacao_list = list()
for (ano in pib_df_desde_2010[['Ano']]) {
  inflacao_list[[as.character(ano)]] = as.double(gsub(",", "", pib_df_desde_2010[pib_df_desde_2010[['Ano']] == ano,][['Inflação...IPCA..var..acumulada.no.ano....Fonte.IBGE']])) / 100
}

# FUNÇÕES AUXILIARES
################################################################################

increment = function(x) {
  eval.parent(substitute(x <- x + 1))
}

fix_inflation = function(acc_value, year) {
  if (year >= 2020) {
    return(acc_value)
  } else {
    new_acc_value = as.double(acc_value * (1 + inflacao_list[[as.character(year)]]))
    new_year = year + 1
    fix_inflation(new_acc_value, new_year)
  }
}

cross_correlate_arima = function(arima, corr_ts) {
  x_residuals = arima$residuals
  y_model = Arima(corr_ts, model = arima)
  y_filtered = residuals(y_model)
  ccf(x_residuals, y_filtered)
  return(list(x_residuals, y_filtered))
}

# TRATAMENTO DE DADOS
################################################################################

pib_list = list()
pib_vector = c()
for (ano in pib_df_desde_2010[['Ano']]) {
  pib_no_ano = as.double(gsub(",", "", pib_df_desde_2010[pib_df_desde_2010[['Ano']] == ano,][['Produto.interno.bruto.em.R..do.último.ano...R...PIB.REAL.EM.R..DE.2020..Fonte..IBGE']]))
  pib_list[[as.character(ano)]] = pib_no_ano
  pib_vector = c(pib_vector, pib_no_ano)
}
pib_ts = ts(
  pib_vector,
  start = 2010,
  frequency = 1
)

nomes_concess = levels(factor(receita_pedagio_df[['concessionaria']]))

dados_por_concess = list()
for (concess in nomes_concess) {
  buffer = 0L
  concess_df = receita_pedagio_df[receita_pedagio_df['concessionaria'] == concess,]

  for (valor in concess_df[['valor']]) {
    if (is.na(valor)) {
      increment(buffer)
    }
  }

  if (buffer == 0 & length(concess_df[['valor']]) == 11) {
    rownames(concess_df) = 1:nrow(concess_df)
    for (i in 1:nrow(concess_df)) {
      concess_df[i, 'valor'] = fix_inflation(
        acc_value = concess_df[i, 'valor'],
        year = concess_df[i, 'ano']
      )
    }
    dados_por_concess[[concess]] = ts(
      data = concess_df[['valor']],
      start = 2010,
      frequency = 1
    )
  }
}

nomes_concess_tratados = names(dados_por_concess)


# Variável principal contendo todas as tseries
# chave                    -> valor
# dados_por_concess$CONCER -> time series
nomes_concess
inflacao_list
pib_list
pib_vector

pib_ts
dados_por_concess
nomes_concess_tratados

# ANÁLISE E INFERÊNCIA
################################################################################

times = c(2010:2020)
ggplot() +
  geom_line(aes(x = times, y = dados_por_concess$`AUTOPISTA FERNÃO DIAS`, color = "AUTOPISTA FERNÃO DIAS")) +
  geom_line(aes(x = times, y = dados_por_concess$`AUTOPISTA FLUMINENSE`, color = "AUTOPISTA FLUMINENSE")) +
  geom_line(aes(x = times, y = dados_por_concess$`AUTOPISTA LITORAL SUL`, color = "AUTOPISTA LITORAL SUL")) +
  geom_line(aes(x = times, y = dados_por_concess$`AUTOPISTA PLANALTO SUL`, color = "AUTOPISTA PLANALTO SUL")) +
  geom_line(aes(x = times, y = dados_por_concess$`AUTOPISTA REGIS BITTENCOURT`, color = "AUTOPISTA REGIS BITTENCOURT")) +
  geom_line(aes(x = times, y = dados_por_concess$CONCER, color = "CONCER")) +
  geom_line(aes(x = times, y = dados_por_concess$CRT, color = "CRT")) +
  geom_line(aes(x = times, y = dados_por_concess$ECOSUL, color = "ECOSUL")) +
  geom_line(aes(x = times, y = dados_por_concess$`RODOVIA DO AÇO`, color = "RODOVIA DO AÇO")) +
  geom_line(aes(x = times, y = dados_por_concess$TRANSBRASILIANA, color = "TRANSBRASILIANA")) +
  geom_line(aes(x = times, y = dados_por_concess$`VIA BAHIA`, color = "VIA BAHIA")) +
  xlab("Tempo (ano)") +
  ylab("Valor (reais)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1)) +
  scale_y_continuous(labels = scales::comma)

# Concessionárias Escolhidas
# AUTOPISTA LITORAL SUL, AUTOPISTA FLUMINENCE
# CONCER, CRT

# K_TEST - Kwiatkowski-Phillips-Schmidt-Shin
################################################################################

k_tests = list()
for (concess in nomes_concess_tratados) {
  # Level ou Trend
  k_tests[[concess]] = kpss.test(dados_por_concess[[concess]], null = "Level")
}
k_tests
k_tests$CONCER
k_tests$CRT
k_tests$`AUTOPISTA LITORAL SUL`
k_tests$`AUTOPISTA FLUMINENSE`

# QUANTIDADE DE ITERAÇÕES PARA SE TORNAR ESTACIONÁRIA
################################################################################

n_iter = list()
for (concess in nomes_concess_tratados) {
  n_iter[[concess]] = ndiffs(dados_por_concess[[concess]], alpha = 0.05)
}
n_iter

# ANALISANDO AS DIFERENÇAS
################################################################################

diff_dados_por_concess = list()
for (concess in nomes_concess_tratados) {
  diff_dados_por_concess[[concess]] = diff(dados_por_concess[[concess]])
}
diff_dados_por_concess

# PLOTAGEM DA DIFF
################################################################################

diff_times = c(2011:2020)
ggplot() +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`AUTOPISTA FERNÃO DIAS`[10], x = "Fernão Dias", color = "FERNÃO DIAS")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`AUTOPISTA FLUMINENSE`[10], x = "Fluminense", color = "FLUMINENSE")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`AUTOPISTA LITORAL SUL`[10], x = "Litoral Sul", color = "LITORAL SUL")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`AUTOPISTA PLANALTO SUL`[10], x = "Planalto Sul", color = "PLANALTO SUL")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`AUTOPISTA REGIS BITTENCOURT`[10], x = "Regis..", color = "REGIS BITTENCOURT")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$CONCER[10], x = "Concer", color = "CONCER")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$CRT[10], x = "Crt", color = "CRT")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$ECOSUL[10], x = "Ecosul", color = "ECOSUL")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`RODOVIA DO AÇO`[10], x = "Rod do Aço", color = "RODOVIA DO AÇO")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$TRANSBRASILIANA[10], x = "Transbra...", color = "TRANSBRASILIANA")) +
  geom_bar(stat = "identity", aes(y = diff_dados_por_concess$`VIA BAHIA`[10], x = "Via BA", color = "VIA BAHIA")) +
  xlab("Concessionárias") +
  ylab("Valor (reais)") +
  theme(axis.text.x = element_text(size = rel(0.7), angle = 90)) +
  scale_y_continuous(labels = scales::comma)

# FIT ARIMA
################################################################################
nomes_concess_escolhidas = c("CONCER", "CRT", "AUTOPISTA FLUMINENSE", "AUTOPISTA LITORAL SUL")

fit_arima = list()
for (concess in nomes_concess_escolhidas) {
  fit_arima[[concess]] = auto.arima(
    dados_por_concess[[concess]],
    stepwise = FALSE,
    approximation = FALSE,
    trace = TRUE,
    d = 1,
    D = 1
  )
  checkresiduals(fit_arima[[concess]])
}
fit_arima

cross_correlate_arima(
  arima = fit_arima$CONCER,
  corr_ts = pib_ts
)

pib_concess_unions = list()
for (concess in nomes_concess_escolhidas) {
  pib_concess_unions[[concess]] = ts.union(dados_por_concess[[concess]], pib_ts)
}
pib_concess_unions

plot.ts(pib_concess_unions$CONCER, main = "Dados de Concessionarias e PIB", xlab = "Tempo", ylab = "Valores", col = "blue", lwd = 4, plot.type = "multiple")
plot.ts(pib_concess_unions$CRT, main = "Dados de Concessionarias e PIB", xlab = "Tempo", ylab = "Valores", col = "blue", lwd = 4, plot.type = "multiple")
plot.ts(pib_concess_unions$`AUTOPISTA FLUMINENSE`, main = "Dados de Concessionarias e PIB", xlab = "Tempo", ylab = "Valores", col = "blue", lwd = 4, plot.type = "multiple")
plot.ts(pib_concess_unions$`AUTOPISTA LITORAL SUL`, main = "Dados de Concessionarias e PIB", xlab = "Tempo", ylab = "Valores", col = "blue", lwd = 4, plot.type = "multiple")

set.seed(999)
linear_reg = list()
for (concess in nomes_concess_escolhidas) {
  linear_reg[[concess]] = lm(
    pib_concess_unions[[concess]][, 1] ~ pib_concess_unions[[concess]][, 2]
  )
  checkresiduals(linear_reg[[concess]])
}

arima = list()
for (concess in nomes_concess_escolhidas) {
  arima[[concess]] = auto.arima(
    pib_concess_unions[[concess]][, 1],
    xreg = pib_concess_unions[[concess]][, 2]
  )
  checkresiduals(arima[[concess]])
}


pib_diff = diff(pib_ts)
concess_diff = diff_dados_por_concess$CONCER

Banco = cbind(pib_diff, concess_diff)

model_var = VAR(Banco, p = 2, type = "const")
model_var

model_var2 = VAR(Banco, type = "const", lag.max = 3, ic = "AIC")
model_var2

causality(model_var, cause = "concess_diff")$Granger

causality(model_var, cause = "pib_diff")$Granger


########################## TÉCNICAS DE MODELAGEM ###############################


# DF - Dickey-Fuller Aumentado
DF_tests = list()
for (concess in nomes_concess_tratados) {
  DF_tests[[concess]] = ur.df(dados_por_concess[[concess]], type = "none", lags = 1)
}
DF_tests
summary(DF_tests$CONCER)

# K_TEST DA DIFF
################################################################################

diff_k_test = list()
for (concess in nomes_concess_tratados) {
  diff_k_test[[concess]] = kpss.test(diff_dados_por_concess[[concess]], type = "tau")
}
summary(diff_k_test$CONCER)
