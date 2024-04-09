###############################################################################################################################
# --- LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------------------------------ #
###############################################################################################################################
source("Functions_and_Libraries.R")
library(tseries)
library(urca)

###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")

###############################################################################################################################
# --- TESTS ----------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
bp.test <- function(serie) {
  library(lmtest)
  data <- data.frame(x = 1:length(serie), y = serie)
  fit <- lm(y ~ x, data = data)
  return(bptest(fit))
}

# TESTES COM SÉRIES SINTÉTICAS

# Gerar série temporal estacionária
serie_estacionaria <- rnorm(100) # 100 observações de uma distribuição normal

# Gerar série temporal não estacionária (caminhada aleatória)
serie_nao_estacionaria <- cumsum(rnorm(100)) # Soma acumulada aumenta a variância ao longo do tempo

# Gerar série temporal heterocedastica
n <- 100 # Número de observações
X <- 1:n # Variável independente
serie_heterocedastica <- 2 * X + rnorm(n, sd = X*0.1) # Relação linear com heterocedasticidade

# Plotar as séries temporais
par(mfrow = c(3, 1)) # Configura o layout do gráfico para 2 linhas, 1 coluna

plot(serie_estacionaria, type = 'l', main = "Série Temporal Estacionária", ylab = "Valor", xlab = "Tempo")
plot(serie_nao_estacionaria, type = 'l', main = "Série Temporal Não Estacionária", ylab = "Valor", xlab = "Tempo")
plot(serie_heterocedastica, type = 'l', main = "Série Temporal Heterocedastica", ylab = "Valor", xlab = "Tempo")

# Aplicação dos testes
ts <- ts(serie_heterocedastica)
adf_pvalue <- adf.test(ts)$p.value
pp_pvalue  <- PP.test(ts)$p.value
bp_pvalue  <- bp.test(ts)$p.value
cat("|adf_pvalue:", adf_pvalue, if(adf_pvalue < 0.05){"(estacionária)"} else{"(NÃO ESTACIONÁRIA)"},
    "|pp_pvalue :", pp_pvalue,  if(pp_pvalue  < 0.05){"(estacionária)"} else{"(NÃO ESTACIONÁRIA)"},
    "|bp_pvalue :", bp_pvalue,  if(bp_pvalue  < 0.05){"(HETEROCEDASTICA)"} else{"(homocedasticA)"})


# TESTES COM SÉRIES DE PRODUÇÃO DE ETANOL

states <- c("SP", "GO", "MG", "MT", "MS", "PR")

for(state in states){
  for(etanol in c("hydrous", "anhydrous")){
    scenario <- paste0(state, "-", etanol)
    data <- dataset[dataset$Estado_Sigla == state, ]
    if(etanol == "hydrous"){
      data <- data$PROD_ETANOL_HIDRATADO
    }else if(etanol=="anhydrous"){
      data <- data$PROD_ETANOL_ANIDRO
    }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
    ts <- ts(data)
    adf_pvalue <- adf.test(ts)$p.value
    pp_pvalue  <- PP.test(ts)$p.value
    bp_pvalue  <- bp.test(ts)$p.value
    cat("\nScenario", scenario, 
        "|adf_pvalue:", adf_pvalue, if(adf_pvalue < 0.05){"(estacionária)"} else{"(NÃO ESTACIONÁRIA)"},
        "|pp_pvalue :", pp_pvalue,  if(pp_pvalue  < 0.05){"(estacionária)"} else{"(NÃO ESTACIONÁRIA)"},
        "|bp_pvalue :", bp_pvalue,  if(bp_pvalue  < 0.05){"(HETEROCEDASTICA)"} else{"(homocedasticA)"})
  }
  cat("\n")
}











###############################################################################################################################
# --- STATIONARITY TESTS KPSS ----------------------------------------------------------------------------------------------- #
###############################################################################################################################
states <- c("SP", "GO", "MG", "MT", "MS", "PR")
for(state in states){
  for(etanol in c("hydrous", "anhydrous")){
    scenario <- paste0(state, "-", etanol)
    data <- dataset[dataset$Estado_Sigla == state, ]
    if(etanol == "hydrous"){
      data <- data$PROD_ETANOL_HIDRATADO
    }else if(etanol=="anhydrous"){
      data <- data$PROD_ETANOL_ANIDRO
    }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
    
    p_adf <- suppressWarnings(adf.test(x=ts(data), alternative="stationary")$p.value)
    p_kpss <- suppressWarnings(kpss.test(x=ts(data), null = "Level")$p.value)
    
    cat("\n Pelo teste KPSS a série temporal", state, "|", etanol, "é",
        if(p_kpss > 0.05){
          "estacionária"
        }else{
          "NÃO ESTACIONÁRIA"
        },
        "com p-valor=", p_kpss)
    
    cat("\n")
  }
}  



