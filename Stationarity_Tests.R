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
# --- STATIONARITY TESTS rity test ------------------------------------------------------------------------------------------ #
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
    p_kpss <- suppressWarnings(kpss.test(x=ts(data))$p.value)
    
    cat("\n Pelo teste ADF a série temporal", state, "|", etanol, "é",
        if(p_adf < 0.05){
          "estacionária"
        }else{
          "NÃO ESTACIONÁRIA"
        },
        "com p-valor=", p_adf)
    
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
