###############################################################################################################################
# --- LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------------------------------ #
###############################################################################################################################
source("Functions_and_Libraries.R")


###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")


###############################################################################################################################
# --- DEFINING THE BEST WAVELET FILTER TO EACH SCENARIO --------------------------------------------------------------------- #
###############################################################################################################################

for(state in c("SP", "GO", "MG", "MT", "MS", "PR")){
  for(etanol in c("hydrous", "anhydrous")){
    par(mfrow = c(3, 2))
    for(filter in c("haar", "d4", "la8", "bl14", "c6")){
      data <- dataset
      data <- data[data$Estado_Sigla == state, ]
      # Selecting the Correct Type of Ethanol and assembling the "x" vector
      if(etanol == "hydrous"){
        data <- data$PROD_ETANOL_HIDRATADO
      }else if(etanol=="anhydrous"){
        data <- data$PROD_ETANOL_ANIDRO
      }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
      
      F_WAVELET(data, wavelet_filter=filter, scenario=paste0(state, " | ", etanol))
    }
  }
  par(mfrow = c(1, 1))
}
par(mfrow = c(1, 1))


###############################################################################################################################
# --- PARAMETERS ------------------------------------------------------------------------------------------------------------ #
###############################################################################################################################
# a) General parameters
remove_anos_finais = 0
AnoTesteInicial = (max(year(as.Date(dataset$Data))))
sw_par=seq(9, 18, 3)
input_size = c(1:10)

# b) Specific parameters
PRE_MLM = list(
  gmm_wavelet_lstm   = list(base_model = ts_lstm(ts_norm_gminmax()), ranges = list(epochs=700)),
  gmm_wavelet_elm    = list(base_model = ts_elm(ts_norm_gminmax()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 
                                                                                                'tribas', 'relu', 'purelin'))),
  gmm_wavelet_svm    = list(base_model = ts_svm(ts_norm_gminmax()), ranges = list(kernel=c("radial", "poly", "linear", 
                                                                                           "sigmoid"),
                                                                          epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  gmm_wavelet_mlp    = list(base_model = ts_mlp(ts_norm_gminmax()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), 
                                                                                  maxit=700)),
  gmm_wavelet_conv1d = list(base_model = ts_conv1d(ts_norm_gminmax()), ranges = list(epochs=700))
)


###############################################################################################################################
# --- TRAINING AND TESTING SCENARIOS ---------------------------------------------------------------------------------------- #
###############################################################################################################################

# --------------------------------------------------------------------------------------------------------------------------- #
# 1) STATE:SP | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 2) STATE:SP | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 3) STATE:GO | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "GO", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 4) STATE:GO | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "GO", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 5) STATE:MG | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MG", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 6) STATE:MG | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MG", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 7) STATE:MT | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MT", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 8) STATE:MT | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MT", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)


###############################################################################################################################
# --- INTEGRATE SCENARIO RESULTS INTO A .CSV FILE---------------------------------------------------------------------------- #
###############################################################################################################################
integrateAndSaveResults("results")



