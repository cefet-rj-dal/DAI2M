###############################################################################################################################
# --- LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------------------------------ #
###############################################################################################################################
source("Functions_and_Libraries.R")


###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("data/Etanol_df.csv")


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
  diff_wavelet_lstm   = list(base_model = ts_lstm(ts_norm_diff()), ranges = list(epochs=700)),
  diff_wavelet_elm    = list(base_model = ts_elm(ts_norm_diff()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 
                                                                                                      'tribas', 'relu', 'purelin'))),
  diff_wavelet_svm    = list(base_model = ts_svm(ts_norm_diff()), ranges = list(kernel=c("radial", "poly", "linear", 
                                                                                         "sigmoid"),
                                                                                epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  diff_wavelet_mlp    = list(base_model = ts_mlp(ts_norm_diff()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), 
                                                                                maxit=700)),
  diff_wavelet_conv1d = list(base_model = ts_conv1d(ts_norm_diff()), ranges = list(epochs=700)),
  gmm_wavelet_lstm   = list(base_model = ts_lstm(ts_norm_gminmax()), ranges = list(epochs=700)),
  gmm_wavelet_elm    = list(base_model = ts_elm(ts_norm_gminmax()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 
                                                                                                'tribas', 'relu', 'purelin'))),
  gmm_wavelet_svm    = list(base_model = ts_svm(ts_norm_gminmax()), ranges = list(kernel=c("radial", "poly", "linear", 
                                                                                           "sigmoid"),
                                                                          epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  gmm_wavelet_mlp    = list(base_model = ts_mlp(ts_norm_gminmax()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), 
                                                                                  maxit=700)),
  gmm_wavelet_conv1d = list(base_model = ts_conv1d(ts_norm_gminmax()), ranges = list(epochs=700)),
  an_wavelet_lstm   = list(base_model = ts_lstm(ts_norm_an()), ranges = list(epochs=700)),
  an_wavelet_elm    = list(base_model = ts_elm(ts_norm_an()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 
                                                                                                  'tribas', 'relu', 'purelin'))),
  an_wavelet_svm    = list(base_model = ts_svm(ts_norm_an()), ranges = list(kernel=c("radial", "poly", "linear", 
                                                                                     "sigmoid"),
                                                                            epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  an_wavelet_mlp    = list(base_model = ts_mlp(ts_norm_an()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), 
                                                                            maxit=700)),
  an_wavelet_conv1d = list(base_model = ts_conv1d(ts_norm_an()), ranges = list(epochs=700))
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


# --------------------------------------------------------------------------------------------------------------------------- #
# 9) STATE:MS | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "MS", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 10) STATE:MS | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "MS", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 11) STATE:PR | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "PR", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# 12) STATE:PR | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "PR", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM, 
             wavelet=TRUE)

###############################################################################################################################
# --- INTEGRATE SCENARIO RESULTS INTO A .CSV FILE---------------------------------------------------------------------------- #
###############################################################################################################################
integrateAndSaveResults("results")




###############################################################################################################################
# --- INDIVIDUAL ADJUST------------------------------------------------------------------------------------------------------ #
###############################################################################################################################
AnoTeste = 2019
remove_anos_finais = max(year(dataset$Data)) - AnoTeste
TipoMLM = names(PRE_MLM)[1]
product = "anhydrous"
state = "MT"
base_model = PRE_MLM$an_wavelet_lstm$base_model
ranges = PRE_MLM$an_wavelet_lstm$ranges

print("=================================================================================")
print(paste0(TipoMLM, " - Etanol ", product, " - ", state, " - Teste em ", AnoTeste))
modelo_avaliado <- F_TSReg(df = dataset, estado=state, etanol=product,
                           meses_teste = 12, sw_par=sw_par,
                           input_size = input_size,
                           base_model = base_model,
                           ranges = ranges,
                           titulo = TipoMLM,
                           seed = 1,
                           remove_anos_finais=remove_anos_finais,
                           wavelet=TRUE)
print(modelo_avaliado)


