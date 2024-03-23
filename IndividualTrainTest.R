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
  # b1) an_mlm
  an_lstm   = list(base_model = ts_lstm(ts_norm_an()), ranges = list(epochs=700)),
  an_elm    = list(base_model = ts_elm(ts_norm_an()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                          'relu', 'purelin'))),
  an_svm    = list(base_model = ts_svm(ts_norm_an()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                    epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  an_mlp    = list(base_model = ts_mlp(ts_norm_an()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  an_conv1d = list(base_model = ts_conv1d(ts_norm_an()), ranges = list(epochs=700)),
  
  # b2) diff_mlm
  diff_lstm   = list(base_model = ts_lstm(ts_norm_diff()), ranges = list(epochs=700)),
  diff_elm    = list(base_model = ts_elm(ts_norm_diff()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                              'relu', 'purelin'))),
  diff_svm    = list(base_model = ts_svm(ts_norm_diff()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                        epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  diff_mlp    = list(base_model = ts_mlp(ts_norm_diff()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  diff_conv1d = list(base_model = ts_conv1d(ts_norm_diff()), ranges = list(epochs=700)),
  # b3) gmm_mlm
  gmm_lstm   = list(base_model = ts_lstm(ts_norm_gminmax()), ranges = list(epochs=700)),
  gmm_elm    = list(base_model = ts_elm(ts_norm_gminmax()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                                'relu', 'purelin'))),
  gmm_svm    = list(base_model = ts_svm(ts_norm_gminmax()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                          epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
  gmm_mlp    = list(base_model = ts_mlp(ts_norm_gminmax()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
  gmm_conv1d = list(base_model = ts_conv1d(ts_norm_gminmax()), ranges = list(epochs=700))
)


###############################################################################################################################
# --- ADJUST ---------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################

modelo_avaliado <- F_TSReg(df = dataset, estado="MG", etanol="hydrous",
                           meses_teste = 12, sw_par=sw_par,
                           input_size = input_size,
                           base_model = PRE_MLM$gmm_lstm$base_model,
                           ranges = PRE_MLM$gmm_lstm$ranges,
                           titulo = "gmm_lstm",
                           seed = 1,
                           remove_anos_finais=remove_anos_finais)
print(modelo_avaliado)

# Corrigindo o valor de R2_Treino
results_MG_hydrous[71, 6] <- 0.927608428049699
# Corrigindo o valor de R2_Teste
results_MG_hydrous[71, 7] <- 0.6129238726444
# Salvando o dataframe corrigido em arquivo .RDS
saveRDS(results_MG_hydrous, "results/results_MG_hydrous.RDS")



###############################################################################################################################
# --- DEFINING THE BEST WAVELET FILTER TO EACH SCENARIO --------------------------------------------------------------------- #
###############################################################################################################################
for(state in c("SP", "GO", "MG", "MT", "MS", "PR")){
  for(etanol in c("hydrous", "anhydrous")){
    #par(mfrow = c(3, 2))
    #for(filter in c("haar", "d4", "la8", "bl14", "c6")){
    data <- dataset
    data <- data[data$Estado_Sigla == state, ]
    # Selecting the Correct Type of Ethanol and assembling the "x" vector
    if(etanol == "hydrous"){
      data <- data$PROD_ETANOL_HIDRATADO
    }else if(etanol=="anhydrous"){
      data <- data$PROD_ETANOL_ANIDRO
    }else{print("PRODUCT NOT SPECIFIED CORRECTLY")}
    
    F_WAVELET(data, scenario=paste0(state, " | ", etanol))
    #}
  }
  par(mfrow = c(1, 1))
}
par(mfrow = c(1, 1))
