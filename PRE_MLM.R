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
# a) General Params 
meses_teste = 12
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
# --- TRAINING AND TESTING SCENARIOS ---------------------------------------------------------------------------------------- #
###############################################################################################################################

# --------------------------------------------------------------------------------------------------------------------------- #
# 1) STATE:SP | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 2) STATE:SP | PRODUCT: ANHYDROUS ETHANOL
F_PRE_MLM_RO(state = "SP", product = "anhydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM)

# --------------------------------------------------------------------------------------------------------------------------- #
# 3) STATE:GO | PRODUCT: HYDROUS ETHANOL
F_PRE_MLM_RO(state = "GO", product = "hydrous", AnoTesteInicial=AnoTesteInicial, PRE_MLM=PRE_MLM)


###############################################################################################################################
# --- INTEGRATE SCENARIO RESULTS INTO A .CSV FILE---------------------------------------------------------------------------- #
###############################################################################################################################
integrateAndSaveResults("results")


# CORREÇÃO PARA OS DATASETS DE RESULTADOS DE SP-HIDRATADO E SP-ANIDRO

#results_SP_anhydrous[["Ordem"]] <- NA
#results_SP_hydrous[["Ordem"]] <- NA

#colnames <- colnames(results_SP_anhydrous)
#nova_ordem <- c("Estado", "Produto", "Ano_Teste", "Modelo", "preprocess", "R2_Treino", "R2_Teste", "Ordem",
#                "best_sw", "input_size", "nhid", "actfun", "kernel", "epsilon", "cost", "size", "decay",
#                "maxit", "epochs" )

#results_SP_anhydrous <- results_SP_anhydrous[, nova_ordem]
#results_SP_hydrous <- results_SP_hydrous[, nova_ordem]

#saveRDS(results_SP_anhydrous, "results/results_SP_anhydrous.RDS")
#saveRDS(results_SP_hydrous, "results/results_SP_hydrous.RDS")
