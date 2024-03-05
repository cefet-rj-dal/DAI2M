###############################################################################################################################
# --- LOAD LIBRARIES AND FUNCTIONS ------------------------------------------------------------------------------------------ #
###############################################################################################################################
source("Functions_and_Libraries.R")



###############################################################################################################################
# --- LOAD DATA ------------------------------------------------------------------------------------------------------------- #
###############################################################################################################################
dataset <- read.csv2("input/Etanol_df.csv")



###############################################################################################################################
# --- TRAIN AND TEST PARAMETERS --------------------------------------------------------------------------------------------- #
###############################################################################################################################
# a) General
meses_teste = 12
remove_anos_finais = 0
AnoTesteInicial = (max(year(as.Date(dataset$Data))))
TiposDeEtanol <- c("hidratado", "anidro")

# b) PRE+MLM
sw_par=seq(9, 18, 3)
input_size = c(1:10) #input size options should be between 1 and sw_size-2



###############################################################################################################################
# --- ARIMA TRAIN AND AVALIATION -------------------------------------------------------------------------------------------- #
###############################################################################################################################
# a) ARIMA Results' dataset creation
resultadoARIMA <- data.frame()

Estados <- c("SP", "GO", "MG", "MT", "MS", "PR")

# b) Train and Test
for(Estado in Estados){
  for(Tipo in TiposDeEtanol){
    for(AnoTeste in (AnoTesteInicial-4):AnoTesteInicial){
      titulo  <- paste0("ARIMA - Etanol ", Tipo, " - ", Estado, " - Teste em ", AnoTeste)
      remove_anos_finais = max(year(dataset$Data)) - AnoTeste
      modelo <- F_ARIMA(df = dataset, estado=Estado, etanol=Tipo, meses_teste = meses_teste, titulo = titulo, 
                        seed=1, remove_anos_finais=remove_anos_finais)
      print(modelo)
      resultadoARIMA <- rbind(resultadoARIMA, modelo)
    }}}

# c) Saving ARIMA results dataset
saveRDS(resultadoARIMA, "results/resultadoARIMA.RDS")



################################################################################################################################
# --- PRE+MLM TRAIN AND AVALIATION ------------------------------------------------------------------------------------------- #
################################################################################################################################
# a) Geração da lista com as opções PRE+MLM a serem avaliadas
Autoregressivos_list <- list(
  modelos = list(
    an_lstm   = list(base_model = ts_lstm(ts_norm_an()), ranges = list(epochs=700)),
    an_elm    = list(base_model = ts_elm(ts_norm_an()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                            'relu', 'purelin'))),
    an_svm    = list(base_model = ts_svm(ts_norm_an()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                      epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
    an_mlp    = list(base_model = ts_mlp(ts_norm_an()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
    an_conv1d = list(base_model = ts_conv1d(ts_norm_an()), ranges = list(epochs=700)),
    diff_lstm   = list(base_model = ts_lstm(ts_norm_diff()), ranges = list(epochs=700)),
    diff_elm    = list(base_model = ts_elm(ts_norm_diff()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                                'relu', 'purelin'))),
    diff_svm    = list(base_model = ts_svm(ts_norm_diff()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                          epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
    diff_mlp    = list(base_model = ts_mlp(ts_norm_diff()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
    diff_conv1d = list(base_model = ts_conv1d(ts_norm_diff()), ranges = list(epochs=700)),
    gmm_lstm   = list(base_model = ts_lstm(ts_norm_gminmax()), ranges = list(epochs=700)),
    gmm_elm    = list(base_model = ts_elm(ts_norm_gminmax()), ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas',
                                                                                                  'relu', 'purelin'))),
    gmm_svm    = list(base_model = ts_svm(ts_norm_gminmax()), ranges = list(kernel=c("radial", "poly", "linear", "sigmoid"),
                                                                            epsilon=seq(0, 1, 1/20), cost=seq(1, 10, 1))),
    gmm_mlp    = list(base_model = ts_mlp(ts_norm_gminmax()), ranges = list(size = 1:10, decay = seq(0, 1, 1/20), maxit=700)),
    gmm_conv1d = list(base_model = ts_conv1d(ts_norm_gminmax()), ranges = list(epochs=700))
  )
)

# ---------------------------------------------------------------------------------------------------------------------------- #
# 1) ESTADO:SP - TREINO E AVALIAÇÃO DOS MODELOS PRE+MLM
# Recuperação dos resultados de etapas anteriores de treinamento / avaliação
resultado <- data.frame()
if(file.exists("results/resultado.RDS")){
  resultado <- readRDS("results/resultado.RDS")}

# Avaliação dos modelos PRE+MLM
resultado <- F_PRE_MLM(Estado="SP", TiposDeEtanol=TiposDeEtanol, AnoTesteInicial=AnoTesteInicial, 
                       PRE_MLM=Autoregressivos_list$modelos, resultado=resultado)
# Gravação dos resultadoS
saveRDS(resultado, "results/resultado.RDS")

# ---------------------------------------------------------------------------------------------------------------------------- #
# 2) ESTADO:GO - TREINO E AVALIAÇÃO DOS MODELOS PRE+MLM
# Recuperação dos resultados de etapas anteriores de treinamento / avaliação
resultado <- readRDS("results/resultado.RDS")

# Avaliação dos modelos PRE+MLM
resultado <- F_PRE_MLM(Estado="GO", TiposDeEtanol=TiposDeEtanol, AnoTesteInicial=AnoTesteInicial, 
                       PRE_MLM=Autoregressivos_list$modelos, resultado=resultado)
# Gravação dos resultadoS
saveRDS(resultado, "results/resultado.RDS")

# ---------------------------------------------------------------------------------------------------------------------------- #