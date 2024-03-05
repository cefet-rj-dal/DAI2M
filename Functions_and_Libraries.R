# 1) Bibliotecas

library("daltoolbox")
library("forecast")
library("nnet")
library("elmNNRcpp")
library("e1071")
library(stats)
library(ggplot2)
library(zoo)
library(corrplot)
library(reticulate)
library(lubridate)


# 2) Funções

F_ARIMA <- function(df, estado, etanol, meses_teste, titulo, seed=1, remove_anos_finais=0){
  set.seed(seed)
  TS_ORIGINAL <- df
  TS_ORIGINAL <- TS_ORIGINAL[TS_ORIGINAL$Estado_Sigla == estado, ]
  TS_ORIGINAL <- TS_ORIGINAL[1:(nrow(TS_ORIGINAL) - (12 * remove_anos_finais)),]
  
  if(etanol == "hidratado"){
    x <- TS_ORIGINAL$PROD_ETANOL_HIDRATADO
  }else {
    x <- TS_ORIGINAL$PROD_ETANOL_ANIDRO
  }
  
  #sliding windows
  sw_size <- 0 # Para ARIMA, sliding window precisa ser = 0
  ts <- ts_data(x, sw_size)
  
  #Segregação dos dados em treino e teste
  test_size <- meses_teste
  samp <- ts_sample(ts, test_size)
  
  #Treinamento do modelo ARIMA
  model <- ts_arima()     # Cria um objeto do tipo "modelo de regressão ARIMA"
  io_train <- ts_projection(samp$train)  # Cria um objeto do tipo do lista com a base de treino da TS (série temporal)
  model <- fit(model, x=io_train$input, y=io_train$output)  # Ajusta o modelo de TS estimando os componentes subjacentes
  # de tendência e sazonalidade
  #Evaluation of adjustment
  adjust <- predict(model, io_train$input)
  ev_adjust <- evaluate(model, io_train$output, adjust)    # Avalia as predições realizadas comparando as mesmas com a
  # base de treino
  
  #Prediction of test
  steps_ahead <- meses_teste  # Define a quantidade de predições a serem realizadas
  io_test <- ts_projection(samp$test)  # Cria um objeto do tipo do lista com a base de teste da TS
  # Realiza a predição com base no modelo previamente treinado
  prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
  prediction <- as.vector(prediction)
  
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  
  #Evaluation of test data
  ev_test <- evaluate(model, output, prediction)
  
  #Plot results
  yvalues <- c(io_train$output, io_test$output)
  Data <- as.Date(TS_ORIGINAL$Data)
  Data <- tail(Data, n=length(yvalues))
  grafico <- plot_ts_pred(x = Data, y=yvalues, yadj=adjust, ypre=prediction, color_adjust = "blue", color_prediction = "red") +
    theme(text = element_text(size=18)) +
    labs(title = titulo)
  plot(grafico)
  
  #Cálculo do R2
  R2_Treino <- 1 - sum((io_train$output - adjust)^2) / sum((io_train$output - mean(io_train$output))^2)
  print(paste("train_R2=", R2_Treino))
  
  df  <- data.frame(real = as.vector(io_test$output), previsto = prediction)
  df$ape <- abs((df$real - df$previsto)/df$real)
  
  R2_Teste <- 1 - (sum(abs(df$real-df$previsto)^2) / sum((df$real - mean(df$real))^2))
  print(paste("test_R2 =", R2_Teste))
  
  #Output
  saida <- data.frame(Estado = estado, Produto = etanol, Ano_Teste = max(year(Data)),Modelo = "ARIMA", preprocess = NA,
                      R2_Treino= R2_Treino, R2_Teste = R2_Teste, 
                      Ordem = paste0("ARIMA(", model$p, "," , model$d , "," , model$q , ")"), best_sw = NA,
                      input_size = NA,
                      nhid = NA,
                      actfun = NA,
                      kernel = NA,
                      epsilon = NA,
                      cost = NA,
                      size = NA,
                      decay = NA,
                      maxit = NA,
                      epochs = NA)
  
  return(saida)
}


F_TSReg <- function(df, estado, etanol, meses_teste, sw_par, input_size, base_model, ranges, titulo, seed=1, remove_anos_finais=0){
  set.seed(seed)
  # 1) Registra data e hora do início do treinamentyo
  print(paste("Início do treinamento:", format(Sys.time(), format = "%B %d, %Y %H:%M:%S")))
  
  # 2) Carrega os dados da série temporal e associa os mesmos à variável x
  TS_ORIGINAL <- df
  TS_ORIGINAL <- TS_ORIGINAL[TS_ORIGINAL$Estado_Sigla == estado, ]
  TS_ORIGINAL <- TS_ORIGINAL[1:(nrow(TS_ORIGINAL) - (12 * remove_anos_finais)),]
  
  if(etanol == "hidratado"){
    x <- TS_ORIGINAL$PROD_ETANOL_HIDRATADO
  }else if(etanol=="anidro"){
    x <- TS_ORIGINAL$PROD_ETANOL_ANIDRO
  }else{print("PRODUTO NÃO ESPECIFICADO CORRETAMENTE!")}
  
  # 3)Ajuste do modelo, incluíndo a otimização do hiperparâmetro sliding window
  best_R2  <-  0
  for (sw_size in sw_par){
    ts <- ts_data(x, sw_size)
    samp <- ts_sample(ts, meses_teste)
    tune <- ts_tune(input_size=input_size, base_model = base_model)
    io_train <- ts_projection(samp$train)
    ranges <- ranges
    model <- fit(tune, x=io_train$input, y=io_train$output, ranges)
    adjust <- predict(model, io_train$input)
    ev_adjust <- evaluate(model, io_train$output, adjust)
    R2_Treino <- 1 - sum((as.vector(io_train$output) - adjust)^2) / sum((as.vector(io_train$output) - mean(as.vector(io_train$output)))^2)
    print(paste("R2 no treino", "para sw_size =", sw_size, "=", R2_Treino))
    if (best_R2 == 0) {
      best_R2 <- R2_Treino
      best_sw = sw_size
      best_model = model
    } else {
      if (R2_Treino > best_R2) {
        best_R2 <- R2_Treino
        best_sw = sw_size
        best_model = model
      }
    }
  }
  print(paste("best_sw =", best_sw))
  model  <-  best_model
  
  # 4) Realização e avaliação das predições com o modelo ajustado
  adjust <- predict(model, io_train$input)
  ev_adjust <- evaluate(model, io_train$output, adjust)
  steps_ahead <- meses_teste
  io_test <- ts_projection(samp$test)
  prediction <- predict(model, x=io_test$input[1,], steps_ahead = steps_ahead)
  prediction <- as.vector(prediction)
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  
  ev_test <- evaluate(model, output, prediction)
  
  # 6) Exibição do gráfico com os resultados
  yvalues <- c(io_train$output, io_test$output)
  Data <- as.Date(TS_ORIGINAL$Data)
  Data <- tail(Data, n=length(yvalues))
  grafico <- plot_ts_pred(x = Data, y=yvalues, yadj=adjust, ypre=prediction, color_adjust = "blue", color_prediction = "red") +
    theme(text = element_text(size=18)) +
    labs(title = paste0(titulo, " - Etanol ", etanol, " - ", estado, " Teste em ", max(year(Data))))
  plot(grafico)
  
  #Cálculo do R2
  R2_Treino <- 1 - sum((as.vector(io_train$output) - adjust)^2) / sum((as.vector(io_train$output) - mean(as.vector(io_train$output)))^2)
  print(paste("R2_Treino=", R2_Treino))
  
  df  <- data.frame(real = as.vector(io_test$output), previsto = prediction)
  df$ape <- abs((df$real - df$previsto)/df$real)
  
  R2_Teste <- 1 - (sum(abs(df$real-df$previsto)^2) / sum((df$real - mean(df$real))^2))
  
  print(paste("R2_Teste =", R2_Teste))
  
  print(df)
  
  print(paste("Final do treinamento:", format(Sys.time(), format = "%B %d, %Y %H:%M:%S")))
  
  # 7) Retorno do modelo treinado e ajustado
  
  saida <- data.frame(Estado = estado, Produto = etanol, Ano_Teste = max(year(Data)), Modelo = titulo, preprocess = class(model$preprocess)[1],
                      R2_Treino= R2_Treino, R2_Teste = R2_Teste, 
                      best_sw = best_sw,
                      input_size = model$input_size, 
                      nhid = if(!is.null(model$nhid)){model$nhid} else {NA},
                      actfun = if(!is.null(model$actfun)){model$actfun} else {NA},
                      kernel = if(!is.null(model$kernel)){model$kernel} else {NA},
                      epsilon = if(!is.null(model$epsilon)){model$epsilon} else {NA},
                      cost = if(!is.null(model$cost)){model$cost} else {NA},
                      size = if(!is.null(model$size)){model$size} else {NA},
                      decay = if(!is.null(model$decay)){model$decay} else {NA},
                      maxit = if(!is.null(model$maxit)){model$maxit} else {NA},
                      epochs = if(!is.null(model$epochs)){model$epochs} else {NA})
  
  return(saida)
}

F_PRE_MLM <- function(Estado, TiposDeEtanol, AnoTesteInicial, PRE_MLM, resultado){
  for(Tipo in TiposDeEtanol){
    for(AnoTeste in (AnoTesteInicial-4):AnoTesteInicial){
      remove_anos_finais = max(year(dataset$Data)) - AnoTeste
      nome_modelo = 1
      for (modelo in PRE_MLM){
        TipoMLM = names(PRE_MLM)[nome_modelo]
        print("=================================================================================")
        print(paste0(TipoMLM, " - Etanol ", Tipo, " - ", Estado, " - Teste em ", AnoTeste))
        modelo_avaliado <- F_TSReg(df = dataset, estado=Estado, etanol=Tipo,
                                   meses_teste = meses_teste, sw_par=sw_par,
                                   input_size = input_size,
                                   base_model = modelo$base_model,
                                   ranges = modelo$ranges,
                                   titulo = TipoMLM,
                                   seed = 1,
                                   remove_anos_finais=remove_anos_finais)
        print(modelo_avaliado)
        resultado <- rbind(resultado, modelo_avaliado)
        nome_modelo = nome_modelo + 1
      }
    }
  }
  saida <- resultado
  return(saida)
}