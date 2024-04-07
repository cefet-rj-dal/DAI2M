IntegratedResults <- read_csv("results/IntegratedResults.csv")

###############################################################################################################
# ANÁLISE SEM O USO DO FILTRO WAVELET
###############################################################################################################
Results_No_Wavelet <- IntegratedResults[!grepl(pattern="wavelet",  
                                               IntegratedResults$Modelo, 
                                               ignore.case = TRUE), ]


# Calcular a média de 'coluna_valor' agrupada por 'coluna_texto'
medias <- aggregate(R2_Teste ~ Modelo, data = Results_No_Wavelet, FUN = mean)
medias <- medias[order(medias$R2_Teste, decreasing = TRUE), ]

# Exibir os resultados
print(medias)

for(i in 2:nrow(medias)){
  TestModels <- subset(Results_No_Wavelet, Modelo %in% c(medias$Modelo[1], medias$Modelo[i])) 
  
  # Visualizar a distribuição dos R2 por modelo
  grafico <- ggplot(TestModels, aes(x = Modelo, y = R2_Teste, fill = Modelo)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Distribuição de R2 por Modelo", y = "R2", x = "Modelo")
  plot(grafico)
  
  # Teste t para amostras independentes
  t_test_result <- t.test(R2_Teste ~ Modelo, data = TestModels)
  #print(t_test_result)
  
  # Teste de Wilcoxon para amostras independentes (teste não paramétrico)
  wilcox_test_result <- wilcox.test(R2_Teste ~ Modelo, data = TestModels, paired = TRUE)
  #print(wilcox_test_result)
  
  #if(t_test_result$p.value < 0.05){
  #  cat("\n O teste T indica que     ", medias$Modelo[1], "é diferente de", medias$Modelo[i])
  #} else{
  #  cat("\n O teste T indica que     ", medias$Modelo[1], "NÂO é diferente de", medias$Modelo[i])
  #}
  
  if(wilcox_test_result$p.value < 0.05){
    cat("\n O teste Wilcox indica que", medias$Modelo[1], "é diferente de", medias$Modelo[i])
  } else{
    cat("\n O teste Wilcox indica que", medias$Modelo[1], "NÂO é diferente de", medias$Modelo[i])
  }
}




###############################################################################################################
# ANÁLISE COM O USO DO FILTRO WAVELET
###############################################################################################################
Results_Wavelet <- IntegratedResults

# Calcular a média de 'coluna_valor' agrupada por 'coluna_texto'
medias <- aggregate(R2_Teste ~ Modelo, data = Results_Wavelet, FUN = mean)
medias <- medias[order(medias$R2_Teste, decreasing = TRUE), ]

# Exibir os resultados
print(medias)

for(i in 2:nrow(medias)){
  TestModels <- subset(Results_Wavelet, Modelo %in% c(medias$Modelo[1], medias$Modelo[i])) 
  
  # Visualizar a distribuição dos R2 por modelo
  grafico <- ggplot(TestModels, aes(x = Modelo, y = R2_Teste, fill = Modelo)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Distribuição de R2 por Modelo", y = "R2", x = "Modelo")
  plot(grafico)
  
  # Teste t para amostras independentes
  t_test_result <- t.test(R2_Teste ~ Modelo, data = TestModels)
  #print(t_test_result)
  
  # Teste de Wilcoxon para amostras independentes (teste não paramétrico)
  wilcox_test_result <- wilcox.test(R2_Teste ~ Modelo, data = TestModels, paired = TRUE, exact = FALSE)
  #print(wilcox_test_result)
  
  #if(t_test_result$p.value < 0.05){
  #  cat("\n O teste T indica que     ", medias$Modelo[1], "é diferente de", medias$Modelo[i])
  #} else{
  #  cat("\n O teste T indica que     ", medias$Modelo[1], "NÂO é diferente de", medias$Modelo[i])
  #}
  
  if(wilcox_test_result$p.value < 0.05){
    cat("\n O teste Wilcox indica que", medias$Modelo[1], "é diferente de", medias$Modelo[i])
  } else{
    cat("\n O teste Wilcox indica que", medias$Modelo[1], "NÂO é diferente de", medias$Modelo[i])
  }
}

