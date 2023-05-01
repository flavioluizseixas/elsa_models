library(caret)
library(magrittr)
library(dplyr)
library(ROSE)

library(pROC)
library(ROCR)

library(ggplot2)

#setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/Dynamic-Models")

rm(list = ls())
#######################################################################################
performModels <- function(new_train, testing, atts_f, class_name) {

  ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE,
                       summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

  # Definir modelos a serem avaliados
  models <- c("ranger", "glm", "nnet", "rf")

  # Treinar os modelos e avaliar o desempenho
  results <- lapply(models, function(m) {
    train(atts_f,
          data = new_train,
          method = m,
          metric = "Precision",
          trControl = ctrl)
  })

  names(results) <- models

  for (i in 1:length(results)) {
    # Usa o modelo ajustado para prever as respostas do conjunto de dados de teste
    predictions <- predict(results[[i]], newdata = testing)
    results[[i]]$predictions <- predictions

    # Avalia a precisão das previsões usando a matriz de confusão
    confusionMatrix <- confusionMatrix(data = predictions, reference = testing[[class_name]], positive = "yes")
    results[[i]]$confusionMatrix <- confusionMatrix
  }

  return(results)
}
#######################################################################################

selectAttributes <- function(df, atts_f, models, class_name, n_attributes) {

  # calculate feature importance without models
  class_i <- grep(class_name, colnames(df))
  roc_imp <- filterVarImp(x = df[, -class_i], y = df[, class_i])
  roc_imp_df <- data.frame(cbind(variable = rownames(roc_imp), score = roc_imp[,1]))
  names(roc_imp_df)[1] <- "var"
  roc_imp_df$score <- as.double(roc_imp_df$score)
  roc_imp_df$norm <- (roc_imp_df$score - min(roc_imp_df$score)) /
    (max(roc_imp_df$score) - min(roc_imp_df$score))

  # calculate feature importance with models
  # considera-se o modelo de índice 2 aquele com melhor desempenho.
  var_imp <- varImp(models[[2]], scale = FALSE)
  var_imp_df <- as.data.frame(var_imp$importance)
  var_imp_df$variavel <- row.names(var_imp_df)
  names(var_imp_df)[1] <- "score"
  names(var_imp_df)[2] <- "var"
  var_imp_df <- var_imp_df[, c("var", "score")]
  var_imp_df$norm <- (var_imp_df$score - min(var_imp_df$score)) /
    (max(var_imp_df$score) - min(var_imp_df$score))

  # calculate feature importance using RFE
  rfeResults <- rfe(atts_f, data = df, sizes = c(1:8),
                    rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 5))
  rfe_df <- rfeResults$variables %>%
    group_by(var) %>%
    summarize(score = mean(Overall))
  rfe_df$norm <- (rfe_df$score - min(rfe_df$score)) /
    (max(rfe_df$score) - min(rfe_df$score))

  df_sum <- merge(roc_imp_df, var_imp_df, by = "var", all = TRUE)
  df_sum <- merge(df_sum, rfe_df, by = "var", all = TRUE)
  df_sum$total <- rowSums(df_sum[, c("norm.x", "norm.y", "norm")], na.rm = TRUE)
  df_sum <- df_sum[order(df_sum$total,decreasing = TRUE),]

  df_sum <- df_sum[1:n_attributes-1,]
  df_class <- df[[class_name]]
  df <- select(df, one_of(df_sum$var))
  atts_f <- as.formula(paste0(class_name, " ~ ", paste(names(df), collapse = " + ")))
  df[[class_name]] <- df_class

  return(list(df_sum=df_sum, atts_f=atts_f))
}
#######################################################################################

ext <- "Nurse"
filename <- "Elsa"
use_CTF <- TRUE
path <- "Data/"


for (ref in 4:7) {

  set.seed(123)
  w_ref <- paste0("w", ref)
  load(paste0(path, ref, "_", filename, "_", ext, ".RData"))
  df[[class_name]] <- ifelse(df[[class_name]] == 0, 2, 1)
  df[[class_name]] <- as.factor(df[[class_name]])
  df <- df[complete.cases(df[[class_name]]), ]
  levels(df[[class_name]]) <- c("yes", "no")

  trainIndex <- createDataPartition(df[[class_name]], p = .7, list = FALSE) #criar índice de treinamento
  training <- df[trainIndex, ] #conjunto de treinamento
  testing <- df[-trainIndex, ] #conjunto de teste

  my_names <- c("under", "over", "under_CTF", "over_CTF")
  dfs <- list()
  dfs <- c(dfs, list(ovun.sample(att_f, data = training, method = "under", seed = 123)$data))
  dfs <- c(dfs, list(ovun.sample(att_f, data = training, method = "over",  seed = 123)$data))
  dfs <- c(dfs, list(ovun.sample(att_f_CTF, data = training, method = "under", seed = 123)$data))
  dfs <- c(dfs, list(ovun.sample(att_f_CTF, data = training, method = "over",  seed = 123)$data))
  names(dfs) <- my_names

  atts_f <- list()
  atts_f <- c(atts_f, list(att_f))
  atts_f <- c(atts_f, list(att_f))
  atts_f <- c(atts_f, list(att_f_CTF))
  atts_f <- c(atts_f, list(att_f_CTF))
  names(atts_f) <- my_names

  rs <- list()
  for (i in 1:length(atts_f)) {
    r <- performModels(dfs[[i]], testing, atts_f[[i]], class_name)
    rs <- c(rs, list(r))
  }

  my_names_sel <- c("under_CTF_sel", "over_CTF_sel")
  selectAtt <- list()
  terms_formula <- terms(atts_f[[1]])
  num_attrs <- length(attr(terms_formula, "variables")) - 1
  for (i in 3:4) {
    r <- selectAttributes(dfs[[i]], atts_f[[i]], rs[[i]], class_name, num_attrs)
    selectAtt <- c(selectAtt, list(r$df_sum))
    atts_f <- c(atts_f, list(r$atts_f))
  }
  names(selectAtt) <- my_names_sel
  for (i in 5:6) {
    r <- performModels(dfs[[i-2]], testing, atts_f[[i]], class_name)
    rs <- c(rs, list(r))
  }

  my_names <- c(my_names, my_names_sel)
  names(rs) <- my_names
  names(atts_f) <- my_names

  save(path, rs, selectAtt, atts_f, file = paste0(ref, "_", filename, "_", ext, "_results.RData"))
}


