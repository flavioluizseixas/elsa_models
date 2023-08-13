library(caret)
library(dplyr)
library(DMwR)
library(nestedcv)
library(themis)
library(xgboost)

library(ggplot2)
library(patchwork)

rm(list = ls())

###################################
# ELSA Features
###################################
experiment_name <- "Exp3"
path_data   <- "Data/"
classname <- "dementia_w9"
n <- 123

load(paste0(path_data, "Data_processed", ".RData"))

###################################
# Missing data pre-treatment
###################################
skim_select <- skim_select %>% filter(complete_rate > 0.5)
data_select <- data_select %>% select(all_of(skim_select$skim_variable))

skim_select_withCTF <- skim_select_withCTF %>% filter(complete_rate > 0.5)
data_select_withCTF <- data_select_withCTF %>% select(all_of(skim_select_withCTF$skim_variable))

for (exp in 1:3) {
  if (exp == 1) {
    data <- data_select
    data_name <- "withOriginalFeatures"
  }
  if (exp == 2) {
    data <- data_select_withCTF
    data_name <- "withCTF"
    
  }
  if (exp == 3) {
    data <- data_hcap
    data_name <- "withHCAP"
  }
  
  ###################################
  # Indice da classe
  ###################################
  attY_idx <- which(names(data) == classname)
  attX_idx <- setdiff(seq_along(data), attY_idx)
  #attX_idx <- setdiff(1:length(data), attY_idx)
  
  ###################################
  # Split training VS test set
  ###################################
  set.seed(n)
  trainIndex <- createDataPartition(data[,attY_idx], p = .7, list = FALSE)
  training   <- data[trainIndex, ]
  test       <- data[-trainIndex, ] 
  
  print("Dealing with missing values on training set...")
  ###################################
  # Missing values - Training set
  ###################################
  X      <- training[,attX_idx]
  target <- training[,attY_idx]
  training.knnimputation <- knnImputation(X, k = 3, meth = 'median', distData = NULL)
  training.knnimputation <- cbind(training.knnimputation, target)
  names(training.knnimputation)[names(training.knnimputation) == "target"] <- classname
  attY_idx <- which(names(training.knnimputation) == classname)
  attX_idx <- setdiff(seq_along(training.knnimputation), attY_idx)
  #attX_idx <- setdiff(1:length(training.knnimputation), attY_idx)
  
  print("Creating inner and outer folds...")
  ###################################
  # Train Model - Nested CV
  ###################################
  attY_idx <- which(names(training.knnimputation) == classname)
  #attX_idx <- setdiff(1:length(training.knnimputation), attY_idx)
  attX_idx <- setdiff(seq_along(training.knnimputation), attY_idx)
  X      <- training.knnimputation[,attX_idx]
  target <- training.knnimputation[,attY_idx]

  out_folds <- caret::createFolds(target, k = 5)

  in_folds <- lapply(out_folds, function(i) {

    training.knnimputation.outer <- target[-i]
    caret::createFolds(training.knnimputation.outer, k = 10) 
  
  })

  print("Training xgb model...")
  model.xgb <- nestcv.train(target, X, method = "xgbTree",
                            cv.cores = 8,
                            metric = "logLoss",
                            outer_folds = out_folds,
                            tuneLength = 5,
                            balance = "smote",
                            verboseIter = TRUE)

  print("Training rf model...")
  model.rf <- nestcv.train(target, X, method = "rf",
                           cv.cores = 8,
                           metric = "logLoss",
                           outer_folds = out_folds,
                           tuneLength = 5,
                           balance = "smote",
                           verboseIter = TRUE)

  ###################################
  # Predictions
  ###################################
  confusionMatrix.rf.lst  <- list()
  confusionMatrix.xgb.lst <- list()
  for (i in 1:5) {
    
    fold_idx  <- model.xgb$outer_folds[[i]]
    test.fold <- test[fold_idx,]
    
    attY_idx <- which(names(test.fold) == classname)
    attX_idx <- setdiff(seq_along(test.fold), attY_idx)
    #attX_idx <- setdiff(1:length(test.fold), attY_idx)
    X      <- test.fold[,attX_idx]
    target <- test.fold[,attY_idx]
    
    print(paste0("Dealing with missing values on test set fold ", i, "..."))
    test.fold.knnimputation <- knnImputation(X, k = 3, meth = 'median', distData = NULL)
    test.fold.knnimputation <- cbind(test.fold.knnimputation, target)
    names(test.fold.knnimputation)[names(test.fold.knnimputation) == "target"] <- classname

    predicted.xgb <- predict(model.xgb, newdata = test.fold.knnimputation)
    predicted.rf  <- predict(model.rf,  newdata = test.fold.knnimputation)
    
    observed  <- test.fold.knnimputation[, classname]

    confusionMatrix.xgb.lst <- c(confusionMatrix.xgb.lst, list(confusionMatrix(data = predicted.xgb, reference = observed, positive = 'yes')))
    confusionMatrix.rf.lst <-  c(confusionMatrix.rf.lst,  list(confusionMatrix(data = predicted.rf,  reference = observed, positive = 'yes')))
  }


  ###################################
  # Visualization
  ###################################
  metrics <- c("Sensitivity", "Specificity", "Precision",
               "NegPredValue", "F1", "Accuracy",
               "Kappa", "AccuracyPValue")
  models <- c("xgbTree", "rf")

  m <- data.frame()
  for (i in 1:5) {
    for(j in 1:2) {
      if(j == 1) confusionMatrix <- confusionMatrix.xgb.lst[[i]]
      if(j == 2) confusionMatrix <- confusionMatrix.rf.lst[[i]]
  
      mi <- data.frame(Fold = i,
                       Model = models[j],
                       Sensitivity =  confusionMatrix$byClass[["Sensitivity"]],
                       Specificity =  confusionMatrix$byClass[["Specificity"]],
                       Precision =    confusionMatrix$byClass[["Precision"]],
                       NegPredValue = confusionMatrix$byClass[["Neg Pred Value"]],
                       F1 =           confusionMatrix$byClass[["F1"]],
                       Accuracy =     confusionMatrix$overall[["Accuracy"]],
                       Kappa =        confusionMatrix$overall[["Kappa"]],
                       AccuracyPValue = confusionMatrix$overall[["AccuracyPValue"]])
      
      if (nrow(m) > 0) { 
        m <- rbind(m, mi)
      }
      else {
        m <- mi
      }
    }
  }

  gl <- list()
  for (i in 1:8) {
    mi <- m %>% select("Fold", "Model", metrics[i])
    names(mi)[3] <- "values"
    g = ggplot(data = mi, aes(x = Fold, y = values, colour = Model, group = Model)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(y = metrics[i]) +
      ggtitle(metrics[i]) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    gl[[i]] = g
  }
  
  filename <- paste0(path_data, experiment_name, "_", data_name)

  my_plot <- wrap_plots(gl[[1]], gl[[2]], gl[[3]], gl[[4]], gl[[5]], gl[[6]], gl[[7]], gl[[8]], nrow=2, ncol=4)
  ggsave(paste0(filename, ".png"), my_plot, width = 12, height = 5, dpi = 150)

  save(
    data_select,
    data_select_withCTF,
    training.knnimputation,
    out_folds,
    in_folds,
    model.rf,
    model.xgb,
    m,
    g,
    my_plot,
    classname,
    file = paste0(filename, ".RData")
  )
}

#importance.xgb      <- varImp(model.xgb, scale = FALSE)$importance
#importance.rf       <- varImp(model.rf,  scale = FALSE)$importance

#save(training, test, training.smote, classname, file = paste0(path_data, "Models-Exp4", ".RData"))
