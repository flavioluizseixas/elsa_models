rm(list = ls())

library(skimr)

path_savedata <- "Data/"
load(paste0(path_savedata, "Data.RData"))

a <- data.frame()
for(i in 9:9) {
  classname <- paste0("dementia_w", i)

  atts <- c(paste0("hedbdde_w", i),
            paste0("hedibde_w", i),
            paste0("hedbsde_w", i))

  teste_0 <- 0
  teste_1 <- 0
  for (att in atts) {
    print(paste0(classname, " : ", att))

    criterio_1 <- as.numeric(data[[att]] == 1)
    if (length(criterio_1) > 0) {
      if (length(teste_1) > 0) {
        teste_1 <- teste_1 + criterio_1
      }
      else {
        teste_1 <- criterio_1
      }
    }
    criterio_0 <- as.numeric(data[[att]] == 0)
    if (length(criterio_0) > 0) {
      if (length(teste_0) > 0) {
        teste_0 <- teste_0 + criterio_0
      }
      else {
        teste_0 <- criterio_0
      }
    }
  }
  
  teste_final <- ifelse(teste_1 >= 1, 1, ifelse(teste_0 >= 1, 0, NA))
  print(table(teste_final))

  b <- data.frame(coluna = teste_final)
  names(b)[1] <- classname
  if (classname %in% names(data)) {
    data[,classname] <- teste_final
  }
  else {
    data <- cbind(data, b)
  } 
}

print("skimming...")
skimmed <- skim(data)
save(data, skimmed, file = paste0(path_savedata, "Data.RData"))
