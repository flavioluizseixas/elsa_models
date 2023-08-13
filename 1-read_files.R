library(haven)
library(readxl)

library(skimr)

rm(list = ls())

###################################
# ELSA Features
###################################
file_xls    <- "Atts-ELSA-HCAP-G2"
path_config     <- "Config/"
path_savedata   <- "Data/"
ext_xls  <-    ".xlsx"
ext_data <-    ".dta"
data_xls <- read_excel(paste0(path_config, file_xls, ext_xls), sheet="Harmonized ELSA G.2 (2002-2019)")
data     <- data.frame()

###################################
# Extract feature
###################################
extract_feature <- function(a, b) {
  if(!grepl(" ", a)) {
    feature <- a
  }
  else {
    espacos <- gregexpr("\\s", a)[[1]]
    if(!is.na(as.numeric(substr(a, 1, 1)))) { 
      feature <- substr(a, espacos[1]+1, espacos[2]-1)
      locate_number <- regexpr("\\d+", feature)[[1]]
      if(locate_number > 0) {
        number <- substr(feature, locate_number[1], locate_number)
        feature <- sub(number, b, feature)
      }
      feature <- tolower(feature)
    }
    else {
      feature <- substr(a, 1, espacos[1]-1)
    }
  }
  return(feature)
}


###################################
# Transfer data
###################################
transfer_data <- function (df, data_xls, ext = 0) {
  data <- data.frame()
  df_colnames <- colnames(df)
  for(j in 2:ncol(data_xls)) {
    for(i in 1:nrow(data_xls)) {
      if(!is.na(data_xls[[i,j]])) {
        feature <- extract_feature(data_xls[[i,1]], data_xls[[i,j]])
        feature <- tolower(feature)
        k <- grep(feature, df_colnames, ignore.case = TRUE)
        if (length(k) == 0) {
          print(paste("not found:", feature, file_data))
        }
        else {
          print(paste("add:", feature, file_data))
          b <- data.frame(as.numeric(df[[k[1]]]))
          names(b) <- feature
          if(ext == 1) {
            if(feature != "idauniq") {
              names(b) <- paste0(feature, "_w", data_xls[[i,j]])
            }
          }
          else {
            if(ext == 2) {
              if(feature != "idauniq") {
                names(b) <- paste0(feature, "_", data_xls[[i,j]])
              }
            }
          }
          if(ncol(data) > 0) {
            data <- cbind(data, b)
          }
          else {
            data <- b
          }
        }
      }
    }
  }
  return(data)
}

###################################
# Harmonized ELSA
###################################
for (i in 1:nrow(data_xls)) {
  a <- data_xls[[i,1]]
  if(!is.na(a)) {
    if(substr(a, 1, 4) == "path") {
      path_data <- sub("path ", "", a)
    }
    
    if(substr(a, 1, 8) == "filename") {
      file_data <- sub("filename ", "", a)
      df_data <- read_stata(paste0(path_data, file_data, ext_data))
      data_seg <- data_xls[,1:10]
      data_tmp <- transfer_data(df_data, data_seg)
      if (ncol(data) > 0) {
        data <- merge(data, data_tmp, by = "idauniq", all = TRUE)
      }
      else {
        data <- data_tmp
      }
    }
  }
}

###################################
# ELSA Origin
###################################
data_xls <- read_excel(paste0(path_config, file_xls, ext_xls), sheet="ELSA Origin")
for (i in 1:nrow(data_xls)) {
  a <- data_xls[[i,1]]
  if(!is.na(a)) {
    if(substr(a, 1, 4) == "path") {
      path_data <- sub("path ", "", a)
    }
    
    if(substr(a, 1, 8) == "filename") {
      file_data <- sub("filename ", "", a)
      print(paste0("loading...", file_data, ext_data))
      df_data  <- read_stata(paste0(path_data, file_data, ext_data))
      data_seg <- data_xls[(i+1):(i+8),1:10]
      data_tmp <- transfer_data(df_data, data_seg, 1)
      if (ncol(data_tmp) > 1) {
        if (ncol(data) > 0) {
          data <- merge(data, data_tmp, by = "idauniq", all = TRUE)
        }
        else {
          data <- data_tmp
        }
      }
    }
  }
}

###################################
# ELSA HCAP 2018
###################################
data_xls <- read_excel(paste0(path_config, file_xls, ext_xls), sheet="ELSA HCAP")
for (i in 1:nrow(data_xls)) {
  a <- data_xls[[i,1]]
  if(!is.na(a)) {
    if(substr(a, 1, 4) == "path") {
      path_data <- sub("path ", "", a)
    }
    
    if(substr(a, 1, 8) == "filename") {
      file_data <- sub("filename ", "", a)
      print(paste0("loading...", file_data, ext_data))
      df_data  <- read_stata(paste0(path_data, file_data, ext_data))
      data_seg <- data_xls[(i+1):(i+32),1:2]
      data_tmp <- transfer_data(df_data, data_seg, 2)
      if (ncol(data_tmp) > 1) {
        if (ncol(data) > 0) {
          data <- merge(data, data_tmp, by = "idauniq", all = TRUE)
        }
        else {
          data <- data_tmp
        }
      }
    }
  }
}

print("skimming...")
skimmed <- skim(data)
save(data, skimmed, file = paste0(path_savedata, "Data.RData"))

