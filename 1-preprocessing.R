#setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/Dynamic-Models")
#setwd("~/IdUFF/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features")
#setwd("~/IdUFF/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/Dynamic-Models")

library(foreign)
library(dplyr)
library(readxl)
library(openxlsx)

rm(list = ls())
source('0-preprocessing-functions.R')

sheetname <- "covariates"
ext <- "_Nurse"
filename <- "Elsa"
path <- "Data/"

for (ref in 7:7) {
  w_ref <- paste0("w", ref)

  ###############################################################################
  rename_names <- function(my_env) {
    lst_rename <- grep("-", my_env$attnames$covariate)
    for(i in lst_rename) {
      lst_rename_w <- grep("X", my_env$attnames[i, 3:ncol(my_env$attnames)])
      new_attname <- gsub("-", "", my_env$attnames[i,2])
      for(j in lst_rename_w) {
        ix <- grep(paste0(my_env$attnames[i,2], "_", colnames(my_env$attnames)[j+2]), colnames(my_env$df))
        names(my_env$df)[ix] <- paste0(new_attname, "_", colnames(my_env$attnames)[j+2])
      }
      my_env$attnames[i,2] <- new_attname
    }
  }

  ###############################################################################
  ###############################################################################

  data  <- read.arff(paste0("../../Elsa", ext, ".arff"))
  my_env <- new.env()
  my_env$df <- data

  filename_params <- "att-list.xlsx"
  my_env$attnames <- read_excel(filename_params, sheet=paste0("covariates", ext))
  rename_names(my_env)

  # converter os atributos tipo factor para numeric
  for(i in 1:nrow(my_env$attnames)) {
    attname = my_env$attnames[i, 2]
    attname_w <- grep("X", my_env$attnames[i, 3:ncol(my_env$attnames)])
    for (j in attname_w) {
      print(attname[[1]])
      if (colnames(my_env$attnames)[j+2] == "NA") {
        ix <- grep(my_env$attnames[i,2], colnames(my_env$df))
      }
      else {
        ix <- grep(paste0(my_env$attnames[i,2], "_", colnames(my_env$attnames)[j+2]), colnames(my_env$df))
      }
      if(is.factor(my_env$df[[ix]])) {
        my_env$df[ix] <- as.numeric(as.character(my_env$df[[ix]]))
      }
    }
  }

  atts <- c()
  for(i in 1:nrow(my_env$attnames)) {
    attname = my_env$attnames[i, 2]
    for (j in 3:ncol(my_env$attnames)) {
      if(!is.na(my_env$attnames[i,j])) {
        if(my_env$attnames[i,j] == "X") {
          if (colnames(my_env$attnames)[j] == "NA") {
            atts <- append(atts, paste0(attname)[1])
          }
          else {
            atts <- append(atts, paste0(attname, "_", colnames(my_env$attnames)[j]))
          }
        }
      }
    }
  }

  ###############################################################################
  ## monotonicity
  ###############################################################################
  f_params <- read_excel(filename_params, sheet = paste0("f_monotonicity", ext))
  for (i in 1:nrow(f_params)) {
    if (is.na(f_params[i,1])) {
      break
    }
    l_df <- data.frame()
    att_name <- f_params[i,2]
    att_name <- gsub("-", "", att_name)
    f_params_cols <- grep(w_ref, substr(colnames(f_params), 1, 2))
    for (j in f_params_cols) {
      if(!is.na(f_params[i,j])) {
        colname <- paste0(att_name, "_w", f_params[i,j])
        coldf <- my_env$df %>% select(colname)
        if (ncol(coldf) > 0) {
          if (ncol(l_df) == 0) {
            l_df <- data.frame(coldf)
          }
          else {
            l_df <- cbind(l_df, coldf)
          }
        }
      }
    }
    if(ncol(l_df) > 1) {
      my_env$df <- cbind(my_env$df, f_monotonicity(l_df))
    }
  }
  print("monotonicity")
  print(dim(my_env$df))

  ###############################################################################
  ## slope
  ###############################################################################
  f_params <- read_excel(filename_params, sheet = paste0("f_slope", ext))
  for (i in 1:nrow(f_params)) {
    if (is.na(f_params[i,1])) {
      break
    }
    l_df <- data.frame()
    att_name <- f_params[i,2]
    att_name <- gsub("-", "", att_name)
    f_params_cols <- grep(w_ref, substr(colnames(f_params), 1, 2))
    for (j in f_params_cols) {
      if(!is.na(f_params[i,j])) {
        colname <- paste0(att_name, "_w", f_params[i,j])
        coldf <- my_env$df %>% select(colname)
        if (ncol(coldf) > 0) {
          if (ncol(l_df) == 0) {
            l_df <- data.frame(coldf)
          }
          else {
            l_df <- cbind(l_df, coldf)
          }
        }
      }
    }
    if(ncol(l_df) > 1) {
      my_env$df <- cbind(my_env$df, f_slope(l_df))
    }
  }
  print("slope")
  print(dim(my_env$df))

  ###############################################################################
  ## difference
  ###############################################################################
  f_params <- read_excel(filename_params, sheet = paste0("f_difference", ext))
  for (i in 1:nrow(f_params)) {
    if (is.na(f_params[i,1])) {
      break
    }
    l_df <- data.frame()
    att_name <- f_params[i,2]
    att_name <- gsub("-", "", att_name)
    f_params_cols <- grep(w_ref, substr(colnames(f_params), 1, 2))
    for (j in seq(f_params_cols[1], length(f_params_cols) + f_params_cols[1] - 1, by = 2)) {
      if (!is.na(f_params[i,j])) {
        colname_A <- paste0(att_name, "_w", f_params[i,j])
        colname_B <- paste0(att_name, "_w", f_params[i,j+1])
        l_df <- my_env$df %>% select(colname_A, colname_B)
        my_env$df <- cbind(my_env$df, f_difference_between_last_two(l_df))
      }
    }
  }
  print("difference")
  print(dim(my_env$df))

  ###############################################################################
  ## ratio
  ###############################################################################
  f_params <- read_excel(filename_params, sheet = paste0("f_ratio", ext))
  for (i in 1:nrow(f_params)) {
    if (is.na(f_params[i,1])) {
      break
    }
    att_name <- f_params[i,2]
    att_name <- gsub("-", "", att_name)
    f_params_cols <- grep(w_ref, substr(colnames(f_params), 1, 2))
    for (j in seq(f_params_cols[1], length(f_params_cols) + f_params_cols[1] - 1, by = 2)) {
      if (!is.na(f_params[i,j])) {
        colname_A <- paste0(att_name, "_w", f_params[i,j])
        colname_B <- paste0(att_name, "_w", f_params[i,j+1])
        l_df <- my_env$df %>% select(colname_A, colname_B)
        my_env$df <- cbind(my_env$df, f_ratio_between_last_two(l_df))
      }
    }
  }
  print("ratio")
  print(dim(my_env$df))

  ###############################################################################
  ## agebased percentile
  ###############################################################################
  f_params <- read_excel(filename_params, sheet = paste0("f_agebased_percentile", ext))
  for (i in 1:nrow(f_params)) {
    if (is.na(f_params[i,1])) {
      break
    }
    att_name <- f_params[i,2]
    att_name <- gsub("-", "", att_name)
    f_params_cols <- grep(w_ref, substr(colnames(f_params), 1, 2))
    l_df <- data.frame()
    for (j in f_params_cols) {
      if (!is.na(f_params[i,j])) {
        colname_A <- paste0(att_name, "_w", f_params[[j]][i])
        if (ext == "_Nurse") {
          colname_B <- paste0("indager_w", 8)  # exceção para o caso do Nurse que só tem indager_w8
        }
        else {
          colname_B <- paste0("indager_w", f_params[[j]][i])
        }
        l_df <- my_env$df %>% select(colname_A, colname_B)
        my_env$df <- cbind(my_env$df, f_agebased_percentile(l_df))
      }
    }
  }
  print("agebased_percentile")
  print(dim(my_env$df))


  ###############################################################################

  class_name <- paste0("class_dementia_w", ref + 1)
  for (j in 1:ref) {
    if (j == 1) {
      att_i <- grep(paste0("w", j), atts)
    }
    else {
      att_i <- c(att_i, grep(paste0("w", j), atts))
    }
  }
  att_i <- c(att_i, grep("sex", atts))
  att_str <- atts[att_i]
  att_str_f <- paste(att_str, collapse = " + ")
  att_str_f <- paste(class_name, "~", att_str_f)
  att_f <- as.formula(att_str_f)

  attsdf <- names(my_env$df)
  att_i_CTF <- grep("^f_", attsdf)
  att_str_CTF <- attsdf[att_i_CTF]

  att_str_CTF <- c(att_str, att_str_CTF, class_name)
  att_str_f_CTF <- paste(att_str_CTF, collapse = " + ")
  att_str_f_CTF <- paste0(class_name, " ~ ", att_str_f_CTF)
  att_f_CTF <- as.formula(att_str_f_CTF)

  df <- my_env$df %>% select(att_str_CTF)
  save(df, class_name, att_f_CTF, att_f,
       att_str_CTF, att_str, file=paste0(path, ref, "_", filename, ext, ".RData"))
}
