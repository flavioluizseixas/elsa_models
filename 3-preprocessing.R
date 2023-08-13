library(dplyr)
library(readxl)
library(skimr)

rm(list = ls())

###############################################################################
# monotonicity
###############################################################################
f_monotonicity <- function(l_df) {
  attnames <- sort(colnames(l_df), decreasing = TRUE)
  l_df <- l_df[attnames]
  
  p <- 0.1
  
  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    trend <- 0
    e <- 0
    
    valor <- c()
    primeiro <- 0
    for(j in 1:(ncol(l_df)-1)) {
      if(!is.na(l_df[i,j])) {
        primeiro <- 1
        valor <- c(valor, l_df[i,j])
      }
      else {
        if(primeiro == 1) {
          break
        }
      }
    }
    
    if (length(valor) >= n_min) {
      for (j in 1:(length(valor)-1)) {
        ###########################
        # Critério para considerar alteração
        ###########################
        if (valor[j] == 0) {
          d <- (valor[j+1] - valor[j]) / (valor[j+1] + 1)
        }
        else {
          d <- (valor[j+1] - valor[j]) / valor[j]
        }
        if (abs(d) < p) {
          break
        }
        else {
          if (d < 0) {
            if (trend == 0) {
              trend <- 1
            }
            if (trend == 1) {
              e <- e + 1
            }
            else {
              break
            }
          }
          else {
            if (trend == 0) {
              trend <- -1
            }
            if (trend == -1) {
              e <- e - 1
            }
            else {
              break
            }
          }
        }
      }
    }
    else {
      e <- NA
    }
    result[i,1] <- e
  }
  names(result)[1] <- paste0("f_", sub("_.+$", "", attnames[1]), "_monotonicity")
  print(names(result)[1])
  return(result)
}

###############################################################################
# slope
###############################################################################
f_slope <- function(l_df) {
  attnames <- sort(colnames(l_df))
  l_df <- l_df[attnames]

  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    x <- c()
    y <- c()
    for (j in 1:ncol(l_df)) {
      if (!is.na(l_df[i,j])) {
        y <- c(y, l_df[i,j])
        x <- c(x, j)
      }
    }
    if (length(x) >= n_min) {
      model <- lm(y ~ x)
      result[i,1] <- coef(model)[2]  # slope
      result[i,2] <- coef(model)[1]  # intercept
    }
    else {
      result[i,1] <- NA
      result[i,2] <- NA
    }
  }
  
  names(result)[1] <-  paste0("f_", sub("_.+$", "", attnames[ncol(l_df)]), "_slope")
  names(result)[2] <-  paste0("f_", sub("_.+$", "", attnames[ncol(l_df)]), "_intercept")
  print(names(result)[1])
  print(names(result)[2])

  return(result)
}

###############################################################################
# difference
###############################################################################
f_difference_between_last_two <- function(l_df) {
  attnames <- sort(colnames(l_df), decreasing = TRUE)
  l_df <- l_df[attnames]
  
  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    if(!is.na(l_df[i,1]) & (!is.na(l_df[i,2]))) {
      result[i,1] = l_df[i,1] - l_df[i,2] 
    }
    else {
      result[i,1] = NA
    }
  }

  names(result)[1] <- paste0("f_", attnames[1], "_", attnames[2], "_diff")
  print(names(result)[1])

  return(result)
}

###############################################################################
# ratio
###############################################################################
f_ratio_between_last_two <- function(l_df) {
  attnames <- sort(colnames(l_df), decreasing = TRUE)
  l_df <- l_df[attnames]
  
  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    if(!is.na(l_df[i,1]) & (!is.na(l_df[i,2]))) {
      if (l_df[i,2] != 0) {
        result[i,1] = l_df[i,1] / l_df[i,2]
      }
      else {
        result[i,1] = NA
      }
    }
    else {
      result[i,1] = NA
    }
  }

  names(result)[1] <- paste0("f_", attnames[1], "_", attnames[2], "_ratio")
  print(names(result)[1])

  return(result)
}


###################################
# ELSA Features
###################################
file_xls    <- "Atts-ELSA-HCAP-G2"
path_config <- "Config/"
path_data   <- "Data/"
ext_xls     <- ".xlsx"
ext_data    <- ".dta"
data_xls    <- read_excel(paste0(path_config, file_xls, ext_xls), sheet="Harmonized ELSA G.2 (2002-2019)")

load(paste0(path_data, "Data.RData"))
classname   <- "dementia_w9"
n_min <- 3

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
###################################
###################################
###################################
# Filtra apenas as instâncias com classe preenchida
###################################
data <- data[complete.cases(data[[classname]]), ] # 8732, 460

###################################
# Substitui valores categoricos
###################################
data <- replace(data, data < 0, NA)

###################################
# Retira alguns atributos que estão errados
###################################
data <- data[, -grep("r8agey", colnames(data))]
data <- data[, -grep("r7agey", colnames(data))]

print("1. Skimming data original...")
dim_data_orig <- dim(data)
skim_data_orig <- skim(data)

###################################
# Add CTFs
###################################
data_debug_CTF <- data.frame()
for (k in 1:nrow(data_xls)) {
  if (!is.na(data_xls[[k,1]])) {
    if (data_xls[[k,1]] == "Section B: Health") {
      
      for (i in (k+1):nrow(data_xls)) {
        
        df <- data.frame()
        for (j in 2:10) {
          if(!is.na(data_xls[[i,j]])) {
            feature <- extract_feature(data_xls[[i,1]], data_xls[[i,j]])
            atti <- grep(feature, colnames(data))
            if (length(atti) > 0) {
              if (ncol(df) == 0) { 
                df <- data[atti[1]]
              }
              else {
                df <- cbind(df, data[atti[1]])
              }
            }
          }
        }
        
        if (ncol(df) >= n_min) {
          r <- f_monotonicity(df)
          data <- cbind(data, r)
          
          a <- cbind(df, r)
          if(ncol(data_debug_CTF) == 0) {
            data_debug_CTF <- a
          }
          else {
            data_debug_CTF <- cbind(data_debug_CTF, a)
          }

          r <- f_slope(df)
          data <- cbind(data, r)
          
          a <- cbind(df, r)
          data_debug_CTF <- cbind(data_debug_CTF, a)
        }

        if (ncol(df) >= 2 ) {
          r <- f_difference_between_last_two(df)
          data <- cbind(data, r)

          a <- cbind(df, r)
          data_debug_CTF <- cbind(data_debug_CTF, a)

          r <- f_ratio_between_last_two(df)
          data <- cbind(data, r)
          
          a <- cbind(df, r)
          data_debug_CTF <- cbind(data_debug_CTF, a)
        }
                
        if (is.na(data_xls[[k,1]])) {
          break
        }
      }
    }
  }
}

print("2. Skimming data with CTFs...")
dim_data_CTF <- dim(data)
skim_data_CTF <- skim(data)
skim_select <- skim_data_CTF

###################################
# Exclude HED* (HEDBDAD, HEDIBAD, HEDBDDE, HEDIBDE, HEDBSDE, HEDIAD8, HEDIAD9)
###################################
skim_select <- skim_select %>% filter(!startsWith(skim_variable, "hed"))

###################################
# Demographic
###################################
skim_select_demographic <- skim_select %>% filter(grepl("^ra", skim_variable))

###################################
# Divide per wave
###################################
skim_select_w1 <- skim_select %>% filter(grepl("^r1", skim_variable))
skim_select_w2 <- skim_select %>% filter(grepl("^r2", skim_variable))
skim_select_w3 <- skim_select %>% filter(grepl("^r3", skim_variable)
                                         | skim_variable == "dementia_w3")

skim_select_w4 <- skim_select %>% filter(grepl("^r4", skim_variable)
                                         | skim_variable == "dementia_w4")

skim_select_w5 <- skim_select %>% filter(grepl("^r5", skim_variable)
                                         | skim_variable == "dementia_w5")

skim_select_w6 <- skim_select %>% filter(grepl("^r6", skim_variable)
                                         | skim_variable == "dementia_w6")

skim_select_w7 <- skim_select %>% filter(grepl("^r7", skim_variable)
                                         | skim_variable == "dementia_w7")

skim_select_w8 <- skim_select %>% filter(grepl("^r8", skim_variable)
                                         | skim_variable == "dementia_w8")

skim_select_w9 <- skim_select %>% filter(grepl("^r9", skim_variable)
                                         | skim_variable == "dementia_w9")

skim_select_ctf <- skim_select %>% filter(grepl("^f_", skim_variable)
                                         | skim_variable == "dementia_w9")

skim_select_hcap <- skim_select %>% filter(grepl("_res$", skim_variable)
                                         | grepl("_inf$", skim_variable)
                                         | skim_variable == "dementia_w9")

data_w1 <- data %>% select(all_of(skim_select_w1[["skim_variable"]]))
data_w2 <- data %>% select(all_of(skim_select_w2[["skim_variable"]]))
data_w3 <- data %>% select(all_of(skim_select_w3[["skim_variable"]]))
data_w4 <- data %>% select(all_of(skim_select_w4[["skim_variable"]]))
data_w5 <- data %>% select(all_of(skim_select_w5[["skim_variable"]]))
data_w6 <- data %>% select(all_of(skim_select_w6[["skim_variable"]]))
data_w7 <- data %>% select(all_of(skim_select_w7[["skim_variable"]]))
data_w8 <- data %>% select(all_of(skim_select_w8[["skim_variable"]]))
data_w9 <- data %>% select(all_of(skim_select_w9[["skim_variable"]]))
data_ctf <- data %>% select(all_of(skim_select_ctf[["skim_variable"]]))
data_demographic <- data %>% select(all_of(skim_select_demographic[["skim_variable"]]))

###################################
# Select data HCAP
###################################
data_hcap <- data %>% select(all_of(skim_select_hcap[["skim_variable"]]))
data_hcap <- data_hcap %>% filter(!is.na(sex_res))

###################################
# SELECT DATA
###################################
data_select <- cbind(data_demographic, data_w7, data_w8, data_w9)
data_select_withCTF <- data_ctf

skim_select <- skim(data_select)
skim_select_withCTF <- skim(data_select_withCTF)
skim_select_hcap <- skim(data_hcap)

data_select[[classname]] <- as.factor(data_select[[classname]])
levels(data_select[[classname]]) <- c("no", "yes")

data_select_withCTF[[classname]] <- as.factor(data_select_withCTF[[classname]])
levels(data_select_withCTF[[classname]]) <- c("no", "yes")

data_hcap[[classname]] <- as.factor(data_hcap[[classname]])
levels(data_hcap[[classname]]) <- c("no", "yes")

save(data_select, data_select_withCTF, data_hcap,
     skim_select, skim_select_withCTF, skim_select_hcap,
     dim_data_orig, skim_data_orig,
     dim_data_CTF, skim_data_CTF, data_debug_CTF,
     file = paste0(path_data, "Data_processed.RData"))
