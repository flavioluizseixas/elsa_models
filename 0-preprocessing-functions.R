library(dplyr)

# Usage:
# df <- cbind(df, f_extract_feature(df, "indager", f_ratio_between_last_two_measurements, c("w7", "w8")))

###############################################################################
f_split_attname_wave <- function(l_colname) {
  split_colname <- strsplit(l_colname, "_")
  l_attname <- split_colname[[1]][1]
  l_wave <- split_colname[[1]][2]
  return(list(attname = l_attname, wave = l_wave))
}

###############################################################################
f_filter_cols <- function(l_df, l_attname, waves) {
  names <- as.list(colnames(l_df))
  new_list_1 <- l_df[grep(paste0("^", l_attname), names)]
  new_list_2 <- new_list_1[grepl(paste(waves, collapse = "|"), names(new_list_1))]
  return(new_list_2)
}

###############################################################################
f_extract_feature <- function(l_df, att_name, op, waves) {
  sub_df <- f_filter_cols(l_df, att_name, waves)
  feature_df <- op(sub_df)
  return(feature_df)
}

###############################################################################
f_agebased_percentile <- function(l_df) {
  names(l_df)[2] <- "age"
  feature_name <- names(l_df)[1]
  names(l_df)[1] <- "feature"

  resp <- data.frame(coluna = rep(NA, nrow(l_df)))
  names(resp) <- paste0("f_", feature_name, "_agebased_percentile")
  l_df <- l_df %>%
    group_by(age10 = as.integer(age * 10)) %>%
    mutate(percentile = rank(feature)/(length(feature)+1))

  resp[[1]] = l_df$percentile
  print(names(resp))
  return(resp)
}

###############################################################################
###############################################################################
f_difference_between_last_two <- function(l_df) {
  names <- sort(colnames(l_df))
  split_names <- lapply(names, f_split_attname_wave)

  resp_attname <- paste0("f_", split_names[[1]][1],
                         "_difference_",
                         split_names[[2]][2], "_", split_names[[1]][2])
  resp <- data.frame(matrix(ncol = length(resp_attname), nrow = nrow(l_df)))
  colnames(resp) <- resp_attname
  resp[, resp_attname] = l_df[, names[2]] - l_df[, names[1]]

  print(resp_attname)
  return(resp)
}

###############################################################################
###############################################################################
f_ratio_between_last_two <- function(l_df) {
  names <- sort(colnames(l_df))
  split_names <- lapply(names, f_split_attname_wave)

  resp_attname <- paste0("f_", split_names[[1]][1],
                         "_ratio_",
                         split_names[[2]][2], "_", split_names[[1]][2])
  resp <- data.frame(matrix(ncol = length(resp_attname), nrow = nrow(l_df)))
  colnames(resp) <- resp_attname
  for(i in 1:nrow(l_df)) {
    if(l_df[i, names[1]] != 0) {
      resp[i, resp_attname] = l_df[i, names[2]] / l_df[i, names[1]]
    }
    else {
      resp[i, resp_attname] = 0
    }
  }
  print(resp_attname)
  return(resp)
}

###############################################################################
###############################################################################
f_slope <- function(l_df) {
  attnames <- sort(colnames(l_df))
  l_df <- l_df[attnames]
  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    wave_number <- sapply(attnames, function(x) substr(x, nchar(x), nchar(x)))
    y <- as.vector(unlist(l_df[i,]))
    x <- as.numeric(wave_number)

    model <- lm(y ~ x)
    result[i,1] <- coef(model)[2]
    #    result[i,2] <- summary(model)$r.squared
    #    result[i,2] <- summary(model)$sigma^2
  }

  names(result)[1] <-  paste0("f_", sub("_.+$", "", attnames[1]), "_slope")
  print(names(result)[1])
  return(result)
}

###############################################################################
###############################################################################
f_monotonicity <- function(l_df) {
  attnames <- sort(colnames(l_df), decreasing = TRUE) #w7, w6, w5, ...
  l_df <- l_df[attnames]

  r <- (max(l_df) - min(l_df)) * 0.01


  result <- data.frame()
  for(i in 1:nrow(l_df)) {
    for(j in 2:ncol(l_df)) {
      lim_min <- l_df[i,j-1] - r
      lim_max <- l_df[i,j-1] + r

      if ((l_df[i,j] > lim_min) & (l_df[i,j] < lim_max)) {
        if (j == 2) {
          trend <- 0
          e <- 0
        }

        if (trend != 0) {
          break
        }
      }
      else {
        if (l_df[i,j] > l_df[i,j-1]) {
          if (j == 2) {
            trend <- -1
            e <- 0
          }

          if (trend == -1) {
            e <- e + 1
          }
          else {
            break
          }
        }
        else {
          if (l_df[i,j] < l_df[i,j-1]) {
            if (j == 2) {
              trend <- 1
              e <- 0
            }

            if (trend == 1) {
              e <- e - 1
            }
            else {
              break
            }
          }
        }
      }
    }
    result[i,1] <- e
  }
  names(result)[1] <- paste0("f_", sub("_.+$", "", attnames[1]), "_monotonicity")
  print(names(result)[1])
  return(result)
}

