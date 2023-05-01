setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/Dynamic-Models")

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

rm(list = ls())
d_f <- data.frame()

ext <- "Core"
filename <- "Elsa"

methods <- c("under", "over", "under_CTF", "over_CTF", "under_CTF_sel", "over_CTF_sel")
methodstxt <- methods

models <- c("ranger", "glm", "nnet", "rf")
#models <- c("ranger", "glm")
modelstxt <- models

metricsbyClass <- c("Sensitivity", "Specificity", "Precision", "Neg Pred Value", "F1")
metricsOverall <- c("Accuracy", "Kappa")

if (file.exists("results.RData")) {
  load("results.RData")
} else {
  for (ref in 4:7) {
    load(paste0(ref, "_", filename, "_", ext, "_results.RData"))
    for (i in methods) {                # methods
      for (j in models) {               # models
        for (k in metricsbyClass) {
          d <- data.frame(method = i,
                          model = j,
                          metric = k,
                          class_name = paste0("w", ref),
                          value = rs[[i]][[j]][["confusionMatrix"]][["byClass"]][[k]])
          rownames(d) <- nrow(d_f) + 1
          d_f <- rbind(d_f, d)
        }
        for (k in metricsOverall) {
          d <- data.frame(method = i,
                          model = j,
                          metric = k,
                          class_name = paste0("w", ref),
                          value = rs[[i]][[j]][["confusionMatrix"]][["overall"]][[k]])
          rownames(d) <- nrow(d_f) + 1
          d_f <- rbind(d_f, d)
        }
      }
    }
  }
  save(d_f, file = "results.RData")
}

for (i in methods) {
  gs <- list()

  for (j in metricsbyClass) {
    df_g <- d_f %>% filter(method==i, metric==j)
    g <- ggplot(df_g, aes(x = class_name, y = value, color = model, group = model)) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      ggtitle(paste(metricsbyClass[which(metricsbyClass==j)], methodstxt[which(methods==i)])) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    gs <- c(gs, list(g))
  }
  for (j in metricsOverall) {
    df_g <- d_f %>% filter(method==i, metric==j)
    g <- ggplot(df_g, aes(x = class_name, y = value, color = model, group = model)) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      ggtitle(paste(metricsOverall[which(metricsOverall==j)], methodstxt[which(methods==i)])) +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
    gs <- c(gs, list(g))
  }


  my_plot <- wrap_plots(gs[[1]], gs[[2]], gs[[3]], gs[[4]], gs[[5]], gs[[6]], nrow=2, ncol=3)
  ggsave(paste0(i, ".png"), my_plot, width = 12, height = 5, dpi=150)
}
