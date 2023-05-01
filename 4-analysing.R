ext <- "Nurse"
filename <- "Elsa"
path <- "Data/"

sink(paste0(path, "0_", ext, "_outfile.txt"))

methods <- c("under", "over", "under_CTF", "over_CTF", "under_CTF_sel", "over_CTF_sel")
methodstxt <- methods

models <- c("ranger", "glm", "nnet", "rf")
modelstxt <- models

metricsbyClass <- c("Sensitivity", "Specificity", "Precision", "Neg Pred Value", "F1")
metricsOverall <- c("Accuracy", "Kappa")

for (ref in 4:4) {
  filename <- paste0(path, ref, "_", filename, "_", ext, "_results.RData")
  load(filename)
  print("--------------------------------")
  print(paste0("Starting Wave ", ref))
  print("--------------------------------")
  print(filename)
  for (i in methods) {                # methods
    for (j in models) {               # models
      print(i)
      print(j)
      print(rs[[i]][[j]][["confusionMatrix"]])
    }
  }
}
sink()
