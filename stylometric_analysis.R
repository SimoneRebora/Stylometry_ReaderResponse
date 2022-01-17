library(stylo)

# define features for analysis
all_mfw <- 1:1163
all_distances <- c("dist.manhattan", "dist.euclidean", "dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg", "dist.argamon", "dist.cosine", "dist.entropy", "dist.minmax", "dist.simple")
methods_combination <- expand.grid(all_mfw ,all_distances, stringsAsFactors = FALSE)

# prepare results
attribution_quality <- numeric()
attribution <- character()

# run iterative analysis
for(n in 1:dim(methods_combination)[1]){
  
  # pick up configuration
  mfw_choice <- methods_combination[,1][n]
  dist_choice <- methods_combination[,2][n]
  
  # run analysis
  my_results <- stylo(gui = F, 
                      frequencies = "corpus/table_with_frequencies.txt",
                      corpus.lang = "English",
                      encoding = "UTF-8",
                      mfw.min = mfw_choice,
                      mfw.max = mfw_choice,
                      mfw.incr = 0,
                      analysis.type = "CA",
                      distance.measure = dist_choice,
                      write.png.file = F
  )
  
  # check if out of MFW range (to avoid repeated analyses)
  if(length(my_results$features.actually.used) < mfw_choice){
    print("Out of range")
    next
  }
  
  # store result  
  results_quality <- sort(my_results$distance.table[3,])[2]
  attribution[n] <- unlist(strsplit(names(results_quality), "_"))[1]

  # print progress to log file
  cat(n/dim(methods_combination)[1], file = "progress.log")
  
}

# delete working files (many!!)
unlink("*_EDGES.csv")

# save results
colnames(methods_combination) <- c("MFW", "distance")
methods_combination$attribution <- attribution

write.csv(methods_combination, file = "results/stylo_attributions.csv")