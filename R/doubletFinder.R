doubletFinder <- function(data, select.genes, proportion.artificial = 0.25, k = round(ncol(data) * 0.005)) {
  library(RANN)
  
  ## Step 1: Generate artificial doublets from norm.dat
  print("Creating artificial doublets...")
  real.cells <- colnames(data)
  n_real.cells <- length(real.cells)
  n_doublets <- round(n_real.cells/(1-proportion.artificial)-n_real.cells)
  real.cells1 <- sample(real.cells, n_doublets, replace = TRUE)
  real.cells2 <- sample(real.cells, n_doublets, replace = TRUE)
  doublets <- (data[ , real.cells1] + data[ , real.cells2])/2
  colnames(doublets) <- paste("X", 1:n_doublets, sep="")
  
  data_wdoublets <- cbind(data, doublets)
  data_wdoublets@x = log2(data_wdoublets@x+1)
  sampled.cells = sample(1:ncol(data), ncol(data)*0.25) 
  
  print("Running PCA")
  rd.dat = scrattch.hicat::rd_PCA(data_wdoublets, select.genes=select.genes, select.cells= colnames(data_wdoublets), th=0, max.pca=50, sampled.cells= sampled.cells)
  
  print("Initialize pANN structure") 
  knn.idx = RANN::nn2(rd.dat, k=k)[[1]] # matrix with near neighbour indices
  #knn.dist = nn2(rd.dat, k=k)[[2]] #matrix with near neighbour Euclidean distances
  #knn = nn2(rd.dat, k=k) #2 lists (1) indices (2) distances
  doublet.freq = knn.idx  > ncol(data)
  doublet.score = rowMeans(doublet.freq)
  doublet.score = doublet.score[1:ncol(data)]
  doublet.score = setNames(doublet.score, colnames(data))
  
  return(doublet.score)
}