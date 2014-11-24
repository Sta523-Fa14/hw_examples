# Calculate pi by monte carlo sampling

options(digits=10)
size = 1e9

files = dir("results/","^mcpi_out")

results = mean(unlist(lapply(files, read.table)))

cat("Estimating pi using",size*length(files),"monte carlo draws:\n`")
cat("\n")
cat("    pi =", results, "\n")
cat("\n")
