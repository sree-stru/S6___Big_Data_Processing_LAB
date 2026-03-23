scores <- c(10, 20, 30, 40, 50, 60)

cat("Mean:", mean(scores), "\n")
cat("Median:", median(scores), "\n")
cat("Standard Deviation:", sd(scores), "\n")
cat("Range:", paste(range(scores), collapse = " - "), "\n")
cat("Summary:\n")
print(summary(scores))
