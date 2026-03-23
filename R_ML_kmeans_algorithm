data <- data.frame(
  x = c(67, 320, 33, 175, 108),
  y = c(55, 32, 33, 76, 25)
)

initial_centers <- data.frame(
  x = c(120, 113),
  y = c(32, 33)
)

kmeans_result <- kmeans(
  data,
  centers = initial_centers,
  iter.max = 100
)

plot(
  data$x, data$y,
  col = kmeans_result$cluster,  
  pch = 16,                      
  xlab = "X",
  ylab = "Y",
  main = "K-means Clustering"
)

points(
  kmeans_result$centers[,1],     
  kmeans_result$centers[,2],    
  pch = 8,                      
  cex = 2,                       
  col = "black"
)

legend(
  "topleft",
  legend = c("Cluster 1", "Cluster 2", "Centers"),
  col = c(1, 2, "black"),
  pch = c(16, 16, 8)
)
