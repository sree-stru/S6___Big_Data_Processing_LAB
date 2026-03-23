Study_Hours <- c(2, 4, 6, 8, 10)
Exam_Score <- c(65, 70, 85, 58, 92)
Caffeine_mg <- c(50, 100, 150, 200, 250)

data <- data.frame(Study_Hours, Exam_Score, Caffeine_mg)

var_study <- var(data$Study_Hours)
var_score <- var(data$Exam_Score)
var_caffeine <- var(data$Caffeine_mg)

cov_val <- cov(data$Study_Hours, data$Exam_Score)
cor_val <- cor(data$Study_Hours, data$Exam_Score)

cor_matrix <- cor(data)

cat("Variance (Study Hours):", var_study, "\n")
cat("Variance (Exam Score):", var_score, "\n")
cat("Variance (Caffeine mg):", var_caffeine, "\n")
cat("Covariance (Study vs Score):", cov_val, "\n")
cat("Correlation (Study vs Score):", cor_val, "\n")
cat("Full Correlation Matrix:\n")
print(cor_matrix)
