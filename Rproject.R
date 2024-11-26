library(readr)
business <- read_csv("Downloads/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(business)

library(readr)
user <- read_csv("Downloads/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user)

business_values <- business[1:5]
print(business_values)

cont_table <- table(business$stars)
pie(cont_table, main = "Stars", col = rainbow(5))


ggplot(business) + geom_boxplot(aes(x = review_count, y = stars))


stars1 <- business$stars[business$stars == 1]
stars5 <- business$stars[business$stars == 5]

print(stars1)
print(stars5)


chisq.test(stars1)
chisq.test(stars5)





#Users data

colnames(user)

votes_data <- user[, c("cool_votes", "funny_votes", "useful_votes")]


correlation_matrix <- cor(votes_data, method = "pearson")
print(correlation_matrix)


model <- lm(useful_votes ~ cool_votes, data = user)


summary(model)


intercept <- coef(model)[1]
slope <- coef(model)[2]
cat("Equation of the fit line: useful_votes =", intercept, "+", slope, "* cool_votes\n")


plot(user$cool_votes, user$useful_votes, main = "Linear Regression of Useful Votes on Cool Votes",
     xlab = "Cool Votes", ylab = "Useful Votes")
abline(model, col = "red")


review_fan_model <- lm(fans ~ review_count, data = user)


summary(review_fan_model)


plot(user$review_count, user$fans, main = "Linear Regression of Fans on Review Count",
     xlab = "Review Count", ylab = "Fans")
abline(review_fan_model, col = "blue")

#predict fans

fans_useful_model <- lm(fans ~ useful_votes, data = user)

summary(fans_useful_model)

plot(user$useful_votes, user$fans, main = "Linear Regression of Fans on Useful Votes",
     xlab = "Useful Votes", ylab = "Fans")
abline(fans_useful_model, col = "green")


library(ggplot2)

kmeans_data <- user[, c("review_count", "fans")]

wcss <- function(k) {
  kmeans(kmeans_data, centers = k, nstart = 10)$tot.withinss
}

k_values <- 1:10

wcss_values <- sapply(k_values, wcss)

print(wcss_values)

plot(k_values, wcss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Determining Optimal k")

kmeans_result <- kmeans(kmeans_data, centers = best_k, nstart = 10)


user$cluster <- as.factor(kmeans_result$cluster)

ggplot(users) + 
  geom_point(aes(x = review_count, y = fans, color = cluster)) +
  labs(title = "K-Means Clustering on Review Count and Fans",
       x = "Review Count", y = "Fans")






































