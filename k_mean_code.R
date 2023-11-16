library(tidyverse)
library(readxl)
library(cluster)
library(GGally)
library(plotly)
library(tidyr)

df <- read_excel("kmeans.xlsx")

## Data Preprocessing

# Checking for missing values
mean(complete.cases(df))

# Tranform column names and convert order date to Date type
colnames(df) <- tolower(colnames(df))
df$order.date <- as.Date(df$order.date)

# Calculate total sales and set a reference date
df$total_sale <- df$sales * df$quantity
current_date <- as.Date("2018-01-01")

# RFM calculate
RFM <- df %>%
  group_by(customer.id)%>%
  summarise(
    Recency = as.numeric(difftime(current_date, max(order.date), units = "days")),
    Frequency = n(),
    Monetary = mean(total_sale)
  )%>%
  arrange(Recency)

# Write RFM data to a CSV file
final <- write.csv(RFM, "RFM_kmeans.csv", row.names = FALSE)

# Read the RFM data
RFM_final <- read.csv("RFM_kmeans.csv")

## K-means clustering using RFM 

# Filter out customer ID
RFM_final <- RFM_final %>%
  select(Recency, Frequency, Monetary)

# Change data types to numeric
RFM_final$Recency <- as.numeric(RFM_final$Recency)
RFM_final$Frequency <- as.numeric(RFM_final$Frequency)
RFM_final$Monetary <- as.numeric(RFM_final$Monetary)

# Histograms
par(mfrow = c(1, 3))

v1 <- hist(RFM_final$Recency, col = "skyblue", border = "black",main = "Recency Histogram", xlab ="Values")
v2 <- hist(RFM_final$Frequency, col = "salmon", border = "black",main = "Frequency Histogram", xlab ="Values")
v3 <- hist(RFM_final$Monetary, col = "greenyellow", border = "black",main = "Monetary Histogram", xlab ="Values")

# Reset the layout to default
par(mfrow = c(1, 1))

# Standardize the data 
standard_rfm <- scale(RFM_final)

# Elbow method
wss <- vector("numeric", length = 10)

for (i in 2:10) {
  kmeans_model <- kmeans(standard_rfm, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}

plot(2:10, wss[2:10], type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Sum of Squares")

# Optimal k
optimal_k <- 4

# K-means clustering
setseed(9)
kmeans_model <- kmeans(standard_rfm, centers = optimal_k, nstart = 10)

# Add Cluster Assignments
kmeans_final <- cbind(RFM_final, Cluster = kmeans_model$cluster)

## Analyze clusters
summary_1 <- kmeans_final %>%
  group_by(Cluster) %>%
  summarise(avg_recency = median(Recency),
            avg_frequency = median(Frequency),
            avg_monetary = median(Monetary))

# Bar chart
unpivot_data <- summary %>%
  gather(key = "Variable", value = "Value", -Cluster)

ggplot(unpivot_data, aes(x = Cluster, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Values by Cluster",
       x = "Cluster",
       y = "Average Value",
       fill = "Variable") +
  theme_minimal()

# 3D plot
plot_ly(kmeans_final, x = ~Recency, y = ~Frequency, z = ~Monetary) %>%
  add_markers(color = ~Cluster)

# Pair pot 
ggpairs(kmeans_final,
        aes(color = factor(Cluster)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        upper = list(continuous = "blank"),
        diag = list(continuous = wrap("barDiag", bins = 20)))

# Scatter plot
ggplot(kmeans_final, aes(x = Frequency, y = Monetary, color = factor(Cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Results with RFM") +
  theme_minimal()
