#Load the packages
library(tidyverse) #data cleaning
library(skimr)  #descriptive statistics
library(ggvis) #scatterplots
library(corrgram)
library(corrplot)
library(GGally)
library(ggplot2)
library(dplyr) 
library(purrr)
library(caTools)
library(reshape2)
library(ClusterR)
library(cluster)
library(dendextend)
library(datasets)
library(dbscan)
library(latexpdf)
library(tinytex)

# Load dataset 
customer_data <- read.csv("C:/Users/LENOVO/Desktop/ThinkPacificProject/Mall_Customers.csv")

#Rename some column names
customer_data <- rename(customer_data, Annual_Income=Annual.Income..k..,
                   Spending_Score=Spending.Score..1.100., Gender=Genre)

#View customer
View(customer_data)

# Convert heart_data to a data frame
customer_df <- as.data.frame(customer_data)


# make a copy of the df
customer_df_km <- customer_df

# view the head
head(customer_df)

#dimensions of the data
dim(customer_df)

#data structure
str(customer_df)

# list types for each attribute
sapply(customer_df, class)

#column names
colnames(customer_df)

#data summary
summary(customer_df)

#checking for missing values
sum(is.na(customer_df))

# Check missing values for each of the columns
customer_df %>%
  purrr::map_df(~ sum(is.na(.)))

# View summary statistics with numeric hist
skimmed_data <- skim(customer_df)
View(skimmed_data)

# Count the occurrences of unique values in the "target" column
table(customer_df$Gender)

# Exploratory Data Analysis

# Creating a barplot to assess gender distribution of my sample of customers.
ggplot(customer_df,aes(x= Gender)) +
  geom_bar(stat="count",width=0.5,fill="steelblue") +
  theme_minimal()+
  labs(title="Barplot to display Gender Comparison", xlab="Gender")


# Age distribution of the customers
ggplot(customer_df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Customer Ages",
       x = "Age",
       y = "Count") +
  theme_minimal()


# Annual income distribution of the customers
ggplot(customer_df, aes(x = Annual_Income)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Annual Income",
       x = "Annual Income",
       y = "Count") +
  theme_minimal()


# Annual income distribution of the customers
ggplot(customer_df, aes(x = Spending_Score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Spending Score",
       x = "Spending Score",
       y = "Count") +
  theme_minimal()

# Calculate the correlation matrix
numeric_customer_df <- customer_df[, sapply(customer_df, is.numeric)]
corr_matrix <- cor(numeric_customer_df)

# Heatmap 
ggplot(data = melt(corr_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add labels with 2 decimal places
  scale_fill_gradient(low = "steelblue", high = "gray") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap")


# Create a histogram with ggplot
ggplot(customer_df, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 1, color = 'black', alpha = 0.7) +
  labs(title = 'Age Distribution of Customers by Gender',
       x = 'Age',
       y = 'Count') +
  theme_minimal() +
  scale_fill_manual(values = c("gray", 'steelblue'))


# Histogram to show dispersion of mall customers based on age groups
ggplot(customer_df, aes(x = Age)) +
  geom_vline(aes(xintercept = mean(Age)), color = "black",  #adding an intercept to indicate mean age
             linetype = "dashed", size = 1.0) +
  geom_histogram(binwidth = 5, aes(y = ..density..), 
                 color = "black", fill = "steelblue") +
  geom_density(alpha = 0.4, fill = "gray") +  #adding density plot
  labs(title = "Histogram to Show Density of Age Groups")


# Creating density plot to show customer's annual income
ggplot(customer_df, aes(x = Annual_Income)) +
  geom_density(alpha=0.4, fill="steelblue") +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(title="Density Plot for Annual Income")


# Assuming 'data' is your data frame
plot(customer_df$CustomerID, customer_df$Annual_Income, 
     type = "p", 
     col = "steelblue", 
     main = "Annual Income by CustomerID", 
     xlab = "CustomerID", 
     ylab = "Annual Income"
)

# Assuming 'customer_df' is your data frame
ggplot(customer_df, aes(x = Age, y = Annual_Income, color = Gender)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs Annual Income by Gender",
       x = "Age",
       y = "Annual Income") +
  scale_color_manual(values = c("orange","steelblue")) +
  theme_minimal()


# Assuming 'customer_df' is your data frame
ggplot(customer_df, aes(x = Age, y = Spending_Score, color = Gender)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs Spending Score by Gender",
       x = "Age",
       y = "Spending Score") +
  scale_color_manual(values = c("orange","steelblue")) +
  theme_minimal()


# Assuming 'customer_df' is your data frame
ggplot(customer_df, aes(y = Spending_Score, x = Annual_Income, color = Gender)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Annual Income vs Spending Score by Gender",
       y = "Spending Score",
       x = "Annual Income") +
  scale_color_manual(values = c("orange","steelblue")) +
  theme_minimal()

# Assuming 'customer_df' is your data frame
ggplot(customer_df, aes(x = Spending_Score, y = Gender, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot showing customers' Spending Score by Gender") +
  scale_fill_manual(values = c("orange","steelblue")) +
  theme_minimal()


# Model Implementation Using K-Means Clustering

# Setting seed to 125 for reproducibility
set.seed(1)

# Using the elbow method to plot the variations of clusters 
opt <- Optimal_Clusters_KMeans(customer_df[, 3:5], max_clusters = 10, plot_clusters = T)

# Using the elbow method to get the optimal clusters 
opt <- Optimal_Clusters_KMeans(customer_df[, 3:5], max_clusters = 10, plot_clusters = T, criterion = 'silhouette')


#Creating the customer clusters with KMeans
km6<-kmeans(customer_df[,3:5], 6, iter.max = 100, nstart=50,
           algorithm = "Lloyd")

#Print the result
km6

# Define the clustering variable
cluster_variable <- km6$cluster

# Showing the six KMeans clusters
clusplot(
  customer_df,                    # Data frame
  cluster_variable,               # Clustering variable
  color = TRUE,                   # Use color for clusters
  shade = TRUE,                   # Use shading
  labels = 0,                     # Do not show point labels
  lines = 0,                      # Do not show lines
  main = "Cluster Plot of Customer Behaviour",  # Main title
  sub = paste("Number of Clusters:", max(cluster_variable)),  # Subtitle with the number of clusters
  col.p = "steelblue",            # Color for points
  col.clus = c("darkred", "green", "blue", "orange"),  # Colors for cluster ellipses
  cex = 0.8                       # Size of labels and points
)


#Perform Principal Component Analysis
pcclust<-prcomp(customer_df[, 3:5], scale=FALSE)

#Checking the summary of the PCA model
summary(pcclust)

# Applying the PCA model on the data
pcclust$rotation[, 1:2]

#Create a plot of the customers segments
ggplot(customer_df, aes(x = Annual_Income , y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(km6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")


#Create a plot of the customers segments
ggplot(customer_df, aes(x = Annual_Income , y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(km6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c( "6", "1", "4", "3", "2","5"),
                       labels=c("Low Income, High Spending", "Medium Income, Medium Spending", "Low Income, Low Spending",
                                "Medium Income, Medium Spending","High Income, Low Spending", "High Income, High Spending")) +
  labs(x="Annual Income", y="Spending Score") +
  ggtitle("Segments of Mall X Customers", 
          subtitle = "Using K-means Clustering")


# Model Implementation Using Hierarchical clustering

# Comparing the distances between the data points using ‘hclust’ function
dist_customers <- dist(customer_df[,3:5])
hc_customers <- hclust(dist_customers, method = 'complete')

# Choosing the optimal cluster
for(i in 2:7) { 
  nam <- paste("clust", i, sep = "")
  assign(nam, cutree(hc_customers, k = i))
}

par(mfrow = c(3, 2))

plot(silhouette(clust2, dist_customers), col = "blue")
plot(silhouette(clust3, dist_customers), col = 'blue')
plot(silhouette(clust4, dist_customers), col = "blue")
plot(silhouette(clust5, dist_customers), col = "blue")
plot(silhouette(clust6, dist_customers), col = "blue")
plot(silhouette(clust7, dist_customers), col = "blue")



# plot the dendrogram.
clust_customers <- cutree(hc_customers, k = 5)
dend_customers <- as.dendrogram(hc_customers)
dend_colored <- color_branches(dend_customers, k = 5)
par(mfrow = c(1, 1))
plot(dend_colored)

# Extract the clusters
km <- kmeans(customer_df[,3:4], 5)
customer_df_km$ClusterNumber <- km$cluster
segment_customers <- mutate(customer_df_km, cluster = clust_customers)
segment_customers = subset(segment_customers, select = -c(ClusterNumber))

#Create a plot of the customers segments
ggplot(segment_customers, aes(x = Annual_Income, y = Spending_Score, color = factor(cluster))) +
  geom_point() +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Low Income, Low Spending","Low Income, High Spending", "Medium Income, Medium Spending","High Income, High Spending",
                                "High Income, Low Spending")) +
  labs(x="Annual Income", y="Spending Score") +
  ggtitle("Segments of Mall X Customers", 
          subtitle = "Using Hierarchical Clustering")

# Getting the characteristcs each of the groups
segment_customers %>% group_by(cluster, Gender) %>%
  summarise_all(list(mean)) %>% arrange(cluster)



# Model Implementation Using DBSCAN clustering


# Extracting the columns to be standardized
columns_to_standardize <- c("Age", "Annual_Income", "Spending_Score")
customer_df_scaled <- customer_df

# Standardizing the selected columns in the dataframe
customer_df_scaled[, columns_to_standardize] <- scale(customer_df[, columns_to_standardize])


# Run DBSCAN
dbscan_result <- dbscan(customer_df_scaled[, c("Age", "Annual_Income", "Spending_Score")], eps = 0.5, minPts = 5)

# Access cluster assignments and noise points
clusters <- dbscan_result$cluster
noise_points <- dbscan_result$cluster == 0  # Noise points will have cluster ID 0

# Convert to a data frame for ggplot
customer_df_scaled$Cluster <- as.factor(clusters)

# Visualize clusters and noise points using ggplot
ggplot(customer_df_scaled, aes(x = Annual_Income, y = Spending_Score, color = Cluster)) +
  geom_point() +
  geom_point(data = subset(customer_df_scaled, noise_points), color = "black", size = 3) +
  labs(x = "Annual Income", y = "Spending Score", title = "DBSCAN Clustering") +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "3", "2", "4", "6","5"),
                       labels=c("Low Income, High Spending", "Medium Income, Medium Spending", "Low Income, Low Spending",
                                "Medium Income, Medium Spending","High Income, Low Spending", "High Income, High Spending")) +
  theme_minimal()



