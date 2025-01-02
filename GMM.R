#GMM (pca data)

#Gmm optimal cluster
gmm_auto <- Mclust(pca_data)

optimal_clusters_gmm <- gmm_auto$G
cat("Optimal number of clusters:", optimal_clusters_gmm, "\n")

gmm_pca <- Mclust(pca_data)
summary(gmm_pca)

bic_scaled <- gmm_scaled$BIC
bic_pca <- gmm_pca$BIC
cat("BIC for scaled data:", bic_scaled, "\n")
cat("BIC for PCA data:", bic_pca, "\n")

gmm_auto <- Mclust(pca_data)

optimal_clusters <- gmm_auto$G
cat("Optimal number of clusters:", optimal_clusters, "\n")

pca_data_frame_gmm <- as.data.frame(pca_data[, 1:3]) 
pca_data_frame_gmm$Cluster <- as.factor(gmm_pca$classification)

plot_ly(pca_data_frame_gmm, 
        x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, 
        color = ~Cluster, colors = "Set1",
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D PCA Clustering Visualization",
         scene = list(
           xaxis = list(title = "PCA Dimension 1"),
           yaxis = list(title = "PCA Dimension 2"),
           zaxis = list(title = "PCA Dimension 3")))

#Evaluation 
sil_gmm <- silhouette(gmm_pca$classification, dist(pca_data))
fviz_silhouette(sil_gmm )

davies_bouldin_gmm<- index.DB(pca_data, gmm_pca$classification)
print(davies_bouldin_gmm$DB)

calinski_harabasz_gmm <- cluster.stats(d = dist(pca_data), clustering = gmm_pca$classification)$ch
print(calinski_harabasz_gmm)

#Plot after cluster gmm

data_Cluster_gmm <- cbind(merged_data_clean, Cluster = gmm_pca$classification)

# Education 
education_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Cluster), fill = as.factor(Education_numeric))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Education in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Education") +
  theme_minimal() 

print(education_gmm)


#Family Size 
familySize_gmm <- ggplot(data_Cluster_gmm, aes(x =  as.factor(Cluster), y = Family_Size, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Family Size Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Family Size") +
  theme_minimal() 

print(familySize_gmm)

#Spending In Cluster 
spending_gmm <- ggplot(data_Cluster_gmm, aes(x =  as.factor(Cluster), y = Spending, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Spending Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Spending Score") +
  theme_minimal() 

print(spending_gmm)

#Income In cluster 
income_gmm <- ggplot(data_Cluster_gmm, aes(x =  as.factor(Cluster), y = Income, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Income Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Income") +
  theme_minimal() 

print(income_gmm)

# Age Distribution 
age_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Cluster), y = Age, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Umur Berdasarkan Cluster",
       x = "Cluster",
       y = "Age") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(10, 80, by = 10))

print(age_gmm)

#Store Purchase 
store_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Cluster), y = NumStorePurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Store Berdasarkan Cluster",
       x = "Cluster",
       y = "Store") +
  theme_minimal() 

print(store_gmm)


#Discount In Cluster 
discount_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Cluster), fill = as.factor(Discount))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Discount in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Discount") +
  theme_minimal()

print(discount_gmm)


#Web Purchase 
web_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Cluster), y = NumWebPurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Web Berdasarkan Cluster",
       x = "Cluster",
       y = "Web") +
  theme_minimal() 

print(web_gmm)

# Category Spending !!
category_spending <- c("MntWines", "MntFruits", "MntMeatProducts", 
                       "MntFishProducts", "MntSweetProducts", "MntGoldProds")

category_Analysis <-aggregate(cbind(Age, MntWines, MntFruits, MntMeatProducts, 
                                    MntFishProducts, MntSweetProducts, MntGoldProds) ~ Cluster, 
                              data = data_Cluster, 
                              FUN = mean)

for (col in category_spending) {
  category_spending_gmm <- ggplot(data_Cluster_gmm, aes_string(x = "as.factor(Cluster)", y = col, fill = "as.factor(Cluster)")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col, "by Cluster"),
         x = "Cluster 1 Cluster 2  Cluster 3 ",
         y = col) +
    theme_minimal()
  print(category_spending_gmm)
}

# Total Spending - Income !!
spending_income_gmm <- ggplot(data_Cluster_gmm, aes(x = Spending, y = Income, color = as.factor(Cluster))) +
  geom_point(alpha = 0.7) + 
  labs(title = "Hubungan antara Income dan Total Spending Berdasarkan Cluster",
       x = "Income",
       y = "Spending",
       color = "Cluster") +
  theme_minimal()

print(spending_income_gmm)


# Total Spending - Diskon !!
spending_discount_gmm <- ggplot(data_Cluster_gmm, aes(x = as.factor(Discount), y = Spending, fill = as.factor(Discount))) +
  geom_boxplot() +
  labs(title = "Distribusi Total Spending Berdasarkan Diskon",
       x = "Discount (1 = Yes, 0 = No)",
       y = "Total Spending") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "orange"))

print(spending_discount_gmm)
