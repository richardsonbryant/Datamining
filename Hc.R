#Hierarchical Clustering
# Euclidean distance matrix
dist_pca <- dist(pca_data) 

# Perform hierarchical clustering using Ward's method
hc_pca <- hclust(dist_pca, method = "ward.D2")

# Visualize the dendrogram
plot(hc_pca, main = "Dendrogram (PCA Data)", xlab = "", sub = "", cex = 0.6)

# Cut the dendrogram to form clusters (e.g., 3 clusters)
clusters_hc_pca <- cutree(hc_pca, k = 3)


# Visualize the clusters on the PCA space
fviz_cluster(list(data = pca_data, cluster = clusters_hc_pca),
             geom = "point", ellipse.type = "convex",
             main = "Clusters (PCA Data)")

plot_ly(pca_data_frame_hc, 
        x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, 
        color = ~Cluster, colors = "Set1",
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D PCA Clustering Visualization",
         scene = list(
           xaxis = list(title = "PCA Dimension 1"),
           yaxis = list(title = "PCA Dimension 2"),
           zaxis = list(title = "PCA Dimension 3")))

#Evaluation
sil_hc <- silhouette(clusters_hc_pca, dist_pca)
fviz_silhouette(sil_hc)

davies_bouldin_hc <- index.DB(pca_data, clusters_hc_pca)
print(davies_bouldin_hc$DB)

calinski_harabasz_hc <- cluster.stats(d = dist_pca, clustering = clusters_hc_pca)$ch
print(calinski_harabasz_hc)

#Plot After Cluster 

data_Cluster_hc <- cbind(merged_data_clean, Cluster = as.factor(clusters_hc_pca))


# Education 
education_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Cluster), fill = as.factor(Education_numeric))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Education in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Education") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

print(education_hc)


#Family Size 
familySize_hc <- ggplot(data_Cluster_hc, aes(x =  as.factor(Cluster), y = Family_Size, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Family Size Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Family Size") +
  theme_minimal() 

print(familySize_hc)

#Spending In Cluster 
spending_hc <- ggplot(data_Cluster_hc, aes(x =  as.factor(Cluster), y = Spending, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Spending Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Spending Score") +
  theme_minimal() 

print(spending_hc)

#Income In cluster 
income_hc <- ggplot(data_Cluster_hc, aes(x =  as.factor(Cluster), y = Income, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Income Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Income") +
  theme_minimal() 

print(income_hc)

# Age Distribution 
age_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Cluster), y = Age, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Umur Berdasarkan Cluster",
       x = "Cluster",
       y = "Age") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(10, 80, by = 10))

print(age_hc)

#Store Purchase 
store_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Cluster), y = NumStorePurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Store Berdasarkan Cluster",
       x = "Cluster",
       y = "Store") +
  theme_minimal() 

print(store_hc)


#Discount In Cluster 
discount_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Cluster), fill = as.factor(Discount))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Discount in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Discount") +
  theme_minimal() 

print(discount_hc)


#Web Purchase 
web_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Cluster), y = NumWebPurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Web Berdasarkan Cluster",
       x = "Cluster",
       y = "Web") +
  theme_minimal() 

print(web_hc)

# Category Spending !!
category_spending <- c("MntWines", "MntFruits", "MntMeatProducts", 
                       "MntFishProducts", "MntSweetProducts", "MntGoldProds")

category_Analysis <-aggregate(cbind(Age, MntWines, MntFruits, MntMeatProducts, 
                                    MntFishProducts, MntSweetProducts, MntGoldProds) ~ Cluster, 
                              data = data_Cluster, 
                              FUN = mean)

for (col in category_spending) {
  category_spending_hc <- ggplot(data_Cluster_hc, aes_string(x = "as.factor(Cluster)", y = col, fill = "as.factor(Cluster)")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col, "by Cluster"),
         x = "Cluster 1 Cluster 2  Cluster 3 ",
         y = col) +
    theme_minimal()
  print(category_spending_hc)
}

# Total Spending - Income !!
spending_income_hc <- ggplot(data_Cluster_hc, aes(x = Spending, y = Income, color = as.factor(Cluster))) +
  geom_point(alpha = 0.7) + 
  labs(title = "Hubungan antara Income dan Total Spending Berdasarkan Cluster",
       x = "Income",
       y = "Spending",
       color = "Cluster") +
  theme_minimal()

print(spending_income_hc)


# Total Spending - Diskon !!
spending_discount_hc <- ggplot(data_Cluster_hc, aes(x = as.factor(Discount), y = Spending, fill = as.factor(Discount))) +
  geom_boxplot() +
  labs(title = "Distribusi Total Spending Berdasarkan Diskon",
       x = "Discount (1 = Yes, 0 = No)",
       y = "Total Spending") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "orange"))

print(spending_discount_hc)
