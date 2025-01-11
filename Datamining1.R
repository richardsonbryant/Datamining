library(dplyr)
library(cluster)
library(clusterSim)
library(corrplot)
library(data.table)
library(dendextend)
library(dplyr)
library(evaluate)
library(factoextra)
library(FactoMineR)
library(flashClust)
library(grid)
library(gridExtra)
library(mclust)
library(plotly)
library(reshape2)
library(cluster)
library(fpc)
library(scales)

#Data merge and load 

primary_data <- read.csv("customer_segmentation 2.csv", header = TRUE )
support1_data <- read.csv("project1_df 2.csv", header = TRUE)

primary_data$Age <- 2014 - primary_data$Year_Birth


NA_count_primary <- colSums(is.na(primary_data)) #Have NA

Duplicated_primary <- primary_data[duplicated(primary_data), ] #No Duplicate

NA_count_support1 <- colSums(is.na(support1_data)) #No NA

Duplicated_support1 <- support1_data[duplicated(support1_data), ] #No Duplicate

primary_data_clean <- na.omit(primary_data) #Delete NA

support1_data <- support1_data %>%
  mutate( id = substr(as.character(CID), 1, 4)) 

primary_data_clean <- primary_data_clean %>%
  rename(id = ID)

support_data_select <- support1_data %>% 
  select(Discount.Availed, id)

support_data_select <- support_data_select %>%
  distinct(id, .keep_all = TRUE) #Select Unique

merged_data <- merge(primary_data_clean, support_data_select, by = "id", all.x = TRUE)

merged_data_clean <- na.omit(merged_data)

#Data Preprocessing 
sapply(merged_data_clean, typeof)

# Total Spending
merged_data_clean <- merged_data_clean %>%
  mutate(Spending = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)

# Change Martial Status to 2 Category Partner or Single
merged_data_clean <- merged_data_clean %>%
  mutate(Status = case_when(
    Marital_Status %in% c("Married", "Together") ~ "Partner",
    Marital_Status %in% c("Absurd", "Widow", "YOLO", "Divorced", "Single", "Alone") ~ "Single",
    TRUE ~ NA_character_  # Handles unexpected cases
  ))

# Count Child In Home 
merged_data_clean <- merged_data_clean %>%
  mutate(Child = Kidhome + Teenhome)

# Change relation status to Integer 
merged_data_clean <- merged_data_clean %>%
  mutate(
    Status = case_when(
      Status == "Single" ~ 1,
      Status == "Partner" ~ 2
    ))

# Count Family Size
merged_data_clean <- merged_data_clean %>%
  mutate(Family_Size = Status + Child)

# Change Education to only 3 Category
merged_data_clean <- merged_data_clean %>%
  mutate(Education = case_when(
    Education == "Basic" ~ "Undergraduate",
    Education == "2n Cycle" ~ "Undergraduate",
    Education == "Graduation" ~ "Graduate",
    Education == "Master" ~ "Postgraduate",
    Education == "PhD" ~ "Postgraduate",
    TRUE ~ Education  # Keeps other values unchanged, if any
  ))

# Change Education to numeric for cluster model
merged_data_clean <- merged_data_clean %>%
  mutate(Education_numeric = case_when(
    Education == "Undergraduate" ~ 1,
    Education == "Graduate" ~ 2,
    Education == "Postgraduate" ~ 3
  ))

# Change Column Name Discount and Purchase Method
merged_data_clean <- merged_data_clean %>%
  rename(Discount = Discount.Availed)

# Change Discount to Numeric 
merged_data_clean <- merged_data_clean %>%
  mutate(
    Discount = case_when(
      Discount == "No" ~ 0,
      Discount == "Yes" ~ 1
    ))

colnames(merged_data_clean)

# Drop Useless Column
merged_data_clean <- merged_data_clean %>% select(-c(Year_Birth, id, Z_CostContact, 
                                                    Z_Revenue, AcceptedCmp1,  AcceptedCmp2,  AcceptedCmp4,
                                                    AcceptedCmp5,  AcceptedCmp3, Dt_Customer,Kidhome, Teenhome
                                                    ,Response, Complain, Recency, NumDealsPurchases, NumCatalogPurchases, 
                                                    NumWebVisitsMonth))

# Income
distribution_income <- ggplot(merged_data_clean, aes(x = Income)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Income", x = "Income", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_income)


# Spending category Distribution 
spending_to_plot <- c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")

spending_subset <- merged_data_clean[ , spending_to_plot]

spending_long <- pivot_longer(spending_subset, 
                          cols = everything(), 
                          names_to = "Category", 
                          values_to = "Value")

spending_plot <- ggplot(spending_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(length(unique(spending_long$Category)))) +
  theme_minimal() +
  labs(title = "Distribution Of Product Category", x = "Category", y = "Value") + 
  scale_y_continuous(labels = label_number())  

print(spending_plot)

# Distribution Of Number Web Purchase 
distribution_NumWeb <- ggplot(merged_data_clean, aes(x = NumWebPurchases)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of number web purchase", x = "NumWeb", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_NumWeb)

# Distribution Of Number Store Purchase 
distribution_NumStore <- ggplot(merged_data_clean, aes(x = NumStorePurchases)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of number store purchase", x = "NumStore", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_NumStore)


# Distribution Of Age
distribution_Age <- ggplot(merged_data_clean, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_Age)

# Distribution Of Spending
distribution_Spending <- ggplot(merged_data_clean, aes(x = Spending)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Spending", x = "Spending", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_Spending)

# Distribution Of Child
distribution_Child <- ggplot(merged_data_clean, aes(x = Child)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Child", x = "Child", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_Child)

#Distribution Of Family Size 
distribution_FamilySize<- ggplot(merged_data_clean, aes(x = Family_Size)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Family Size ", x = "Family Size", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_FamilySize)

#Distribution Of Education
education_table <- table(merged_data_clean$Education)

education_df <- as.data.frame(education_table)
colnames(education_df) <- c("Education", "Count") 

# Generate the plot using the data frame
distribution_Education <- ggplot(education_df, aes(x = Education, y = Count, fill = Education)) +
  geom_bar(stat = "identity", color = "black") +  
  scale_fill_manual(values = rainbow(length(unique(education_df$Education)))) +  
  labs(title = "Distribution of Education Level", x = "Education", y = "Count") +
  theme_minimal()

print(distribution_Education)

#Distribution Mnt Wines 
distribution_mntWines <- ggplot(merged_data_clean, aes(x = MntWines)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Wines", x = "Mnt Wines", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_mntWines)

#Distribution Mnt Fruits
distribution_mntFruits <- ggplot(merged_data_clean, aes(x = MntFruits)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Fruit", x = "Mnt Fruit", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_mntFruits)

#Distribution Mnt Meat
distribution_mntMeat <- ggplot(merged_data_clean, aes(x = MntMeatProducts)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Meat", x = "Mnt Meat Product", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number())  

print(distribution_mntMeat)

#Distribution Mnt Fish
distribution_mntFish <- ggplot(merged_data_clean, aes(x = MntFishProducts)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Fish", x = "Mnt Fish Product", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number()) 

print(distribution_mntFish)


#Distribution Mnt Sweet
distribution_mntSweet <- ggplot(merged_data_clean, aes(x = MntSweetProducts)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Sweet", x = "Mnt Sweet Product", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number()) 

print(distribution_mntSweet)

#Distribution Mnt Gold
distribution_mntGold <- ggplot(merged_data_clean, aes(x = MntGoldProds)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Mnt Gold", x = "Mnt Gold Product", y = "Count") +
  theme_minimal() +
  scale_x_continuous(labels = label_number()) +  
  scale_y_continuous(labels = label_number()) 

print(distribution_mntGold)


#Function To Check Outlier 
count_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  print(paste("Lower bound:", lower_bound))
  print(paste("Upper bound:", upper_bound))
  sum(x < lower_bound | x > upper_bound)  
}

#Age outlier
Age_outlier <- apply(as.matrix(merged_data_clean$Age), 2, count_outliers_iqr)
print(Age_outlier) #10  82 | 2

merged_data_clean <- merged_data_clean[merged_data_clean$Age >= 10 & merged_data_clean$Age <= 82, ]

#Income Outlier 
Income_outlier <- apply(as.matrix(merged_data_clean$Income), 2, count_outliers_iqr)
print(Income_outlier) #-14998.125  119524.875 | 7

merged_data_clean <- merged_data_clean[merged_data_clean$Income >= -14998.125 & merged_data_clean$Income <= 119524.875, ]

#NumStore
NumStore_outlier <- apply(as.matrix(merged_data_clean$NumStorePurchases), 2, count_outliers_iqr)
print(NumStore_outlier) #-4.5  15.5  | 0 (No Outlier)


#Num Web Purchase 
NumWeb_outlier <- apply(as.matrix(merged_data_clean$NumWebPurchases), 2, count_outliers_iqr)
print(NumWeb_outlier) #-4   12  |  2

merged_data_clean$NumWebPurchases[merged_data_clean$NumWebPurchases < -4 | 
  merged_data_clean$NumCatalogPurchases > 12] <- median(merged_data_clean$NumWebPurchases, na.rm = TRUE)

#Spending
Spending_outlier <- apply(as.matrix(merged_data_clean$Spending), 2, count_outliers_iqr)
print(Spending_outlier) #-1419.25  2554.75  |  0 (No Outlier)

colnames(merged_data_clean)


write.csv(merged_data_clean, "Data Merged Clean.csv", row.names = FALSE)


#Make A heatmap
numeric_cols <- c("Income", "MntWines", "MntFruits", "MntMeatProducts", 
                  "MntFishProducts", "MntSweetProducts", "MntGoldProds" 
                  ,"NumWebPurchases", "NumStorePurchases", 
                  "Age", "Spending","Discount","Family_Size","Education_numeric", "Recency")

final_Data_cluster <- merged_data_clean[, numeric_cols]

corrmax <- cor(final_Data_cluster)

# Melt the correlation matrix into long format
corr_melted <- melt(corrmax)

heatmap_plot <- ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +  # Coolwarm color map
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +  # Add numbers inside each box
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", fill = "Correlation Value")

ggsave("heatmap_ggplot2.png", plot = heatmap_plot, width = 25, height = 20, units = "in")




#Select Coloumn Numeric for clustering Model
numeric_colss <- c("Income", "MntWines", "MntFruits", "MntMeatProducts", 
                   "MntFishProducts", "MntSweetProducts", "MntGoldProds", 
                   "NumWebPurchases", "NumStorePurchases", "Age", "Spending","Discount",
                   "Family_Size","Education_numeric")


data_Cluster <- merged_data_clean[, numeric_colss]


scale_data <- scale(data_Cluster)


fviz_nbclust(scale_data, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2)

pca_result <- PCA(scale_data, ncp = 10, graph = FALSE) 
pca_data <- pca_result$ind$coord

kmeans_model <- kmeans(pca_data, centers = 3, nstart = 25)

fviz_cluster(kmeans_model, data = pca_data)

data_Cluster$Cluster <- kmeans_model$cluster

pca_data_frame <- as.data.frame(pca_data)
pca_data_frame$Cluster <- as.factor(kmeans_model$cluster)


# Create a scatterplot
plot_ly(pca_data_frame, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, 
        color = ~Cluster, colors = "Set1", type = "scatter3d", mode = "markers") %>%
  layout(title = "3D PCA Clustering",
         scene = list(xaxis = list(title = "PC1"),
                      yaxis = list(title = "PC2"),
                      zaxis = list(title = "PC3")))


#Evaluation

davies_bouldin_kmeans <- index.DB(pca_data, kmeans_model$cluster, centrotypes = "centroids")
print(davies_bouldin_kmeans$DB)

sil_kmeans <- silhouette(kmeans_model$cluster, dist(pca_data))
fviz_silhouette(sil_kmeans)

calinski_harabasz_kmeans <- cluster.stats(d = dist(pca_data), clustering = kmeans_model$cluster)$ch
print(calinski_harabasz_kmeans)


#Plot After Cluster 


# Education 
education_kmeans <- ggplot(data_Cluster, aes(x = as.factor(Cluster), fill = as.factor(Education_numeric))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Education in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Education Level") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

print(education_kmeans)


#Family Size 
familySize_kmeans <- ggplot(data_Cluster, aes(x =  as.factor(Cluster), y = Family_Size, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Family Size Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Family Size") +
  theme_minimal() 

print(familySize_kmeans)

#Spending In Cluster 
spending_kmeans <- ggplot(data_Cluster, aes(x =  as.factor(Cluster), y = Spending, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Spending Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Spending Score") +
  theme_minimal() 

print(spending_kmeans)

#Income In cluster 
income_kmeans <- ggplot(data_Cluster, aes(x =  as.factor(Cluster), y = Income, fill = as.factor(Cluster))) +
  geom_boxplot() +
  ggtitle("Income Distribution in Clusters") +
  xlab("Cluster") +
  ylab("Income") +
  theme_minimal() 

print(income_kmeans)

# Age Distribution 
age_kmeans <- ggplot(data_Cluster, aes(x = as.factor(Cluster), y = Age, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Umur Berdasarkan Cluster",
       x = "Cluster",
       y = "Age") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(10, 80, by = 10))

print(age_kmeans)

#Store Purchase 
store_kmeans <- ggplot(data_Cluster, aes(x = as.factor(Cluster), y = NumStorePurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Store Berdasarkan Cluster",
       x = "Cluster",
       y = "Store") +
  theme_minimal() 

print(store_kmeans)


#Discount In Cluster 
discount_kmeans <- ggplot(data_Cluster, aes(x = as.factor(Cluster), fill = as.factor(Discount))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Discount in Each Cluster",
       x = "Cluster",
       y = "Count",
       fill = "Discount") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

print(discount_kmeans)


#Web Purchase 
web_kmeans <- ggplot(data_Cluster, aes(x = as.factor(Cluster), y = NumWebPurchases, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribusi Pembelian Web Berdasarkan Cluster",
       x = "Cluster",
       y = "Web") +
  theme_minimal()

print(web_kmeans)

# Category Spending !!
category_spending <- c("MntWines", "MntFruits", "MntMeatProducts", 
                       "MntFishProducts", "MntSweetProducts", "MntGoldProds")

category_Analysis <-aggregate(cbind(Age, MntWines, MntFruits, MntMeatProducts, 
                                    MntFishProducts, MntSweetProducts, MntGoldProds) ~ Cluster, 
                              data = data_Cluster, 
                              FUN = mean)

for (col in category_spending) {
  category_spending_Kmeans <- ggplot(data_Cluster, aes_string(x = "as.factor(Cluster)", y = col, fill = "as.factor(Cluster)")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col, "by Cluster"),
         x = "Cluster 1 Cluster 2  Cluster 3 ",
         y = col) +
    theme_minimal()
  print(category_spending_Kmeans)
}

# Total Spending - Income !!
spending_income_Kmeans <- ggplot(data_Cluster, aes(x = Spending, y = Income, color = as.factor(Cluster))) +
  geom_point(alpha = 0.7) + 
  labs(title = "Hubungan antara Income dan Total Spending Berdasarkan Cluster",
       x = "Income",
       y = "Spending",
       color = "Cluster") +
  theme_minimal()

print(spending_income_Kmeans)


# Total Spending - Diskon !!
spending_discount_Kmeans <- ggplot(data_Cluster, aes(x = as.factor(Discount), y = Spending, fill = as.factor(Discount))) +
  geom_boxplot() +
  labs(title = "Distribusi Total Spending Berdasarkan Diskon",
       x = "Discount (1 = Yes, 0 = No)",
       y = "Total Spending") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "orange"))

print(spending_discount_Kmeans)






