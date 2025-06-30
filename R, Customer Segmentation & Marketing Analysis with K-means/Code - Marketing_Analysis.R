# 🟦 Customer Marketing Analytics - R Script
# Performs customer segmentation using K-means clustering

## 1. Load Required Libraries ----
# Install if not already (run once)
# install.packages(c("readxl", "dplyr", "ggplot2", "factoextra", "cluster", "scales", "forcats"))

library(readxl)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(scales)
library(forcats) # For fct_infreq()

## 2. Import and Explore the Data ----

# Load dataset
data <- read_excel("Customer_marketing_dataset.xlsx")

# Explore data
cat("\n🟦 First few rows:\n")
print(head(data, 5))
cat("\n🟦 Structure:\n")
str(data)
cat("\n🟦 Summary stats:\n")
summary(data)

## 3. Data Cleaning ----

# Check missing values
cat("\n🟦 Missing values per column:\n")
print(colSums(is.na(data)))

# Impute missing Income with median
if(any(is.na(data$Income))) {
  data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)
}

# Convert categorical columns to factors
data$Education <- as.factor(data$Education)
data$Marital_Status <- as.factor(data$Marital_Status)
data$Complain <- factor(data$Complain, levels = c(0,1), labels = c("No", "Yes"))
data$Response <- factor(data$Response, levels = c(0,1), labels = c("No", "Yes"))

cat("\n🟦 After cleaning - missing values:\n")
print(colSums(is.na(data)))

## 4. Feature Engineering ----

# Age
data$Age <- as.numeric(format(Sys.Date(), "%Y")) - data$Year_Birth

# Family Size
data$Family_Size <- data$Kidhome + data$Teenhome

# Total Spending (Wines, Fruits, Meat, Fish, Sweet)
data$Total_Spent <- data$MntWines + data$MntFruits + data$MntMeatProducts +
  data$MntFishProducts + data$MntSweetProducts

# Total Purchases (Web, Catalog, Store)
data$Total_Purchases <- data$NumWebPurchases + data$NumCatalogPurchases + data$NumStorePurchases

# Years with company (may require date format fix if fails)
data$Customer_Since_Years <- as.numeric(difftime(Sys.Date(),
                                                 as.Date(data$Dt_Customer, format = "%d-%m-%Y"),
                                                 units = "days")) / 365

## 5. Descriptive Statistics & Visualizations ----

# Nice theme for all plots
my_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank()
  )
theme_set(my_theme)

# Age distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#209CEE", color = "white", alpha = 0.9) +
  labs(title = "Customer Age Distribution", subtitle = "Distribution of customer ages", x = "Age", y = "Count")

# Income distribution - with x-axis fixed: 0-200,000, ticks every 50k, comma labels
ggplot(data[is.finite(data$Income), ], aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "#23D160", color = "white", alpha = 0.9) +
  labs(
    title = "Customer Income Distribution", 
    subtitle = "Distribution of incomes", 
    x = "Income", 
    y = "Count"
  ) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    breaks = seq(0, 200000, by = 50000), 
    limits = c(0, 200000)
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

# Education level
ggplot(data, aes(x = fct_infreq(Education), fill = Education)) +
  geom_bar(alpha = 0.9, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(title = "Customer Education Levels", subtitle = "Education distribution", x = "Education Level", y = "Count")

# Marital status
ggplot(data, aes(x = fct_infreq(Marital_Status), fill = Marital_Status)) +
  geom_bar(alpha = 0.9, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title = "Customer Marital Status", subtitle = "Marital status distribution", x = "Marital Status", y = "Count")

# Total spending (with pretty axis)
ggplot(data[is.finite(data$Total_Spent), ], aes(x = Total_Spent)) +
  geom_histogram(binwidth = 100, fill = "#FF3860", color = "white", alpha = 0.9) +
  labs(title = "Total Customer Spending", subtitle = "Distribution of total spending", x = "Total Spending", y = "Count") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

# Response rate
ggplot(data, aes(x = Response, fill = Response)) +
  geom_bar(alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values = c("#FFDD57", "#3273DC")) +
  labs(title = "Customer Campaign Response", subtitle = "How many responded to campaign", x = "Response", y = "Count") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

## 6. Customer Segmentation (K-means Clustering) ----

# Prepare features
clust_data <- data %>%
  select(Age, Income, Total_Spent, Total_Purchases)

# Scale for K-means
clust_data_scaled <- scale(clust_data)

# Elbow plot to find optimal clusters
elbow <- fviz_nbclust(clust_data_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Optimal Number of Clusters (Elbow Method)", subtitle = "Look for the 'elbow' point") +
  my_theme
print(elbow)

# Run K-means (pick 4 clusters based on elbow)
set.seed(123)
kmeans_res <- kmeans(clust_data_scaled, centers = 4, nstart = 25)
data$Segment <- as.factor(kmeans_res$cluster)

# Visualize clusters
cluster_vis <- fviz_cluster(kmeans_res, data = clust_data_scaled,
                            palette = "Set2", ellipse.type = "norm", geom = "point", ggtheme = my_theme) +
  labs(title = "Customer Segments Visualization") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))  # adjust these numbers if needed

print(cluster_vis)


## 7. Segment Profiling ----

# Segment summary table
segment_summary <- data %>%
  group_by(Segment) %>%
  summarise(
    Count = n(),
    Percent = round(n()/nrow(data)*100, 1),
    Avg_Age = round(mean(Age), 1),
    Avg_Income = round(mean(Income), 0),
    Avg_Spent = round(mean(Total_Spent), 0),
    Avg_Purchases = round(mean(Total_Purchases), 1),
    Avg_Family_Size = round(mean(Family_Size), 1)
  ) %>%
  arrange(desc(Avg_Spent))
cat("\n🟦 Segment Profiles:\n")
print(segment_summary)

# Boxplots: Age by segment
ggplot(data, aes(x = Segment, y = Age, fill = Segment)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Age by Segment", y = "Age", x = "Segment") +
  my_theme

# Boxplots: Income by segment
ggplot(data, aes(x = Segment, y = Income, fill = Segment)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Income by Segment", y = "Income", x = "Segment") +
  my_theme

# Boxplots: Total Spent by segment
ggplot(data, aes(x = Segment, y = Total_Spent, fill = Segment)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Total Spending by Segment", y = "Total Spent", x = "Segment") +
  my_theme

## 8. Save the Results ----

write.csv(data, "Customer_Segments_Output.csv", row.names = FALSE)
write.csv(segment_summary, "Segment_Summary.csv", row.names = FALSE)

cat("\n🟦 Analysis complete! Results saved as:\n")
cat(" - Customer_Segments_Output.csv\n")
cat(" - Segment_Summary.csv\n")
