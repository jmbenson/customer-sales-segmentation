###############################################
# Sales segmentation with k-means clustering
###############################################

# load packages 
library(data.table)
library(ggplot2)
library(cluster)

# load in sales data downloaded from Kaggle
sales <- fread("C:/Users/Jennifer/Desktop/business_portfolio/sales_data_sample.csv")

# explore the data (i.e. look for NAs, )
clean_sales <- na.omit(sales) # 1074 observations have been emitted due to incomplete information

# Unique customers? Unique orders?
length(unique(clean_sales$ORDERNUMBER)) # 188 unique orders
length(unique(clean_sales$CUSTOMERNAME)) # 54 unique customers

# data is organized long; need customer level dataset with monetary value
# per product line for each customer

# create customer-level dataset
customers <- as.data.table(unique(clean_sales$CUSTOMERNAME))
names(customers) <- "customer.name"

# frequency: need to get customer orders per year and merge back with dataset
#invoice_yearly <- clean_sales[,.(invoice.year = length(ORDERNUMBER)/length(unique(YEAR_ID))),by="CUSTOMERNAME"]
#customers <- merge(customers, invoice_yearly, by.x = "customer.name", by.y = "CUSTOMERNAME")

# monetary value of each customer by product line/year
sales_product <- clean_sales[,.(total.ordered = sum(QUANTITYORDERED)/length(unique(YEAR_ID))),by=.(CUSTOMERNAME,PRODUCTLINE)]

# now need to switch into wide format so one column per product type
sales_product <- dcast(sales_product, CUSTOMERNAME ~ PRODUCTLINE)
sales_product[is.na(sales_product)] <- 0 # replace all NAs with 0

# need to scale the quantity data; unadjusted quantities presents a problem to the k-means algorithm. 
# some customers are larger than others so they purchase higher volumes. 
# Convert customer purchase quantity to percentage of total quantity -----------
sales_mat <- as.matrix(sales_product[,-1])  # Drop first five columns
sales_mat <- prop.table(sales_mat, margin = 1)  # column-wise pct
sales_product <- dplyr::bind_cols(sales_product[,1], as.data.table(sales_mat))

# merge back with customer names now
customers <- merge(customers, sales_product, by.x = "customer.name", by.y = "CUSTOMERNAME")

# perform clustering here
customers_cluster_product <- kmeans(as.matrix(customers[,-1]), centers = 3)

# display averages for each numeric variable 
customers_cluster_product$centers

# table of customers within each cluster
table(customers_cluster_product$cluster)

# iterate testing different cluster amounts
kmeans_test <- customers[,-1]  # Extract only customer columns
kmeans_test_t <- t(kmeans_test)  # Get customers in rows and products in columns

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 4    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(kmeans_test_t, centers = centr))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeans_test_t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

# pick clusters with largest silhoueet average width - 2
customers_cluster_product <- kmeans(as.matrix(customers[,-1]), centers = 2)

# display averages for each numeric variable 
customers_cluster_product$centers

# table of customers within each cluster
table(customers_cluster_product$cluster)

# CLUSTER 1: arguably evenly split between classic cars, planes, ships, trucks and 
# buses and vintage cars

# CLUSTER 2: mostly classic cars followed by motorcycles, trucks and buses and vintage cars

# segment customers into the groups now
customer_segmented <- as.data.table(dplyr::bind_cols(customers, as.data.frame(customers_cluster_product$cluster)))

# rename cluster column
setnames(customer_segmented, "customers_cluster_product$cluster", "cluster")


