## Classification and Cluster Analysis
## Online Retail Customer Segmentation based on RFM(Recency, Frequency, Monetary)
#
#
## Definition of Terms:
#---- Recency: How recently a customer has made a purchase
#---- Frequency: The number of times a customer made transaction 
#---- Monetary: Total amount spent by a customer
#
#
## Data Source: https://archive.ics.uci.edu/ml/datasets/online+retail
#
#
## Author: Latifat Ajibola
## Setup

require(tidyverse)
require(DataExplorer)
require(lubridate)
require(rfm)
require(GGally)
require(NbClust)
require(factoextra)
require(flexclust)

rm(list = ls())
## Load Data set

setwd("/Users/lateefah/Downloads/")
retail_df <- read_csv("Online Retail.csv")
retail_df %>% head()
retail_df %>% spec() ## check column specification
glimpse(retail_df)  ## check data structure
## Check for missing observations
colSums(is.na(retail_df))
## Dropping the missing observations in Description and customer ID
retail_df <- retail_df %>% 
  drop_na(Description) %>% 
  drop_na(CustomerID)  ## Simply use dro_na()
## Filter out Cancellations
retail_df <- retail_df %>%
  filter(!str_detect(InvoiceNo, 'C'))


## Get summary of  numeric variables
retail_df %>%
  select(Quantity, UnitPrice) %>% summary()
## Some unit prices are o, but we will leave them
## Data Preparation
## Convert Invoice Date to Datetime
retail_df <-
  retail_df %>% 
  mutate(InvoiceDate = mdy_hm(InvoiceDate))
## Get a total cost variable
last_transaction_date <- max(retail_df$InvoiceDate)

retail_df <-
  retail_df %>% 
  mutate(TotalSales = UnitPrice*Quantity, 
         dulp = difftime(last_transaction_date, retail_df$InvoiceDate, units = 'days')) ## dulp = days until last purchase

R <-
  retail_df %>% 
  group_by(CustomerID) %>% 
  summarise(Recency = min(dulp))

Fr <- 
  retail_df %>% 
  group_by(CustomerID) %>% 
  summarise(Frequency = n())

M <-
  retail_df %>% 
  group_by(CustomerID) %>% 
  summarise(Monetary = sum(TotalSales))

RFM_data <-
  inner_join(R, Fr, by = 'CustomerID') %>% 
  inner_join(., M, by = 'CustomerID')
## Or, You can Simply use the rfm package in R as below:
rfm_result <- rfm_table_order(retail_df, CustomerID, InvoiceDate, TotalSales, last_transaction_date)
rfm_result
## Convert Recency to numeric
RFM_data <- 
  mutate_at(RFM_data, vars(Recency), as.numeric)
## Round Recency to 2 dp
RFM_data <-
  mutate_at(RFM_data, vars(Recency), round, 2)
## Data Exploration
RFM_data %>% glimpse()
#create_report(RFM_data %>%  select(-CustomerID))
plot_str(RFM_data %>%  select(-CustomerID))
introduce(RFM_data %>%  select(-CustomerID))
plot_missing(RFM_data %>%  select(-CustomerID)) 
plot_intro(RFM_data %>%  select(-CustomerID))
plot_histogram(RFM_data %>%  select(-CustomerID))
plot_correlation(RFM_data %>%  select(-CustomerID))
ggpairs(RFM_data %>%  select(-CustomerID)) + theme_bw()
## KMeans Clustering
## Scale Cluster Data
RFM <- 
  RFM_data %>% select(-CustomerID) %>% 
  scale()
## Select Optimal Cluster
fviz_nbclust(RFM, kmeans, method = 'wss', k.max = 15)  ## Looks like elbow is at 3
fviz_nbclust(RFM, kmeans, method = 'silhouette', k.max = 15) ## Using silhouette
## Using 23 different approaches
n_clust <- NbClust(RFM, min.nc=2, max.nc=15, method="kmeans") 
fviz_nbclust(n_clust)
## Based on Majority Vote, 3 is the optimal number of cluster
set.seed(2021)
km <- kmeans(RFM, 3, nstart=25)
km$size
fviz_cluster(km, data=RFM, geom = "point") + ggtitle("k = 3")
## Some Plotting
means <- as.data.frame(km$centers) 
means$cluster <- 1:nrow(means) 
plotdata <- gather(means, key="variable", value="value", -cluster) 
ggplot(plotdata, 
       aes(x=variable,
           y=value,
           fill=variable,
           group=cluster)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0),
        legend.position="none") +
  labs(x="", y="Standardized scores",
       title = "Mean Cluster Profiles")
## We chose standardize so that we can see the distribution better
aggregate(RFM_data %>% select(-CustomerID), by=list(cluster=km$cluster), mean)
## Customers in cluster 3 turns out to be the most important. High frequency, more recent buyers, and heavy spender. 
## Next in line is customers in Cluster 1, and finally customers in Cluster 2.
## Hence, three customer segment is evident:
#*--- Heavy Spenders ==> cluster 2
#*--- Medium Spender ==> Cluster 1
#*--- Low Spenders ==> Cluster 2
##### -------------------------------####
