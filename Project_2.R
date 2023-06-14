# Imports
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)

# Function to load data from the Excel spreadsheet (Concatenating the Two Tabs)
carrega_dados <- function()
{
  setwd('/Users//BigData/')
  sheet1 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2009-2010')
  sheet2 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2010-2011')
  dados_combinados <- rbind(sheet1, sheet2)
  return(dados_combinados)
}

# Run the Function
dados <- carrega_dados()
dim(dados)
View(dados)

# Function to check missing values
verifica_missing <- function(x)
{
  return(colSums(is.na(x)))
}

# Run the Function
verifica_missing(dados)

# Based on the function above, it was possible to detect that the column "Description" has
# about 4K missing values compared to the total rows of the 1M dataframe,
# the most viable solution is to basically erase those 4K of missing values.

# Sequentially, column "Customer ID" has a number of missing values
# very high, about 243K. Thus, a solution would be Imputation (calculate
# the mean/mode/median of all values in the column, and thereby replace the values
# missing by this resulting new value). However, it doesn't make much sense in this case,
# as we would be creating sales records for customers we are not sure of
# if they actually made the purchase. Just because a customer bought a large amount of
# products, does not mean that he bought that other certain product (absent).
# So let's just delete the missing values from that column (243K).

# It is extremely important to justify your choice.

# Let's just delete the records with missing values
# Function to clean and pre-process the data
preprocessa_dados <- function(data1)
{
  # Creating a column named TotalPrice
  data1$TotalPrice <- data1$Quantity * data1$Price
  # Remove records with missing values
  data1 <- na.omit(data1)
  # We remove the rows from the Invoice column that contain the letter C (meaning that this order has been cancelled)
  # This information will not be used, because in practice there was a purchase, but it was
  # Cancelled. As a result, these lines are irrelevant to the final product of the solution.
  # If we included all canceled orders, maybe we would have a result,
  # with wrong information running away from the reality of the solution.
  data1 <- data1[!grepl("C",data1$Invoice),]
  return(data1)
}

# Run the function
dataset <- preprocessa_dados(dados)
View(dataset)

# Checking the distribution of the Total Price variable
ggplot(dataset, aes(x = TotalPrice)) +
geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 3.5) +
labs(title = 'Distribuição da Variável TotalPrice')

# Number of customers
# Actually means we have 805K "Commercial Transactions"
length(dataset$`Customer ID`)
# Actual amount of "Number of Customers"
length(unique(dataset$`Customer ID`))

# Total money spent per customer
# Adding the "Total Price" Column, grouping by each "Customer ID"
# Creating a new Column "Sum" with the sum of what each customer spent
total_gasto <- dataset %>% group_by(`Customer ID`) %>% summarise(Sum = sum(TotalPrice))
View(total_gasto)

# Creating a custom date, Christmas 2011 (Last sale made)
# Considering Christmas 2011, what was the most recent purchase?
# Based on this date we will base our Recency
View(dataset)
max(dataset$InvoiceDate)
date1 = as.Date.character("25/12/2011","%d/%m/%Y")

# Function to convert dates from POISxt format to Date format
# We are not going to use the values of Hour, Minute and Second of Column "InvoiceData"
converte_data <- function(x)
{
  options(digits.secs = 3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
}

# Run the Function
dataset$InvoiceDate <- converte_data(dataset)
View(dataset)

# Function to calculate Recency, Frequency and Monetary Value
# Initially we are going to group the data by the "Cuser ID" column
# Next, let's do a four-element Summarization
# First Element: Recency (How recently the customer made the last purchase)
# Difference between "date1" (Business Dep.) and the highest purchase date of each customer
# Second Element: Frequency (Number of purchases each customer made)
# Third Element: Sum of "Total Price"
# Fourth Element: Extracting the first purchase from the dataframe
calcula_rfm <- function(x){
  z <- x %>% group_by(`Customer ID`) %>% 
     summarise(Recency = as.numeric(date1 - max(InvoiceDate)), 
               Frequency = n(), 
               Monetary = sum(TotalPrice),
               primeira_compra = min(InvoiceDate))
  
  # Removing transactions with values above the 3rd Quartile and below the 1st Quartile (outliers)
  # Extreme values of customers who bought too much or customers who bought too little
  # Outliers are individuals that deviate from the general average of the dataframe and impact the analysis
  # With this we will have the closest result to the general average of the dataframe
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR))
  return(z)
}

# Run the Function
valores_rfm <- calcula_rfm(dataset)
View(valores_rfm)

# Machine Learning - Kmeans Clustering
# Defining a "Set Seed", to standardize the random process
# In order to initialize in the same way, however, continuing randomly
set.seed(1029)

# Function for segmenting customers based on RFM values
# Creating the function that will develop the customer segmentation
segmenta_cliente <- function(rfm)
{
  # Create a list
  resultados <- list()
  # Gets the RFM values (Filtering the columns that will actually be used)
  dados_rfm <- select(rfm, c('Recency','Frequency','Monetary'))
  # Create the model based on the "kmeans" that the function that contains the algorithm of
  # clustering for unsupervised learning, training the model with the
  # selection of five distinct customer "groups" based on the FRM
  # Set 50 interactions (training time)
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  # Plot of the model, this will be one of the results of the machine learning model
  resultados$plot <- fviz_cluster(modelo_kmeans, data = dados_rfm, 
  geom = c('point'), ellipse.type = 'euclid')
  # Arrange the data, another result for the machine learning model
  dados_rfm$`Customer ID` <- rfm$`Customer ID`
  dados_rfm$clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
    return(resultados)
}

# Run the functon
grafico <- segmenta_cliente(valores_rfm)[1]
grafico
tabela_rfm <- segmenta_cliente(valores_rfm)[2]
View(as.data.frame(tabela_rfm))

# With this it is possible to deliver this graph and table to the marketing department
# so that it is now possible to identify five groups of customers by similarity.
# It is possible to filter customers from each filter.
# For example, customers from group five, have a higher or lower resentment than the clients in group four?
