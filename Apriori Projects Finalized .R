#Installing necessary packages

install.packages("tidyverse")
install.packages("readr")
install.packages("arules")
install.packages("dplyr")
library(dplyr)
library(arules)
library(readr)
library(tidyverse)
library(arulesViz)

#Source of data: https://www.kaggle.com/datasets/carrie1/ecommerce-data

data <- read_csv("Online Retail.csv")
head(data)

#Inspecting whether corrupted data exist
summary(data)


#Cleaning, minus quantity and price and missing Customer Id
clean_data <- data %>%
  filter(
    Quantity > 0,
    UnitPrice > 0,
    !is.na(Description),
    !is.na(CustomerID)
  )
#Check total number of original rows
nrow(data)

#Check what's how many rows are left
nrow(clean_data)

#Double check if any other missing value still exist
summary(clean_data)

#Group by invoice and description
baskets <- data %>%
  group_by(InvoiceNo) %>%
  summarise(items = paste(unique(Description), collapse = ","))

View(baskets)

#Write to CSV
write.csv(baskets, "baskets.csv", row.names = FALSE)

#Then I cleaned the data again in Spreadsheet
#Removing wrong data

transaction <- read.csv("Cleaned - baskets.csv")

#Separating the string as individual string
basket_list <- strsplit(transaction$items, ",\\s*")
transactions <- as(basket_list, "transactions")

#Running the algorithm
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.1, maxlen = 4))

#Checking the algorithm
inspect(head(sort(rules, by="lift"), 100))
