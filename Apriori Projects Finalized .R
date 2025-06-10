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
inspect(head(sort(rules, by = "lift"), 100))

#Convert rules into dataframe
rules_df <- as(rules, "data.frame")

View(rules_df)

#Add lhs and rhs as clean character columns
rules_df$lhs <- labels(lhs(rules))
rules_df$rhs <- labels(rhs(rules))

#Remove curly braces from lhs and rhs for readability
rules_df$lhs <- gsub("[\\{\\}]", "", rules_df$lhs)
rules_df$rhs <- gsub("[\\{\\}]", "", rules_df$rhs)

#Optional: Reorder columns
rules_df <- rules_df[, c("lhs", "rhs", setdiff(names(rules_df), c("lhs", "rhs")))]


#Sorting rules by lift descending
rules_sorted <- rules_df[order(-rules_df$lift), ]

View(rules_sorted)

#export to csv
write.csv(rules_sorted, "apriori.csv", row.names = FALSE)

