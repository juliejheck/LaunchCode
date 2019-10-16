##### Load required libraries #####
require(tidyverse); require(funModeling); require(data.table);
require(lubridate); require(readxl)

##### Change the working directory #####
## Programmatically or from Session drop down
setwd("~/Documents/Projects/CoderGirl/Week 4/In Class")

## Check out what files are in this directory
list.files()

##### Figure out which sheets to read in & read in the data #####
## Source - https://www.ibm.com/communities/analytics/watson-analytics-blog/sales-products-sample-data/
## Note I have modified for size and learning purposes
## Because we will be using multiple times, I am going to store the file as a variable
sales_data_file <- 'Camping_Equipment_Sales_Class.xlsx'

## What sheets are in our workbook?
excel_sheets(sales_data_file)

## Read each sheet into its own data frame to start
sales_data_2012 <- read_excel(sales_data_file, sheet = '2012 Sales')
sales_data_2013 <- read_excel(sales_data_file, sheet = '2013 Sales')
sales_data_2014 <- read_excel(sales_data_file, sheet = '2014 Sales')

##### Let's quickly explore each sheet to ensure uniformity #####
df_status(sales_data_2012)
df_status(sales_data_2013)
df_status(sales_data_2014)

##### Long to wide data #####
## It appears that the 2014 data is formatted differently based on the column names we have
## Let's take a look at it now and see what we can do about that
View(sales_data_2014)

## The data we ended up with for 2014 has been formatted to be "long"
## In other words, rather than a single record per row, we have a record that spans multiple rows
## This isn't fun to work with, so let's fix that now - Note there are multiple ways to do this
## I am using spread()
sales_data_2014_2 <- sales_data_2014 %>% 
  spread(key = 'Measure', value = 'Value')

## Let's make sure it matches the format for the other two data frames
View(sales_data_2014_2)

## It does look like the columns are slightly out of order, so let's use select to fix that
## in our spread call
sales_data_2014_2 <- sales_data_2014 %>% 
  spread(key = 'Measure', value = 'Value') %>% 
  select(c(1:7, 11, 9, 8, 10))

##### Merging / Appending Data #####
## Now that we have all of the data formatted properly, let's put all of the years together
## This will make it easier to work with when we start to explore and aggregate
## Quick name check
names(sales_data_2012)
names(sales_data_2013)
names(sales_data_2014_2)

## Since all of the names match, we can bind the data togther like so
## In tidyverse, there is the bind_rows function.
## There is also a base R function, rbind, that would work as well although for large datasets this it slower
sales_data_all <- bind_rows(sales_data_2012, sales_data_2013, sales_data_2014_2)

## Some clean up
rm(sales_data_2012, sales_data_2013, sales_data_2014, sales_data_2014_2, sales_data_file)

## Clean names for working with the data
names(sales_data_all) <- c('retailer_country','order_method_type','retailer_type','product_line',
                           'product_type','year','quarter','quantity','gross_revenue',
                           'cost_of_goods_sold','net_revenue')

##### Basic Aggregation with Group By & Summarise #####
## Aggregating your data will be incredibly important for both understanding and later visualizing your data
## There are visual tools like Tableau but it is good to have this skill outside of that in case
## your company doesn't that option
## group_by and summarise(ze) will almost always be used together to aggregate your data
## Sales by Year by Product Line
sales_by_year_by_product <- sales_data_all %>% 
  group_by(year, product_line) %>% 
  ## Dividing by 1000 to put the numebrs into 000s
  summarise(gross_revenue = as.integer(sum(gross_revenue) / 1000),
            cogs = as.integer(sum(cost_of_goods_sold) / 1000),
            net_revenue = as.integer(sum(net_revenue) / 1000))

## Profit Margin by Country & Retailer
sales_by_country_by_retailer <- sales_data_all %>% 
  group_by(retailer_country, retailer_type) %>% 
  summarise(profit_margin = sum(net_revenue) / sum(gross_revenue))

##### Aggregation with Group By, Summarise, and Mutate #####
## Most Profitable Product Type
profitable_products <- sales_data_all %>% 
  group_by(product_type) %>% 
  summarise(gross_revenue = sum(gross_revenue),
            profit_margin = sum(net_revenue) / sum(gross_revenue)) %>% 
  mutate(gr_rank = rank(-gross_revenue),
         profit_rank = rank(-profit_margin),
         final_rank = rank((gr_rank + profit_rank) / 2))

## Percent of Sales by Order Method Type by Country
percent_sales_by_omt_by_country <- sales_data_all %>% 
  group_by(retailer_country) %>% 
  mutate(country_sales = sum(gross_revenue)) %>% 
  group_by(retailer_country, order_method_type) %>% 
  summarise(perc_sales = sum(gross_revenue) / max(country_sales)) %>% 
  mutate(perc_sales = paste(round(perc_sales*100, 2), '%', sep = ''))