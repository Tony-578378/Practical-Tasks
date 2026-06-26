-- This project uses several queries to analyse the data "chocolate_sales_sample.csv" for chocolate sales.
-- Run the following codes in the console first:
-- library(DBI)
-- library(RSQLite)
-- # Create the same database connection
-- con <- dbConnect(SQLite(), ":memory:")
-- # Read your CSV
-- sales <- read.csv("Chocolate_Sales.csv")
-- # Upload it to SQLite
-- dbWriteTable(con, "sales", sales, overwrite = TRUE)
-- !preview conn=con

-- 1.Show the whole table
--select * from sales; --Uncomment any queries only when you need to run them, since RStudio will execute all queries.

-- 2.Show specific columns
--select order_date, quantity, customer_id from sales;
--select quantity, customer_id, store_id from sales;

-- 3.Add an operation column
Select order_date, quantity, cost, profit, cost + profit from sales;




