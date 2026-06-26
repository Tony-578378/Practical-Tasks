-- This project uses several queries to analyse the data "chocolate_sales_sample.csv" for chocolate sales in 2023-2024.
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
--select * from sales; --Uncomment any queries only when you need to run them, since RStudio will execute all queries. Each query ends with a semicolon.

-- 2.Show specific columns
--select order_date, quantity, customer_id from sales;
--select quantity, customer_id, store_id from sales;

-- 3.Add an operation column
--Select order_date, quantity, cost, profit, cost + profit from sales;
--Select order_date, quantity, cost, profit, cost + profit as 'Revenue' from sales; --give the column a name

-- 4.Show specific rows
--select * from sales where quantity > 3;
--select * from sales where quantity > 3 order by quantity; --ascending order by quantity
--select * from sales where quantity > 3 order by quantity desc; --descending order by quantity
--select * from sales where store_id = 'S054' order by product_id, quantity desc; --first order by product_id then by quantity
--select * from sales where quantity > 3 and order_date >= '2023-06-26';
--select * from sales where quantity >= 1 and quantity <= 4;
--select * from sales where store_id = 'S096' or store_id = 'S090';
--select * from sales where store_id in ('S096', 'S090'); --same as the previous one
--select * from sales where product_id like 'P01%'; --all product_id started by P01
--select * from sales where product_id like '%12%'; --all product_id containing 12

-- 5.Show specific rows and columns
--select order_date, quantity from sales where quantity > 3 and order_date <= '2023-12-31' order by quantity desc; --show only 2023

-- 6.Change numerical to categorical
--Select order_date, quantity,
--       case      when quantity < 2 then 'Fewer than 2'
--                 when quantity < 3 then 'Fewer than 3'
--                 when quantity < 4 then 'Fewer than 4'
--                 when quantity < 5 then 'Fewer than 5'
--             else '5'
--         end as 'Quantity category'
--from sales;

-- 7.Grouping
--select store_id, sum(quantity), avg(quantity)
--from sales
--group by store_id;

--select store_id, discount, sum(quantity) as 'Total quantity'
--from sales
--where discount <> 0 --discount not equal to 0
--group by store_id
--order by `Total quantity` desc
--limit 20; --top 20

