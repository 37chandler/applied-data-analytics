SELECT description, 
       sum(sales) as sales
FROM product_year_month
GROUP BY description
ORDER BY sales desc


