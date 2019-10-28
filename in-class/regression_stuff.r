library(tidyverse)
library(DBI)

con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0("wedge_reporting.db"))

# Listing the tables
dbListTables(con)

# And creating connections to the two tables. 
owner.ym <- tbl(con, "owner_year_month")

for.plot <- owner.ym %>% 
  group_by(card_no,year) %>% 
  summarize(sales = sum(sales)) %>% 
  filter(year %in% c(2013,2014,2015),
         card_no != 3) %>% 
  collect

for.plot <- 
  spread(for.plot,key=year,value=sales) %>% 
  rename(year_2014 = `2014`,
         year_2015 = `2015`,
         year_2013 = `2013`)

for.plot <- for.plot %>% 
  filter(year_2014 < 30000,
         year_2015 < 30000)

ggplot(for.plot,aes(x=year_2014,y=year_2015)) +
  geom_point(alpha=0.1) +
  theme_minimal()

#####################################

head(for.plot)

lm.1 <- 
  lm(year_2015 ~ year_2014 + year_2013,
   data=for.plot)

arm::display(lm.1)








