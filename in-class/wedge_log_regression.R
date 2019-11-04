# Build regression model to estimate Future Predicted Value,
# a key component of Lifetime Value (LTV) modeling. 

library(tidyverse)
library(reshape2)
library(DBI)
library(scales)

# Let's hook up to our Wedge reporting DB
# and get the owner table

path.to.db <- paste0("C:\\Users\\jchan\\Dropbox\\",
                     "Teaching\\2019\\ADA\\",
                     "applied-data-analytics\\wedge-solution\\")

#path.to.db <- paste0("C:\\Users\\jmeese\\Desktop\\ADA\\",
#                     "applied-data-analytics\\wedge-solution\\")


# Creating the connection to the DB. Similar to 
# a cursor. 
con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(path.to.db,"wedge_reporting.db"))

# Listing the tables
dbListTables(con)

# And creating connections to the two tables. 
owner.ym <- tbl(con, "owner_year_month")

md <- owner.ym %>% 
  filter(card_no != "3",
         between(year,2013,2015)) %>% 
  distinct(card_no) %>% 
  collect 

holder <- owner.ym %>% 
  filter(card_no != "3",
         year==2015,
         month==11) %>%
  select(card_no,sales) %>% 
  rename(nov_spend = sales) %>% 
  collect
  
md <- left_join(md,
                holder,
                by="card_no")

md <- md %>% 
  mutate(nov_spend = if_else(is.na(nov_spend),
                             0,
                             nov_spend))

holder <- owner.ym %>% 
  filter(card_no != "3",
         year==2015,
         month==10) %>%
  select(card_no,sales) %>% 
  rename(oct_spend = sales) %>% 
  collect

md <- left_join(md,
                holder,
                by="card_no")

md <- md %>% 
  mutate(oct_spend = if_else(is.na(oct_spend),
                             0,
                             oct_spend))
md <- md %>% 
  mutate(nov_shopped = if_else(nov_spend > 0,
                               1,
                               0))

summary(glm(nov_shopped ~ oct_spend, data=md))
summary(glm(nov_shopped ~ I(oct_spend/100), data=md))

# Let's add a couple of other columns. 
md <- md %>% 
  left_join(owner.ym %>% 
  filter(card_no != "3",
         year==2015,
         month==9) %>%
  select(card_no,sales) %>% 
  rename(sep_spend = sales) %>% 
  collect,
  by="card_no")

md <- md %>% 
  left_join(owner.ym %>% 
              filter(card_no != "3",
                     year==2014,
                     month==11) %>%
              select(card_no,sales) %>% 
              rename(prev_nov_spend = sales) %>% 
              collect,
            by="card_no")

summary(glm(nov_shopped ~ I(oct_spend/100) + 
              I(sep_spend/100) + 
              I(prev_nov_spend/100), 
            data=md))

md %>% 
  select(contains("spend")) %>% 
  pairs

summary(glm(nov_shopped ~ I(oct_spend/100) + 
              I(sep_spend/100) + 
              I(prev_nov_spend/100), 
            data=md),
        subset=mean(oct_spend,sep_spend,prev_nov_spend) < 10000)
