# Build regression model to estimate Future Predicted Value,
# a key component of Lifetime Value (LTV) modeling. 

library(tidyverse)
library(reshape2)
library(DBI)
library(scales)

# Let's hook up to our Wedge reporting DB
# and get the owner table

#path.to.db <- paste0("C:\\Users\\jchan\\Dropbox\\",
#                     "Teaching\\2019\\ADA\\",
#                     "applied-data-analytics\\wedge-solution\\")

path.to.db <- paste0("C:\\Users\\jmeese\\Desktop\\ADA\\",
                                    "applied-data-analytics\\wedge-solution\\")


# Creating the connection to the DB. Similar to 
# a cursor. 
con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(path.to.db,"wedge_reporting.db"))

# Listing the tables
dbListTables(con)

# And creating connections to the two tables. 
owner.ym <- tbl(con, "owner_year_month")

# Let's assume that we're trying to predict spend
# in 2015, given the data in 2013 and 2014. I'm 
# going to build this up in R. There are 
# faster ways to do this, but I'm just going with 
# slow and steady. 

# md = modeling data. Start with annual
md <- owner.ym %>% 
  filter(card_no != "3",
         between(year,2013,2015)) %>% 
  distinct(card_no) %>% 
  collect 

# Now start bringing in our covariates. First 2013
md <- left_join(md,
                owner.ym %>% 
                  filter(year==2013) %>% 
                  group_by(card_no) %>% 
                  summarize(sales_2013 = sum(sales),
                            trans_2013 = sum(transactions),
                            items_2013 = sum(items)) %>% 
                  collect,
                by="card_no") 

md <- left_join(md,
                owner.ym %>% 
                  filter(year==2014,
                         month <= 6) %>% 
                  group_by(card_no) %>% 
                  summarize(sales_1h_2014 = sum(sales),
                            trans_1h_2014 = sum(transactions),
                            items_1h_2014 = sum(items)) %>% 
                  collect,
                by="card_no") 

# I'm also interested in the last two quarters of 2014. I'm 
# going to brute force it since these aren't too big
md <- left_join(md,
                owner.ym %>% 
                  filter(year==2014,
                         month %in% c(7,8,9)) %>% 
                  group_by(card_no) %>% 
                  summarize(sales_q3_2014 = sum(sales),
                            trans_q3_2014 = sum(transactions),
                            items_q3_2014 = sum(items)) %>% 
                  collect,
                by="card_no") 

md <- left_join(md,
                owner.ym %>% 
                  filter(year==2014,
                         month %in% c(10,11,12)) %>% 
                  group_by(card_no) %>% 
                  summarize(sales_q4_2014 = sum(sales),
                            trans_q4_2014 = sum(transactions),
                            items_q4_2014 = sum(items)) %>% 
                  collect,
                by="card_no") 

# Finally, our response variable
md <- left_join(md,
                owner.ym %>% 
                  filter(year==2015) %>% 
                  group_by(card_no) %>% 
                  summarize(sales_2015 = sum(sales)) %>% 
                  collect,
                by="card_no") 

# There are some super big values in here. I'm going to 
# drop anyone with more than 40K in annual spend.
md <- md %>% 
  filter(sales_2013 < 40000,
         sales_1h_2014 + sales_q3_2014 + sales_q4_2014 < 40000,
         sales_2015 < 40000)


# Now let's build a holdout sample. 
holdout.card.no <- md %>% 
  select(card_no) %>% 
  distinct(card_no) %>% 
  sample_frac(size=0.15)

include.card.no <- md %>% 
  filter(!(card_no %in% holdout.card.no$card_no)) %>% 
  distinct(card_no)

# how co-linear are these? 
pairs(md %>% 
        select(contains("2013")) %>% 
        filter(sales_2013 < 15000),
      pch=".")
# sales pretty correlated with items

md <- md %>% 
  mutate(trans_2014 = trans_1h_2014 + trans_q3_2014 + trans_q4_2014)

lm.1 <- lm(sales_2015 ~ sales_2013 + trans_2013 + 
             sales_1h_2014 + sales_q3_2014 + sales_q4_2014,
           data=md,
           subset=card_no %in% include.card.no$card_no)

plot(lm.1)
anova(lm.1)
summary(lm.1)

broom::tidy(lm.1)

# let's see how we do on the holdout sample. 
holdout.estimates <- md %>% 
  filter(card_no %in% 
           holdout.card.no$card_no)

holdout.estimates <- holdout.estimates %>% 
  mutate(pred_2015 = predict(lm.1,holdout.estimates))
  
ggplot(holdout.estimates,
       aes(x=sales_2015,y=pred_2015)) + 
  geom_point(shape=1,alpha=0.2) + 
  geom_abline(slope=1,intercept=0) +
  theme_minimal() + 
  scale_x_log10(label=dollar) + 
  scale_y_log10(label=dollar) + 
  stat_smooth(method="lm",se=F,col="red") + 
  stat_smooth(se=F) + 
  labs(x="Actual 2015 Spend",
       y="Predicted 2015 Spend")

# Now some summary stats
holdout.estimates <- holdout.estimates %>% 
  mutate(resid = sales_2015 - pred_2015,
         ape = abs(resid)/sales_2015)

Hmisc::describe(holdout.estimates %>% select(resid,ape))
# median ape is 30%. Not terrible. 

