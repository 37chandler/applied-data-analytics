library(tidyverse)
library(scales)
library(lubridate)

d <- read_tsv("OwnerTransactions_30.txt")

johns.spend.by.date <- d %>% 
  filter(card_no==18736,
         trans_type == "I") %>% 
  group_by(dt = date(datetime)) %>% 
  summarize(total_spend = sum(total)) %>% 
  ungroup

johns.spend.by.date <- 
  johns.spend.by.date %>% 
  mutate(year=year(dt),
         day_in_year=yday(dt))
  
ggplot(johns.spend.by.date,
       aes(x=day_in_year,
           y=total_spend)) + 
  geom_line() +
  facet_wrap(~year,
             ncol=1) + 
  theme_minimal() +
  labs(x="Day in Year",
       y="Total Spend") + 
  stat_smooth(se=F) +
  scale_y_continuous(labels=dollar)

ggplot(johns.spend.by.date,
       aes(x=total_spend)) + 
  geom_density()


d %>% 
  filter(card_no==18736,
         trans_type == "I") %>% 
  group_by(dt = date(datetime)) %>% 
  summarize(total_spend = sum(total)) %>% 
  ungroup %>% 
  filter(year(dt)==2012) %>% 
  ggplot(aes(x=dt,y=total_spend)) + 
  geom_point(col="purple") + 
  geom_vline(xintercept=ymd("2012-07-04"),
             col="red",
             lwd=2) + 
  coord_equal()
  
  
d %>% 
  filter(card_no==18736,
         trans_type == "I") %>% 
  group_by(dt = date(datetime),
           department) %>% 
  summarize(total_spend = sum(total)) %>% 
  ungroup %>% 
  filter(department %in% c(2,5,13)) %>% 
  ggplot(aes(x=as.factor(year(dt)),y=total_spend)) +
  geom_violin() + 
  facet_wrap(~department) +
  scale_y_continuous(label=dollar) + 
  coord_flip()









