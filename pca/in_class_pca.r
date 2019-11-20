
# Here's a PCA on some Wedge Data. Give 
# this a look. Can you explain what's going 
# on here? 

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)

d <- readr::read_tsv("owner_level_top_prod_sales.txt")

d <- d %>% 
  filter(owner != 3)

# To make our code run a bit faster and get rid of some extreme
# values, let's total up the amount spent on the top products and
# cut down our data. 
total.spend <- data.frame(owner=d$owner,
                          spend=rowSums(d[,2:1000]))

ggplot(total.spend,
       aes(x=spend)) + 
  geom_density() + 
  scale_x_log10(label=dollar)

quantile(total.spend$spend,
         prob=0:10/10)
  
# Let's cutoff at $25,000
mean(total.spend$spend < 25000)

d <- d %>% 
  filter(owner %in% 
           (total.spend %>% 
           filter(spend < 25000) %>% 
           pull(owner)))

pca1 <- prcomp(d[,-1])

for.plot <- data.frame(sd=pca1$sdev)
for.plot <- for.plot %>% 
  mutate(eigs=sd^2) %>% 
  mutate(cume.var = cumsum(eigs/sum(eigs)),
         id=1:n())

names(for.plot) <- c("Standard Deviation","eigs",
                     "Cumulative Variance","id")

for.plot <- melt(for.plot,
                 id.vars = "id")

ggplot(for.plot %>% filter(variable != "eigs"),
       aes(x=id,y=value)) +
  geom_line() + 
  facet_grid(variable ~ .,
             scales="free") + 
  theme_bw() + 
  labs(y="Variance",
       x="Component Number")

if (ncol(d) > 31){
  max.col <- 20
} else {
  max.col <- ncol(d)
}

sort(pca1$rotation[,1],decreasing = T)[1:max.col]
sort(pca1$rotation[,1],decreasing = F)[1:max.col]

sort(pca1$rotation[,2],decreasing = T)[1:max.col/2]
sort(pca1$rotation[,2],decreasing = F)[1:max.col/2]

sort(pca1$rotation[,3],decreasing = T)[1:max.col/2]
sort(pca1$rotation[,3],decreasing = F)[1:max.col/2]

sort(pca1$rotation[,4],decreasing = T)[1:max.col/2]
sort(pca1$rotation[,4],decreasing = F)[1:max.col/2]

# Let's build derived variables from these components. 

# first, let's illustrate the idea.
pc1.loadings <- pca1$rotation[,1] # loadings on first PC

# Owner 19682 spent a lot ($35519.11), Owner 49219 spent very little ($3.08). 
# Let's look at their scores on PCA 1
as.numeric(d[d$owner=="19682",2:1000]) %*% pc1.loadings

# %*% is matrix multiplication in R

as.numeric(d[d$owner=="49219",2:1000]) %*% pc1.loadings

# we can do this en masse and add columns to d
# based on the PCs

num.pcs <- 5

for(i in 1:num.pcs) { 
  col.name <- paste0("score_PC",i)
  
  d[,col.name] <- as.matrix(d[,2:1000]) %*% pca1$rotation[,i]
}


ggplot(d %>% sample_frac(0.1),
       aes(x=score_PC3,y=score_PC4)) + 
  geom_point(alpha=0.2) + 
  theme_minimal()

# Interesting, some crazy outlier there. Let's look at them
d %>% 
  filter(score_PC4 > 400) %>% 
  select(owner,score_PC4) %>% 
  arrange(as.numeric(score_PC4))

d %>% 
  filter(owner==50028) %>% 
  melt(id.vars="owner") %>% 
  arrange(value) %>% 
  tail(n=20)
# this person spent a *ton* on deli stuff. Seems weird 


# we could remove some of these extreme values 
# (I wouldn't call them "outliers") and maybe 
# get better a better PCA. right now we may be pulling off
# just a handful of people with each dimension. 
