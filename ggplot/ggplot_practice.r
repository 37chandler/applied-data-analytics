# This script gives us some practice on ggplot

library(tidyverse)
library(scales)
library(RColorBrewer)

# Switch working directory so we can read locally
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gd <- read_tsv("grocery_data.txt")
month.summary <- read_tsv("month_summary.txt")
dept.summary <- read_tsv("dept_summary.txt")
mosaic.data <- read_tsv("counts_for_mosaic.txt")

# problem 1. dotchart of sales by month by department
gd <- gd %>%
    mutate(month = as.factor(month),
           departmentName = reorder(departmentName,ownerSales,mean))

# Here's a basic one:
ggplot(gd,
       aes(x=ownerSales,y=departmentName)) + 
  geom_point() + 
  theme_minimal() + 
  scale_x_continuous(label=dollar) #fancy axis from scales

# Things to add:
# * Divide sales by 1000
# * Add some transparency to the points with `alpha`
# * Add axis labels using `labs`


# prob 2, add median lines
medians <- gd %>% 
  group_by(departmentName) %>%
  summarise(med.val = median(ownerSales/1000))

# add these lines to the plot using
# geom_segment. That function/layer takes four values
# x, xend, y, and y.end. You can get y.values with
# something like "as.numeric(departmentName)-0.5" (or plus)
# The x's should be the median values


# Prob 3 was both hard and kind of boring. 
# I'm leaving in the solution.
p3.size <- 4
p3.cols <- c("gray15","gray40","gray75")
p <- ggplot(gd,
            aes(x=ownerSales/1000)) + 
  ylab("Density") + 
  geom_line(stat="density",col=p3.cols[1],size=1.25,bw=1) + 
  geom_line(stat="density",col=p3.cols[2],size=1.25,bw=5) + 
  geom_line(stat="density",col=p3.cols[3],size=1.25,bw=25) + 
  theme_minimal() + 
  scale_x_continuous(label=dollar) + 
  xlab("Monthly Sales (000)") + 
  geom_text(aes(x=20,y=0.05),
            label="Bandwidth = 1",
            col=p3.cols[1],
            check_overlap = T,
            size=p3.size) + 
  geom_text(aes(x=20.5,y=0.03),
            label="Bandwidth = 5",
            col=p3.cols[2],
            check_overlap = T,
            size=p3.size) + 
  geom_text(aes(x=75,y=0.009),
            label="Bandwidth = 25",
            col=p3.cols[3],
            check_overlap = T,
            size=p3.size)

print(p)


# Problem 4. Use `gd`. First, make a density
# plot of sales (using geom_density). 
# Then facet that plot by department using
# `facet_wrap`



# Prob 6 
# violin plots!
# Use gd, build violin plots of ownerSales by department. 
# Optional: add quantile lines, use nice labels,
# orient chart with departments on the y-axis, 
# add a vertical line for the overall mean

# your code here


# Prob 7
# Mosaic charts are hard. Here's a simplified 
# version of the one that's in the book. 


mosaic.table <- mosaic.data %>% 
  spread(Segment,count) %>% 
  rename(Dept=DepartmentName) %>% 
  arrange(Primary) %>% 
  as.data.frame
  
# Let's do some renaming
row.names(mosaic.table) <- mosaic.table$Dept
mosaic.table$Dept <- NULL

rownames(mosaic.table)[rownames(mosaic.table)=="SUPPLEMENTS"] <- "SUPP."
rownames(mosaic.table)[rownames(mosaic.table)=="REF GROCERY"] <- "REF GROC"
rownames(mosaic.table)[rownames(mosaic.table)=="PACKAGED GROCERY"] <- "PKG GROC"
  
mosaicplot(mosaic.table,main="Segment by Department",las=2)  


