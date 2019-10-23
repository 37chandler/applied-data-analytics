library(ggplot2)
library(scales)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(magrittr)

#working.dir <- "C:\\Users\\jchan\\Dropbox\\Teaching\\2016_Fall\\ADA\\R\\DataViz\\"
working.dir <- ""

gd <- fread(paste0(working.dir,"grocery_data.txt"))
gdd <- fread(paste0(working.dir,"shopper_dept_segment.txt"))
month.summary <- fread(paste0(working.dir,"month_summary.txt"))
dept.summary <- fread(paste0(working.dir,"dept_summary.txt"))

# problem 1. dotchart of sales by month by department
gd <- 
  gd %>%
    mutate(month = as.factor(month),
           departmentName = reorder(departmentName,ownerSales,mean))


p <- 
  ggplot(gd,
         aes(x=ownerSales/1000,y=departmentName)) + 
  geom_point(position=position_jitter(h=0.5),
             alpha=0.1) + 
  theme_minimal() + 
  scale_x_continuous(label=dollar) + 
  labs(x="Owner Sales (000s)",y="")

print(p)

# prob 2, add median lines
medians <- gd %>% 
  group_by(departmentName) %>%
  summarise(med.val = median(ownerSales/1000))

p <- p + 
  geom_segment(data=medians,aes(x=med.val,
                                xend=med.val,
                                y=as.numeric(departmentName)-0.5,
                                yend=as.numeric(departmentName)+0.5),
               col="red")

print(p)


# prob 3 density for gd
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


# Problem 4. Facet of densities
p <- 
  ggplot(gd,
         aes(x=ownerSales/1000)) +
  geom_density(bw="SJ") + 
  facet_wrap(~departmentName,scales="free_y") + 
  scale_x_continuous(label=dollar) + 
  labs(x="Sales (000s)",y="")

print(p)


# make better
p <- 
  ggplot(gd %>% filter(departmentName != "BEER & WINE"),
         aes(x=ownerSales/1000)) +
  geom_density(bw="SJ") + 
  facet_wrap(~ departmentName,scales="free") + 
  theme_minimal() + 
  scale_x_continuous(label=dollar) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        strip.text.x = element_text(size = 7)) + 
  labs(x="Sales (000s)",y="")

print(p)

# Prob 6 
p <- 
  ggplot(gd,
         aes(y=ownerSales/1000,x=departmentName)) + 
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),
              scale = "width") + 
  theme_minimal() +
  scale_y_log10(label=dollar) + 
  labs(x="",y="Monthly Sales (000s)") + 
  geom_hline(data=gd %>% mutate(mean.sales=mean(ownerSales/1000)),
             aes(yintercept=mean.sales),
             col="red") +
  coord_flip() 

print(p)


# Prob 7
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


# Prob 8
fp <- gdd %>%
  filter(Segment != "Light") %>% 
  group_by(Segment, year, month) %>%
  summarise(Sales=sum(TotalSales)) %>%
  mutate(ym=paste(year,month,sep="-")) %>%
  arrange(year,month)

yms <- unique(fp$ym)
num.points <- length(yms)
break.points <- seq(1,num.points,by=12)

p <- ggplot(fp,
            aes(x=ym,y=Sales,group=Segment,color=Segment)) +
  geom_line() +
  theme_minimal() + 
  labs(x="",y="Log(Scale)")

print(p)

# Prob 9
p <- p + 
  stat_smooth(method="lm",se=F) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  scale_y_log10(labels=dollar,breaks=c(1000,10000,100000,1000000)) + 
  scale_x_discrete(breaks=yms[break.points]) 
p


