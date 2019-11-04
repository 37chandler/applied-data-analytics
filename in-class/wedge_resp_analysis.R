library(tidyverse)

working.dir <- paste0("C:\\Users\\jmeese\\Desktop\\ADA\\",
                      "applied-data-analytics\\in-class\\")

d <- read_tsv(paste0(working.dir,"wedge_responses.txt"))

d <- janitor::clean_names(d)


