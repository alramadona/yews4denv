library(xlsx)

f.total <- read.xlsx("~/R/R-Project/yews4denv/data/dataset_total.xlsx", 1)
training <- subset(f.total,year<2011)
