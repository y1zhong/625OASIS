library(oro.nifti)
library(stringr)
library(dplyr)

#read data
setwd("~/Dropbox (University of Michigan)/umich/biostat625/final/hdr")
data = read.csv("../code/data/oasis_cross-sectional.csv")
data = data %>% 
  mutate(CDR = ifelse(CDR == 0, 0, 1)) %>%
  filter(Age >= 60)
write.csv(data, file = "../code/data/oasis_cross-sectional_filter.csv",row.names = F)

#save hdr file into image list
id = data[data$Age>=60, ]$ID
file_list <- list.files(pattern = "\\.hdr$")
file_name <- sapply(file_list, function(x)str_sub(x,1,13))
file_filter <- file_list[file_name %in% id]
img_list = lapply(file_filter, function(x){
  readANALYZE(x)@.Data
})

names(img_list) = sapply( file_filter, function(x) str_sub(x,1,13) )
save(img_list, file = "../code/data/img_list.rds")



