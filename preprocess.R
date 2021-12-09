#read data
library(oro.nifti)
library(stringr)
# img <- readANALYZE("OAS1_0231_MR1_mpr_n4_anon_111_t88_masked_gfc.hdr")
# a<-img@.Data[,,,1]
# b<-a[1,,]

setwd("~/Dropbox (University of Michigan)/umich/biostat625/final/hdr")
data = read.csv("../code/data/oasis_cross-sectional.csv")
id = data[data$Age>60, ]$ID
file_list <- list.files(pattern = "\\.hdr$")
file_name <- sapply(file_list, function(x)str_sub(x,1,13))
file_filter <- file_list[file_name %in% id]
img_list = lapply(file_filter, function(x){
  readANALYZE(x)@.Data[,,,1]
})
save(img_list, file = "../code/data/img_list.rds")
# plot(img_list)
# library(misc3d)
# library(rgl)
# image3d(a)
