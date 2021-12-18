library(dplyr)
library(mice)
library(gtsummary)

oasis = read.csv("./data/oasis_cross-sectional_filter.csv")
sapply(oasis, function(x) sum(is.na(x)))
#18 missing

oasis %>%
  group_by(ID) %>%
  filter(is.na(SES)) %>%
  pull(ID) %>%
  unique()
#18 ID

oasis_f = oasis[,c(1,2,4:10)]
set.seed(123)
pos = sample(1:5,1)
mi.oasis_f = mice(oasis_f, m=5, printFlag =FALSE)
mi.temp.oasis_f = complete(mi.oasis_f,"all")
new_oasis = mi.temp.oasis_f[[pos]][, c(2:5,7)]
labels = new_oasis$CDR

sum_tb_after_imp = tbl_summary(new_oasis, by = 'CDR')%>% add_overall()
save(sum_tb_after_imp, file = './tables and figures/summary_afterImp')
