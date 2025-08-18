nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

library(tidyverse)
setwd("~/Documents/Teaching/Emory/epi590r-inclass/data/raw/")
nlsy <- read_csv("nlsy.csv",
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)

#error: dont do setwd, since not everyone has the same wd. So, use HERE package instead, it constructs all the filepaths.
# TAKEAWAY: USE HERE INSTEAD OF SETWD.

install.packages("here")
here::here()
getwd()

setwd("/Users/nithyasudhakar/Desktop/EPI 590 R Git Practice/Data")
here::here()
getwd()

#NOTE: sometimes, setwd Data doesnt work and we need the file path to the FOLDER where the data sits.

#Stop
nlsy <- read_csv("nlsy.csv",
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)





library(dplyr)
nlsy <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

nlsy <- na.omit(nlsy)

setwd("../clean/")
write_rds(nlsy, "nlsy-complete-cases.rds")
