library(readr)
library(tidyverse)
tvivienda <- read_csv("formatted-data/tvivienda.csv")

target <- c(4,5,6)

sat = tvivienda%>%
  filter(P5_1 %in% target)%>%
  select(vid,
         EST_DIS,
         UPM_DIS,
         FACTOR,
         P6_3_1,
         P6_3_2,
         P6_3_3,
         P6_3_4,
         P6_3_5,
         P6_3_6,
         P6_4_1,
         P6_4_2,
         P6_4_3,
         P6_4_4,
         P6_4_5)

sum(is.na(sat)) 

write.csv(sat, "satisfaction_data.csv")
