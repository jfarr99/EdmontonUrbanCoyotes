# Ordinal Regressions February 5 2022

library(ordinal)
library(tidyverse)
library(Hmisc)
library(lubridate)
# pull Coyote_Data from CoyotesSpatialData_Jan2022.Rmd

## COYOTE RESPONSES 
resp.bin = Coyote_Data %>%
  mutate(Response_Bin = fct_collapse(Coyote_Data$CoyoteResponse, unk = "Unknown", sight = "0", avoid = c("1", "2"), 
                                     indiff = c("3", "4", "5"), aggres = c("6", "7", "8", "9")), 
         Response_Bin = as.ordered(Response_Bin), 
         Response_Bin= fct_relevel(Response_Bin, c("unk", "sight", "avoid", "indiff", "aggres"))) %>%
  filter(Year != "2010")


str(resp.bin$Response_Bin)


resp.time = resp.bin %>%
  group_by(fourmonth_total = floor_date(Date, "4 months"), Response_Bin) %>%
  summarise(count=n())%>%
  mutate(perc = count/sum(count)*100) %>%
  filter(Response_Bin != "unk", Response_Bin !="sight")

# adding season to data frame
resp.time$Season = factor(rep(c("Breeding", "PupRearing", "Dispersal"), each =3, times=10))

ggplot(resp.time, aes(x = fourmonth_total, y = perc, fill=Season))+
  geom_bar(stat="identity", position = "dodge") + # could also do as points, tbd 
  geom_smooth(aes(group = 1), method = lm, color = "black") + 
  facet_grid(~ Response_Bin) + 
  theme_bw()


## HUMAN PERCEPTIONS 

perc.time = Coyote_Data %>%
  filter(Year != "2010")%>%
  group_by(fourmonth_total = floor_date(Date, "4 months"), HumanPerception) %>%
  summarise(count=n())%>%
  mutate(perc = count/sum(count)*100) %>%
  filter(HumanPerception != "Concern", HumanPerception!="Unknown")

ggplot(perc.time, aes(x = fourmonth_total, y = perc))+
  geom_bar(stat="identity", position = "dodge") + # could also do as points, tbd 
  geom_smooth(aes(group = 1), method = lm, color = "black") + 
  facet_grid(~ HumanPerception) + 
  theme_bw()

  





# ordinal reg with resp. bin 
resp.data = resp.bin %>%
  filter(Response_Bin !="unk", Response_Bin!= "sight", HumanActivity != "Driving")
str(resp.data$Response_Bin)

summary(clm(Response_Bin ~Roads200m, data = resp.data)) # more roads = LESS conflict odds
summary(clm(Response_Bin ~Mow200m, data = resp.data)) # more mow = more conflict
summary(clm(Response_Bin ~Mow200m*Roads200m, data = resp.data)) # more roads & mow = MORE CONFLICT BIG



  hist(resp.data$RoadDistDecay)



