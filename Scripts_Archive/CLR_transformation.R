# Using centered log-ratio (CLR) transformation on proportional data


library(tidyverse)


#read in data
data=read.csv("CoyoteDataFeb12022.csv") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% # formatting data properly
    filter(Date >= "2012-1-1") %>% # removing reports before Jan 1 2012
    mutate(Bold = fct_collapse(CoyoteResponse, unk = "Unknown", sight = "0", avoid = c("1", "2"), # binning boldness
                               indiff = c("3", "4", "5"), aggres = c("6", "7", "8", "9")), 
           Bold= as.ordered(Bold), #making boldness an ordered factor
           Bold= fct_relevel(Bold, c("unk", "sight", "avoid", "indiff", "aggres")), # boldness = increasing order
           Year_int = Year - 2011, 
           Act = fct_relevel(HumanActivity, c("Unknown", "Walking", "Cycling", "Driving", "OutdoorAct", "HomeYard")), 
           Vul = fct_relevel(VulnerableIndividual, c("Unknown", "Child", "Dog", "Cat", "Multiple")), 
           Num = fct_relevel(CoyoteNumber, c("Unknown", "One", "Two", "Three", "More")), 
           Health = fct_relevel(Health, c("Unknown", "Unhealthy", "Healthy")))

# making a data frame with only the proportional variables in need of clr transformation 
clrData = data[,c("Mod200m", "Mow200m", "Nat200m", "Com200m", "Res200m", "Water200m")]
head(clrData)
clrData  = clrData +1
  clrData


colnames(clrData) = c("Mod200m_CLR", "Mow200m_CLR", "Nat200m_CLR", "Com200m_CLR", "Res200m_CLR", "Water200m_CLR")

clrData = data.frame(compositions::clr(clrData)) # running clr transformation 
data = cbind(data, clrData) # adding CLR transformed variables back into the main data 


Hmisc::rcorr(as.matrix(data[,c("RoadDistDecay", "Mod200m_CLR", "Mow200m_CLR", "Build200m",
                             "Nat200m_CLR", "Com200m_CLR", "Res200m_CLR")]), type = c("spearman"))
Hmisc::rcorr(as.matrix(data[,c("RoadDistDecay", "Resclr", "Mowclr", "Build200m",
                               "Natclr", "Comclr", "Modclr")]), type = c("spearman"))


bold.data = data %>% # data for analyses with no sightings or unknown boldness
  filter(Bold !="unk", Bold!="sight")

# let's run a few univariate models to see if this data transformation changes our results
library(ordinal)

summary(clm(Bold ~ Mod200m, data = bold.data)) # the original model for Mod200m
summary(clm(Bold ~ Mod200m_CLR, data = bold.data)) # oh weird, the significant effect of modified open habitat is gone! 

summary(clm(Bold ~ Nat200m, data = bold.data)) # the original model for Nat200m
summary(clm(Bold ~ Nat200m_CLR, data = bold.data)) # again, weird, the significant effect is gone

summary(clm(Bold ~ Mow200m, data = bold.data)) # the original model for Mow200m
summary(clm(Bold ~ Mow200m_CLR, data = bold.data)) # ok weird, the model now shows a MUCH more significant effect...

summary(clm(Bold ~ Res200m, data = bold.data)) # the original model for Res200m
summary(clm(Bold ~ Res200m_CLR, data = bold.data)) # a weaker effect after CLR transforming

summary(clm(Bold ~ Com200m, data = bold.data)) # the original model 
summary(clm(Bold ~ Mow200m_CLR, data = bold.data)) # ok weird, the model now shows a MUCH more significant effect...



# looking at a multivariate model, we get quite different results using the original proportions 
# compared to the CLR transformed data 
summary(clm(Bold ~ Mod200m + Mow200m + Nat200m + Res200m+ Com200m + Season + Year_int + Build200m + RoadDistDecay, data = bold.data, na.action = "na.fail")) 
summary(MuMIn::dredge(clm(Bold ~ Mod200m_CLR + Mow200m_CLR + Nat200m_CLR + Res200m_CLR+ Com200m + Season + Year_int + Build200m + RoadDistDecay, 
                  na.action = "na.fail", data = bold.data)))

results <- data.frame(
  conf_.5 = confint(rsfmod, level=0.99)[,1],
  odds = coef(rsfmod),
  conf_99.5 = confint(rsfmod, level=0.99)[,2])
rsf_results





# alternatively, I could do the clr transformation for each variable individually? 
data$Modclr = compositions::clr(data$Mod200m)
data$Mowclr = compositions::clr(data$Mow200m)
data$Natclr = compositions::clr(data$Nat200m)
data$Resclr = compositions::clr(data$Res200m)
data$Comclr = compositions::clr(data$Com200m)



