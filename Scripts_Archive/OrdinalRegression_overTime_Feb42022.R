library(ordinal)
library(tidyverse)
library(Hmisc)

# making three data frames, one for each season, for response
resp_pup = Coyote_Data %>%
  dplyr::filter(CoyoteResponse != "Unknown", CoyoteResponse != 0, Season=="PupRearing", 
                Year !="2010", Year !="2011", HumanActivity!="Driving") %>%
  mutate(CoyoteResponse = as.ordered(CoyoteResponse))
resp_bre = Coyote_Data %>%
  dplyr::filter(CoyoteResponse != "Unknown", CoyoteResponse != 0, Season=="Breeding", 
                Year !="2010", HumanActivity!="Driving")%>%
  mutate(CoyoteResponse = as.ordered(CoyoteResponse))
resp_dis = Coyote_Data %>%
  dplyr::filter(CoyoteResponse != "Unknown", CoyoteResponse != 0, Season=="Dispersal", 
                Year !="2010", HumanActivity!="Driving")%>%
  mutate(CoyoteResponse = as.ordered(CoyoteResponse))


# finding the appropriate scale for each spatial variable
pup_resp_models <- list()
for(i in c(1:5)){ # For each of your five scales (100, 200, 400, 800, 1600)
  # Define the variables you want to test.
  if(i==1){tests <- c("Build100m", "RoadDistDecay", "Nat100m", "Mod100m", "Mow100m", "Com100m", "Res100m")}
  if(i==2){tests <- c("Build200m", "RoadDistDecay", "Nat200m", "Mod200m", "Mow200m", "Com200m", "Res200m")}
  if(i==3){tests <- c("Build400m", "RoadDistDecay", "Nat400m", "Mod400m", "Mow400m", "Com400m", "Res400m")}
  if(i==4){tests <- c("Build800m", "RoadDistDecay", "Nat800m", "Mod800m", "Mow800m", "Com800m", "Res800m")}
  if(i==5){tests <- c("Build1600m", "RoadDistDecay", "Nat1600m", "Mod1600m", "Mow1600m", "Com1600m", "Res1600m")}
  
  # Create a data frame to store the results from all the models.
  pup_resp_models[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(tests)){
    
    model <- clm(CoyoteResponse ~ resp_pup[,j], # Create the predictive model
                 data = resp_pup)
    
    pup_resp_models[[i]][j,1] <- j # First column is the predictor
    pup_resp_models[[i]][j,2] <- AIC(model) # Second column is improvement in AIC
    
  }
  
  colnames(pup_resp_models[[i]]) <- c("predictor", "AIC")
  rm( model, tests) # Keep workspace clean
}

pup_resp_models <- dplyr::bind_rows(pup_resp_models)

# finding the appropriate scale for each spatial variable
bre_resp_models <- list()
for(i in c(1:5)){ # For each of your five scales (100, 200, 400, 800, 1600)
  # Define the variables you want to test.
  if(i==1){tests <- c("Build100m", "RoadDistDecay", "Nat100m", "Mod100m", "Mow100m", "Com100m", "Res100m")}
  if(i==2){tests <- c("Build200m", "RoadDistDecay", "Nat200m", "Mod200m", "Mow200m", "Com200m", "Res200m")}
  if(i==3){tests <- c("Build400m", "RoadDistDecay", "Nat400m", "Mod400m", "Mow400m", "Com400m", "Res400m")}
  if(i==4){tests <- c("Build800m", "RoadDistDecay", "Nat800m", "Mod800m", "Mow800m", "Com800m", "Res800m")}
  if(i==5){tests <- c("Build1600m", "RoadDistDecay", "Nat1600m", "Mod1600m", "Mow1600m", "Com1600m", "Res1600m")}
  
  # Create a data frame to store the results from all the models.
  bre_resp_models[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(tests)){
    
    model <- clm(CoyoteResponse ~ resp_bre[,j], # Create the predictive model
                 data = resp_bre)
    
    bre_resp_models[[i]][j,1] <- j # First column is the predictor
    bre_resp_models[[i]][j,2] <- AIC(model) # Second column is improvement in AIC
    
  }
  
  colnames(bre_resp_models[[i]]) <- c("predictor", "AIC")
  rm( model, tests) # Keep workspace clean
}

bre_resp_models <- dplyr::bind_rows(bre_resp_models)

# finding the appropriate scale for each spatial variable
dis_resp_models <- list()
for(i in c(1:5)){ # For each of your five scales (100, 200, 400, 800, 1600)
  # Define the variables you want to test.
  if(i==1){tests <- c("Build100m", "RoadDistDecay", "Nat100m", "Mod100m", "Mow100m", "Com100m", "Res100m")}
  if(i==2){tests <- c("Build200m", "RoadDistDecay", "Nat200m", "Mod200m", "Mow200m", "Com200m", "Res200m")}
  if(i==3){tests <- c("Build400m", "RoadDistDecay", "Nat400m", "Mod400m", "Mow400m", "Com400m", "Res400m")}
  if(i==4){tests <- c("Build800m", "RoadDistDecay", "Nat800m", "Mod800m", "Mow800m", "Com800m", "Res800m")}
  if(i==5){tests <- c("Build1600m", "RoadDistDecay", "Nat1600m", "Mod1600m", "Mow1600m", "Com1600m", "Res1600m")}
  
  # Create a data frame to store the results from all the models.
  dis_resp_models[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(tests)){
    
    model <- clm(CoyoteResponse ~ resp_dis[,j], # Create the predictive model
                 data = resp_dis)
    
    dis_resp_models[[i]][j,1] <- j # First column is the predictor
    dis_resp_models[[i]][j,2] <- AIC(model) # Second column is improvement in AIC
    
  }
  
  colnames(dis_resp_models[[i]]) <- c("predictor", "AIC")
  rm( model, tests) # Keep workspace clean
}

dis_resp_models <- dplyr::bind_rows(dis_resp_models)




# DISPERSAL SEASON
# top scales for dispersal season
# RoadDistDecay, Mow100m, Build200m, Mod200m, Res100m, Com100m, Nat800m, 
summary(clm(CoyoteResponse ~ RoadDistDecay, data = resp_dis))
summary(clm(CoyoteResponse ~ Mow100m, data = resp_dis))
summary(clm(CoyoteResponse ~ Mod200m, data = resp_dis))
summary(clm(CoyoteResponse ~ Nat800m, data = resp_dis)) # REMOVE, non-sig P val
summary(clm(CoyoteResponse ~ Res100m, data = resp_dis))
summary(clm(CoyoteResponse ~ Com100m, data = resp_dis))
summary(clm(CoyoteResponse ~ Build200m, data = resp_dis))

# PUP REARING SEASON
# top scales for pup rearing season
# Mod400m, Build400m, Res200m, RoadDistDecay, Nat200m, Mow400m, Com1600m
summary(clm(CoyoteResponse ~ RoadDistDecay, data = resp_pup))
summary(clm(CoyoteResponse ~ Mow400m, data = resp_pup))
summary(clm(CoyoteResponse ~ Mod400m, data = resp_pup))
summary(clm(CoyoteResponse ~ Nat200m, data = resp_pup))
summary(clm(CoyoteResponse ~ Res200m, data = resp_pup))
summary(clm(CoyoteResponse ~ Com1600m, data = resp_pup))  # REMOVE, non-sig P val)
summary(clm(CoyoteResponse ~ Build400m, data = resp_pup))

rcorr(as.matrix(resp_pup[,c("RoadDistDecay", "Mow400m", "Mod400m", "Nat200m", "Res200m", "Build400m")]), type = c("spearman"))
# Remove Nat200m because build is better predictor and they are correlated

pup_response_full = clm(CoyoteResponse ~ as.factor(Year)+ RoadDistDecay + Mow400m + Mod400m + Res200m + Build400m, data = resp_pup, na.action="na.fail")
summary(pup_response_full)
pup_dredge_response <- MuMIn::dredge(pup_response_full) 
view(pup_dredge_response)
pup_dredge_results_response <- MuMIn::model.avg(pup_dredge_response, subset=delta < 2)
summary(pup_dredge_results_response)

pup_response_results <- data.frame(
  conf_2.5 = confint(pup_dredge_results_response, level=0.95)[,1],
  odds = coef(pup_dredge_results_response),
  conf_97.5 = confint(pup_dredge_results_response, level=0.95)[,2]
)

pup_response_results = pup_response_results[-c(1:8),]
pup_response_results
pup_response_results$label = factor(pup_response_results$label, labels = c( "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", 
                                                      "Build400", "Mod400", "Mow400", "Res200", "RoadDist"))

ggplot(pup_response_results, aes(x=label, y=odds)) + 
  ylab("\nLog Odds of Change in Ordinal Scale (95% CI)") + xlab("\nSpatial Predictor") +
  theme_classic()+
  theme(
    axis.text.x=element_text(size=15, family="Times"),
    axis.text.y=element_text(size=15, family="Times"), 
    axis.title.y=element_text(size=18, face="bold", family="Times"),
    axis.title.x=element_text(size=18, face="bold", family="Times"),
    legend.title=element_text(size=24, face="bold", family="Times"), 
    legend.text=element_text(size=22, family="Times"), 
    legend.position= "none")+ 
  geom_hline(yintercept=0, linetype=2)+
  geom_errorbar(aes(x=label, ymin=conf_2.5, ymax=conf_97.5), width=0, size=1, position=position_dodge(0.3)) + 
  theme(panel.spacing.x=unit(0, "lines"),
        strip.text=element_text(size=20, face="bold", family="Times"),
        strip.placement = "outside", 
        panel.border = element_rect(fill=NA, color="black"))   

# BREEDIGN SEASON
# top scales for breeding season
# Build100m, RoadDistDecay, Res100m, Mow100m, Nat400m, Mod1600m, Com1600m, 
summary(clm(CoyoteResponse ~ RoadDistDecay, data = resp_bre))
summary(clm(CoyoteResponse ~ Mow100m, data = resp_bre))
summary(clm(CoyoteResponse ~ Mod1600m, data = resp_bre)) # REMOVE, non sig p value
summary(clm(CoyoteResponse ~ Nat400m, data = resp_bre))
summary(clm(CoyoteResponse ~ Res100m, data = resp_bre))
summary(clm(CoyoteResponse ~ Com1600m, data = resp_bre))  # REMOVE, non-sig P val)
summary(clm(CoyoteResponse ~ Build100m, data = resp_bre))










str(response)
response_full = clm(CoyoteResponse ~  Mod200m  + Nat200m +Mow200m + Com200m + Res200m + Season + Year, data = response, na.action="na.fail")
summary(response_full)
dredge_response <- MuMIn::dredge(response_full) 
dredge_results_response <- MuMIn::model.avg(dredge_response, subset=delta < 2)

summary(clm(CoyoteResponse ~  Mod200m + Nat200m  +Mow200m + Com200m + Res200m, # + Season + as.factor(Year), 
            data = response, na.action="na.fail"))
# RES IS WEIRD - CHECK THIS OUT 
summary(dredge_results_response)
str(response)









Responses = Responses %>%
  mutate(CoyoteResponse = as.ordered(CoyoteResponse))%>%
  filter(HumanActivity !="Driving", VulnerableIndividual!="Cat", Year!=2010)
# finding the appropriate scale for each spatial variable
resp_models <- list()
for(i in c(1:5)){ # For each of your five scales (100, 200, 400, 800, 1600)
  # Define the variables you want to test.
  if(i==1){tests <- c("Build100m", "RoadDistDecay", "Nat100m", "Mod100m", "Mow100m", "Com100m", "Res100m")}
  if(i==2){tests <- c("Build200m", "RoadDistDecay", "Nat200m", "Mod200m", "Mow200m", "Com200m", "Res200m")}
  if(i==3){tests <- c("Build400m", "RoadDistDecay", "Nat400m", "Mod400m", "Mow400m", "Com400m", "Res400m")}
  if(i==4){tests <- c("Build800m", "RoadDistDecay", "Nat800m", "Mod800m", "Mow800m", "Com800m", "Res800m")}
  if(i==5){tests <- c("Build1600m", "RoadDistDecay", "Nat1600m", "Mod1600m", "Mow1600m", "Com1600m", "Res1600m")}
  
  # Create a data frame to store the results from all the models.
  resp_models[[i]] <- data.frame()
  
  # For every variable you want to test:
  for(j in c(tests)){
    
    model <- clm(CoyoteResponse ~ Responses[,j], # Create the predictive model
                 data = Responses)
    
    resp_models[[i]][j,1] <- j # First column is the predictor
    resp_models[[i]][j,2] <- AIC(model) # Second column is improvement in AIC
    
  }
  
  colnames(resp_models[[i]]) <- c("predictor", "AIC")
  rm( model, tests) # Keep workspace clean
}

resp_models <- dplyr::bind_rows(resp_models)


# TOP VARIABLES
# Build200m, Res100m, Mod400m, Nat800m, Com1600m, Mow100m
summary(clm(CoyoteResponse ~ RoadDistDecay, data = Responses))
summary(clm(CoyoteResponse ~ Mow100m, data = Responses))
summary(clm(CoyoteResponse ~ Mod200m, data = Responses)) 
summary(clm(CoyoteResponse ~ Nat800m, data = Responses))
summary(clm(CoyoteResponse ~ Res100m, data = Responses))
summary(clm(CoyoteResponse ~ Com1600m, data = Responses)) 
summary(clm(CoyoteResponse ~ Build100m, data = Responses))

rcorr(as.matrix(resp_pup[,c("RoadDistDecay", "Mow100m", "Mod200m", "Nat800m", "Res100m", "Build100m", "Com1600m")]), type = c("spearman"))
# corrs: res:roaddist, res:build, REMOVE RES and road dist









summary(clm(CoyoteResponse ~  Mod200m, # + Season + as.factor(Year), 
            data = resp_pup, na.action="na.fail"))
summary(clm(CoyoteResponse ~  Mod200m + Nat200m  +Mow200m + Com200m + Res200m, # + Season + as.factor(Year), 
            data = response, na.action="na.fail"))
summary(clm(CoyoteResponse ~  Mod200m + Nat200m  +Mow200m + Com200m + Res200m, # + Season + as.factor(Year), 
            data = response, na.action="na.fail"))
summary(clm(CoyoteResponse ~  Mod200m + Nat200m  +Mow200m + Com200m + Res200m, # + Season + as.factor(Year), 
            data = response, na.action="na.fail"))




