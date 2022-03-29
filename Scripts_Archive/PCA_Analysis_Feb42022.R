## Trying out a PCA analysis to condense all of our wacko enviro variables into 1 


library(stats)
#install.packages("devtools")
library(devtools)
library(ggfortify)

# Looking at coyote responses to humans
Responses = Coyote_Data %>%
  filter(CoyoteResponse!="Unknown", CoyoteResponse!="0")

Responses_PCA_Data = Responses %>%
  select(c(Nat200m, Res200m, Mow200m, Com200m, Mod200m, Build200m, Roads200m))


dat.pca = prcomp(Responses_PCA_Data,
                 center = TRUE, 
                 scale = TRUE)

autoplot(dat.pca,
         data = Responses,
         loadings = TRUE, 
         loadings.label = TRUE, 
         frame = TRUE,
        colour = "CoyoteResponse",
         loadings.colour = "grey50",
         loadings.label.colour = "black",
         loadings.label.size = 5)+
  theme_classic()



# Looking at human perceptions of coyotes
Percep = Coyote_Data %>%
  filter(HumanPerception!="Unknown", HumanPerception!="Concern")

perc_PCA_Data =Percep %>%
  select(c(Res200m,Nat200m, Mow200m, Mod200m, Com200m))


dat.pca.perc = prcomp(perc_PCA_Data)

autoplot(dat.pca.perc,
         data = Percep,
         loadings = TRUE, 
         loadings.label = TRUE, 
         frame = TRUE,
         colour = "HumanPerception",
         loadings.colour = "grey50",
         loadings.label.colour = "black",
         loadings.label.size = 5)+
  theme_classic()


