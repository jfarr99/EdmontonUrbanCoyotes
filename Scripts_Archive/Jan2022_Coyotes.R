setwd("C:/Users/Jonathan Farr/OneDrive - ualberta.ca/Desktop/Urban Coyote Project")
data=read.csv("JJFCoyotesMarch82021.csv")


library(tidyverse)
library(lubridate)
library(viridis)
str(covid)

covid = data %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(covid = ifelse(Date > "2020-04-01", "Y", "N"), 
         CoyoteResponse = as.ordered(CoyoteResponse), 
         HumanPerception = as.ordered(HumanPerception),
         MO100m = ag100m + oNm100m,
         MO200m = ag200m + oNm200m,
         MO400m = ag400m + oNm400m,
         MO800m = ag800m + oNm800m,
         MO1600m = ag1600m + oNm1600m)%>%
  filter(Date > "2010-12-31")
  


time_barplot_response = covid %>%
  filter(CoyoteResponse != "Unknown") %>%
  group_by(fourmonth_total = floor_date(Date, "4 months"), CoyoteResponse) %>%
  summarise(count=n())%>%
  mutate(prop = count/sum(count))

ggplot(time_barplot_response, aes(x=fourmonth_total, y = count, fill = CoyoteResponse))+ 
  geom_bar(stat="identity", position="stack") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_fill_viridis(option="plasma", discrete=TRUE) + 
  theme_bw() + xlab("Date (4-month intervals)") + ylab("Number of coyote reports")+
  scale_y_continuous(expand=c(0,0)) + 
  geom_vline(xintercept =as.Date(c("2015-10-31", "2014-10-31","2011-10-31", "2012-10-31","2013-10-31", 
                                   "2016-10-31","2017-10-31", "2018-10-31", "2019-10-31", "2020-10-31")),
             linetype="dashed", size = 1)


library(ordinal)
response = covid %>%
  filter(CoyoteResponse != "Unknown", CoyoteResponse != 0)


summary(response$CoyoteResponse)
str(response)
response_full = clm(CoyoteResponse ~ RoadDistDecay + MO200m  + nat200m + om200m + com200m + res200m + Season + covid + Year, 
                    data = response, na.action="na.fail")

summary(response_full)
dredge_response <- MuMIn::dredge(response_full) 
view(dredge_response)
dredge_results_response <- MuMIn::model.avg(dredge_response, subset=delta < 2)
summary(dredge_results_response)

response_results <- data.frame(
  conf_2.5 = confint(dredge_results_response, level=0.99)[,1],
  odds = coef(dredge_results_response),
  conf_97.5 = confint(dredge_results_response, level=0.99)[,2]
)

response_results = response_results[-c(1:8),]
response_results$label = rownames(response_results)

ggplot(response_results, aes(x=label, y=odds)) + 
  ylab("\nLog Odds of Change in Ordinal Scale (99% CI)") + xlab("\nSpatial Predictor") +
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




time_barplot_percep = covid %>%
  dplyr::filter(HumanPerception !="Unknown")%>%
  dplyr::group_by(fourmonth_total = floor_date(Date, "4 months"), HumanPerception) %>%
  dplyr::summarise(count=n())%>%
  dplyr::mutate(prop = count/sum(count))


ggplot(subset(time_barplot_percep, HumanPerception != "Unknown"), aes(x=fourmonth_total, y = count, fill = HumanPerception))+ 
  geom_bar(stat="identity", position="stack") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_fill_viridis(option="magma", discrete=TRUE)



