neom <- read.csv(file.choose())
str(neom)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(car)

##Goal of graph
##Measuring Ideological difference between democrats and republicans
##Seeing how the last three presidents have affected the difference
##Seeing whether the difference is uniform across representatives across the United states or whether it differs based on region
##Seeing whether the recession affected the difference in any way

nom1 <- fct_collapse(neom$state_abbrev ,Midwest=c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"), 
               Northeast=c("CT", "ME", "MA", "NH","RI","VT","NJ","NY","PA"),
               South=c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
               West=c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA"))
nom1
str(nom1)
neom1 <- cbind(neom, nom1)
str(neom1)

new_theme <- theme_classic()+ theme(plot.title = element_text(hjust=0.5),
                                     plot.subtitle = element_text(hjust=0.5))+
  theme(plot.caption = element_text(face="italic", color="slategray"))
neom1%>%
  mutate(year=(2*congress-1)+1789)%>%
  filter(congress>106 & chamber!="President")%>%
  group_by(year, congress, chamber, nom1)%>%
  summarize(mean_id=mean(nominate_dim1[party_code=="200"], na.rm=TRUE)-mean(nominate_dim1[party_code=="100"], na.rm=TRUE))%>%
  ggplot(., aes(x=year, y=mean_id, col=chamber))+
  geom_line()+
  facet_grid(.~nom1)+
  geom_rect(aes(xmin=2007,xmax=2010,ymin=-Inf,ymax=Inf,fill="Economic Recession"),colour=NA,alpha=0.05)+
  geom_vline(xintercept=c(2002,2008,2017), linetype="dotted")+
  xlab("Timeline")+
  ylab("Distance between party means")+
  scale_x_continuous(breaks= c(2002, 2008, 2017), labels=c("Bush","Obama","Trump"))+
  labs(title="The mean ideological difference between Democrats and Republicans in Congress", 
       subtitle="Under the last three Presidents by US Region*",
       caption="Source:https://voteview.com\n*US Regions defined by Census Bureau",
       col="Chamber")+
  scale_fill_manual('',
                    values = 'pink',  
                    guide = guide_legend(override.aes = list(alpha = 1)))+
  new_theme
  




  
  