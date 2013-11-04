# Set working directoty locally. 
setwd("~/Documents/Programming/Crisi-and-Risk-Indexes_Study")

# Renaming the col from ISO3 to iso3. 
colnames(inform)[1] <- "iso3"

#### Comparing Humanitarian and Risk Indexes. ####
library(ggplot2)
library(WDI)
library(countrycode)

#### Todo ####
# 1. Get the whole data from inform not only the risk index. 


#### Loading four datasets. ####

echo <- read.csv("data/echo_gna_index.csv", header=T)
gfm <- read.csv("data/global_focus_model_2013.csv", header=T)
inform <- read.csv("data/inform_risk_index_2013.csv", header=T)
unu <- read.csv("data/unu_world_risk_index.csv", header=T)


#### Adding ISO3 Numbers to all datasets. ####

iso3 <- countrycode(gfm$Country, "country.name", "iso3c")
gfm <- cbind(iso3,gfm)


#### Creating comarable data.frame based on iso3 codes. ####

data <- merge(echo, gfm, by='iso3')
data <- merge(data, inform, by='iso3')
data <- merge(data, unu, by='iso3')


#### Plotting ####


## GFM and InfoRM ## 
cor(data$RISK, data$Risk, use="pairwise.complete.obs") ## 0.87


## Scatterplot that uses colors to separate groups. Both the InfoRM and the GFM have similar numbers, presenting a correlation of 0.87. Could this indicate that the private datasets held by Mapplecroft weren't as valuable as we originally thought? 
ggplot(data, aes(x=RISK, y=Risk)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=Region, size=Focus)) + 
  labs(x = "Global Focus Model 2013", 
       y = "InfoRM 2013", 
       title = "GFM vs. InfoRM (2013)"
       ) + 
  annotate("text", 
           x = 2.3, 
           y = 8, 
           label = "R (correlation) = 0.91", 
           fontface = "italic", 
           size = 4
           ) 

## Plot 2 - UNU vs. InfoRM

cor(data$WorldRiskIndex, data$Risk, use='pairwise.complete.obs') ## 0.1744695

ggplot(data, aes(x=WorldRiskIndex, y=Risk)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=Region, size=Focus)) + 
  labs(x = "UNU World Risk Index", 
       y = "InfoRM 2013", 
       title = "GFM vs. InfoRM (2013)"
  ) + 
  annotate("text", 
           x = 26, 
           y = 7, 
           label = "R (correlation) = 0.8", 
           fontface = "italic", 
           size = 4
  ) 


## Plot 2 - UNU vs. InfoRM

cor(data$GNA.Final.Index, data$Risk, use='pairwise.complete.obs') ## -0.04482483

ggplot(data, aes(x=GNA.Final.Index, y=Risk)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=Region, size=Focus)) + 
  labs(x = "ECHO GNA Risk Index", 
       y = "InfoRM 2013", 
       title = "GFM vs. InfoRM (2013)"
  ) + 
  annotate("text", 
           x = 26, 
           y = 7, 
           label = "R (correlation) = 0.8", 
           fontface = "italic", 
           size = 4
  ) 

## Plot 3 - UNU vs. GFM

cor(data$GNA.Final.Index, data$Risk, use='pairwise.complete.obs') ## -0.04482483

ggplot(data, aes(x=GNA.Final.Index, y=RISK)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=Region, size=Focus)) + 
  labs(x = "ECHO GNA Risk Index", 
       y = "GFM", 
       title = "ECHO GNA vs. GFM (2013)"
  ) + 
  annotate("text", 
           x = 26, 
           y = 7, 
           label = "R (correlation) = ###", 
           fontface = "italic", 
           size = 4
  ) 

  
## Plot 4 - UNU vs. InfoRM

ggplot(data, aes(x=WorldRiskIndex, y=RISK)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=Region, size=Focus)) + 
  labs(x = "ECHO GNA Risk Index", 
       y = "GFM", 
       title = "GFM vs. InfoRM (2013)"
  ) + 
  annotate("text", 
           x = 26, 
           y = 7, 
           label = "R (correlation) = ###", 
           fontface = "italic", 
           size = 4
  ) 
