#### Comparing Humanitarian and Risk Indexes. ####
library(ggplot2)
library(WDI)
library(countrycode)

#### Comparison of the year 2013 ####

inform <- read.csv("data/inform_risk_index_2013.csv", header=T)
echo_crisis <- read.csv("data/echo_gna_crisis_index.csv", header=T)
echo_vulnerability <- read.csv("data/echo_hna_vulnerability.csv", header=T)
gfm <- read.csv("data/gfm_2013.csv", header=T)
unu <- read.csv("data/UNU_World_Index.csv", header=T)


#### Adding ISO3 Numbers to all databases. 

iso3 <- countrycode(gfm$Country, "country.name", "iso3c")
iso3 <- countrycode(unu$Country, "country.name", "iso3c")
unu <- cbind(iso3,unu)


#### Making the same plot (and correlations) available in the World Humanitarian Data Report. #### 


## GFM and InfoRM ## 

gfm_new <- subset(gfm, gfm$iso3, gfm$Focus)

write.csv(inform, file="inform_with_iso3.csv", row.names=FALSE)
write.csv(gfm, file="gfm_with_iso3.csv", row.names=FALSE)


cor(gfm_inform$InfoRM.Risk.Index, gfm_inform$Focus, use="pairwise.complete.obs") ## 0.902207


## Scatterplot that uses colors to separate groups. 
ggplot(gfm_inform, aes(x=InfoRM.Risk.Index, y=Focus)) + 
  geom_text(aes(label=ISO3), 
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
  

cor(gfm_inform$Vulnerability.Index, gfm_inform$Vulnerability, use="pairwise.complete.obs") ## 0.8474009

ggplot(gfm_inform, aes(x=Vulnerability.Index, y=Vulnerability)) + 
  geom_point() + 
  geom_text(aes(label=ISO3), 
            size = 2.5,
            vjust = 0,
            position = position_jitter(w = 0.5, h = 0.5)
  ) + 
  stat_smooth(method="lm", se=FALSE) + 
  labs(x = "Global Focus Model 2013 / Vulnerability", y = "InfoRM 2013 / Vulnerability Index") + 
  theme_bw()




cor(echo_crisis$GNA.Crisis.Index, echo_vulnerability$GNA.Vulnerability.Index, use="pairwise.complete.obs") ## 0.3115115

cor(gfm$Focus, inform$InfoRM.Risk.Index, use="pairwise.complete.obs") ## Not working -- check data. 

qqplot(gfm$Focus, inform$InfoRM.Risk.Index, na.rm = TRUE) + geom_point()





#### Comparing GNA and UNU World Risk Index #### 

cor(gna_unu$WorldRiskIndex, gna_unu$GNA.Vulnerability.Index) ## R (correlation) = 0.08082911

cor(gna_unu$Vulnerability, gna_unu$GNA.Vulnerability.Index) ## R (correlation) = 0.7935976

cor(echo_unu$WorldRiskIndex, echo_unu$GNA.Final.Index) ## R (correlation) = -0.06607858

ggplot(echo_unu, aes(x=GNA.Final.Index, y=WorldRiskIndex)) + 
  geom_text(aes(label=iso3), 
            size = 2.5, fontface = "bold",
            vjust = 0,
            color = "grey",
            position = position_jitter(w = 0.4, h = 0.4)) + 
  # stat_smooth(method="lm", se=FALSE) + 
  geom_point(aes(color=WorldRiskIndex, size=2)) + 
  labs(x = "GNA Final Index", 
       y = "UNU World Risk Index", 
       title = "ECHO GNA vs. UNU"
  ) + 
  annotate("text", 
           x = 3, 
           y = 30, 
           label = "R (correlation) = -0.67", 
           fontface = "italic", 
           size = 4
  ) 


