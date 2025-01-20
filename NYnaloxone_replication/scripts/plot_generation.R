library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)
library(fixest)
library(stringr)
library(viridis)
library(scales)
library(RWmisc)  
##Load cleaned data
final_ny_data <- read_csv("original_data/final_ny_data.csv")

#################### FIGURE 1 #############################
#Generate average per county opioid deaths timeseries plots
death_count_plot<-ggplot(final_ny_data[final_ny_data$agent_type=="COOP",] %>%
                           rename(`Pain Relievers`=death_counts_pain_relievers,
                                  `All Opioids`=death_counts_all_opioids,
                                  Heroin=death_counts_heroin) %>%
                           pivot_longer(cols=c(`Pain Relievers`,
                                               `All Opioids`,
                                               Heroin),
                                        names_to = "Opioid Type",
                                        values_to = "Opioid Deaths") %>% group_by(date,`Opioid Type`) %>%
                           summarise(se=sd(`Opioid Deaths`)/sqrt(length(`Opioid Deaths`)),
                                     t.score=qt(p=0.05/2,df=length(`Opioid Deaths`)-1,lower.tail = F),
                                     margin.error=t.score*se,
                                     tot_trends=mean(`Opioid Deaths`),
                                     lower.bound=ifelse(is.na(se), tot_trends,
                                                        ifelse(tot_trends-margin.error<0,0,
                                                               tot_trends - margin.error)),
                                     upper.bound = ifelse(is.na(se),tot_trends,tot_trends+margin.error)),
                         aes(x=date,group=`Opioid Type`))  +
  geom_ribbon(aes(ymin= lower.bound,
                  ymax= upper.bound,
                  fill=`Opioid Type`),alpha=0.2,lwd=0.01)+
  geom_line(aes(y=tot_trends,color=`Opioid Type`),lwd=1.25,alpha=0.75)+
  ggtitle ("Opioid-Related Deaths") +
  xlab("Quarter") +  ylab ("Deaths, per county-quarter")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = as.numeric(as.Date("2017-07-01")), lwd=1, linetype=4)

crude_rate_plot<-ggplot(final_ny_data[final_ny_data$agent_type=="COOP",] %>% 
                          rename(`Pain Relievers`=death_rate_pain_relievers,
                                 `All Opioids`=death_rate_all_opioids,
                                 Heroin=death_rate_heroin) %>%
                          pivot_longer(cols=c(`Pain Relievers`,
                                              `All Opioids`,
                                              Heroin),
                                       names_to = "Opioid Type",
                                       values_to = "Opioid Deaths") %>% group_by(date,`Opioid Type`) %>%
                          summarise(se=sd(`Opioid Deaths`)/sqrt(length(`Opioid Deaths`)),
                                    t.score=qt(p=0.05/2,df=length(`Opioid Deaths`)-1,lower.tail = F),
                                    margin.error=t.score*se,
                                    tot_trends=mean(`Opioid Deaths`),
                                    lower.bound=ifelse(is.na(se), tot_trends,
                                                       ifelse(tot_trends-margin.error<0,0,
                                                              tot_trends - margin.error)),
                                    upper.bound = ifelse(is.na(se),tot_trends,tot_trends+margin.error)),
                        aes(x=date,group=`Opioid Type`))  +
  geom_ribbon(aes(ymin= lower.bound,
                  ymax= upper.bound,
                  fill=`Opioid Type`),alpha=0.2,lwd=0.01)+
  geom_line(aes(y=tot_trends,color=`Opioid Type`),lwd=1.25,alpha=0.75)+
  ggtitle ("Crude Rate for Opioid-Related Deaths") +
  xlab("Quarter") +  ylab ("Deaths per 100k")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = as.numeric(as.Date("2017-07-01")), lwd=1, linetype=4)

#Plot death-rate series together
(crude_rate_plot/death_count_plot)+
  plot_layout(guides = "collect") & theme(legend.position = 'right')

#################### FIGURE 2 #############################
##Generate Naloxone Admin Plot
naloxone_admin_plot<-ggplot(final_ny_data %>%
         rename(`First Responder\nType`=agent_type) %>%
         group_by(date,`First Responder\nType`) %>%
         summarise(se=sd(naloxone_count)/sqrt(length(naloxone_count)),
                   t.score=qt(p=0.05/2,df=length(naloxone_count)-1,lower.tail = F),
                   margin.error=t.score*se,
                   tot_trends=mean(naloxone_count),
                   lower.bound=ifelse(is.na(se), tot_trends,
                                      ifelse(tot_trends-margin.error<0,0,
                                             tot_trends - margin.error)),
                   upper.bound = ifelse(is.na(se),tot_trends,tot_trends+margin.error)),
       aes(x=date,group=`First Responder\nType`))  +
  geom_ribbon(aes(ymin= lower.bound,
                  ymax= upper.bound,
                  fill=`First Responder\nType`),alpha=0.2,lwd=0.01)+
  geom_line(aes(y=tot_trends,color=`First Responder\nType`),lwd=1.25,alpha=0.75)+
  ggtitle ("Naloxone Administration by First Responder Type") +
  xlab("Quarter") +  ylab ("Unique Administrations Per County")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept = as.numeric(as.Date("2017-07-15")), lwd=1, linetype=4)
#Plot series
naloxone_admin_plot

#################### FIGURE 3 #############################
##Generate Event Studies
#PANEL (A):
iplot(feglm(naloxone_count ~ i(time_to_treat_alt, treat_alt, ref = -1)+
              equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings| ## Our key interaction: time × treatment status
              county.f[period]+county.f[(period)^2]+county.f^year+year+county.f^quarter+agent_type_f,
            cluster = ~county.f,          ## Clustered SEs
            data = final_ny_data[final_ny_data$agent_type %in% c("LEO","COOP"),],
            family = quasipoisson),
      xlab = 'Time to treatment',
      ylab = 'Coefficient estimates',
      main = 'Event study - LEO Treated',
      pt.join = TRUE)


#PANEL (B):
iplot(feglm(naloxone_count ~ i(time_to_treat_alt, treat_alt, ref = -1)+
              equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings| ## Our key interaction: time × treatment status
              county.f[period]+county.f[(period)^2]+county.f^year+year+county.f^quarter+agent_type_f,
            cluster = ~county.f,          ## Clustered SEs
            data = final_ny_data,
            family = quasipoisson),
      xlab = 'Time to treatment',
      ylab = 'Coefficient estimates',
      main = 'Event study - LEO and EMS Treated',
      pt.join = TRUE)

#################### FIGURE 4 #############################
##Plotting heterogeneous treatment effects

#Sample to only those with at least one admin/year/agent type
annual_naloxone<-final_ny_data[final_ny_data$post ==1,] %>% group_by(county,treat_alt) %>%
  summarise(naloxone_count=sum(naloxone_count))
drop_cos<-unique(annual_naloxone$county[annual_naloxone$naloxone_count==0])
final_ny_data_drop<-final_ny_data[!(final_ny_data$county %in% drop_cos),]

#Correct post indicator
final_ny_data_drop$post<-ifelse(final_ny_data_drop$period>9, 1, 0)

#load NY counties shapefile
counties_ny <- counties("New York", cb= TRUE, year = 2014)

#Estimate heterogenous, county-level treatment effects
final_ny_data_drop$agent_type_f<-as.factor(final_ny_data_drop$agent_type_f)
final_ny_data_drop<-final_ny_data_drop %>%
  mutate(agent_type_f = relevel(agent_type_f,c("LEO"))) %>% 
  mutate(agent_type_f = relevel(agent_type_f,c("COOP")))
hetero_mod<-feglm(naloxone_count ~ post*treat_alt:county.f |
                    county.f^agent_type_f + county.f^period,            ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data_drop,
                  family = quasipoisson)

#Extract treatment effects and estimate effect magnitudes for each county
hetero_coefs<-data.frame(coefs=as.numeric(hetero_mod$coeftable[1:53,1]),
                         county=substr(rownames(hetero_mod$coeftable)[1:53],24,
                                       str_length(rownames(hetero_mod$coeftable)[1:53])))
pre_treat_means<-final_ny_data[final_ny_data$post==1,] %>% group_by(county, treat_alt) %>%
  summarise(treated_tot=sum(naloxone_count))
pre_treat_means<-final_ny_data[final_ny_data$post==1,] %>% group_by(county) %>%
  summarise(cum_tot=sum(naloxone_count)) %>%
  merge(pre_treat_means,.,by="county")
pre_treat_means<-merge(pre_treat_means,hetero_coefs,by=c("county"))
pre_treat_means$delta<-(exp(-pre_treat_means$coef)-1)*pre_treat_means$treated_tot
pre_treat_means$delta<-ifelse((-pre_treat_means$delta)>pre_treat_means$treated_tot,
                              -pre_treat_means$treated_tot,pre_treat_means$delta)
pre_treat_means$counter_fact<-pre_treat_means$cum_tot+pre_treat_means$delta
hetero_treatment<-pre_treat_means[pre_treat_means$treat_alt==1,] %>% group_by(NAME=county) %>%
  summarise(percent_change=100*((cum_tot/counter_fact)-1))

#Collect shapefiles for map
counties_NA<-counties_ny[counties_ny$NAME %in% final_ny_data$county,]
counties_ny<-counties_ny[counties_ny$NAME %in% hetero_coefs$county,]
counties_ny<-merge(counties_ny,hetero_treatment, all.x=TRUE)
state_ny<-states(cb= TRUE, year = 2014)
state_ny<-state_ny[state_ny$STUSPS=='NY',]
urban_ny<-urban_areas(cb= TRUE, year = 2014)
ny_cities<-c('Buffalo','Rochester','Syracuse','Albany','New Rochelle',
             'Mount Vernon','Schenectady','Utica')
urban_ny<-urban_ny[str_detect(urban_ny$NAME10,"NY"),]
urban_ny<-st_intersection(state_ny, urban_ny)

#Generate map
hetero_map<-ggplot()+
  geom_sf(data=counties_NA, fill='grey')+
  geom_sf(data=counties_ny, aes(fill = percent_change))+
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = muted("blue"),
    midpoint = 0,
    limits=c(-125,max(hetero_treatment$percent_change)),
    breaks=c(-100,-10,0,10,400),
    trans='pseudo_log',
    name='Naloxone\nAdministrations,\nPercent Change'
  )+
  theme_rw() +                        # clean plot
  theme(axis.text = element_blank(),  # no lat/long values
        axis.ticks = element_blank()) + # no lat/long ticks
  geom_sf(data = state_ny,fill = NA,col="Black",size=1.25)+
  geom_sf(data = urban_ny,fill = 'black',alpha=0.35,col=NA,size=1.25)+
  theme(text=element_text(size=20))

#Plot map
hetero_map