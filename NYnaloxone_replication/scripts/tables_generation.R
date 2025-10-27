library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(tidyverse)
##Load cleaned data
final_ny_data <- read_csv("original_data/final_ny_data.csv")

#Set Post indicator for all observations after East Liverpool event (Q2 2017) 
final_ny_data$post_treat<-ifelse(final_ny_data$period>9,1,0)

########################### Table 1 #########################################
#Generate summary stats table
library(stargazer)

##Create wide DF
final_ny_data_wide<-final_ny_data[,c("county","period","agent_type","naloxone_count","death_counts_all_opioids",
                                     "death_counts_heroin","death_counts_pain_relievers",
                                     "equifax",
                                     "pp_medicaid_enroll","unemployment","pp_constr_employ",
                                     "avg_earnings")]
final_ny_data_wide<-pivot_wider(final_ny_data_wide,names_from = agent_type,values_from=naloxone_count)
final_ny_data_wide[c("LEO","EMS","COOP","death_counts_all_opioids",
                     "death_counts_heroin","death_counts_pain_relievers",
                     "equifax",
                     "pp_medicaid_enroll","unemployment","pp_constr_employ",
                     "avg_earnings")]
##Select and reorder key variables
final_ny_data_wide<-final_ny_data_wide[,c("LEO","EMS","COOP","death_counts_all_opioids",
                                          "death_counts_heroin","death_counts_pain_relievers",
                                          "equifax",
                                          "pp_medicaid_enroll","unemployment","pp_constr_employ",
                                          "avg_earnings")]

#Generate/output summary stats table
stargazer(as.data.frame(final_ny_data_wide),
          summary.stat = c("n","mean","median","sd"),
          covariate.labels =c(
            "LEO Naloxone Administrations",
            "EMS Naloxone Administrations",
            "COOP Naloxone Administrations",
            "Deaths (All Opioids)",
            "Deaths (Heroin)",
            "Deaths (Other Opioids)",
            "% Pop. Sub-prime Credit",
            "% Pop. Medicaid",
            "Unemployment Rate",
            "% Labor Force in Construction",
            "Average HH Earnings"
          ),
          summary = TRUE, type="text")

########################### Table 2 #########################################
#Estimate perferred model with LEOs as treated against COOPs
mod_did1=feglm(naloxone_count ~ post_treat*treat|
                 year+quarter+agent_type_f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","COOP"),],
               family = quasipoisson)
mod_did2=feglm(naloxone_count ~ post_treat*treat + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                 agent_type_f,           ## FEs
               cluster = ~~county,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","COOP"),],
               family = quasipoisson)
mod_did3=feglm(naloxone_count ~ post_treat*treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 county.f^period+county.f+agent_type_f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","COOP"),],
               family = quasipoisson)
mod_did4=feglm(naloxone_count ~ post_treat*treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 year+county.f^period+agent_type_f^county.f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","COOP"),],
               family = quasipoisson)
##Generate table
etable(list(mod_did1,mod_did2, mod_did3,mod_did4))

########################### Table 3 #########################################
#Estimate alternative models where both LEOs and EMS are treated against COOPS
mod_did_alt1=feglm(naloxone_count ~ post_treat*treat_alt|
                     year+quarter+agent_type_f,           ## FEs
                   cluster = ~county.f,  ## Clustered SEs
                   data = final_ny_data,
                   family = quasipoisson)
mod_did_alt2=feglm(naloxone_count ~ post_treat*treat_alt + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                     county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                     agent_type_f,           ## FEs
                   cluster = ~~county,  ## Clustered SEs
                   data = final_ny_data,
                   family = quasipoisson)
mod_did_alt3=feglm(naloxone_count ~ post_treat*treat_alt + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                     county.f^period+county.f+agent_type_f,           ## FEs
                   cluster = ~county.f,  ## Clustered SEs
                   data = final_ny_data,
                   family = quasipoisson)
mod_did_alt4<-feglm(naloxone_count ~ post_treat*treat_alt + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                      year+county.f^period+agent_type_f^county.f,           ## FEs
                    cluster = ~county.f,  ## Clustered SEs
                    data = final_ny_data,
                    family = quasipoisson)
##Generate table
etable(list(mod_did_alt1,mod_did_alt2,mod_did_alt3,
            mod_did_alt4))

########################### Table 4 #########################################
###################Robustness checks
## PANEL (A): Pop. Weighted
etable(list(feglm(naloxone_count ~ post_treat*agent_type_f|
                    year+quarter+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(naloxone_count ~ post_treat*agent_type_f + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                    agent_type_f,           ## FEs
                  cluster = ~~county,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(naloxone_count ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f^period+county.f+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(naloxone_count ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    year+county.f^period+agent_type_f^county.f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population)),
       keep = c("post_treat x agent_type_fEMS","post_treat x agent_type_fLEO"),
       drop.section	= c("fixef","slopes"))

## PANEL (B): Naloxone per death
etable(list(feglm(nalonxe_per_death ~ post_treat*agent_type_f|
                    year+quarter+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(nalonxe_per_death ~ post_treat*agent_type_f + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                    agent_type_f,           ## FEs
                  cluster = ~~county,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(nalonxe_per_death ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f^period+county.f+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population),
            feglm(nalonxe_per_death ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    year+county.f^period+agent_type_f^county.f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  family = quasipoisson,
                  weights = final_ny_data$population)),
       keep = c("post_treat x agent_type_fEMS","post_treat x agent_type_fLEO"),
       drop.section	= c("fixef","slopes"))

## PANEL (C): OLS
etable(list(feols(naloxone_count ~ post_treat*agent_type_f|
                    year+quarter+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  weights = final_ny_data$population),
            feols(naloxone_count ~ post_treat*agent_type_f + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                    agent_type_f,           ## FEs
                  cluster = ~~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  weights = final_ny_data$population),
            feols(naloxone_count ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    county.f^period+county.f+agent_type_f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  weights = final_ny_data$population),
            feols(naloxone_count ~ post_treat*agent_type_f + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                    year+county.f^period+agent_type_f^county.f,           ## FEs
                  cluster = ~county.f,  ## Clustered SEs
                  data = final_ny_data,
                  weights = final_ny_data$population)),
       keep = c("post_treat x agent_type_fEMS","post_treat x agent_type_fLEO"),
       drop.section	= c("fixef","slopes"))

########################### Table 4 #########################################
## PANEL (A): Media dosage model
etable(feglm(naloxone_count ~ post_treat:X2017_pct_bb_covrg:factor(agent_type,levels = c("LEO","EMS","COOP"))+
               equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
               county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
               agent_type_f,          ## FEs
             cluster = ~county.f,  ## Clustered SEs
             data = final_ny_data,
             family = quasipoisson),
       drop = c("equifax","pp_medicaid_enroll","unemployment","pp_constr_employ",
                "avg_earnings"),
       drop.section	= c("fixef","slopes"))

## PANEL (B): Pharmacy standing order analysis
##Load pharmacy standing order participation data
standing_order <- read_csv("original_data/standing_order.csv")
#Merge with NY data
final_ny_data<-merge(final_ny_data,standing_order,all.x=TRUE, by=c("county","year"))
final_ny_data$pharms_per100k<-(final_ny_data$nalo_pharms/final_ny_data$population)*100000
#Set number of participating pharmacies to 0 before adoption of standing orders
final_ny_data$pharms_per100k[is.na(final_ny_data$pharms_per100k)]<-0

#Estimate model
#(note: because pharmacy data is recorded at annual frequency, CountyxYear FE
#is collinear- instead, we use a CountyxQuarterOfYear FE to absorb some of this lost effect)
esttex(feglm(naloxone_count ~ pharms_per100k:factor(agent_type,levels = c("LEO","EMS","COOP"))+
               equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
               county.f[period]+county.f[(period)^2]+county.f^quarter+year+quarter+
               agent_type_f,          ## FEs
             cluster = ~~county.f,  ## Clustered SEs
             data = final_ny_data,
             family = quasipoisson,
             glm.iter	= 50),
       drop = c("equifax","pp_medicaid_enroll","unemployment","pp_constr_employ",
                "avg_earnings"),
       drop.section	= c("fixef","slopes"))

########################### Table A1 #########################################
#Generate centered period measure to reduce collinearity between post_treat and the trend
final_ny_data$period_c <- with(final_ny_data, period - mean(period, na.rm = TRUE))

# LEO-only ITS
its_leo_lin <- feglm(
  naloxone_count ~ post_treat + period_c + post_treat:period_c +
    equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
    county.f + quarter,
  cluster = ~ county.f,
  family  = quasipoisson,
  data    = final_ny_data[final_ny_data$agent_type %in% "LEO", ]
)

# EMS-only
its_ems_lin <- feglm(
  naloxone_count ~ post_treat + period_c + post_treat:period_c +
    equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
    county.f + quarter,
  cluster = ~ county.f,
  family  = quasipoisson,
  data    = final_ny_data[final_ny_data$agent_type %in% "EMS", ]
)

# COOP-only
its_coop_lin <- feglm(
  naloxone_count ~ post_treat + period_c + post_treat:period_c +
    equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
    county.f + quarter,
  cluster = ~ county.f,
  family  = quasipoisson,
  data    = final_ny_data[final_ny_data$agent_type %in% "COOP", ]
)

etable(list("LEO ITS"     = its_leo_lin,
            "EMS ITS"     = its_ems_lin,
            "COOP ITS"    = its_coop_lin),
       drop = c("equifax","pp_medicaid_enroll","unemployment",
                "pp_constr_employ","avg_earnings"),
       tex  = TRUE)

########################### Table A2 #########################################
#Estimate perferred model with LEOs as treated against EMS
mod_did_ems1=feglm(naloxone_count ~ post_treat*treat|
                 year+quarter+agent_type_f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","EMS"),],
               family = quasipoisson)
mod_did_ems2=feglm(naloxone_count ~ post_treat*treat + post_treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 county.f[period]+county.f[(period)^2]+county.f^year+year+quarter+
                 agent_type_f,           ## FEs
               cluster = ~~county,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","EMS"),],
               family = quasipoisson)
mod_did_ems3=feglm(naloxone_count ~ post_treat*treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 county.f^period+county.f+agent_type_f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","EMS"),],
               family = quasipoisson)
mod_did_ems4=feglm(naloxone_count ~ post_treat*treat + equifax + pp_medicaid_enroll + unemployment + pp_constr_employ + avg_earnings |
                 year+county.f^period+agent_type_f^county.f,           ## FEs
               cluster = ~county.f,  ## Clustered SEs
               data = final_ny_data[final_ny_data$agent_type %in% c("LEO","EMS"),],
               family = quasipoisson)
##Generate table
etable(list(mod_did_ems1,mod_did_ems2, mod_did_ems3,mod_did_ems4),
       drop = c("equifax","pp_medicaid_enroll","unemployment",
                "pp_constr_employ","avg_earnings"), tex = TRUE)
