library(tidyverse)
source("functions_sev.R")

ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}
############################# loading data ###################################

data <- read.csv("input/data/HTR_2019_round1_merged_cleaned_master_2019-09-24.csv", stringsAsFactors = F, na.strings = c("", "NA"))

weight<-read.csv("input/weights/REACH_AFG_H2R_R1_weights_0924.csv", stringsAsFactors = F, na.strings = c("", "NA"))

#############################################################################
#### EDUCATION

# create variables

# % of assessed settlements in which children are not able to attend school
data$school_attend<-case_when(data$edu_boys=='no'|data$edu_girls=='no'~ 1,TRUE~0)

# % of assessed settlements in which at least one chil was reportedly working instead of attending school
# data$children_no_school<-case_when(data$boys_work_no_school=='yes'|data$girls_work_no_school=='yes'~ 1,TRUE~0)


data$edu_children_bin<-case_when(data$edu_children=='no'~ 1,TRUE~0)

# children removed from school
data$children_removed_bin<-case_when(data$children_removed=='yes'~ 1,TRUE~0)

#group by district
edu_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(total_settlements=n(),
            school_not_attend_perc=sum(school_attend)/n(),
            no_edu_children_perc=sum(edu_children_bin)/n(),
            children_removed_perc=sum(children_removed_bin)/n())

# give weights
edu_sev <-edu_sev %>% 
  mutate(no_edu_children=2*village_threshold_quarters(no_edu_children_perc),
         school_not_attend=2*village_threshold_quarters(school_not_attend_perc),
         children_removed=4*village_threshold_quarters(children_removed_perc))

# edu score
edu_sev$htr_eie_score<-coerc(edu_sev[["no_edu_children"]])+coerc(edu_sev[["school_not_attend"]])+coerc(edu_sev[["children_removed"]])

# edu rank
edu_sev$htr_eie_rank<-car::recode(edu_sev$htr_eie_score,
                             "0:3='1';
                             4:7='2';
                             8:11='3';
                             12:16='4'")   

edu_sev$htr_eie_rank_high<-car::recode(edu_sev$htr_eie_rank,
                                         "1:2='0';
                                         3:4='1'")  

############## Export analysis in CSV format ############
write.csv(edu_sev, "sector_output/HTR_round1_edu_sev.csv")


#############################################################################
#### PROTECTION

#### create variables

# extreme protection incidents
data$protect_incident_extreme<-case_when(data$adult_incidents.assault_weapon>0|data$adult_incidents.forced_work>0|data$adult_incidents.forced_detained>0|data$adult_incidents.forced_recruitment>0|data$adult_incidents.hinder_move>0|data$child_incidents.assault_weapon>0|data$child_incidents.forced_work>0|data$child_incidents.forced_detained>0|data$child_incidents.forced_recruitment>0|data$child_incidents.hinder_move>0~1,TRUE~0)

# severe protection incidents
data$protect_incident_severe<-case_when(data$adult_incidents.verbal_threat>0|data$adult_incidents.assault_no_weapon>0|data$child_incidents.verbal_threat>0|data$child_incidents.assault_no_weapon>0~1,TRUE~0)

# incidents targeting women
data$gbv_incidents<-case_when(data$other_incidents=='targeting_women'~ 1,TRUE~0)

# people injured by conflict or natural disaster in the past 3 months
data$phys_injury<-case_when(data$phys_injury_disaster=='yes'|data$phys_injury_conflict=='yes'~ 1,TRUE~0)

# majority males with no tazkira
data$no_tazkira<-case_when(data$men_tazkira=='account_very_few'~ 1,TRUE~0)

# impact due to the presence of explosive
data$explosive_extreme<-case_when(data$explosive_impact.death_disability==1|data$explosive_impact.constrain_service_access==1~ 1,TRUE~0)

data$explosive_severe<-case_when(data$explosive_impact.livelihood_income==1|data$explosive_impact.psych_wellbeing==1~ 1,TRUE~0)

data$explosive_stress<-case_when(data$explosive_impact.constrain_playing_access==1~ 1,TRUE~0)

# safety rating convert to binary

data$safety_rating_bin<-case_when(data$safety_rating=='very_poor'~1,TRUE~0)


# group by district
prot_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            protect_incident_extreme_perc=sum(protect_incident_extreme)/n(),
            protect_incident_severe_perc=sum(protect_incident_severe)/n(),
            gbv_incidents_perc=sum(gbv_incidents)/n(),
            phys_injury_perc=sum(phys_injury)/n(),
            no_tazkira_perc=sum(no_tazkira)/n(),
            explosive_extreme_perc=sum(explosive_extreme)/n(),
            explosive_severe_perc=sum(explosive_severe)/n(),
            explosive_stress_perc=sum(explosive_stress)/n(),
            safety_rating_perc=sum(safety_rating_bin)/n())

# give weights
prot_sev <-prot_sev %>% 
  mutate(protect_incident_extreme=3*village_threshold_quarters(protect_incident_extreme_perc),
         protect_incident_severe=2*village_threshold_quarters(protect_incident_severe_perc),
         gbv_incidents=3*village_threshold_quarters(gbv_incidents_perc),
         phys_injury=3*village_threshold_quarters(phys_injury_perc),
         no_tazkira=village_threshold_quarters(no_tazkira_perc),
         explosive_extreme=3*village_threshold_quarters(explosive_extreme_perc),
         explosive_severe=2*village_threshold_quarters(explosive_severe_perc),
         explosive_stress=village_threshold_quarters(explosive_stress_perc),
         safety_rating=2*village_threshold_quarters(safety_rating_perc))

# prot score
prot_sev$htr_prot_score<-coerc(prot_sev[["protect_incident_extreme"]])+coerc(prot_sev[["protect_incident_severe"]])+coerc(prot_sev[["gbv_incidents"]])+coerc(prot_sev[["phys_injury"]])+coerc(prot_sev[["no_tazkira"]])+coerc(prot_sev[["explosive_extreme"]])+coerc(prot_sev[["explosive_severe"]])+coerc(prot_sev[["explosive_stress"]])+coerc(prot_sev[["safety_rating"]])

# prot rank
prot_sev$htr_prot_rank<-car::recode(prot_sev$htr_prot_score,
                              "0:10='1';
                             11:20='2';
                             21:30='3';
                             31:40='4'")   


prot_sev$htr_prot_rank_high<-car::recode(prot_sev$htr_prot_rank,
                                       "1:2='0';
                                       3:4='1'")  

############## Export analysis in CSV format ############
write.csv(prot_sev, "sector_output/HTR_round1_prot_sev.csv")

#############################################################################
#### NUTRITION

#### create variables
data$nutrition_no_access<-case_when(data$nutrition_access=='no_too_far'|data$nutrition_access=='no_too_expensive'|data$nutrition_access=='no_not_accessible'~ 1,TRUE~0)

# malnutrition severity
data$malnut_extreme<-case_when(data$malnutrition_severity=='malnour_more_half'~ 1,TRUE~0)

data$malnut_severe<-case_when(data$malnutrition_severity=='malnour_less_half'~ 1,TRUE~0)

data$malnut_stress<-case_when(data$malnutrition_severity=='malnour_very_few'~ 1,TRUE~0)

#group by district
nut_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            nutrition_no_access_perc=sum(nutrition_no_access)/n(),
            malnut_extreme_perc=sum(malnut_extreme)/n(),
            malnut_severe_perc=sum(malnut_severe)/n(),
            malnut_stress_perc=sum(malnut_stress)/n())



# give weights
nut_sev <-nut_sev %>% 
  mutate(nutrition_no_access=3*village_threshold_quarters(nutrition_no_access_perc),
         malnut_extreme=3*village_threshold_quarters(malnut_extreme_perc),
         malnut_severe=2*village_threshold_quarters(malnut_severe_perc),
         malnut_stress=village_threshold_quarters(malnut_stress_perc));

nut_sev$malnutrition_score<-case_when(nut_sev$malnut_extreme_perc>0.495~nut_sev$malnut_extreme,nut_sev$malnut_severe==4~nut_sev$malnut_severe,nut_sev$malnut_extreme==2~nut_sev$malnut_extreme,nut_sev$malnut_severe==2~nut_sev$malnut_severe, TRUE~nut_sev$malnut_stress)

# take one of extreme or severe (if one is max then the other cannot be)

# nut score
nut_sev$htr_nut_score<-coerc(nut_sev[["nutrition_no_access"]])+coerc(nut_sev[["malnutrition_score"]])

# nut rank
nut_sev$htr_nut_rank<-car::recode(nut_sev$htr_nut_score,
                            "0:3='1';
                             4:6='2';
                             7:9='3';
                             10:14='4'")   

nut_sev$htr_nut_rank_high<-car::recode(nut_sev$htr_nut_rank,
                                        "1:2='0';
                                         3:4='1'")  

############## Export analysis in CSV format ############
write.csv(nut_sev, "sector_output/HTR_round1_nut_sev.csv")

#############################################################################
#### FSA

#### create variables
# hunger severity
data$hunger_extreme<-case_when(data$hunger_level=='hunger_worst'~ 1,TRUE~0)
data$hunger_severe<-case_when(data$hunger_level=='hunger_bad'~ 1,TRUE~0)

# borrow severity
data$borrow_extreme<-case_when(data$borrow_food_proportion=='more_half'~ 1,TRUE~0)
data$borrow_severe<-case_when(data$borrow_food_proportion=='half'~ 1,TRUE~0)

# reduced food for children severity
data$reduced_food_extreme<-case_when(data$reduced_food_proportion=='more_half'~ 1,TRUE~0)
data$reduced_food_severe<-case_when(data$reduced_food_proportion=='half'~ 1,TRUE~0)

# livestock impact
data$livestock_impact_extreme<-case_when(data$livestock_negative_impacts=='livestock_died'|data$livestock_negative_impacts=='livestock_left'~ 1,TRUE~0)
data$livestock_impact_severe<-case_when(data$livestock_negative_impacts=='livestock_ill'|data$livestock_negative_impacts=='livestock_less_produce'~ 1,TRUE~0)

# ag impact
data$ag_impact_extreme<-case_when(data$ag_negative_impact=='ag_impact_more_half'~ 1,TRUE~0)
data$ag_impact_severe<-case_when(data$ag_negative_impact=='ag_impact_half'~ 1,TRUE~0)

# group by district
fsac_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            hunger_extreme_perc=sum(hunger_extreme)/n(),
            hunger_severe_perc=sum(hunger_severe)/n(),
            borrow_extreme_perc=sum(borrow_extreme)/n(),
            borrow_severe_perc=sum(borrow_severe)/n(),
            reduced_food_extreme_perc=sum(reduced_food_extreme)/n(),
            reduced_food_severe_perc=sum(reduced_food_severe)/n(),
            livestock_impact_extreme_perc=sum(livestock_impact_extreme)/n(),
            livestock_impact_severe_perc=sum(livestock_impact_severe)/n(),
            ag_impact_extreme_perc=sum(ag_impact_extreme)/n(),
            ag_impact_severe_perc=sum(ag_impact_severe)/n())

# give weights
fsac_sev <-fsac_sev %>% 
  mutate(hunger_extreme=4*village_threshold_quarters(hunger_extreme_perc),
         hunger_severe=2*village_threshold_quarters(hunger_severe_perc),
         borrow_extreme=2*village_threshold_quarters(borrow_extreme_perc),
         borrow_severe=village_threshold_quarters(borrow_severe_perc),
         reduced_food_extreme=2*village_threshold_quarters(reduced_food_extreme_perc),
         reduced_food_severe=village_threshold_quarters(reduced_food_severe_perc),
         livestock_impact_extreme=3*village_threshold_quarters(livestock_impact_extreme_perc),
         livestock_impact_severe=village_threshold_quarters(livestock_impact_severe_perc),
         ag_impact_extreme=2*village_threshold_quarters(ag_impact_extreme_perc),
         ag_impact_severe=village_threshold_quarters(ag_impact_severe_perc))

# fsac score
fsac_sev$htr_fsa_score<-coerc(fsac_sev[["hunger_extreme"]])+coerc(fsac_sev[["hunger_severe"]])+coerc(fsac_sev[["borrow_extreme"]])+coerc(fsac_sev[["borrow_severe"]])+coerc(fsac_sev[["reduced_food_extreme"]])+coerc(fsac_sev[["reduced_food_severe"]])+coerc(fsac_sev[["livestock_impact_extreme"]])+coerc(fsac_sev[["livestock_impact_severe"]])+coerc(fsac_sev[["ag_impact_extreme"]])+coerc(fsac_sev[["ag_impact_severe"]])

# fsac rank
fsac_sev$htr_fsa_rank<-car::recode(fsac_sev$htr_fsa_score,
                                    "0:8='1';
                                    9:18='2';
                                    19:28='3';
                                    29:36='4'")   

fsac_sev$htr_fsa_rank_high<-car::recode(fsac_sev$htr_fsa_rank,
                                         "1:2='0';
                                         3:4='1'")  


############## Export analysis in CSV format ############
write.csv(fsac_sev, "sector_output/HTR_round1_fsa_sev.csv")


#############################################################################
#### WASH

# create variables
# extreme unprotected water source
data$unprotect_water_extreme<-case_when(data$water_source=='sfc_water'~ 1,TRUE~0)
data$unprotect_water_severe<-case_when(data$water_source=='water_trucking'| data$water_source=='unprotected_well'~ 1,TRUE~0)

# waste disposal
data$waste_disposal_severe<-case_when(data$waste_disposal=='street_open_space'|data$waste_disposal=='burning'~ 1,TRUE~0)

# water access
data$water_no_access<-case_when(data$water_access=='very_few' & (data$reason_water_needs=='water_too_far'|data$reason_water_needs=='water_high_risk'|data$reason_water_needs=='water_social_restrictions')~1,TRUE~0)

# soap
data$soap_access_bin<-case_when(data$soap_access=='no'~1,TRUE~0)

# open defecation
# data$open_defecation_bin<-case_when(data$open_defecation=='yes'~1,TRUE~0)

#group by district
wash_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            unprotect_water_extreme_perc=sum(unprotect_water_extreme)/n(),
            unprotect_water_severe_perc=sum(unprotect_water_severe)/n(),
            waste_disposal_severe_perc=sum(waste_disposal_severe)/n(),
            water_no_access_perc=sum(water_no_access)/n(),
            soap_access_perc=sum(soap_access_bin)/n())

# give weights
wash_sev <-wash_sev %>% 
  mutate(unprotect_water_extreme=3*village_threshold_quarters(unprotect_water_extreme_perc),
         unprotect_water_severe=2*village_threshold_quarters(unprotect_water_severe_perc),
         waste_disposal_severe=village_threshold_quarters(waste_disposal_severe_perc),
         water_no_access=3*village_threshold_quarters(water_no_access_perc),
         soap_access=village_threshold_quarters(soap_access_perc))

# wash score
wash_sev$htr_wash_score<-coerc(wash_sev[["unprotect_water_extreme"]])+coerc(wash_sev[["unprotect_water_severe"]])+coerc(wash_sev[["waste_disposal_severe"]])+coerc(wash_sev[["water_no_access"]])+coerc(wash_sev[["soap_access"]])

# wash rank
wash_sev$htr_wash_rank<-car::recode(wash_sev$htr_wash_score,
                                  "0:4='1';
                                  5:8='2';
                                  9:14='3';
                                  15:20='4'")   


wash_sev$htr_wash_high<-car::recode(wash_sev$htr_wash_rank,
                                           "1:2='0';
                                           3:4='1'")  

############## Export analysis in CSV format ############
write.csv(wash_sev, "sector_output/HTR_round1_wash_sev.csv")

#############################################################################
#### ESNFI

# create variables
# shelter type
data$shelter_type_severe<-case_when(data$shelter_type=='shelter_tent'|data$shelter_type=='shelter_makeshift'~1,TRUE~0)

# open without shelter
data$no_shelter_open_bin<-case_when(data$no_shelter_open=='yes'~1,TRUE~0)

# shelter destroyed
data$shelter_destroyed_extreme<-case_when((data$shelter_destroyed=='100_more_hh'|data$shelter_destroyed=='50_100_hh')&(data$shelter_damage=='yes_both'|data$shelter_damage=='yes_disaster'|data$shelter_damage=='yes_conflict')~1,TRUE~0)
data$shelter_destroyed_severe<-case_when(data$shelter_destroyed=='20_50_hh'&(data$shelter_damage=='yes_both'|data$shelter_damage=='yes_disaster'|data$shelter_damage=='yes_conflict')~1,TRUE~0)

# nfi market
data$nfi_market_bin<-case_when(data$nfi_market=='no'~1,TRUE~0)


#group by district
esnfi_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            shelter_type_severe_perc=sum(shelter_type_severe)/n(),
            no_shelter_open_perc=sum(no_shelter_open_bin)/n(),
            shelter_destroyed_extreme_perc=sum(shelter_destroyed_extreme)/n(),
            shelter_destroyed_severe_perc=sum(shelter_destroyed_severe)/n(),
            nfi_market_perc=sum(nfi_market_bin)/n())

# give weights
esnfi_sev <-esnfi_sev %>% 
  mutate(shelter_type_severe=2*village_threshold_quarters(shelter_type_severe_perc),
         no_shelter_open=village_threshold_quarters(no_shelter_open_perc),
         shelter_destroyed_extreme=3*village_threshold_quarters(shelter_destroyed_extreme_perc),
         shelter_destroyed_severe=2*village_threshold_quarters(shelter_destroyed_severe_perc),
         nfi_market=2*village_threshold_quarters(nfi_market_perc))

# esnfi score
esnfi_sev$htr_esnfi_score<-coerc(esnfi_sev[["shelter_type_severe"]])+coerc(esnfi_sev[["no_shelter_open"]])+coerc(esnfi_sev[["shelter_destroyed_extreme"]])+coerc(esnfi_sev[["shelter_destroyed_severe"]])+coerc(esnfi_sev[["nfi_market"]])

# esnfi rank
esnfi_sev$htr_esnfi_rank<-car::recode(esnfi_sev$htr_esnfi_score,
                                    "0:4='1';
                                    5:8='2';
                                    9:14='3';
                                    15:20='4'")   

esnfi_sev$htr_esnfi_rank_high<-car::recode(esnfi_sev$htr_esnfi_rank,
                                             "1:2='0';
                                              3:4='1'")  

############## Export analysis in CSV format ############
write.csv(esnfi_sev, "sector_output/HTR_round1_esnfi_sev.csv")

#############################################################################
#### health

# create variables
# death cause
# data$death_cause_event<-case_when(data$death_cause=='conflict_cause'|data$death_cause=='natural_cause'|data$death_cause=='disease_cause'~1,TRUE~0)

# more people died recently
data$more_people_died<-case_when(data$more_ppl_died=='yes'~1,TRUE~0)

# health functioning access
data$health_functioning_access_bin<-case_when(data$health_functioning_access=='no'~1,TRUE~0)

# health facilities closed
data$health_closed_bin<-case_when(data$health_closed=='yes'~1,TRUE~0)

# health facilities closed
data$birth_hospital_bin<-case_when(data$birth_hospital=='no'~1,TRUE~0)

# health as one of top 3 priority needs
data$health_prio<-case_when(data$priority_needs.needs_health==1~1,TRUE~0)

#group by district
health_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
            more_people_died_perc=sum(more_people_died)/n(),
            health_functioning_access_perc=sum(health_functioning_access_bin)/n(),
            health_closed_perc=sum(health_closed_bin)/n(),
            birth_hospital_perc=sum(birth_hospital_bin)/n(),
            health_prio_perc=sum(health_prio)/n())

# give weights
health_sev <-health_sev %>% 
  mutate(more_people_died=village_threshold_quarters(more_people_died_perc),
         health_functioning_access=3*village_threshold_quarters(health_functioning_access_perc),
         health_closed=3*village_threshold_quarters(health_closed_perc),
         birth_hospital=village_threshold_quarters(birth_hospital_perc),
         health_prio=2*village_threshold_quarters(health_prio_perc))

# edu score
health_sev$htr_health_score<-coerc(health_sev[["more_people_died"]])+coerc(health_sev[["health_functioning_access"]])+coerc(health_sev[["health_closed"]])+coerc(health_sev[["birth_hospital"]])+coerc(health_sev[["health_prio"]])

# edu rank
health_sev$htr_health_rank<-car::recode(health_sev$htr_health_score,
                                   "0:4='1';
                                    5:8='2';
                                    9:14='3';
                                    15:20='4'")   

health_sev$htr_health_rank_high<-car::recode(health_sev$htr_health_rank,
                                        "1:2='0';
                                        3:4='1'")  

############## Export analysis in CSV format ############
write.csv(health_sev, "sector_output/HTR_round1_health_sev.csv")

############# Livelihood##########################################

# settlements using the following coping strategies
# consumed seed
data$coping_consumed_seed_bin<-case_when(data$coping_consumed_seed=='yes'~1,TRUE~0)

# beg charity
data$coping_beg_charity_bin<-case_when(data$coping_beg_charity=='yes'~1,TRUE~0)

# sold land
data$coping_sold_land_bin<-case_when(data$coping_sold_land=='yes'~1,TRUE~0)

#group by district
coping_sev<-data %>% 
  group_by(district_reporting) %>% 
  summarize(
    coping_consumed_seed_perc=sum(coping_consumed_seed_bin)/n(),
    coping_beg_charity_perc=sum(coping_beg_charity_bin)/n(),
    coping_sold_land_perc=sum(coping_sold_land_bin)/n())

############## Join and Export all composite indicators ##########

data<-full_join(edu_sev, prot_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, nut_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, wash_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, fsac_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, wash_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, esnfi_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, health_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, coping_sev,by = c("district_reporting"="district_reporting"))

data<-full_join(data, coping_sev,by = c("district_reporting"="district_reporting"))

write.csv(data, "HTR_round1_sev_dist.csv")  


############## OVERALL COMP INDICATORS #################################

# join with weight

data<-full_join(data, weight,by = c("district_reporting"="district"))

data$weight<-coerc(data[["num_interviews"]])/coerc(data[["total_villages"]])


# shorter alternative to the below:

#   weighted_mean_by_rank<-function(x,weight){
#     map_dbl(1:4,~ weighted.mean(x==.x,weight))
#   }
# 
# data %>%
#   select(ends_with("_rank")) %>% 
#   map(weighted_mean_by_rank,data$weight)


overall_sev<-data %>% 
  summarize(eie1= weighted.mean(htr_eie_rank==1, weight),
            eie2= weighted.mean(htr_eie_rank==2, weight),
            eie3= weighted.mean(htr_eie_rank==3, weight),
            eie4= weighted.mean(htr_eie_rank==4, weight),
            eie_sev_high_perc= weighted.mean(htr_eie_rank_high, weight),
            esnfi1= weighted.mean(htr_esnfi_rank==1, weight),
            esnfi2= weighted.mean(htr_esnfi_rank==2, weight),
            esnfi3= weighted.mean(htr_esnfi_rank==3, weight),
            esnfi4= weighted.mean(htr_esnfi_rank==4, weight),
            esnfi_sev_high_perc= weighted.mean(htr_esnfi_rank_high, weight),
            fsa1= weighted.mean(htr_fsa_rank==1, weight),
            fsa2= weighted.mean(htr_fsa_rank==2, weight),
            fsa3= weighted.mean(htr_fsa_rank==3, weight),
            fsa4= weighted.mean(htr_fsa_rank==4, weight),
            fsac_sev_high_perc= weighted.mean(htr_fsa_rank_high, weight),
            nut1= weighted.mean(htr_nut_rank==1, weight),
            nut2= weighted.mean(htr_nut_rank==2, weight),
            nut3= weighted.mean(htr_nut_rank==3, weight),
            nut4= weighted.mean(htr_nut_rank==4, weight),
            nut_sev_high_perc= weighted.mean(htr_nut_rank_high, weight),
            prot1= weighted.mean(htr_prot_rank==1, weight),
            prot2= weighted.mean(htr_prot_rank==2, weight),
            prot3= weighted.mean(htr_prot_rank==3, weight),
            prot4= weighted.mean(htr_prot_rank==4, weight),
            prot_sev_high_perc= weighted.mean(htr_prot_rank_high, weight),
            wash1= weighted.mean(htr_wash_rank.x==1, weight),
            wash2= weighted.mean(htr_wash_rank.x==2, weight),
            wash3= weighted.mean(htr_wash_rank.x==3, weight),
            wash4= weighted.mean(htr_wash_rank.x==4, weight),
            wash_sev_high_perc= weighted.mean(htr_wash_high.x, weight),
            health1= weighted.mean(htr_health_rank==1, weight),
            health2= weighted.mean(htr_health_rank==2, weight),
            health3= weighted.mean(htr_health_rank==3, weight),
            health4= weighted.mean(htr_health_rank==4, weight),
            health_sev_high_perc= weighted.mean(htr_health_rank_high, weight),
            coping_consumed_seed_perc= weighted.mean(coping_consumed_seed_perc.x, weight),
            coping_beg_charity_perc= weighted.mean(coping_beg_charity_perc.x, weight),
            coping_sold_land_perc= weighted.mean(coping_sold_land_perc.x, weight)

  )

write.csv(overall_sev, "HTR_round1_sev_dist_av.csv")  

