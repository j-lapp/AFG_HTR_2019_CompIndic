source("weights.R")


coerc<-function(x){as.numeric(as.character(x))}






# load & prep inputs -------------------------------------------------------------

data <- read.csv("input/data/HTR_2019_round1_final_v2.csv", stringsAsFactors = F, na.strings = c("", "NA"))

# turns out both weights give the same results. I'm using the sample size counted from the data rows per district, not the sample size listed in the samplingframe

data<-add_htr_weights(data,
                      weights_original = TRUE,
                      weights_updated = FALSE)



# prep indicators ---------------------------------------------------------



priority_needs<-data %>%
  select(starts_with("priority_needs"),weight) %>%
  select(-priority_needs) %>% as_tibble


priority_needs %>% lapply(weighted.mean,priority_needs$weight,na.rm = T) %>%
  t %>% t %>% write.csv("priority_needs.csv")



type_assistance<-data %>%
  select(starts_with("type_assistance"),weight) %>%
  select(-type_assistance) %>% as_tibble


type_assistance %>% lapply(weighted.mean,type_assistance$weight,na.rm=T) %>%
  t %>% t %>% write.csv("type_assistance.csv")
  

data$hum_aid_3_months[data$hum_aid_3_months=="no_answer"]<-NA

summary_hum_aid_3_months<-weighted.mean(data$hum_aid_3_months=="yes", 
                                         data$weight, # using weights from data column 
                                         na.rm = TRUE) # and pretend NA records don't exist

names(summary_hum_aid_3_months)<-"yes"
write.csv(summary_hum_aid_3_months,"./hum_aid_3_months.csv")






    