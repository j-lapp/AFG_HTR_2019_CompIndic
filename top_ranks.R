coerc<-function(x){as.numeric(as.character(x))}






# load & prep inputs -------------------------------------------------------------

data <- read.csv("input/data/HTR_2019_round1_final_v2.csv", stringsAsFactors = F, na.strings = c("", "NA"))
weight<-read.csv("input/weights/HTR_round1_sev_dist.csv", stringsAsFactors = F, na.strings = c("", "NA"))

data<-full_join(data, weight,by = c("district_reporting"="district"))
data$weight<-coerc(data[["num_interviews"]])/coerc(data[["total_villages"]])



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
  










    