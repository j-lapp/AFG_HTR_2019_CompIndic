########### weighting function by district

dist_sev_weight<- function(x){
  (x %>% 
  group_by(district_reporting) %>% 
    summarize(total_settlements=n()))
}

######### village threshold function ##################
######### 0-19, 20 - 39, 40+ #################

village_threshold_fun<-function(x){
(case_when(
      (x<0.255)~0,
      (0.255<=x & x<0.505)~1,
      (0.505<=x)~2))
}

######### village threshold function 2 ##################
######### 0-15, 16 - 32, 33+ #################

village_threshold_fun_16_32<-function(x){
  (case_when(
    (x<0.155)~0,
    (0.155<=x & x<0.325)~1,
    (0.325<=x)~2))
}

######### village threshold function 2 ##################
######### 0-50, 51 - 75, 76+ #################

village_threshold_fun_50_75<-function(x){
  (case_when(
    (x<0.505)~0,
    (0.505<=x & x<0.755)~1,
    (0.755<=x)~2))
}



