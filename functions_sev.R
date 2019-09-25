

########### weighting function by district

dist_sev_weight<- function(x){
  (x %>% 
  group_by(district_reporting) %>% 
    summarize(total_settlements=n()))
}

######### village threshold function ##################

######### 0-2 -> 0, 25 - 49 -> 1, 50-74 -> 2, 75-100 -> 3 #################

village_threshold_quarters<-function(x){
  breaks_smaller <- c(0.25, 0.50, 0.75)
  (case_when(
    (x<breaks_smaller[1])~0,
    (x<breaks_smaller[2])~1,
    (x<breaks_smaller[3])~2,
    (x <= 1)~3))
}

is_correct<-village_threshold_quarters(c(0.1,0.24,0.25,0.49,0.5,0.51,0.74,0.75,0.76)) == c(0,0,1,1,2,2,2,3,3)
if(!all(is_correct)){stop("village_threshold_quarters not working correctly")}


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



