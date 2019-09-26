if(!require(surveyweights)){
  devtools::install_github("mabafaba/surveyweights")
  library(surveyweights)
}  


add_htr_weights<-function(data,weights_original = FALSE, weights_updated = FALSE){
  
  if(!is.data.frame(data)){stop("load data as object 'data' first")}
  if(!sum(weights_original, weights_updated)==1){
    stop("set one of weights_original and weights_updated TRUE")
  }
  # set column names and file name based on which source was selected
  if(weights_original){
   data_stratum_column <- "district_reporting"
   samplingframe_stratum_column<-"district"
   samplingframe_size_column <- "total_villages"
   weight_source <- "./input/weights/REACH_AFG_H2R_R1_weights_0924.csv"
  }else if(weights_updated){
    weight_source<-"input/weights/HTR_round1_sev_dist.csv"
    data_stratum_column <- "district_reporting"
    samplingframe_stratum_column<-"districts"
    samplingframe_size_column <- "X..villages"
  }else{
    stop("one of weights_original and weights_updated must be TRUE")
  }
  
  weight<-read.csv(weight_source,stringsAsFactors = FALSE)

  weighting_fun <- surveyweights::weighting_fun_from_samplingframe(weight,data.stratum.column = data_stratum_column,
                                                  sampling.frame.population.column = samplingframe_size_column,
                                                  sampling.frame.stratum.column = samplingframe_stratum_column)
  
  data$weight<-weighting_fun(data) %>% as.vector
  
  if(!mean(data$weight)==1){stop("weights wrong")}
  message("added column: 'weight'")
  return(data)
  
  
}
