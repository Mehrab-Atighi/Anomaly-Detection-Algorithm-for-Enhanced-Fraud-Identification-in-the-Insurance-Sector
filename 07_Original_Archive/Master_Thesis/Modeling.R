
modeling = function( percent1 = 0.5 , percent2 = 0.8 , percent3 = 0.9 , path = "carclaims.csv" , 
                     response_column_number = 33  , YesOrNo = TRUE){
  percent1 = percent1 ; percent2 = percent2 ; percent3 = percent3 ; path = path ;
  response_column_number = response_column_number  ; YesOrNo = YesOrNo
  source(file = "Labaling.R")
  Labaling = Labaling(path = path , response_column_number = response_column_number 
                      ,percent1 = percent1 , percent2 = percent2 , percent3 = percent3)
  attach(Labaling)
  source("ml_modeling.R")
  source("svm_modeling.R")
  source("adaboost_modeling.R")
  source("knn_modeling.R")
  source("Nb_modeling.R")
  source("rf_modeling.R")
  source("Xgboost_modeling.R")
  final_outputs = rbind(   svm = output_svm , ml = output_ml 
                         , adab = output_adab , knn = output_knn , nb = output_nb , 
                         rf = output_rf , xgb = output_xgb
                        )
  return(final_outputs)
}
modeling(percent1 = 0.5 , percent2 = 0.8 , percent3 = 0.9 , path = "carclaims.csv" , 
         response_column_number = 33  , YesOrNo = TRUE)
