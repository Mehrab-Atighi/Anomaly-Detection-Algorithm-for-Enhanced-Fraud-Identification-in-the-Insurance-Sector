# library(entropy)


paths = c("carclaims.csv" , "insurance fraud claims.csv"  , "fraud_oracle.csv")
response_columns = c(33 , 39 , 16)
YesOrNo = c(TRUE , TRUE , FALSE)

LOOCV_Entropi = function(csv_paths , response_columns ,YesOrNo  ,  probs = 0.8){
  library(caret)
  #Entropy function
  entropi <- function(target) {
    freq <- table(target)/length(target)
    # vectorize
    vec <- as.data.frame(freq)[,2]
    #drop 0 to avoid NaN resulting from log2
    vec<-vec[vec>0]
    #compute entropy
    -sum(vec * log2(vec))
  }
  
  #calculate LOOCV entropy mean
  cm= list()
  out = list()
  final = list()
  for(p in 1:length(csv_paths)){
    Data = read.csv(csv_paths[p])
    Data = Data[1:100,]
    j = 1
    Entropies = c()
    for(i in 1:nrow(Data)){
      Entropies[j] = mean(apply(Data[-i,-response_columns[p]] , 2 , entropi))
      
      j = j+1
    }
    out[[p]] = which(Entropies >= quantile(Entropies , probs) & Entropies >= quantile(Entropies , 1-probs))

    # print(paste0("the " , p , " result is " , out))
    if(YesOrNo[p]){
      final[[p]] = rep("No" , nrow(Data))
      for(q in out[[p]]){
        final[[p]][q] = "Yes"
      }
    }else{

        final[[p]] = rep(0 , nrow(Data))
        for(q in out[[p]]){
          final[[p]][q] = 1
        }
      
    }
    cm[[p]] = confusionMatrix( reference =  factor(Data[,response_columns[p]]) , data = factor(final[[p]]) , mode = "everything")
  }
  return(cm)
}
entropyMethod_result = LOOCV_Entropi(paths , response_columns = response_columns , YesOrNo= YesOrNo, probs = 0.8)

# save.image("EntropyImage.RData")