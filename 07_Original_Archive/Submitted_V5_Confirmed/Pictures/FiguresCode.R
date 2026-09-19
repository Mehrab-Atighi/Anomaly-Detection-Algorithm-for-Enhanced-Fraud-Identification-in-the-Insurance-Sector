library(readxl)
library(tidyverse)
######################### FIG1

data1 = read.csv("N_ML7_Lev1_EachMethod.csv")
names(data1) = c("Method" , "Metrics" , "Percent" , "T AD")
data1$Percent = 100 * data1$Percent
data1$Method = factor(data1$Method , levels = c("Simple-A" , "Simple-B" , "Multilayer"))
jpeg(filename = "FIG1.JPEG"  , width = 6000, height = 3000, units = "px" , res = 720)
plot1 = data1 %>%  
  ggplot() +
  geom_line(mapping = aes(x = `T AD` , y = Percent ,linetype   = Method , color = Method) ) +
  geom_point(mapping = aes(x = `T AD` , y = Percent , color = Method , shape = Method)) + xlab(expression(T[AD]))

print(plot1 + facet_wrap(.~   Metrics ) + 
        guides( linetype = guide_legend(override.aes = list(size = 3))))
dev.off()


######################### FIG2
data2 = read.csv("N_AN5_Lev3_EachMethod.csv")
names(data2) = c("Method" , "Metrics" , "Percent" , "T ML")
data2$Percent = 100 * data2$Percent
data2$Method = factor(data2$Method , levels = c("Simple-A" , "Simple-B" , "Multilayer"))

png(filename = "FIG2.PNG"  , width = 6000, height = 3000, units = "px" , res = 720)
plot2 = data2 %>%  
  ggplot() +
  geom_line(mapping = aes(x = `T ML` , y = Percent ,linetype   = Method , color = Method) ) +
  geom_point(mapping = aes(x = `T ML` , y = Percent , color = Method , shape = Method))+ xlab(expression(T[ML]))

print(plot2 + facet_wrap(.~   Metrics ) + 
        guides( linetype = guide_legend(override.aes = list(size = 3))))
dev.off()

######################### FIG3
data3 = read.csv("Accuracy_N_ML7_Lev1.csv")
names(data3) = c("Metrics" , "ML Model" , "Percent of Accuracy" , "T AD" , "Method")
data3$`ML Model` = ifelse(data3$`ML Model` == "svm" , "SVM" ,ifelse(data3$`ML Model` == "xgb" , "XGBoost" , 
                                                                    ifelse(data3$`ML Model` == "total" , "SAD", 
                                                                           ifelse(data3$`ML Model` == "adab" , "Adaboost" , 
                                                                                  ifelse(data3$`ML Model` == "knn" , "KNN" , 
                                                                                         ifelse(data3$`ML Model` == "nn" , "ANN" , 
                                                                                                ifelse(data3$`ML Model` == "rf" , "RF", 
                                                                                                       ifelse(data3$`ML Model` == "ml" , "ML" , 
                                                                                                              ifelse(data3$`ML Model` == "nb" , "NB" , "")))))))))

data3$`Percent of Accuracy` = 100 * data3$`Percent of Accuracy`
data3$`ML Model` = factor(data3$`ML Model` , levels  = c("SAD" , "Adaboost" , "KNN" , "ML" , "NB" , "ANN" , "RF" , "SVM" , "XGBoost"))
data3$Method = factor(data3$Method , levels = c("Simple-A" , "Simple-B" , "Multilayer"))
png(filename = "FIG3_1.PNG", width = 6000, height = 3000, units = "px" , res = 720)
plot3_1= data3 %>%
  ggplot() +
  geom_line(mapping = aes(x = `T AD` , y = `Percent of Accuracy` ,linetype   = `ML Model` , color = `ML Model`) ) +
  geom_point(mapping = aes(x = `T AD` , y = `Percent of Accuracy` , color = `ML Model` , shape  = `ML Model`))+ xlab(expression(T[AD]))

print(plot3_1 + facet_wrap(.~   Method ) + 
        guides( linetype = guide_legend(override.aes = list(size = 3))))
dev.off()

png(filename = "FIG3_2.PNG", width = 6000, height = 3000, units = "px" , res = 720)
plot3_2= data3 %>%
  ggplot() +
  geom_line(mapping = aes(x = `T AD` , y = `Percent of Accuracy` ,linetype   = Method , color = Method) ) +
  geom_point(mapping = aes(x = `T AD` , y = `Percent of Accuracy` , color = Method , shape  = Method))+ xlab(expression(T[AD]))

print(plot3_2 + facet_wrap(.~   `ML Model` ) + 
        guides( linetype = guide_legend(override.aes = list(size = 3))))
dev.off()



######################### FIG4
data4 = read.csv("Accuracy_N_AN5_Lev3.csv")
names(data4) = c("ML Model" , "Percent of Accuracy" , "T ML" , "Method")
data4$`ML Model` = ifelse(data4$`ML Model` == "svm" , "SVM" ,ifelse(data4$`ML Model` == "xgb" , "XGBoost" , 
                                                                    ifelse(data4$`ML Model` == "total" , "SAD", 
                                                                           ifelse(data4$`ML Model` == "adab" , "Adaboost" , 
                                                                                  ifelse(data4$`ML Model` == "knn" , "KNN" , 
                                                                                         ifelse(data4$`ML Model` == "nn" , "ANN" , 
                                                                                                ifelse(data4$`ML Model` == "rf" , "RF", 
                                                                                                       ifelse(data4$`ML Model` == "ml" , "ML" , 
                                                                                                              ifelse(data4$`ML Model` == "nb" , "NB" , "")))))))))

data4$`Percent of Accuracy` = 100 * data4$`Percent of Accuracy`
data4$`ML Model` = factor(data4$`ML Model` , levels  = c("SAD" , "Adaboost" , "KNN" , "ML" , "NB" , "ANN" , "RF" , "SVM" , "XGBoost"))
data4$Method = factor(data4$Method , levels = c("Simple-A" , "Simple-B" , "Multilayer"))
png(filename = "FIG4_1.PNG", width = 6000, height = 3000, units = "px" , res = 720)
plot4_1= data4 %>%
  ggplot() +
  geom_line(mapping = aes(x = `T ML` , y = `Percent of Accuracy` ,linetype   = `ML Model` , color = `ML Model`) ) +
  geom_point(mapping = aes(x = `T ML` , y = `Percent of Accuracy` , color = `ML Model` , shape  = `ML Model`))+ xlab(expression(T[ML]))

print(plot4_1 + facet_wrap(.~   Method )  + 
        guides( linetype = guide_legend(override.aes = list(size = 3))) ) 
dev.off()

png(filename = "FIG4_2.PNG", width = 6000, height = 3000, units = "px" , res = 720)
plot4_2= data4 %>%
  ggplot() +
  geom_line(mapping = aes(x = `T ML` , y = `Percent of Accuracy` ,linetype   = Method , color = Method) ) +
  geom_point(mapping = aes(x = `T ML` , y = `Percent of Accuracy` , color = Method , shape  = Method))+ xlab(expression(T[ML]))

print(plot4_2 + facet_wrap(.~   `ML Model` )  + 
        guides( linetype = guide_legend(override.aes = list(size = 3))) ) 
dev.off()


################### FIG5_Lengths.PNG

data5 = readxl::read_excel("F:/thesis/Type/Latex/figs/ThesisPlots/Lenghts_Es.xlsx" , sheet = 3)
names(data5)[4:7] = 1:4 #c("|E1|" , "|E2|" , "|E3|" , "|E4|")
dd = pivot_longer(data5 , cols = c(4:7) )
names(dd)[2:5] = c("T_AD" , "T_ML" , "Round" , "Number of Proposed fraud")
dd$Round = as.integer(dd$Round ) 
dd$T_AD = as.factor(dd$T_AD)
dd$T_ML = as.factor(dd$T_ML)
png(filename = "FIG5.PNG", width = 6000, height = 3000, units = "px" , res = 720)
dd %>%
  ggplot() + 
  geom_line(mapping = aes(x = Round , y = `Number of Proposed fraud`  ,color =  T_AD  , linetype  = T_AD )) + 
  geom_point(mapping = aes(x = Round , y = `Number of Proposed fraud`  ,color =  T_AD  , shape   = T_AD ))+ 
  scale_x_continuous(labels = as.character(c("1" , "2" , "3" , "4"))) + xlab("Partition") +
  facet_wrap(. ~  T_ML , ncol = 5) + 
  guides( linetype = guide_legend(override.aes = list(size = 3))) 
dev.off()


