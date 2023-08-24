library(data.table)
library(eHOF)
library(ggplot2)
library(flexmix)

data5<- data.table::fread('C:/Moth/Thermal_bundance_shape/table/data5.csv')
data_model_type<-data.table::fread('C:/Moth/Thermal_bundance_shape/table/230804_eHOF_fit4.csv')
sel <- unique(data5$Species)
table<-data.frame(matrix(NA,length(sel),8))
colnames(table)<-c("species","sample_size","model_type","G1_skewness", "Critical_value_5", "Critical_value_10", "Shape_type_5", "Shape_type_10")
table[,1]<-sel

#expotential formula raw point
Expotential_formula2<- data.table::fread('C:/Moth/Thermal_bundance_shape/table/G1_critical_table.csv')
#G1
value5_G1_formula<-Expotential_formula2[critical_value=="5% Upper"]
value5_G1_formula<-value5_G1_formula[,.(sample_size,skewness)] 

value10_G1_formula<-Expotential_formula2[critical_value=="10% Upper"]
value10_G1_formula<-value10_G1_formula[,.(sample_size,skewness)]

value5_G1_exponential.model<- lm(log(`skewness`)~ sample_size, data = value5_G1_formula)
value10_G1_exponential.model<- lm(log(`skewness`)~ sample_size, data = value10_G1_formula)

#setwd("C:/Moth/Thermal_bundance_shape/HOF plot")

for (i in 1:length(sel)) {
  #settings
  sp<-sel[i]
  data<-data5[Species==sp]
  m<-max(data$N)
  model_type<-data_model_type[species==sp]
  model_type<-model_type$model_type
  data.Temp<-data$Temperature
  mo <- HOF(data$N, data$Temperature, M=m, family=poisson, bootstrap=NULL, model=model_type)
  
  table[i,2]<-sum(data$N)
  table[i,3]<-model_type
  
  mo
  
  #G1 skewness calculation
  if (model_type=="I") {
    G1_skewness<-0
    table[i,4]<-G1_skewness
  }else{
    ls<-data.frame(Temperature=seq(min(range(data.Temp)),max(range(data.Temp)),by=0.1))
    test.pred<-predict(mo,model=model_type,newdata=ls)
    test.pred$Temperature<-test.pred$Temperature*m
    test.pred.dataframe<-data.frame(Temperature=seq(min(range(data.Temp)),max(range(data.Temp)),by=0.1),test.pred=test.pred$Temperature)
    test.Mean<-sum(test.pred.dataframe$Temperature*test.pred.dataframe$test.pred)/sum(test.pred.dataframe$test.pred)
    test.SD<-sqrt((sum(((test.pred.dataframe$Temperature-test.Mean)^2)*test.pred.dataframe$test.pred)/(sum(test.pred.dataframe$test.pred))))
    N<-sum(test.pred.dataframe$test.pred)  
    G1_righter<-sum((((test.pred.dataframe$Temperature-test.Mean)/test.SD)^3)*test.pred.dataframe$test.pred)
    G1_skewness<-G1_righter*(N/((N-1)*(N-2)))
    table[i,4]<-G1_skewness
  }
  
  #critical_value
  sample.size<-sum(data$N)
  Critical.value5_G1<- round(exp(predict(value5_G1_exponential.model,list(sample_size=sample.size))),9)
  table[i,5]<-Critical.value5_G1
  Critical.value10_G1<- round(exp(predict(value10_G1_exponential.model,list(sample_size=sample.size))),9)
  table[i,6]<-Critical.value10_G1  
  
  if (abs(G1_skewness)<Critical.value5_G1){
    Shape_type.5<-"Symmetric" 
  } else if (abs(G1_skewness)>Critical.value5_G1&G1_skewness<0){
    Shape_type.5<-"Agree TPC"   
  } else {
    Shape_type.5<-"Against TPC"   
  }
  table[i,7]<-Shape_type.5
  
  if (abs(G1_skewness)<Critical.value10_G1){
    Shape_type.10<-"Symmetric" 
  } else if (abs(G1_skewness)>Critical.value10_G1&G1_skewness<0){
    Shape_type.10<-"Agree TPC"   
  } else {
    Shape_type.10<-"Against TPC"   
  }
  table[i,8]<-Shape_type.10
  
  
  #  mypath <- file.path("C:/Moth/Thermal_bundance_shape/HOF plot2/",paste(i, "_", sp, ".jpg", sep = ""))
  #  jpeg(file=mypath, width = 800, height = 800, units = "px" , res = 150)
  #  mytitle = paste(sp)
  #  par(mar=c(4,4,1,6)+.1)
  #  plot(mo, main=mytitle, model=model_type)
  #  legend("bottomleft", legend = c(paste("G1 skewness =", round(G1_skewness, 4)), paste("Sample size =", sample.size),
  #                              paste("Shape_type.5 =",Shape_type.5),
  #                              paste("Shape_type.10 =",Shape_type.10)), 
  #        bty = "n")
  #  dev.off()
}

for (i in 1:length(sel)) {
  sp<-sel[i]
  data<-data5[Species==sp]
  m<-max(data$N)
  mo <- HOF(data$N, data$Temperature, M=m, family=poisson, bootstrap=NULL, test="BIC")
  table[i,2]<-pick.model(mo,'BIC')
  mypath <- file.path("C:/Moth/Thermal_bundance_shape/HOF plot/HOF_para/",paste(i, "_", sp, ".jpg", sep = ""))
  jpeg(file=mypath, width = 1000, height = 800, units = "px" , res = 150)
  mytitle = paste(sp)
  par(mar=c(5,5,8,5)+.1)
  plot(mo, main=mytitle, para=TRUE, onlybest=FALSE)
  dev.off()
}

write.csv(table,'C:/Moth/Thermal_bundance_shape/table/230817_eHOF_fit6.csv')

table<-data.frame(matrix(NA,length(sel),8))
colnames(table)<-c("species","I.BIC.Diff","II.BIC.Diff","III.BIC.Diff","IV.BIC.Diff","V.BIC.Diff","VI.BIC.Diff","VII.BIC.Diff")
table[,1]<-sel

library(stringr)

for (i in 1:length(sel)) {
  i<-149
  sp<-sel[i]
  data<-data5[Species==sp]
  m<-max(data$N)
  mo <- HOF(data$N, data$Temperature, M=m, family=poisson, bootstrap=NULL, test="BIC")
  temp = capture.output(mo)
  temp
  table[i,2]<-word(toString(temp[12]),-1)
  table[i,3]<-word(toString(temp[13]),-1)
  table[i,4]<-word(toString(temp[14]),-1)
  table[i,5]<-word(toString(temp[15]),-1)
  table[i,6]<-word(toString(temp[16]),-1)
  table[i,7]<-word(toString(temp[17]),-1)
  table[i,8]<-word(toString(temp[18]),-1)
}

write.csv(table,'C:/Moth/Thermal_bundance_shape/table/230725_eHOF_fit2.csv')
