#library
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)
library(janitor)
library(dotwhisker)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(grid)
library(broom)
library(ggpubr)

#import
#data1

LM_data1<- data.table::fread('C:/Users/User/Desktop/Moth abundance distribution data/dataset 1_1.csv')

#data2

LM_data2<- data.table::fread('C:/Users/User/Desktop/Moth abundance distribution data/dataset 1_2.csv')


#data3
LM_data3<- data.table::fread('C:/Moth/Thermal_bundance_shape/table/data_weighted2.csv')

#model construction

model1<- lmer(G1_RI_skewness ~ scale(RI) +
                scale(Weighted_DTR) +
                scale(Range_size) +
                scale(Mean_log_weight) +
                scale(Range_size)*scale(Mean_log_weight) +
                (1|Family),
              data = LM_data1)


model2<- lmer(G1_skewness ~ scale(RI) +
                scale(Weighted_DTR) +
                scale(Range_size) +
                scale(Average_log_weight) +
                scale(Range_size)*scale(Average_log_weight) +
                (1|Subfamily),
              data = LM_data2)


m1<-model1
m2<-model2

#plot1: dataset1
p1<-plot_model(m1,
               type="est",
               vline.color="black",
               sort.est = TRUE, show.values = TRUE, value.offset = .3)+
  theme_gray(base_size = 18)


p2<-plot_model(m2,
               type="est",
               vline.color="black",
               sort.est = TRUE, show.values = TRUE, value.offset = .3)+
  theme_gray(base_size = 18)

pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1:2))

#model averaging


model1<- lm(G1_skewness ~ scale(Relative_position) +
              scale(Weighted_DTR) +
              scale(Range_size) +
              scale(Mean_log_weight) +
              scale(Range_size)*scale(Mean_log_weight),  
            data = LM_data1)


model2<- lm(G1_skewness ~ scale(Relative_position) +
              scale(Weighted_DTR) +
              scale(CTrange) +
              scale(Range_size) +
              scale(Mean_log_weight) +
              scale(Range_size)*scale(Mean_log_weight),  
            data = LM_data2)


model3<- lm(Temp_skewed ~ scale(Temp_opt) +
              scale(Weighted_DTR) +
              scale(Range_size) +
              scale(Mean_log_weight) +
              scale(Range_size)*scale(Mean_log_weight),  
            data = LM_data3)



#m1
options(na.action = "na.fail") # Required for dredge to run
GTmodel_dredge <- dredge(model1, evaluate = T, rank = AICc)
options(na.action = "na.omit") # set back to default

top_model <- get.models(GTmodel_dredge, subset = 1)[[1]]
ModelAvg<-model.avg(GTmodel_dredge, subset = delta <= 2, fit = TRUE)
m1<-ModelAvg

summary(m1)

#m2
options(na.action = "na.fail") # Required for dredge to run
GTmodel_dredge <- dredge(model2, evaluate = T, rank = AICc)
options(na.action = "na.omit") # set back to default

top_model <- get.models(GTmodel_dredge, subset = 1)[[1]]
ModelAvg<-model.avg(GTmodel_dredge, subset = delta <= 2, fit = TRUE)
m2<-ModelAvg

summary(m2)



#m3
options(na.action = "na.fail") # Required for dredge to run
GTmodel_dredge <- dredge(model3, evaluate = T, rank = AICc)
options(na.action = "na.omit") # set back to default

top_model <- get.models(GTmodel_dredge, subset = 1)[[1]]
ModelAvg<-model.avg(GTmodel_dredge, subset = delta <= 2, fit = TRUE)
m3<-ModelAvg

summary(m3)


p3<-plot_model(m3,
               type="est",
               vline.color="black",
               sort.est = TRUE, show.values = TRUE, value.offset = .3)+
  theme_gray(base_size = 18)+
  theme(plot.margin=unit(c(1,1,1,1),'cm'))



pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))

