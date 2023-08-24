#temperatrue-split gaussian

#import:transformed data which contain Species, Temperature, N(individuals)

data5<- data.table::fread('C:/Moth 2021/Data ananlyzed/table/data.csv')


#library
library("data.table")
library("dplyr")
library("ggplot2")
library("minpack.lm")

#temp.
split.Gaussian<-function(Temperature, p.max, temp.opt, sigma.max, sigma.min){
  performance <-ifelse(Temperature>=temp.opt,p.max*exp(-((Temperature-temp.opt)/(sigma.max))^2),p.max*exp(-((Temperature-temp.opt)/(sigma.min))^2))
  return(performance)  
}

#loess testing
data.use<-data5[Species=="NOC034"]
data.name <-data.use$Temperature

test<-loess(N~Temperature, data=data.use,span=0.8)
test.pred<-predict(test, data.frame(Temperature=seq(min(range(data.name)),max(range(data.name)),by=0.1)))
test.pred.dataframe<-data.frame(Temperature=seq(min(range(data.name)),max(range(data.name)),by=0.1),test.pred=test.pred)

ggplot(data = data.use, aes(x = Temperature, y = N))+
  geom_point(shape = 1)

#ggplot2-loess
ggplot(data = test.pred.dataframe, aes(x = Temperature, y = test.pred))+
  geom_point(shape = 1)


#split gaussian example plot

ggplot(data = test.pred.dataframe, aes(x = Temperature, y = test.pred))+
  geom_point(shape = 1, size =3)+
  ylab("Abundance") + xlab("Temperature") + theme_gray(base_size = 18) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
  theme_gray(base_size = 18)+
  geom_smooth(aes(color="red"), 
              data = test.pred.dataframe,
              method = "nlsLM",
              formula = y~ifelse(x>=temp.opt,p.max*exp(-((x-temp.opt)/(sigma.max))^2),p.max*exp(-((x-temp.opt)/(sigma.min))^2)),
              se = FALSE,
              method.args = list(start=c(temp.opt=0.5*(max(test.pred.dataframe$Temperature)+min(test.pred.dataframe$Temperature)),
                                         p.max=max(test.pred.dataframe$test.pred),
                                         sigma.max=0.5*(max(test.pred.dataframe$Temperature)-min(test.pred.dataframe$Temperature)),
                                         sigma.min=0.5*(max(test.pred.dataframe$Temperature)-min(test.pred.dataframe$Temperature))
              )))+
  scale_color_manual(name="Models",
                     labels=c("Gaussian"),
                     values=c("red"))



#for loop split gaussian-temp
setwd("C:/Moth 2021/Data ananlyzed/table")
file_name <- "data3.csv"
dat <- fread(file_name)
Species = unique(dat$Species)
specie_plots = list()
number<-1
for(specie_ in Species) {
  data.use<-dat[Species==specie_]
  data.name<-data.use$Temperature
  test<-loess(N~Temperature, data=data.use,span=0.8)
  test.pred<-predict(test, data.frame(Temperature=seq(min(range(data.name)),max(range(data.name)),by=0.1)))
  test.pred.dataframe<-data.frame(Temperature=seq(min(range(data.name)),max(range(data.name)),by=0.1),test.pred=test.pred)
  
  specie_plots[[specie_]] = ggplot(test.pred.dataframe, aes(x=Temperature, test.pred)) + geom_point(shape=1) +
    theme(plot.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 32, face = "bold", hjust=0.5),
          axis.title = element_text(size=16, colour="black"),
          axis.text = element_text(size=10, colour="black"),
          strip.text = element_text(size = 9, colour="white"),
          strip.background = element_rect(size = 50))+
    ggtitle(specie_) + ylab("Abundance") + xlab("Temperature") + theme_gray(base_size = 18) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) +
    geom_smooth(aes(color="red"), 
                data = test.pred.dataframe,
                method = "nlsLM",
                formula = y~ifelse(x>=temp.opt,p.max*exp(-((x-temp.opt)/(sigma.max))^2),p.max*exp(-((x-temp.opt)/(sigma.min))^2)),
                se = FALSE,
                method.args = list(start=c(temp.opt=0.5*(max(test.pred.dataframe$Temperature)+min(test.pred.dataframe$Temperature)),
                                           p.max=max(test.pred.dataframe$test.pred),
                                           sigma.max=0.5*(max(test.pred.dataframe$Temperature)-min(test.pred.dataframe$Temperature)),
                                           sigma.min=0.5*(max(test.pred.dataframe$Temperature)-min(test.pred.dataframe$Temperature))
                )))+
    scale_color_manual(name="Models",
                       labels=c("Gaussian"),
                       values=c("red"))
  
  print(specie_plots[[specie_]])
  ggsave(specie_plots[[specie_]], file=paste0(number,"_",specie_,"_split.Gaussian_loess.png"), width = 44.45, height = 27.78, units = "cm", dpi=300)
  number<-number+1
}