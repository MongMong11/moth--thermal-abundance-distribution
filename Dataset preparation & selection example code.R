library(data.table)

#import raw data
Taiwan_Moth_data<- data.table::fread('C:/Moth/Thermal_bundance_shape/Moth Raw data/Taiwan_Moth_data.csv')
China_Moth_data<- data.table::fread('C:/Moth/Thermal_bundance_shape/Moth Raw data/China_Moth_data.csv')
Malaysia_Moth_data<- data.table::fread('C:/Moth/Thermal_bundance_shape/Moth Raw data/Malaysia_Moth_data.csv')


dt1<-Taiwan_Moth_data[,Dry_Weight,by=Species]
dt2<-China_Moth_data[,Dry_Weight,by=Species]
dt3<-Malaysia_Moth_data[,Dry_Weight,by=Species]

data<-rbind(dt1, dt2, dt3)
length(unique(data$Species))

#check raw datas' altitude orders
s1<-unique(Taiwan_Moth_data$Altitude)
s2<-unique(China_Moth_data$Altitude)
s3<-unique(Malaysia_Moth_data$Altitude)
ss1<-scale(s1)
ss2<-scale(s2)
ss3<-scale(s3)

length(unique(Taiwan_Moth_data$Altitude))
length(unique(China_Moth_data$Altitude))
length(unique(Malaysia_Moth_data$Altitude))

#calculate species, sites-China
df<-China_Moth_data
test<-df
ss<-unique(test$Species)

# table: species, number of observed sites, total individuals
table<-data.frame(matrix(NA,length(ss),3))
colnames(table)<-c("species","number of observered sites","total individuals")
table[,1]<-ss


#for loop--fill the table's information
for (i in 1:length(ss)) {
  num<-nrow(unique(test[which(test$Species==ss[i]),8]))
  num2<-nrow(test[which(test$Species==ss[i]),3])
  table[i,2]<-num
  table[i,3]<-num2
}

library(data.table)

#select data--number of site more than 3 sites; individuals more than 
table<-data.table(table)
table2<-table[`number of observered sites`>=3]
table2<-table2[`total individuals`>=10]

names(table2)[1]<-"Species"
ss2<-table2$Species

#fill the datas to sort
dt2<-China_Moth_data[df$Species %in% ss2,]


#calculate species, sites-Taiwan
df<-Taiwan_Moth_data
test<-df
ss<-unique(test$Species)

# table: species, number of observed sites, total individuals
table<-data.frame(matrix(NA,length(ss),3))
colnames(table)<-c("species","number of observered sites","total individuals")
table[,1]<-ss


for (i in 1:length(ss)) {
  num<-nrow(unique(test[which(test$Species==ss[i]),4]))
  num2<-nrow(test[which(test$Species==ss[i]),9])
  table[i,2]<-num
  table[i,3]<-num2
}

table<-data.table(table)
table3<-table[`number of observered sites`>=3]
table3<-table3[`total individuals`>=10]

names(table3)[1]<-"Species"
ss2<-table3$Species

dt3<-Taiwan_Moth_data[df$Species %in% ss2 ,]

#calculate species, sites-Malaysia
df<-Malaysia_Moth_data
test<-df
ss<-unique(test$Species)
table<-data.frame(matrix(NA,length(ss),3))
colnames(table)<-c("species","number of observered sites","total individuals")
table[,1]<-ss


for (i in 1:length(ss)) {
  num<-nrow(unique(test[which(test$Species==ss[i]),5]))
  num2<-nrow(test[which(test$Species==ss[i]),9])
  table[i,2]<-num
  table[i,3]<-num2
}

table<-data.table(table)
table4<-table[`number of observered sites`>=3]
table4<-table4[`total individuals`>=10]

names(table4)[1]<-"Species"
ss2<-table4$Species

dt4<-Malaysia_Moth_data[df$Species %in% ss2 ,]
