set.seed(124)

# Generate 20000 values
returns = rnorm(1:20000, mean = 0, sd = 1)

# plot a histogram
#par(mar=c(1,1,1,1))
#hist(returns)

#sample size:10~100, step 10
sample_size<-c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

#critical arr
my_arr <- array(dim = c(7, 10, 50))

#for loop
for(k in 1:50) {
  table<-matrix(ncol = 10,nrow = 7)
  for(j in 1:10) {
    num<-sample_size[j]
    X = matrix(ncol = num,nrow = 20000)
    for(value in 1:num) {
      # for each year sample the return 10000 times
      for(i in 1:20000){
        X[i,value] = sample(returns,1)
      }
    }
    #create X.dataframe, m vector
    X.dataframe<-as.data.frame(X)
    m<-c()
    #calculate skewness
    for(i in 1:20000){
      sample.mean<-mean(X[i,])
      sample.median<-median(X[i,])
      sample.sd<-sd(X[i,])
      m[i]<-(3*(sample.mean-sample.median))/sample.sd
    }
    
    #1.Percentile: 65%, z-score: 0.385
    cre65_value<-0.385*sd(m)+mean(m)
    table[1,j]<-cre65_value    
    
    #2.Percentile: 70%, z-score: 0.524
    cre70_value<-0.524*sd(m)+mean(m)
    table[2,j]<-cre70_value
    
    #3.Percentile: 75%, z-score: 0.674
    cre75_value<-0.674*sd(m)+mean(m)
    table[3,j]<-cre75_value
    
    #4.Percentile: 80%, z-score: 0.842
    cre80_value<-0.842*sd(m)+mean(m)
    table[4,j]<-cre80_value
    
    #5.Percentile: 85%, z-score: 1.036
    cre85_value<-1.036*sd(m)+mean(m)
    table[5,j]<-cre85_value
    
    #6.Percentile: 90%, z-score: 1.282
    cre90_value<-1.282*sd(m)+mean(m)
    table[6,j]<-cre90_value
    
    #7.Percentile: 95%, z-score: 1.645
    cre95_value<-1.645*sd(m)+mean(m)
    table[7,j]<-cre95_value
    print(j)
  }
  my_arr[,,k]<-table
  print("-----")
  print(k)
  print("-----")
}

matrix_all<- my_arr[,,1]
for (k in 2:50) {
  matrix1<-matrix_all
  matrix2<-my_arr[,,k]
  matrix_all<-matrix1+matrix2
}

matrix_all<-as.data.frame(matrix_all)
write.csv(matrix_all, "C:/Users/User/Desktop/matrix_all.csv")

#fread
sk2_data<- data.table::fread('C:/Moth/Data ananlyzed/table/sk2_critical_table.csv')
library(ggplot2)
sk2_data$critical_value<- factor(sk2_data$critical_value, levels = c("5% Upper", "10% Upper", "15% Upper", "20% Upper",
                                                                     "25% Upper", "30% Upper", "35% Upper"))

#ggplot2
ggplot(sk2_data, aes(x=sample_size, y=skewness, group = critical_value)) +
  geom_line(aes(color = critical_value), position="identity")+
  geom_point()+
  labs(x = "sample size", color = "critical value")



---
  #G1 Skewness
  ---  
  
  # Generate 20000 values
  returns = rnorm(1:20000, mean = 0, sd = 1)

# plot a histogram
#par(mar=c(1,1,1,1))
#hist(returns)

#sample size:10~100, step 10
sample_size<-c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

#critical arr
my_arr <- array(dim = c(7, 10, 50))


#for loop
for(k in 1:50) {
  table<-matrix(ncol = 10,nrow = 7)
  for(j in 1:10) {
    num<-sample_size[j]
    X = matrix(ncol = num,nrow = 20000)
    for(value in 1:num) {
      # for each year sample the return 10000 times
      for(i in 1:20000){
        X[i,value] = sample(returns,1)
      }
    }
    #create X.dataframe, m vector
    X.dataframe<-as.data.frame(X)
    m<-c()
    #calculate skewness
    for(i in 1:20000){
      m1<-((X[i,]-mean(X[i,]))/sd(X[i,]))^3
      m2<-sum(m1)
      m[i]<-(num/((num-1)*(num-2)))*m2
    }
    
    #1.Percentile: 65%, z-score: 0.385
    cre65_value<-0.385*sd(m)+mean(m)
    table[1,j]<-cre65_value    
    
    #2.Percentile: 70%, z-score: 0.524
    cre70_value<-0.524*sd(m)+mean(m)
    table[2,j]<-cre70_value
    
    #3.Percentile: 75%, z-score: 0.674
    cre75_value<-0.674*sd(m)+mean(m)
    table[3,j]<-cre75_value
    
    #4.Percentile: 80%, z-score: 0.842
    cre80_value<-0.842*sd(m)+mean(m)
    table[4,j]<-cre80_value
    
    #5.Percentile: 85%, z-score: 1.036
    cre85_value<-1.036*sd(m)+mean(m)
    table[5,j]<-cre85_value
    
    #6.Percentile: 90%, z-score: 1.282
    cre90_value<-1.282*sd(m)+mean(m)
    table[6,j]<-cre90_value
    
    #7.Percentile: 95%, z-score: 1.645
    cre95_value<-1.645*sd(m)+mean(m)
    table[7,j]<-cre95_value
    print(j)
  }
  my_arr[,,k]<-table
  print("-----")
  print(k)
  print("-----")
}

matrix_all<- my_arr[,,1]
for (k in 2:50) {
  matrix1<-matrix_all
  matrix2<-my_arr[,,k]
  matrix_all<-matrix1+matrix2
}

matrix_all<-as.data.frame(matrix_all)
write.csv(matrix_all, "C:/Users/User/Desktop/matrix_all.csv")

#fread
G1_data<- data.table::fread('C:/Moth/Data ananlyzed/table/G1_critical_table.csv')
library(ggplot2)
G1_data$critical_value<- factor(G1_data$critical_value, levels = c("5% Upper", "10% Upper", "15% Upper", "20% Upper",
                                                                   "25% Upper", "30% Upper", "35% Upper"))

#ggplot2
ggplot(G1_data, aes(x=sample_size, y=skewness, group = critical_value)) +
  geom_line(aes(color = critical_value), position="identity")+
  geom_point()+
  labs(x = "sample size", color = "critical value")+
  theme_gray(base_size = 24)+
  scale_x_continuous(limits=c(10, 100), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2))


#fread
skewness_data<- data.table::fread('C:/Moth/Data ananlyzed/table/skewness_critical_table.csv')
library(ggplot2)
skewness_data$critical_value<- factor(skewness_data$critical_value, levels = c("5% Upper", "10% Upper", "15% Upper", "20% Upper",
                                                                               "25% Upper", "30% Upper", "35% Upper"))

#ggplot2
ggplot(skewness_data, aes(x=sample_size, y=skewness, group = skewness_type)) +
  geom_line(aes(color = skewness_type), position="identity")+
  geom_point()+
  labs(x = "sample size", color = "skewness type")+
  facet_wrap(~critical_value, ncol = 4, nrow = 2)+
  theme(legend.position="bottom")