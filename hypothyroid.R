
# CS513-B Final Project by
# Siddharth Limaye,Palak Gangwal,Majed Hariri

#Remove all the previous objects
rm(list=ls())
hypothyro_data <- read.csv("hypothyroid.csv") 
View(hypothyro_data)



tmpdata<-data.frame(age_ZONE1=hypothyro_data$age_ZONE1,age_ZONE2=hypothyro_data$age_ZONE2,age_ZONE3=hypothyro_data$age_ZONE3,
                    age_ZONE4=hypothyro_data$age_ZONE4,gender_m=hypothyro_data$gender_m,gender_f=hypothyro_data$gender_f,
                    on_thyroxine=hypothyro_data$on_thyroxine,query_on_thyroxine=hypothyro_data$query_on_thyroxine,
                    sick=hypothyro_data$sick,pregnant=hypothyro_data$pregnant,lithium=hypothyro_data$lithium,
                    goitre=hypothyro_data$goitre,
                    T3_ZONE1=hypothyro_data$T3_ZONE1,T3_ZONE2=hypothyro_data$T3_ZONE2,T3_ZONE3=hypothyro_data$T3_ZONE3,T3_ZONE4=hypothyro_data$T3_ZONE4,
                    TT4_ZONE1=hypothyro_data$TT4_ZONE1,TT4_ZONE2=hypothyro_data$TT4_ZONE2,TT4_ZONE3=hypothyro_data$TT4_ZONE3,TT4_ZONE4=hypothyro_data$TT4_ZONE4,
                    T4U_ZONE1=hypothyro_data$T4U_ZONE1,T4U_ZONE2=hypothyro_data$T4U_ZONE2,T4U_ZONE3=hypothyro_data$T4U_ZONE3,T4U_ZONE4=hypothyro_data$T4U_ZONE4,
                    FTI_ZONE1=hypothyro_data$FTI_ZONE1,FTI_ZONE2=hypothyro_data$FTI_ZONE2,FTI_ZONE3=hypothyro_data$FTI_ZONE4,FTI_ZONE4=hypothyro_data$FTI_ZONE4,
                    predict_thyroid=hypothyro_data$predict_thyroid)


#View(tmpdata)

indx <- seq( 1, nrow(tmpdata), 5)


newtest<-tmpdata[indx,]    # Create test dataset
newtest1<-tmpdata[indx,-29] # # Create test dataset without target column
newtraining<-tmpdata[-indx,]    # Create training dataset

#View(newtest)
#View(newtest1)
#View(newtraining)


library(class)
?knn()
newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=1)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_1 <- (good_predictions/total_predictions) * 100
successrate_K_1 

newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=3)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_3 <- (good_predictions/total_predictions) * 100
successrate_K_3 


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=5)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_5 <- (good_predictions/total_predictions) * 100
successrate_K_5 


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=7)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_7 <- (good_predictions/total_predictions) * 100
successrate_K_7 


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=9)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_9 <- (good_predictions/total_predictions) * 100
successrate_K_9 


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=11)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_11 <- (good_predictions/total_predictions) * 100
successrate_K_11


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=13)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_13 <- (good_predictions/total_predictions) * 100
successrate_K_13 


newpredict<-knn(newtraining[,-29], newtest[,-29],newtraining[,29],k=15)
newresults<-cbind(newtest,as.character(newpredict))  # adds new column from the result
table(newresults[,29],newresults[,30])
df<-as.data.frame(table(newresults[,29],newresults[,30]))
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_K_15 <- (good_predictions/total_predictions) * 100
successrate_K_15 


k<-c(1,3,5,7,9,11,13,15)
s<-c(successrate_K_1,successrate_K_3,successrate_K_5,successrate_K_7,successrate_K_9,successrate_K_11,
     successrate_K_13,successrate_K_15)
ks<-cbind(k,s)
plot(ks,col="blue",main = "KNN results for hypothyroid",type="b")
###################################################################################################

###### *** KKNN ***

library(class)
library(kknn)

newpredict <- kknn(sentement_~., newtraining, newtest, distance=1,k=1)
fit <- fitted(newpredict)
s<-table(newtest$sentement_, fit) # table gives the result in tabular form
df1<-as.data.frame(s)
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_KKNN_1 <- (good_predictions/total_predictions) * 100
successrate_KKNN_1 

newpredict <- kknn(sentement_~., newtraining, newtest, distance=1,k=3)
fit <- fitted(newpredict)
s<-table(newtest$sentement_, fit) # table gives the result in tabular form
df1<-as.data.frame(s)
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_KKNN_3 <- (good_predictions/total_predictions) * 100
successrate_KKNN_3 


newpredict <- kknn(sentement_~., newtraining, newtest, distance=1,k=5)
fit <- fitted(newpredict)
s<-table(newtest$sentement_, fit) # table gives the result in tabular form
df1<-as.data.frame(s)
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_KKNN_5 <- (good_predictions/total_predictions) * 100
successrate_KKNN_5 



newpredict <- kknn(sentement_~., newtraining, newtest, distance=1,k=7)
fit <- fitted(newpredict)
s<-table(newtest$sentement_, fit) # table gives the result in tabular form
df1<-as.data.frame(s)
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_KKNN_7 <- (good_predictions/total_predictions) * 100
successrate_KKNN_7 

newpredict <- kknn(sentement_~., newtraining, newtest, distance=1,k=9)
fit <- fitted(newpredict)
s<-table(newtest$sentement_, fit) # table gives the result in tabular form
df1<-as.data.frame(s)
good_predictions<-sum(df$Freq[df$Var1==df$Var2])
total_predictions<- sum(df$Freq)
successrate_KKNN_9 <- (good_predictions/total_predictions) * 100
successrate_KKNN_9 


k<-c(1,3,5,7,9)
s<-c(successrate_K_1,successrate_K_3,successrate_K_5,successrate_K_7,successrate_K_9)
ks<-cbind(k,s)
plot(ks,col="blue",main = "KKNN results for hypothyroid",type="b")

# Getting constant successrate with any K value

#######################################################################################
###### decision tree ########

###### use different csv file
rm(list=ls())

hypothyro_data <- read.csv("hypothyroid-ds.csv")


#create data frame
tmpdata<-data.frame(age_ZONE1=hypothyro_data$age_ZONE1,age_ZONE2=hypothyro_data$age_ZONE2,age_ZONE3=hypothyro_data$age_ZONE3,
                    age_ZONE4=hypothyro_data$age_ZONE4,gender_m=hypothyro_data$gender_m,gender_f=hypothyro_data$gender_f,
                    on_thyroxine=hypothyro_data$on_thyroxine,query_on_thyroxine=hypothyro_data$query_on_thyroxine,
                    sick=hypothyro_data$sick,pregnant=hypothyro_data$pregnant,lithium=hypothyro_data$lithium,
                    goitre=hypothyro_data$goitre,
                    T3_ZONE1=hypothyro_data$T3_ZONE1,T3_ZONE2=hypothyro_data$T3_ZONE2,T3_ZONE3=hypothyro_data$T3_ZONE3,T3_ZONE4=hypothyro_data$T3_ZONE4,
                    TT4_ZONE1=hypothyro_data$TT4_ZONE1,TT4_ZONE2=hypothyro_data$TT4_ZONE2,TT4_ZONE3=hypothyro_data$TT4_ZONE3,TT4_ZONE4=hypothyro_data$TT4_ZONE4,
                    T4U_ZONE1=hypothyro_data$T4U_ZONE1,T4U_ZONE2=hypothyro_data$T4U_ZONE2,T4U_ZONE3=hypothyro_data$T4U_ZONE3,T4U_ZONE4=hypothyro_data$T4U_ZONE4,
                    FTI_ZONE1=hypothyro_data$FTI_ZONE1,FTI_ZONE2=hypothyro_data$FTI_ZONE2,FTI_ZONE3=hypothyro_data$FTI_ZONE4,FTI_ZONE4=hypothyro_data$FTI_ZONE4,
                    predict_thyroid=hypothyro_data$predict_thyroid)

#View(tmpdata)

indx <- seq( 1, nrow(tmpdata), 5)

newtest<-tmpdata[indx,]    # Create test dataset
newtest1<-tmpdata[indx,-29] # # Create test dataset without target column
newtraining<-tmpdata[-indx,]    # Create training dataset

#View(newtest)
#View(newtraining)

library(rpart)
library(rpart.plot)

dtm<- rpart(predict_thyroid ~ age_ZONE1+age_ZONE2+age_ZONE3+age_ZONE4+
              T3_ZONE1+T3_ZONE2+T3_ZONE3+T3_ZONE4+
              TT4_ZONE1+TT4_ZONE2+TT4_ZONE3+TT4_ZONE4+
              T4U_ZONE1+T4U_ZONE2+T4U_ZONE3+T4U_ZONE4+
              FTI_ZONE1+FTI_ZONE2+FTI_ZONE3+FTI_ZONE4,newtraining,method = "class")


rpart.plot(dtm,type = 4,extra =102)
title("Decision tree for hypothyroid",line = 3)

rpart.plot(dtm,type = 4,extra =102,branch=0)
title("Decision tree for hypothyroid",line = 3)

summary(dtm)


#######################################################################################

# R Graphs

# histogram plot of age
age<-hypothyro_data$age
h<-hist(age,breaks = 30,col = "red",
        main = "Histogram of age",xlab = "age")
xfit<-seq(min(age),max(age),length=40)
yfit<-dnorm(xfit,mean=mean(age),sd=sd(age))
yfit <- yfit*diff(h$mids[1:2])*length(age)
lines(xfit, yfit, col="blue", lwd=2)

# kernel density plot of age
d <- density(hypothyro_data$age)
plot(d, main="Kernel Density of age",xlab = "age")
polygon(d, col="red", border="blue")


#install.packages("hexbin")
library(hexbin)

# T3 levels
age<-hypothyro_data$age
t3_levels<-hypothyro_data$T3_continuous
plot(hexbin(age,t3_levels,xbins = 30),colramp=BTY,main="Age vs T3 levels scatter plot")

# TT4 levels
age<-hypothyro_data$age
tt4_levels<-hypothyro_data$TT4_continuous
plot(hexbin(age,tt4_levels,xbins = 30),colramp=BTC,main="Age vs TT4 levels scatter plot")

# T4U levels
age<-hypothyro_data$age
t4u_levels<-hypothyro_data$T4U_continuous
plot(hexbin(age,t4u_levels,xbins = 30),colramp=plinrain,main="Age vs T4U levels scatter plot")

# FTI levels
age<-hypothyro_data$age
fti_levels<-hypothyro_data$FTI_continuous
plot(hexbin(age,fti_levels,xbins = 30),colramp=LinOCS,main="Age vs FTI levels scatter plot")

#### boxplots  #######
# goitre vs age
x<- factor(hypothyro_data$goitre)
x
plot(x,age,main="Goitre vs Age",ylab="age",xlab="goitre")

# predict thyroid vs age
x<- factor(hypothyro_data$predict_thyroid)
x
plot(x,age,main="predict_thyroid vs age",ylab="age",xlab="predict_thyroid")













