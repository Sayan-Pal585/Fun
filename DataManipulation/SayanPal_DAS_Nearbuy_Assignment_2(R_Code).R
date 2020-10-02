##------------------------------------------------
##NEARBUY.COM ASSIGNMENT
##------------------------------------------------

data<-read.csv(file.choose(),header=TRUE)
View(data)
attach(data)

##PLOTTING 
#GTW Travel
dataGTW<-data[data$category_id=="GTW",]
dataGTW1<-dataGTW[,-c(1,2,5)]

plot(dataGTW1$Transactions,dataGTW1$GR)
cor(dataGTW1$Transactions,dataGTW1$GR)   ##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.06)

#HNL Hobbies & Learning
dataHNL<-data[data$category_id=="HNL",]
dataHNL1<-dataHNL[,-c(1,2,5)]

plot(dataHNL1$Transactions,dataHNL1$GR)
cor(dataHNL1$Transactions,dataHNL1$GR)##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.09)

#SNM Spa & Massages
dataSNM<-data[data$category_id=="SNM",]
dataSNM1<-dataSNM[,-c(1,2,5)]

plot(dataSNM1$Transactions,dataSNM1$GR)
cor(dataSNM1$Transactions,dataSNM1$GR)##Merely positvely correlated(i.e., if transactions increase GR will decrease with a rate 0.11)

#LOS Home & Auto Services
dataLOS<-data[data$category_id=="LOS",]
dataLOS1<-dataLOS[,-c(1,2,5)]

plot(dataLOS1$Transactions,dataLOS1$GR)
cor(dataLOS1$Transactions,dataLOS1$GR)##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.105)

#MVE Movies & Events
dataMVE<-data[data$category_id=="MVE",]
dataMVE1<-dataMVE[,-c(1,2,5)]

plot(dataMVE1$Transactions,dataMVE1$GR)
cor(dataMVE1$Transactions,dataMVE1$GR)##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.09)

#ACT Activities
dataACT<-data[data$category_id=="ACT",]
dataACT1<-dataACT[,-c(1,2,5)]

plot(dataACT1$Transactions,dataACT1$GR)
cor(dataACT1$Transactions,dataACT1$GR)##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.087)

#BNS Beauty & Salon
dataBNS<-data[data$category_id=="BNS",]
dataBNS1<-dataBNS[,-c(1,2,5)]

plot(dataBNS1$Transactions,dataBNS1$GR)
cor(dataBNS1$Transactions,dataBNS1$GR)##Merely positvely correlated(i.e., if transactions increase GR will C with a rate 0.005)

#HEA Health
dataHEA<-data[data$category_id=="HEA",]
dataHEA1<-dataHEA[,-c(1,2,5)]

plot(dataHEA1$Transactions,dataHEA1$GR)
cor(dataHEA1$Transactions,dataHEA1$GR)##Merely positvely correlated(i.e., if transactions increase GR will decrease with a rate 0.153)

#FIT Fitness
dataFIT<-data[data$category_id=="FIT",]
dataFIT1<-dataFIT[,-c(1,2,5)]

plot(dataFIT1$Transactions,dataFIT1$GR)
cor(dataFIT1$Transactions,dataFIT1$GR)##Merely positvely correlated(i.e., if transactions increase GR will decrease with a rate 0.07)

#FNB Food & Beverages
dataFNB<-data[data$category_id=="FNB",]
dataFNB1<-dataFNB[,-c(1,2,5)]

plot(dataFNB1$Transactions,dataFNB1$GR)
cor(dataFNB1$Transactions,dataFNB1$GR)##Merely positvely correlated(i.e., if transactions increase GR will increase with a rate 0.03)

#LOR In-store Retail
dataLOR<-data[data$category_id=="LOR",]
dataLOR1<-dataLOR[,-c(1,2,5)]

plot(dataLOR1$Transactions,dataLOR1$GR)
cor(dataLOR1$Transactions,dataLOR1$GR)

#SNS Spa & Salon
dataSNS<-data[data$category_id=="SNS",]
dataSNS1<-dataSNS[,-c(1,2,5)]

plot(dataSNS1$Transactions,dataSNS1$GR)
cor(dataSNS1$Transactions,dataSNS1$GR)

#HNF Health & Fitness
dataHNF<-data[data$category_id=="HNF",]
dataHNF1<-dataHNF[,-c(1,2,5)]

plot(dataHNF1$Transactions,dataHNF1$GR)
cor(dataHNF1$Transactions,dataHNF1$GR)

#TTD Things To Do
dataTTD<-data[data$category_id=="TTD",]
dataTTD1<-dataTTD[,-c(1,2,5)]

plot(dataTTD1$Transactions,dataTTD1$GR)
cor(dataTTD1$Transactions,dataTTD1$GR)

##DATA MANIPULATION
##dplyr library
library("dplyr", lib.loc="~/R/win-library/3.4")

##1. Return the field customer_id, Transactions
a<-select(data,customer_id,Transactions)
View(a)

##2. Rename the columns customer_id to user_id
b<-rename(data,user_id = customer_id)
View(b)

##3. What is the mean of GR FOR Category FNB
c<-filter(data,category_id=="FNB",GR)
mean(c$GR)

##4. Return the customer_id/user_id where category = GTW & TransactionValue > 0
d<-filter(data,category_id=="GTW",TransactionValue>0)
d1<-d[,-c(1,4,6)]
View(d1)

##5. Plot the GR for SNS month on month
f<-filter(data,category_id=="SNS",GR)
f1<-f[,-c(2,4,5)]
View(f1)

str(f1)

Date<-as.Date(f1$date,format="%m/%d/%y")
f2<-data.frame(Data,f1$category_id,f1$GR)

f3<-f2[order(as.Date(f2$Date, format="%Y-%m-%d")),]
View(f3)

library("lubridate", lib.loc="~/R/win-library/3.4")

bymonth<-aggregate(f1$GR~f3$Date,data=f1,FUN=sum)

plot(bymonth,type="l",main="GR FOR SNS")