#setwd("set your work direction")
#(caffordability=(monthly mortgage) /(after-tax monthly income); (monthly median home value * 0.7)= monthly loan,  (monthly loan/pvifa)= monthly mortgage; suppose annual interest rate=0.4%, term=30 years; income tax = 20%; loan =70%houseprice; ). 
#This simply affordability calculator model is intended for planning, forcasting and reporting purposes only. The output of the tool is not a loan offer or solicitation, nor is it financial or legal advice. 

HousePrice <- read.csv("Bostonhome22.csv")
AnnualWage <- read.csv("annualwage.csv")   
AnnualWage$meanwage<- AnnualWage$meanwage*0.8/12 #after-tax monthly income 
MonthlyWage<- AnnualWage #change table name


#calculate monthly mortgage
install.packages("FinCal")
library(FinCal)
pvifa <- pv(r = 0.04/12, n = 360,fv = 0,pmt = -1,type = 0) #pvifa  the present value interest factor of annuity, monthly mortgage payment where interest rate is 0.4% and term is 30 years*/
HousePrice[,(ncol(HousePrice)+1)] <- NA #create a new col in HousePrice dataframe
for(i in 1:length(HousePrice$zip)){
  HousePrice$V3[i] <- HousePrice$zhvi[i]*0.7/pvifa 
}# use (monthly median home value * 0.7)= monthly loan,  monthly loan/pvifa= monthly mortgage
colnames(HousePrice)[3]<- "monthlymortgage"   

#calculate each occupation's affordability=(monthly mortgage) /(after-tax monthly income)
affordability <-matrix(nrow=length(HousePrice$zip), ncol=length(AnnualWage$Occupations)) #create empty matrix
for(i in 1:length(MonthlyWage$meanwage)){
  FUN<- function(x){ 
    x/MonthlyWage$meanwage[i]}
  affordability[,i] <- sapply(HousePrice$monthlymortgage, FUN)
}
View(affordability)
Afford<- data.frame(affordability) 
colnames(Afford)<- c("Management","Legal","Computer and Mathematical","Healthcare Practitioners and Technica","Architecture and Engineering", 
                     "Business and Financial Operations",
                     "Life Physical and Social Science", 
                     "Education Training and Library", 
                     "Arts Design Entertainment Sports and Media",
                     "Construction and Extraction", 
                     "Installation Maintenance and Repair", 
                     "Protective Service", 
                     "Sales and Related", 
                     "Community and Social Service", 
                     "Office and Administrative Support", 
                     "Production", 
                     "Transportation and Material Moving",
                     "Building and Grounds Cleaning and Maintenance", 
                     "Healthcare Support", 
                     "Farming Fishing and Forestry",
                     "Personal Care and Service", 
                     "Food Preparation and Serving Related")
Afford<-cbind(HousePrice,Afford) # bind 2 tables 
View(Afford)
write.csv(Afford,"/Users/yuchen/Desktop/UTOFUN/BostonAfford.csv", row.names=F)

