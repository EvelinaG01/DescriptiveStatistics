# set the path in R
setwd("C:../DescriptiveStatistics")
# Data Insertion
rm(list=ls())
Data<-read.table(file="data.txt" ,header=T ,na.strings=c("uns")) 

# Histogram, Boxplot & numeric methods of Damage
Damage<-Data[,1]
par(mfrow=c(1,2))
hist(Damage, ylab="Frequency",xlab="Damage Value", main = "Histogram of Damage")
boxplot(Damage, ylab="Damage Value",main="Boxplot of Damage")
fivenum(Damage)
summary(Damage)
var(Damage)
sd(Damage)
IQR<-quantile(Damage,0.75)-quantile(Damage,0.25)
Range<-max(Damage)-min(Damage)


#Barplot & numeric methods of Sex
Sex<-Data[,2]
Sex<-as.factor(Sex)
TSex<-table(Sex)
par(mfrow=c(1,1))
barplot(TSex, ylab= "Frequency",main="Barplot of Sex")
table(Sex) # Exact Frequency
prop.table(table(Sex)) # Relative Frequency

#Barplot & numeric methods of Faction
Faction<-Data[,3]
Faction<-as.factor(Faction)
TFaction<-table(Faction)
par(mfrow=c(1,1))
barplot(TFaction, ylab= "Frequency",main="Barplot of Faction")
table(Faction) # Exact Frequency
prop.table(table(Faction)) # Relative Frequency

#Barplot & numeric methods of Class
Class<-Data[,4]
Class<-as.factor(Class)
TClass<-table(Class)
par(mfrow=c(1,1))
barplot(TClass, ylab= "Frequency",main="Barplot of Class")
table(Class) # Exact Frequency
prop.table(table(Class)) # Relative Frequency


# Histogram, Boxplot & numeric methods of Damage
Attitude<-Data[,5]
par(mfrow=c(1,2))
hist(Attitude, ylab="Frequency",xlab="Damage Value", main = "Histogram of Attitude")
boxplot(Attitude, ylab="Frequency",xlab="Damage Value", main = "Boxplot of Attitude")
fivenum(Attitude)
summary(Attitude)
var(Attitude)
sd(Attitude)
IQR<-quantile(Attitude,0.75)-quantile(Attitude,0.25)
Range<-max(Attitude)-min(Attitude)


# Damage-Sex
par(mfrow=c(1,1))
Children<-subset(Data,sex == "children")
Men<-subset(Data,sex == "man")
Women<-subset(Data,sex == "woman")
boxplot(Children[,1],Men[,1],Women[,1],names=c("Children's Damage","Men's Damage","Women's Damage"))
title(main="Boxplot of Damage by Sex")


#Damage-Faction
par(mfrow=c(1,1))
Elf<-subset(Data,faction == "elf")
Human<-subset(Data,faction == "human")
Ogre<-subset(Data,faction == "ogre")
boxplot(Elf[,1],Human[,1],Ogre[,1],names=c("Elf's Damage","Human's Damage","Ogre's Damage"))
title(main="Boxplot of Damage by Faction")

#Damage-Class
par(mfrow=c(1,1))
Warrior<-subset(Data,class == "warrior")
Wizard<-subset(Data,class == "wizard")
boxplot(Warrior[,1],Wizard[,1],names=c("Warrior's Damage","Wizard's Damage"))
title(main="Boxplot of Damage by Class")

#Damage-Attitude
plot(Damage,Attitude,xlab="Damage",ylab="Attitude",main="Damage(Attitude)")


n<-length(Attitude)
f_attitude<-rep("High",48)
f_attitude[Attitude>80 & Attitude<=110]<-"Medium"
f_attitude[Attitude<=80]<-"Low"
f_attitude<-as.factor(f_attitude)
Data2<-transform(Data,f_attitude=f_attitude)
Tf_attitude<-table(f_attitude)
Tf_attitude
prop.table(Tf_attitude)
Elf<-subset(Data2,faction == "elf")
par(mfrow=c(1,1))
ElfLow<-subset(Elf, f_attitude=="Low")
ElfMedium<-subset(Elf, f_attitude == "Medium")
ElfHigh<-subset(Elf, f_attitude == "High")
boxplot(ElfLow[,1],ElfMedium[,1],ElfHigh[,1],names=c("ElfLow's Dameage","ElfMedium's Damage","ElfHigh's Damage)"))


table(Faction,Class)
prop.table(table(Faction,Class))
par(mfrow=c(1,2))
barplot(FC_Table, xlim=c(0,3), xlab="Class", legend=levels(Faction), col=1:3)
barplot(FC_Table, xlim=c(0,10), xlab="Class", legend=levels(Faction), col=1:3, beside=TRUE)