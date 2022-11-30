#Importing and installing ggplot library
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)

#Reading the csv file and printing it
Molecules <- read.csv("File_name_real.csv", header = T, sep=";")
Molecules


#Exploratory analysis of the Number of Atoms
boxplot(Molecules$NumAtoms)
mean(Molecules$NumAtoms) #mean = 21.01
median(Molecules$NumAtoms) #median = 21
hist(Molecules$NumAtoms) #histogram


#Exploratory analysis of Molecular Weight
boxplot(Molecules$MolWt)
mean(Molecules$MolWt) #mean = 300.88
median(Molecules$MolWt) #median = 301.14
hist(Molecules$MolWt)


#Exploratory analysis of TPSA
boxplot(Molecules$TPSA)
mean(Molecules$TPSA) #mean = 67.45
median(Molecules$TPSA) #median = 66.04
hist(Molecules$TPSA)


#Exploratory analysis of MolLogp
boxplot(Molecules$MolLogp)
mean(Molecules$MolLogp) #mean = 2.28
median(Molecules$MolLogp) #median = 2.48
hist(Molecules$MolLogp)


#Exploratory analysis of RingCount
#Creating graphic to better understand the correlation
ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HDonors, colour=HAcceptors)) + geom_smooth(mapping = aes(x=TPSA, y=HDonors))
cor.test(x=Molecules$TPSA, y=Molecules$HDonors, method = "pearson")
#0.3 (Weak positive correlation)

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HAcceptors, colour=HDonors)) + geom_smooth(mapping = aes(x=TPSA, y=HAcceptors))
cor.test(x=Molecules$TPSA, y=Molecules$HAcceptors, method = "pearson")
#0.68 (Strong positive correlation)

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=RotableBonds))

ggplot(data=Molecules) + geom_point(mapping = aes(x=NumAtoms, y=MolWt))

ggplot(data=Molecules) + geom_point(mapping = aes(x=MolWt, y=NumAtoms, color=HDonors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=MolWt, y=NumAtoms, color=HAcceptors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=MolLogp, colour=HDonors)) + geom_smooth(mapping = aes(x=TPSA, y=MolLogp))
cor.test(x=Molecules$TPSA, y=Molecules$MolLogp, method = "pearson")
# -0.41 (Negative average correlation)


#Finding out which molecules are more polar
index <- sort(Molecules$TPSA, decreasing = TRUE)
Molecules$ZINC_ID[index]
# Top 10
# "ZINC00000913" "ZINC00000913" "ZINC00000913" "ZINC00000869" "ZINC00000869" 
# "ZINC00000869" "ZINC00000869" "ZINC00000869" "ZINC00000869" "ZINC00000869"


#Finding out which molecules are more apolar/ less polar
index <- sort(Molecules$HAcceptors, decreasing = TRUE)
index
Molecules$ZINC_ID[index]


#Selecting the ID of molecules that have regions with better Hydrogen acceptors
HighHAccept <- filter(Molecules, HAcceptors >= 10)
select(HighHAccept, ZINC_ID)



