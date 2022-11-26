#Importing and installing ggplot library
install.packages('tidyverse')
library(tidyverse)

Molecules <- read.csv("C:/Users/mathe/OneDrive/Área de Trabalho/GitHub/Zinc12-description/File_name.csv", header = T, sep=";")
names(Molecules)

#Análise exploratória do numero de átomos 
boxplot(Molecules$NumAtoms)
mean(Molecules$NumAtoms)
median(Molecules$NumAtoms)
hist(Molecules$NumAtoms)

#Análise exploratória do peso molecular
boxplot(Molecules$MolWt)
mean(Molecules$MolWt)
median(Molecules$MolWt)
hist(Molecules$MolWt)

#Anális explotatória do TPSA
boxplot(Molecules$TPSA)
hist(Molecules$TPSA)
mean(Molecules$TPSA)
median(Molecules$TPSA)

#Anális3 explotatória do MolLogp
boxplot(Molecules$MolLogp)
hist(Molecules$MolLogp)
mean(Molecules$MolLogp)
median(Molecules$MolLogp)

#Anális3 explotatória do RingCount


ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HDonors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HAcceptors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=RotableBonds))

ggplot(data=Molecules) + geom_point(mapping = aes(x=NumAtoms, y=MolWt))

ggplot(data=Molecules) + geom_point(mapping = aes(x=RindCount, y=RotableBonds))

