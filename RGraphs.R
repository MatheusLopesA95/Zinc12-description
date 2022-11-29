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


ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HDonors, colour=HAcceptors)) + geom_smooth(mapping = aes(x=TPSA, y=HDonors))
cor.test(x=Molecules$TPSA, y=Molecules$HDonors, method = "pearson")
#0.3 (Correlação positiva fraca)

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=HAcceptors, colour=HDonors)) + geom_smooth(mapping = aes(x=TPSA, y=HAcceptors))
cor.test(x=Molecules$TPSA, y=Molecules$HAcceptors, method = "pearson")
#0.68 (Correlação positiva forte)

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=RotableBonds))

ggplot(data=Molecules) + geom_point(mapping = aes(x=NumAtoms, y=MolWt))

ggplot(data=Molecules) + geom_point(mapping = aes(x=MolWt, y=NumAtoms, color=HDonors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=MolWt, y=NumAtoms, color=HAcceptors))

ggplot(data=Molecules) + geom_point(mapping = aes(x=TPSA, y=MolLogp, colour=HDonors)) + geom_smooth(mapping = aes(x=TPSA, y=MolLogp))
cor.test(x=Molecules$TPSA, y=Molecules$MolLogp, method = "pearson")
# -0.41 (Correlação negativa média)

#Descobrindo quais são as moleculas mais polares
index <- sort(Molecules$TPSA, decreasing = TRUE)
Molecules$ZINC_ID[index]
# Top 10
# "ZINC00000913" "ZINC00000913" "ZINC00000913" "ZINC00000869" "ZINC00000869" 
# "ZINC00000869" "ZINC00000869" "ZINC00000869" "ZINC00000869" "ZINC00000869"

#Descobrindo quais são as moleculas mais apolares
index <- sort(Molecules$HAcceptors, decreasing = TRUE)
index
Molecules$ZINC_ID[index]

#Selecionando o ID das mol com mais regiões aceptoras de H
HighHAccept <- filter(Molecules, HAcceptors >= 10)
select(HighHAccept, ZINC_ID)
