# necessary to run to import libraries, data, and create necessary objects

library(shiny)
library(ggplot2)
library(scales)

palette <- c("#56B4E9", "#F0E442", "#FF9999")
colorvec = c(palette[1], palette[2])

GDP = read.csv("GDP.csv", check.names = FALSE)
GDP = GDP[1:258,]
LifeExp = read.csv("lifeexpectancy.csv", check.names = FALSE)
LifeExp = LifeExp[1:258,]
InfantMortality = read.csv("infantmortality.csv", check.names = FALSE)
InfantMortality = InfantMortality[1:258,]
CellPhone = read.csv("cellphone.csv", check.names = FALSE)

#create vectors of country names
countries <- as.character(GDP[,1])
countries <- as.character(LifeExp[,1])

#create usable data frames and matrices
IM2015Data <- data.frame(InfantMortality[colnames(InfantMortality) == "2015"], check.names = FALSE, row.names = countries)
IM2011Data <- data.frame(InfantMortality[colnames(InfantMortality) == "2011"], check.names = FALSE, row.names = countries)
LifeExp2015Data <- data.frame(LifeExp[colnames(LifeExp) == "2015"], check.names = FALSE, row.names = countries)
Data2015 <- data.frame(IM2015Data, LifeExp2015Data, check.names = FALSE, row.names = countries)
colnames(Data2015) = c("Infant Mortality", "Life Expectancy")
GDPData2011 <- data.frame(GDP[colnames(GDP) == "2011"], check.names = FALSE, row.names = countries)
GDPData2011 <- data.frame(GDPData2011[1:258,], check.names = FALSE, row.names = countries[1:258])
LifeExp2011Data <- data.frame(LifeExp[colnames(LifeExp) == "2011"], check.names = FALSE, row.names = countries)
Data2011 <- data.frame(GDPData2011, LifeExp2011Data, IM2011Data, check.names = FALSE, row.names = countries)
colnames(Data2011) = c("GDP", "Life Expectancy", "Infant Mortality")
Data2011Matrix <- as.matrix(Data2011)

