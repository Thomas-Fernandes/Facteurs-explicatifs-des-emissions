rm(list = ls())

library(readxl)
library(tidyverse)
library(stargazer)
library(lubridate)
library(plotly)
library(ggplot2)

#---------------------------
#importation des données----
#---------------------------

#Emissions de CO2/hab
Emissions_CO2 <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/CO2_per_capita.csv")
Emissions_CO2 <- as.data.frame(Emissions_CO2)

#Pib/hab
Pib <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/GDP_per_capita.csv")
Pib <- as.data.frame(Pib)

#Températures
Temperature <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/Global_Temperature.csv")
Temperature <- as.data.frame(Temperature)

#Population
Population <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/Population.csv")
Population <- as.data.frame(Population)

#Niveau de la mer
#Niveau_mer <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/Change_in_Mean_Sea_Levels.csv")
#Niveau_mer <- as.data.frame(Niveau_mer)
#DB finalement pas utilisée car trop compliquée à traiter
#Les changement moyen du niveau de la mer sont classés par mer/ocean et ne sont ratachés à aucun pays
#Il faudrait associé chaque mer à chaque pays ou utiliser la base au format geojson, ce qui dans les deux cas est bien trop long


#--------------------------
#Traitement des données----
#--------------------------

#On empile les données d'abord pour pouvoir plot, puis on les met de sort à avoir 1 colonne par pays


#------Emissions de CO2/hab--------

#On renomme les colonnes et on supprime les colonnes inutiles
Emissions_CO2 <- Emissions_CO2[,-2]
colnames(Emissions_CO2) <- c("Pays", "Année", "Emissions_CO2")

#On ne garde que les dates après 1961
Emissions_CO2 <- Emissions_CO2 %>% filter(Année >= 1961)

#Graph des emissions de CO2 pour l'Autriche, l'Irlande, la Chine et l'Inde
Emissions_CO2 %>% 
  filter(Pays %in% c("Austria", "Ireland", "China", "India")) %>% 
  ggplot(aes(x = Année, y = Emissions_CO2, color = Pays)) +
  geom_line() +
  labs(title = "Evolution des emissions de CO2/hab", x = "Année", y = "Emissions de CO2/hab") +
  theme(plot.title = element_text(hjust = 0.5))

#On spread les données pour avoir une colonne pour chaque pays
Emissions_CO2 <- Emissions_CO2 %>% spread(Pays, Emissions_CO2)

#Si une colonne contien NA, on la supprime
Emissions_CO2 <- Emissions_CO2[,-which(colSums(is.na(Emissions_CO2)) > 0)]

#------Pib/hab--------

Pib <- Pib[,-c(2,3,4)]

#On renomme la colonne country name en Pays pour uniformiser les tables
colnames(Pib)[1] <- "Pays"

#On transpose les données en gardant une colonne pour les années (àprès 1961)
Pib <- Pib %>% pivot_longer(cols = -Pays, names_to = "Année", values_to = "Pib")
Pib <- subset(Pib, Année >= 1961)

#On transforme la colonne Année en numérique
Pib$Année <- as.numeric(Pib$Année)

#On plot le Pib/hab pour l'Autriche, l'Irlande, la Chine et l'Inde
Pib %>% 
  filter(Pays %in% c("Autriche", "Irlande", "Chine", "Inde")) %>% 
  ggplot(aes(x = Année, y = Pib, color = Pays)) +
  geom_line() +
  labs(title = "Evolution du Pib/hab", x = "Année", y = "Pib/hab") +
  theme(plot.title = element_text(hjust = 0.5))

#Une colonne par pays
Pib <- na.omit(Pib)
Pib <- Pib %>% spread(Pays, Pib)
Pib <- Pib[,-which(colSums(is.na(Pib)) > 0)]

#------Températures--------

#On supprime les colonnes inutiles
Temperature <- Temperature[,-c(1,3:10)]
colnames(Temperature)[1] <- "Pays"

#On supprime le F des colonnes
colnames(Temperature) <- gsub("F", "", colnames(Temperature))

#On renomme la chine comme China
Temperature$Pays <- ifelse(Temperature$Pays == "China, P.R.: Mainland", "China", Temperature$Pays)

#On transposer les données en gardant une colonne pour les années
Temperature <- Temperature %>% pivot_longer(cols = -Pays, names_to = "Année", values_to = "Temperature")

#On ne garde que les données après 1961
Temperature <- subset(Temperature, Année >= 1961)

#On transforme la colonne Année en numérique
Temperature$Année <- as.numeric(Temperature$Année)

#On plot la température on utilise loess pour avoir une courbe lisse

Temperature %>% 
  filter(Pays %in% c("Austria", "Ireland", "China", "India")) %>% 
  ggplot(aes(x = Année, y = Temperature, color = Pays)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Evolution de la température", x = "Année", y = "Température") +
  theme(plot.title = element_text(hjust = 0.5))

#Une colonne par pays
Temperature <- na.omit(Temperature)
Temperature <- Temperature %>% spread(Pays, Temperature)
Temperature <- Temperature[,-which(colSums(is.na(Temperature)) > 0)]

#------Population--------

#On supprime les colonnes inutiles
Population <- Population[,-c(2:4)]
colnames(Population)[1] <- "Pays"

#On transpose les données en gardant une colonne pour les années
Population <- Population %>% pivot_longer(cols = -Pays, names_to = "Année", values_to = "Population")
Population <- subset(Population, Année >= 1960)
Population$Année <- as.numeric(Population$Année)

#On plot la population
Population %>%
  filter(Pays %in% c("Austria", "Ireland", "China", "India")) %>% 
  ggplot(aes(x = Année, y = Population, color = Pays)) +
  geom_line() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Evolution de la croissance démographique", x = "Année", y = "Population") +
  theme(plot.title = element_text(hjust = 0.5))

#Pays en colonne
Population <- Population %>% spread(Pays, Population)

#----------------------------
#------------Modèle----------
#----------------------------

#On cherche à expliquer les émissions de CO2/hab par le Pib/hab, la température et la population
#On doit traiter les données de population de sorte à les avoir en taux de croissance

#------Taux de croissance de la population--------

#Autriche

#Nouveau dataframe avec le taux de croissance de l'Autriche
Population_Autriche <- Population[,c("Année", "Austria")]
Population_Autriche$Taux_Croissance <- (Population_Autriche$Austria - lag(Population_Autriche$Austria))/lag(Population_Autriche$Austria)*100
Population_Autriche <- Population_Autriche[-1,]

#Irlande

Population_Irlande <- Population[,c("Année", "Ireland")]
Population_Irlande$Taux_Croissance <- (Population_Irlande$Ireland - lag(Population_Irlande$Ireland))/lag(Population_Irlande$Ireland)*100
Population_Irlande <- Population_Irlande[-1,]

#Inde

Population_Inde <- Population[,c("Année", "India")]
Population_Inde$Taux_Croissance <- (Population_Inde$India - lag(Population_Inde$India))/lag(Population_Inde$India)*100
Population_Inde <- Population_Inde[-1,]

#Chine
Population_Chine <- Population[,c("Année", "China")]
Population_Chine$Taux_Croissance <- (Population_Chine$China - lag(Population_Chine$China))/lag(Population_Chine$China)*100
Population_Chine <- Population_Chine[-1,]


#On fait un nouveau dataframe pour chaque pays, avec les données correspondantes pour le modèle

#Autriche
Autriche <- data.frame(Année = Emissions_CO2$Année, Emissions_CO2 = Emissions_CO2$Austria, Pib = Pib$Autriche, Temperature = Temperature$Austria, Population = Population_Autriche$Taux_Croissance)

#Matrice de corrélation pour vérifier les hypothèses
cor(Autriche)

model1 <- lm(Emissions_CO2 ~ Pib + Temperature + Population, data = Autriche)
summary(model1)
model2 <- lm(Emissions_CO2 ~ Pib + Population, data = Autriche)
summary(model2)
model3 <- lm(Emissions_CO2 ~ Temperature + Population, data = Autriche)
summary(model3)
model4 <- lm(Emissions_CO2 ~ Pib, data = Autriche)
summary(model4)

#On exporte les résultats dans un fichier word avec stargazer
setwd("G:/Drive partagés/L3/Atelier économétrie/Projet/Models")
stargazer(model1, model2, model3, model4,
          dep.var.caption = "Dependant variable : Emissions de CO2/hab",
          type = "html",
          dep.var.labels = "Regression pour l'Autriche",
          covariate.labels = c("Pib/hab", "Temperature", "Population", "Constante"),
          out = "model_autriche.doc")

#On plot les données
ggplot(Autriche, aes(x = Pib, y = Emissions_CO2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Autriche", x = "Pib/hab", y = "Emissions de CO2") +
  theme(plot.title = element_text(hjust = 0.5))


#On plot la normalité des résidus
ggplot(model1, aes(sample = resid(model1))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normalité des résidus", x = "Quantiles théoriques", y = "Quantiles des résidus") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la densité des résidus
plot(density(model1$resid), main = "Densité des résidus", xlab = "Résidus", ylab = "Densité")

#On plot la variance des résidus
plot(model1, which = 3)

#Irlande
Irlande <- data.frame(Année = Emissions_CO2$Année, Emissions_CO2 = Emissions_CO2$Ireland, Pib = Pib$Irlande, Temperature = Temperature$Ireland, Population = Population_Irlande$Taux_Croissance)

cor(Irlande)

model1 <- lm(Emissions_CO2 ~ Pib + Temperature + Population, data = Irlande)
summary(model1)
model2 <- lm(Emissions_CO2 ~ Pib + Population, data = Irlande)
summary(model2)
model3 <- lm(Emissions_CO2 ~ Temperature + Population, data = Irlande)
summary(model3)
model4 <- lm(Emissions_CO2 ~ Pib, data = Irlande)
summary(model4)

#On exporte les résultats dans un fichier word avec stargazer
stargazer(model1, model2, model3, model4,
          dep.var.caption = "Dependant variable : Emissions de CO2/hab",
          type = "html",
          dep.var.labels = "Regression pour l'Irlande",
          covariate.labels = c("Pib/hab", "Temperature", "Population", "Constante"),
          out = "model_Irlande.doc")

#On plot les données
ggplot(Irlande, aes(x = Pib, y = Emissions_CO2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Irlande", x = "Pib/hab", y = "Emissions de CO2") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la normalité des résidus
ggplot(model1, aes(sample = resid(model1))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normalité des résidus", x = "Quantiles théoriques", y = "Quantiles des résidus") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la densité des résidus
plot(density(model1$resid), main = "Densité des résidus", xlab = "Résidus", ylab = "Densité")

#On plot la variance des résidus
plot(model1, which = 3)

#On regarde pour l'Irlande entre 1960 et 2010
Irlande <- data.frame(Année = Emissions_CO2$Année, Emissions_CO2 = Emissions_CO2$Ireland, Pib = Pib$Irlande, Temperature = Temperature$Ireland, Population = Population_Irlande$Taux_Croissance)
Irlande <- Irlande[1:51,]
model1 <- lm(Emissions_CO2 ~ Pib + Temperature + Population, data = Irlande)
summary(model1)

#Chine
Chine <- data.frame(Année = Emissions_CO2$Année, Emissions_CO2 = Emissions_CO2$China, Pib = Pib$Chine, Temperature = Temperature$China, Population = Population_Chine$Taux_Croissance)

cor(Chine)

model1 <- lm(Emissions_CO2 ~ Pib + Temperature + Population, data = Chine)
summary(model1)
model2 <- lm(Emissions_CO2 ~ Pib + Population, data = Chine)
summary(model2)
model3 <- lm(Emissions_CO2 ~ Temperature + Population, data = Chine)
summary(model3)
model4 <- lm(Emissions_CO2 ~ Pib, data = Chine)
summary(model4)

#On exporte les résultats dans un fichier word avec stargazer
stargazer(model1, model2, model3, model4,
          dep.var.caption = "Dependant variable : Emissions de CO2/hab",
          type = "html",
          dep.var.labels = "Regression pour la Chine",
          covariate.labels = c("Pib/hab", "Temperature", "Population", "Constante"),
          out = "model_Chine.doc")

#On plot les données
ggplot(Chine, aes(x = Pib, y = Emissions_CO2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Chine", x = "Pib/hab", y = "Emissions de CO2") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la normalité des résidus
ggplot(model1, aes(sample = resid(model1))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normalité des résidus", x = "Quantiles théoriques", y = "Quantiles des résidus") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la densité des résidus
plot(density(model1$resid), main = "Densité des résidus", xlab = "Résidus", ylab = "Densité")

#On plot la variance des résidus
plot(model1, which = 3)

#Inde
Inde <- data.frame(Année = Emissions_CO2$Année, Emissions_CO2 = Emissions_CO2$India, Pib = Pib$Inde, Temperature = Temperature$India, Population = Population_Inde$Taux_Croissance)

cor(Inde)

model1 <- lm(Emissions_CO2 ~ Pib + Temperature + Population, data = Inde)
summary(model1)
model2 <- lm(Emissions_CO2 ~ Pib + Population, data = Inde)
summary(model2)
model3 <- lm(Emissions_CO2 ~ Temperature + Population, data = Inde)
summary(model3)
model4 <- lm(Emissions_CO2 ~ Pib, data = Inde)
summary(model4)

#On exporte les résultats dans un fichier word avec stargazer
stargazer(model1, model2, model3, model4,
          dep.var.caption = "Dependant variable : Emissions de CO2/hab",
          type = "html",
          dep.var.labels = "Regression pour l'Inde",
          covariate.labels = c("Pib/hab", "Temperature", "Population", "Constante"),
          out = "model_Inde.doc")

#On plot les données
ggplot(Inde, aes(x = Pib, y = Emissions_CO2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Inde", x = "Pib/hab", y = "Emissions de CO2") +
  theme(plot.title = element_text(hjust = 0.5))

#On plot la normalité des résidus avec labels
ggplot(Inde, aes(sample = model1$residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normalité des résidus", x = "Quantiles théoriques", y = "Quantiles des résidus") + 
  theme(plot.title = element_text(hjust = 0.5))

#On plot la normalité des résidus
residus <- resid(model1)
plot(density(residus), main = "Densité des résidus", xlab = "Résidus", ylab = "Densité")

#On vérifie que la variance des résidus est constante
plot(model1, which = 3)

#------------------------
#----Deuxième partie-----
#------------------------

#Cette partie devait venir appuyer les limites de notre modèle
#Cependant elle contient trop de biais pour être utilisée

#On va essayer d'expliquer les émissions de CO2/hab par le nombre de désastres climatiques par habitant

#Nombre de désastres climatiques
Disaster <- read_csv("G:/Drive partagés/L3/Atelier économétrie/Projet/Données/Données finales/Climate_related_disaster.csv")
Disaster <- as.data.frame(Disaster)

#------Disaster--------

Disaster <- Disaster[,-c(1,3:11)]
colnames(Disaster)[1] <- "Pays"
Disaster$Pays <- ifelse(Disaster$Pays == "China, P.R.: Mainland", "China", Disaster$Pays)
colnames(Disaster) <- gsub("F", "", colnames(Disaster))
Disaster[is.na(Disaster)] <- 0
Disaster <- Disaster %>% pivot_longer(cols = -Pays, names_to = "Année", values_to = "Disaster")
Disaster$Année <- as.numeric(Disaster$Année)

#On fait la somme des disasters par pays et par année
Disaster <- Disaster %>% group_by(Pays, Année) %>% summarise(Disaster = sum(Disaster))

#On fait une nouvelle colonne avec disaster_cumul qui copie la colonne disaster pour faire un joli graphique
Disaster$Disaster_cumul <- Disaster$Disaster

#On fait une boucle pour cumuler les disasters par pays
for (i in 1:nrow(Disaster)) {
  if (i > 1) {
    if (Disaster$Pays[i] == Disaster$Pays[i-1]) {
      Disaster$Disaster_cumul[i] <- Disaster$Disaster_cumul[i] + Disaster$Disaster_cumul[i-1]
    }
  }
}

#On plot le cumul des disasters en fonction du temps pour l'Autriche, l'Irlande, la Chine et l'Inde
Disaster %>%
  filter(Pays %in% c("Austria", "Ireland", "China", "India")) %>% 
  ggplot(aes(x = Année, y = Disaster_cumul, color = Pays)) +
  geom_line() +
  labs(title = "Cumul des disasters", x = "Année", y = "Disaster") +
  theme(plot.title = element_text(hjust = 0.5))

#On supprime la colonne cumulative
Disaster <- Disaster[,-4]

#On met les pays en colonne
Disaster <- Disaster %>% spread(Pays, Disaster)
Disaster <- Disaster %>% select(Année, Austria, Ireland, China, India)

#On fait une boucle pour calculer le nombre de disaster par habitant
for (i in 1:nrow(Disaster)) {
  if (i > 1) {
    if (Disaster$Année[i] == Disaster$Année[i-1]) {
      Disaster$Austria[i] <- Disaster$Austria[i] / Population_Autriche$Population[i]
      Disaster$Ireland[i] <- Disaster$Ireland[i] / Population_Irlande$Population[i]
      Disaster$China[i] <- Disaster$China[i] / Population_Chine$Population[i]
      Disaster$India[i] <- Disaster$India[i] / Population_Inde$Population[i]
    }
  }
}

Disaster %>%
  filter(Année %in% c(1990:2018)) %>% 
  ggplot(aes(x = Année, y = China, color = "China")) +
  geom_line() +
  geom_line(aes(y = India, color = "India")) +
  geom_line(aes(y = Austria, color = "Austria")) +
  geom_line(aes(y = Ireland, color = "Ireland")) +
  labs(title = "Nombre de disaster par habitant", x = "Année", y = "Disaster") +
  theme(plot.title = element_text(hjust = 0.5))

#Autriche
CO2_Disaster_Autriche <- Emissions_CO2 %>% select(Année, Austria) %>% filter(Année %in% c(1980:2021))
CO2_Disaster_Autriche$Disaster <- Disaster$Austria

model2 <- lm(Disaster ~ Austria, data = CO2_Disaster_Autriche)
summary(model2)

#Inde
CO2_Disaster_Inde <- Emissions_CO2 %>% select(Année, India) %>% filter(Année %in% c(1980:2021))
CO2_Disaster_Inde$Disaster <- Disaster$India

model3 <- lm(Disaster ~ India, data = CO2_Disaster_Inde)
summary(model3)