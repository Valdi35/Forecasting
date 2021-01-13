#########################################################
# Prevision sur la consommation de biens 
# magasins d'articles de sport, de loisirs, d'instruments de musique et
# librairies dans le Michigan

#Realise par : Valdi Mayima 
############################################################

#Remettre le classeur a zero
rm(list=ls())

#Packages pour la prevision 
library(fpp2)
library(TSstudio)
library(lmtest)
library(rugarch)

#Chargement des donnees 
df <- read.csv("SERIE_10.csv",sep=',', header=TRUE)

#Renomme les variables
library(questionr)
names(df)
df<- rename.variable(df, "observation_date", "date")
df<- rename.variable(df, "SMU26000004245100001", "Serie")


#Transformer en series temporelles 
Y <- ts(df[,2], start=c(1990,1), frequency=12)

########################################################
#Analyses principales
#######################################################

#Plot de la serie
autoplot(Y) + 
  ggtitle("Retail trade") +
  ylab("Milliers de personnes")

#Rupture brutale en debut 2020
#On enleve donc cette partie de la serie

Y2 <- ts(df[,2], start=c(1990,1), end=c(2019,12), frequency=12)

autoplot(Y2) + 
  ggtitle("Retail trade : Nouvelle serie") +
  ylab("Milliers de personnes")

#On observe un mouvement a la hausse entre 1998 et 2010

#Regardons la tendance saisonnieres de chaque annees
ggsubseriesplot(Y2) + 
  ggtitle("Retail trade (saisonniers)") +
  ylab("Milliers de personnes")

#Tendances sous-saisonnieres (mensuelles)
ggmonthplot(Y2) +
  ggtitle("Retail trade (mensuelles)") +
  ylab("Milliers de personnes")

#####################################
#Test de non stationnarite de la serie 
#####################################
library(tseries)

#Test de Dickey-Fuller
adf.test(Y2, k=2)

#On utilise la premiere difference pour rendre la serie stationnaire

DY2 <- diff(Y2)
autoplot(DY2) + 
  ggtitle("Retail trade : Serie differencier") +
  ylab("Milliers de personnes")

#Test de Dickey-Fuller augmente pour serie a la premiere difference
adf.test(DY2, k=2)

#Fonction pour decomposer la serie 
d <- ts_decompose(Y2, type="additive")

#Outliers de la serie
tsoutliers(Y2)

#Diviser l'echantillon en donnees de test et d'entrainements
split_df <- ts_split(Y2, sample.out = 12)
training <- split_df$train
testing <- split_df$test

#######################
#Modele naif
######################
Naivf <- snaive(training)
autoplot(Naivf)
check_res(Naivf)

fcst4 <- forecast(Naivf, h=12)
test_forecast(actual = Y2, forecast.obj = fcst4, test= testing)

#Mesure de precision du modele naif
accuracy(fcst4, testing)

#Test de Pesaran-Timmerman
DACTest(fcst4$fitted, fcst4$x, test=c("PT"))

#######################
#Lissage exponentiel
#######################

#Trend multiplicatif et presence de saisonnalite --> Holt winters avec Tt et St

#Previsions intra echantillon

#On travaille sur les donnees d'entrainements

Hw <- ets(training)
Hw
autoplot(Hw)
check_res(Hw)

#Prevision intra-echantillon
fcst1 <- forecast(Hw, h=12)
test_forecast(actual = Y2, forecast.obj = fcst1, test= testing)

#Mesure de la prevision
accuracy(fcst1, testing)

#Test de Pesaran-Timmerman
DACTest(fcst1$fitted, fcst1$x, test=c("PT"),conf.level=0.95 )


###############
#Modele SARIMA
###############

BoxCox.lambda(Y2)

split_df2 <- ts_split(DY2, sample.out = 12)
training2 <- split_df$train
testing2 <- split_df$test

ggtsdisplay(Y2)
ggtsdisplay(DY2)

fit_arima <- auto.arima(training)

autoplot(fit_arima)
check_res(fit_arima)

fcst3 <- forecast(fit_arima, h=12)
test_forecast(actual = Y2, forecast.obj = fcst3, test= testing2)

#Mesure de la precision de la prevision
accuracy(fcst3, testing2)

###############################
#Comparaison des previsions
###############################

#Test de Diebold-Mariano
dm.test(Hw$residuals, fit_arima$residuals, alternative=c("less"),h=12)

#Test d'enveloppement
encomptest(Hw, fit_arima)

#Prevision hors-echantillon

#erreur multiplicative
#Tendance additive
#Saisonnalite multiplicative

Y3 <- ts(Y2,start=c(2006,1), end=c(2019,12), frequency=12)
autoplot(Y3) + 
  ggtitle("Retail trade") +
  ylab("Milliers de personnes")
#Prediction sur modele de prevision hors-echantillon
fcst <- forecast(Y3, model=fit_arima, h=72)
plot_forecast(fcst)
summary(fcst)
check_res(fcst)
