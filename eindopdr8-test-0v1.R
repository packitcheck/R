#2018-02-14 Eindopdr8-test
#laden van benodigde libraries
library(tidyverse)
library(lubridate)
library(dplyr)

#ken variable toe aan te verwerken data
#door variabeles te gebruiken wordt code herbruikbaar voor toekomstige data frames
pad_2_data_dh_2006_2015 <- "/home/shaam/Pri/R/eindopdr8_R/eindopr8/ongevallen_dh_2006_2015.csv"

#inlezen geregistreerde	ongevallen in 2014 van website en 1 variabele aan toekennen
ongevallen_dh_2006_2015 <- read.csv(pad_2_data_dh_2006_2015, header = TRUE, sep = ",")
#View(ongevallen_dh_2006_2015)

#datum naar juiste formaat omzetten
datum_ongeval_x_formaat <- ymd(ongevallen_dh_2006_2015$Datum)
#View(datum_ongeval_x_formaat)

#ken variabele weeknummer toe
weeknr_ongeval <- isoweek(datum_ongeval_x_formaat)
#View(weeknr_ongeval)

#creeer nieuwe data frame van interesse
df_interesse <- data.frame(Datum = ongevallen_dh_2006_2015$Datum,
                           Dag = ongevallen_dh_2006_2015$Dag,
                           AangegevenMaxSnelheid = ongevallen_dh_2006_2015$AangegevenMaxSnelheid,
                           AantalSlachtoffers = ongevallen_dh_2006_2015$AantalSlachtoffers,
                           Aard = ongevallen_dh_2006_2015$Aard,
                           Afloop3 = ongevallen_dh_2006_2015$Afloop3,
                           Afloop4 = ongevallen_dh_2006_2015$Afloop4,
                           Afloop5 = ongevallen_dh_2006_2015$Afloop5,
                           BebouwdeKom = ongevallen_dh_2006_2015$BebouwdeKom,
                           DodelijkSlachtoffers = ongevallen_dh_2006_2015$DodelijkSlachtoffers,
                           Latitude= ongevallen_dh_2006_2015$Latitude,
                           Longitude = ongevallen_dh_2006_2015$Longitude,
                           Manouvre = ongevallen_dh_2006_2015$Manoeuvre,
                           OngevalID = ongevallen_dh_2006_2015$OngevalID, 
                           Tijd = ongevallen_dh_2006_2015$Tijd,
                           Wegdekken = ongevallen_dh_2006_2015$Wegdekken,
                           Wegdekken_Anders = ongevallen_dh_2006_2015$Wegdekken_Anders,
                           weeknr = weeknr_ongeval,
                           Wegsituatie = ongevallen_dh_2006_2015$Wegsituatie,
                           Wegverharding = ongevallen_dh_2006_2015$Wegverharding,
                           Wegverharding_Anders = ongevallen_dh_2006_2015$Wegverharding_Anders,
                           Wegverlichting = ongevallen_dh_2006_2015$Wegverlichting
                           )
View(df_interesse)

rbv <- lm(Latitude ~ Datum + Dag + AangegevenMaxSnelheid + AantalSlachtoffers 
          + Aard + Afloop3 + Afloop4 + Afloop5 + BebouwdeKom + DodelijkSlachtoffers + 
            Latitude + Longitude + Manouvre + OngevalID + Tijd + Wegdekken + 
            Wegdekken_Anders + weeknr + Wegsituatie + Wegverharding + 
            Wegverharding_Anders + Wegverlichting, data=df_interesse)

#inspect data
#head(df_interesse)
#tail(df_interesse)
#summary(df_interesse)

#Groepeer interessante data max snelheid
gegrouppeerde_interessante_data_AangegevenMaxSnelheid <- df_interesse %>%
  group_by(Longitude,Latitude) %>% summarise("n_ongeval/week" = (count = n()))
ggplot(gegrouppeerde_interessante_data_AangegevenMaxSnelheid, aes(x=weeknr_ongeval)) + 
  geom_line(aes(y = `n_ongeval/week`),          
            color = "dark red") 
  #sort(gegrouppeerde_interessante_data_AangegevenMaxSnelheid, decreasing = TRUE)
View(gegrouppeerde_interessante_data_AangegevenMaxSnelheid)
#Groepeer interessante data Longitude
gegrouppeerde_interessante_data_Longitude <- df_interesse %>%
  group_by(Longitude) #%>%
  #summarize(DAY.MAX = round(max(OngevalID), 3),
   #         DAY.MIN = round(min(OngevalID), 3),
    #        DAY.AVG = round(mean(OngevalID), 3))

View(gegrouppeerde_interessante_data_max_snelheid)

View(gegrouppeerde_interessante_data_Longitude)

#overzicht om onderzoeksvraag te formuleren
ggplot(df_interesse, mapping = aes(x= Manouvre)) +
  geom_bar() + 
  facet_wrap(~ Dag)

#Stel dataframe op om kruistabel op te stellen
df_interesse <- data.frame((DodelijkSlachtoffers = filter(ongevallen_dh_2006_2015, DodelijkSlachtoffers >= 1))) 
#Afloop3 = ongevallen_dh_2006_2015$Afloop3, 
#Afloop4 = ongevallen_dh_2006_2015$Afloop4, 
#Afloop5 = ongevallen_dh_2006_2015$Afloop5) %>% group_by(Datum)
View(df_interesse)

df_interesse <- data.frame(weeknr_ongeval, Datum = ongevallen_dh_2006_2015$Datum, 
                           DodelijkSlachtoffers = ongevallen_dh_2006_2015$DodelijkSlachtoffers, 
                           Afloop3 = ongevallen_dh_2006_2015$Afloop3, 
                           Afloop4 = ongevallen_dh_2006_2015$Afloop4, 
                           Afloop5 = ongevallen_dh_2006_2015$Afloop5) %>% group_by(Datum)


#voorbeeld syntax filteren: haal gegevens van dh eruit en plaats deze in df
#ongevallen2014_dh <- filter(ongevallen2014, GemeenteNaam == "'s-Gravenhage")
#View(ongevallen2014_dh)

#Kruistabel om gekozen variabelen te visualiseren
variabelen_kruistabel_ongevallen_dh_2006_2015 <- data.frame(Datum = ongevallen_dh_2006_2015$Datum, 
                                                            DodelijkSlachtoffers = ongevallen_dh_2006_2015$DodelijkSlachtoffers) %>%
  group_by(weeknr_ongeval) 

#groepeer op Datum en bereken totaal per Datum 
n_reg_ongevallen_per_datum <- ongevallen_dh_2006_2015 %>% 
  group_by(Datum) %>% summarise("n_ongeval_per_datum" = (count = n()))
datum_max_ongevallen <- arrange(n_reg_ongevallen_per_datum, desc(n_ongeval_per_datum))
View(datum_max_ongevallen)

#groepeer op DodelijkSlachtoffers per Datum en bereken totaal DodelijkSlachtoffers per Datum
n_dodelijkslachtoffers_per_datum <- ongevallen_dh_2006_2015 %>% 
  group_by(Datum) %>% summarise("n_dodelijkslachtoffers_per_datum" = (count = n()))
datum_max_ongevallen <- arrange(n_reg_ongevallen_per_datum, desc(n_ongeval_per_datum))
View(datum_max_ongevallen)

#grafische weergave ongevallen per Datum
ggplot(ongevallen2014_dh, mapping = aes(x=Dag)) +
  geom_bar() +
  facet_wrap(~ WijkNaam)

#groepeer op Latitude en Longitude en bereken totaal per Latitude, Longitude 
n_reg_ongevallen_per_long_lat <- ongevallen2014_dh %>% 
  group_by(Longitude, Latitude) %>% summarise("n_ongeval_per_lat_long" = (count = n()))
long_lat_max_ongevallen <- arrange(n_reg_ongevallen_per_long_lat, desc(n_ongeval_per_lat_long))
View(long_lat_max_ongevallen)

#grafische weergave ongevallen per long en lat
ggplot(ongevallen2014_dh, mapping = aes(x=Dag)) +
  geom_bar() +
  facet_wrap(~ Latitude)

#grafische weergave vrouwen zijn betere bestuurders dan mannen
ggplot(ongevallen2014_dh, mapping = aes(x=Dag)) +
  geom_bar() +
  facet_wrap(~ Geslacht)

#groepeer op StraatNaam en bereken totaal per StraatNaam 
n_reg_ongevallen_per_straatnaam <- ongevallen2014_dh %>% 
  group_by(StraatNaam) %>% summarise("n_ongeval_per_straatnaam" = (count = n()))
straatnaam_max_ongevallen <- arrange(n_reg_ongevallen_per_straatnaam, desc(n_ongeval_per_straatnaam))
View(straatnaam_max_ongevallen)

#grafische weergave ongevallen per long en lat
ggplot(ongevallen2014_dh, mapping = aes(x=Datum)) +
  geom_bar() +
  facet_wrap(~ StraatNaam)

#groepeer op Merk en bereken totaal per Merk 
n_reg_ongevallen_per_merk <- ongevallen2014_dh %>% 
  group_by(Merk) %>% summarise("n_ongeval_per_merk" = (count = n()))
merk_max_ongevallen <- arrange(n_reg_ongevallen_per_merk, desc(n_ongeval_per_merk))
View(merk_max_ongevallen)

#grafische weergave ongevallen per long en lat
ggplot(ongevallen2014_dh, mapping = aes(x=Datum)) +
  geom_bar() +
  facet_wrap(~ Merk)

#datum uit ongevallen2014 halen en 1 variabele aan toekennen
datum_ongeval <- ongevallen2014$Datum

#datum naar juiste formaat omzetten
datum_ongeval_x_formaat <- ymd(datum_ongeval)

#ken variabele weeknummer toe
weeknr_ongeval <- isoweek(datum_ongeval_x_formaat)

#weeknr naar dag van de week
dag_van_week <- wday(datum_ongeval_x_formaat, label = TRUE, abbr = TRUE)

#tabel datum_ongeval_x_formaat, weeknr_ongeval en dag_van_week
#tabel_datum_ongeval_x_formaat_weeknr_ongeval_dag_van_week <- data_frame(datum_ongeval_x_formaat, weeknr_ongeval, dag_van_week) 
#View(tabel_datum_ongeval_x_formaat_weeknr_ongeval_dag_van_week)
tabel_OngevalID_weeknr_ongeval_dag_van_week <- data_frame(OngevalID =
                                                            ongevallen2014$OngevalID, weeknr_ongeval, dag_van_week)

#2017-11-23 PF2-1.3ii
#Genereer	een	lijndiagram	waarin	het	aantal	geregistreerde	ongevallen	per	week	
#wordt	weergegeven
n_reg_ongevallen <- tabel_OngevalID_weeknr_ongeval_dag_van_week %>% 
  group_by(weeknr_ongeval) %>% summarise("n_ongeval/week" = (count = n()))
ggplot(n_reg_ongevallen, aes(x=weeknr_ongeval)) + 
  geom_line(aes(y = `n_ongeval/week`),          
            color = "dark red") 

#2017-11-23 PF2-1.3iii
#Genereer	een	kruistabel met	in	de	rijkoppen	het	weeknummer,	in	de	
#kolomkoppen	de	afloop 
#(gebruik	de	variabele	Afloop5 (ga	zelf	na	wat	het	verschil is tussen	de	variabelen	Afloop3,	Afoop4	en	
#Afloop5) en	in	de	cellen de	aantallen	geregistreerde	ongevallen
variabelen_kruistabel_ongeval2014 <- data.frame(weeknr = tabel_OngevalID_weeknr_ongeval_dag_van_week$weeknr_ongeval, 
                                                afloop3 = ongevallen2014$Afloop3, afloop4 = ongevallen2014$Afloop4, 
                                                afloop5 = ongevallen2014$Afloop5, dag = ongevallen2014$Dag, 
                                                datum = ongevallen2014$Datum) %>%
  group_by(afloop5, afloop4, afloop5, weeknr) %>%
  #group_by(afloop4, afloop4, afloop5, weeknr) %>%  
  #group_by(afloop3, afloop4, afloop5, weeknr) %>%
  summarise(aantal_afloop = (count = n()))
#View(variabelen_kruistabel_ongeval2014)
#typeof(tabel_OngevalID_weeknr_ongeval_dag_van_week)
#View(tabel_OngevalID_weeknr_ongeval_dag_van_week)
kruistabel_ongeval2014 <- spread(variabelen_kruistabel_ongeval2014, weeknr, aantal_afloop)
View(kruistabel_ongeval2014)