#2017-11-23 PF2-1.3i
#laden van benodigde libraries
library(tidyverse)
library(lubridate)
library(dplyr)

#ken variable toe aan te verwerken data
#door variabeles te gebruiken wordt code herbruikbaar voor toekomstige data frames
url_naar_data <- "https://ckan.dataplatform.nl/dataset/bdfadb4d-b9b6-4946-a7da-41265dda8e90/resource/811825a3-0b9e-45c0-a962-f018aa34da08/download/ongevallen-2014.csv"

#inlezen geregistreerde	ongevallen in 2014 van website en 1 variabele aan toekennen
ongevallen2014 <- read.csv(url_naar_data, header = TRUE, sep = ",")

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
