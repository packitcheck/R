#2018-02-14 Eindopdr8-test
#laden van benodigde libraries

setwd("/home/shaam/Pri/R/aio-stoomreinigers-nl/")

# Functie Packlib - laden libaries en wanneer package niet beschikbaar is installeren
inladen <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

# Definieer benodigde pakketten
benodigde_packages  <- c("foreign",
                         "NLP",
                         "scales",
                         "tidyverse",
                         "caret",
                         "e1071",
                         "tm",
                         "readxl",
                         "lubridate",
                         "ggmap",
                         "ggplot2",
                         "devtools",
                         "RDSTK",
                         "rgdal")
#if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

# Installeren / inladen van de gebruikte libraries
inladen(benodigde_packages)

# Even opruimen
rm(benodigde_packages, inladen)

#ken variable toe aan te verwerken data
#door variabeles te gebruiken wordt code herbruikbaar voor toekomstige data frames
pad_2_data_dh_2006_2015 <- "/home/shaam/Pri/R/eindopdr8_R/eindopr8/ongevallen_dh_2006_2015.csv"

#inlezen geregistreerde	ongevallen in 2014 van website en 1 variabele aan toekennen
#om hoofdonderzoeksvraag te beantwoorde:
#aantal	verkeersongevallen	binnen	de	gemeente	den haag te	verminderen
ongevallen_dh_2006_2015 <- read.csv(pad_2_data_dh_2006_2015, header = TRUE, sep = ",")

#info data
#str(ongevallen_dh_2006_2015)

#datum naar datum formaat jjjjmmdd
datum2jjjjmmdd_ongeval_dh_2006_2015 <- ymd(ongevallen_dh_2006_2015$Datum)

#reken weeknr uit datum_jjjj_dd_mm_formaat
weeknr_ongeval_dh_2006_2015 <- isoweek(datum2jjjjmmdd_ongeval_dh_2006_2015)

#weeknr toevoegen aan originele data frame
ongevallen_dh_2006_2015 <- ongevallen_dh_2006_2015 %>% mutate(weeknr_ongeval_dh_2006_2015)

# ongeval_dh_2006_2015 <- select(ongevallen_dh_2006_2015, (everything)) %>% mutate(ongevallen_dh_2006_2015, weeknr_ongeval_dh_2006_2015 = weeknr)

#Zet datum om naar dag van de week met zondag zijnde 1ste dag van week
ongevallen_dh_2006_2015_Dag <- factor(ongevallen_dh_2006_2015$Dag ,levels= c("ZO", "MA", "DI", "WO", "DO", "VR", "ZA")) # analyse per dag

#Datum splitsen in Jaar-Maand-Dag
ongevallen_dh_2006_2015 <- select(ongevallen_dh_2006_2015, everything()) %>%
  separate(Datum, into = c("Jaar", "MaandDag"), remove = FALSE, sep = 4) %>%
  separate(MaandDag, into = c("Maand", "Dag van de maand"), remove = TRUE, sep = 2)

#Visualiseer locatie omgelukken 2006 t/m 2015
register_google(key = "AIzaSyC0ewYIpxMCOQjwhbmiaKGMr2LE2ITloao")
map_dh <- get_map(location = "houston, texas", zoom = 12, scale = 2, maptype = "satellite")
#map_dh <- get_googlemap("The Hague, The Netherlands", zoom = 10, size = c(640, 640), scale = 2) %>% ggmap()
map_dh

###visualisatie ongevallen per jaar 
##Jaar 2006
ongevallen_dh_2006 <- filter(ongevallen_dh_2006_2015, Jaar == "2006")
plaats_ongeval_dh_2006_vlekken <- ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = count), data = ongevallen_dh_2006))  + geom_point(color="violet",alpha=0.3)
ongevallen_dh_2006_bloxplot <- ggplot(ongevallen_dh_2006, aes(x=Longitude, y=Latitude, group = 1)) + geom_boxplot()
aantal_ongevallen_avond_06_15 <- ongevallen_dh_2006 %>%
  group_by(Jaar, Maand, weeknr_ongeval_dh_2006_2015, Dag) %>%
  summarise("aantal ongevallen" = (count=n()))
kruistabel_ongeval2006 <- spread(ongevallen_dh_2006, Longitude, Latitude)
View(kruistabel_ongeval2006)
#vlekken visualisatie
plaats_ongeval_dh_2006_vlekken
#bloxplot visualisatie
ongevallen_dh_2006_bloxplot
n_reg_ongevallen_per_datum <- ongevallen_dh_2006 %>% 
  group_by(Datum) %>% summarise("n_ongeval_per_datum" = (count = n()))
datum_max_ongevallen_2006 <- arrange(n_reg_ongevallen_per_datum, desc(n_ongeval_per_datum))
View(datum_max_ongevallen_2006)
#mlm_2006 <- (OngevalID ~ Longitude, Latitude, ongevallen_dh_2006, main = test) 
ggplot(ongevallen_dh_2006, mapping = aes(x=OngevalID)) +
  geom_bar() +
  facet_wrap(~ WijkNaam)

##Jaar 2007
ongevallen_dh_2007 <- filter(ongevallen_dh_2006_2015, Jaar == "2007")
plaats_ongeval_dh_2007_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2007))  + geom_point(color="magenta",alpha=0.3)
ongevallen_dh_2007_bloxplot <- ggplot(ongevallen_dh_2007, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2007_vlekken
#bloxplot visualisatie
ongevallen_dh_2007_bloxplot

##Jaar 2008
ongevallen_dh_2008 <- filter(ongevallen_dh_2006_2015, Jaar == "2008")
plaats_ongeval_dh_2008_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2008))  + geom_point(color="purple",alpha=0.3)
ongevallen_dh_2008_bloxplot <- ggplot(ongevallen_dh_2008, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2008_vlekken
#bloxplot visualisatie
ongevallen_dh_2008_bloxplot

##Jaar 2009
ongevallen_dh_2009 <- filter(ongevallen_dh_2006_2015, Jaar == "2009")
plaats_ongeval_dh_2009_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2009))  + geom_point(color="blue",alpha=0.3)
ongevallen_dh_2009_bloxplot <- ggplot(ongevallen_dh_2009, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2009_vlekken
#bloxplot visualisatie
ongevallen_dh_2009_bloxplot

##Jaar 2010
ongevallen_dh_2010 <- filter(ongevallen_dh_2006_2015, Jaar == "2010")
plaats_ongeval_dh_2010_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2010))  + geom_point(color="red",alpha=0.3)
ongevallen_dh_2010_bloxplot <- ggplot(ongevallen_dh_2010, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2010_vlekken
#bloxplot visualisatie
ongevallen_dh_2010_bloxplot

##Jaar 2011
ongevallen_dh_2011 <- filter(ongevallen_dh_2006_2015, Jaar == "2011")
plaats_ongeval_dh_2011_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2011))  + geom_point(color="brown",alpha=0.3)
ongevallen_dh_2011_bloxplot <- ggplot(ongevallen_dh_2011, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2011_vlekken
#bloxplot visualisatie
ongevallen_dh_2011_bloxplot

##Jaar 2012
ongevallen_dh_2012 <- filter(ongevallen_dh_2006_2015, Jaar == "2012")
plaats_ongeval_dh_2012_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2012))  + geom_point(color="orange",alpha=0.3)
ongevallen_dh_2012_bloxplot <- ggplot(ongevallen_dh_2012, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2012_vlekken
#bloxplot visualisatie
ongevallen_dh_2012_bloxplot

##Jaar 2013
ongevallen_dh_2013 <- filter(ongevallen_dh_2006_2015, Jaar == "2013")
plaats_ongeval_dh_2013_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2013))  + geom_point(color="black",alpha=0.3)
ongevallen_dh_2013_bloxplot <- ggplot(ongevallen_dh_2013, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2013_vlekken
#bloxplot visualisatie
ongevallen_dh_2013_bloxplot

##Jaar 2014
ongevallen_dh_2014 <- filter(ongevallen_dh_2006_2015, Jaar == "2014")
### originele data set latitude is longitude daarom y=x en x=y naam x en y nog aan te passen
Longitude <- ongevallen_dh_2014$Latitude
Latitude <- ongevallen_dh_2014$Longitude
new_df_ongevallen_dh_2014 <- select(ongevallen_dh_2014, everything(), -Longitude, -Latitude)
ongevallen_dh_2014 <- cbind(new_df_ongevallen_dh_2014, Longitude, Latitude)
plaats_ongeval_dh_2014_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2014))  + geom_point(color="green",alpha=0.3)
ongevallen_dh_2014_bloxplot <- ggplot(ongevallen_dh_2014, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2014_vlekken
#bloxplot visualisatie
ongevallen_dh_2014_bloxplot

##Jaar 2015
ongevallen_dh_2015 <- filter(ongevallen_dh_2006_2015, Jaar == "2015")
plaats_ongeval_dh_2015_vlekken <-  ggmap(map_dh, base_layer = ggplot(aes(x = Longitude, y = Latitude, size = Jaar), data = ongevallen_dh_2015))  + geom_point(color="turquoise",alpha=0.3)
ongevallen_dh_2015_bloxplot <- ggplot(ongevallen_dh_2015, aes(x=Longitude, y=Latitude)) + geom_boxplot()
#vlekken visualisatie
plaats_ongeval_dh_2015_vlekken
#bloxplot visualisatie
ongevallen_dh_2015_bloxplot

#gebruik quick map voor 1 indruk
map_dh_2006_2016 <- qmap(location = c(lon = 52.04523, lat = 4.206037), zoom = 14, source = 'google')


#aantal verkeersongevallen per jaar
#n_ongevalid_per_jaar <- ongevallen_dh_2006_2015 %>% group_by(Jaar) %>%
 #  summarise("tot_n_ongevalid_per_jaar" = (count = n())) 

#n_ongevalid_per_maand_dh_2006_2015 <- ongevallen_dh_2006_2015 %>% group_by(Jaar, Maand)%>%
 # summarise("tot_n_ongevalid_per_maand" = (count = n()))

#ggplot(n_ongevalid_per_maand_dh_2006_2015, mapping = aes(x = Jaar)) +
 # geom_bar() +
  # facet_wrap(~ tot_n_ongevalid_per_maand)
   
#ggplot(n_ongevalid_per_maand_dh_2006_2015, mapping = aes(x=tot_n_ongevalid_per_maand)) +
 # geom_bar() + facet_wrap(~ Maand)
 
#

#### SPLITSEN/SAMENVOEGEN VARIABELEN ####
#


# afsplitsen uren en minuten, oordprokelijke kolom tijd behouden
# Alle waarden van tijd omzetten naar vier cijfer, 
# door afdwingen van format vier getallen (%04d)
ongevallen_dh_2006_2015$Tijd <-  sprintf("%04d", as.integer(ONGEVALLEN_2006_2015$Tijd))
ongevallen_dh_2006_2015      <- select(ongevallen_dh_2006_2015, everything()) %>%
  separate(Tijd, into = c("Uur", "Minuut"), remove = FALSE, sep = 2)

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
                           Wegverlichting = ongevallen_dh_2006_2015$Wegverlichting)
#View(df_interesse)

p <- ggplot(ongevallen_dh_2006_2015, aes(Afloop3, AantalSlachtoffers)) + geom_point() + stat_smooth() + facet_wrap(~Jaar)
p
rbv <- lm(AantalSlachtoffers ~ AangegevenMaxSnelheid + Datum + Dag + DodelijkSlachtoffers + Latitude + Longitude + Tijd + weeknr, data=df_interesse)
rbv <- lm(AantalSlachtoffers ~ AangegevenMaxSnelheid + Datum + DodelijkSlachtoffers + Latitude + Longitude, data=df_interesse)
rbv <- lm(AantalSlachtoffers ~ Tijd + weeknr, data=df_interesse)


summary(rbv)
