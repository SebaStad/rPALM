#Libraries for shiny GUI
library(shiny)
library(shinyTree)
library(DT)

#Libraries for static-driver
library(R6)
library(ncdf4)
library(sp)
library(rgdal)
library(ggplot2)
library(reshape2)


#source("base_functions.R")


Building   <- "Building"
Pavement   <- "Pavement"
Vegetation <- "Vegetation"
Water      <- "Water"


Buildtypes <- c("Wohn bis 1950", "Wohn 1950 bis 2000", "Wohn seit 2000",
                "Buero bis 1950", "Buero 1950 bis 2000", "Buero seit 2000")
Pavetypes  <- c("Nutzerdefiniert", "Unbekannter Asphalt", "Asphalt",
                "Beton", "Pflaster", "Pflastersteine (Paving)", "Pflastersteine (Cobble)",
                "Metal", "Holz", "Kies", "Feiner Kies", "Kieselstein", "Hackschnitzel",
                "Tartan", "Kunstrasen", "Lehm", "Gebaeude (Dummy)")
Vegtypes   <- c("Nutzerdefiniert", "Erdboden", "Feld (Getreide)", "Kurzes Gras", "Immergrüne Nadelbäume",
                "Laubabwerfender Nadelbaum", "Immergrüner Laubbaum", "Laubbaum", "Hohes Gras",
                "Wüste", "Tundra", "Bewässertes Feld", "Halbwüste", "Gletscher (funktioniert nicht)",
                "Suempfe und Marsche", "Immergrüne Straeucher", "Laubabwerfende Straeucher", "Mischwald",
                "Unterbrochener Wald")
Wattypes  <-  c("Nutzerdefiniert", "See", "Fluss", "Ozean", "Teich", "Brunnen")
