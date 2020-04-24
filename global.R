#######codici per ottenere l'autorizzazione al drive di google da fare una sola volta###
# library(googledrive)
# options(gargle_oauth_cache = ".secrets")
# gargle::gargle_oauth_cache()
# drive_auth()
# list.files(".secrets/")#<---questo codice fa solo vedere il file presente nella cartella .secrets creata dal codice
##precedente... la cartella .secrets deve essere inserita tra i documenti da mettere nel deploy per le applicazioni shiny
####################################################################
  library(shiny)
  library(shinydashboard)
  library (DT)
  library(tidyverse)
  library(googlesheets4)
  library(ggthemes)
  library(googledrive)
  library(patchwork)
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = TRUE
  )
  drive_auth()
  sheets_auth(token = drive_token())
    mydrive<-drive_find(type = "spreadsheet") 
  id<-mydrive[1,2]
  dati<-read_sheet(id$id)
  dati<-dati[,-1]
        

  

nomi<-c("reg","ruolo", "tipo","lattazione", "asciutte", "nmanze","biogas","sensori",
        "interesse", "noint",  "sambbovlat","sambbovasc","sambmanze","sambvit",
        "sabovlat","sabovasc","samanze", "savit",
        "robot","prodmilk","compomilk","condumilk","bcs","healthpam", "pesaut","acrob","al24","al25",
        "collare","bolo","orecc","pedom","al30", "al31","mov","staz","eat","rumina",
        "Sbcs","estro","parto","distress","al40","al41","clima", "qair", "altro44", "al45",
        "temp","umid","co2","nh3","luce", "al51","al52","usedtime","agevo", "qualprod",
        "welfare","ecoaz","al58","al59","easyuse","innov")

names(dati)<-nomi
    








