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
        

  

nomi<-c("info",
        "cons",
        "reg",
        "ruolo", 
        "tipo",
        "lattazione", 
        "asciutte", 
        "nmanze",
        "biogas",
        "sensori",
        "interesse", 
        "noint", 
        
        "sambbovlat",
        "sambbovasc",
        "sambmanze",
        "sambvit",
        
        "sabovlat",
        "sabovasc",
        "samanze", 
        "savit",
          
        "2sambbovlat",
        "2sambbovasc",
        "2sambmanze",
        "2sambvit",
        
        "2sabovlat",
        "2sabovasc",
        "2samanze", 
        "2savit",
        
        "collare",
        "bolorum",
        "marca",
        "pedom",
        "altro33",
        "al34", 
        
        "mov",
        "staz",
        "eat",
        "rumina",
        "Sbcs",
        "estro",
        "parto",
        "distress",
        "altro43",
        "al44",
        
        "clima", "qair", "altro47", "al48",
        
        "temp","umid","co2","nh3","luce", "altro54","al55",
        
        "collare2","bolorum2","marca2","pedom2","altro60","al61", 
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        "robot",
        "prodmilk","compomilk","condumilk","bcs","healthpam", "pesaut","acrob","al24","al25",
        
        ,"usedtime","agevo", "qualprod",
        "welfare","ecoaz","al58","al59","easyuse","innov")

names(dati)<-nomi
    








