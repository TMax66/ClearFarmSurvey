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
  gs4_auth(token = drive_token())
    mydrive<-drive_find(type = "spreadsheet") 
  id<-mydrive[1,2]
  dati<-read_sheet(id$id)
  #dati<-dati[,-1]
        

  

nomi<-c("info",
        "cons", #<-d1
        "reg", #<-d2
        "ruolo", #<-d3
        "tipo", #<-d4
        "lattazione", #d5
        "asciutte", #d6
        "nmanze", #d7
        "biogas", #d8
        "sensori", #d9
        "interesse", #d10
        "noint", # d11
        
        "sambbovlat", "sambbovasc", "sambmanze","sambvit",  #<--d12
        
        "sabovlat", "sabovasc", "samanze", "savit", #<--d13
          
        "sambbovlat2", "sambbovasc2", "sambmanze2", "sambvit2", #<---d14
        
        "sabovlat2", "sabovasc2", "samanze2", "savit2", #<---d15
        
        "collare", "bolorum", "marca", "pedom", "altro33", "al34", #<---d16
        
        "mov", "staz", "eat", "rumina", "Sbcs", "estro", "parto", "distress", "altro43", "al44", #<---d18
        
        "clima", "qair", "altro47", "al48", #<---d20
        
        "temp","umid","co2","nh3","luce", "altro54","al55", #<---d22
        
        "collare2","bolorum2","marca2","pedom2","altro60","al61", #<---d24
        
        "mov2","staz2", "eat2","rumina2","Sbcs2","estro2","parto2","distress2","altro70","al71", #<---d26
        
        "clima2", "qair2", "altro74", "al75", #<---d28
        
        "temp2","umid2","bco2","bnh3","luce2", "altro81","al82", #<---d30
        
        "robot", #d32
        
        "prodmilk","compomilk","condumilk","bcs","healthpam", "pesaut","acrob", "altro91", "al92", #d33
        
        "usedtime", #d35
        
        "agevo", #d36
        
        "qualprod", "welfare","ecoaz", "altro98", "al99", #d37
        
        "easyuse", #d39
        
        "lesioni", "rotsens", "lostdata", "internet", "asstecn", "riftecn", "tempogdati", "altro108", "al109", #d40
        
        "invest", #d42
        
        "likert", #d43
        
        "innov" ) #d44

names(dati)<-nomi

dati$reg<-casefold(dati$reg, upper=TRUE)
    






