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
  # library(topicmodels)
  # library(tidytext)
  # library(tm)
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
    



 dati %>% 
  filter(moresens=="Sì") %>% 
  select(12:15) %>% rename('bov in lattazione'=bovlat, 'bov in asciutta'=bovasc ,
                           'manze'= bmanze, 'vitelli'= vit) %>% 
  pivot_longer(1:4, names_to = "categoria", values_to = "risposta") %>% 
  group_by(categoria, risposta) %>% 
  summarise(n=n()) %>% 
  #filter(risp=="SI") %>% 
  ungroup() %>% 
  arrange(n) %>% 
  mutate(categoria=factor(categoria, unique(categoria))) %>% 
  ggplot(aes(x=categoria, y=n, fill=risposta))+
  scale_fill_manual(values = c("steelblue", "blue"))+
  geom_bar(stat = "identity")+labs(title="categorie di animali per le quali si utilizzano sensori",
                                   y="n.aziende", x='')+
  coord_flip()+
  transition_states(categoria)+
  ease_aes('sine-in-out')


 anim_save("prova.gif")












dati %>% 
  filter(sensori=="Sì")%>% 
  select(agevo) %>% 
  mutate(agevo=factor(agevo)) %>% 
  group_by(agevo) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=agevo, y=n))+geom_bar(stat="identity", fill="steelblue3")+
  labs(x='', y='n.aziende', title="questa tecnologia ha agevolato il suo lavoro")


