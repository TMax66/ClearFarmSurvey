
server <- function(input, output, session) {

####SEZIONE 1##################################################
  output$Quest <- renderValueBox({
    valueBox(
      dim(dati)[1], "# Questionari", icon = icon("keyboard"),
      color = "yellow"
    )
  })
  
  output$hsize <- renderValueBox({
    valueBox(
      median(dati$lattazione), "Mediana capi in lattazione ",
      color = "blue"
    )
  })
  
  output$dry <- renderValueBox({
    valueBox(
      median(dati$asciutte), "Mediana capi in asciutta",
      color = "blue"
    )
  })
  
  output$heif <- renderValueBox({
    valueBox(
      round(median(dati$nmanze),0), " Mediana n. Manze",
      color = "blue"
    )
  })
  
  #<<<regione
reg<-reactive({
    
    dati %>% 
      group_by(reg) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(reg=factor(reg, unique(reg))) %>% 
      ggplot(aes(x=reg, y=n))+
    geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Regione")+coord_flip()+ 
      theme(axis.text=element_text(size=12))+theme_clean()+
    geom_text(aes(y=n, label=n), hjust=2, 
              color="yellow", size=5)
    
    
  })

output$preg<-renderPlot(reg())
  
#<<<ruolo
  ruol<-reactive({
    
    dati %>% 
      group_by(ruolo) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(ruolo=factor(ruolo, unique(ruolo))) %>% 
      ggplot(aes(x=ruolo, y=n))+
      geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Ruolo in azienda")+coord_flip()+ 
      theme(axis.text=element_text(size=12))+ theme_clean()+
      geom_text(aes(y=n, label=n), hjust=2, 
                color="yellow", size=5)
    
  })
  
output$pruol<-renderPlot(ruol())
  
  #<<<<tipologia
  tipol<-reactive({
    
    dati %>% 
      group_by(tipo) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(tipo=factor(tipo, unique(tipo))) %>% 
      ggplot(aes(x=tipo, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Tipo allevamento")+coord_flip()+
      theme(axis.text=element_text(size=12))+ theme_clean()+
      geom_text(aes(y=n, label=n), hjust=2, 
                color="yellow", size=5)
      
    
  })
  
  output$ptipol<-renderPlot(tipol()) 
  
  
  #<<<<uso di sensori??##
  sensor<-reactive({
    dati %>% 
      group_by(sensori) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(sensori=factor(sensori, unique(sensori))) %>% 
      ggplot(aes(x=sensori, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Sensori in azienda?")+coord_flip()+ 
      theme(axis.text=element_text(size=12))+ theme_clean()+
      geom_text(aes(y=n, label=n), hjust=2, 
                color="yellow", size=5)
  })
  
  
  output$psensor<-renderPlot(sensor())
  
  #<<<<se no è interessato?
  yesint<-reactive({
    
    dati %>% 
      filter(sensori=="No") %>% 
      select(interesse) %>% 
      group_by(interesse) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(interesse=factor(interesse, unique(interesse))) %>% 
      ggplot(aes(x=interesse, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Se non ha sensori è interessato?")+coord_flip()+
      theme(axis.text=element_text(size=12))+ theme_clean()+
      geom_text(aes(y=n, label=n), hjust=2, 
                color="yellow", size=5)
  })

  output$pyesint<-renderPlot(yesint())
  
  #<<<non interessato ai sensori perchè....
  
  nonint<-reactive({
    
    dati %>% 
      filter(interesse=="No") %>% 
      group_by(noint) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(nonint=factor(noint, unique(noint))) %>% 
      ggplot(aes(x=noint, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
      labs(x="", title="Motivi di mancato interesse")+coord_flip()+ 
      theme(axis.text=element_text(size=12))+theme_clean()+
      geom_text(aes(y=n, label=n), hjust=2, 
                color="yellow", size=5)
    
  })
  
  output$pnonint<-renderPlot(nonint())
  

  # output$gr1<-renderPlot(
  #   
  #   (reg()/ruol()/tipol())
  # )
  # 
  # 
  # output$gr2<-renderPlot(
  #   
  #   (sensor()/yesint()/nonint())
  # )
  
  
  #<-biogas
  output$gas <- renderValueBox({
    valueBox(
      table(dati$biogas)[2] , "# aziende con biogas",
      color = "green"
    )
  })
  
  #<-sensori
  output$senso <- renderValueBox({
    valueBox(
      dati %>% 
        select(sensori) %>% 
        filter(sensori!="No") %>% 
        summarise(n=n()), "# aziende che utilizzano sensori",
      color = "red"
    )
  })
  
  
  
  
  #<-interesse <<<< fare un barplot

  output$inter <- renderValueBox({
    valueBox(
      dati %>% 
        filter(sensori=="No") %>% 
        select(interesse) %>% 
        filter(interesse!="No") %>% 
        summarise(n=n()), "# aziende interessate all'uso di sensori",
      color = "red"
    )
  })
  
  
  ###<-non interessato
  
  output$noninter <- renderValueBox({
    valueBox(
      dati %>% 
        filter(sensori=="No") %>% 
        select(interesse) %>% 
        filter(interesse=="No") %>% 
        summarise(n=n()), "# aziende non interessate all'uso di sensori",
      color = "red"
    )
  })
  
  
  
 #########SEZIONE 2################################

samb<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(11:14) %>% rename('bov in lattazione'=sambbovlat, 'bov in asciutta'=sambbovasc ,
                             'manze'= sambmanze, 'vitelli'= sambvit) %>% 
    pivot_longer(1:4, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    mutate(risposta=factor(risposta)) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    coord_flip()+theme_clean()+
    labs(title="Utilizzo sensori ambientali (cat.animali)",
         y="n.aziende", x='')
  
    
    })

output$psamb<-renderPlot(samb())


sanim<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(15:18) %>% rename('bov in lattazione'=sabovlat, 'bov in asciutta'=sabovasc ,
                             'manze'= samanze, 'vitelli'= savit) %>% 
    pivot_longer(1:4, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity", width = 0.6)+
    labs(title="Utilizzo sensori su animali",
    y="n.aziende", x='')+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    coord_flip()+theme_clean()
  
})

output$psanim<-renderPlot(sanim())


output$robott <- renderValueBox({
  valueBox(
    table(dati$robot)[2] , "# aziende che utilizzano robot di mungitura",
    color = "blue"
  )
})

inforobo<-reactive({ dati %>% 
    filter(robot=="Sì") %>% 
    select(20:27) %>% rename('produzione latte'=prodmilk,'composizione latte'=compomilk ,
                             'conducibilità'= condumilk, 'BCS'= bcs,
                             "parametri sanitari"=healthpam, "pesatura autom"=pesaut, 
                             "accesso al robot"=acrob,"altro"=al24) %>% 
    pivot_longer(1:8, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n))%>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    labs(title="tipologia di informazioni raccolte dal robot di mungitura",
         y="n.aziende",x='')+
    coord_flip()+theme_clean()
  })

output$pinforobo<-renderPlot(inforobo())


output$norobot <- renderValueBox({
  valueBox(
    table(dati$robot)[1] , "# aziende che non utilizzano robot di mungitura",
    color = "blue"
  )
})


norob<-reactive({ dati %>% 
    filter(robot=="No") %>% 
    select(29:33) %>% rename('bolo ruminale'=bolo ,
                             'orecchino'= orecc, 'pedometro'= pedom,
                             "altro"=al30) %>% 
    pivot_longer(1:5, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    labs(title="tipologia di sensori su animali (no robot)",
         y="n.aziende", x='')+
    coord_flip()+theme_clean()
  })

output$pnorob<-renderPlot(norob())


anpar<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(35:43) %>% rename('movimento'=mov, "stazione"=staz, "alimentazione"=eat,
                             'ruminazione'= rumina,  
                               'BCS'=Sbcs, 'distress termico'=distress, "altro"=al40) %>% 
    pivot_longer(1:9, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    labs(title="parametri rilevati dai sensori sugli animali",
                                     y="n.aziende")+
    coord_flip()+theme_clean()})

output$panpar<-renderPlot(anpar())

amb<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(45:47) %>% rename('qualità aria'=qair,   "altro"=altro44) %>% 
    pivot_longer(1:3, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    labs(title="sensori-dispositivi ambientali",
                                     y="n.aziende", x='')+
    coord_flip()+theme_clean()})

output$pamb<-renderPlot(amb())


ambpr<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(49:54) %>% rename('temperatura'=temp,   'umidità'=umid, 
                            "CO2"=co2,"Ammoniaca"=nh3,"illuminazione"=luce, "altro"=al51) %>% 
    pivot_longer(1:6, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
    labs(title=" parametri rilevati dai sensori-dispositivi ambientali",
                                     y="n.aziende", x='')+
    coord_flip()+theme_clean()})

output$pambpr<-renderPlot(ambpr())

######SEZIONE 3########################################



output$ptuso<-renderPlot(
  
  dati %>% 
    filter(sensori!="No")%>% 
    group_by(usedtime) %>% 
    summarise(n=n()) %>% 
    arrange(n) %>% 
    mutate(usedtime=factor(usedtime, unique(usedtime))) %>% 
    ggplot(aes(x=usedtime, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
    labs(x="", title="Da quanto tempo usa questa tecnologia", y='n.aziende')+coord_flip()+
    theme_clean()+
    geom_text(aes(y=n, label=n), hjust=2, 
             color="yellow", size=5)

)



output$likert<-renderPlot(
 
dati %>% 
  filter(sensori!="No")%>% 
  select(agevo) %>% 
  mutate(agevo=factor(agevo, levels=c("Non sono d'accordo", "Sono poco d'accordo",
                                       "Sono d'accordo", "Sono molto d'accordo", 
                                       "Sono totalmente d'accordo"))) %>% 
  group_by(agevo) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=agevo, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
  labs(x='', y='n.aziende', title="Questa tecnologia ha agevolato il suo lavoro!")+
  theme_clean()+
  geom_text(aes(y=n, label=n), vjust=2, 
            color="yellow", size=5)
)


miglior<-reactive({ dati %>% 
    filter(sensori!="No") %>% 
    select(58:61) %>% rename('qualità produzione'=qualprod, 'benessere'=welfare, 
                             'economia aziendale'=ecoaz, "altro"=al58) %>% 
    pivot_longer(1:4, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
    scale_fill_brewer(palette="Paired")+
    geom_bar(stat = "identity",width = 0.6)+
    geom_text(position=position_stack(vjust=0.5),color="yellow", size=4.2)+
    labs(title="Cosa è migliorato?",
                                     y="n.aziende", x='')+
    coord_flip()+theme_clean()})

output$pmiglior<-renderPlot(miglior())

output$easy <- renderValueBox({
  valueBox(
    dati %>% 
      filter(sensori!="No") %>% 
      select(easyuse) %>% 
      filter(easyuse=="Sì") %>% 
      summarise(n=n()), "# aziende ritengono i dati raccolti di facile utilizzo",
    color = "red"
  )
})

output$pinnov<-renderPlot(
  
  dati %>% 
    filter(sensori!="No") %>% 
    select(innov) %>%
    mutate( innov=recode(innov, `Avere un riscontro immediato di un cambiamento nel comportamento dell’animale`='riscontro immediato del comportamento',
                              `Usare più sensori integrati contemporaneamente`='usare più sensori',
                         `Avere un riscontro immediato dello stato di benessere degli animali`="riscontro immediato del benessere"
                         )) %>% 
    group_by(innov) %>% 
    summarise(n=n()) %>% 
    arrange(n) %>% 
    mutate(innov=factor(innov, unique(innov))) %>% 
    ggplot(aes(x=innov, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
    labs(x="", title="quale innovazione sarebbe più utile?", y='n.aziende')+coord_flip()+
    theme_clean()+
    geom_text(aes(y=n, label=n), hjust=2, 
              color="yellow", size=5)
  )



}