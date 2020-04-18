
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
      median(dati$lattazione), "# Capi in lattazione (consistenza mediana)",
      color = "blue"
    )
  })
  
  output$dry <- renderValueBox({
    valueBox(
      median(dati$asciutte), "# Capi in asciutta (consistenza mediana)",
      color = "blue"
    )
  })
  
  output$heif <- renderValueBox({
    valueBox(
      round(median(dati$nmanze),0), "# Manze (consistenza mediana)",
      color = "blue"
    )
  })
  
  
reg<-reactive({
    
    dati %>% 
      group_by(reg) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(reg=factor(reg, unique(reg))) %>% 
      ggplot(aes(x=reg, y=n))+geom_bar(stat="identity", fill="steelblue3")+
      labs(x="", title="Regione")+coord_flip()+ theme(axis.text=element_text(size=12))
    
    
  })
  
  ruol<-reactive({
    
    dati %>% 
      group_by(ruolo) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(ruolo=factor(ruolo, unique(ruolo))) %>% 
      ggplot(aes(x=ruolo, y=n))+geom_bar(stat="identity", fill="steelblue3")+
      labs(x="", title="Ruolo in azienda")+coord_flip()+ theme(axis.text=element_text(size=12))
    
  })
  
  
  tipol<-reactive({
    
    dati %>% 
      group_by(tipo) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(tipo=factor(tipo, unique(tipo))) %>% 
      ggplot(aes(x=tipo, y=n))+geom_bar(stat="identity", fill="steelblue3")+
      labs(x="", title="Tipo allevamento")+coord_flip()+ theme(axis.text=element_text(size=12))
    
  })
  
  nonint<-reactive({
    
    dati %>% 
      filter(interesse=="No") %>% 
      group_by(noint) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(nonint=factor(noint, unique(noint))) %>% 
      ggplot(aes(x=noint, y=n))+geom_bar(stat="identity", fill="steelblue3")+
      labs(x="", title="Motivi di mancato interesse all'introduzione di sensori")+coord_flip()+ theme(axis.text=element_text(size=12))
    
  })
  
  
  output$gr1<-renderPlot(
    
    (reg()/ruol())|(tipol()/nonint())

  )
  
  
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
      table(dati$sensori)[2] , "# aziende che utilizzano sensori o altri dispositivi",
      color = "red"
    )
  })
  #<-interesse
  output$inter <- renderValueBox({
    valueBox(
      table(dati$interesse)[2] , "# aziende interessate all'uso di sensori",
      color = "red"
    )
  })
  
 #########SEZIONE 2################################

  output$morsens <- renderValueBox({
    valueBox(
      table(dati$moresens)[2] , "# aziende che utilizzano più sensori",
      color = "red"
    )
  })
  
  

q12<-reactive({ dati %>% 
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
  coord_flip()})

output$quest12<-renderPlot(q12())


output$robott <- renderValueBox({
  valueBox(
    table(dati$robot)[2] , "# aziende che utilizzano robot di mungitura",
    color = "blue"
  )
})

q14<-reactive({ dati %>% 
    filter(robot=="Sì") %>% 
    select(17:22) %>% rename('produzione latte'=prodmilk,'composizione latte'=compomilk ,
                             'conducibilità'= condumilk, 'BCS'= bcs,
                             "parametri sanitari"=healthpam, "altro"=al14) %>% 
    pivot_longer(1:6, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    #filter(risp=="Si") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title="tipologia di informazioni raccolte dal robot di mungitura",
                                     y="n.aziende",x='')+
    coord_flip()})

output$quest14<-renderPlot(q14())


output$norobot <- renderValueBox({
  valueBox(
    table(dati$robot)[1] , "# aziende che non utilizzano robot di mungitura",
    color = "blue"
  )
})


q16<-reactive({ dati %>% 
    filter(robot=="No") %>% 
    select(24:28) %>% rename('bolo ruminale'=bolo ,
                             'orecchino'= orecc, 'podometro'= podom,
                             "altro"=al16) %>% 
    pivot_longer(1:5, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    # filter(risp=="SI") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title="tipologia di sensori applicati agli animali",
                                     y="n.aziende", x='')+
    coord_flip()})

output$quest16<-renderPlot(q16())


q18<-reactive({ dati %>% 
    filter(sensori=="Sì") %>% 
    select(30:40) %>% rename('movimento'=mov, "stazione"=staz, "alimentazione"=eat,
                             'ruminazione'= rumina, 'accesso robot mung'= robomung,
                             'zoppia'=zoppo, 'BCS'=Sbcs, 'distress termico'=distress, "altro"=al18) %>% 
    pivot_longer(1:9, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    # filter(risp=="SI") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title="parametri rilevati dai sensori sugli animali",
                                     y="n.aziende")+
    coord_flip()})

output$quest18<-renderPlot(q18())

q20<-reactive({ dati %>% 
    filter(sensori=="Sì") %>% 
    select(42:44) %>% rename('qualità aria'=qair,   "altro"=altro20) %>% 
    pivot_longer(1:3, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    # filter(risp=="SI") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title="sensori-dispositivi ambientali",
                                     y="n.aziende", x='')+
    coord_flip()})

output$quest20<-renderPlot(q20())


q22<-reactive({ dati %>% 
    filter(sensori=="Sì") %>% 
    select(46:50) %>% rename('temperatura'=temp,   'umidità'=umid, 
                            'polveri'=polv, "altro"=al22) %>% 
    pivot_longer(1:3, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    # filter(risp=="SI") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title=" parametri rilevati dai sensori-dispositivi ambientali",
                                     y="n.aziende", x='')+
    coord_flip()})

output$quest22<-renderPlot(q22())

######SEZIONE 3########################################

output$quest24<-renderPlot(
  
  dati %>% 
    filter(sensori=="Sì")%>% 
    group_by(usedtime) %>% 
    summarise(n=n()) %>% 
    arrange(n) %>% 
    mutate(usedtime=factor(usedtime, unique(usedtime))) %>% 
    ggplot(aes(x=usedtime, y=n))+geom_bar(stat="identity", fill="steelblue3")+
    labs(x="", title="tempo utilizzo della tecnologia", y='n.aziende')+coord_flip()

)

output$likert<-renderPlot(
 
dati %>% 
  filter(sensori=="Sì")%>% 
  select(agevo) %>% 
  mutate(agevo=factor(agevo)) %>% 
  group_by(agevo) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=agevo, y=n))+geom_bar(stat="identity", fill="steelblue3")+
  labs(x='', y='n.aziende', title="questa tecnologia ha agevolato il suo lavoro")
)


q26<-reactive({ dati %>% 
    filter(sensori=="Sì") %>% 
    select(54:57) %>% rename('qualità produzione'=qualprod, 'benessere'=welfare, 
                             'economia aziendale'=ecoaz, "altro"=al26) %>% 
    pivot_longer(1:3, names_to = "categoria", values_to = "risposta") %>% 
    group_by(categoria, risposta) %>% 
    summarise(n=n()) %>% 
    # filter(risp=="SI") %>% 
    ungroup() %>% 
    arrange(n) %>% 
    mutate(categoria=factor(categoria, unique(categoria))) %>% 
    ggplot(aes(x=categoria, y=n, fill=risposta))+
    scale_fill_manual(values = c("steelblue", "blue"))+
    geom_bar(stat = "identity")+labs(title="Cosa è migliorato?",
                                     y="n.aziende", x='')+
    coord_flip()})

output$quest26<-renderPlot(q26())


output$quest29<-renderPlot(
  
  dati %>% 
    filter(sensori=="Sì")%>% 
    select(innov) %>%
    mutate( innov=recode(innov, `Avere un riscontro immediato di un cambiamento nel comportamento dell’animale`='riscontro immediato del comportamento',
                              `Usare più sensori integrati contemporaneamente`='usare più sensori',
                         `Avere un riscontro immediato dello stato di benessere degli animali`="riscontro immediato del benessere"
                         )) %>% 
    group_by(innov) %>% 
    summarise(n=n()) %>% 
    arrange(n) %>% 
    mutate(innov=factor(innov, unique(innov))) %>% 
    ggplot(aes(x=innov, y=n))+geom_bar(stat="identity", fill="steelblue3")+
    labs(x="", title="quale innovazione sarebbe più utile?", y='n.aziende')+coord_flip()
  
)




}