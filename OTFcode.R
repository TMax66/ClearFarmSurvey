dati %>%
  #filter(sensori!="No") %>%
  select(innov) %>%
  # mutate( innov=recode(innov, `Usare più sensori integrati contemporaneamente` = 'usare più sensori contemp',
  #                      
  #                      `Avere un riscontro immediato di un cambiamento nel comportamento dell’animale`='riscontro immediato del comportamento',
  #                      
  #                      `Avere un riscontro immediato dello stato di benessere degli animali` = 'riscontro immediato del benessere')) %>%
  group_by(innov) %>%
  summarise(n=n()) %>%
  drop_na() %>% 
  arrange(n) %>%
  mutate(innov=factor(innov, unique(innov))) %>%
  ggplot(aes(x=innov, y=n))+geom_bar(stat="identity", fill="steelblue3", width = 0.6)+
  labs(x="", title="quale innovazione sarebbe più utile?", y='n.aziende')+coord_flip()+
  theme_clean()+
  geom_text(aes(y=n, label=n), hjust=2,
            color="yellow", size=5)
 




 