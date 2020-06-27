dati %>% 
  #filter(sensori!="No") %>% 
  select(25:28) %>% rename('bov da latte'=sabovlat2, 'bov in asciutta'=sabovasc2, 
                           'manze'=samanze2, 'vitelle'=savit2) %>% 
  pivot_longer(1:4, names_to = "categoria", values_to = "risposta") %>% 
  group_by(categoria, risposta) %>% 
  summarise(n=n()) %>% 
  drop_na() %>% 
  mutate(risposta=factor(risposta)) %>% 
  ggplot(aes(x=categoria, y=n, fill=risposta, label=n))+
  scale_fill_brewer(palette="Paired")+
  geom_bar(stat = "identity",width = 0.6)+
  geom_text(position=position_stack(vjust=0.5),color="yellow", size=5)+
  coord_flip()+theme_clean()+
  labs(title="su quali categorie di animali sono applicati i sensori?",
       y="n.aziende", x='')


 