teste = read.csv("movies.csv")

meanTomato=teste %>% summarize(mean(Tomatometer,na.rm=T)) %>% pull()
teste %>% group_by(year) %>% 
  #filter(year >=2009) %>% 
  summarize(Tomato=mean(Tomatometer,na.rm=T),mean(Audience.Score, na.rm=T),mean(averageRating,na.rm=T),mean(runtimeMinutes,na.rm=T),
            mean(numVotes,na.rm=T),mean(Gross,na.rm=T)) %>% 
  ggplot(aes(x=year, y=Tomato)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept = meanTomato)
meanAudience=teste %>% summarize(mean(Audience.Score,na.rm=T)) %>% pull()
teste %>% group_by(year) %>% 
  #filter(year >=2009) %>% 
  summarize(Tomato=mean(Tomatometer,na.rm=T),Audience=mean(Audience.Score, na.rm=T),mean(averageRating,na.rm=T),mean(runtimeMinutes,na.rm=T),
            mean(numVotes,na.rm=T),mean(Gross,na.rm=T)) %>% 
  ggplot(aes(x=year, y=Audience)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept = meanAudience)
meanRating=teste %>% 
  mutate(big=case_when(year>=2009~1,TRUE~0)) %>% 
  group_by(big) %>% summarize(mean(averageRating,na.rm=T)) %>% pull()
teste %>% group_by(year) %>% 
  #filter(year >=2009) %>% 
  summarize(Tomato=mean(Tomatometer,na.rm=T),Audience=mean(Audience.Score, na.rm=T),rating=mean(averageRating,na.rm=T),mean(runtimeMinutes,na.rm=T),
            mean(numVotes,na.rm=T),mean(Gross,na.rm=T)) %>% 
  mutate(big=case_when(year>=2009~1,TRUE~0)) %>% 
  ggplot(aes(x=year, y=rating,group=big)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept = meanRating)
teste = teste %>% mutate(highlight=case_when(won==1~paste0(year," - ",Release),TRUE~"")) 
teste %>% 
ggplot( aes(x=Audience.Score, y=Tomatometer)) +
  geom_smooth(method = "loess", size = .9)+
  geom_point(color = "gray", alpha = 0.7,size=4)+
  geom_point(data = subset(teste, won==1), aes(x = Audience.Score, y = Tomatometer), colour= "gold", size = 4)+
  theme(
    # Background
    plot.background = element_rect(fill = "#15161E", colour = "#15161E"),
    panel.background = element_rect(fill = "#15161E", colour = "#15161E"),
    # Colour Text
    text = element_text(colour = "lightgray", family = "bodyFont"),
    # Legend 
    legend.position = "none",
    # Title and subtitle
    plot.title.position = "plot"
   
  )+ 
  geom_text_repel(
    aes(label = highlight),
    family = "Poppins",
    size = 3,
    min.segment.length = 0, 
    seed = 42, 
    box.padding = 0.5,
    max.overlaps = Inf,
    #arrow = arrow(length = unit(0.010, "npc")),
    nudge_x = -0.15,
    nudge_y = -0.5,
    color = "gold"
  )
