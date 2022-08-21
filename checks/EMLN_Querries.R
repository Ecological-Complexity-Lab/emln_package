##using the folder 




# Visualize Network Type --------------------------------------------------

### Counts the number of networks that are either Spatial, Temporal etc... ###
 
 #First I create a list that contains all the network types 
counted_network_types <- count(network_type, value, name = 'count')

#calculate the proportion of the layer types out of all the networks
counted_network_types <- counted_network_types %>% 
  arrange(desc(value)) %>%
  mutate(prop = count / sum(counted_network_types$count) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
  mutate(labels = percent(prop/100))

ggplot(counted_network_types, aes(x="",y=prop)) +
  geom_col(aes(fill = value), width = 1, color = NA) + 
  paper_figs_theme + labs(x = "", y = "Proportion of Network Type") +
  coord_polar(theta = "y") +
  scale_fill_brewer(name = "Network Type:") + theme(axis.ticks = element_blank(),
                                                   panel.grid = element_blank())
#without lab theme
ggplot(counted_network_types, aes(x="",y=prop)) +
  geom_col(aes(fill = value), width = 1, color = NA) + coord_polar(theta = "y") +
  labs(x = "", y = "Proportion of Network Type") + scale_fill_discrete(name = "Network Type:") 


#with lab theme
ggplot(counted_network_types, aes(x="",y=prop)) +
  geom_col(aes(fill = value), width = 1, color = NA) + coord_polar(theta = "y") +
  labs(x = "", y = "Proportion of Network Type") +
  scale_fill_discrete(name = "Network Type:") + theme_void() + geom_text(aes(y = ypos, label = labels), color = "black", size=14) + theme(
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )


ggsave("~/GitHub/emln_db/EMLN_Datasets/metadata/pie_network_types.pdf",width = 15,height =15) 
dev.off()

# Visualize Layers --------------------------------------------------------

#types of layers
ggplot(counted_types, aes(x="",y=prop, fill=layer_type)) +
  geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start = 0) +
  paper_figs_theme + theme_void() #+ geom_text(aes(y = ypos, label = prop), color = "white", size=6)

ggsave("~/GitHub/emln_db/EMLN_Datasets/metadata/pie_layer_types.pdf",width = 15,height =15) 
dev.off()

#no. of layers per paper
num_of_layers<-tibble(id=1:81,num=num_of_layers)
num_layers_plot <- ggplot(num_of_layers, aes(x=id, y=num, label = num))    
num_layers_plot + geom_line() + geom_point() + 
  scale_y_continuous(breaks=number_ticks(10)) + 
  paper_figs_theme + xlab('ID') + ylab('No. of Layers') +
  ggtitle('No. of Layers per Network')

ggsave("~/GitHub/emln_db/EMLN_Datasets/metadata/num_layers_plot.pdf",width = 20,height =10) 
dev.off()

# Visualize Nodes ---------------------------------------------------------
ggplot(as.data.frame(num_of_nodes), aes(x=num_of_nodes)) + geom_histogram(binwidth=50,fill="#54b7a8") + scale_x_continuous(breaks=number_ticks(30)) +
  theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
ggsave("~/GitHub/emln_db/EMLN_Datasets/metadata/num_nodes_plot.pdf",width = 20,height =10) 
dev.off()


num_layers_plot <- ggplot(num_of_nodes, aes(x=id, y=num, label = num))    
num_layers_plot + geom_line() + geom_point() + 
  scale_y_continuous(breaks=number_ticks(10)) + 
  paper_figs_theme + xlab('ID') + ylab('No. of Layers') +
  ggtitle('No. of Layers per Network')


# Visualize Sources used ----------------------------------------------------



# Map Visualization -------------------------------------------------------
latitudes<-NULL
longitudes<-NULL
for(i in 1:78){
  if (i!=56){
    latitudes<-rbind(latitudes,all_layers[[i]] %>% filter(attribute=="latitude"))
    longitudes<-rbind(longitudes,all_layers[[i]] %>% filter(attribute=="longitude")) 
  }
}



s <- strsplit(latitudes$value, split = ",")
latitudes<-data.frame(attribute = rep(latitudes$attribute, sapply(s, length)), value = unlist(s))
s <- strsplit(longitudes$value, split = ",")
longitudes<-data.frame(attribute = rep(longitudes$attribute, sapply(s, length)), value = unlist(s))

coorrdinates<-tibble(latitude=latitudes$value,longitude=longitudes$value)
coorrdinates<-coorrdinates %>% na.omit() %>% distinct(latitude,longitude)

worldmap <- map_data ("world")
ggplot(worldmap) +
  geom_map(data = worldmap, map = worldmap, aes(map_id=region), col = "white", fill = "lightgray") +
  expand_limits(x = worldmap$long, y = worldmap$lat) +
  geom_point(data=coorrdinates,aes(x=as.numeric(longitude),y=as.numeric(latitude),col="data_location"))
ggsave("~/GitHub/emln_db/EMLN_Datasets/metadata/worldmap_plot_new.pdf",width = 25,height =15) 
dev.off()

