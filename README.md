# Group2Project
ISDS


world <- map_data("world")
ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") + 
                 coord_fixed(1.3) + 
                 guides(fill = F)
                 
