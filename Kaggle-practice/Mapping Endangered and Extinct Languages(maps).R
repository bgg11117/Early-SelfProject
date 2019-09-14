library(ggplot2)
library(maps)
library(scales)
library(readr)

languages <- read_csv("./input/data.csv")

not_extinct <- languages[languages$`Degree of endangerment` != "Extinct", ]
extinct <- languages[languages$`Degree of endangerment` == "Extinct", ]

getmyMap <- function() {
  ## Getting the world map
  world_map <- map_data("world")
  ## creating a blank plot
  p <- ggplot() + coord_fixed() + xlab("") + ylab("")
  #Add map to base plot
  base_world_messy <- p + 
    geom_polygon(data=world_map, aes(x=long, y=lat, group=group), colour="black", fill="white")
  ## Cleaning up the plot
  cleanup <- theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   panel.background = element_rect(fill = 'white', colour = 'white'), 
                   axis.line = element_blank(),
                   axis.ticks= element_blank(), 
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank())
  ## Final plot to map the points
  base_world <- base_world_messy + cleanup
  return(base_world)
}


### Map of Extinct Languages based on Degree of Endangerment
getmyMap() +
  geom_point(data=languages, 
             aes(x=Longitude, y=Latitude, colour=`Degree of endangerment`), size = 2) +
  ggtitle("Map of Extinct Languages based on Degree of Endangerment")


getmyMap() +
  geom_point(data=extinct, 
             aes(x=Longitude, y=Latitude), size = 3, color = "red") +
  theme(legend.position = "right")  +
  scale_size_continuous(labels=comma) + 
  scale_color_continuous(labels=comma) + 
  ggtitle("Map of All Languages which have become Extinct")

getmyMap() +
  geom_point(data=not_extinct, 
             aes(x=Longitude, y=Latitude, colour=`Number of speakers`, size = `Number of speakers`)) +
  theme(legend.position = "right")  +
  scale_size_continuous(labels=comma) + 
  scale_color_continuous(labels=comma, low =" blue",  high = "darkgreen") + 
  ggtitle("Map of All Languages which are in Danger of becoming Extinct")

languages$Extinct <-  ifelse(languages$`Degree of endangerment` == "Extinct", c("Extinct"), c("Endangered"))

getmyMap() +
  geom_point(data=languages, 
             aes(x=Longitude, y=Latitude, colour='Extinct'), size = 3) +
  theme(legend.position = "right") + 
  ggtitle("Map Extinct and Endangered")