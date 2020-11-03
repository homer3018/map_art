library(osmdata)
bbx <- getbb("Orleans France")

min_lon <- 1.84; max_lon <- 1.99
min_lat <- 47.81; max_lat <- 47.93
bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min","max")

bbx <- getbb("brisbane australia")

min_lon <- 152.7; max_lon <- 153.5
min_lat <- -27.7; max_lat <- -27
bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min","max")

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

library(ggplot2)
library(sf)
ggplot() +
  geom_sf(data = highways$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)+
  theme_void()

streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)+
  theme_void()

color_roads <- rgb(0.42,0.449,0.488)
ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8)+
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()

river <- bbx%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

lake <- bbx%>%
  opq()%>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

railway <- bbx%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

coast_data <- bbx %>%
  opq() %>%
  add_osm_feature (key = "natural", value = "coastline") %>%
  osmdata_sf()

coast <- osmplotr::osm_line2poly(coast_data$osm_lines, bbox = bbx)

osm_basemap (bbox = bbox) %>%
  add_osm_objects(coast$sea, col = 'cadetblue2') %>%
  add_osm_objects(coast$land, col = 'sienna2')

ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8) +
  geom_sf(data = coast_data$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .8,
          alpha = 0.8) +
  geom_sf(data = coast_data$osm_polygons,
          inherit.aes = FALSE,
          color = "red",
          size = .8,
          alpha = 0.3) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .7) +
  geom_sf(data = lake$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .7) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE) +
  theme_void()