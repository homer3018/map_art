library(osmdata)
library(dplyr)
library(ggplot2)
library(sf)
library(lwgeom)

bbx <- getbb("Orleans France")

min_lon <- 1.8; max_lon <- 2.03
min_lat <- 47.81; max_lat <- 47.98
bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min", "max")

bbx <- getbb("brisbane australia")

min_lon <- 152.7; max_lon <- 153.5
min_lat <- -27.7; max_lat <- -27
bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min", "max")

highways <- bbx %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk",
                          "primary", "secondary",
                          "tertiary", "motorway_link",
                          "trunk_link", "primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = highways$osm_lines,
          aes(color = highway),
          size = .4,
          alpha = .65) +
  theme_void()

streets <- bbx %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "service", "unclassified",
                            "pedestrian", "footway",
                            "track", "path")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          aes(color = highway),
          size = .4,
          alpha = .65) +
  theme_void()

color_roads <- rgb(0.42, 0.449, 0.488)
ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8) +
  coord_sf(xlim = c(min_lon, max_lon),
           ylim = c(min_lat, max_lat),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()

river <- bbx %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

lake <- bbx %>%
  opq() %>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

railway <- bbx %>%
  opq() %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

coast_data <- bbx %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

water <- bbx %>% 
  opq() %>% 
  add_osm_feature(key = 'natural', value = 'water') %>% 
  osmdata_sf()

JD <- highways[["osm_lines"]] %>% 
  filter(name == "Rue John Deere")

JD <- highways[["osm_lines"]] %>% 
  filter(name %in% c("Avenue Gaston Galloux", "Avenue Gaston Galloux - Pont René Thinat"))

home_coord <- c(1.927538, 47.828039)
JD_coord <- c(1.915899, 47.951552)

library(osrm)
pers_route <- osrmRoute(src = c("A", home_coord),
                        dst = c("B", JD_coord),
                        returnclass = "sf",
                        overview = "full")

ggplot() +
  # theme_void() +
  geom_sf(data = sea,
          inherit.aes = FALSE,
          fill = "steelblue",
          color = "steelblue",
          size = .8,
          alpha = .7) +
  geom_sf(data = coast_data$osm_polygons,
          inherit.aes = FALSE,
          color = "black",
          size = .8,
          alpha = 0.8) +
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
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "steelblue",
          color = "steelblue",
          size = .4,
          alpha = .7) +
  # geom_sf(data = water_bodies,
  #         inherit.aes = FALSE,
  #         fill = "steelblue",
  #         size = .4,
  #         alpha = .7) +
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
          linetype = "dotdash",
          alpha = .5) +
  # geom_sf(data = JD,
  #         inherit.aes = FALSE,
  #         color = "orange",
  #         size = 1,
  #         alpha = 1) +
  # geom_sf(data = st_geometry(pers_route),
  #         inherit.aes = FALSE,
  #         col = "red",
  #         size = 1,
  #         alpha = 0.5) +
  coord_sf(xlim = c(min_lon, max_lon),
           ylim = c(min_lat, max_lat),
           expand = FALSE)

ggsave(last_plot(), 
       filename = "Map_art.pdf",
       scale = 1, 
       width = 36, 
       height = 24, 
       units = "in",
       dpi = 500)



blade <- coast_data$osm_lines %>% st_union %>% st_line_merge
blade %>% plot()

p_bbx <- rbind(c(min_lon, min_lat),
               c(max_lon, min_lat),
               c(max_lon, max_lat),
               c(min_lon, max_lat),
               c(min_lon, min_lat))
# Putting the coordinates into a squared polygon object
pol <- st_polygon(list(p_bbx)) %>% st_geometry
st_crs(pol) <- 4326
pol %>% plot()

multipol <- st_split(st_geometry(pol), st_geometry(blade))
land <- st_cast(multipol[[1]][[1]], 'POLYGON') %>% st_geometry %>% st_sf
st_crs(land) <- 4326
land %>% plot()
sea <- st_difference(pol, land) %>% st_geometry %>% st_sf
st_crs(sea) <- 4326
sea %>% plot()

blade <- water$osm_lines %>% st_union %>% st_line_merge
blade %>% plot()
multipol <- st_split(st_geometry(pol), st_geometry(blade))

1:(multipol[[1]] %>% length()) %>% 
  purrr::walk(function(x) {
    df <- st_cast(multipol[[1]][[x]], 'POLYGON') %>% st_geometry() %>% st_sf()
    # st_crs(df) <- 4326
    
    g <- ggplot() +
      geom_sf(data = df, fill = "red", alpha = 0.5) +
      coord_sf(xlim = c(min_lon, max_lon),
               ylim = c(min_lat, max_lat),
               expand = FALSE)
    print(g)
  })

sea <- st_cast(multipol[[1]][[3]], 'POLYGON') %>% st_union(st_cast(multipol[[1]][[2]], 'POLYGON')) %>% st_geometry %>% st_sf
st_crs(sea) <- 4326
sea %>% plot()

tmp <- st_cast(multipol[[1]][[1]], 'POLYGON') %>% st_geometry() %>% st_sf()
st_crs(tmp) <- 4326

water_bodies <- st_difference(pol, tmp) %>% st_geometry %>% st_sf
st_crs(sea) <- 4326


