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

saran <- c(1.915899, 47.951552, "Saran", "France")
torreon <- c(-103.35533, 25.47737, "Torreon", "Mexico")
waterloo <- c(-92.43332, 42.46813, "Waterloo", "USA")
rosario <- c(-60.71211, -32.86711, "Rosario", "Argentina")
tianjin <- c(117.72722, 39.07944, "Tianjin", "China")
pune <- c(73.92919, 18.51553, "Pune", "India")
springfield <- c(-93.20083, 37.24398, "Springfield", "USA")
coffeyville <- c(-95.58388, 37.09781, "Coffeyville", "USA")
moline <- c(-90.42295, 41.47645, "Moline", "USA")

choice <- springfield

center <- choice[1:2] %>% as.numeric()
center_country <- choice[4]
loc_lab <- paste0(choice[3], ", ", choice[4])
GPS_lab <- paste0(if_else(center[2] < 0,
                          paste0(-center[2] %>% round(2), "°S"),
                          paste0(center[2] %>% round(2), "°N")),
                  " / ",
                  if_else(center[1] < 0,
                          paste0(-center[1] %>% round(2), "°W"),
                          paste0(center[1] %>% round(2), "°E")))

size_lon <- 0.23
size_lat <- 0.17
buffer <- 0.1
min_lon <- center[1] - size_lon
max_lon <- center[1] + size_lon
min_lat <- center[2] - size_lat
max_lat <- center[2] + size_lat
bbx <- rbind(x = c(min_lon, max_lon),
             y = c(min_lat, max_lat))
bbx_big <- rbind(x = c(min_lon - buffer, max_lon + buffer),
             y = c(min_lat - buffer, max_lat + buffer))
colnames(bbx) <- c("min", "max")
colnames(bbx_big) <- c("min", "max")

bbx <- getbb("brisbane australia")

min_lon <- 152.7; max_lon <- 153.5
min_lat <- -27.7; max_lat <- -27
bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
colnames(bbx) <- c("min", "max")

highways <- bbx_big %>%
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

streets <- bbx_big %>%
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

river <- bbx_big %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

lake <- bbx_big %>%
  opq() %>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

railway <- bbx_big %>%
  opq() %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

coast_data <- bbx_big %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

water <- bbx_big %>%
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
  # LABEL UP
# geom_text(aes(x = center[1],
#               y = center[2] + 0.85 * size_lat),
#           label = "John Deere",
#           size = 18,
#           fontface = "bold",
#           family = "Roboto") +
#   geom_text(aes(x = center[1],
#                 y = center[2] + 0.73 * size_lat),
#             label = loc_lab,
#             size = 12,
#             fontface = "bold",
#             family = "Roboto") +
#   geom_text(aes(x = center[1],
#                 y = center[2] + 0.772 * size_lat),
#             label = paste0("___",
#                            rep(" ", loc_lab %>% stringr::str_length() + 14) %>%
#                              stringr::str_c(collapse = ""),
#                            "___"),
#             size = 12,
#             family = "Roboto") +
#   geom_text(aes(x = center[1],
#                 y = center[2] + 0.64 * size_lat),
#             label = GPS_lab,
#             size = 10,
#             family = "Roboto") +
  # LABEL Down
  geom_text(aes(x = center[1],
                y = center[2] - 0.65 * size_lat),
            label = "John Deere",
            size = 18,
            fontface = "bold",
            family = "Roboto") +
  geom_text(aes(x = center[1],
                y = center[2] - 0.772 * size_lat),
            label = loc_lab,
            size = 12,
            fontface = "bold",
            family = "Roboto") +
  geom_text(aes(x = center[1],
                y = center[2] - 0.73 * size_lat),
            label = paste0("___",
                           rep(" ", loc_lab %>% stringr::str_length() + 14) %>%
                             stringr::str_c(collapse = ""),
                           "___"),
            size = 12,
            family = "Roboto") +
  geom_text(aes(x = center[1],
                y = center[2] - 0.85 * size_lat),
            label = GPS_lab,
            size = 10,
            family = "Roboto") +
  geom_point(aes(x = center[1],
                 y = center[2]),
             shape = 21,
             col = "#367C2B",
             size = 5,
             stroke = 3) +
  geom_point(aes(x = center[1],
                 y = center[2]),
             shape = 21,
             col = "#367C2B",
             size = 15,
             stroke = 3) +
  geom_point(aes(x = center[1],
                 y = center[2]),
             shape = 21,
             col = "#367C2B",
             size = 25,
             stroke = 3) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat),
    expand = FALSE
  ) +
  labs(x = "",
       y = "")

ggsave(last_plot(),
  filename = paste0(choice[3], ".png"),
  # scale = 1,
  width = 17,
  height = 15,
  # units = "cm",
  dpi = 500
)



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
land <- st_cast(multipol[[1]][[1]], "POLYGON") %>%
  st_geometry() %>%
  st_sf()
st_crs(land) <- 4326
land %>% plot()
sea <- st_difference(pol, land) %>%
  st_geometry() %>%
  st_sf()
st_crs(sea) <- 4326
sea %>% plot()

blade <- water$osm_lines %>%
  st_union() %>%
  st_line_merge()
blade %>% plot()
ggplot() +
  geom_sf(data = blade, fill = "red", alpha = 0.5) +
  coord_sf(
    xlim = c(min_lon, max_lon),
    ylim = c(min_lat, max_lat),
    expand = FALSE
  )
  
multipol <- st_split(st_geometry(pol), st_geometry(blade))

st <- streets$osm_lines
hi <- highways$osm_lines
st_crs(st) <- 4326
st_crs(hi) <- 4326

1:(multipol[[1]] %>% length()) %>%
  purrr::walk(function(x) {
    df <- st_cast(multipol[[1]][[x]], "POLYGON") %>%
      st_geometry() %>%
      st_sf()
    st_crs(df) <- 4326

    g <- ggplot() +
      geom_sf(
        data = st,
        col = color_roads,
        size = .4,
        alpha = .65
      ) +
      geom_sf(
        data = hi,
        col = color_roads,
        size = .6,
        alpha = .8
      ) +
      geom_sf(data = df, fill = "red", alpha = 0.5) +
      coord_sf(
        xlim = c(min_lon, max_lon),
        ylim = c(min_lat, max_lat),
        expand = FALSE
      ) +
      labs(title = paste0("X = ", x))
    print(g)
  })

sea <- st_cast(multipol[[1]][[2]], "POLYGON") %>%
  # st_union(st_cast(multipol[[1]][[3]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[4]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[5]], "POLYGON")) %>%
  # st_union(st_cast(multipol[[1]][[6]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[7]], "POLYGON")) %>%
  # st_union(st_cast(multipol[[1]][[8]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[9]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[10]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[11]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[12]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[13]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[14]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[15]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[16]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[17]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[18]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[19]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[20]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[21]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[22]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[23]], "POLYGON")) %>%
  st_geometry() %>%
  st_sf()
st_crs(sea) <- 4326
sea %>% plot()

land <- st_cast(multipol[[1]][[2]], "POLYGON") %>%
  # st_union(st_cast(multipol[[1]][[3]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[4]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[5]], "POLYGON")) %>%
  # st_union(st_cast(multipol[[1]][[6]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[7]], "POLYGON")) %>%
  # st_union(st_cast(multipol[[1]][[8]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[9]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[10]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[11]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[12]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[13]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[14]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[15]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[16]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[17]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[18]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[19]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[20]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[21]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[22]], "POLYGON")) %>%
  st_union(st_cast(multipol[[1]][[23]], "POLYGON")) %>%
  st_geometry() %>%
  st_sf()
st_crs(land) <- 4326
land %>% plot()

tmp <- st_cast(multipol[[1]][[1]], "POLYGON") %>%
  st_geometry() %>%
  st_sf()
st_crs(tmp) <- 4326

water_bodies <- st_difference(pol, tmp) %>%
  st_geometry() %>%
  st_sf()
st_crs(water_bodies) <- 4326
