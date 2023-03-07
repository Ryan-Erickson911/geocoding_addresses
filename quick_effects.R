# install the packages if necessary

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("elevatr")) install.packages("elevatr")
if(!require("terra")) install.packages("terra")
if(!require("whitebox")) install.packages("whitebox")
if(!require("tidyterra")) install.packages("tidyterra")
if(!require("giscoR")) install.packages("giscoR")
if(!require("ggnewscale")) install.packages("ggnewscale")

# packages
library(sf)
library(elevatr)
library(tidyverse)
library(terra)
library(whitebox)
library(ggnewscale)
library(tidyterra)
library(giscoR)
library(units)

#ust <- gisco_get_countries(country = "Contiguous United States", resolution = "03")
ust = USAboundaries::us_counties(states= "Colorado", resolution="high") %>% 
  st_transform(crs="EPSG:4269")
st_crs(ust)
plot(ust$geometry)
ust_un = st_union(ust)
plot(ust_un)
mdt <- get_elev_raster(ust, z = 7)
plot(mdt)
mdt <- rast(mdt) %>% 
  mask(vect(ust)) 
# reproject
mdt <- project(mdt, crs(ust))
# reproject vect
#suiz <- st_transform(suiz, st_crs(suiz_lakes))
# convert the raster into a data.frame of xyz
mdtdf <- as.data.frame(mdt, xy = TRUE)
names(mdtdf)[3] <- "alt"

# map
ggplot() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt)) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")

sl <- terrain(mdt, "slope", unit = "radians")
plot(sl)
asp <- terrain(mdt, "aspect", unit = "radians")
plot(asp)
hill_single <- shade(sl, asp, 
                     angle = 45, 
                     direction = 300,
                     normalize= TRUE)

# final hillshade 
plot(hill_single, col = grey(1:100/100))
# convert the hillshade to xyz
hilldf_single <- as.data.frame(hill_single, xy = TRUE)

# map 
ggplot() +
  geom_raster(data = hilldf_single,
              aes(x, y, fill = hillshade),
              show.legend = FALSE,
              alpha=1) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt),
              alpha = 0.7) +
  scale_fill_distiller(palette = "Spectral") +
  geom_sf(data = ust,
          colour = "black", fill = NA) +
  guides(fill = guide_colorsteps(barwidth = 20,
                                 barheight = .5,
                                 title.position = "right")) +
  labs(fill = "m") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")

test=origAddress %>% 
  dplyr::select(record_id, study_id, visit_datetime, which(colnames(origAddress)==st[1]):which(colnames(origAddress)==com[1])) %>% 
  filter(origAddress[,twn[1]]!="") 
#geocoding
test=test %>% 
  mutate(st_abr = ifelse(test[,state[1]]==1, "CO", test[,ostate[1]])) %>% 
  mutate(st_abr = ifelse(test[,state[1]]==3, test[,outus[1]], .$st_abr)) %>% 
  mutate(full_address = paste0(test[,st[1]],", ",
                               test[,twn[1]],", ",
                               .$st_abr,", ",
                               test[,zip[1]]),
         move_number=mvnm[1]) %>% 
  mutate_geocode(full_address, output="latlona")
st_crs(test)