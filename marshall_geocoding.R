library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(ggspatial)

#function junction
marshall_map = function(sf_point_object, field, title, Stitle, Ltitle) {
  ggplot(sf_point_object) + 
    annotation_map_tile() +
    annotation_scale() +
    geom_sf(aes(color=sf_point_object[[field]])) + 
    scale_color_viridis_d(option = "B")+
    ggtitle(title, subtitle = Stitle)  + 
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) + 
    guides(colour=guide_legend(title=Ltitle)) 
}

#geocoding - assiging lats and longs via Google API - will need key
geo_marshall = read.csv2("marshall_w1.csv", sep=",") %>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") 
#NA lat and long - one weird one
what_this = geo_marshall[is.na(geo_marshall$age),]
 
#mapping - only for points in CO
co_co = st_read("/Users/RErickson/Documents/GitHub/air-quality-kriging/co_counties/geo_export_ecf55ab8-2f88-4a1f-b4fb-b21f58fb9432.shp")

# Select only Denver metro counties and geometry - might be able to narrow down to CDP if wanted
den_co = co_co %>%
  filter(county %in% c("BOULDER")) %>%
  select(county, geometry) %>% 
  st_as_sf(crs=4326)

#raw survey data
marshall_gc = geo_marshall %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326) %>% 
  st_filter(co_bound, .predicate=st_within)

#cleaned data (no NA in impact_cat)
marshall_gc_clean = marshall_gc %>%
  filter(impact_cat != "") %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326) %>% 
  st_filter(co_bound, .predicate=st_within)

#template map - raw data, boulder county limits included
ggplot(den_co) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(fill=NA) +
#  geom_sf_text(aes(label=county))+ for mapping multiple counties...maybe
  geom_sf(data=marshall_gc, aes(color=impact_cat)) #+ lims(y = c(39.9, 40.1), x = c(-105.35, -105)) 

ggplot(marshall_gc) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=impact_cat)) + 
  scale_color_viridis_d(option = "B")+
  ggtitle("Marshall Fire Survey Results", subtitle = "Zoomed") + 
  guides(colour=guide_legend(title="Impact Level")) #+ gghighlight::gghighlight("")

##Final Map##
ggplot(marshall_gc_clean) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=impact_cat)) + #everything after this line is just making the map noice
  scale_color_viridis_d(option = "B")+
  ggtitle("Marshall Fire Survey Results", subtitle = "Zoomed") + 
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) + 
  guides(colour=guide_legend(title="Impact Level")) #+ gghighlight::gghighlight(impact_cat == "Complete loss")

#function testing
marshall_map(marshall_gc_clean, "impact_cat", title = "Marshall Fire Survey Results", Ltitle = "Impact Level", Stitle = "Zoomed")