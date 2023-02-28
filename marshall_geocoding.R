library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(ggspatial)

#function junction
change_display = function(category) {
  ggplot(geo_marshall) + 
    annotation_map_tile() +
    annotation_scale() +
    geom_sf(aes(color=impact_cat)) + 
    scale_color_viridis_d(option = "B")+
    ggtitle("Marshall Fire Survey Results", subtitle = paste0("Total Respondants Displayed: ",nrow(geo_marshall))) + 
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5)) +
    guides(colour=guide_legend(title="Impact Level")) + 
    gghighlight::gghighlight(impact_cat==category)
}
###need to connect to UCB research drive
#Win - R:/Marshall Fire Health/
#mac - /Volumes/research/Marshall Fire Health/
m_addresses<-read.csv("marshall_w1.csv", sep=",")

##addys to fix
#553 GRANT ST == 553 GRANT AVE
m_addresses[m_addresses$mailingaddr1=="553 GRANT ST",]$mailingaddr1 = "553 GRANT AVE"
 
#geocoding - assiging lats and longs via Google API - will need key
geo_marshall = m_addresses %>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") %>%
  filter(!is.na(lat)) %>% 
  filter(impact_cat != "") %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326) %>%
  st_filter(den_co, .predicate=st_within)
#NA lat and long - one weird one
#what_this = geo_marshall[is.na(geo_marshall$age),]

#mapping - only for points in CO
co_co = st_read("C:/Users/RErickson/Documents/GitHub/geocoding_addresses/aqi_kriging-main/co_counties/geo_export_ecf55ab8-2f88-4a1f-b4fb-b21f58fb9432.shp")

# Select only Denver metro counties and geometry
den_co = co_co %>%
  filter(county %in% c("BOULDER")) %>%
  select(county, geometry) %>% 
  st_transform(crs=st_crs(geo_marshall))  

#template map - raw data, boulder county limits included, zoomed out
ggplot(den_co) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(fill=NA) +
#  geom_sf_text(aes(label=county))+ for mapping multiple counties...maybe
  geom_sf(data=geo_marshall, aes(color=impact_cat))

#Final Map
ggplot(geo_marshall) + 
  annotation_map_tile() +
  annotation_scale() +
  geom_sf(aes(color=impact_cat)) +
  scale_color_manual(values = c("Complete loss" = "#d7191c", 
                                "Damaged, living there" = "#fdae61",
                                "Damaged, not living there" = "#ffffbf",
                                "No damage, living there" = "#a6d96a",
                                "No damage, not living there" = "#1a9641")) +
  ggtitle("Marshall Fire Survey Results", subtitle=paste0("Total Respondants Displayed: ",nrow(geo_marshall))) + 
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  guides(colour=guide_legend(title="Impact Level"))
#for easy highlighting
func = unique(geo_marshall$impact_cat) #get specific damage types (codebook)
#1 = "Damaged, living there"   
#2 = "Complete loss"         
#3 = "Damaged, not living there"
#4 = "No damage, living there"  
#5 = "No damage, not living there"
change_display(func[1])
