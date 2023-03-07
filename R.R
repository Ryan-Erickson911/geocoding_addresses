library(dplyr)
library(sf)
library(ggmap)
library(stringr)
library(ggplot2)
library(ggspatial)
library(raster)

#function junction
us_boundary = USAboundaries::us_states(resolution="high") %>% 
  filter(jurisdiction_type=="state"&state_abbr!=c("AK","HI"))
usa_map = function(point,var,polygon=us_boundary,color_1=NA,color_2=NA,title=NA,subtitle=NA,ltitle=NA){
  poly=polygon
  pnt=point
  t=ifelse(is.na(title),"Title",title)
  st=ifelse(is.na(title),"Subtitle",subtitle)
  lt=ifelse(is.na(ltitle),"Legend",ltitle)
  col1=ifelse(is.na(color_1),"black",color_1)
  col2=ifelse(is.na(color_2),pnt$move_in_year,color_2)
  ggplot() +
    annotation_map_tile(type = "hotstyle") +
    geom_sf(data=poly, col = col1 ,fill=NA, alpha = 0, size = 2) +
    geom_sf(data=pnt, aes(col=var)) +
#    scale_colour_viridis_c(paste0(lt)) +
    ggtitle(t, st) +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))
}
nrow(us_df[,move_in_year])
usa_map(us_df,st_drop_geometry(us_df[,"move_in_year"]))
usa_map(co_df, co_boundary)

us_df$move_in_year
sep_geo = function(st1,com1,twn1,state1,ostate1,outus1,zip1,mvnm1) {
  #only get populated survey dates
  t=origAddress %>% 
    dplyr::select(record_id, study_id, visit_datetime, which(colnames(origAddress)==st1):which(colnames(origAddress)==com1)) %>% 
    filter(origAddress[,twn1]!="") 
  #geocoding
  try({t=t %>% 
    mutate(st_abr = ifelse(t[,state1]==1, "CO", t[,ostate1])) %>% 
    mutate(st_abr = ifelse(t[,state1]==3, t[,outus1], .$st_abr)) %>% 
    mutate(full_address = paste0(t[,st1],", ",
                                  t[,twn1],", ",
                                  .$st_abr,", ",
                                  t[,zip1]),
           move_number=mvnm1) %>% 
    mutate_geocode(full_address, output="latlona")
  #fix names for consistancy
  names(t)=c("record_id","study_id","visit_date","street_address","town","zipcode",
             "state","state_outside_co","outus","time_at_address",
             "time_comment","move_in_month","move_in_year","move_out_month",
             "move_out_year","address_comment","st_abr","full_address",
             "times_moved","lon","lat","address")
  #make st object (maybe sp object?)
  t %>% 
    filter(!is.na(t$lon)) %>% 
    st_as_sf(coords = c("lon", "lat"),  crs = "EPSG:4326")
  })
}

#6mo or greater of exposure time 
#create column that resembles hoy many days the participate stayed in CO up to the day the study was taken
api_key = "AIzaSyAeoVaM_45SQ4G-lHeci1RhRNbhpxO3BDc"
register_google(key = api_key) #WILL NEED API KEEEEEEYYYYYYYYYYYYYYYYYYYY

origAddress = read.csv2("geocoding.csv", sep=",") %>% 
  filter(visit_datetime!="") #takeout those without survey day

##seasons are start of month, need monthly code book
cb = as.data.frame(seq(1,28))
colnames(cb) = ("code")
cb = mutate(cb, year=seq(1994,2021))
cb = as.data.frame(seq(1,28))
colnames(cb) = ("code")
cb = mutate(cb, year=seq(1994,2021))
cbmo = as.data.frame(seq(1,16))
colnames(cbmo) = ("code")
mo = c(seq(1,12),12,3,6,9)
cbmo = mutate(cbmo, month=mo)

#make lists for loops
movein_mo=grep("movedin_mo",names(origAddress),value=T)
moveout_mo=grep("movedout_mo",names(origAddress),value=T)
movein_yr=grep("movedin_yr",names(origAddress),value=T)
moveout_yr=grep("movedout_yr",names(origAddress),value=T)
st=grep("^address_st\\d",names(origAddress),value=T)
com=grep("address_comment",names(origAddress),value=T)
twn=grep("address_town",names(origAddress),value=T)
state=c(grep("address_state\\d_v",names(origAddress),value=T),
        grep("address_state\\d\\d_v",names(origAddress),value=T))
ostate=c(grep("address_state\\d_ot",names(origAddress),value=T),
         grep("address_state\\d\\d_ot",names(origAddress),value=T))
outus=grep("_outus_",names(origAddress),value=T)
zip=grep("address_zip",names(origAddress),value=T)
mvnm = seq(1:19)
f=as.Date(unlist(strsplit(origAddress$visit_date,"\\s")), format = "%Y-%m-%d")
f=f[!is.na(f)]
fi=(strsplit(as.character(f),"-"))

for(x in 1:79){print(fi[[x]][[2]])}

#formatting dates
for(x in 1:nrow(origAddress)) {
  for(z in movein_mo) {
    ifelse(origAddress[x,z] %in% cbmo$code, 
           (origAddress[x,z]=cbmo[origAddress[x,z],"month"]), 
           origAddress[x,z])
  }
  for(z in moveout_mo) {
    ifelse(origAddress[x,z] %in% cbmo$code, 
           (origAddress[x,z]=cbmo[origAddress[x,z],"month"]), 
           origAddress[x,z])
    ifelse(origAddress[x,z]==0, 
           (origAddress[x,z]=fi[[x]][[2]]), 
           origAddress[x,z])
  }
  for(z in movein_yr) {
    ifelse(origAddress[x,z] %in% cb$code, 
           (origAddress[x,z]=cb[origAddress[x,z],"year"]), 
           origAddress[x,z])
  }
  for(z in moveout_yr) {
    ifelse(origAddress[x,z] %in% cb$code, 
           (origAddress[x,z]=cb[origAddress[x,z],"year"]), 
           origAddress[x,z])
    ifelse(origAddress[x,z]==0, 
           (origAddress[x,z]=fi[[x]][[1]]), 
           origAddress[x,z])
  }  
}

full_df=sep_geo(st[1],com[1],twn[1],state[1],ostate[1],outus[1],zip[1], mvnm[1])
st_crs(full_df)
for(i in 2:19) {
  output = sep_geo(st[i],com[i],twn[i],state[i],ostate[i],outus[i],zip[i], mvnm[i])
  full_df = rbind(full_df,output)
  }  #318 obs - do something about Thorton record

##mapping
#####world map
ggplot(full_df, aes(color=move_in_year))+
  annotation_map_tile() +
  annotation_scale() +
  geom_sf() +
  scale_colour_viridis_c()+
  ggtitle("Overall Distribution of unique addresses", "Need to filter down to people in CO for atleast 6mo since survey")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

####Addresses only in CO (+showing all in US)
us_boundary = USAboundaries::us_states(resolution="high") %>% 
  filter(jurisdiction_type=="state"&state_abbr!=c("AK","HI")) %>% 
  st_transform(st_crs(full_df))
us_un = st_union(us_boundary)
us_elev = elevatr::get_elev_raster(us_un, z=1) %>% 
  raster::
  raster::projectExtent(4326)
crs(us_df)
st_crs((us_boundary))

elev_clip = crop(us_elev,us_boundary)
elev = mask(elev_clip,us_boundary)
e_slp = terrain(elev, "slope")
e_asp = terrain(elev,"aspect")
hillshade = hillShade(e_slp,e_asp)
plot(hillshade)
plot(elev, alpha=0.5, add=T)
plot(us_un, add=T)

us_df = full_df %>% 
  st_filter(.,us_boundary, .predicate = st_within)

ggplot() +
  annotation_map_tile(type = "hotstyle") +
  geom_sf(data=us_boundary, col = "black",fill=NA, alpha = 0, size = 2) +
  geom_sf(data=us_df, aes(color=move_in_year)) +
  gghighlight::gghighlight(us_df$state==1) +
  scale_colour_viridis_c("Year Moved") +
  ggtitle("Overall Distribution of unique addresses", paste0("Addresses in CO: ",sum(us_df$state==1,na.rm=T))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

#Only CO
co_boundary = USAboundaries::us_counties(states="Colorado",resolution="high") %>% 
  st_transform(st_crs(full_df))

co_df = full_df %>% 
  st_filter(.,co_boundary, .predicate = st_within) 

ggplot() +
 # annotation_map_tile(type = "hotstyle") +
  geom_raster(data = hilldf_single,
              aes(x, y, fill = hillshade),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  new_scale_fill() +
  geom_raster(data = mdtdf,
              aes(x, y, fill = alt),
              alpha = .7) +
  scale_fill_hypso_tint_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  geom_sf(data = ust,
          colour = "black", fill = NA) +
  coord_sf() +
  ggtitle("Colorado", paste0("Qualified Addresses")) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) 

#format(datesp, format = "%Y-%m-%d") #convert to mo yr format

write.csv2(file="final_product.csv")
test = read.csv2("final_product.csv", sep=";")
buffered = st_buffer(origAddress, 1)