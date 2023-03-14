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
#nrow(us_df[,move_in_year])
#usa_map(us_df,st_drop_geometry(us_df[,"move_in_year"]))
#usa_map(co_df, co_boundary)

#us_df$move_in_year

sep_geo = function(df, st1,com1,twn1,state1,ostate1,outus1,zip1,mvnm1) {
  #only get populated survey dates
  t=df %>% 
    dplyr::select(record_id, study_id, visit_datetime, which(colnames(df)==st1):which(colnames(df)==com1)) %>% 
    filter(df[,twn1]!="") 
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

format.dates = function(df,in_mo,in_yr,out_mo,out_yr) {
  f=as.Date(unlist(strsplit(df$visit_date,"\\s")), format = "%Y-%m-%d")
  f=f[!is.na(f)]
  fi=(strsplit(as.character(f),"-"))
  ##seasons are first day of month
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
  for(x in 1:nrow(df)) {
    for(z in in_mo) {
      ifelse(df[x,z] %in% cbmo$code, 
             (df[x,z]=cbmo[df[x,z],"month"]), 
             df[x,z])
    }
    for(z in out_mo) {
      ifelse(df[x,z] %in% cbmo$code, 
             (df[x,z]=cbmo[df[x,z],"month"]), 
             df[x,z])
      ifelse(df[x,z]==0, 
             (df[x,z]=fi[[x]][[2]]), 
             df[x,z])
    }
    for(z in in_yr) {
      ifelse(df[x,z] %in% cb$code, 
             (df[x,z]=cb[df[x,z],"year"]), 
             df[x,z])
    }
    for(z in out_yr) {
      ifelse(df[x,z] %in% cb$code, 
             (df[x,z]=cb[df[x,z],"year"]), 
             df[x,z])
      ifelse(df[x,z]==0, 
             (df[x,z]=fi[[x]][[1]]), 
             df[x,z])
    }  
  }
}

#6mo or greater of exposure time 
#create column that resembles hoy many days the participate stayed in CO up to the day the study was taken
api_key = "AIzaSyAeoVaM_45SQ4G-lHeci1RhRNbhpxO3BDc"
register_google(key = api_key) #WILL NEED API KEEEEEEYYYYYYYYYYYYYYYYYYYY

origAddress = read.csv2("geocoding.csv", sep=",") %>% 
  filter(visit_datetime!="") #takeout those without survey day

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

format.dates(origAddress,movein_mo,moveout_mo,movein_yr,moveout_yr)

full_df=sep_geo(origAddress,st[1],com[1],twn[1],state[1],ostate[1],outus[1],zip[1], mvnm[1])
for(i in 2:19) {
  output = sep_geo(origAddress,st[i],com[i],twn[i],state[i],ostate[i],outus[i],zip[i], mvnm[i])
  full_df = rbind(full_df,output)
  }  #318 obs - do something about Thorton record

add_dates=read.csv2("addthese.csv",sep=",") %>% 
  filter(visitdate!="") %>% 
  dplyr::select(study_id_checklist,visitdate)
add=read.csv2("addthese.csv",sep=",") %>% 
  filter(study_id_resid_hist!="") %>% 
  dplyr::select(-c(redcap_event_name, residential_history_questionnaire_complete, data_entered_by,visitdate)) %>% 
  left_join(add_dates, by="study_id_checklist") %>% 
  mutate(data_checked_by=visitdate) %>% 
  dplyr::select(study_id_resid_hist,study_id_checklist,visitdate,everything(),-data_checked_by)
names(add)[1:3]=c("record_id","study_id","visit_datetime")

amovein_mo=grep("movedin_mo",names(add),value=T)
amoveout_mo=grep("movedout_mo",names(add),value=T)
amovein_yr=grep("movedin_yr",names(add),value=T)
amoveout_yr=grep("movedout_yr",names(add),value=T)
ast=grep("^address_st\\d",names(add),value=T)
acom=grep("address_comment",names(add),value=T)
atwn=grep("address_town",names(add),value=T)
astate=c(grep("address_state\\d$",names(add),value=T),
        grep("address_state\\d\\d$",names(add),value=T))
aostate=c(grep("address_state\\d_ot",names(add),value=T),
         grep("address_state\\d\\d_ot",names(add),value=T))
aoutus=grep("_outus_",names(add),value=T)
azip=grep("address_zip",names(add),value=T)
amvnm=1:19

format.dates(add,amovein_mo,amoveout_mo,amovein_yr,amoveout_yr)

add_df=sep_geo(add,ast[1],acom[1],atwn[1],astate[1],aostate[1],aoutus[1],azip[1],amvnm[1])
for(i in 2:19) {
  output = sep_geo(add,ast[i],acom[i],atwn[i],astate[i],aostate[i],aoutus[i],azip[i],amvnm[i])
  add_df = rbind(add_df,output)
} 

full_df=rbind(full_df,add_df)
full_df$move_in_month=as.integer(full_df$move_in_month)
full_df$move_out_month=as.integer(full_df$move_out_month)
full_df$move_in_year=as.integer(full_df$move_in_year)
full_df$move_out_year=as.integer(full_df$move_out_year)

co_pnts = full_df %>%  
  filter(state==1,
         nchar(move_in_year)==4) %>% 
         #,
         #!is.na(zipcode)) %>% 
  mutate(yr_diff=move_out_year-move_in_year,
         mo_diff=(move_out_month-move_in_month)+(12*yr_diff)) %>% 
  filter(yr_diff<=2&yr_diff>=0,
         mo_diff>=6) %>% 
  dplyr::select(record_id,study_id,visit_date,mo_diff,yr_diff,everything())
co_300m_buffer = st_buffer(co_pnts,dist=300)
co_1000m_buffer = st_buffer(co_pnts,dist=1000)
co_2000m_buffer = st_buffer(co_pnts,dist=2000)
#map cause cool
ggplot()+
  geom_sf(data=co_counties, aes(col=awater))+
  geom_sf(data=co_2000m_buffer,aes(color=study_id))+
  geom_sf(data=co_1000m_buffer,aes(color=study_id))+
  geom_sf(data=co_300m_buffer,aes(color=study_id))+
  geom_sf(data=co_pnts,pch=3,cex=1)

PM2.5_exposure_co = co_300m_buffer %>% 
  filter(move_in_year==2020) %>% 
  mutate(move_in = paste0(move_in_month,"-",move_in_year),
         move_out = paste0(move_out_month,"-",move_out_year)) %>%
  mutate(move_in,as.Date(move_in, format = "%Y-%m"),
         move_out,as.Date(move_out, format = "%Y-%m")) %>% 
  dplyr::select(study_id,move_in,move_out,geometry) 
d1=as.Date(paste0("202001","01"), "%Y%m%d")
d2=as.Date(paste0("202212","31"), "%Y%m%d")

months <- format(seq(d1,d2,by="month"), "%b.%Y")
# Add multiple empty columns
PM2.5_exposure_co[ ,months] <- NA


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#############mapping
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
#  annotation_map_tile(type = "hotstyle") +
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
  geom_sf(data=co_boundary) +
  geom_sf(data = co_df,
          colour = "black", fill = NA) +
  coord_sf() +
  ggtitle("Colorado", paste0("Qualified Addresses")) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) 

#format(datesp, format = "%Y-%m-%d") #convert to mo yr format

write.csv2(file="final_product.csv")
test = read.csv2("final_product.csv", sep=";")
buffered = st_buffer(origAddress, 1)