library(dplyr)
library(sf)
library(ggmap)
library(stringr)
library(ggplot2)
library(ggspatial)

#function junction
sep_geo = function(st1,com1,twn1,state1,ostate1,outus1,zip1,mvnm1) {
  #only get populated survey dates
  t=origAddress %>% 
    select(record_id, study_id, visit_datetime, which(colnames(origAddress)==st1):which(colnames(origAddress)==com1)) %>% 
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
    st_as_sf(coords = c("lon", "lat"),  crs = 4326)
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

#formatting dates
for(i in 1:nrow(origAddress)) {
  for(z in movein_mo) {
    ifelse(origAddress[i,z] %in% cbmo$code, 
           (origAddress[i,z]=cbmo[origAddress[i,z],"month"]), 
           origAddress[i,z])
    }
  for(z in moveout_mo) {
    ifelse(origAddress[i,z] %in% cbmo$code, 
           (origAddress[i,z]=cbmo[origAddress[i,z],"month"]), 
           origAddress[i,z])
    }
  for(z in movein_yr) {
    ifelse(origAddress[i,z] %in% cb$code, 
           (origAddress[i,z]=cb[origAddress[i,z],"year"]), 
           origAddress[i,z])
    }
  for(z in moveout_yr) {
    ifelse(origAddress[i,z] %in% cb$code, 
           (origAddress[i,z]=cb[origAddress[i,z],"year"]), 
           origAddress[i,z])
    }
}

# fix = c("address_town2_v2","address_town3_v2","address_town4_v2","address_town5_v2","address_town6_v2","address_town7_v2","address_town8_v2","address_town9_v2","address_town10_v2","address_town11_v2","address_town12_v2","address_town13_v2","address_town14_v2","address_town15_v2","address_town16_v2","address_town17_v2","address_town18_v2")
# fix2 = c("address_st2_v2","address_st3_v2","address_st4_v2","address_st5_v2","address_st6_v2","address_st7_v2","address_st8_v2","address_st9_v2","address_st10_v2","address_st11_v2","address_st12_v2","address_st13_v2","address_st14_v2","address_st15_v2","address_st16_v2","address_st17_v2","address_st18_v2")
# origAddress[74,fix]=NA
# origAddress[74,fix2]=NA
# look=origAddress[74,]

full_df = rbind(sep_geo(st[1],com[1],twn[1],state[1],ostate[1],outus[1],zip[1], mvnm[1]),
                sep_geo(st[2],com[2],twn[2],state[2],ostate[2],outus[2],zip[2], mvnm[2]),
                sep_geo(st[3],com[3],twn[3],state[3],ostate[3],outus[3],zip[3], mvnm[3]),
                sep_geo(st[4],com[4],twn[4],state[4],ostate[4],outus[4],zip[4], mvnm[4]),
                sep_geo(st[5],com[5],twn[5],state[5],ostate[5],outus[5],zip[5], mvnm[5]),
                sep_geo(st[6],com[6],twn[6],state[6],ostate[6],outus[6],zip[6], mvnm[6]),
                sep_geo(st[7],com[7],twn[7],state[7],ostate[7],outus[7],zip[7], mvnm[7]),
                sep_geo(st[8],com[8],twn[8],state[8],ostate[8],outus[8],zip[8], mvnm[8]),
                sep_geo(st[9],com[9],twn[9],state[9],ostate[9],outus[9],zip[9], mvnm[9]),
                sep_geo(st[10],com[10],twn[10],state[10],ostate[10],outus[10],zip[10], mvnm[10]),
                sep_geo(st[11],com[11],twn[11],state[11],ostate[11],outus[11],zip[11], mvnm[11]),
#                sep_geo(st[12],com[12],twn[12],state[12],ostate[12],outus[12],zip[12], mvnm[12]),
#                sep_geo(st[13],com[13],twn[13],state[13],ostate[13],outus[13],zip[13], mvnm[13]),
#                sep_geo(st[14],com[14],twn[14],state[14],ostate[14],outus[14],zip[14], mvnm[14]),
#                sep_geo(st[15],com[15],twn[15],state[15],ostate[15],outus[15],zip[15], mvnm[15]),
#                sep_geo(st[16],com[16],twn[16],state[16],ostate[16],outus[16],zip[16], mvnm[16]),
#                sep_geo(st[17],com[17],twn[17],state[17],ostate[17],outus[17],zip[17], mvnm[17]),
#                sep_geo(st[18],com[18],twn[18],state[18],ostate[18],outus[18],zip[18], mvnm[18]),
                sep_geo(st[19],com[19],twn[19],state[19],ostate[19],outus[19],zip[19], mvnm[19]))

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

us_df = full_df %>% 
  st_filter(.,us_boundary, .predicate = st_within)

ggplot() +
  annotation_map_tile(type = "hotstyle") +
  geom_sf(data=us_boundary, col = "black",fill=NA, alpha = 0, size = 2) +
  geom_sf(data=us_df, aes(color=move_in_year)) +
  gghighlight::gghighlight(us_df$state==1) +
  scale_colour_viridis_c("D") +
  ggtitle("Overall Distribution of unique addresses", paste0("Addresses in CO: ",sum(us_df$state==1,na.rm=T))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

#Only CO
co_boundary = USAboundaries::us_counties(states="Colorado",resolution="high") %>% 
  st_transform(st_crs(full_df))

co_df = full_df %>% 
  st_filter(.,co_boundary, .predicate = st_within)

ggplot() +
  annotation_map_tile(type = "hotstyle") +
  geom_sf(data=co_boundary, col = "black",fill=NA, alpha = 0, size = 2) +
  geom_sf(data=co_df, aes(color=move_in_year)) +
#  gghighlight::gghighlight(us_df$state==1) +
  scale_colour_viridis_c("D") +
  ggtitle("Colorado Addresses", paste0("Qualified Addresses",sum(us_df$state==1,na.rm=T))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

sur_dates = format(datesp, format = "%Y-%m-%d") #convert to mo yr format

write.csv2(file="final_product.csv")
test = read.csv2("final_product.csv", sep=";")
buffered = st_buffer(origAddress, 1)
