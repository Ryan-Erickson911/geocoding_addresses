library(dplyr)
library(sf)
library(ggmap)
library(stringr)

#function junction
sep_geo = function(st1,com1,twn1,state1,ostate1,outus1,zip1,mvnm1) {
  t=origAddress %>% 
    select(record_id, study_id, visit_datetime, which(colnames(origAddress)==st1):which(colnames(origAddress)==com1)) %>% 
    filter(origAddress[,twn1]!="") 
  t %>% 
    mutate(st_abr = ifelse(t[,state1]==1, "CO", t[,ostate1])) %>% 
    mutate(st_abr = ifelse(t[,state1]==3, t[,outus1], .$st_abr)) %>% 
    mutate(full_address = paste0(t[,st1],", ",
                                  t[,twn1],", ",
                                  .$st_abr1,", ",
                                  t[,zip1]),
           move_number=mvnm1) %>% 
    mutate_geocode(full_address, output="latlona")
  names(t)=c("record_id","study_id","visit_date","address_st1_v2","address_town1_v2","address_zip1_v2",
             "address_state1_v2","address_state1_other_v2","address_state1_outus_v2","address_timecat_v2",
             "address_timecat_other_v2","movedin_mocat1_v2","movedin_yrcat1_v2","movedout_mocat1_v2",
             "movedout_yrcat1_v2","address_comment1_v2","st_abr","full_address",
             "move_number","lon","lat","address")
  t %>% 
    filter(!is.na(t$lon)) %>% 
    st_as_sf(coords = c("lon", "lat"),  crs = 4326)
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
loop1 = c("movedin_mocat1_v2","movedin_mocat2_v2","movedin_mocat3_v2","movedin_mocat4_v2","movedin_mocat5_v2","movedin_mocat6_v2","movedin_mocat7_v2","movedin_mocat8_v2","movedin_mocat9_v2","movedin_mocat10_v2","movedin_mocat11_v2","movedin_mocat12_v2","movedin_mocat13_v2","movedin_mocat14_v2","movedin_mocat15_v2","movedin_mocat16_v2","movedin_mocat17_v2","movedin_mocat18_v2","movedin_mocat19_v2")
loop2 = c("movedout_mocat1_v2","movedout_mocat2_v2","movedout_mocat3_v2","movedout_mocat4_v2","movedout_mocat5_v2","movedout_mocat6_v2","movedout_mocat7_v2","movedout_mocat8_v2","movedout_mocat9_v2","movedout_mocat10_v2","movedout_mocat11_v2","movedout_mocat12_v2","movedout_mocat13_v2","movedout_mocat14_v2","movedout_mocat15_v2","movedout_mocat16_v2","movedout_mocat17_v2","movedout_mocat18_v2","movedout_mocat19_v2")
loop3 = c("movedin_yrcat1_v2","movedin_yrcat2_v2","movedin_yrcat3_v2","movedin_yrcat4_v2","movedin_yrcat5_v2","movedin_yrcat6_v2","movedin_yrcat7_v2","movedin_yrcat8_v2","movedin_yrcat9_v2","movedin_yrcat10_v2","movedin_yrcat11_v2","movedin_yrcat12_v2","movedin_yrcat13_v2","movedin_yrcat14_v2","movedin_yrcat15_v2","movedin_yrcat16_v2","movedin_yrcat17_v2","movedin_yrcat18_v2","movedin_yrcat19_v2")
loop4 = c("movedout_yrcat1_v2","movedout_yrcat2_v2","movedout_yrcat3_v2","movedout_yrcat4_v2","movedout_yrcat5_v2","movedout_yrcat6_v2","movedout_yrcat7_v2","movedout_yrcat8_v2","movedout_yrcat9_v2","movedout_yrcat10_v2","movedout_yrcat11_v2","movedout_yrcat12_v2","movedout_yrcat13_v2","movedout_yrcat14_v2","movedout_yrcat15_v2","movedout_yrcat16_v2","movedout_yrcat17_v2","movedout_yrcat18_v2","movedout_yrcat19_v2")

for(i in 1:nrow(origAddress)) {
  for(z in loop1) {
    ifelse(origAddress[i,z] %in% cbmo$code, 
           (origAddress[i,z]=cbmo[origAddress[i,z],"month"]), 
           origAddress[i,z])
    }
  for(z in loop2) {
    ifelse(origAddress[i,z] %in% cbmo$code, 
           (origAddress[i,z]=cbmo[origAddress[i,z],"month"]), 
           origAddress[i,z])
    }
    
  for(z in loop3) {
    ifelse(origAddress[i,z] %in% cb$code, 
           (origAddress[i,z]=cb[origAddress[i,z],"year"]), 
           origAddress[i,z])
    }
  
  for(z in loop4) {
    ifelse(origAddress[i,z] %in% cb$code, 
           (origAddress[i,z]=cb[origAddress[i,z],"year"]), 
           origAddress[i,z])
    }
}

st = c("address_st1_v2","address_st2_v2","address_st3_v2","address_st4_v2","address_st5_v2","address_st6_v2","address_st7_v2","address_st8_v2","address_st9_v2","address_st10_v2","address_st11_v2","address_st12_v2","address_st13_v2","address_st14_v2","address_st15_v2","address_st16_v2","address_st17_v2","address_st18_v2","address_st19_v2")
com = c("address_comment1_v2","address_comment2_v2","address_comment3_v2","address_comment4_v2","address_comment5_v2","address_comment6_v2","address_comment7_v2","address_comment8_v2","address_comment9_v2","address_comment10_v2","address_comment11_v2","address_comment12_v2","address_comment13_v2","address_comment14_v2","address_comment15_v2","address_comment16_v2","address_comment17_v2","address_comment18_v2","address_comment19_v2")
twn = c("address_town1_v2","address_town2_v2","address_town3_v2","address_town4_v2","address_town5_v2","address_town6_v2","address_town7_v2","address_town8_v2","address_town9_v2","address_town10_v2","address_town11_v2","address_town12_v2","address_town13_v2","address_town14_v2","address_town15_v2","address_town16_v2","address_town17_v2","address_town18_v2","address_town19_v2")
state = c("address_state1_v2","address_state2_v2","address_state3_v2","address_state4_v2","address_state5_v2","address_state6_v2","address_state7_v2","address_state8_v2","address_state9_v2","address_state10_v2","address_state11_v2","address_state12_v2","address_state13_v2","address_state14_v2","address_state15_v2","address_state16_v2","address_state17_v2","address_state18_v2","address_state19_v2")
ostate = c("address_state1_other_v2","address_state2_other_v2","address_state3_other_v2","address_state4_other_v2","address_state5_other_v2","address_state6_other_v2","address_state7_other_v2","address_state8_other_v2","address_state9_other_v2","address_state10_other_v2","address_state11_other_v2","address_state12_other_v2","address_state13_other_v2","address_state14_other_v2","address_state15_other_v2","address_state16_other_v2","address_state17_other_v2","address_state18_other_v2","address_state19_other_v2")
outus = c("address_state1_outus_v2","address_state2_outus_v2","address_state3_outus_v2","address_state4_outus_v2","address_state5_outus_v2","address_state6_outus_v2","address_state7_outus_v2","address_state8_outus_v2","address_state9_outus_v2","address_state10_outus_v2","address_state11_outus_v2","address_state12_outus_v2","address_state13_outus_v2","address_state14_outus_v2","address_state15_outus_v2","address_state16_outus_v2","address_state17_outus_v2","address_state18_outus_v2","address_state19_outus_v2")
zip = c("address_zip1_v2","address_zip2_v2","address_zip3_v2","address_zip4_v2","address_zip5_v2","address_zip6_v2","address_zip7_v2","address_zip8_v2","address_zip9_v2","address_zip10_v2","address_zip11_v2","address_zip12_v2","address_zip13_v2","address_zip14_v2","address_zip15_v2","address_zip16_v2","address_zip17_v2","address_zip18_v2","address_zip19_v2")
mvnm = seq(1:19)
#origAddress[origAddress$address_town2_v2=="Thornton",]$address_town2_v2=

full_df = rbind(sep_geo(st[1],com[1],twn[1],state[1],ostate[1],outus[1],zip[1], mvnm[1]),
                sep_geo(st[2],com[2],twn[2],state[2],ostate[2],outus[2],zip[2], mvnm[2]))

test1=sep_geo(st[1],com[1],twn[1],state[1],ostate[1],outus[1],zip[1], mvnm[1])
test2=sep_geo(st[2],com[2],twn[2],state[2],ostate[2],outus[2],zip[2], mvnm[2])
full_df = rbind(test1,test2)
sur_dates = format(datesp, format = "%Y-%m-%d") #convert to mo yr format

write.csv2(file="final_product.csv")
test = read.csv2("final_product.csv", sep=";")
buffered = st_buffer(origAddress, 1)
#ifelse(.$address_state1_v2==3,NA,paste0(.$address_st1_v2,", ",.$address_town1_v2,", ", .$st_abr,", ",.$address_zip1_v2))
ifelse(origAddress$address_state1_v2==3,NA,paste0(origAddress$address_st1_v2,", ",origAddress$address_town1_v2,", ", origAddress$st_abr,", ",origAddress$address_zip1_v2)) 

# Select the file from the file chooser
######me thinks this will be useful to make an interactive function
#fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
#origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)

# Initialize the data frame
#geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon

for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}

# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)