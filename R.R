library(dplyr)
library(sf)
library(ggmap)
library(stringr)


#6mo or greaeter of exposure time 
#remember julian dates
#create column that resembles hoy maay days the participate stayed in CO up to the day the study was taken
api_key = "AIzaSyAeoVaM_45SQ4G-lHeci1RhRNbhpxO3BDc"
register_google(key = api_key) #WILL NEED API KEEEEEEYYYYYYYYYYYYYYYYYYYY

origAddress = read.csv2("geocoding.csv", sep=",") %>% 
  filter(visit_datetime!="") #takeout those without survey day

###time cleaning
datesp = as.Date(origAddress$visit_datetime) #create date object
in_mo = origAddress$movedin_mocat1_v2
times_moved = as.data.frame(in_mo) %>% 
  mutate(in_yr = origAddress$movedin_yrcat1_v2, 
         out_mo = origAddress$movedout_mocat1_v2, 
         out_yr = origAddress$movedout_yrcat1_v2)
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

#lol
move_1 = origAddress %>% 
  select(record_id, study_id, visit_datetime, which(colnames(.)=="address_st1_v2"):which(colnames(.)=="address_comment1_v2")) %>% 
  filter(.[,"address_town1_v2"]!="") %>% 
  mutate(st_abr1 = ifelse(.[,"address_state1_v2"]==1, "CO", .[,"address_state1_other_v2"])) %>% 
  mutate(full_address1 = paste0(.[,"address_st1_v2"],", ",
                                .[,"address_town1_v2"],", ",
                                .[,"st_abr1"],", ",
                                .[,"address_zip1_v2"])) %>% 
   mutate_geocode(full_address1, output="latlona") %>% 
   st_as_sf(coords = c("lon", "lat"),  crs = 4326)


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