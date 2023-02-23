library(dplyr)
library(sf)
library(ggmap)


#6mo or greaeter of exposure time 
#remember julian dates
#create column that resembles hoy maay days the participate stayed in CO up to the day the study was taken
api_key = "AIzaSyAeoVaM_45SQ4G-lHeci1RhRNbhpxO3BDc"
register_google(key = api_key) #WILL NEED API KEEEEEEYYYYYYYYYYYYYYYYYYYY


origAddress = read.csv2("geocoding.csv", sep=",") %>% 
  mutate(st_abr1 = ifelse(.$address_state1_v2==1, "CO", .$address_state1_other_v2),
         full_address1 = paste0(.$address_st1_v2,", ",.$address_town1_v2,", ", .$st_abr,", ",.$address_zip1_v2)) %>% 
  #filter(!is.na(full_address1)) %>% #temp until fixed
  mutate_geocode(full_address1, output="latlona") %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326)

geo_marshall = read.csv2("marshall_w1.csv", sep=",") %>% 
  mutate(full_address = paste0(.$mailingaddr1,", ",.$mailingcity,", ", .$mailingstate,", ",.$mailingzip)) %>% 
  mutate_geocode(full_address, output="latlona") %>% 
  st_as_sf(coords = c("lon", "lat"),  crs = 4326)


function_name <- function(df) {
  df %>% 
    mutate(st_abr1 = ifelse(.$address_state1_v2==1, "CO", .$address_state1_other_v2),
           full_address1 = paste0(.$address_st1_v2,", ",.$address_town1_v2,", ", .$st_abr,", ",.$address_zip1_v2)) %>% 
    #filter(!is.na(full_address1)) %>% #temp until fixed
    mutate_geocode(full_address1, output="latlona") %>% 
    st_as_sf(coords = c("lon", "lat"),  crs = 4326)
}

function_name(origAddress)


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