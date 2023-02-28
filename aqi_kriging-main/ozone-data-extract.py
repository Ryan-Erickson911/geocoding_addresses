import requests
import json
import pandas as pd
import numpy as np
import datetime
import geopandas as gpd
from decimal import Decimal


# Most Commented code is Emma's code from .ipynb files
# FROM EMMA'S REPO:
    
# air-quality-kriging
# kriging air quality data (PM2.5 and Ozone) as part of a larger Denver-area study
# **How to use this repository:**
# * please ignore the "SLC_AQ" folder for now, I need to rehome it in another repository*
# **Step 1:** Use AQ/code > AQ_cleaning > ozone_data_extract.ipynb to extract ozone data using EPA API (you will need to create your own key for this step)
# **Step 2:** Use AQ/code > AQ_cleaning > oz_processing.ipynb to clean ozone data
# **Step 3:** Use AQ/code > AQ_cleaning > kriging > spatial_krige.Rmd to krige data for each month.<- GOAL from 2018-2021

# Step 3 utilizes a shapefile for CO county boundaries (**found in co_counties folder**) as the spatial extent. I used the autokrige package to fit variograms and interpolate the surface.

# Loading data from EPA API (https://aqs.epa.gov/aqsweb/documents/data_api.html#signup)
# AQS codes: https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.html
# ozone code = 44201
# CO FIPS = 08
# Boulder FIPS = 013
# request a key - https://aqs.epa.gov/data/api/signup?email=myemail@example.com
# new key may need to be requested even if you already have one

# var_defs = "https://aqs.epa.gov/data/api/metaData/fieldsByService?email=ryer7052@colorado.edu&key=bluehare13&service=sampleData" #Returns a list and definitions of fields in the Sample Data service:
# oz_17_resp = "https://aqs.epa.gov/data/api/sampleData/byState?email=ryer7052@colorado.edu&key=bluehare13&param=44201&bdate=20170101&edate=20171231&state=08" #returns all ozone samples from Colorado (01/01/2017-12/31/2017):
oz_18_resp = "https://aqs.epa.gov/data/api/sampleData/byState?email=ryer7052@colorado.edu&key=bluehare13&param=44201&bdate=20180101&edate=20181231&state=08" #returns all ozone samples from Colorado (01/01/2018-12/31/2018):
oz_19_resp = "https://aqs.epa.gov/data/api/sampleData/byState?email=ryer7052@colorado.edu&key=bluehare13&param=44201&bdate=20190101&edate=20191231&state=08" #returns all ozone samples from Colorado (01/01/2019-12/31/2019):
oz_20_resp = "https://aqs.epa.gov/data/api/sampleData/byState?email=ryer7052@colorado.edu&key=bluehare13&param=44201&bdate=20200101&edate=20201231&state=08" #returns all ozone samples from Colorado (01/01/2020-12/31/2020):
oz_21_resp = "https://aqs.epa.gov/data/api/sampleData/byState?email=ryer7052@colorado.edu&key=bluehare13&param=44201&bdate=20210101&edate=20211231&state=08" #returns all ozone samples from Colorado (01/01/2021-12/31/2021):

#var_defs_resp = requests.get(var_defs).json()
#epa_oz_17_resp = requests.get(oz_17_resp).json()
epa_oz_18_resp = requests.get(oz_18_resp).json()
epa_oz_19_resp = requests.get(oz_19_resp).json()
epa_oz_20_resp = requests.get(oz_20_resp).json()
epa_oz_21_resp = requests.get(oz_21_resp).json()

# initializing lists to create dataframes for each json request
#oz_17_list = [] #not sure if we use 2017
oz_18_list = []
oz_19_list = []
oz_20_list = []
oz_21_list = []

# function to create dataframes based on json data
def json_to_df(lists,json_data):
    for row in json_data["Data"]:
        lists.append(row)        
    df = pd.DataFrame(data = lists)
    return df

# using function to create lists
#oz_17_df = json_to_df(oz_17_list,epa_oz_17_resp)
#oz_20_df

oz_18_df = json_to_df(oz_18_list,epa_oz_18_resp)
#oz_18_df

oz_19_df = json_to_df(oz_19_list,epa_oz_19_resp)
#oz_19_df
    
oz_20_df = json_to_df(oz_20_list,epa_oz_20_resp)
#oz_20_df

oz_21_df = json_to_df(oz_21_list,epa_oz_21_resp)
#oz_20_df

# join dataframes
#oz_17_21 = pd.concat([oz_17_df,oz_18_df,oz_19_df,oz_20_df,oz_21_df])
oz_18_21 = pd.concat([oz_18_df,oz_19_df,oz_20_df,oz_21_df])

# join date and time columns, turn into datetime object
#cols = ["date_local","time_local"]
#oz_17_21["date_time"] = oz_17_21[cols].apply(lambda x: '-'.join(x.values.astype(str)), axis="columns")
#oz_17_21["date_time"] = pd.to_datetime(oz_17_21["date_time"], infer_datetime_format = True)
cols = ["date_local","time_local"]
oz_18_21["date_time"] = oz_18_21[cols].apply(lambda x: '-'.join(x.values.astype(str)), axis="columns")
oz_18_21["date_time"] = pd.to_datetime(oz_18_21["date_time"], infer_datetime_format = True)

# remove non Denver metro counties
# Denver metro counties: Adams, Arapahoe, Boulder, Broomfield, Denver, Douglas, Jefferson
den_metro_counties = ["Adams","Arapahoe","Boulder","Broomfield","Denver","Douglas","Jefferson"]

#oz_17_21 = oz_17_21[oz_17_21["county"].isin(den_metro_counties)]
oz_18_21 = oz_18_21[oz_18_21["county"].isin(den_metro_counties)]

#oz_17_21.to_csv("hrly_o3_17to21.csv")
#oz_18_21.to_csv("hrly_o3_18to21.csv")

# remove columns that aren't useful for the analysis
#oz_17_21 = oz_17_21.drop(columns=["parameter_code","poc","parameter","date_local","time_local","date_gmt",
#                                  "time_gmt","units_of_measure","units_of_measure_code","sample_duration",
#                                  "sample_duration_code","sample_frequency","detection_limit","method",
#                                  "method_type","method_code","date_of_last_change","cbsa_code"])
oz_18_21 = oz_18_21.drop(columns=["parameter_code","poc","parameter","date_local","time_local","date_gmt",
                                  "time_gmt","units_of_measure","units_of_measure_code","sample_duration",
                                  "sample_duration_code","sample_frequency","detection_limit","method",
                                  "method_type","method_code","date_of_last_change","cbsa_code"])
#np.unique(oz_17_21.site_number)
np.unique(oz_18_21.site_number)

# calculate max 8 hour daily average of ozone
#eight_means = oz_17_21.set_index(["date_time"]).sort_index().groupby("site_number").rolling(window=8, min_periods=6).agg({"sample_measurement": ["mean"]}).reset_index()
eight_means = oz_18_21.set_index(["date_time"]).sort_index().groupby("site_number").rolling(window=8, min_periods=6).agg({"sample_measurement": ["mean"]}).reset_index()

# calculate mda8 for ozone
mda8_frame = eight_means.set_index(["date_time"]).sort_index().groupby("site_number").resample("D").max().rename(columns={"site_number":"sites","mean":"mda8"}).reset_index(level="site_number").drop(columns="sites")

mda8_frame

# calculate daily mean and max
#daily_max = oz_17_21.set_index("date_time").sort_index().groupby("site_number").resample("D").agg({"sample_measurement":["mean","max"]}).rename(columns={"mean":"daily_avg","max":"daily_max"}).reset_index(level="site_number")
daily_max = oz_18_21.set_index("date_time").sort_index().groupby("site_number").resample("D").agg({"sample_measurement":["mean","max"]}).rename(columns={"mean":"daily_avg","max":"daily_max"}).reset_index(level="site_number")

daily_max

# join two dfs together
oz_final = daily_max.merge(mda8_frame, how="left", on=["date_time","site_number"])

#oz.to_csv("hrly_o3_17to21.csv")
#oz_final.to_csv("Documents/Github/aqi_kriging/daily_oz_md8_18to21.csv")
#processing
# download daily files from: https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily
# read in each year as a pandas df
csv_18 = "C:/Users/RErickson/Documents/GitHub/aqi_kriging/data/daily_44201_2018.csv"
csv_19 = "C:/Users/RErickson/Documents/GitHub/aqi_kriging/data/daily_44201_2019.csv"
csv_20 = "C:/Users/RErickson/Documents/GitHub/aqi_kriging/data/daily_44201_2020.csv"
csv_21 = "C:/Users/RErickson/Documents/GitHub/aqi_kriging/data/daily_44201_2021.csv"

oz_pd18 = pd.read_csv(csv_18)
oz_pd19 = pd.read_csv(csv_19)
oz_pd20 = pd.read_csv(csv_20)
oz_pd21 = pd.read_csv(csv_21)

# add in leading zeros

def rep_leading_zero(df):
    
    df["State Code"] = df["State Code"].astype(str)\
    .apply(lambda x:x.zfill(2))
    
    df["County Code"] = df["County Code"].astype(str)\
    .apply(lambda x:x.zfill(3))


    df["Site Num"] = df["Site Num"].astype(str)\
    .apply(lambda x:x.zfill(4))
    
    return df

oz_pd18 = rep_leading_zero(oz_pd18)
oz_pd19 = rep_leading_zero(oz_pd19)
oz_pd20 = rep_leading_zero(oz_pd20)
oz_pd21 = rep_leading_zero(oz_pd21)

# drop all obs for non Denver area sites
# Adams = 001, Arapahoe = 005, Boulder = 013, Broomfield = 014, Denver = 031, Douglas = 035, Jefferson = 059, Weld = 123, EL Paso = 041, Larimer = 069

def drop_nonFOCO(df):
    den_metro_counties = ["001","005","013","014","031","059"]
    df = df[(df["State Code"] == "08") & (df["County Code"].isin(den_metro_counties))]
    return df

oz_pd18 = drop_nonFOCO(oz_pd18)
oz_pd19 = drop_nonFOCO(oz_pd19)
oz_pd20 = drop_nonFOCO(oz_pd20)
oz_pd21 = drop_nonFOCO(oz_pd21)

# Function to check that aspects like parameter, sample duration, events, datum, etc. are what is expected

def check_consistency(df):
    cons_frame = []
    n_param_code = np.unique(df["Parameter Code"])
    n_sites = len(np.unique(df["Site Num"]))
    n_param_name = np.unique(df["Parameter Name"])
    n_sample_dur = np.unique(df["Sample Duration"])
    n_poll_std = np.unique(df["Pollutant Standard"])
    n_units = np.unique(df["Units of Measure"])
    n_event = len(df[df["Event Type"] != "None"])
    n_poc = np.unique(df["POC"])
    n_datum = np.unique(df["Datum"])
    cons_frame.append([n_param_code,n_param_name,n_sites,n_sample_dur,n_poll_std,n_units,n_event,n_poc,n_datum])
    return cons_frame

check_consistency(oz_pd18)

# everything as expected, no events (many more events when adding new counties), two datums (req trans), indicates duplicated dates for a site

test = oz_pd18[oz_pd18["Event Type"] != "None"]
test[["Arithmetic Mean","Event Type"]]
####

check_consistency(oz_pd19)

# everything as expected, no events, two datums (req trans), indicates duplicated dates for a site

test = oz_pd19[oz_pd19["Event Type"] != "None"]
test[["Arithmetic Mean","Event Type"]]
####

check_consistency(oz_pd20)


# everything as expected, no events, two datums (req trans), indicates duplicated dates for a site

test = oz_pd20[oz_pd20["Event Type"] != "None"]
test[["Arithmetic Mean","Event Type"]]
####

check_consistency(oz_pd21)


# everything as expected, no events, two datums (req trans), indicates duplicated dates for a site

test = oz_pd21[oz_pd21["Event Type"] != "None"]
test[["Arithmetic Mean","Event Type"]]
# remove unnecessary columns (includes units, event type because confirmed to be consistent/unimportant earlier)

def drop_noise(df):
    df = df.drop(columns=["Parameter Code", "Parameter Name", "Sample Duration", "Pollutant Standard", 
                          "Observation Count", "1st Max Value", "Units of Measure", "1st Max Hour", "AQI",
                          "Method Code", "Method Name", "Address", "City Name", "CBSA Name", "Date of Last Change",
                          "State Name", "County Name", "Event Type"])
    return df

oz_pd18 = drop_noise(oz_pd18)
oz_pd19 = drop_noise(oz_pd19)
oz_pd20 = drop_noise(oz_pd20)
oz_pd21 = drop_noise(oz_pd21)

# remove observations with less than 80% completeness

def full_obs(df):
    df = df[df["Observation Percent"] > 79]
    return df

oz_pd18 = full_obs(oz_pd18)
oz_pd19 = full_obs(oz_pd19)
oz_pd20 = full_obs(oz_pd20)
oz_pd21 = full_obs(oz_pd21)

# rename columns

def rename_cols(df):
    df = df.rename(columns={"State Code": "state_code", "County Code": "county_code", "Site Num": "site_num",
                            "Latitude":"lat","Longitude":"long","Datum":"datum","Date Local":"date",
                            "Observation Percent":"obs_perc","Arithmetic Mean":"mda8","Local Site Name":"site_name"})
    return df

oz_pd18 = rename_cols(oz_pd18)
oz_pd19 = rename_cols(oz_pd19)
oz_pd20 = rename_cols(oz_pd20)
oz_pd21 = rename_cols(oz_pd21)

# turns df into a geopandas object with geometry,
# transforms NAD83 to WGS84
# standardizes datum

def datum_transform(df):
    gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.long,df.lat))
    gdf = gdf.set_crs("EPSG:4326")
    return gdf
oz_pd18 = datum_transform(oz_pd18)
oz_pd19 = datum_transform(oz_pd19)
oz_pd20 = datum_transform(oz_pd20)
oz_pd21 = datum_transform(oz_pd21)

oz_pd18["UniqueID"] = oz_pd18["state_code"]+"_"+oz_pd18["county_code"]+"_"+oz_pd18["site_num"]
oz_pd19["UniqueID"] = oz_pd19["state_code"]+"_"+oz_pd19["county_code"]+"_"+oz_pd19["site_num"]
oz_pd20["UniqueID"] = oz_pd20["state_code"]+"_"+oz_pd20["county_code"]+"_"+oz_pd20["site_num"]
oz_pd21["UniqueID"] = oz_pd21["state_code"]+"_"+oz_pd21["county_code"]+"_"+oz_pd21["site_num"]

o18 = oz_pd18.drop(columns=["state_code","county_code","site_num","POC","obs_perc","datum"])
o19 = oz_pd19.drop(columns=["state_code","county_code","site_num","POC","obs_perc","datum"])
o20 = oz_pd20.drop(columns=["state_code","county_code","site_num","POC","obs_perc","datum"])
o21 = oz_pd21.drop(columns=["state_code","county_code","site_num","POC","obs_perc","datum"])

o3 = pd.concat([o18,o19,o20,o21],axis=0)
#o3 = o3.set_index("UniqueID")

# create date as a datetime object
o3["date"] = pd.to_datetime(o3["date"])
# group by site
o3_gr = o3.groupby("site_name")


# resample grouped sites for weekly and monthly mean
o3_mthly_avg = o3_gr.resample("M", on="date").mean()
o3_wkly_avg = o3_gr.resample("W",on="date").mean()


# reset index
o3_mthly_avg = o3_mthly_avg.reset_index()
o3_wkly_avg = o3_wkly_avg.reset_index()


#o3_mthly_avg
# correct lat/long rounding error
o3_mthly_avg["lat"] = round(o3_mthly_avg["lat"],8)
o3_mthly_avg["long"] = round(o3_mthly_avg["long"],8)

o3_pivot = o3_mthly_avg.pivot(index=["site_name","lat","long"],
             columns="date",
             values="mda8")
#incomplete: Aspen Park, Welch, Evergreen
o3_pivot.to_csv("Documents/Github/aqi_kriging/monthly_o3_pivot_18to21.csv")
