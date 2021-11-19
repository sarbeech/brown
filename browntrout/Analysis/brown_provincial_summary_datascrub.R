# Stocking data compiler and scrubber - Brown Trout - Province-wide
#---------------------------------------------------------------------------------------------
# combines FSIS data (2001 - present) and older 'non-FSIS' data (mostly from FISHARC)
# this script is re-run each time new FSIS data are downloaded (three files required - fsis_raw.csv; fsis_comments.csv; fsis_tag.csv)
# script categorizes life stages and calculates year class and generally cleans up messy data

# Colin Lake - January 4, 2019
# Last updated - Nov 17, 2021

rm(list=ls())

library(data.table)
library(lubridate)
library(magrittr)
library(readxl)
library(sjmisc)
library(tidyr)
library(readr)
library(usethis)
library(httr)
library(jsonlite)
library(rjson)
library(sp)
library(dplyr)
library(rgdal)

# Step 1: load and join fsis data ----------------------------------------------------------------------------------------------

# fsis data 'df'

# csv that was downloaded from FSIS was already queried for Brown Trout

#conflict_prefer("select", "dplyr")

df1 <- read.csv("Data/Raw/bt_fsis_data.csv", stringsAsFactors = FALSE, skip=1)

# The COMMENTS download from FSIS appears to be messy (throws errors), but using read_delim and omitting junk columns works.
comments <- read_delim("Data/Raw/bt_fsis_comments.csv", skip = 1, delim = ",", escape_double = FALSE)
comments <- select(comments, STOCKING_EVENT_RECORD_NUM, INVENTORY_COMMENTS, STOCKING_COMMENTS,MARKING_COMMENTS)
comments$STOCKING_EVENT_RECORD_NUM <- as.integer(comments$STOCKING_EVENT_RECORD_NUM)
comments <- subset(comments,(STOCKING_EVENT_RECORD_NUM %in% df1$STOCKING_EVENT_RECORD_NUM))

# tags
tags <- read_delim("Data/Raw/bt_fsis_tags.csv", skip = 1, delim = ",", escape_double = FALSE)

tags <- subset(tags,(STOCKING_EVENT_RECORD_NUM %in% df1$STOCKING_EVENT_RECORD_NUM))

# remove columns that have only NA or 0 as values
tags <- Filter(function(x) !all(is.na(x)|x == 0), tags)

# step-wise left-join; make sure there's no funny business here
df1 <- left_join(df1,comments, by = "STOCKING_EVENT_RECORD_NUM")
df1 <- left_join(df1, tags, by = "STOCKING_EVENT_RECORD_NUM")


# Step 2: stocking site coordinates (bane of my existence) ----
# only need to do this for the FSIS data; the older stocking data from GeoHub is already in DD coordinates

# For FSIS data, more straight forward to parse out each UTM zone and handle them independently, then join

coords <- select(df1, "STOCKING_EVENT_RECORD_NUM","STOCKING_SITE_UTM")

coords <- coords %>% separate(STOCKING_SITE_UTM, c("Zone", "Easting", "Northing"))

coords[2:4] <- lapply(coords[2:4], as.numeric)

FSIS15 <- filter(coords, Zone ==15)
FSIS16 <- filter(coords, Zone ==16)
FSIS17 <- filter(coords, Zone ==17)
FSIS18 <- filter(coords, Zone ==18)

utmcoor<-SpatialPoints(cbind(FSIS15$Easting,FSIS15$Northing), proj4string=CRS("+proj=utm +zone=15"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
FSIS15$LONGITUDE <- coordinates(longlatcoor)[,1]
FSIS15$LATITUDE <- coordinates(longlatcoor)[,2]

utmcoor<-SpatialPoints(cbind(FSIS16$Easting,FSIS16$Northing), proj4string=CRS("+proj=utm +zone=16"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
FSIS16$LONGITUDE <- coordinates(longlatcoor)[,1]
FSIS16$LATITUDE <- coordinates(longlatcoor)[,2]

utmcoor<-SpatialPoints(cbind(FSIS17$Easting,FSIS17$Northing), proj4string=CRS("+proj=utm +zone=17"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
FSIS17$LONGITUDE <- coordinates(longlatcoor)[,1]
FSIS17$LATITUDE <- coordinates(longlatcoor)[,2]

utmcoor<-SpatialPoints(cbind(FSIS18$Easting,FSIS18$Northing), proj4string=CRS("+proj=utm +zone=18"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
FSIS18$LONGITUDE <- coordinates(longlatcoor)[,1]
FSIS18$LATITUDE <- coordinates(longlatcoor)[,2]

coords <- rbind(FSIS15, FSIS16, FSIS17, FSIS18)

coords <- coords %>% select(STOCKING_EVENT_RECORD_NUM, LONGITUDE, LATITUDE)

df1 <- left_join(df1, coords)

# move new coordinate variables to follow original UTM value
df1 <- df1 %>% relocate(LATITUDE, LONGITUDE, .after = STOCKING_SITE_UTM)

# clean up
rm(FSIS15)
rm(FSIS16)
rm(FSIS17)
rm(FSIS18)
rm(coords)
rm(longlatcoor)
rm(utmcoor)

# Step 3: historical stocking data from GeoHub ----

# grab GeoJSON data from GeoHub API

path <- "https://opendata.arcgis.com/datasets/1428226f4c0e4a99aebd6ecb415d3e2a_0.geojson"

request <- GET(url = path)

df2 <- jsonlite::fromJSON(rawToChar(request$content))

df2 <- df2$features$properties # old (pre FSIS records; all species)

df2 <- df2 %>% filter(Species == "Brown Trout")

# rename columns so rbind can be done

df2 <- df2%>% select(
  STOCKING_EVENT_RECORD_NUM = Stocking_ID,
  DEVELOPMENT_STAGE_DESCRIPTION = Developmental_Stage,
  DISTRICT_NAME = District,
  FISH_WEIGHT = Mean_Weight,
  REARING_SITE_NAME = Rearing_Location,
  REARING_TEMPERATURE = Rearing_Temperature,
  SITE_TEMPERATURE = Site_Temperature,
  SPECIES_COMMON_NAME = Species,
  STOCKING_YEAR = Stocking_Year,
  SPAWN_YEAR = Spawn_Year,
  FISH_STOCKED_COUNT = Stocked_Fish_Count,
  STOCK_STRAIN_NAME = Stock_Strain,
  STOCKED_WATERBODY_NAME = Stocked_Waterbody_Official_Name,
  STOCKING_SITE_NAME = Stocked_Site_Name,
  DESTINATION_WATERBODY_NAME = Destination_Waterbody_Official_,
  STOCKING_EVENT_DATETIME = Stocking_Date,
  STOCKING_PURPOSE_DESCRIPTION = Stocking_Purpose,
  LATITUDE = Stocked_Waterbody_Latitude,
  LONGITUDE = Stocked_Waterbody_Longitude
  )

df2$STOCKING_YEAR <- as.integer(df2$STOCKING_YEAR)
df2$REARING_TEMPERATURE <- as.numeric(df2$REARING_TEMPERATURE)
df2$REARING_TEMPERATURE <- as.numeric(df2$REARING_TEMPERATURE)
df2$SITE_TEMPERATURE <- as.numeric(df2$SITE_TEMPERATURE)

# fish weight - unknown weights should be NAs, not zeros
df2$FISH_WEIGHT[df2$FISH_WEIGHT == 0] <- NA


# old data have irregularities wrt site names, lack of unique site id values, duplicate coordinates, etc.
# need to determine unique sites and assign unique ids

dfloc <- df2 %>%
  select(
    STOCKED_WATERBODY_NAME,
    STOCKING_SITE_NAME,
    LATITUDE,
    LONGITUDE
  )

dfloc <- unique(dfloc)

dfloc$STOCKING_SITE_ID <- NA

dfloc <- dfloc %>%
  mutate(STOCKING_SITE_ID = if_else(is.na(STOCKING_SITE_ID),
                                    paste0("id_", row_number()),
                                    as.character(STOCKING_SITE_ID)))


# join new unique id back into dataset

df2 <- left_join(df2, dfloc)


# Step 4: combine data --------------------------------------------------------------------------------------------

df1$STOCKING_EVENT_RECORD_NUM <- as.character(df1$STOCKING_EVENT_RECORD_NUM)
df1$STOCKING_SITE_ID <- as.character(df1$STOCKING_SITE_ID)

df <- bind_rows(df1,df2)

# make blanks NA 
df <- df %>%
  mutate(across(where(is.character), ~na_if(., "")))

# remove columns that have only NA or 0 as values
df <- Filter(function(x) !all(is.na(x)|x == 0), df)

# THIS IS IMPORTANT - incorrect dates will screw up determining the life stages (spring vs fall fingerlings, etc.)
df$DATE_orig <- df$STOCKING_EVENT_DATETIME #makes copy of date variable, so 'original' can be checked against parsed values
df$STOCKING_EVENT_DATETIME <- parse_date_time(df$STOCKING_EVENT_DATETIME, orders = c('%m/%d/%Y %H:%M', '%Y-%m-%d %H:%M%:S', '%m/%d%/Y', '%Y-%m-%d'))
df$STOCKING_MONTH <- as.integer(month(df$STOCKING_EVENT_DATE))
df$STOCKING_DAY <- as.integer(day(df$STOCKING_EVENT_DATE))

# reorder dates so the columns are adjacent
df <- select(df, 1:STOCKING_EVENT_DATETIME, DATE_orig, STOCKING_DAY, STOCKING_MONTH, everything())

# Step 5: life stage ------------------------------------------------------------------------------------------------

# Add a new column ("LIFESTAGE") that uses more conventional terms for life stage.
# Leaves original variable ("DEVELOPMENT_STAGE_DESCRIPTION") untouched for reference.
# removes part of string that is in parentheses, copies remaining bit to the new variable
# for example, "Fingerling (3-9 months)" will become simply "Fingerling"

df$LIFESTAGE <- sub("\\(.*", "", df$DEVELOPMENT_STAGE_DESCRIPTION)
df$LIFESTAGE <- trimws(df$LIFESTAGE, which = c("both"), whitespace = "[ \t\r\n]")

# take a look at what we have after first cut
stagechk <- count(df, vars = LIFESTAGE)

df$LIFESTAGE[df$LIFESTAGE == "Fingerlings"] <- "Fingerling"
df$LIFESTAGE[df$LIFESTAGE =="Eyed Eggs"] <- "Egg"
df$LIFESTAGE[df$LIFESTAGE =="Subadult"] <- "Sub-adult"
df$LIFESTAGE[grepl("Juvenile", df$LIFESTAGE)] <- "Juvenile / Adult"

# reorder so the new life stage variable is beside the DEVELOPMENT_STAGE_DESCRIPTION variable
df <- select(df, 1:DEVELOPMENT_STAGE_DESCRIPTION, LIFESTAGE, everything())

stagechk <- dplyr::count(df, LIFESTAGE) # <- re-check life stages and count each

# following life stage variables not entered; values assigned based on assessment of each record (age/size/month stocked, etc)

df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == "A9907-1 (1974-1991)"] <- "Fry"
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == "A9908-1 (1974-1991)"] <- "Fry"
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == "X0003-1 (1992-1996)"] <- "Fingerling"
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == "H3074-1 (1997-2000)"] <- "Yearling"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# take a quick look at how the newly created 'LIFESTAGE' variable shakes out
# check that age bins make sense as well as month stocked

stagechk <- df %>% group_by(LIFESTAGE) %>%
  summarise(min_age = min(FISH_AGE, na.rm = TRUE),
            max_age = max(FISH_AGE, na.rm = TRUE),
            min_month = min(STOCKING_MONTH, na.rm = TRUE),
            max_month = max(STOCKING_MONTH, na.rm = TRUE),
            n = n())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Error corrections
# there are some records that will not categorize properly due to suspected errors - fix these ones manually before proceeding

# all of these records were submitted by Bluewater Anglers Fish Culture Station - should check their data - either weight or age is wrong
df$FISH_AGE[df$STOCKING_EVENT_RECORD_NUM == 23375] <- 36  # six kilo Adult listed as 12 months old - assume age is wrong
df$FISH_AGE[df$STOCKING_EVENT_RECORD_NUM == 26383] <- 36  # six kilo Adult listed as 24 months old - assume age is wrong
df$FISH_AGE[df$STOCKING_EVENT_RECORD_NUM == 14586] <- 36  # four kilo Adult listed as 18 months old - assume age is wrong
df$FISH_AGE[df$STOCKING_EVENT_RECORD_NUM == 27350] <- 36  # four kilo Adult listed as 15 months old - assume age is wrong


# incorrect life stage entered originally (MNRF data)
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == 43137] <- "Sub-adult"  # 22 month old fish originally entered as yearlings
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == 43138] <- "Sub-adult"  # 22 month old fish originally entered as yearlings
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == 43139] <- "Sub-adult"  # 22 month old fish originally entered as yearlings
df$LIFESTAGE[df$STOCKING_EVENT_RECORD_NUM == "F0670-1 (1992-1996)"] <- "Sub-adult"  # 400g fish, no age; originally entered as yearlings

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# assign/rename life stages based on age and time of stocking

# ==== EGG
# Many egg lifestages have an age of 'na' = make them zero instead (helps with sorting)
df$FISH_AGE[df$LIFESTAGE == "Egg"] <- 0

# ==== Fry - by definition, should be <=2 months old or else they are fingerlings
#df$LIFESTAGE[df$LIFESTAGE =="Fry" & df$FISH_AGE>2] <- "Fingerling"
#df$LIFESTAGE[df$LIFESTAGE =="Fingerling" & df$FISH_AGE<=2] <- "Fry"

# ==== clean up inconsistencies
df$LIFESTAGE[df$LIFESTAGE == 'Juvenile / Adult'] <- "Yearling" # this is a silly life stage that doesn't mean anything - make it yearling
df$LIFESTAGE[df$LIFESTAGE == "Adult" & df$FISH_AGE < 23] <- "Yearling" # Adults should be at least two years old
df$LIFESTAGE[df$LIFESTAGE =="Yearling" & df$FISH_AGE < 12] <- "Fingerling" # Yearlings should not be < 12 mos old
df$LIFESTAGE[df$LIFESTAGE =="Fingerling" & df$FISH_AGE >= 12] <- "Yearling" # Fingerlings should not be a year old (or older)

# ==== FINGERLINGS
df$LIFESTAGE[df$LIFESTAGE =="Fingerling" & df$STOCKING_MONTH <=6] <- "Spring Fingerling"
df$LIFESTAGE[df$LIFESTAGE =="Fingerling" & df$STOCKING_MONTH >=7] <- "Fall Fingerling"

# ==== YEARLINGS
df$LIFESTAGE[df$LIFESTAGE =="Yearling" & df$STOCKING_MONTH <=6] <- "Spring Yearling"
df$LIFESTAGE[df$LIFESTAGE =="Yearling" & df$STOCKING_MONTH >=7] <- "Fall Yearling"


# ==== ADULTS and SUB_ADULTS (difference appears to be arbitrary, but age shouldn't overlap)

df$LIFESTAGE[df$LIFESTAGE == "Sub-adult" & df$FISH_AGE > 35] <- "Adult"
df$LIFESTAGE[df$LIFESTAGE == "Adult" & df$FISH_AGE < 36] <- "Sub-adult"


# Step 6: clip codes ------------------------------------------------------------------------------------------------

# blank clip codes should be zero; NAs should also be zero
df$CLIP1_TYPE_CODE[df$CLIP1_TYPE_CODE == ""] <- 0
df$CLIP2_TYPE_CODE[df$CLIP2_TYPE_CODE == ""] <- 0

df$CLIP1_TYPE_CODE[is.na(df$CLIP1_TYPE_CODE)] <- 0
df$CLIP2_TYPE_CODE[is.na(df$CLIP2_TYPE_CODE)] <- 0

# bring in clip lookup table
clipcode <- read.csv("Data/Lookup/clipcode.csv", stringsAsFactors = FALSE)

# clipcodes are typically up to four characters (RPAD = right pectoral + adipose)
# in FSIS, there are two variables for CLIP, and no enforcement of order (you can have RPAD and ADRP - same clip, different name)
# first, concatenate the two FSIS codes so we can clean this mess up

df <- left_join(df, clipcode, by = c("CLIP1_TYPE_CODE" = "CLIP"))
df <- left_join(df, clipcode, by = c("CLIP2_TYPE_CODE" = "CLIP"))

# join both new clip fields into one variable
df$CLIPCODE <- paste(df$CLIPCODE.x, df$CLIPCODE.y, sep = "")

# now that join has been made, drop the resultant extra x and y variables
df <- select(df, -c("CLIPCODE.x", "CLIPCODE.y", "FIN.x", "FIN.y"))

# see what CLIPCODE looks like
chkclip <- dplyr::count(df, CLIPCODE)

# need to clean some things up.....
df$CLIPCODE <- gsub("NoneNone","NONE",df$CLIPCODE)
df$CLIPCODE <- gsub("None","",df$CLIPCODE, ignore.case = FALSE)
df$CLIPCODE <- gsub("ADRV","RVAD",df$CLIPCODE)

# check to see that you're happy with the changes
chkclip <- dplyr::count(df, CLIPCODE)
chkclipwide <- dplyr::count(df, CLIP1_TYPE_CODE, CLIP2_TYPE_CODE, CLIPCODE)

# move new clip variable to follow original clip value
df <- df %>% move_columns("CLIPCODE", .after = "CLIP_FLAG")

# Step 7: proponent and rearing ----------------------------------------------------------------------------------

# values for PROPONENT_TYPE_CODE, PROPONENT_NAME, REARING_SITE_TYPE_CODE, REARING_SITE_NAME are awful.  FSIS does not enforce consistent data entry for these values
# as a result, it's a mess.  Generally, differentiating between MNR and Community Hatcheries (CHP or CFIP) is enough, however, it's nice to have clean data too.

# see what this mess looks like before we start
chkprop <- dplyr::count(df, PROPONENT_TYPE_CODE, PROPONENT_NAME, REARING_SITE_TYPE_CODE, REARING_SITE_NAME)

df$PROPONENT_NAME[grepl("Brant", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Brant Rod and Gun Club"
df$PROPONENT_NAME[grepl("Barrow", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Barrow Bay and District Sportfishing Association"
df$PROPONENT_NAME[grepl("bpsa", df$PROPONENT_NAME, ignore.case=TRUE)] <- "BPSA"
df$PROPONENT_NAME[grepl("BPSC", df$PROPONENT_NAME, ignore.case=TRUE)] <- "BPSA"
df$PROPONENT_NAME[grepl("Bruce", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Bruce Peninsula Sportsman Association"
df$PROPONENT_NAME[grepl("Credit", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Credit River Anglers Association"
df$PROPONENT_NAME[grepl("CRAA", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Credit River Anglers Association"
df$PROPONENT_NAME[grepl("Georgian", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Georgian Triangle Anglers Association"
df$REARING_SITE_NAME[grepl("islington", df$REARING_SITE_NAME, ignore.case = TRUE)] <- "Islington Sportsmans Club"
df$PROPONENT_NAME[grepl("Islington", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Islington Sportsmans Club"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "Islington Sportsmans Club"] <- "Islington Sportsmans Club"
df$PROPONENT_NAME[grepl("Erie", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Lake Erie Salmon and Trout Club"
df$PROPONENT_NAME[grepl("Cham", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Chaminade Environmental"
df$PROPONENT_NAME[grepl("Hep", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Hepworth Anglers Club"
df$PROPONENT_NAME[grepl("Huron", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Lake Huron Fishing Club"
df$PROPONENT_NAME[grepl("lhfc", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Lake Huron Fishing Club"
df$REARING_SITE_NAME[grepl("Huron", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Kincardine"
df$PROPONENT_NAME[grepl("Linwood", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Linwood Public School"
df$PROPONENT_NAME[grepl("Paul Helm Memorial Hatchery", df$REARING_SITE_NAME, ignore.case=TRUE)] <- "Brant Rod and Gun Club"
df$REARING_SITE_NAME[grepl("Ringwood", df$REARING_SITE_NAME, ignore.case=TRUE)] <- "Ringwood"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Ringwood" & df$STOCKING_YEAR >=2007 & df$STOCKING_YEAR<=2011] <- "OFAH - MEA - Ringwood"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Ringwood" & df$STOCKING_YEAR >=2012] <- "MEA - Ringwood"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "OFAH - MEA - Ringwood" & df$PROPONENT_NAME == "Metro East Anglers"] <- "OFAH - MEA - Ringwood"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "OFAH - MEA - Ringwood" & df$PROPONENT_NAME == "OFAH"] <- "OFAH - MEA - Ringwood"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "MEA - Ringwood"] <- "Metro East Anglers"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Ringwood" & df$STOCKING_YEAR<2007] <- "RINGWOOD"
df$PROPONENT_NAME[grepl("GTAA", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Georgian Triangle Anglers Association"
df$PROPONENT_NAME[grepl("ssa", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Sydenham Sportsman"
df$REARING_SITE_NAME[grepl("Sydenham", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Weaver Creek Hatchery"
df$PROPONENT_NAME[grepl("Thames", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Thames River Anglers Hatchery"
df$REARING_SITE_NAME[grepl("Thames", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Thames River Anglers Hatchery"
df$PROPONENT_NAME[grepl("Pauline", df$PROPONENT_NAME, ignore.case=TRUE)] <- "Pauline Johnson Collegiate Vocational Institute"
df$PROPONENT_TYPE_CODE[grepl("FCS", df$REARING_SITE_NAME, ignore.case=TRUE)] <- "MNR"
df$REARING_SITE_TYPE_CODE[df$PROPONENT_TYPE_CODE =="MNR"] <- "MNR"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_TYPE_CODE =="MNR"] <- "MNR"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Blue Jay Creek FCS"] <- "BLUE JAY CREEK"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Chatsworth FCS"] <- "CHATSWORTH"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Harwood FCS"] <- "HARWOOD"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "COLLINGWOOD TWP."] <- "Collingwood Township"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Normandale FCS"] <- "NORMANDALE"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "Tarentorus FCS"] <- "TARENTORUS"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "White Lake FCS"] <- "WHITE LAKE"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "PORT STANLEY LESTC CFIP"] <- "Port Stanley LESTC CFIP"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "Port Stanley LESTC CFIP"] <- "Lake Erie Salmon and Trout Club"
df$REARING_SITE_NAME[df$REARING_SITE_NAME == "P.HELM MEM. HATCH."] <- "Paul Helm Memorial Hatchery"
df$PROPONENT_NAME[df$PROPONENT_NAME == "BPSA"] <- "Bruce Peninsula Sportsman Association"
df$PROPONENT_NAME[df$REARING_SITE_NAME == "Parkview Hatchery"] <- "Metro East Anglers"

df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Bluewater Anglers Fish Culture Station"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Chaminade Fish Hatchery"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Credit River Angers Association Hatchery"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Islington Sportsmans Club"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Linwood Public School"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "MEA - Ringwood"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Parkview Hatchery"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Paul Helm Memorial Hatchery"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Pauline Johnson School"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Port Stanley LESTC CFIP"] <- "CFIP"
df$PROPONENT_TYPE_CODE[df$REARING_SITE_NAME == "Thames River Anglers Hatchery"] <- "CFIP"

chkprop <- dplyr::count(df, PROPONENT_TYPE_CODE, PROPONENT_NAME, REARING_SITE_TYPE_CODE, REARING_SITE_NAME)


# Step 8: strain --------------------------------------------------------------------------

# check STOCK_STRAIN_NAME variable before we start
strainchk <- dplyr::count(df, STOCK_STRAIN_NAME)

#df$STOCK_STRAIN_NAME[df$STOCK_STRAIN_NAME == "GNCT" ] <- "Ganaraska River (CT)"
#df$STOCK_STRAIN_NAME[df$STOCK_STRAIN_NAME == "Unknown Stock" ] <- NA

# generally don't care which hatchery fish are produced at - change hatchery acronym to 'MNRF' in strain descriptor
fcs <- c("WKC|CTC|CWC|HWC|HLC|TTC|DNC|NMC|CT")
df$STRAIN <- str_replace_all(df$STOCK_STRAIN_NAME, fcs, "MNRF")

# check new STRAIN variable
strainchk <- dplyr::count(df, STRAIN)

# move new strain variables to follow original strain value
df <- df %>% relocate(STRAIN, .after = STOCK_STRAIN_NAME)

# Step 9: biomass --------------------------------------------------------------------------

# recalculate RECORD_BIOMASS_CALC to ensure that there are no NAs
df$FISH_WEIGHT <- as.numeric(df$FISH_WEIGHT)
df$RECORD_BIOMASS_CALC <- df$FISH_STOCKED_COUNT*df$FISH_WEIGHT

# populate new biomass(kg) variable
df$BIOMASS_KG <- round(df$RECORD_BIOMASS_CALC/1000,digits =1)

# move new biomass variable to follow original biomass value
df <- df %>% relocate(BIOMASS_KG, .after = RECORD_BIOMASS_CALC)

# Step 10: export ----------------------------------------------------------------------------------------

brown_fsis <- df

save(brown_fsis, file="Data/Processed/brown_fsis.rda")