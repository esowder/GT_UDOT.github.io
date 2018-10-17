####################################################
# install.packages(c("dplyr", "jsonlite", "lubridate", "psych", "ggplot"))

####################################################
# 
# Load Libraries
#
####################################################
library(dplyr)
library(lubridate)
library(jsonlite)
library(psych)
library(ggplot2)
  
####################################################
# 
# Global Vars
#
####################################################
# archive_url <- "https://dashboard.udot.utah.gov/resource/xngc-cqph.json"

# socrataEmail <- "phil.nacamuli@us.gt.com"

# socrataPassword <- "INSERT PASSWORD HERE"

# now <- Sys.time()

####################################################
# 
# Read in Data
#
####################################################
# setwd('C:/Users/us58957/Documents/UDOT/Datasets_TEMP')

## ROW Data
row_data <- read.csv("ROWdata.csv", header = TRUE)
 # fromJSON("https://dashboard.udot.utah.gov/resource/mqjj-r3px.json?$limit=1000000000")
# fromJSON("https://dashboard.udot.utah.gov/resource/7aik-44re.json?$limit=1000000000") # Sep4 file

####################################################
# 
# Explore Data
#
####################################################

# head(row_data)
# summary(row_data)
# variable.names(row_data)

####################################################
# 
# Change data types (Dates & Numerics)
#
####################################################
names(row_data) <- tolower(names(row_data))
## Dates
row_data$acquisition_cleared_date <- mdy(row_data$acquisition_cleared_date)
row_data$committed_advertise_date <- mdy(row_data$committed_advertise_date)
row_data$construction_cleared_date <- mdy(row_data$construction_cleared_date)
row_data$process_end <- mdy(row_data$process_end)
row_data$process_start <- mdy(row_data$process_start)
row_data$closing_completed_date <- mdy(row_data$closing_completed_date)
row_data$eminent_domain_settled_date <- mdy(row_data$eminent_domain_settled_date)

## Numerics
# Convert NAs to "" & numeric to use sum() function and must make NAs zero
row_data$parcel_cost <- as.numeric(row_data$parcel_cost)
row_data$parcel_cost[is.na(row_data$parcel_cost)] <- 0
row_data$process_days <- as.numeric(row_data$process_days)
row_data$process_days[is.na(row_data$process_days)] <- 0

## Characters
row_data$eminent_domain_resolution <- as.character(row_data$eminent_domain_resolution)

####################################################
# 
# Manipulate Data 
#
####################################################

## update emminent domain resolution to meaningful data pts
row_data$eminent_domain_resolution[is.na(row_data$eminent_domain_resolution)] <- 'Eminent Domain Not Used'
row_data$eminent_domain_resolution[row_data$eminent_domain_resolution == ''] <- 'Eminent Domain Not Used'
row_data$eminent_domain_resolution[row_data$eminent_domain_resolution == 'AG'] <- 'Eminent Domain Settled By Stipulation'
row_data$eminent_domain_resolution[row_data$eminent_domain_resolution == 'ROW_DEPT'] <- 'Eminent Domain Settled by Contract'
## Need to find the data point that points to trial used!
# row_data$eminent_domain_resolution[row_data$eminent_domain_resolution == 'X'] <- 'Eminent Domain Trials'

## Adding Column for # parcels in project
#create tibble to count # of parcels by project
count <- row_data %>%
group_by(proj_xref_no) %>%
  summarize(count_parcel_by_project = n()) 
#create joined table with calculated fields from tibble
row_data <- inner_join(row_data, count, by = NULL, copy = FALSE)
# Change data type - numeric
row_data$count_parcel_by_project <- as.numeric(row_data$count_parcel_by_project)
row_data$count_parcel_by_project[is.na(row_data$count_parcel_by_project)] <- 0

## Adding column for Fiscal Years - acquisition cleared date
#create calculated field for acquisition cleared date
row_data$y <- year(row_data$acquisition_cleared_date)
row_data$q <- quarter(row_data$acquisition_cleared_date, with_year = FALSE, fiscal_start = 7)
row_data$fy_acquisition_cleared_date <- ifelse(row_data$q<=2, row_data$y+1, row_data$y)

## Adding column for Fiscal Years - process end date
#create calculated field for process end date
row_data$y1 <- year(row_data$process_end)
row_data$q1 <- quarter(row_data$process_end, with_year = FALSE, fiscal_start = 7)
row_data$fy_process_end <- ifelse(row_data$q1<=2, row_data$y1+1, row_data$y1)

# Remove unneccesary columns & remove voided out parcels
row_data <- row_data %>%
  select(-32, -33, -35, -36) %>%
  filter(status_type_id != '16')

## Add Total Sum for Last Full Fiscal Yr
row_data2 <- row_data %>%
  group_by(fy_acquisition_cleared_date) %>%
  summarize(fy_parcel_cost = sum(parcel_cost)) %>%
  filter(!is.na(fy_acquisition_cleared_date))
# add the summarized data to the dataset
row_data <- left_join(row_data, row_data2, by = NULL, copy = FALSE)

## Create groupings for parcel size
row_data$parcel_category[row_data$count_parcel_by_project <= 50 ] = "A (1-50)"
row_data$parcel_category[row_data$count_parcel_by_project > 50 & row_data$count_parcel_by_project <= 100] = "B (51-100)"  
row_data$parcel_category[row_data$count_parcel_by_project > 100 & row_data$count_parcel_by_project <= 200] = "C (101-200)"
row_data$parcel_category[row_data$count_parcel_by_project > 200 & row_data$count_parcel_by_project <= 300] = "D (201-300)"  
row_data$parcel_category[row_data$count_parcel_by_project > 300 & row_data$count_parcel_by_project <= 500] = "E (301-400)"
row_data$parcel_category[row_data$count_parcel_by_project > 400 & row_data$count_parcel_by_project <= 500] = "F (401-500)"
row_data$parcel_category[row_data$count_parcel_by_project > 500] = "G (+501)"

## Add limitation days saved value
# create temporary process end data of today's data to avoid NAs
today <-Sys.Date()
row_data$temp_process_end <- row_data$process_end
row_data$temp_process_end[is.na(row_data$temp_process_end)] <- today

row_data$limitation_days_saved <- ifelse(row_data$limitation == 'Y', 
                                         row_data$committed_advertise_date - row_data$temp_process_end, '')
# change data type - numeric
row_data$limitation_days_saved <- as.numeric(row_data$limitation_days_saved)
# row_data$limitation_days_saved[is.na(row_data$limitation_days_saved)] <- 0

# group by project
count_by_project_days_saved <- row_data %>%
  filter(limitation == 'Y') %>%
  group_by(proj_xref_no) %>%
  select(proj_xref_no, pin, limitation_days_saved, fy_process_end) %>%
  distinct(proj_xref_no, pin, limitation_days_saved, fy_process_end)

# create column in count_by_project to evaluate pass or fail for Socrata Viz
count_by_project_days_saved <- count_by_project_days_saved %>%
  mutate(sucess = ifelse(limitation_days_saved >= 0, "A - Meets Deadline", "B - Does Not Meet Deadline"))

## Create Dummy Variable to be used in Socrata Viz
# parcel_id & ownership_id have duplicates & this gets reflected in costs & domain rates
row_data$parcel_id_duplicated <- duplicated(row_data$parcel_id)
row_data$ownership_id_duplicated <- duplicated(row_data$ownership_id)

# Remove temp_process_end from file
row_data <- row_data %>%
  select(-temp_process_end)
####################################################
# 
# Export New Dataset
#
####################################################
# Clean up NAs
# Convert NAs to ""
row_data$acquisition_cleared_date <- as.character(row_data$acquisition_cleared_date)
row_data$committed_advertise_date <- as.character(row_data$committed_advertise_date)
row_data$construction_cleared_date <- as.character(row_data$construction_cleared_date)
row_data$process_end <- as.character(row_data$process_end)
row_data$process_start <- as.character(row_data$process_start)
row_data$closing_completed_date <- as.character(row_data$closing_completed_date)
row_data$eminent_domain_settled_date <- as.character(row_data$eminent_domain_settled_date)

row_data[is.na(row_data)] <- ""
count_by_project_days_saved$limitation_days_saved[is.na(count_by_project_days_saved$limitation_days_saved)] <- 0
count_by_project_days_saved[is.na(count_by_project_days_saved)] <- ""

# write csv
write.csv(row_data, file = "row_data.csv", row.names = FALSE)
write.csv(count_by_project_days_saved, file = "row_data_limitaitonsAvg.csv", row.names = FALSE)


