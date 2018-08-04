###########################################################################################
# Developed by Mike Barough                                                               #
# Latest Revision Jul 2018                                                                #
###########################################################################################

#################### Installing required packages ##########################
for(package in c("dplyr", "ggplot2", "lubridate", "ROCR", "rjson"
                 , "manipulate", "boot","RODBC", "plotly"
                 , "openxlsx", "tidyr", "stringr", "readxl", "forecast"
                 , "corrplot", "randomForest", "sqldf", "car", "rpart"
                 , "party", "rpart.plot", "rpart.utils", "logisticPCA"
                 , "robustbase", "cluster", "logisticPCA", "ggrepel"
                 , "NbClust", "utils", "PCAmixdata"
                 , "maptools", "rgeos", "rgdal", "fiftystater", "mapproj"
                 , "colorplaner", "reshape", "data.table", "fuzzyjoin"
                 , "stringr", "ggmap") ) {
  print(package)
  if(!require(package, character.only=T, quietly=T)){
    library(package, character.only=T)
  }
  if(require(package)){
    library(package, character.only=T) 
  }
}

sessionInfo()

#################### Global Parameters ######################################
# Setting Seed Value
set.seed(123)

# Setting R active directory path
myPath ="C:/Users/abdolho/Desktop/ZBB-LTI"
setwd(myPath)


##################### Input Data
RawInputData <- read_excel("NJ WEX Accepting Gas Stations.xlsx", sheet = 1)

#############################################################################
# The input data (aka address format) needs to follow as below otherwise    #
# it wont work with the API:                                                #
# Address, City, State (e.g. NJ), Zip (5 or 9), Country                     #
#############################################################################

RawInputData$address <- paste0(RawInputData$Address, 
                               ", ", RawInputData$City,
                               ", ", RawInputData$`State / Province`,
                               " ", RawInputData$`Postal Code`,
                               ", USA")

DistinctLocations <- as.data.frame(unique(RawInputData$address))
DistinctLocations <- cbind(row.names(DistinctLocations), DistinctLocations)
colnames(DistinctLocations) <- c("Index", "StationAddress")
sapply(DistinctLocations, function(x) sum(is.na(x)))

# Function to get the Geo details for any address
getGeoDetails <- function(address){   
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  # Now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #Mechanism to avoid reaching Google's query limit
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("Query Limit Reached - Pausing for a few seconds at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(ceiling(runif(1, 3.0, 8.0))) 
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  # Return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  # Otherwise extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  return(answer)
}

# Initialise a dataframe to hold the results
geocoded <- data.frame()

# Find out where to start in the address list (if the script was interrupted before):
startindex <- 1

# If a temp file exists - load it up and count the rows
tempfilename <- paste0("NJ WEX Accepting Gas Stations", '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address 
# geocode() function has a random function built-in which randomizes
# query time to avoide reaching Google's query limit per seconds

for (ii in seq(startindex, nrow(DistinctLocations))){
  print(paste("Working on index", ii, "of", nrow(DistinctLocations)))
  # Query the google geocoder - this will pause here if we are over the limit
  result = getGeoDetails(as.character(DistinctLocations[ii,"StationAddress"])) 
  print(result$status)     
  result$index <- ii
  geocoded <- rbind(geocoded, result)
  # Save temporary results
  saveRDS(geocoded, tempfilename)
  print(paste(ii, result))
}

# Add the latitude and longitude to the main data
DistinctLocations$lat <- geocoded$lat
DistinctLocations$long <- geocoded$long
DistinctLocations$accuracy <- geocoded$accuracy

# Write aggrigated file to the output
saveRDS(DistinctLocations, paste0("NJ WEX Accepting Gas Stations" ,"_geocoded.rds"))
write.table(DistinctLocations, file=paste0("NJ WEX Accepting Gas Stations" ,"_geocoded.csv"), sep=",", row.names=FALSE)