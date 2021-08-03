WeatherData=function(stations=NULL, years=NULL){
#x <- coops_search(begin_date=20110101,end_date=20111229,datum="stnd",product="monthly_mean")
#change to be hourly
library(Rcpp) #Justin: had to change to get ncdc_locs_cats to work correctly
library(rnoaa)
library(tidyverse)
getwd()
noaa_token = 'psmhQTgHNybFHDfcChcODtUpHpEMyDvx'
ncdc_locs_cats(token=noaa_token)
myDataTypes <- c('TOBS')
myDataTypes <- c('TMAX','PRCP',"TMIN","SNWD","ANWD","SNOW")
myStations=lapply(stations, function(x) paste0("GHCND:", x))
#  myStations <- c('GHCND:JA000047412'
#                 , 'GHCND:CA1SK000130'
#                 , 'GHCND:USC00118740'
#                 , 'GHCND:KSM00047101'
#                 , 'GHCND:'
#                 , 'GHCND:CHM00058424')
for(yearNo in years) {
  my_start_date <- paste(yearNo, '-01-01', sep = "")
  my_end_date <- paste(yearNo, '-12-31', sep = '')
  for (dataTypeNo in 1:length(myDataTypes)) {
    my_data_type <- myDataTypes[dataTypeNo]
    firstRun = T #I think tDA000021430hat this is where it is breaking down: this tells the below if else to build the 
    #fileout data from the first station. However, this does not work when there is not data from that station 
    #for a given data type. 
    #The loop is specifically breaking on cases where there is no data for the first station for a given datatype. 
    for (stationNo in 1:length(myStations)) {
      my_station=paste(myStations[stationNo])
      print(my_station)
      out <-
        ncdc(
          datasetid = 'GHCND',
          stationid = my_station,
          datatypeid = my_data_type,
          startdate = my_start_date,
          enddate = my_end_date,
          limit = 1000,
          token = noaa_token
        )
      if (firstRun) {
        if (is.null(out$meta$totalCount)) {
          print(paste("no data",my_station,' '))
        }else{
        firstRun = F
        fileOut = out$data
        fileOut<-rename(fileOut, !!my_station:="value") #This is where the loop is breaking down. Not sure 
        #Why it is breaking down only on some w/o data and not all
        fileOut <- fileOut[!names(fileOut) %in% c("fl_m","fl_q","fl_so","fl_t","station")]
        }
      } else {
        if(is.null(fileOut$date)){
          fileOut = out$data
          fileOut<-rename(fileOut, !!my_station:="value") #This is where the loop is breaking down. Not sure 
          #Why it is breaking down only on some w/o data and not all
          fileOut <- fileOut[!names(fileOut) %in% c("fl_m","fl_q","fl_so","fl_t","station")]
        } else {
        if(is.null(out$meta$totalCount)){print(paste("no data",my_station,' '))}else{
          mergeData<-out$data
          mergeData<-rename(mergeData, !!my_station:="value")
          fileOut <-
            merge(fileOut, mergeData[, c("date", my_station)], by = "date", all =
                    TRUE)
        }
        }
      }
    }
    my_year <- substr(my_start_date, 1, 4)
    write.csv(fileOut, paste("output_", my_year, "_data_all_", my_data_type, ".csv", sep = ""))
  }
}
}
