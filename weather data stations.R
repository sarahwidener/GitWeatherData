#x <- coops_search(begin_date=20110101,end_date=20111229,datum="stnd",product="monthly_mean")
#change to be hourly
library(rnoaa)
library(tidyverse)
noaa_token = 'psmhQTgHNybFHDfcChcODtUpHpEMyDvx'
ncdc_locs_cats( token=noaa_token)
myDataTypes <- c('TOBS')
myDataTypes <- c('TMAX','PRCP',"TMIN","SNWD","ANWD","SNOW")
myStations <- c('GHCND:JA000047412'
                # , 'GHCND:CA1SK000130'
                , 'GHCND:USC00118740'
                , 'GHCND:KSM00047101')
                # , 'GHCND:DA000021430'
                # , 'GHCND:CHM00058424')
for(yearNo in 2015:2015) {
  my_start_date <- paste(yearNo, '-01-01', sep = "")
  my_end_date <- paste(yearNo, '-12-31', sep = '')
  for (dataTypeNo in 1:length(myDataTypes)) {
    my_data_type <- myDataTypes[dataTypeNo]
    firstRun = T
    for (stationNo in 1:length(myStations)) {
      my_station <- myStations[stationNo]
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
        firstRun = F
        fileOut = out$data
        fileOut<-rename(fileOut, !!my_station:="value")
        fileOut <- fileOut[!names(fileOut) %in% c("fl_m","fl_q","fl_so","fl_t","station")]
      } else {
        if(is.null(out$meta$totalCount)){print(paste("no data",my_station,' '))}else
          {
          mergeData<-out$data
          mergeData<-rename(mergeData, !!my_station:="value")
          fileOut <-
            merge(fileOut, mergeData[, c("date", my_station)], by = "date", all =
                    TRUE)
        }
      }
    }
    my_year <- substr(my_start_date, 1, 4)
    write.csv(fileOut, paste("output_", my_year, "_data_all_", my_data_type,".csv", sep = ""))
  } 
}
