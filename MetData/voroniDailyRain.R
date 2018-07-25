voroniRain <- function(start.date,
                       end.date, 
                       daily = TRUE, 
                       plotVoroni = FALSE, 
                       fid = c("D:/Data/AHPC/climate_Daily/dlydatabase.rds","D:/Data/AHPC/climate_Hourly/hrldatabase.rds"){
  #Input Require
  #start.date a character vector of length 1 'yyyy-mm-dd'
  #end.date a character vector of length 1 'yyyy-mm-dd'
  #daily a boolean indicating whether it is horuly (FALSE) or daily (TRUE) data.   
  #plotVoroni a boolean, indicating whether to plot the Voroni tesellation       
  #fid a character string of the name of the rds file to upload. Created by xxx.R                       
  
  #Input Optional:
  #daily a boolean (default = TRUE) whether to load available daily stations (TRUE)
  # or whether to load available hourly stations
  
  #Value:
  #A spatial polygons dataframe
  # A Thesian polygon of active daily rain gauges in the database
  # Attributes of the polygons includes:
  #   the staion name
  #   the station height
  #   the lat and long coordinates of the station
  #   the start and end dates of the daily record (note this may include some NAs)
  #   the folder location where daily rain data is held (relative to D:/Data/AHPC/climate_Daily)
  
  #Requires:
  #The presence of D:/Data/AHPC/climate_Daily/dlydatabase.rds as created by
  #downloadDaily_Ireland.R
  
  #Libraries
  require(rgdal)
  require(sp)
  require(dismo)
  require(raster)
  #require(rgeos)
  
  if (daily){
    dailyDB <- readRDS(fid)
  } else {
    dailyDB <- readRDS(fid)
  }
  pos2strip <- which(dailyDB$stations$long == 0)
  if (length(pos2strip) > 0 ){
    dailyDB$stations <- dailyDB$stations[-pos2strip,]
  }
  stationDaily <- SpatialPointsDataFrame(coords = dailyDB$stations[,c("long","lat")], 
                                         data = dailyDB$stations,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  pos.start <- which(as.Date(stationDaily$start) <= as.Date(start.date))
  pos.end <-which(as.Date(stationDaily$end) >= as.Date(end.date))
  
  if (length(pos.start) == 0) stop("Start Date not in Range")
  if (length(pos.end) == 0) stop("End Date not in Range")
  pos.indates <- intersect(pos.start,pos.start)
  if (length(pos.indates) == 0) stop("No data in date range")
  
  stationDailyTrimmed <- stationDaily[pos.indates,]
  #irelnd <- spTransform(irelnd, CRSobj = CRS(proj4string(stationDaily)))
  
  #Thesian Polygon from coordinates of points
  xys <- coordinates( stationDailyTrimmed)
  vor <- voronoi(xys, ext = c())
  proj4string(vor) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #append station data to each polygon
  vordat <- over(vor,stationDailyTrimmed)
  vor$station <- vordat$station
  vor$height <- vordat$height
  vor$lat <- vordat$lat
  vor$long <- vordat$long
  vor$start <- vordat$start
  vor$end <- vordat$end
  vor$folder <- vordat$folder
  
  #to do intersect the voroni with a map of ireland  
  #vor2 <- intersect(vor,ireland)
  
  if (plotVoroni){
    plot(vor)
    points(stationDailyTrimmed)
  }
  
  return(vor)
}
