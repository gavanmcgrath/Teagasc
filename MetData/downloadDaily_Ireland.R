#Pull Met Eireann rainfall data
#Author: 
#  Gavan McGrath
#  Environment Soils and Land Use Department,
#  Teagasc, Ireland

#Description:
# The following code trawls Met Eireann for available meterological sites with daily rainfall data
# Downloads the data to a specified directory
# Reads the data and saves it in an r object dlydatabase.rds


#get current working directory
cwd <- getwd()
#edit the following to chose save directory
setwd("D:/Data/AHPC/climate_Daily")

#base url to search for data
met.ie_drlySites <- "https://cli.fusio.net/cli/climate_data/webdata/"
#loop over possible site names from 1 to 9999 checking for a site
#if exists then download the data, unzip and delete the downloaded zip file
sapply(1:9999, FUN = function(x) {
  tryCatch({
    site = paste0(met.ie_drlySites,"dly",x,".zip")
    destfile = paste0("dly",x,".zip")
    zdir <- paste0("dly",x)
    download.file(site, 
                  destfile = destfile,
                  method = "wininet")
    
    unzip(destfile, exdir = zdir)
    unlink(destfile) 
  }, 
  error = function(e) print(paste0("Not valid url: ", site ) 
  )
  )
})


#Create a data base
wrking.dir <- getwd()
dirs <- list.dirs()
n <- length(dirs)
hrly.database <- data.frame(station = vector("character", length = n), 
                            height = vector("numeric", length =n), 
                            lat = vector("numeric", length =n), 
                            long = vector("numeric", length =n),
                            start = vector("character", length =n),
                            end = vector("character", length =n),
                            folder = vector("character",length = n) 
                            , stringsAsFactors = FALSE)
#Loop over the downlaoded data, read in relevant information from the 
#header such as coordinates etc, read in the data and store the data in hrly.database
weather <- vector("list", length =n)
for (i in 2:length(dirs)){
  setwd(dirs[i])
  climfile <- list.files(".", pattern = ".csv")
  dat <- readLines(climfile)
  stn.id <- trimws(strsplit(dat[1], split = ":")[[1]][2])
  stn.height <- as.numeric(strsplit(trimws(strsplit(dat[2], split = ":")[[1]][2]), split = " ")[[1]][1])
  latlong <- as.numeric(strsplit(gsub("[[:alpha:,:punct:]]*", "", dat[3]), split = ",")[[1]])
  pos <- which(dat == "")
  print(paste0("Station id: ",stn.id))
  print(paste0("Lat: ",latlong[1], " Lon: ", latlong[2]))
  hrly.database$station[i-1] <- stn.id
  hrly.database$height[i-1] <- stn.height
  hrly.database$lat[i-1] <- latlong[1]
  hrly.database$long[i-1] <- latlong[2]
  hrly.database$folder[i-1] <- dirs[i]
  variables <- c()
  unit.var <- c()
  for (j in (pos[2]+1):(pos[3]-1)){
    ln1 <- dat[j]
    varbl <- strsplit(ln1,split = ":")[[1]]
    variables <- c(variables,varbl[1] )
    unt <- trimws(strsplit(varbl[2], split = "-")[[1]][2])
    unit.var <- c(unit.var , unt)
  }
  climdat <- read.table(climfile, skip = pos[3], header = TRUE,
                        colClasses = c("character",rep("numeric", length(variables)-1)),
                        fill = TRUE, sep = ",")
  dates <- as.POSIXct( climdat[,1], format = "%d-%b-%Y")
  climdat[,1] <- dates
  dtrng <- range(dates, na.rm=TRUE)
  print(paste0("Date Range. From: ", dtrng[1], " To: ",dtrng[2] ))
  print("")
  hrly.database$start[i-1] <- as.character(dtrng[1])
  hrly.database$end[i-1] <- as.character(dtrng[2])
  
  attr(climdat, "units") <- unit.var
  attr(climdat, "variables") <- variables
  weather[[i-1]] <- climdat
  
  setwd(wrking.dir)
}
#Save the database to an R data object
saveRDS(list(stations = hrly.database,
             weather = weather), 
        file = "dlydatabase.rds")
#Reset the working directory
setwd(cwd)



