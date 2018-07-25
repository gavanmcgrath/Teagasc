#Pulls Hourly Met Eireann rainfall data from a selection of sites
cwd <- getwd()
#edit this to an appropriate directory
setwd("D:/Data/AHPC/climate_Hourly")
#List of urls
met.ie_hrlySites <- c(
#Johnstown Castle
"https://cli.fusio.net/cli/climate_data/webdata/hly1775.zip",
#Mulingar
"https://cli.fusio.net/cli/climate_data/webdata/hly875.zip",
#Gurteen
"https://cli.fusio.net/cli/climate_data/webdata/hly1475.zip",
#Markree
"https://cli.fusio.net/cli/climate_data/webdata/hly1275.zip",
#Mt_Dillon
"https://cli.fusio.net/cli/climate_data/webdata/hly1975.zip",
#Dunsany
"https://cli.fusio.net/cli/climate_data/webdata/hly1375.zip",
#Belmullet
"https://cli.fusio.net/cli/climate_data/webdata/hly2375.zip",
#Claremorris
"https://cli.fusio.net/cli/climate_data/webdata/hly2175.zip",
#Knock_Airport
"https://cli.fusio.net/cli/climate_data/webdata/hly4935.zip",
#Newport
"https://cli.fusio.net/cli/climate_data/webdata/hly1175.zip",
#Valentia_Observatory
"https://cli.fusio.net/cli/climate_data/webdata/hly2275.zip",
#Athenry
"https://cli.fusio.net/cli/climate_data/webdata/hly1875.zip",
#Mace_Head
"https://cli.fusio.net/cli/climate_data/webdata/hly275.zip",
#Casement
"https://cli.fusio.net/cli/climate_data/webdata/hly3723.zip",
#Dublin_Airport
"https://cli.fusio.net/cli/climate_data/webdata/hly532.zip",
#Phoenix_Park
"https://cli.fusio.net/cli/climate_data/webdata/hly175.zip",
#Fineer
"https://cli.fusio.net/cli/climate_data/webdata/hly2075.zip",
#Malin_Head
"https://cli.fusio.net/cli/climate_data/webdata/hly1575.zip",
#Cork_Airport
"https://cli.fusio.net/cli/climate_data/webdata/hly3904.zip",
#Moore_Park
"https://cli.fusio.net/cli/climate_data/webdata/hly575.zip",
#Roches_Point
"https://cli.fusio.net/cli/climate_data/webdata/hly1075.zip",
#Sherkin_Island
"https://cli.fusio.net/cli/climate_data/webdata/hly775.zip",
#Shannon_Airport
"https://cli.fusio.net/cli/climate_data/webdata/hly518.zip",
#Ballyhaise
"https://cli.fusio.net/cli/climate_data/webdata/hly675.zip",
#Oak_Park
"https://cli.fusio.net/cli/climate_data/webdata/hly375.zip"
)
dest.files <- strsplit(met.ie_hrlySites,split = "https://cli.fusio.net/cli/climate_data/webdata/")
dest.files<- unlist(dest.files)
dest.files <- dest.files[dest.files!= ""]
dest.files <- paste0("D:/Data/AHPC/climate_Hourly/", dest.files)
sapply(1:length(dest.files), FUN = function(x) {
  download.file(met.ie_hrlySites[x], 
                destfile = dest.files[x],
                method = "wininet")
  zdir <- strsplit(dest.files[x],split=".zip")[[1]]
  unzip(dest.files[x], exdir = zdir)
  unlink(dest.files[x])
})

#Create Database
wrking.dir <- getwd()
dirs <- list.dirs()
n <- length(met.ie_hrlySites)
hrly.database <- data.frame(station = vector("character", length = n), 
                            height = vector("numeric", length =n), 
                            lat = vector("numeric", length =n), 
                            long = vector("numeric", length =n),
                            start = vector("character", length =n),
                            end = vector("character", length =n),
                            folder = vector("character", length =n),
                            end = vector("numeric", length =n),
                            stringsAsFactors = FALSE)
weather <- vector("list", length =n)
for (i in 2:length(dirs)){
  setwd(dirs[i])
  climfile <- list.files(".", pattern = ".csv")
  dat <- readLines(climfile)
  stn.id <- trimws(strsplit(dat[1], split = ":")[[1]][2])
  stn.height <- as.numeric(strsplit(trimws(strsplit(dat[2], split = ":")[[1]][2]), split = " ")[[1]][1])
  latlong <- as.numeric(strsplit(gsub("[[:alpha:,:punct:]]*", "", dat[3]), split = ",")[[1]])
  pos.blanks <- pos <- which(dat == "")
  print(paste0("Station id: ",stn.id))
  print(paste0("Lat: ",latlong[1], " Lon: ", latlong[2]))
  hrly.database$station[i-1] <- stn.id
  hrly.database$height[i-1] <- stn.height
  hrly.database$lat[i-1] <- latlong[1]
  hrly.database$long[i-1] <- latlong[2]
  hrly.database$folder[i-1] <- dirs[i]
  hrly.database$d[i-1] <- i
  
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
  dates <- as.POSIXct( climdat[,1], format = "%d-%b-%Y %H:%M")
  start <- as.character(as.Date(dates[1]))
  end <- as.character(as.Date(dates[length(dates)]))
  hrly.database$start[i-1] <- start
  hrly.database$end[i-1] <- end
  
  climdat[,1] <- dates
  dtrng <- range(dates, na.rm=TRUE)
  print(paste0("Date Range. From: ", dtrng[1], " To: ",dtrng[2] ))
  print("")
  attr(climdat, "units") <- unit.var
  attr(climdat, "variables") <- variables
  weather[[i-1]] <- climdat
  
  setwd(wrking.dir)
}
#Save data to an R object
saveRDS(list(stations = hrly.database, 
             weather = weather), 
        file = "hrldatabase.rds")
#Reset the working directory
setwd(cwd)


