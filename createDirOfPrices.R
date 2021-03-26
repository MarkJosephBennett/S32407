library(tseries)
source("acquirePrices.R")
source("acquireCoalescedPrices.R") #????needed
source("findAllPrices.R")
#required: ticker symbol files
if(!file.exists("NYSEclean.txt") || !file.exists("NASDAQclean.txt"))
  error("Need to download both NYSEclean.txt and NASDAQclean.txt from repository.")
  
split_path <- function(path) {
  setdiff(strsplit(path,"/|\\\\")[[1]], "")
}
#required: homeuser user path variable and .../FinAnalytics subdir
homeuser <- getwd()
subDir <- "FinAnalytics"
#check for and create .../FinAnalytics if not present
if (rev(split_path(homeuser))[1] == subDir){
  setwd(file.path(homeuser, subDir))
} else { #Need to create it
  dir.create(file.path(homeuser, subDir))
  setwd(file.path(homeuser, subDir))
  current <- homeuser
  new <- getwd()
  filelist <- list.files(current, "*clean.txt")
  # copy the 2 files to the new folder
  file.copy(paste0(current,"/",filelist[1]), paste0(new,"/",filelist[1]))
  file.copy(paste0(current,"/",filelist[2]), paste0(new,"/",filelist[2]))
}
#obtain entire market from start to end dates: daily adjClose
res <- findAllPrices(dir = "MVO3.2020.11",
                     start = "2017-11-28",
                     end   = "2020-11-29",needToAcquire=T)
