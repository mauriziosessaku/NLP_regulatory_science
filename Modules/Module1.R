#############################################################################################
#Module 1: Retrieve excel and links
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
path2<-paste("PIP", Sys.Date(), sep = "_")
dir.create(path2)
setwd(path2)

destfile <- paste(FileName,"_en.xlsx", sep = "")
curl::curl_download(url, destfile)
df <- read_excel(destfile, skip = 7)
setwd(Path1)
###########################################
# END
###########################################
