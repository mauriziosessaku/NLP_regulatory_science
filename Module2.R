#############################################################################################
#Module 2: Extract url from the webpage
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################

setwd(path2)

for(i in 1:N)   {  
  page <- read_html(df$URL[i])
  raw_list <- page %>% # takes the page above for which we've read the html
    html_nodes("a") %>%       # find all links in the page
    html_attr("href") %>%     # get the url for these links
    str_subset("\\.pdf") 
  dir <- paste0(basename(substr(raw_list,55,100)))
  a <- sub(".*/", "", df$URL[i])
  dir <- paste0(dir,"_",a,".pdf")
  ifelse(is.na(raw_list),print("Maurizio is sad"),download.file(raw_list, dir, mode="wb"))
}

setwd(Path1)
###########################################
# END
###########################################
