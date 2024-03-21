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
  a <- "https://www.ema.europa.eu"
  dir <- paste0(a,raw_list)
  fn = substr(dir,53,63)
  fn = paste0(df$`Decision number`[i],"_",fn,"_",i,".pdf")
  ifelse(is.na(raw_list),print("Maurizio is sad"),download.file(dir, fn, mode="wb"))
}

setwd(Path1)
###########################################
# END
###########################################
