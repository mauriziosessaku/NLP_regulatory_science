#############################################################################################
#Module 4_PIP_number
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  
  res<-data.frame(str_detect(test,"emea-"))
  #res<-data.frame(str_detect(test,"emea-| emea/"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    
    a <- str_split(test[res]," ", simplify = TRUE)
    for (j in  1:length(a)){ 
      
      if (str_detect(a[j],"emea")){
        a1<-sub(".*emea","",a[j])
          if (substr(a1, nchar(a1), nchar(a1))=="-"){
          a1<-paste0(a1,trimws(test[res+1]), sep = '')
          }
        a1<-sub(").*","",a1)
        a1<-sub(",.*","",a1)
          
          df2[i,4] <- paste0("emea",a1)
          }
    }
  }
}


setwd(Path1)

###########################################
# END
###########################################
