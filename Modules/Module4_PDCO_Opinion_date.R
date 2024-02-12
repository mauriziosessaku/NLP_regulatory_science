#############################################################################################
#Module 4_PDCO_Opinion_date
#Version 1.0 
#Last change: 12/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  res<-data.frame(str_detect(test,"opinion of the paediatric committee of the european medicines|opinion of the paediatric c ommittee of the european medicines"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    a1<-paste0(test[res]," ",test[res+1])
    if (str_detect(a1,"issued on")){
      a1<-sub(".*issued on","",a1)
    } else { if (str_detect(a1,"[s1]")){
      a1<-sub(".*]on","",a1)
    }}
    
    if (str_detect(a1,"formulated on")){
      a1<-sub(".*formulated on","",a1)} 
    if (str_detect(a1,"london")){
      a1<-sub(".*london,","",a1)} 
    a1<-sub(",.*","",a1)
  res <- str_split(a1," ", simplify = TRUE)[,2:4]
  a1<-paste0(res[1]," ",res[2]," ", res[3])
  df2[i,7]<- as.Date(dmy(a1))
  
  if (is.na(df2[i,7])) {
    a1<-stringr::str_replace(a1, "209", "2009")
    
    df2[i,7] <- as.Date(dmy(a1)) 
  }
  
  if (is.na(df2[i,7])) {
    
  res<-data.frame(str_detect(test,"procedure started on"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  a <- as.numeric(row.names(res))
  a <- test[a]
  a <- str_remove(a, "the procedure started on ")
  a <- removePunctuation(a)
  a <- trimws(a)
  a <- str_split(a," ", simplify = TRUE)
  a1<-stringr::str_replace(a1, "in", a[3])
  df2[i,7] <- as.Date(dmy(a1))
  }
  
  }  
}

setwd(Path1)

###########################################
# END
###########################################
