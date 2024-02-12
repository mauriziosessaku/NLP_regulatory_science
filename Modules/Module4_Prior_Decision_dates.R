#############################################################################################
#Module 4_Prior_Decision_dates
#Version 1.0 
#Last change: 12/04/2023
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
 
  res<-data.frame(str_detect(test,"Having regard to the European Medicines Agency"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    test2 <- test[res:(res+6)]
    res<-data.frame(str_detect(test2,"issued on"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res))
     if (length(res)>0){
      date_pattern <- "\\b\\d{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}\\b"
      test3<- test2[res[1]]
      for(j in 2:length(res))   {
        test3<- paste0( test3," ",test2[res[j]])
      }
      a <- str_extract_all( test3, date_pattern)[[1]]
      a <- paste(a, collapse = "; ")
      if ( !is.na(test2[res[j]+1])){
      if (! str_detect(test2[res[j]+1],"Having regard to the")){
        a1<-  str_extract_all( paste0( word(test2[res[j]],-2), " ",word(test2[res[j]],-1)," ",  test2[res[j]+1] ), date_pattern)[[1]]
        a1 <- paste(a1, collapse = "; ")
        a<- paste0(a,"; ", a1 )
      }
      }
     a <- trimws(a)
     a <- sub("^;", "", a)
     a <- sub(";$", "", a)
     a <- trimws(a)
    
     df2[i,9]<-a
    } 
  }else{
    res<-data.frame(str_detect(test,"Having regard to the decision P/"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
      test2 <- test[res:(res+6)]
      res<-data.frame(str_detect(test2,"on"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res))
     
      if (length(res)>0){
        date_pattern <- "\\b\\d{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}\\b"
        test3<- test2[res[1]]
        res<-data.frame(str_detect(test2,"issued on"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
        res<-as.numeric(row.names(res))
        
        if (length(res)>0){
        
        for(j in 2:length(res))   {
          test3<- paste0( test3," ",test2[res[j]])
        }
        }
        a <- str_extract_all( test3, date_pattern)[[1]]
        a <- paste(a, collapse = "; ")
        a <- trimws(a)
        a <- sub("^;", "", a)
        a <- sub(";$", "", a)
        a <- trimws(a)
        
        df2[i,9]<-a
      } 
    
    }
    }
  
}
setwd(Path1)
###########################################
# END
###########################################
