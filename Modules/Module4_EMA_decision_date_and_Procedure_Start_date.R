#############################################################################################
#Module 4_EMA_decision date_and_Procedure_Start_date
#Version 1.0 
#Last change: 12/04/2023
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)


months <- c("january", "february", "march", "april", "may", "june",
            "july", "august", "september", "october", "november", "december")

for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  
  
  res<-data.frame(str_detect(test,"procedure started on"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  a <- as.numeric(row.names(res))
  a <- test[a]
  a <- str_remove(a, "the procedure started on ")
  a <- removePunctuation(a)
  a <- trimws(a)
  a1 <- dmy(a)
  a1 <- a1[1]
  ifelse(is.na(a), df2[i,6] <- NA ,df2[i,6] <- as.character(a1))
  
  
  if (is.na(df2[i,6])) {
  
    a<-stringr::str_replace(a, "spetember", "september")
    a1 <- dmy(a)
    a1 <- a1[1]
    ifelse(is.na(a), df2[i,6] <- NA ,df2[i,6] <- as.character(a1))
    
  }
  
  if (is.na(df2[i,6])) {
    
    a<-stringr::str_replace(a, "20120", "2020")
    a1 <- dmy(a)
    a1 <- a1[1]
    ifelse(is.na(a), df2[i,6] <- NA ,df2[i,6] <- as.character(a1))
    
  }

  
  
  a1 <- data.frame(test[4:15])
  
  
  for (jj in 1:12){
    aa<-  str_contains(a1[jj, 1],months)
    aa<- aa[aa == 'TRUE']
    
    if (length(aa) == 1 ) {
     
      df2[i,3] <- as.Date(dmy(test[jj+3]))
      break
    }
  }
  if (is.na(df2[i,3])) {
    a1 <- data.frame(test[16:27])
    for (jj in 1:12){
      aa<-  str_contains(a1[jj, 1],months)
      aa<- aa[aa == 'TRUE']
      
      if (length(aa) == 1 ) {
        
        df2[i,3] <- as.Date(dmy(test[jj+15]))
        break
      }
    }
  }
  if (is.na(df2[i,3])) {
    a1 <- test[4:15]
    
    
    res<-data.frame(str_detect(a1,"of"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    a <- as.numeric(row.names(res))[1]
    a <- a1[a]
    a <- str_remove(a, "of")
    a <- dmy(a)
    a <- a[1]
    df2[i,3] <- a
    
  
  }
}


setwd(Path1)

###########################################
# END
###########################################
  
