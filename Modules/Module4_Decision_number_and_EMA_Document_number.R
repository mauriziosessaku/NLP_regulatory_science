#############################################################################################
#Module 4_Decision_number and EMA_Document_number
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)


for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  
  res<-data.frame(str_detect(test,"p/"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-row.names(res)[1]
  if (!is.na(res)){
    
    a <- as.numeric(res)
    
   
    
    
    a<-sub(".*p","",test[a])
      
     
        
        df2[i,2] <- str_trim( paste0("P",a), "left") 
        
      
    }
    

  
  res<-data.frame(str_detect(test[1:20],"emea/"))
  
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
        
        
  df2[i,1]<- paste0("emea",a1)
  
  df3[i,1]<- paste0("emea",a1)
  
      }
      }
  } else {
      
    res<-data.frame(str_detect(test[1:10],"ema/"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    
    if (!is.na(res)){
      #a <- str_split(test[res]," ", simplify = TRUE)
      #a<- a[1]  
      a<-  test[res]
      a<-sub(".*ema/","",a)
      a<-sub(").*","",a)
      df2[i,1] <- paste0("ema/",a)
      #df3[i,1]<- paste0("emea",a)
      df3[i,1]<- paste0("ema/",a)
    }
    
    }
  df2[i,10]<- wb[i,]
  
}


setwd(Path1)

###########################################
# END
###########################################
