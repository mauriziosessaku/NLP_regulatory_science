#############################################################################################
#Module 4_PDCO_Document_number
#Version 1.0 
#Last change: 12/04/2023
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  

  res<-data.frame(str_detect(test,"pdco/"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    a <- str_split(test[res]," ", simplify = TRUE)
    for (j in  1:length(a)){ 
      
      if (str_detect(a[j],"pdco")){
         a1<-sub(".*pdco/","",a[j])
         a1<-sub(").*","",a1)
         a1<-sub(".*pdco/","",a1)

        df2[i,5] <- paste0("PDCO/",a1)
      }
    }
 
  }else {
  
    res<-data.frame(str_detect(test,"/pdco"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
    a <- str_split(test[res]," ", simplify = TRUE)
    for (j in  1:length(a)){ 
      a1<-sub(".*/pdco","",a[j])
      if (str_detect(a[j],"pdco")){
        a1<-sub(").*","",a1)
        a1<-sub(".*/pdco","",a1)
        
        df2[i,5] <- paste0("PDCO/",a1)
      }
    }
   }
  }
}


setwd(Path1)

###########################################
# END
###########################################
