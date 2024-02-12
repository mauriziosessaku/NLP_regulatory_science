#############################################################################################
#Module 4_Deferral_for_measures
#Version 1.0 
#Last change: 12/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  

  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "Follow-up, completion and deferral of PIP|Follow-up, completion and deferral of pip")
  if (length(start) == 1 ){
    test <- lines[start:(start+20)]
    test2<-str_split_fixed(test, " {3,}", 3)
    res<-data.frame(str_detect(test,"Deferral for"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
      a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
      if (is_empty(a)){
        a<- ifelse(is_empty(test2[res+1,3]), test2[res+1,2],test2[res+1,3]) 
      }
      if (is_empty(a)){
        a<- ifelse(is_empty(test2[res-1,3]), test2[res-1,2],test2[res-1,3]) 
      }
      a <- removePunctuation(a)
      if(str_contains(a,c("Yes","No"), logic = "or")){
        df2[i,21] <- a
      }
    }
  }else {
    
    start <- stringr::str_which(lines, "Deferral for some or all studies contained in the paediatric investigation plan:")
    if (length(start) == 1 ){
      test <- lines[start:(start+20)]
      test2<-str_split_fixed(test, " {3,}", 3)
      res<-data.frame(str_detect(test,"Deferral for"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if (is_empty(a)){
          a<- ifelse(is_empty(test2[res+1,3]), test2[res+1,2],test2[res+1,3]) 
        }
        if (is_empty(a)){
          a<- ifelse(is_empty(test2[res-1,3]), test2[res-1,2],test2[res-1,3]) 
        }
        a <- removePunctuation(a)
        if(str_contains(a,c("Yes","No"), logic = "or")){
          df2[i,21] <- a
        }
      }
    }
  } 
}
setwd(Path1)

###########################################
# END
###########################################
