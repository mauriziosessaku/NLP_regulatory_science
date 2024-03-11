#############################################################################################
#Module 4_Date_of_completion
#Version 1.0 
#Last change: 04/03/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

months <- c("january", "february", "march", "april", "may", "june",
            "july", "august", "september", "october", "november", "december")
for(i in 1:nrow(wb))   {  
  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "Follow-up, completion and deferral of PIP|Follow-up, completion and deferral of pip")
  
  if (length(start) == 1 ){
    test <- lines[start:(start+20)]
    test2<-str_split_fixed(test, " {3,}", 3)
    res<-data.frame(str_detect(test,"Date of completion"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
      a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
      if (is_empty(a)){
        a<- ifelse(is_empty(test2[res+1,3]), test2[res+1,2],test2[res+1,3]) 
      }
      aa<- str_contains(tolower(a),months)
      aaa<- aa[aa == 'TRUE']
      if (length(aaa)==0){
       Tx1<-c(test2[res-1,2],test2[res-1,3],test2[res,2],test2[res,3],test2[res+1,2],test2[res+1,3])
        for (jjj in 1:length(Tx1)) {
          aa<-  str_contains(tolower(Tx1[jjj]),months)
          aa<- aa[aa == 'TRUE']
          if (length(aa) == 1 ) {
            a<- ifelse(jjj>4, paste0(a," ",Tx1[jjj]),paste0(Tx1[jjj]," ",a)) 
            break
          }
        } 
      }
      if (!grepl("[[:digit:]]", a)){
        Tx1<-c(test2[res-1,2],test2[res-1,3],test2[res,2],test2[res,3],test2[res+1,2],test2[res+1,3])
        for (jjj in 1:length(Tx1)) {
          if (grepl("[[:digit:]]", Tx1[jjj])) {
            a<- paste0(a," ",Tx1[jjj])
            break
          }
        }
      }
      a<-sub(".*By","",a)
      a <- removePunctuation(a)
      df2[i,20] <- a
    }
  }else {
    start <- stringr::str_which(lines, "Date of completion of the paediatric investigation plan:")
    if (length(start) == 1 ){
      test <- lines[start:(start+20)]
      test2<-str_split_fixed(test, " {3,}", 3)
      res<-data.frame(str_detect(test,"Date of completion"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if (is_empty(a)){
          a<- ifelse(is_empty(test2[res+1,3]), test2[res+1,2],test2[res+1,3]) 
        }
        aa<- str_contains(tolower(a),months)
        aaa<- aa[aa == 'TRUE']
        if (length(aaa)==0){
          Tx1<-c(test2[res-1,2],test2[res-1,3],test2[res,2],test2[res,3],test2[res+1,2],test2[res+1,3])
          for (jjj in 1:length(Tx1)) {
            aa<-  str_contains(tolower(Tx1[jjj]),months)
            aa<- aa[aa == 'TRUE']
            if (length(aa) == 1 ) {
              a<- ifelse(jjj>4, paste0(a," ",Tx1[jjj]),paste0(Tx1[jjj]," ",a)) 
              break
            }
          }
        }
        if (!grepl("[[:digit:]]", a)){
          Tx1<-c(test2[res-1,2],test2[res-1,3],test2[res,2],test2[res,3],test2[res+1,2],test2[res+1,3])
          for (jjj in 1:length(Tx1)) {
            if (grepl("[[:digit:]]", Tx1[jjj])) {
              a<- paste0(a," ",Tx1[jjj])
              break
            }
          }
        }
        a<-sub(".*By","",a)
        a <- removePunctuation(a)
        df2[i,20] <- a
      }
    }
  }
}
setwd(Path1)

###########################################
# END
###########################################
