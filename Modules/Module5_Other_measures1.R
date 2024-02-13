#############################################################################################
#Module 5_Other_measures1
#Version 1.0 
#Last change: 08/01/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################

setwd(path2)

for(i in 1:nrow(wb))   {  

  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "2. Paediatric")
  if (length(start) == 1 ){
    test <- lines[start:(start+200)]
    res<-data.frame(str_detect(test,"2.1.4. Measures"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Other measures"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
          res3<-data.frame(str_detect(test2,"3. Follow-up|2.2. Condition"))
          colnames(res3)<-"Result"
          res3<-subset(res3,res3$Result==TRUE)
          res3<-as.numeric(row.names(res3)[1])
          if (!is.na(res3)){
            for (jj in 1: (res3-res-1)){ 
              a2 <- str_trim( test2[res+jj,2], "left")
              if (a2 !=""){
                a<-paste0(a," ",a2)
              }
            }
          }
        }
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,12] <- a
      }
    }
    test <- lines[start:(start+500)]
    res<-data.frame(str_detect(test,"2.2.4. Measures"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Other measures"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
        res3<-data.frame(str_detect(test2,"3. Follow-up`|2.3. Condition"))
          colnames(res3)<-"Result"
          res3<-subset(res3,res3$Result==TRUE)
          res3<-as.numeric(row.names(res3)[1])
          if (!is.na(res3)){
            for (jj in 1: (res3-res-1)){ 
              a2 <- str_trim( test2[res+jj,2], "left")
              if (a2 !=""){
                a<-paste0(a," ",a2)
              }
            }
          }
        }
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,23] <- a
      }
    }
    test <- lines[start:(start+800)]
    res<-data.frame(str_detect(test,"2.3.4. Measures"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Other measures"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
          res3<-data.frame(str_detect(test2,"3. Follow-up"))
          colnames(res3)<-"Result"
          res3<-subset(res3,res3$Result==TRUE)
          res3<-as.numeric(row.names(res3)[1])
          if (!is.na(res3)){
            for (jj in 1: (res3-res-1)){ 
              a2 <- str_trim( test2[res+jj,2], "left")
              if (a2 !=""){
                a<-paste0(a," ",a2)
              }
            }
            
          }
        }
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,34] <- a
      }
    }
  }
} 

setwd(Path1)

###########################################
# END
###########################################
