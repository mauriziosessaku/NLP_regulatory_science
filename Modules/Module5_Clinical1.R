#############################################################################################
#Module 5_Clinical studies1
#Version 1.0 
#Last change: 08/01/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
   lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
   start <- stringr::str_which(lines, criteria2)
  if (length(start) == 1 ){
    test <- lines[start:(start+200)]
    res<-data.frame(str_detect(test,"2.1.4. Measures|3.1.4. Studies|3.5. Studies|2.1.4. Studies|Studies / Measures"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      test3<- test[res2:(res2+5)]
      res4<-data.frame(str_detect(test3,"PHARMACEUTICAL FORM"))
      colnames(res4)<-"Result"
      res4<-subset(res4,res4$Result==TRUE)
      res4<-as.numeric(row.names(res4)[1])
      if (is.na(res4)){
      test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Clinical"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
       a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
          res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,|3.2. Condition|2.2. Condition"))
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
        df3[i,9] <- a
       }  
    }
    }
    test <- lines[start:(start+500)]
    res<-data.frame(str_detect(test,"2.2.4. Measures|3.2.4. Studies|2.2.4. Studies"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
       res2<-as.numeric(row.names(res)[1])
       test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Clinical"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
          res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,|3.3. Condition"))
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
        df3[i,20] <- a
      }
    }
    test <- lines[start:(start+800)]
    res<-data.frame(str_detect(test,"2.3.4. Measures|3.3.4. Studies"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
      res<-data.frame(str_detect(test2,"Clinical"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      res<-as.numeric(row.names(res)[1])
      if (!is.na(res)){
        a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
        if ( str_detect(a,"Study")){
          res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,"))
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
        df3[i,31] <- a
      }
    }
  }else{
    start <- stringr::str_which(lines, "C.1. PAEDIATRIC |C. PAEDIATRIC")
    if (length(start) == 1 ){
      test <- lines[start:(start+200)]
      res<-data.frame(str_detect(test,"• Studies"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
        res<-data.frame(str_detect(test2,"Clinical"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
        res<-as.numeric(row.names(res)[1])
        if (!is.na(res)){
          a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
          if ( str_detect(a,"Study")){
            res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,"))
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
          df3[i,9] <- a
        }
      }
      start <- stringr::str_which(lines, "C.2. PAEDIATRIC")
      if (length(start) == 1 ){
        test <- lines[start:(start+200)]
        res<-data.frame(str_detect(test,"• Studies"))
        colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
        res<-data.frame(str_detect(test2,"Clinical"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
        res<-as.numeric(row.names(res)[1])
        if (!is.na(res)){
          a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
          if ( str_detect(a,"Study")){
            res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,"))
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
          df3[i,20] <- a
         }
      }
      start <- stringr::str_which(lines, "C.3. PAEDIATRIC")
      if (length(start) == 1 ){
        test <- lines[start:(start+200)]
        res<-data.frame(str_detect(test,"• Studies"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        test2<-str_split_fixed(test[res2:(res2+100)], " {9,}", 3)
        res<-data.frame(str_detect(test2,"Clinical"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
        res<-as.numeric(row.names(res)[1])
        if (!is.na(res)){
          a<- ifelse(is_empty(test2[res,3]), test2[res,2],test2[res,3]) 
          if ( str_detect(a,"Study")){
            res3<-data.frame(str_detect(test2,"Extrapolation,|Follow-up,"))
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
          df3[i,31] <- a
        }
      }
    }
   } 
   }
  }
}

setwd(Path1)

###########################################
# END
###########################################
