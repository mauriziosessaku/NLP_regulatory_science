#############################################################################################
#Module 4_Waiver_info
#Version 1.0 
#Last change: 12/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "1. Waiver")
  
  if (!length(start) == 1 ){
  start <- stringr::str_which(lines, "B. WAIVER")
  }
  if (length(start) == 1 ){
  test <- lines[start:(start+60)]
  res<-data.frame(str_detect(test,"waiver applies to:"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
 if (!is.na(res)){
   a<-test[res]
   for (jj in 1: 50){ 
     a2 <- str_trim( test[res+jj], "left")
     if (!is.na(a2)){
     if (a2 !=""){
       a<-paste0(a," ",a2)
     }
     }
     if (!is.na(test[res+jj+1])){
     if (str_detect(test[res+jj+1],"2. Paediatric Investigation Plan")){
       break
     }
       if (str_detect(test[res+jj+1],"C. PAEDIATRIC INVESTIGATION PLAN")){
         break
       }
     if (test[res+jj]=='' & test[res+jj+1]=='' & jj!=1){
       break
     }
   } 
  }
  a <- gsub("•  ", "", a)
  a <- str_trim(a, "right")
  a <- str_trim(a, "left")
  a2<-test[2]
  if (res>2){
  for (jjj in 3:(res-1)) {
    a2<-paste0(a2," ",test[jjj])
    a2 <- gsub("• ", "", a2)
    a2 <- str_trim(a2, "right")
    a2 <- str_trim(a2, "left")
  }
  }
  df2[i,17] <-paste0(a2," ",a)
}else {
  res<-data.frame(str_detect(test,"1.1. Condition"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    a<-test[res]
    for (jj in 1: 50){ 
      a2 <- str_trim( test[res+jj], "left")
      if (!is.na(a2)){
        if (a2 !=""){
          a<-paste0(a," ",a2)
        }
      }
      if (!is.na(test[res+jj+1])){
        if (str_detect(test[res+jj+1],"2. Paediatric Investigation Plan")){
          break
        }
        if (str_detect(test[res+jj+1],"C. PAEDIATRIC INVESTIGATION PLAN")){
          break
        }
        if (test[res+jj]=='' & test[res+jj+1]=='' & jj!=1){
          break
        }
      }
    }
    a <- gsub("•  ", "", a)
    a <- str_trim(a, "right")
    a <- str_trim(a, "left")  
    df2[i,17] <-a
  }
}
  }else {
    start <- stringr::str_which(lines, "waiver applies to: |The waiver applied by the applicant concerns: |waiver applied to: |The waiver applies to:")
    if (length(start) >= 1 ){
    for (j in 1:length(start)) {
      if (substr(lines[start[j]], nchar(lines[start[j]]), nchar(lines[start[j]]))==":"){
        test <- lines[start[j]:(start[j]+60)]
        res<-data.frame(str_detect(test,"waiver applies to:|The waiver applied by the applicant concerns:|waiver applied to:|The waiver applies to:"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
        res2<-as.numeric(row.names(res)[1])
        if (!is.na(res2)){
          a<-test[res2]
          for (jj in 1: 50){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a," ",a2)
            }
            if (!is.na(test[res2+jj+1])){
              if (str_detect(test[res2+jj+1],"2. Paediatric Investigation Plan")){
                break
              }
              if (str_detect(test[res2+jj+1],"C. PAEDIATRIC INVESTIGATION PLAN")){
                break
              }
              if (test[res2+jj]=='' & test[res2+jj+1]=='' & jj!=1){
                break
              }
            if (test[res2+jj]==''& test[res2+jj+1]=='' & jj!=1){
              break
            }
            }
          }
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          a2<-test[2]
          if (res2>2){
            for (jjj in 3:(res2-1)) { 
              a2<-paste0(a2," ",test[jjj])
            }
          }
          df2[i,17] <-paste0(a2," ",a)       
        }
        break
      }}
    }
   }
  }

setwd(Path1)

###########################################
# END
###########################################
