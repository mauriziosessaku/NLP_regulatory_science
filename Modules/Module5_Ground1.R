#############################################################################################
#Module 5_Ground1
#Version 1.0 
#Last change: 08/01/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "1. Waiver|2. Waiver|1. GROUNDS")
 
  if (length(start) == 1 ){
    test <- lines[start:(start+50)]
    res<-data.frame(str_detect(test,"on the grounds "))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (!is.na(a2)){
            if (a2 !=""){
              a<-paste0(a,"; ",a2)
            }
            }
            if (is.na(test[res2+jj+1])){
              break
            }
            if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<50){
              break
            }
            if (test[res2+jj]==''& test[res2+jj+1]==''){
              break
            }
            if (str_detect(test[res2+jj+1],"2.|3.|Annex II")){
              break
            }
          }
          a <- gsub("•  ", "", a)
          a <-sub("\\s*and\\s*$", "", a)
          a <-sub("\\s*;\\s*$", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,3] <- a
    }
    if ( nrow(res) > 1 ){
      res2<-as.numeric(row.names(res)[2])
      a<-test[res2]
      for (jj in 1: 10){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
        }
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<50){
          break
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
        }
        if (str_detect(test[res2+jj+1],"2.|3.|Annex II")){
          break
        }
      }
      a <- gsub("•  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      df3[i,14] <- a
    }
    if ( nrow(res) > 2 ){
      res2<-as.numeric(row.names(res)[3])
      a<-test[res2]
      for (jj in 1: 10){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
        }
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<50){
          break
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
        }
        if (str_detect(test[res2+jj+1],"2.|3.|Annex II")){
          break
        }
      }
      a <- gsub("•  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      df3[i,25] <- a
    }
  } else{
    start <- stringr::str_which(lines, "B. WAIVER")
    if (length(start) == 1 ){
      test <- lines[start:(start+50)]
      res<-data.frame(str_detect(test,"on the grounds "))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        a<-test[res2]
        for (jj in 1: 10){ 
          a2 <- str_trim( test[res2+jj], "left")
          if (!is.na(a2)){
            if (a2 !=""){
              a<-paste0(a,"; ",a2)
            }
          }
          if (is.na(test[res2+jj+1])){
            break
          }
          if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))=="•" & nchar(test[res2+jj+1])<50){
            break
          }
          if (test[res2+jj]==''& test[res2+jj+1]==''){
            break
          }
          if (str_detect(test[res2+jj+1],"C.1.")){
            break
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,3] <- a
      }
      if ( nrow(res) > 1 ){
        res2<-as.numeric(row.names(res)[2])
        a<-test[res2]
        for (jj in 1: 10){ 
          a2 <- str_trim( test[res2+jj], "left")
          if (!is.na(a2)){
            if (a2 !=""){
              a<-paste0(a,"; ",a2)
            }
          }
          if (is.na(test[res2+jj+1])){
            break
          }
          if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))=="•" & nchar(test[res2+jj+1])<50){
            break
          }
          if (test[res2+jj]==''& test[res2+jj+1]==''){
            break
          }
          if (str_detect(test[res2+jj+1],"C.1")){
            break
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,14] <- a
      }
      if ( nrow(res) > 2 ){
        res2<-as.numeric(row.names(res)[3])
        a<-test[res2]
        for (jj in 1: 10){ 
          a2 <- str_trim( test[res2+jj], "left")
          if (!is.na(a2)){
            if (a2 !=""){
              a<-paste0(a,"; ",a2)
            }
          }
          if (is.na(test[res2+jj+1])){
            break
          }
          if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))=="•" & nchar(test[res2+jj+1])<50){
            break
          }
          if (test[res2+jj]==''& test[res2+jj+1]==''){
            break
          }
          if (str_detect(test[res2+jj+1],"C.1")){
            break
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,25] <- a
      }
    }
  }
}

setwd(Path1)

###########################################
# END
###########################################
