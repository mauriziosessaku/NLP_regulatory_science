#############################################################################################
#Module 5_Indication1
#Version 1.0 
#Last change: 08/01/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "2. Paediatric|3. Paediatric Investigation Plan|C. PAEDIATRIC INVESTIGATION PLAN")
  if (length(start) == 1 ){
    test <- lines[start:(start+40)]
    res<-data.frame(str_detect(test,"2.1.1. Indication|3.1.1. Indication|3.2. Indication|• Proposed PIP indication"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
         res2<-as.numeric(row.names(res)[1])
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a," ",a2)
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
            if (str_detect(test[res2+jj+1],"2.1.2.|3.1.2.|3.3.|3.2.|2.1.4|• Subset")){
              break
            }
          }
          a<-stringi::stri_replace_first_fixed(a, "targeted by the PIP", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,4] <- a
    }
    test <- lines[start:(start+340)]
    res<-data.frame(str_detect(test,"2.2.1. Indication|3.2.1. Indication|2.1.3. Indication"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
        a<-test[res2]
        for (jj in 1: 10){ 
          a2 <- str_trim( test[res2+jj], "left")
          if (a2 !=""){
            a<-paste0(a," ",a2)
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
          if (str_detect(test[res2+jj+1],"2.2.2.|3.2.2.|2.1.4.")){
            break
          }
        }
        a<-stringi::stri_replace_first_fixed(a, "targeted by the PIP", "XXX")
        a<-sub(".*XXX","",a)
        a <- gsub("•  ", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,15] <- a
    }
    test <- lines[start:(start+640)]
    res<-data.frame(str_detect(test,"2.3.1. Indication|3.3.1. Indication"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
        a<-test[res2]
        for (jj in 1: 10){ 
          a2 <- str_trim( test[res2+jj], "left")
          if (a2 !=""){
            a<-paste0(a," ",a2)
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
          if (str_detect(test[res2+jj+1],"2.3.2.|3.3.2.|2.2.2.")){
            break
          }  
        }
        a<-stringi::stri_replace_first_fixed(a, "targeted by the PIP", "XXX")
        a<-sub(".*XXX","",a)
        a <- gsub("•  ", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,26] <- a
    }
  } else{
    start <- stringr::str_which(lines, "C.1. PAEDIATRIC|C. PAEDIATRIC")
    if (length(start) == 1 ){
      test <- lines[start:(start+40)]
      res<-data.frame(str_detect(test,"• Proposed PIP indication"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"•")){
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a," ",a2)
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
            if (str_detect(test[res2+jj+1],"•")){
              break 
            }
          }
          a<-stringi::stri_replace_first_fixed(a, "Proposed PIP indication", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,4] <- a 
        }
      }
      test <- lines[start:(start+340)]
      start <- stringr::str_which(lines, "C.2. PAEDIATRIC")
      if (length(start) == 1 ){
      test <- lines[start:(start+40)]
      res<-data.frame(str_detect(test,"• Proposed PIP indication"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"•")){
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a," ",a2)
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
            if (str_detect(test[res2+jj+1],"•")){
              break
            }
          }
          a<-stringi::stri_replace_first_fixed(a, "• Proposed PIP indication", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,15] <- a
        }
      }
      test <- lines[start:(start+640)]
      start <- stringr::str_which(lines, "C.3. PAEDIATRIC")
      if (length(start) == 1 ){
        test <- lines[start:(start+40)]
        res<-data.frame(str_detect(test,"• Proposed PIP indication"))
        colnames(res)<-"Result"
        res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"•")){
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a," ",a2)
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
            if (str_detect(test[res2+jj+1],"•")){
              break          
            }
          }
          a<-stringi::stri_replace_first_fixed(a, "• Proposed PIP indication", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,26] <- a
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
