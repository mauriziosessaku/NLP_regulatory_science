#############################################################################################
#Module 5_Subset1
#Version 1.0 
#Last change: 08/01/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)


for(i in 1:nrow(wb))   {  

  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN")
  if (length(start) == 1 ){
    test <- lines[start:(start+60)]
    res<-data.frame(str_detect(test,"2.1.2. Subset|3.1.2. Subset|2.1.4. Subset|3.3. Subset|• Subset"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){  
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"s")){
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
            if (str_detect(test[res2+jj+1],"2.1.3.|3.1.3.|2.2.|3.4.|• Formulation")){
              break
            }
          }
          a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,5] <- a
        }
    }
    
    test <- lines[start:(start+360)]
    res<-data.frame(str_detect(test,"2.2.2. Subset|3.2.2. Subset|2.1.4. Subset"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      if ( str_detect(test[res2],"s")){
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
          if (str_detect(test[res2+jj+1],"2.2.3.|3.2.3.|2.3.|3.3.|2.2.")){
            break
          }
        }
        a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
        a<-sub(".*XXX","",a)
        a <- gsub("•  ", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,16] <- a
      }
    }
    
    test <- lines[start:(start+560)]
    res<-data.frame(str_detect(test,"2.3.2. Subset|3.3.2. Subset"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      res2<-as.numeric(row.names(res)[1])
      if ( str_detect(test[res2],"s")){
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
          if (str_detect(test[res2+jj+1],"2.3.3.|3.3.3.|2.3.")){
            break
          }
        }
        a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
        a<-sub(".*XXX","",a)
        a <- gsub("•  ", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,27] <- a
      }
    }
  } else{
    start <- stringr::str_which(lines, "C.1. PAEDIATRIC|C. PAEDIATRIC")
    if (length(start) == 1 ){
      test <- lines[start:(start+60)]
      res<-data.frame(str_detect(test,"• Subset"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"s")){
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
          a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,5] <- a
        }
      }
      
      test <- lines[start:(start+360)]
      start <- stringr::str_which(lines, "C.2. PAEDIATRIC")
      if (length(start) == 1 ){
      test <- lines[start:(start+60)]
      res<-data.frame(str_detect(test,"• Subset"))
      colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"s")){
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
          a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,16] <- a
        }
      }
        
      test <- lines[start:(start+560)]
      start <- stringr::str_which(lines, "C.3. PAEDIATRIC")
      if (length(start) == 1 ){
        test <- lines[start:(start+60)]
        res<-data.frame(str_detect(test,"• Subset"))
         colnames(res)<-"Result"
      res<-subset(res,res$Result==TRUE)
      if ( nrow(res) > 0 ){
        res2<-as.numeric(row.names(res)[1])
        if ( str_detect(test[res2],"s")){
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
          a<-stringi::stri_replace_first_fixed(a, "by the paediatric development", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          df3[i,27] <- a
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
