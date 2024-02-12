#############################################################################################
#Module 4_PIP
#Version 1.0 
#Last change: 12/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "2. Paediatric|3. Paediatric|C.1. PAEDIATRIC|C. PAEDIATRIC")
  if (length(start) == 1 ){
    test <- lines[start:(start+200)]
    res<-data.frame(str_detect(test,"Paediatric investigation plan|Paediatric Investigation Plan"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
      a<-test[res]
      for (jj in 1: 90){ 
        a2 <- str_trim( test[res+jj], "left")
        if (!is.na(a2)){
          if (a2 !=""){
            a<-paste0(a," ",a2)
          }
        }
        if (!is.na(test[res+jj+1])){
          if (str_detect(test[res+jj+1],"3. Follow-up")){
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
        }
      }
      df2[i,18] <-paste0(a2," ",a)
    }
  }
}

setwd(Path1)

###########################################
# END
###########################################
