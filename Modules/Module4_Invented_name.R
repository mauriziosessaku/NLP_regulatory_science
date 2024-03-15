#############################################################################################
#Module 4_Invented name
#Version 1.0 
#Last change: 12/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  

  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  start <- stringr::str_which(lines, "Scope of the application")
  if (length(start) == 1 ){
    test <- lines[start:(start+40)]
    res<-data.frame(str_detect(test,"Invented"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      for (j in  nrow(res):1){ 
        res2<-as.numeric(row.names(res)[j])
        if (str_detect(test[res2],":")){
          a<-test[res2]
          for (jj in 1: 10){ 
            a2 <- str_trim( test[res2+jj], "left")
            if (a2 !=""){
              a<-paste0(a,"; ",a2)
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
          }
          a<-stringi::stri_replace_first_fixed(a, ":;", "XXX")
          a<-sub(".*XXX","",a)
          a <- gsub("•  ", "", a)
          a <- str_trim(a, "right")
          a <- str_trim(a, "left")
          if (a !="See Annex II"){ 
          df2[i,12] <- a
          }
        }
        break
      }
    }
  } 
}

setwd(Path1)

###########################################
# END
###########################################
