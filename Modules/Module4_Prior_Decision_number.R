#############################################################################################
#Module 4_Prior_Decision_number
#Version 1.0 
#Last change: 12/04/2023
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  

  if  (!is.na(df2$PIP_number[i]) && str_detect(df2$PIP_number[i],"-m")){
    
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test<-unlist(test)
  test<-tolower(test)
  
  res<-data.frame(str_detect(test,"decision p/"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-as.numeric(row.names(res)[1])
  if (!is.na(res)){
    test2 <- test[res:(res+10)]
    V1<-qdapRegex::ex_between(test2, "p/", "issued" ) 
    V1<-as.data.frame( unlist(V1))
    V1<- na.omit(V1)
    a<- paste0("p/",V1[1,])
      if (nrow(V1) > 1){
    for (j in 2:nrow(V1)) {
      a<- paste0(a,"; p/",V1[j,] )
     }
      }
    if (is.na(V1[1,])){
        V1<-qdapRegex::ex_between(test2, "p/", "on" )
        V1<-as.data.frame( unlist(V1))
        V1<- na.omit(V1)
        a<- paste0("P/",sub("of .*", "", V1[1,]))
        if (nrow(V1) > 1){
          for (j in 2:nrow(V1)) {
            a<- paste0(a,"; P/",sub("of .*", "", V1[j,]) )
          }
        }
      }
    df2[i,8]<- a
  }else{
    res<-data.frame(str_detect(test,"having regard to the european medicines agency’s p/"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-as.numeric(row.names(res)[1])
    if (!is.na(res)){
      test2 <- test[res:(res+10)]
      V1<-qdapRegex::ex_between(test2, "p/", "issued" )
      V1<-as.data.frame( unlist(V1))
      V1<- na.omit(V1)
      a<- paste0("P/",V1[1,])
      if (nrow(V1) > 1){
        for (j in 2:nrow(V1)) {
          a<- paste0(a,"; P/",V1[j,] )
        }
      }
  }
  df2[i,8]<- a
  }
  }
}

setwd(Path1)

###########################################
# END
###########################################


