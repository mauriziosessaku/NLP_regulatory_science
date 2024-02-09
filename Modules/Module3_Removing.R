#############################################################################################
#Module Removing Image and Notification Files
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
path3<-paste(path2,"Notification", sep = "_")
path5<-paste(path2,"Image", sep = "_")
path7<-paste(path2,"Summary", sep = "_")
path9<-paste(path2,"WaterMarked", sep = "_")

dir.create(path3)
dir.create(path5)
dir.create(path7)
dir.create(path9)

path4<-paste(Path1,path3, sep = "/")
path6<-paste(Path1,path5, sep = "/")
path8<-paste(Path1,path7, sep = "/")
path10<-paste(Path1,path9, sep = "/")

setwd(path2)


data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)
 
for(i in 1:nrow(wb))   {
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test2<-unlist(test)
    
    res<-data.frame(str_detect(test2,"To:|Re:"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    res<-row.names(res)[1]
    a <- as.numeric(res)
    if (!is.na(res)){
    if(a<25){
      
      
      file.copy(wb[i,], path4)
      file.remove(wb[i,])
      
    }}
  
    
    }
  




data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)

for(i in 1:nrow(wb))   {
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test2<-unlist(test)
  test2<-tolower(test2)
  res<-data.frame(str_detect(test2,"summary"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  res<-row.names(res)[1]
  a <- as.numeric(res)
  if (!is.na(res)){
    if(a<10){
      
      
      file.copy(wb[i,], path8)
      file.remove(wb[i,])
      
    }}
  
  
}



data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)

for(i in 1:nrow(wb))   {
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test2<-unlist(test)
  test2<-tolower(test2)
  
  res<-data.frame(str_detect(test2," lo"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  
  if (nrow(res)>0){
    for (j in nrow(res):1) {
      a<-row.names(res)[j]
     a <- as.numeric(a)
     
         
     
if  (str_detect(test2[a-1]," ng")){
      
      
      file.copy(wb[i,], path10)
      file.remove(wb[i,])
      break
      
}
     
      }
  }
  
  
}


data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)


for(i in 1:nrow(wb))   {  
  
  test <- pdf_text (wb[i,]) %>% strsplit(split = "\n")
  test2<-unlist(test)
  
  for (j in 1: length(test)){
    if( is_empty(test[[j]])){
      file.copy(wb[i,], path6)
      file.remove(wb[i,])
    }
    
  }
  
  
  res<-data.frame(str_detect(test,"European Medicines Agency"))
  colnames(res)<-"Result"
  res<-subset(res,res$Result==TRUE)
  if (nrow(res)<1){
    file.copy(wb[i,], path6)
    file.remove(wb[i,])
  }
    
}


data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)


setwd(Path1)
###########################################
# END
###########################################
