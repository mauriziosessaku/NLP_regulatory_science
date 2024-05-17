#############################################################################################
#Module 5_ConditionWaiver1AndGround
#Version 1.0 
#Last change: 18/05/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################
setwd(path2)

for(i in 1:nrow(wb))   {  
  
  lines <- unlist(stringr::str_split(pdftools::pdf_text(wb[i,]), "\n"))
  # 
  start <- stringr::str_which(lines, "1. Waiver|2. Waiver|1. GROUNDS |B. WAIVER")
  
  if (length(start) == 1 ){
    
    test <- lines[start:(start+50)]
    res<-data.frame(str_detect(test,"1.1. Condition:|1.1. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.2. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      df3[i,35] <- a
      
      
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.2. Condition:")){
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
    }
    
    test <- lines[start:(start+70)]
    res<-data.frame(str_detect(test,"1.2. Condition:|1.2. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.3. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      df3[i,36] <- a
      
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.3. Condition:")){
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
    }
    
    test <- lines[start:(start+70)]
    res<-data.frame(str_detect(test,"1.3. Condition:|1.3. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.4. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      
      df3[i,37] <- a
      
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.4. Condition:")){
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
    test <- lines[start:(start+70)]
    res<-data.frame(str_detect(test,"1.4. Condition:|1.4. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.5. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      
      df3[i,38] <- a
      
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.5. Condition:")){
            break
            
            
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,41] <- a
      } 
    }
    test <- lines[start:(start+70)]
    res<-data.frame(str_detect(test,"1.5. Condition:|1.5. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.6. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      
      df3[i,39] <- a
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.6. Condition:")){
            break
            
            
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,42] <- a
      } 
    }
    test <- lines[start:(start+70)]
    res<-data.frame(str_detect(test,"1.6. Condition:|1.6. Condition"))
    colnames(res)<-"Result"
    res<-subset(res,res$Result==TRUE)
    if ( nrow(res) > 0 ){
      
      
      res2<-as.numeric(row.names(res)[1])
      a<-test[res2]
      for (jj in 1: 20){ 
        a2 <- str_trim( test[res2+jj], "left")
        if (!is.na(a2)){
          
          
          
          if (a2 !=""){
            a<-paste0(a,"; ",a2)
          }
        }
        if (is.na(test[res2+jj+1])){
          break
          
        }
        
        
        if (substr(test[res2+jj+1], nchar(test[res2+jj+1]), nchar(test[res2+jj+1]))==":" & nchar(test[res2+jj+1])<20){
          break
          
        }
        if (test[res2+jj]==''& test[res2+jj+1]==''){
          break
          
        }
        if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.7. Condition:")){
          break
          
          
        }
      }
      a <- gsub("•  ", "", a)
      a <- gsub("  ", "", a)
      a <-sub("\\s*and\\s*$", "", a)
      a <-sub("\\s*;\\s*$", "", a)
      a <- str_trim(a, "right")
      a <- str_trim(a, "left")
      
      df3[i,40] <- a
      
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
          if (str_detect(test[res2+jj+1],"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|C.1. PAEDIATRIC |C. PAEDIATRIC|Annex II|1.7. Condition:")){
            break
            
            
          }
        }
        a <- gsub("•  ", "", a)
        a <-sub("\\s*and\\s*$", "", a)
        a <-sub("\\s*;\\s*$", "", a)
        a <- str_trim(a, "right")
        a <- str_trim(a, "left")
        df3[i,43] <- a
    }  
    }
  }
}


setwd(Path1)

###########################################
# END
###########################################

