#############################################################################################
#Module 3: Defining variables and read PDFs
#Version 1.0 
#Last change: 09/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
#############################################################################################

setwd(path2)

data.files = list.files()
wb <- list.files(pattern = "*.pdf")
wb <- as.data.frame(wb)

df2 <- data.frame(EMA_Document_number=character(),
                  Decision_number=character(),
                  EMA_Decision_date=as.Date(character()),
                  PIP_number=character(),
                  PDCO_Document_number=character(),
                  Procedure_Start_date=character(),
                  PDCO_Opinion_date=as.Date(character()),
                  Prior_Decision_number=character(),
                  Prior_Decision_dates=character(),
                  File_name=character(),
                  Active_substances=character(),
                  Invented_name=character(),
                  Conditions=character(),
                  Pharmaceutical_forms=character(),
                  Routes_administration=character(),
                  Corporate_name=character(),
                  Waiver_info=character(),
                  PIP=character(),
                  Follow_up_concerns=character(),
                  Date_of_completion=character(),
                  Deferral_for_measures=character(),
                  stringsAsFactors=FALSE)


df3 <- data.frame(EMA_Document_number=character(), 
                  Conditions1=character(),
                  Ground1=character(),
                  Indication1=character(),
                  Subset1=character(),
                  Pharmaceutical_form1=character(),
                  Quality1	=character(),
                  Non_clinical1=character(),	
                  Clinical1=character(),
                  Extrapolation1=character(),	
                  Other_studies1	=character(),
                  Other_measures1=character(),
                  Conditions2=character(),
                  Ground2=character(),
                  Indication2=character(),
                  Subset2=character(),
                  Pharmaceutical_form2=character(),
                  Quality2=character(),
                  Non_clinical2=character(),	
                  Clinical2=character(),
                  Extrapolation2=character(),	
                  Other_studies2=character(),
                  Other_measures2=character(),
                  Conditions3=character(),
                  Ground3=character(),
                  Indication3=character(),
                  Subset3=character(),
                  Pharmaceutical_form3=character(),
                  Quality3=character(),
                  Non_clinical3=character(),	
                  Clinical3=character(),
                  Extrapolation3=character(),	
                  Other_studies3=character(),
                  Other_measures3=character(),
                  ConditionWaiver1=character(),
                  ConditionWaiver2=character(),
                  ConditionWaiver3=character(),
                  ConditionWaiver4=character(),
                  ConditionWaiver5=character(),
                  ConditionWaiver6=character(),
                  ConditionWaiver7=character(),
                  ConditionWaiver8=character(),
                  ConditionWaiver9=character(),
                  stringsAsFactors=FALSE) 


criteria1<-"2. Paediatric|3. Paediatric|C.1. PAEDIATRIC|C. PAEDIATRIC|1. Paediatric investigation plan"
criteria2<-"2. Paediatric|3. Paediatric|C. PAEDIATRIC INVESTIGATION PLAN|1. Paediatric investigation plan"


setwd(Path1)
###########################################
# END
###########################################
