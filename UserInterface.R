
###########################################
#USER GUI - PIP Project
#Version 1.0
#Last change: 14/02/2024
#Authors: Saeed Shakibfar & Maurizio Sessa
###########################################
Path1 <-""

setwd(Path1)

#Module 0 - Libraries & functions
source("Codes/Modules/Module0.R")
path2<-paste("PIP", Sys.Date(), sep = "_")

#MODULE 1 - Retrieve excel and links
url <- "https://www.ema.europa.eu/system/files/documents/other/medicines_output_paediatric_investigation_plans_en.xlsx"
FileName <- "Medicines_output_paediatric_investigation_plans"
source("Codes/Modules/Module1.R")

#MODULE 2 - Extract url from the webpage
N <- nrow(df) 
path2<-""
source("Codes/Modules/Module2.R")


#MODULE 3 - Removing Image and Notification Files 
source("Codes/Modules/Module3_Removing.R")

#MODULE 3 - Read text from PDF 
source("Codes/Modules/Module3.R")


#MODULE 4 - Extract information:
#Module 4_Decision_number and EMA_Document_number and File Name
source("Codes/Modules/Module4_Decision_number_and_EMA_Document_number.R")

#Module 4_EMA_decision_date_and_Procedure_Start_date
source("Codes/Modules/Module4_EMA_decision_date_and_Procedure_Start_date.R")

#Module 4_PIP_Number
source("Codes/Modules/Module4_PIP_Number.R")

#Module 4_PDCO_Document_number
source("Codes/Modules/Module4_PDCO_Document_number.R")

#Module 4_PDCO_Opinion_date
source("Codes/Modules/Module4_PDCO_Opinion_date.R") 

#Module 4_Prior_Decision_number
source("Codes/Modules/Module4_Prior_Decision_number.R")

#Module 4_Prior_Decision_dates
source("Codes/Modules/Module4_Prior_Decision_dates.R")

#Module 4_active_substances
source("Codes/Modules/Module4_Active_substances.R")

#Module 4_Invented_name
source("Codes/Modules/Module4_Invented_name.R")

#Module 4_conditions
source("Codes/Modules/Module4_Conditions.R")

#Module 4_Pharmaceutical_forms
source("Codes/Modules/Module4_Pharmaceutical_forms.R")

#Module 4_Routes_administration
source("Codes/Modules/Module4_Routes_administration.R")

#Module 4_corporate_name
source("Codes/Modules/Module4_Corporate_name.R")

#Module 4_Waiver_info
source("Codes/Modules/Module4_Waiver_info.R")

#Module 4_PIP
source("Codes/Modules/Module4_PIP.R") 

#Module 4_Follow_up_concerns
source("Codes/Modules/Module4_Follow_up_concerns.R")

#Module 4_Date_of_completion
source("Codes/Modules/Module4_Date_of_completion.R") 

#Module 4_Deferral_for_measures
source("Codes/Modules/Module4_Deferral_for_measures.R")

#Module 4_Ema_number
#source("Codes/Modules/Module4_Ema_number.R")


#Saving
write_xlsx(df2,paste0("DataFrame_", path2,".xlsx"))


#For additional variables 
#Module 5_Conditions1
source("Codes/Modules/Module5_Conditions1.R") 

#Module 5_Ground1
source("Codes/Modules/Module5_Ground1.R") 

#Module 5_Indication1
source("Codes/Modules/Module5_Indication1.R") 

#Module 5_Subset1
source("Codes/Modules/Module5_Subset1.R") 

#Module 5_Pharmaceutical_form1
source("Codes/Modules/Module5_Pharmaceutical_form1.R") 

#Module 5_Quality1
source("Codes/Modules/Module5_Quality1.R") 

#Module 5_Non_clinical1
source("Codes/Modules/Module5_Non_clinical1.R") 

#Module 5_Clinical1
source("Codes/Modules/Module5_Clinical1.R") 

#Module 5_Extrapolation1
source("Codes/Modules/Module5_Extrapolation1.R") 

#Module 5_Other_studies1
source("Codes/Modules/Module5_Other_studies1.R")

#Module 5_Other_measures1
source("Codes/Modules/Module5_Other_measures1.R") 

#Module 5_ConditionWaiver
source("Codes/Modules/Module5_ConditionWaiver1.R") 

#Saving
write_xlsx(df3,paste0("Ex_DataFrame_", path2,".xlsx"))


#######################
#END
#######################
