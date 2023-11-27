# Written by x x x
# November 2023.
# Adapted from March 2022 code by Catriona Haddow
# 
# *****************************************
#Purpose: Read in responses, restructure to be longer, add weights
#Create response rates file for tableau - not sample sizes.
# 

#Inputs:  #UPDATE!
#"lookups/questions.rds"
#"output/analysis_output/responses_with_categories.rds"
#"output/weights/weights_vars.rds"
#"lookups/question_lookup.rds"

#Outputs: #UPDATE!
#"output/analysis_output/responses_longer.rds"
#"output/temp/forms_completed_list.rds"

# load in libraries
library(tidyverse)
#version 1.3.2
library(haven)
#version 2.5.1
library(openxlsx)
#version 4.2.5.2

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324")
###set vector of report areas
report_areas <- c("scotland","gp_prac_no","practice_hscp_code","practice_board_code" ,"practice_hscp_cluster")
report_areas_output <- c("Scotland","GP","HSCP","Health Board" ,"GPCL")
###set vector of questions numbers
questions <- readRDS("lookups/questions.rds")

#read in results data####
responses <- readRDS("output/analysis_output/responses_with_categories.rds")
#add new variable for reporting at national level
responses$scotland = "Scotland"
table(responses$q09a,useNA = c("always"))
#add in variables to get 'tick all that apply' totals
#responses <- mutate(responses, tick_all_that_apply(tata))
responses <- responses %>%
  rowwise() %>%
  mutate(q09 = max(q09a,q09b,q09c,q09d,q09e,q09f),
         q14 = max(q14a,q14b,q14c,q14d,q14e,q14f),
         q20 = max(q20a,q20b,q20c,q20d,q20e,q20f),
         q27 = max(q27a,q27b,q27c,q27d,q27e,q27f,q27g,q27h),
         q28 = max(q28a,q28b,q28c,q28d),
         q32 = max(q32a,q32b,q32c,q32d,q32e,q32f,q32g,q32h,q32i),
         q34 = max(q34a,q34b,q34c,q34d,q34e,q34f),
         q35 = max(q35a,q35b,q35c,q35d,q35e,q35f,q35g),
         q39 = max(q39a,q39b,q39c,q39d,q39e,q39f,q39g,q39h,q39i,q39j,q39k))%>%
  mutate(q09a = ifelse(q09 == 1, q09a,NA),
         q09b = ifelse(q09 == 1, q09b,NA),
         q09c = ifelse(q09 == 1, q09c,NA),
         q09d = ifelse(q09 == 1, q09d,NA),
         q09e = ifelse(q09 == 1, q09e,NA),
         q09f = ifelse(q09 == 1, q09f,NA),
         q14a = ifelse(q14 == 1, q14a,NA),
         q14b = ifelse(q14 == 1, q14b,NA),
         q14c = ifelse(q14 == 1, q14c,NA),
         q14d = ifelse(q14 == 1, q14d,NA),
         q14e = ifelse(q14 == 1, q14e,NA),
         q14f = ifelse(q14 == 1, q14f,NA),
         q20a = ifelse(q20 == 1, q20a,NA),
         q20b = ifelse(q20 == 1, q20b,NA),
         q20c = ifelse(q20 == 1, q20c,NA),
         q20d = ifelse(q20 == 1, q20d,NA),
         q20e = ifelse(q20 == 1, q20e,NA),
         q20f = ifelse(q20 == 1, q20f,NA),
         q27a = ifelse(q27 == 1, q27a,NA),
         q27b = ifelse(q27 == 1, q27b,NA),
         q27c = ifelse(q27 == 1, q27c,NA),
         q27d = ifelse(q27 == 1, q27d,NA),
         q27e = ifelse(q27 == 1, q27e,NA),
         q27f = ifelse(q27 == 1, q27f,NA),
         q27g = ifelse(q27 == 1, q27g,NA),
         q27h = ifelse(q27 == 1, q27h,NA),
         q28a = ifelse(q28 == 1, q28a,NA),
         q28b = ifelse(q28 == 1, q28b,NA),
         q28c = ifelse(q28 == 1, q28c,NA),
         q28d = ifelse(q28 == 1, q28d,NA),
         q32a = ifelse(q32 == 1, q32a,NA),
         q32b = ifelse(q32 == 1, q32b,NA),
         q32c = ifelse(q32 == 1, q32c,NA),
         q32d = ifelse(q32 == 1, q32d,NA),
         q32e = ifelse(q32 == 1, q32e,NA),
         q32f = ifelse(q32 == 1, q32f,NA),
         q32g = ifelse(q32 == 1, q32g,NA),
         q32h = ifelse(q32 == 1, q32h,NA),
         q32i = ifelse(q32 == 1, q32i,NA),
         q34a = ifelse(q34 == 1, q34a,NA),
         q34b = ifelse(q34 == 1, q34b,NA),
         q34c = ifelse(q34 == 1, q34c,NA),
         q34d = ifelse(q34 == 1, q34d,NA),
         q34e = ifelse(q34 == 1, q34e,NA),
         q34f = ifelse(q34 == 1, q34f,NA),
         q35a = ifelse(q35 == 1, q35a,NA),
         q35b = ifelse(q35 == 1, q35b,NA),
         q35c = ifelse(q35 == 1, q35c,NA),
         q35d = ifelse(q35 == 1, q35d,NA),
         q35e = ifelse(q35 == 1, q35e,NA),
         q35f = ifelse(q35 == 1, q35f,NA),
         q35g = ifelse(q35 == 1, q35g,NA),
         q39a = ifelse(q39 == 1, q39a,NA),
         q39b = ifelse(q39 == 1, q39b,NA),
         q39c = ifelse(q39 == 1, q39c,NA),
         q39d = ifelse(q39 == 1, q39d,NA),
         q39e = ifelse(q39 == 1, q39e,NA),
         q39f = ifelse(q39 == 1, q39f,NA),
         q39g = ifelse(q39 == 1, q39g,NA),
         q39h = ifelse(q39 == 1, q39h,NA),
         q39i = ifelse(q39 == 1, q39i,NA),
         q39j = ifelse(q39 == 1, q39j,NA),
         q39k = ifelse(q39 == 1, q39k,NA))

#read in weights data####
weights_vars <- readRDS("output/weights/weights_vars.rds")

#joining by all id variables prevents duplication - that is, creation of patientid_sg.x for example
responses <- left_join(responses,select(weights_vars,-eligible_pats,-gp_wt1), by = c("patientid","QH_PSID","patientid_sg"))

#pivot longer####
responses_longer <- responses %>%
                    pivot_longer(all_of(questions),names_to = "question", values_to = "response_option")%>%
                    select("patientid",all_of(report_areas),ends_with('_wt'),question,response_option,eligible_pats)
responses_longer$response_option <- as.character(responses_longer$response_option)

#read in lookup to get weight category
question_lookup <- readRDS("lookups/question_lookup.rds")

table(question_lookup$response_option)
responses_longer <- left_join(responses_longer,select(question_lookup,question,response_option,weight),by = c("question","response_option"))

#select weight for each question####
responses_longer <- responses_longer%>%
                    mutate('nat_wt' = case_when(weight == "T1_Wt_Final" ~ nat_s1_wt,
                                               weight == "T2_Wt_Final" ~ nat_s2_wt,
                                               weight == "T3_Wt_Final" ~ nat_s3_wt,
                                               weight == "T4_Wt_Final" ~ nat_s4_wt,
                                               weight == "T5_Wt_Final" ~ nat_s5_wt,
                                               weight == "T6_Wt_Final" ~ nat_s6_wt,
                                               weight == "T7_Wt_Final" ~ nat_s7_wt,
                                               weight == "T8_Wt_Final" ~ nat_s8_wt,
                                               weight == "No_Weight" ~ 1),
                           'hb_wt' = case_when(weight == "T1_Wt_Final" ~ hb_s1_wt,
                                                weight == "T2_Wt_Final" ~ hb_s2_wt,
                                                weight == "T3_Wt_Final" ~ hb_s3_wt,
                                                weight == "T4_Wt_Final" ~ hb_s4_wt,
                                                weight == "T5_Wt_Final" ~ hb_s5_wt,
                                                weight == "T6_Wt_Final" ~ hb_s6_wt,
                                                weight == "T7_Wt_Final" ~ hb_s7_wt,
                                                weight == "T8_Wt_Final" ~ hb_s8_wt,
                                                weight == "No_Weight" ~ 1),
                           'hscp_wt' = case_when(weight == "T1_Wt_Final" ~ hscp_s1_wt,
                                                 weight == "T2_Wt_Final" ~ hscp_s2_wt,
                                                 weight == "T3_Wt_Final" ~ hscp_s3_wt,
                                                 weight == "T4_Wt_Final" ~ hscp_s4_wt,
                                                 weight == "T5_Wt_Final" ~ hscp_s5_wt,
                                                 weight == "T6_Wt_Final" ~ hscp_s6_wt,
                                                 weight == "T7_Wt_Final" ~ hscp_s7_wt,
                                                 weight == "T8_Wt_Final" ~ hscp_s8_wt,
                                                 weight == "No_Weight" ~ 1),
                           'gpcl_wt' = case_when(weight == "T1_Wt_Final" ~ gpcl_s1_wt,
                                                 weight == "T2_Wt_Final" ~ gpcl_s2_wt,
                                                 weight == "T3_Wt_Final" ~ gpcl_s3_wt,
                                                 weight == "T4_Wt_Final" ~ gpcl_s4_wt,
                                                 weight == "T5_Wt_Final" ~ gpcl_s5_wt,
                                                 weight == "T6_Wt_Final" ~ gpcl_s6_wt,
                                                 weight == "T7_Wt_Final" ~ gpcl_s7_wt,
                                                 weight == "T8_Wt_Final" ~ gpcl_s8_wt,
                                                 weight == "No_Weight" ~ 1),
                           'gp_wt' = case_when(weight == "T1_Wt_Final" ~ gp_s1_wt,
                                                 weight == "T2_Wt_Final" ~ gp_s2_wt,
                                                 weight == "T3_Wt_Final" ~ gp_s3_wt,
                                                 weight == "T4_Wt_Final" ~ gp_s4_wt,
                                                 weight == "T5_Wt_Final" ~ gp_s5_wt,
                                                 weight == "T6_Wt_Final" ~ gp_s6_wt,
                                                 weight == "T7_Wt_Final" ~ gp_s7_wt,
                                                 weight == "T8_Wt_Final" ~ gp_s8_wt,
                                                 weight == "No_Weight" ~ 1))
report_area_wt <- c("nat_wt","hb_wt","hscp_wt","gpcl_wt","gp_wt")

responses_longer <- responses_longer %>% select("patientid",all_of(report_areas),all_of(report_area_wt),question,response_option,eligible_pats)

#check if the same as before
hist.file <- readRDS("output/analysis_output/responses_longer.rds")
identical(hist.file,responses_longer)
#Won't be the same as is reference in lines 108 and 167 changed from patientid_sg to patientid

saveRDS(responses_longer, "output/analysis_output/responses_longer.rds")

#calculate response rate = completed form count / sample size ####
forms_completed_list <- lapply(report_areas, function(x) {
  x <- responses %>% group_by_at(x) %>% summarise(forms_completed = n())})

forms_completed_list <- lapply(seq_along(report_areas), function(x) {
  forms_completed_list[[x]][3] <- names(forms_completed_list[[x]])[1]
  names(forms_completed_list[[x]])[1] <- "report_area"
  names(forms_completed_list[[x]])[3] <- "level"
  forms_completed_list[[x]]
})

forms_completed_list <- bind_rows(forms_completed_list)
forms_completed_list$level <- str_replace_all(forms_completed_list$level, setNames(report_areas_output, report_areas))

#check if the same as before
hist.file <- readRDS("output/temp/forms_completed_list.rds")
identical(hist.file,forms_completed_list)

file.remove("output/temp/forms_completed_list.rds")
saveRDS(forms_completed_list, "output/temp/forms_completed_list.rds")