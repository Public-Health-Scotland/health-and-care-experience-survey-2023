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

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results data####
responses <- readRDS(paste0(data_path,"responses_with_categories.rds"))

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
  mutate(across(q09a:q09f,~if_else(q09 == 1,.x,NA)),
         across(q14a:q14f,~if_else(q14 == 1,.x,NA)),
         across(q20a:q20f,~if_else(q20 == 1,.x,NA)),
         across(q27a:q27h,~if_else(q27 == 1,.x,NA)),
         across(q28a:q28d,~if_else(q28 == 1,.x,NA)),
         across(q32a:q32i,~if_else(q32 == 1,.x,NA)),
         across(q34a:q34f,~if_else(q34 == 1,.x,NA)),
         across(q35a:q35g,~if_else(q35 == 1,.x,NA)),
         across(q39a:q39k,~if_else(q39 == 1,.x,NA)))
         

#read in weights data####
weights_vars <- readRDS(data_path,"weights_vars.rds")

#joining by all id variables prevents duplication - that is, creation of patientid_sg.x for example
responses <- left_join(responses,select(weights_vars,-eligible_pats,-gp_wt1), by = c("patientid","QH_PSID","patientid_sg"))

#pivot longer####
responses_longer <- responses %>%
                    pivot_longer(all_of(questions),names_to = "question", values_to = "response_option")%>%
                    select("patientid",all_of(report_areas),ends_with('_wt'),question,response_option,eligible_pats)
responses_longer$response_option <- as.character(responses_longer$response_option)

#read in lookup to get weight category
question_lookup <- readRDS(lookup_path,"question_lookup.rds")

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

responses_longer <- responses_longer %>% select("patientid",all_of(report_areas),all_of(report_area_wt),question,response_option,eligible_pats)

#check if the same as before
hist.file <- readRDS(paste0(data_path,"responses_longer.rds"))
identical(hist.file,responses_longer)
#Won't be the same as is reference in lines 108 and 167 changed from patientid_sg to patientid

saveRDS(responses_longer, paste0(data_path,"responses_longer.rds"))

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
hist.file <- readRDS(paste0(data_path,"forms_completed_list.rds"))
identical(hist.file,forms_completed_list)

file.remove(paste0(data_path,"forms_completed_list.rds"))
saveRDS(forms_completed_list, paste0(data_path,"forms_completed_list.rds"))
