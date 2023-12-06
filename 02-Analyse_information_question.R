# Written by x x x
# November 2023.
# Adapted from February 2022 code by Catriona Haddow
# 
# *****************************************
#Purpose: Analyse information questions. Read in responses, response rates and output analyses at all levels of reporting.

#Inputs: 
#"lookups/information_questions.rds"
#"lookups/information_questions_tata.rds"
#"output/analysis_output/responses_longer.rds"
#"lookups/question_lookup_info.rds"
#"lookups/Final_Practice_lookup.rds"
#"output/temp/output/temp/sample_size_list_net_of_deaths.rds"
#"output/temp/forms_completed_list.rds"
#"/conf/bss/pat-exp-surveys/health-and-care/201920/Tableau/Datafiles/info_questions.sav"

#Outputs: 
##"output/analysis_output/info_questions_sg.rds"
##"output/analysis_output/info_questions_sg.xlsx"
##"output/analysis_output/info_output_full.rds")


#Note: 28/04/2022 updated after production of the info_questions_sg to create a fuller version of the output including more variables. info_output_full

# load in libraries
library(tidyverse)
#version 1.3.1
library(haven)
#version 2.5.1
library(openxlsx)
#version 4.2.5.2

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324")
###set vector of report areas
report_areas <- c("scotland","gp_prac_no","practice_hscp_code","practice_board_code" ,"practice_hscp_cluster")
report_areas_output <- c("Scotland","GP","HSCP","Health Board" ,"GPCL")
###set vector of information question numbers
information_questions <- readRDS("lookups/information_questions.rds")
information_questions_tata <- readRDS("lookups/information_questions_tata.rds")
#read in results longer data####
responses_longer <- readRDS("output/analysis_output/responses_longer.rds")

#select information questions only
responses_longer <- responses_longer %>% filter(question %in% information_questions)

#read in question_lookup_info
question_lookup_info <- readRDS("lookups/question_lookup_info.rds")

#define the aggregate information questions function.####
aggregate_info <- function(report_areas,wt) {
  responses_longer <- responses_longer %>%
    group_by("report_area" = {{report_areas}},question,response_option)%>%
    summarise(n_response = sum(!is.na(response_option)),
              n_wgt_response = sum({{wt}}),.groups = "keep")}

#define the expand table function.####
#Create an index of the required rows, then use to create a master table to ensure all possible option combinations exist
expand_table <- function(df) {
idx <- rep(1:nrow(question_lookup_info), length(unique(df$report_area)))
expand.table <-  question_lookup_info[idx,]
expand.table <- expand.table %>% select(-weight)%>%
          mutate(report_area = rep(unique(df$report_area),nrow(question_lookup_info)))%>%
        left_join(df,by = c("report_area","question","response_option"))
}

#run at each level####
df <- aggregate_info(scotland,nat_wt)
nat_info <- expand_table(df)
nat_info$level <- "Scotland"

df <- aggregate_info(practice_board_code,hb_wt)
hb_info <- expand_table(df)
hb_info$level <- "Health Board"

df <- aggregate_info(practice_hscp_code,hscp_wt)
hscp_info <- expand_table(df)
hscp_info$level <- "HSCP"

df <- aggregate_info(practice_hscp_cluster,gpcl_wt)
gpcl_info <- expand_table(df)
gpcl_info$level <- "GPCL"

df <- aggregate_info(gp_prac_no,gp_wt)
gp_info <- expand_table(df)
gp_info$level <- "GP"

#from here, could create the results output using spread.

info_output <- bind_rows(nat_info,hb_info,hscp_info,gpcl_info,gp_info)
info_output[c("n_response", "n_wgt_response")][is.na(info_output[c("n_response", "n_wgt_response")])] <- 0

info_output <- info_output %>%
    group_by(report_area,question) %>%
      mutate(n_includedresponses = sum(n_response),
                n_wgt_includedresponses = sum(n_wgt_response)) %>%
      ungroup() %>%
      mutate(percent_response = n_response / n_includedresponses * 100,
           wgt_percent_response= n_wgt_response / n_wgt_includedresponses * 100)

mutate(q09 = max(q09a,q09b,q09c,q09d,q09e,q09f),
       q14 = max(q14a,q14b,q14c,q14d,q14e,q14f),
       q20 = max(q20a,q20b,q20c,q20d,q20e,q20f),
       q27 = max(q27a,q27b,q27c,q27d,q27e,q27f,q27g,q27h),
       q28 = max(q28a,q28b,q28c,q28d),
       q32 = max(q32a,q32b,q32c,q32d,q32e,q32f,q32g,q32h,q32i),
       q34 = max(q34a,q34b,q34c,q34d,q34e,q34f),
       q35 = max(q35a,q35b,q35c,q35d,q35e,q35f,q35g),
       q39 = max(q39a,q39b,q39c,q39d,q39e,q39f,q39g,q39h,q39i,q39j,q39k))%>%

#Code to deal with 'tick all that apply' questions.
#Removes the "No" response to the "tick all that apply" questions q09a-f, q14a-f, q20a-f, q27a-h, q28a-d, q32a-i, q34a-f, q35a-g, a39a-k####

#New code to remove the "No" response to the "tick all that apply" questions q09a-f, q14a-f, q20a-f, q27a-h, q28a-d, q32a-i, q34a-f, q35a-g, a39a-k####

info_output <- info_output %>%
                mutate(retain = if_else(substr(question,1,3) %in% information_questions_tata
                                        & response_option == 0,0,1 ))%>%
                filter (retain == 1)%>%
                select(-retain)

#add on report_area names
#read in Practice lookup, sample size, list completed
practice_lookup <- readRDS("lookups/Final_Practice_lookup.rds")

info_output <- left_join(info_output,distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no"))
info_output <- left_join(info_output,distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))
info_output <- left_join(info_output,distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))

info_output$report_area_name <- "Blank"
info_output$report_area_name[info_output$level == "GP"] <- paste0(info_output$practice_name_letter[info_output$level == "GP"]," (",info_output$report_area[info_output$level == "GP"],")")
info_output$report_area_name[info_output$level == "Health Board"] <- paste0(info_output$practice_board_name[info_output$level == "Health Board"])
info_output$report_area_name[info_output$level == "Health Board"] <- gsub(" and "," & ",info_output$report_area_name[info_output$level == "Health Board"])
info_output$report_area_name[info_output$level == "HSCP"] <- info_output$practice_hscp_name[info_output$level == "HSCP"]
info_output$report_area_name[info_output$level == "Scotland"] <- "Scotland"
info_output$report_area_name[info_output$level == "GPCL"] <- info_output$report_area[info_output$level == "GPCL"]

info_output <- info_output %>% select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

#add on completed forms and sample size####
sample_size_list <- readRDS("output/temp/sample_size_list_net_of_deaths.rds")
forms_completed_list <- readRDS("output/temp/forms_completed_list.rds")

info_output <- left_join(info_output,forms_completed_list, by = c("level","report_area"))
info_output <- left_join(info_output,sample_size_list, by = c("level","report_area"))
info_output <- info_output %>%
                mutate(Response_Rate_perc = forms_completed / sample_pop * 100)

####add on historical data####
info_questions_historical <- read_sav("/conf/bss/pat-exp-surveys/health-and-care/201920/Tableau/Datafiles/info_questions.sav")
ls(info_questions_historical,sorted = FALSE)
table(info_questions_historical$Level)
#take out GP and GP Cluster data
info_questions_historical <- info_questions_historical %>% 
                            filter(!Level %in% c("GP","GPCL") )
info_questions_historical$in_historical_file <- 1
info_questions_historical <- info_questions_historical %>% select(-Forms_completed,-N_IncludedResponses,-Question_Type,-sample_size,
                                                                  -SurveySection,-Question_text,-Response_Rate_perc,     
                                                                  -sample_size,-SurveySection  )
colnames(info_questions_historical)<-tolower(colnames(info_questions_historical)) 
info_questions_historical$response_option <- as.character(info_questions_historical$responseoption)

#Update info_questions_historical$question_2020 to include 'q':
info_questions_historical$question_2020 <- paste("q", info_questions_historical$question_2020, sep="")
#update question_2020 in info_output for q11 (q11 in 2022) & q24 (q30 in 2022) to ensure full match up
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "1" ] <- "q11a"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "2" ] <- "q11b"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "3" ] <- "q11c"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "4" ] <- "q11d"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "5" ] <- "q11e"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q11" & info_questions_historical$response_option == "6" ] <- "q11f"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q24" & info_questions_historical$response_option == "1" ] <- "q24a"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q24" & info_questions_historical$response_option == "2" ] <- "q24b"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q24" & info_questions_historical$response_option == "3" ] <- "q24c"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q24" & info_questions_historical$response_option == "4" ] <- "q24d"
info_questions_historical$question_2020 [info_questions_historical$question_2020 == "q24" & info_questions_historical$response_option == "5" ] <- "q24e"
#update response_option in info_output for q11 (q11 in 2022) & q24 (q30 in 2022) to ensure full match up 
#q11a - not necessary
info_questions_historical$response_option [info_questions_historical$question_2020 == "q11b" & info_questions_historical$response_option == "2" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q11c" & info_questions_historical$response_option == "3" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q11d" & info_questions_historical$response_option == "4" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q11e" & info_questions_historical$response_option == "5" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q11f" & info_questions_historical$response_option == "6" ] <- "1"
#q24a - not necessary
info_questions_historical$response_option [info_questions_historical$question_2020 == "q24b" & info_questions_historical$response_option == "2" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q24c" & info_questions_historical$response_option == "3" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q24d" & info_questions_historical$response_option == "4" ] <- "1"
info_questions_historical$response_option [info_questions_historical$question_2020 == "q24e" & info_questions_historical$response_option == "5" ] <- "1"

#remove q12 "Community Link worker" from info_questions_historical and update response_option for "Another Healthcare worker" from 7 to 6 in order to match onto info_output
info_questions_historical <- subset(info_questions_historical, info_questions_historical$response_text != "Community Link Worker")
info_questions_historical$response_option [info_questions_historical$question_2020 == "q12"
                                          & info_questions_historical$response_text == "Another healthcare professional" 
                                          & info_questions_historical$response_option == "7" ] <- "6"
info_questions_historical$response_text[info_questions_historical$response_text == "Another healthcare professional"] <- "Another Healthcare professional"

#join info_output and info_questions_historical
info_output_full <- left_join(info_output,info_questions_historical,by = c("question_2020","response_option","level","report_area_name"= "report_area"),suffix=c("2022","hist"))


oldnames <- c("level","report_area_name","question","question_text","surveysection","response_option","response_text2022",
              "n_includedresponses","n_response", "wgt_percent_response","n_response_2020","wgt_percent_response_2020",
              "n_response_2018","wgt_percent_response_2018","n_response_2016","wgt_percent_response_2016",
              "n_response_2014","wgt_percent_response_2014","forms_completed","sample_pop","Response_Rate_perc")
newnames <- c("Level","Report_Area","Question_2022","Question_text","SurveySection","ResponseOption","Response_text",
             "N_IncludedResponses","N_Response_2022", "Wgt_Percent_Response_2022","N_Response_2020","Wgt_Percent_Response_2020",
             "N_Response_2018","Wgt_Percent_Response_2018","N_Response_2016","Wgt_Percent_Response_2016",
             "N_Response_2014","Wgt_Percent_Response_2014","Forms_completed","sample_size","Response_Rate_perc")

info_output_full <- info_output_full %>% rename_at(vars(all_of(oldnames)), ~ all_of(newnames))
info_questions <- info_output_full %>% 
  select(all_of(newnames))

#info_output is intended to be a full version of the data
info_output_full <- info_output_full %>% select(-in_historical_file,-responseoption,-response_texthist)
info_output_full <- rename(info_output_full,"report_area_code"= "report_area")
saveRDS(info_output_full, "output/analysis_output/info_output_full.rds")
hist.file2 <- readRDS("output/analysis_output/info_output_full.rds")
identical(info_output_full,hist.file2)
all.equal(info_output_full,hist.file2)
table(info_output_full$report_area,useNA = c("always"))
#check if the same as before
hist.file <- readRDS("output/analysis_output/info_questions_sg.rds")
hist.file <- hist.file %>% arrange(Level,Report_Area,Question_2022,ResponseOption)
info_questions <- info_questions %>% arrange(Level,Report_Area,Question_2022,ResponseOption)
identical(info_questions$N_Response_2020,hist.file$N_Response_2020)

all.equal(info_questions$N_Response_2020,hist.file$N_Response_2020)
attributes(info_questions$N_Response_2020)
attributes(hist.file$N_Response_2020)
check <- if_else(info_questions$N_Response_2020 == hist.file$N_Response_2020,0,1)
table(check)

#save out for SG - filepath + name was originally "output/tableau/info_questions.rds" & "output/tableau/info_questions.xlsx"
#file.remove("output/analysis_output/info_questions_sg.rds")
saveRDS(info_questions, "output/analysis_output/info_questions_sg.rds")
#file.remove("output/analysis_output/info_questions_sg.xlsx")



