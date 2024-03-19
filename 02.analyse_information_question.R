# Name of file: 02.analyse_information_questions.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Analyse information questions. Read in responses, response rates and output analyses at all levels of reporting.
# 
# Approximate run time: 3 min
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

#Inputs: 
#"lookups/information_questions.rds" - created in script 00.create_question_lookup
#"lookups/information_questions_tata.rds" - created in script 00.create_question_lookup
#"output/analysis_output/responses_longer.rds" - created in script 01.create_responses_longer
#"lookups/question_lookup_info.rds"- created in script 00.create_question_lookup
#"lookups/Final_Practice_lookup.rds"  - created as part of preparation work
#"output/temp/output/temp/sample_size_list_net_of_deaths.rds" - ???
#"output/temp/forms_completed_list.rds"
#"historical_data_path,"info_questions_sg.rds"

#Outputs: 
##"output/analysis_output/info_questions_sg.rds"
##"output/analysis_output/info_questions_sg.xlsx"
##"output/analysis_output/info_output_full.rds")

#Note: 28/04/2022 updated after production of the info_questions_sg to create a fuller version of the output including more variables. info_output_full

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))
table(responses_longer$question,useNA = c("always"))

#select information questions only
responses_longer <- responses_longer %>% filter(question %in% information_questions)

question_lookup_info <- readRDS(paste0(lookup_path,"question_lookup_info.rds")) #read in lookup for info questions

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

info_output <- bind_rows(nat_info,hb_info,hscp_info,gpcl_info,gp_info)
info_output[c("n_response", "n_wgt_response")][is.na(info_output[c("n_response", "n_wgt_response")])] <- 0

info_output <- info_output %>%
    group_by(report_area,question) %>%
      mutate(n_includedresponses = sum(n_response),
                n_wgt_includedresponses = sum(n_wgt_response)) %>%
      ungroup() %>%
      mutate(percent_response = n_response / n_includedresponses * 100,
           wgt_percent_response= n_wgt_response / n_wgt_includedresponses * 100)

#Code to deal with 'tick all that apply' questions.
#Removes the "No" response to the "tick all that apply" questions q09a-f, q14a-f, q20a-f, q27a-h, q28a-d, q32a-i, q34a-f, q35a-g, q39a-k####
info_output <- info_output %>%
               filter(!(substr(question,1,3) %in% information_questions_tata & response_option == "0"))

#add on report_area names
#read in Practice lookup
practice_lookup <- readRDS(paste0(lookup_path,"Final_Practice_lookup.rds"))

info_output <- info_output %>% 
  left_join(distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no")) %>% 
  left_join(distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))%>% 
  left_join(distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))%>% 
  mutate(report_area_name = case_when(level == "GP" ~ paste0(practice_name_letter," (",report_area,")"),
                                      level == "Health Board" ~ gsub(" and "," & ",practice_board_name),
                                      level == "HSCP" ~ practice_hscp_name,
                                      level %in% c("Scotland","GPCL")~ report_area, TRUE ~ "Error")) %>% 
  select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

sum(info_output$report_area_name == "Error") #check. Should be 0

#add on completed forms and sample size####
#read in sample size, list completed, 
sample_size_list <- readRDS(paste0(output_path,"sampling/sample_size_list_net_of_deaths.rds"))
forms_completed_list <- readRDS(paste0(analysis_output_path,"forms_completed_list.rds"))

info_output <- info_output %>% 
  left_join(forms_completed_list, by = c("level","report_area")) %>% 
  left_join(sample_size_list, by = c("level","report_area")) %>% 
  mutate(Response_Rate_perc = forms_completed / sample_pop * 100)

####add on historical data####
info_questions_historical <- readRDS(paste0(historical_data_path,"info_questions_sg.rds"))
ls(info_questions_historical,sorted = FALSE)
table(info_questions_historical$Level)

#take out GP and GP Cluster data, and remove unnecessary variables
info_questions_historical <- info_questions_historical %>% 
                            filter(!Level %in% c("GP","GPCL")) %>% 
                            mutate(in_historical_file = 1) %>% 
                            select(-Forms_completed,-N_IncludedResponses,-sample_size,
                                  -SurveySection,-Question_text,-Response_Rate_perc, -sample_size,-SurveySection  ) %>% 
                            rename_with(tolower) 

#Is anything like this needed for 2024?
#remove q12 "Community Link worker" from info_questions_historical and update response_option for "Another Healthcare worker" from 7 to 6 in order to match onto info_output
# info_questions_historical <- subset(info_questions_historical, info_questions_historical$response_text != "Community Link Worker")
# info_questions_historical$response_option [info_questions_historical$question_2020 == "q12"
#                                           & info_questions_historical$response_text == "Another healthcare professional" 
#                                           & info_questions_historical$response_option == "7" ] <- "6"
# info_questions_historical$response_text[info_questions_historical$response_text == "Another healthcare professional"] <- "Another Healthcare professional"

#join info_output and info_questions_historical
info_output_full <- info_output %>% 
        left_join(info_questions_historical,by = c("question_2022","response_option" = "responseoption","level","report_area_name"= "report_area"),suffix=c("2024","hist")) %>%
        rename("question_2024"="question")

#info_output is intended to be a full version of the data
info_output_full <- info_output_full %>% 
  select(-in_historical_file,-response_texthist) %>%  #ch removed responseoption. Not sure why it was there
  rename("report_area_code"= "report_area")

hist.file <- readRDS(paste0(analysis_output_path,"info_output_full.rds"))
identical(info_output_full,hist.file)
saveRDS(info_output_full, paste0(analysis_output_path,"info_output_full.rds"))

#Rename variables for consistency with previous files. Is this still needed?
oldnames <- c("level","report_area_name","question_2024","question_text","surveysection","response_option","response_text2024",
              "n_includedresponses","n_response", "wgt_percent_response","n_response_2022","wgt_percent_response_2022","n_response_2020","wgt_percent_response_2020",
              "n_response_2018","wgt_percent_response_2018","n_response_2016","wgt_percent_response_2016",
              "n_response_2014","wgt_percent_response_2014","forms_completed","sample_pop","Response_Rate_perc")
newnames <- c("Level","Report_Area","Question_2024","Question_text","SurveySection","ResponseOption","Response_text",
              "N_IncludedResponses","N_Response_2024", "Wgt_Percent_Response_2024","N_Response_2022", "Wgt_Percent_Response_2022","N_Response_2020","Wgt_Percent_Response_2020",
              "N_Response_2018","Wgt_Percent_Response_2018","N_Response_2016","Wgt_Percent_Response_2016",
              "N_Response_2014","Wgt_Percent_Response_2014","Forms_completed","sample_size","Response_Rate_perc")

info_questions <- info_output_full %>% 
  rename_with(~newnames, .cols = all_of(oldnames))%>% 
  select(all_of(newnames))

#check if the same as before
hist.file_sg <- readRDS(paste0(analysis_output_path,"/info_questions_sg.rds"))
hist.file_sg <- hist.file_sg %>% arrange(Level,Report_Area,Question_2024,ResponseOption)

info_questions <- info_questions %>% arrange(Level,Report_Area,Question_2024,ResponseOption)
identical(info_questions,hist.file_sg)
#file.remove(paste0(analysis_output_path,"info_questions_sg.rds"))
saveRDS(info_questions, paste0(analysis_output_path,"/info_questions_sg.rds"))
#file.remove(paste0(analysis_output_path,"info_questions_sg.xlsx")) # do we need an excel version