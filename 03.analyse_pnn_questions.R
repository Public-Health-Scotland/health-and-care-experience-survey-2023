# Name of file: 03.analyse_pnn_questions.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Analyse PNN questions. Read in responses longer, add response rates and output analyses at all levels of reporting.
# 
# Approximate run time: 15 min
# 
# Approximate memory usage: 1 GiB
# 
# *****************************************

#Inputs: 
#"lookups/percent_positive_questions.rds" - created in script 00.create_question_lookup
#"output/analysis_output/responses_longer.rds" - created in script 01.create_responses_longer
#"lookups/question_lookup_pnn.rds"- created in script 00.create_question_lookup

#Outputs: 
#"output/temp/nat_pnn.rds"
#"output/temp/hb_pnn.rds"
#"output/temp/hscp_pnn.rds"
#"output/temp/gpcl_pnn.rds"
#"output/temp/gp_pnn.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds")) #read in results longer data####

responses_longer <- responses_longer %>% 
  filter(question %in% percent_positive_questions) %>% #select pnn questions only
  mutate(scotland = "Scotland") #add new variable for reporting at national level

question_lookup_pnn <- readRDS(paste0(lookup_path,"question_lookup_pnn.rds")) #read in question_lookup_pnn

responses_longer <- responses_longer %>% 
  left_join(select(question_lookup_pnn,-weight),by = c("question","response_option"))

#check that these are all 'invalid' response combinations
table(responses_longer$question[is.na(responses_longer$response_text) & responses_longer$response_option != '0'],
      responses_longer$response_option[is.na(responses_longer$response_text) & responses_longer$response_option != '0'])

#this removes all responses which are not positive, neutral or negative, and code the positive responses to 1 so that the mean can be used as percent positive
responses_longer <- responses_longer %>% mutate('pos' = case_when(response_text == "Positive" ~ 1,
                                        response_text %in% c("Negative","Neutral") ~ 0,
                                        TRUE ~ 9)) %>%
                                        filter(pos != 9)
#Make responses_longer a list so that lapply can be used
responses.list <- split(responses_longer, responses_longer$question)
responses.list <- setNames(responses.list,percent_positive_questions)

#this is needed for those questions with 1 positive response per practice
options(survey.lonely.psu="remove")

#define function to aggregate PNN questions, for HB, HSCP and Scotland levels
aggregate_pnn <- function(report_areas,wt,x) {
  pnn_output <- responses.list[[x]] %>% 
  as_survey_design(id = 1,strata = gp_prac_no, fpc = eligible_pats,weights = {{wt}},
                                                    variables = c(question, gp_prac_no, {{report_areas}},{{wt}},response_text,pos)) %>% 
  group_by(question,{{report_areas}}) %>%
  summarise(wgt_percentpositive = survey_mean(pos, vartype = "ci",deff = TRUE),
            n_includedresponses = n(),
            n_positive = sum(pos),
            n_neutral = sum(response_text == "Neutral"),
            n_negative = sum(response_text == "Negative"),
            n_wgt_includedresponses = sum({{wt}}),
            n_wgt_positive = sum({{wt}}*pos),
            n_wgt_neutral = sum({{wt}}[response_text == "Neutral"]),
            n_wgt_negative = sum({{wt}}[response_text == "Negative"]))%>%
  mutate(percentpositive = n_positive / n_includedresponses * 100,
         percentneutral = n_neutral / n_includedresponses * 100,
         percentnegative = n_negative / n_includedresponses * 100,
         wgt_percentpositive = wgt_percentpositive * 100,
         wgt_percentpositive_low = wgt_percentpositive_low * 100,
         wgt_percentpositive_upp = wgt_percentpositive_upp * 100,
         wgt_percentneutral = n_wgt_neutral / n_wgt_includedresponses * 100,
         wgt_percentnegative = n_wgt_negative / n_wgt_includedresponses * 100)}

#run at each level####
#nb: these take some time to run! Especially at GP level ~30 mins plus
nat_pnn  <- lapply(seq_along(percent_positive_questions), function(x) {
  aggregate_pnn(scotland,nat_wt,x)})
nat_pnn <- bind_rows(nat_pnn)
nat_pnn <- nat_pnn %>% mutate(level = "Scotland") %>% rename("report_area" = "scotland")
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"nat_pnn.rds"))
identical(hist.file,nat_pnn)
file.remove(paste0(analysis_output_path,"nat_pnn.rds"))
saveRDS(nat_pnn,paste0(analysis_output_path,"nat_pnn.rds"))

hb_pnn <- lapply(seq_along(percent_positive_questions), function(x) {
  aggregate_pnn(practice_board_code,hb_wt,x)})
hb_pnn <- bind_rows(hb_pnn)
hb_pnn <- hb_pnn %>% mutate(level = "Health Board") %>% rename("report_area" = "practice_board_code")
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"hb_pnn.rds"))
identical(hist.file,hb_pnn)
file.remove(paste0(analysis_output_path,"hb_pnn.rds"))
saveRDS(hb_pnn,paste0(analysis_output_path,"hb_pnn.rds"))

hscp_pnn <- lapply(seq_along(percent_positive_questions), function(x) {
  aggregate_pnn(practice_hscp_code,hscp_wt,x)})
hscp_pnn <- bind_rows(hscp_pnn)
hscp_pnn <- hscp_pnn %>% mutate(level = "HSCP") %>% rename("report_area" = "practice_hscp_code")
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"hscp_pnn.rds"))
identical(hist.file,hscp_pnn)
file.remove(paste0(analysis_output_path,"hscp_pnn.rds"))
saveRDS(hscp_pnn,paste0(analysis_output_path,"hscp_pnn.rds"))

gpcl_pnn <- lapply(seq_along(percent_positive_questions), function(x) {
  aggregate_pnn(practice_hscp_cluster,gpcl_wt,x)})
gpcl_pnn <- bind_rows(gpcl_pnn)
gpcl_pnn <- gpcl_pnn %>% mutate(level = "GPCL") %>% rename("report_area" = "practice_hscp_cluster")
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"gpcl_pnn.rds"))
identical(hist.file,gpcl_pnn)
file.remove(paste0(analysis_output_path,"gpcl_pnn.rds"))
saveRDS(gpcl_pnn,paste0(analysis_output_path,"gpcl_pnn.rds"))

gp_pnn <- lapply(seq_along(percent_positive_questions), function(x) {
  aggregate_pnn(gp_prac_no,gp_wt,x)})
gp_pnn <- bind_rows(gp_pnn)
gp_pnn <- gp_pnn %>% mutate(level = "GP") %>% rename("report_area" = "gp_prac_no")
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"gp_pnn.rds"))
identical(hist.file,gp_pnn)
file.remove(paste0(analysis_output_path,"gp_pnn.rds"))
saveRDS(gp_pnn,paste0(analysis_output_path,"gp_pnn.rds"))
