# Written by x x x
# November 2023.
# Adapted from February 2022 code by Catriona Haddow
# 
# *****************************************
#Purpose: Reads in PNN output. Run significance tests. Reads in historical data. Outputs analyses at all levels of reporting.

#Inputs: 
#"lookups/Final_Practice_lookup.rds"
#"output/temp/output/temp/sample_size_list_net_of_deaths.rds"
#"output/temp/forms_completed_list.rds"
#"lookups/question_lookup_pnn.rds"
#"output/temp/nat_pnn.rds"
#"output/temp/hb_pnn.rds"
#"output/temp/hscp_pnn.rds"
#"output/temp/gpcl_pnn.rds"
#"output/temp/gp_pnn.rds"
#"output/temp/pnn_questions_historical.rds"

#Outputs: 
#"output/analysis_output/pnn_output_sg.rds"
#"output/analysis_output/pnn_output_sg.xlsx"

# load in libraries
library(tidyverse)
#version 1.3.2
library(srvyr)
#version 1.2.0
library(openxlsx)
#version 4.2.5.2

#Note: 28/04/2022 updated after production of file for SG to test use of significance tests,and to remove the tableau output. 
# Any changes to SG output file were checked for and found to be related to attributes only.

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324")

###Read in files
#read in Practice lookup, sample size, list completed,question_lookup_pnn
practice_lookup <- readRDS("lookups/Final_Practice_lookup.rds")
sample_size_list <- readRDS("output/temp/sample_size_list_net_of_deaths.rds")
forms_completed_list <- readRDS("output/temp/forms_completed_list.rds")
question_lookup_pnn <- readRDS("lookups/question_lookup_pnn.rds")

#read in pnn files
nat_pnn <- readRDS("output/temp/nat_pnn.rds")
hb_pnn <- readRDS("output/temp/hb_pnn.rds")
hscp_pnn <- readRDS("output/temp/hscp_pnn.rds")
gpcl_pnn <- readRDS("output/temp/gpcl_pnn.rds")
gp_pnn <- readRDS("output/temp/gp_pnn.rds")

####significance text flags for comparison to scotland

pnn_output_comp <- bind_rows(hb_pnn,hscp_pnn,gpcl_pnn,gp_pnn)
pnn_output_comp <- left_join(pnn_output_comp,select(nat_pnn,question,"nat_wgt_percentpositive" = wgt_percentpositive,
                                                    "nat_wgt_percentpositive_low" = wgt_percentpositive_low,
                                                    "nat_wgt_percentpositive_upp" = wgt_percentpositive_upp,
                                                    "nat_n_includedresponses" = n_includedresponses,
                                                    "nat_wgt_percentpositive_deff" = wgt_percentpositive_deff), by = c("question"))


#using spss equivalent,flag to indicate whether the 2022 local level result is significantly higher or lower than Scotland result.
pnn_output_comp <- pnn_output_comp %>%
  mutate(p_scot=nat_wgt_percentpositive/100,
         n_scot=nat_n_includedresponses,
         p2=wgt_percentpositive/100,
         n2=n_includedresponses,
         scot_serror=sqrt((wgt_percentpositive_deff*(p2*(1-p2))/n2)+ (nat_wgt_percentpositive_deff*(p_scot*(1-p_scot))/n_scot)),
         scot_z_score=(p2-p_scot)/scot_serror, 
         scot_sigf = if_else(is.na(scot_z_score),1,
                             if_else(scot_z_score>qnorm(0.025,0,1) & scot_z_score<qnorm(0.975,0,1),0,1)),
         scot_var=round(100*(p2-p_scot),0.1),
         scot_flag = if_else(scot_sigf==1 & scot_var < 0,"Statistically lower than Scotland",  
                             if_else(scot_sigf==1 & scot_var > 0, "Statistically higher than Scotland",
                                     "Not statistically different from Scotland")))               
                       
           
#Replicate national results 
nat_pnn <- left_join(nat_pnn,select(nat_pnn,question,"nat_wgt_percentpositive" = wgt_percentpositive,
                                                    "nat_wgt_percentpositive_low" = wgt_percentpositive_low,
                                                    "nat_wgt_percentpositive_upp" = wgt_percentpositive_upp,
                                                    "nat_n_includedresponses" = n_includedresponses), by = c("question"))

nat_pnn <- nat_pnn %>%
  mutate(scot_flag = "Comparison not applicable")

pnn_output <-  bind_rows(pnn_output_comp,nat_pnn)


####add the question information####
pnn_output <- left_join(pnn_output,distinct(question_lookup_pnn,question,question_text,question_2020,surveysection),by = c("question"))

#add on report_area names
pnn_output <- left_join(pnn_output,distinct(practice_lookup,gp_prac_no,practice_name_letter),by = c("report_area" = "gp_prac_no"))
pnn_output <- left_join(pnn_output,distinct(practice_lookup,practice_board_code,practice_board_name),by = c("report_area" = "practice_board_code"))
pnn_output <- left_join(pnn_output,distinct(practice_lookup,practice_hscp_code,practice_hscp_name),by = c("report_area" = "practice_hscp_code"))
pnn_output$report_area_name[pnn_output$level == "GP"] <- paste0(pnn_output$practice_name_letter[pnn_output$level == "GP"]," (",pnn_output$report_area[pnn_output$level == "GP"],")")
pnn_output$report_area_name[pnn_output$level == "Health Board"] <- pnn_output$practice_board_name[pnn_output$level == "Health Board"]
pnn_output$report_area_name[pnn_output$level == "Health Board"] <- gsub(" and "," & ",pnn_output$report_area_name[pnn_output$level == "Health Board"])

pnn_output$report_area_name[pnn_output$level == "HSCP"] <- pnn_output$practice_hscp_name[pnn_output$level == "HSCP"]
pnn_output$report_area_name[pnn_output$level == "Scotland"] <- "Scotland"
pnn_output$report_area_name[pnn_output$level == "GPCL"] <- pnn_output$report_area[pnn_output$level == "GPCL"]

pnn_output <- pnn_output %>% select(-practice_name_letter,-practice_board_name,-practice_hscp_name)

#add on completed forms and sample size
pnn_output <- left_join(pnn_output,forms_completed_list, by = c("level","report_area"))
pnn_output <- left_join(pnn_output,sample_size_list, by = c("level","report_area"))
pnn_output <- pnn_output %>%
  mutate(Response_Rate_perc = forms_completed / sample_pop * 100)

####add on historical data####
pnn_questions_historical <- readRDS("output/temp/pnn_questions_historical.rds")
pnn_questions_historical$wgt_percentpositive_2020 <- pnn_questions_historical$wgt_percentpositive_2020 * 100
pnn_questions_historical$wgt_percentpositive_low_2020 <- pnn_questions_historical$wgt_percentpositive_low_2020 * 100
pnn_questions_historical$wgt_percentpositive_upp_2020 <- pnn_questions_historical$wgt_percentpositive_upp_2020 * 100
pnn_output_full <- left_join(pnn_output,pnn_questions_historical,by = c("question_2020","level","report_area_name"= "report_area"),suffix=c("_2022",""))

#tidy question_2020 - this should really be done earlier
pnn_output_full$question_2020[grepl("[^:alnum:]",pnn_output_full$question_2020) == FALSE] <- NA
####significance text flags for comparison to 2020
#using spss equivalent,flag to indicate whether the 2022 result is significantly higher or lower than 2020 result.
        
pnn_output_full <- pnn_output_full %>%
  mutate(p_2020=wgt_percentpositive_2020/100,
         n_2020=n_includedresponses_2020,
         p2=wgt_percentpositive/100,
         n2=n_includedresponses,
         time_serror=sqrt((wgt_percentpositive_deff*(p2*(1-p2))/n2)+ (wgt_percentpositive_deff_2020*(p_2020*(1-p_2020))/n_2020)),
         time_z_score=(p2-p_2020)/time_serror, 
         time_sigf = if_else(time_z_score>qnorm(0.025,0,1) & time_z_score <qnorm(0.975,0,1),0,1),
         time_var=round(100*(p2-p_2020),0.1),
         time_flag = if_else(is.na(wgt_percentpositive_2020),"Not tested against 2020",
                             if_else(time_sigf==1 & time_var < 0,"Statistically lower than 2020",  
                             if_else(time_sigf==1 & time_var > 0, "Statistically higher than 2020",
                                     "Not statistically different from 2020"))))  

pnn_output_sg <- pnn_output_full %>% select("level","report_area","report_area_name","question","question_text","surveysection",
                                            "n_includedresponses","n_positive","n_neutral","n_negative",
                                            "n_wgt_includedresponses","n_wgt_positive","n_wgt_neutral","n_wgt_negative",
                                            "wgt_percentpositive","wgt_percentpositive_low","wgt_percentpositive_upp",
                                            "wgt_percentpositive_deff","wgt_percentneutral","wgt_percentnegative",
                                            "wgt_percentpositive_2020","wgt_percentpositive_2018","wgt_percentpositive_2016",
                                            "wgt_percentpositive_2014","scot_flag","time_flag",
                                            "nat_n_includedresponses","nat_wgt_percentpositive",
                                            "forms_completed","sample_pop","Response_Rate_perc")

#check if the same as before
hist.file <- readRDS("output/analysis_output/pnn_output_sg.rds")
identical(hist.file,pnn_output_sg)

file.remove("output/analysis_output/pnn_output_sg.rds")
saveRDS(pnn_output_sg, "output/analysis_output/pnn_output_sg.rds")
file.remove("output/analysis_output/pnn_output_sg.xlsx")
write.xlsx(pnn_output_sg, "output/analysis_output/pnn_output_sg.xlsx")

