## Written by Catriona Haddow and Martin Leitch
# November 2023.
# Adapted from 2021 code by Catriona Haddow and Martin Leitch
# Finalised February 2024
# *****************************************
# load in libraries

library(tidyverse)
#version 1.3.2
library(janitor)
library(openxlsx)
#version 4.2.5.2
###################################################################################

#Purpose: Reads in patient level data for HACE 2023 Survey and applies and checks validation rules

#Inputs: "HA23_Data_FINAL_V1.xlsx"  #UPDATE!

#Outputs: #UPDATE!
#"rules_summary.xlsx"
#"data_Validated results.rds"
#"data_Validated results.xlsx"
#"anonymised_data_Validated results for SG.rds"
#"anonymised_data_Validated results for SG.csv"

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324/")

#Step 2: Read in reformatted final results####

#get file - ONLY IF RUNNING AGAIN WITHOUT RELOADING ORIGINAL FINAL DATA SUBMISSION FROM QH.
contractor_data <- readRDS("data/Results from Contractor/Final data/Final_unrouted_data.rds") # Update "Test" to "Final"

#Step 3: Recode from na to 0####
#Section 1 Your GP Practice
Section1 <-c("q01","q02","q03","q04","q05","q06","q07","q08","q09a","q09b","q09c","q09d","q09e","q09f","q10","q11",
             "q12a","q12b","q12c","q12d","q12e","q12f","q12g","q13")
contractor_data[Section1][is.na(contractor_data[Section1])] <- 0

#Section 2 Treatment or Advice for your GP Practice
Section2 <-c("q14a","q14b","q14c","q14d","q14e","q14f","q15","q16a","q16b","q16c","q16d","q16e","q16f","q16g","q16h","q16i","q16j","q16k","q16l","q16m","q16n",
             "q17a","q17b")
contractor_data[Section2][is.na(contractor_data[Section2])] <- 0
#hace2324_Final$q18 is a comments response box

#Section 3 Out of Hours Healthcare
Section3 <-c("q19","q20a","q20b","q20c","q20d","q20e","q20f","q21","q22","q23","q24a","q24b","q24c","q24d","q24e","q24f","q24g","q24h","q24i","q25")
contractor_data[Section3][is.na(contractor_data[Section3])] <- 0
#hace2324_Final$q26 is a comments response box

#Section 4 Care, Support and Help with Everyday Living
Section4 <-c("q27a","q27b","q27c","q27d","q27e","q27f","q27g","q27h","q28a","q28b","q28c","q28d","q29","q30a","q30b","q30c","q30d","q30e","q30f","q30g","q30h","q31",
             "q32a","q32b","q32c","q32d","q32e","q32f","q32g","q32h","q32i")
contractor_data[Section4][is.na(contractor_data[Section4])] <- 0

#Section 5 Caring Responsibilities
Section5 <-c("q33","q34a","q34b","q34c","q34d","q34e","q34f","q35a","q35b","q35c","q35d","q35e","q35f","q35g","q35h","q36","q37a","q37b","q37c","q37d","q37e")
contractor_data[Section5][is.na(contractor_data[Section5])] <- 0

#Section 6 About You
Section6 <-c("q38","q39a","q39b","q39c","q39d","q39e","q39f","q39g","q39h","q39i","q39j","q39k","q40","q41","q42","q43","q44")
contractor_data[Section6][is.na(contractor_data[Section6])] <- 0

all_sections <- c(Section1,Section2,Section3,Section4,Section5,Section6)

#Step 4: Apply validation rules####

#Rule 1: FOR QH ONLY: ####
#'Tick one box onlyâ€™ questions: if respondent selects more than one box, then question is cleared. The majority of questions are this type, 
#'so itâ€™s easier to list the questions that this does not apply to: Q11, Q20, Q29, Q30, Q36, Q37 and Q40.

#===
#Rule 2: When did you last contact the GP Practice named on the enclosed letter?####
## a > If Q1 is blank, and Q2 is not blank â€“ set Q1 to 1.####
#Impact check:
Rule2a <-0
Rule2a <- ifelse(contractor_data$q01 ==0 & contractor_data$q02 !=0,1,0)

#Check frequencies before implementing rule
table(contractor_data$q01,contractor_data$q02)

#Implement rule:
contractor_data$q01[contractor_data$q01 == 0 & contractor_data$q02 !=0] <-1
#Check frequencies after implementing rule
table(contractor_data$q01,contractor_data$q02)

#--
## b > If Q1 <> 1 and Q2 to Q17 are not all blank â€“ set Q2 to Q17 to blank.#### 
#Impact check:
Rule2b <-0
q2toq17 <-c("q02","q03","q04","q05","q06","q07","q08","q09a","q09b","q09c","q09d","q09e","q09f","q10","q11",
            "q12a","q12b","q12c","q12d","q12e","q12f","q12g","q13","q14a","q14b","q14c","q14d","q14e","q14f","q15",
            "q16a","q16b","q16c","q16d","q16e","q16f","q16g","q16h","q16i","q16j","q16k","q16l","q16m","q16n",
            "q17a","q17b")
Rule2b <- ifelse(contractor_data$q01 !=1 & rowSums(contractor_data[q2toq17]) !=0,1,0)

#Check frequencies before implementing rule
apply(contractor_data[q2toq17], MARGIN=2, table)

#Implement rule:
contractor_data[q2toq17][contractor_data$q01 !=1 & rowSums(contractor_data[q2toq17]!=0),] <-0
#Check frequencies after implementing rule
apply(contractor_data[q2toq17], MARGIN=2, table)
apply(contractor_data[q2toq17][Rule2b==1,], MARGIN=2, table)

#===
#Rule 3: The last time you needed an appointment with your general practice, what kind of appointmentâ€¦?####
##a>	If Q6 = 7 and Q7 to Q9 are not all blank â€“ set Q7 to Q9 to blank. ####
Rule3a <- 0
q7toq9 <- c("q07","q08","q09a","q09b","q09c","q09d","q09e","q09f")
Rule3a <- ifelse(contractor_data$q06 == 7 & rowSums(contractor_data[q7toq9]) !=0,1,0)

#Check frequencies before implementing rule
apply(contractor_data[q7toq9], MARGIN=2, table)

#Implement rule:
contractor_data[q7toq9][contractor_data$q06 ==7 & rowSums(contractor_data[q7toq9]!=0),] <-0 
#Check frequencies after implementing rule
apply(contractor_data[q7toq9], MARGIN=2, table)
apply(contractor_data[q7toq9][Rule3a==1,], MARGIN=2, table)

##b>	If Q6 = blank and Q7 to Q9 are not all blank â€“ set Q7 to Q9 to blank.####
Rule3b <- 0
Rule3b <- ifelse(contractor_data$q06 == 0 & rowSums(contractor_data[q7toq9]) !=0,1,0)

#Check frequencies before implementing rule
apply(contractor_data[q7toq9], MARGIN=2, table)

#Implement rule:
contractor_data[q7toq9][contractor_data$q06 ==0 & rowSums(contractor_data[q7toq9]!=0),] <-0
#Check frequencies after implementing rule
apply(contractor_data[q7toq9], MARGIN=2, table)

#===
#Rule 4: Were you satisfied with the appointment you were offered?####
#>	If Q8 = 1 and Q9 is not blank â€“ set Q9 to blank####
Rule4 <-0
q9 <- c("q09a","q09b","q09c","q09d","q09e","q09f")
Rule4 <- ifelse(contractor_data$q08 ==1 & rowSums(contractor_data[q9]) !=0,1,0)

#Implement rule:
contractor_data[q9][contractor_data$q08 == 1 & rowSums(contractor_data[q9]!=0),] <-0

#===
#Rule 5: The last time you needed to see or speak to a doctor or nurse from your GP Practice quite urgently, how long did you have to wait? ####
## a > If Q10 = 1, 2 or 4 and Q11 is not blank â€“ set Q11 to blank.####
#Impact check:
Rule5a <-0
Rule5a <- ifelse(contractor_data$q10 %in% c(1,2,4) & contractor_data$q11 !=0,1,0)

#Check frequencies before implementing rule
tabyl(contractor_data[Rule5a == 1,],q10,q11)

#Implement rule:
contractor_data$q11[contractor_data$q10 %in% c(1,2,4) & contractor_data$q11 !=0] <- 0
#Check frequencies after implementing rule
tabyl(contractor_data[Rule5a == 1,],q10,q11)

#--
## b > If Q10 is blank and Q11 is not blank â€“ set Q10 to 3.####
#Impact check:
Rule5b <-0
Rule5b <- ifelse(contractor_data$q10 ==0 & contractor_data$q11 !=0,1,0)

#Check frequencies before implementing rule
tabyl(contractor_data[Rule5b==1,],q10,q11)

#Implement rule:
contractor_data$q10[contractor_data$q10 == 0 & contractor_data$q11 !=0] <-3
#Check frequencies after implementing rule
tabyl(contractor_data[Rule5b==1,],q10,q11)

#===
#Rule 6: The last time you received treatment or advice at your GP practice in the last 12 months, what did you receive treatment or advice for?####
## a > If Q14f = 1 and any of Q14a to Q14e = 1 â€“ set Q14f to blank.####
#Impact check:
Rule6a <-0
q14atoq14e <-c("q14a","q14b","q14c","q14d","q14e")
Rule6a = if_else(contractor_data$q14f ==1 & rowSums(contractor_data[q14atoq14e]) > 0,1,0)

#Check frequencies before implementing rule
lapply(contractor_data[q14atoq14e],table,contractor_data$q14f) 

#Implement rule
contractor_data$q14f[contractor_data$q14f == 1 & rowSums(contractor_data[q14atoq14e]) >0] <- 0
#Check frequencies after implementing rule
lapply(contractor_data[q14atoq14e],table,contractor_data$q14f) 

#--
## b > If Q14f = 1 and Q15 to Q17 are not all blank â€“ set Q15 to Q17 to blank.####
#Impact check
Rule6b <-0
q15toq17 <-c("q15","q16a","q16b","q16c","q16d","q16e","q16f","q16g","q16h","q16i","q16j","q16k","q16l","q16m","q16n","q17a","q17b")
Rule6b <- ifelse(contractor_data$q14f ==1 & rowSums(contractor_data[q15toq17]) !=0,1,0)

#Check frequencies before implementing rule
lapply(contractor_data[q15toq17],table,contractor_data$q14f) 

#Implement rule
contractor_data[q15toq17][contractor_data$q14f ==1 & rowSums(contractor_data[q15toq17]) !=0,] <-0
#Check frequencies after implementing rule
lapply(contractor_data[q15toq17],table,contractor_data$q14f) 

#--
## c > If none of Q14a-e = 1 then set Q15 to Q17 to blank.####
#Impact check
#q14atoq14e as per rule 6a.
#q15toq17 as per rule 6b.
Rule6c <-0
Rule6c <- if_else(rowSums(contractor_data[q14atoq14e]) == 0 & rowSums(contractor_data[q15toq17]) != 0,1,0)

#Check frequencies before implementing rule
apply((contractor_data[q15toq17][Rule6c == 1,]), MARGIN=2, table)

#Implement rule
contractor_data[q15toq17][rowSums(contractor_data[q14atoq14e]) == 0,] <-0

#Check frequencies after implementing rule
apply(contractor_data[q15toq17][Rule6c == 1,], MARGIN=2, table)

#===
#Rule 7: In the past 12 months, have you contacted an NHS service when you wanted to see a GP, but your GP practice was closed?####
## a > If Q19 = 2 and Q20 to Q25 are not all blank â€“ set Q20 to Q25 to blank.####
#Impact check

Rule7a <-0
q20toq25_recode <-c("q20a","q20b","q20c","q20d","q20e","q20f","q21","q22","q23","q24a","q24b","q24c","q24d","q24e","q24f","q24g","q24h","q24i","q25")
Rule7a <- ifelse(contractor_data$q19 ==2 & rowSums(contractor_data[q20toq25_recode]) !=0,1,0)

#Check frequencies before implementing rule
apply(contractor_data[q20toq25_recode], MARGIN=2, table)

#Implement rule
contractor_data[q20toq25_recode][contractor_data$q19 ==2,] <-0
#Check frequencies after implementing rule
apply((contractor_data[contractor_data$q19 ==2,q20toq25_recode]), MARGIN=2, table)

#--
## b > If Q19 is blank and Q20 to Q25 are not all blank â€“ set Q19 to 1.####
#Impact check
Rule7b <-0
Rule7b <- ifelse(contractor_data$q19 ==0 & rowSums(contractor_data[q20toq25_recode]) !=0,1,0)
table(Rule7b)

#Check frequencies before implementing rule
apply(contractor_data[Rule7b ==1,q20toq25_recode], MARGIN=2, table)

#Implement rule
contractor_data$q19[contractor_data$q19 ==0 & rowSums(contractor_data[q20toq25_recode]) !=0] <-1

#Check frequencies after implementing rule
table(contractor_data$q19[rowSums(contractor_data[q20toq25_recode]) !=0])

#--
## c > If Q19 <> 1 then set Q20 to Q25 to blank (note â€“ assumes the above rule has already been applied)####
#Impact check
Rule7c <-0
Rule7c <- ifelse(contractor_data$q19 !=1 & rowSums(contractor_data[q20toq25_recode]) !=0,1,0)
table(Rule7c)

#Check frequencies before implementing rule
apply(contractor_data[contractor_data$q19 !=1,q20toq25_recode], MARGIN=2, table)

#Implement rule
contractor_data[q20toq25_recode][contractor_data$q19 !=1,] <-0
#Check frequencies after implementing rule
apply(contractor_data[q20toq25_recode], MARGIN=2, table)

#===
#Rule 8: In the past 12 months, have you had any help or support with everyday living?####
#check pre-validation status
social_care_section_pre_validation <- contractor_data %>% 
  select(qh_psid,starts_with("q27")|starts_with("q32")) %>% 
  mutate(help_indicator = case_when(q27a == 1 | q27b == 1 |q27c == 1 | q27d == 1|q27e == 1 | q27f == 1 ~ "1 Help received",
                                    q27g == 1~"2 Help needed, not received",
                                    q27h == 1~"3 No help needed",TRUE ~ "4 Not complete"),
         no_help_reason = case_when(q32a == 1|q32b == 1|q32c == 1|q32d == 1|q32e == 1|q32f == 1|q32g == 1|q32h == 1 ~ "1 Reason given",
                                    q32i == 1~ "2 Not applicable",
                                    TRUE ~ "3 Not complete"))

table(social_care_section_pre_validation$no_help_reason,social_care_section_pre_validation$help_indicator)

## a > If Q27a to Q27h are all blank (and Q28 to Q32 are not all blank) - set Q28 to Q32 to blank ####
#This is, if respondent hasn't completed this routing question, blank rest of section.

#Impact check
Rule8a <-0
q27atoq27h <- c("q27a","q27b","q27c","q27d","q27e","q27f","q27g","q27h")
q28toq32i <- c("q28a","q28b","q28c","q28d","q29","q30a","q30b","q30c","q30d","q30e","q30f","q30g","q30h",
               "q31","q32a","q32b","q32c","q32d","q32e","q32f","q32g","q32h","q32i")
Rule8a <- ifelse((rowSums(contractor_data[q27atoq27h]) ==0)
                 & rowSums(contractor_data[q28toq32i])>0,1,0)

table(Rule8a[rowSums(contractor_data[q27atoq27h]) ==0],useNA = c("always"))

#Implement rule
contractor_data <- contractor_data %>% 
  mutate(across(all_of(q28toq32i), ~replace(., Rule8a == 1, 0)))

#check
lapply(contractor_data[q28toq32i][Rule8a == 1,],table) 

## b > If Q27g = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 â€“ set Q27g to blank.####
#Impact check
Rule8b <-0
q27atoq27f <- c("q27a","q27b","q27c","q27d","q27e","q27f")
Rule8b <- ifelse(contractor_data$q27g ==1 & rowSums(contractor_data[q27atoq27f] >0),1,0)

#Check frequencies before implementing rule
table(contractor_data$q27g)

#Implement rule
contractor_data$q27g [contractor_data$q27g ==1 & rowSums(contractor_data[q27atoq27f]) >0]<-0

#Check frequencies after implementing rule
table(contractor_data$q27g)

#--
## c > If Q27h = 1 and any of Q27a, Q27b, Q27c, Q27d, Q27e or Q27f = 1 â€“ set Q27h to blank.####
#Impact check
Rule8c <-0
Rule8c <- ifelse(contractor_data$q27h ==1 & rowSums(contractor_data[q27atoq27f]) >0,1,0)

#Check frequencies before implementing rule
table(contractor_data$q27h)

#Implement rule
contractor_data$q27h[contractor_data$q27h ==1 & rowSums(contractor_data[q27atoq27f]) >0]<-0
#Check frequencies after implementing rule
table(contractor_data$q27h)

#--
## d > If Q27g = 1 and Q27h = 1 â€“ set Q27h to blank.####
#Impact check
Rule8d <-0
Rule8d <- ifelse(contractor_data$q27g ==1 & contractor_data$q27h == 1,1,0)

#Check frequencies before implementing rule
table(contractor_data$q27g,contractor_data$q27h)

#Implement rule
contractor_data$q27h[contractor_data$q27g ==1 & contractor_data$q27h == 1] <- 0
#Check frequencies after implementing rule
table(contractor_data$q27g,contractor_data$q27h)

## e > If Q27g = 1 â€“ set Q28 to Q31 to blank.####
#Impact check
Rule8e <-0
q28toq31 <- c("q28a","q28b","q28c","q28d","q29","q30a","q30b","q30c","q30d","q30e","q30f","q30g","q30h","q31")
Rule8e <- ifelse(contractor_data$q27g ==1 & rowSums(contractor_data[q28toq31]) >0,1,0)
table(Rule8e)

#Check frequencies before implementing rule
apply(contractor_data[Rule8e==1,q28toq31], MARGIN=2, table)

#Implement rule
contractor_data[q28toq31][contractor_data$q27g ==1 & rowSums(contractor_data[q28toq31] >0),] <- 0
#Check frequencies after implementing rule
apply(contractor_data[Rule8e==1,q28toq31], MARGIN=2, table)

## f >If Q27h = 1 â€“ set Q28 to Q32 to blank.####
#Impact check
Rule8f <-0
Rule8f <- ifelse(contractor_data$q27h ==1 & rowSums(contractor_data[q28toq32i]) >0,1,0)
table(Rule8f)

#Check frequencies before implementing rule
apply(contractor_data[Rule8f==1,q28toq32i], MARGIN=2, table)

#Implement rule
contractor_data[q28toq32i][contractor_data$q27h ==1 & rowSums(contractor_data[q28toq32i] >0),] <- 0
#Check frequencies after implementing rule
apply(contractor_data[Rule8f==1,q28toq32i], MARGIN=2, table)

#--
## g > If Q27g = 1 and Q32i = 1 â€“ set Q32i to blank.####
#Impact check
Rule8g <-0
Rule8g <- ifelse(contractor_data$q27g ==1 & contractor_data$q32i == 1,1,0)

#Check frequencies before implementing rule
table(contractor_data$q27g,contractor_data$q32i)

#Implement rule
contractor_data$q32i[contractor_data$q27g ==1 & contractor_data$q32i == 1] <- 0
#Check frequencies after implementing rule
table(contractor_data$q27g,contractor_data$q32i)

#check overall effect
social_care_section <- contractor_data %>% 
  select(qh_psid,starts_with("q27")|starts_with("q32")) %>% 
  mutate(help_indicator = case_when(q27a == 1 | q27b == 1 |q27c == 1 | q27d == 1|q27e == 1 | q27f == 1 ~ "1 Help received",
                                    q27g == 1~"2 Help needed, not received",
                                    q27h == 1~"3 No help needed",TRUE ~ "4 Not complete"),
         no_help_reason = case_when(q32a == 1|q32b == 1|q32c == 1|q32d == 1|q32e == 1|q32f == 1|q32g == 1|q32h == 1 ~ "1 Reason given",
                                    q32i == 1~ "2 Not applicable",
                                    TRUE ~ "3 Not complete"))

table(social_care_section$no_help_reason,social_care_section$help_indicator)

#===
#Rule 9: Do you look after, or give any regular help or support, to â€¦..?####
# > If Q33 not in (1,2,3,4,5) and Q34 to Q37 are not all blank â€“ set Q34 to Q37 to blank.####
#Impact check
Rule9 <-0
q34toq37 <- c("q34a","q34b","q34c","q34d","q34e","q34f","q35a","q35b","q35c","q35d","q35e","q35f","q35g","q35h",
              "q36","q37a","q37b","q37c","q37d","q37e")
Rule9  <- ifelse(contractor_data$q33  %in% c(0,6) & rowSums(contractor_data[q34toq37]) !=0,1,0)

#Check frequencies before implementing rule
apply(contractor_data[Rule9 ==1,q34toq37], MARGIN=2, table)

#Implement rule
contractor_data[q34toq37][contractor_data$q33  %in% c(0,6) & any(contractor_data[q34toq37] !=0),] <- 0
#Check frequencies after implementing rule
apply(contractor_data[Rule9 ==1,q34toq37], MARGIN=2, table)

#===
#Rule 10.	Do you have any of the following?####
# > If any of Q39a to Q39j = 1 and Q39k = 1 â€“ set Q39k to blank.#### 
#Impact check
Rule10 <-0
q39atoq39j <- c("q39a","q39b","q39c","q39d","q39e","q39f","q39g","q39h","q39i","q39j")
Rule10 <- ifelse(rowSums(contractor_data[q39atoq39j]) > 0 & contractor_data$q39k == 1,1,0)

#Check frequencies before implementing rule
lapply(contractor_data[q39atoq39j],table,contractor_data$q39k)

#Implement rule
contractor_data$q39k[rowSums(contractor_data[q39atoq39j]) > 0 & contractor_data$q39k == 1]<-0
#Check frequencies after implementing rule
lapply(contractor_data[q39atoq39j],table,contractor_data$q39k)

#Create rule summary####
rule_names <- c("Rule2a","Rule2b","Rule3a","Rule3b","Rule4","Rule5a","Rule5b","Rule6a","Rule6b","Rule6c","Rule7a","Rule7b","Rule7c",
                "Rule8a","Rule8b","Rule8c","Rule8d","Rule8e","Rule8f","Rule9","Rule10")
rule_values <- c(sum(Rule2a),sum(Rule2b),sum(Rule3a),sum(Rule3b),sum(Rule4),sum(Rule5a),sum(Rule5b),sum(Rule6a),sum(Rule6b),sum(Rule6c),
                 sum(Rule7a),sum(Rule7b),sum(Rule7c),sum(Rule8a),sum(Rule8b),sum(Rule8c),sum(Rule8d),sum(Rule8e),sum(Rule8f),
                 sum(Rule9),sum(Rule10))
rules_summary <- data.frame("Rules" = rule_names,"Records affected" = rule_values)

#save out rule summary
write.xlsx(rules_summary, file = "output/analysis_output/rules_summary.xlsx")

#Drop the comments columns which are all blank
contractor_data$q18 <- NULL
contractor_data$q26 <- NULL

#check if the same as before
hist.file <- readRDS("data/Results/data_Validated_results.rds")
all.equal(hist.file,contractor_data)

#Save outfile####
saveRDS(contractor_data, file="data/Results/data_Validated_results.rds")

#Save outfile as xlsx####
write.xlsx(contractor_data, file="data/Results/data_Validated_results.xlsx")

#29/01/2024 then 06/02/2024 Create anonymised version of validated results for SG:
SGFile_Validated_Prov <- contractor_data

#Remove PSID(QH patient identifier) & PatientID (PHS patient identifier)
SGFile_Validated_Prov$qh_psid <- NULL
SGFile_Validated_Prov$patientid <- NULL

#Save out anonymised version of unvalidated data for SG
#check if the same as before
hist.file <- readRDS("data/Results/anonymised_data_Validated_results_for_SG.rds")
identical(hist.file,SGFile_Validated_Prov)
saveRDS(SGFile_Validated_Prov, file="data/Results/anonymised_data_Validated_results_for_SG.rds")

#Save out anonymised version of validated data for SG as csv
write_excel_csv(SGFile_Validated_Prov, "data/Results/anonymised_data_Validated_results_for_SG.csv") 



