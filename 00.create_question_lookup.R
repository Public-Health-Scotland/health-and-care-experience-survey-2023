# Written by x x x
# November 2023.
# Adapted from January 2022 code by Catriona Haddow
# 
# *****************************************
#Purpose: Create information for each question


#Inputs: #UPDATE!
#"lookups/2022.04.19 ASDHD - Health and Care Experience Survey - 2021 - Questionnaire - mapping document - version 2.2.xlsx"

#Outputs: #UPDATE!
#"lookups/question_lookup_info.rds"
#"lookups/question_lookup_pnn.rds"
#"lookups/question_lookup.rds"
#"lookups/questions.rds"
#"lookups/information_questions.rds"
#"lookups/percent_positive_questions.rds"
#"lookups/information_questions_tata.rds"

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")

#Read in document
Question_lookup <- read_xlsx(paste0(lookup_path,"ASDHD - Health and Care Experience Survey - 2023 - 2024.xlsx"),
                             sheet = "HACE 2023-24 wip",na = "", trim_ws = TRUE)
Question_lookup <- Question_lookup %>%
  mutate(across(everything(), as.character))%>%
  rename_with(tolower) %>% 
  rename(question = 'quest.no.',
         question_text = 'question text') %>% 
  mutate('quest. no. prev year' = if_else((nchar(`quest. no. prev year`) == 1|substr(`quest. no. prev year`,1,1)==9), 
                                          paste0("q0",`quest. no. prev year`),
                                          paste0("q",`quest. no. prev year`)))

table(Question_lookup$question[grepl("all that apply",Question_lookup$question_text) & nchar(Question_lookup$question) == 3],useNA = c("always"))

information_questions_tata <- c("q09","q14","q20","q27","q28","q32","q34","q35","q39") #manual input as question lookup formatting is not consistent
table(Question_lookup$`response options`[substring(Question_lookup$question,1,3) %in% information_questions_tata])
#this is dangerous, because "no" will get mapped as option 2! Recode later.

Question_lookup <- Question_lookup %>%
  mutate(tata = if_else(substring(question,1,3) %in% information_questions_tata,1,0),
        `response options` = if_else(tata == 1,
                                       paste0("1 = ",`response options`,"; 0 = No"),`response options`))

question_lookup_info <- Question_lookup %>%
  filter(`question type` == "Information" | question == "q39")%>%
  mutate('question_2022' = if_else(`comparability 2021-22` %in% c("Tableau","Commentary"),`quest. no. prev year`,""))%>%
  mutate('response_options' = str_replace_all(`response options`, "[0-9]+ = ", "")) %>%
  separate('response_options', into = c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"), sep = ";", extra = 'drop', remove = FALSE)%>%
  pivot_longer(c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"), values_drop_na = TRUE, names_to = 'RO', values_to = "response_text")%>%
  mutate(response_option = str_replace_all(RO,"R",""),
         surveysection = `tableau section`) %>%
  mutate(response_text = str_replace(response_text,", please write in:",""),
         response_text = str_replace(response_text,"^ ",""),
         response_option = if_else(tata == 1 & response_option == "2","0",response_option)) %>% #Recode tata questions so that 0 is no.
  select(question,question_text,weight,response_option,response_text,surveysection,question_2022)

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"question_lookup_info.rds"))
identical(hist.file,question_lookup_info)

saveRDS(question_lookup_info, paste0(lookup_path,"question_lookup_info.rds"))

question_lookup_pnn <- Question_lookup %>%
  filter(`question type` == "Percent positive")%>%
  mutate(across(contains("values"),~ str_replace(.x," ",""))) %>% #remove spaces from values
  mutate('question_2022' = if_else(`comparability 2021-22` %in% c("Tableau","Commentary"),`quest. no. prev year`,""))%>%
  separate(`negative values`, into = c("negative values 1","negative values 2"), sep = ",", extra = 'drop', remove = FALSE)%>%
  pivot_longer(c(`high positive values`,`low positive values`,`neutral values`,"negative values 1","negative values 2"), 
               values_drop_na = TRUE, names_to = 'RO', values_to = "response_option") %>%
  filter(response_option != "-") %>%
  mutate(pnn2 = str_replace_all(RO," Values",""), surveysection = `tableau section`) %>%
  mutate(response_text = if_else(grepl("positive",pnn2) == TRUE, "Positive",if_else(grepl("negative",pnn2) == TRUE,"Negative","Neutral")))%>%
  select(question,question_text,weight,response_option,response_text,surveysection,question_2022)

table(question_lookup_pnn$response_option)

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"question_lookup_pnn.rds"))
identical(hist.file,question_lookup_pnn)

file.remove("lookups/question_lookup_pnn.rds")
saveRDS(question_lookup_pnn, paste0(lookup_path,"question_lookup_pnn.rds"))

question_lookup <- bind_rows(question_lookup_info[question_lookup_info$question != "q38",],question_lookup_pnn)

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"question_lookup.rds"))
identical(hist.file,question_lookup)
saveRDS(question_lookup, paste0(lookup_path,"question_lookup.rds"))

#create vectors of percent positive / information questions
percent_positive_questions <- Question_lookup$question[Question_lookup$`question type` == "Percent positive" & Question_lookup$question != '48hr_ldp']
information_questions <- Question_lookup$question[Question_lookup$`question type` == "Information"| Question_lookup$question == "q38"]
questions <- Question_lookup$question[(Question_lookup$`question type` == "Percent positive"| Question_lookup$`question type` == "Information") & Question_lookup$question != '48hr_ldp']

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"questions.rds"))
identical(hist.file,questions)
hist.file <- readRDS(paste0(lookup_path,"information_questions.rds"))
identical(hist.file,information_questions)
hist.file <- readRDS(paste0(lookup_path,"percent_positive_questions.rds"))
identical(hist.file,percent_positive_questions)
hist.file <- readRDS(paste0(lookup_path,"information_questions_tata.rds"))
identical(hist.file,information_questions_tata)

saveRDS(questions, paste0(lookup_path,"questions.rds"))
saveRDS(information_questions, paste0(lookup_path,"information_questions.rds"))
saveRDS(percent_positive_questions, paste0(lookup_path,"percent_positive_questions.rds"))
saveRDS(information_questions_tata, paste0(lookup_path,"information_questions_tata.rds"))
