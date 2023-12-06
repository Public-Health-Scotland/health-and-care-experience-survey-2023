# Written by x x x
# November 2023.
# Adapted from January 2022 code by Catriona Haddow
# 
# *****************************************
#Purpose: Create information for each question

#to do: is it only 'tableau' comparisons that are needed?

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

# load in libraries
library(tidyverse)
#version 1.3.2
library(readxl)
#version 1.4.2

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324")

#Read in document
Question_lookup <- read_xlsx("lookups/2022.04.22 ASDHD - Health and Care Experience Survey - 2021 - Questionnaire - mapping document - version 2.2.xlsx",
                             sheet = "HACE 2021-22",na = "", trim_ws = TRUE)
Question_lookup <- Question_lookup %>%
  mutate(across(everything(), as.character))

Question_lookup$question <- as.character(Question_lookup$`Quest.No.`) 
Question_lookup$question_text <- Question_lookup$`Question Text`

Question_lookup$`Quest. No. Prev Year` <- if_else((nchar(Question_lookup$`Quest. No. Prev Year`) == 1|substr(Question_lookup$`Quest. No. Prev Year`,1,1)==9), 
                                                  paste0("q0",Question_lookup$`Quest. No. Prev Year`),
                                                  paste0("q",Question_lookup$`Quest. No. Prev Year`))

#this need to be input as question text is not consistent
information_questions_tata <- c("q09","q14","q20","q27","q28","q32","q34","q35","q39")

table(Question_lookup$`Comparability2019-20`,useNA = c("always"))
#this is dangerous, because "no" will get mapped as option 2! Recode later.
Question_lookup$`Response Options`[substring(Question_lookup$question,1,3) %in% information_questions_tata] <- "1 = Yes; 0 = No"
question_lookup_info <- Question_lookup %>%
  filter(`Question Type` == "Information" | question == "q39")%>%
  mutate('question_2020' = if_else(`Comparability2019-20` %in% c("Tableau","Commentary"),`Quest. No. Prev Year`,""))%>%
  mutate('response_options' = str_replace_all(`Response Options`, "[:digit:] = ", "")) %>%
  separate('response_options', into = c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"), sep = ";", extra = 'drop', remove = FALSE)%>%
  pivot_longer(c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11"), values_drop_na = TRUE, names_to = 'RO', values_to = "response_text")%>%
  mutate(response_option = str_replace_all(RO,"R",""),
         surveysection = `Tableau Section`) %>%
  select(question,question_text,Weight,response_option,response_text,surveysection,question_2020)

#update the response_text for Q46 options 10 = Pagan;11 = Another religion 
#q46
question_lookup_info$response_text[(question_lookup_info$question == 'q46') & (question_lookup_info$response_option == '10')] <- "Pagan"
question_lookup_info$response_text[(question_lookup_info$question == 'q46') & (question_lookup_info$response_option == '11')] <- "Another religion"

#update the response_text from "Yes" for retained "tick all that apply" question options:
#q09
question_lookup_info$response_text[question_lookup_info$question == 'q09a'] <- "It was not at the time or on the day I wanted"
question_lookup_info$response_text[question_lookup_info$question == 'q09b'] <- "It was not the type of appointment I wanted"
question_lookup_info$response_text[question_lookup_info$question == 'q09c'] <- "The appointment wasn't soon enough"
question_lookup_info$response_text[question_lookup_info$question == 'q09d'] <- "I couldn't book ahead at my General Practice"
question_lookup_info$response_text[question_lookup_info$question == 'q09e'] <- "It wasn't with my preferred Healthcare Professional"
question_lookup_info$response_text[question_lookup_info$question == 'q09f'] <- "Another reason"
#q11
question_lookup_info$response_text[question_lookup_info$question == 'q14a'] <- "An injury or accident"
question_lookup_info$response_text[question_lookup_info$question == 'q14b'] <- "Another physical health problem"
question_lookup_info$response_text[question_lookup_info$question == 'q14c'] <- "A mental health problem"
question_lookup_info$response_text[question_lookup_info$question == 'q14d'] <- "A routine appointment"
question_lookup_info$response_text[question_lookup_info$question == 'q14e'] <- "Something else"
question_lookup_info$response_text[question_lookup_info$question == 'q14f'] <- "No treatment/advice received"
#q20
question_lookup_info$response_text[question_lookup_info$question == 'q20a'] <- "Phoned NHS24 (111)"
question_lookup_info$response_text[question_lookup_info$question == 'q20b'] <- "Used official NHS website (e.g. NHS Inform)"
question_lookup_info$response_text[question_lookup_info$question == 'q20c'] <- "Called 999 / Ambulance"
question_lookup_info$response_text[question_lookup_info$question == 'q20d'] <- "Contacted Pharmacist/Chemist"
question_lookup_info$response_text[question_lookup_info$question == 'q20e'] <- "Contacted family or friend for advice"
question_lookup_info$response_text[question_lookup_info$question == 'q20f'] <- "Other"
#q27
question_lookup_info$response_text[question_lookup_info$question == 'q27a'] <- "Yes, help for me with personal tasks"
question_lookup_info$response_text[question_lookup_info$question == 'q27b'] <- "Yes, help for me with household tasks"
question_lookup_info$response_text[question_lookup_info$question == 'q27c'] <- "Yes, help for me for activities outside my home e.g. learning, working, socialising"
question_lookup_info$response_text[question_lookup_info$question == 'q27d'] <- "Yes, help for me with adaptations, and/or equipment for my home"
question_lookup_info$response_text[question_lookup_info$question == 'q27e'] <- "Yes, an alarm service (e.g. an electronic device I wear) that can get me help"
question_lookup_info$response_text[question_lookup_info$question == 'q27f'] <- "Yes, emotional/community/peer support"
question_lookup_info$response_text[question_lookup_info$question == 'q27g'] <- "No, not had any help but I feel that I needed it"
question_lookup_info$response_text[question_lookup_info$question == 'q27h'] <- "No, I didn't need any help"
#q28
question_lookup_info$response_text[question_lookup_info$question == 'q28a'] <- "The State/Local Government"
question_lookup_info$response_text[question_lookup_info$question == 'q28b'] <- "Me/My family"
question_lookup_info$response_text[question_lookup_info$question == 'q28c'] <- "I receive unpaid care"
question_lookup_info$response_text[question_lookup_info$question == 'q28d'] <- "Other, such as a charity"
#q32
question_lookup_info$response_text[question_lookup_info$question == 'q32a'] <- "I did not know how or where to ask for help"
question_lookup_info$response_text[question_lookup_info$question == 'q32b'] <- "My current care service is not enough"
question_lookup_info$response_text[question_lookup_info$question == 'q32c'] <- "I contacted my local authority and have not heard back"
question_lookup_info$response_text[question_lookup_info$question == 'q32d'] <- "I have had an assessment, but care has not been provided"
question_lookup_info$response_text[question_lookup_info$question == 'q32e'] <- "I do not qualify for services"
question_lookup_info$response_text[question_lookup_info$question == 'q32f'] <- "I am waiting to be reassigned to a different provider"
question_lookup_info$response_text[question_lookup_info$question == 'q32g'] <- "I am not sure I am eligible"
question_lookup_info$response_text[question_lookup_info$question == 'q32h'] <- "Other"
question_lookup_info$response_text[question_lookup_info$question == 'q32i'] <- "Not applicable"
#q34
question_lookup_info$response_text[question_lookup_info$question == 'q34a'] <- "My Spouse/Partner"
question_lookup_info$response_text[question_lookup_info$question == 'q34b'] <- "My Parent/Grandparent"
question_lookup_info$response_text[question_lookup_info$question == 'q34c'] <- "My Child/Grandchild"
question_lookup_info$response_text[question_lookup_info$question == 'q34d'] <- "Relative (any other relationship)"
question_lookup_info$response_text[question_lookup_info$question == 'q34e'] <- "Friend/neighbour"
question_lookup_info$response_text[question_lookup_info$question == 'q34f'] <- "Someone else"
#q35
question_lookup_info$response_text[question_lookup_info$question == 'q35a'] <- "Help from family, friends or neighbours"
question_lookup_info$response_text[question_lookup_info$question == 'q35b'] <- "Help from Carer Centre/local organisation"
question_lookup_info$response_text[question_lookup_info$question == 'q35c'] <- "Any services or support for me personally to help me have breaks from caring"
question_lookup_info$response_text[question_lookup_info$question == 'q35d'] <- "Services provided to the person I care for, such as overnight or day services to allow me to have a break"
question_lookup_info$response_text[question_lookup_info$question == 'q35e'] <- "Other support"
question_lookup_info$response_text[question_lookup_info$question == 'q35f'] <- "No support or help, but I felt that I needed some"
question_lookup_info$response_text[question_lookup_info$question == 'q35g'] <- "No support or help, but I do not need any"

question_lookup_info$response_text[question_lookup_info$question == 'q39a'] <- "Deafness or partial hearing loss"
question_lookup_info$response_text[question_lookup_info$question == 'q39b'] <- "Blindness or partial sight loss"
question_lookup_info$response_text[question_lookup_info$question == 'q39c'] <- "Full or partial loss of voice or difficulty speaking" 
question_lookup_info$response_text[question_lookup_info$question == 'q39d'] <- "Learning disability" 
question_lookup_info$response_text[question_lookup_info$question == 'q39e'] <- "Learning difficulty" 
question_lookup_info$response_text[question_lookup_info$question == 'q39f'] <- "Developmental disorder"
question_lookup_info$response_text[question_lookup_info$question == 'q39g'] <- "Physical disability" 
question_lookup_info$response_text[question_lookup_info$question == 'q39h'] <- "Mental health condition"
question_lookup_info$response_text[question_lookup_info$question == 'q39i'] <- "Long-term illness, disease or condition"
question_lookup_info$response_text[question_lookup_info$question == 'q39j'] <- "Other condition"
question_lookup_info$response_text[question_lookup_info$question == 'q39k'] <- "No condition"

#Recode tata questions so that 0 is no.
question_lookup_info$response_option[substring(question_lookup_info$question,1,3) %in% information_questions_tata
                           & question_lookup_info$response_option == "2"] <- "0"

colnames(question_lookup_info)<-tolower(colnames(question_lookup_info)) 

#check if the same as before
hist.file <- readRDS("lookups/question_lookup_info.rds")
identical(hist.file,question_lookup_info)
#q39 picking up historical reference for 2020 (q34) when previously wasn't.

saveRDS(question_lookup_info, "lookups/question_lookup_info.rds")

question_lookup_pnn <- Question_lookup %>%
  filter(`Question Type` == "Percent positive")%>%
  mutate('question_2020' = if_else(`Comparability2019-20` %in% c("Tableau","Commentary"),`Quest. No. Prev Year`,""))%>%
  separate(`Negative Values`, into = c("Negative Values 1","Negative Values 2"), sep = ",", extra = 'drop', remove = FALSE)%>%
  pivot_longer(c(`High Positive Values`,`Low Positive Values`,`Neutral Values`,"Negative Values 1","Negative Values 2"), 
               values_drop_na = TRUE, names_to = 'RO', values_to = "response_option") %>%
  filter(response_option != "-") %>%
  mutate(pnn2 = str_replace_all(RO," Values",""), surveysection = `Tableau Section`) %>%
  mutate(response_text = if_else(grepl("Positive",pnn2) == TRUE, "Positive",if_else(grepl("Negative",pnn2) == TRUE,"Negative","Neutral")))%>%
  select(question,question_text,Weight,response_option,response_text,surveysection,question_2020)

table(question_lookup_pnn$response_option)
#temp fix
question_lookup_pnn$response_option[grepl("5",question_lookup_pnn$response_option) == TRUE] <- "5"
question_lookup_pnn$response_option[grepl("4",question_lookup_pnn$response_option) == TRUE] <- "4"
question_lookup_pnn$response_option[grepl("2",question_lookup_pnn$response_option) == TRUE] <- "2"
table(question_lookup_pnn$response_option)

colnames(question_lookup_pnn)<-tolower(colnames(question_lookup_pnn))  

#check if the same as before
hist.file <- readRDS("lookups/question_lookup_pnn.rds")
identical(hist.file,question_lookup_pnn)

file.remove("lookups/question_lookup_pnn.rds")
saveRDS(question_lookup_pnn, "lookups/question_lookup_pnn.rds")

question_lookup <- bind_rows(question_lookup_info[question_lookup_info$question != "q39",],question_lookup_pnn)

#check if the same as before
hist.file <- readRDS("lookups/question_lookup.rds")
identical(hist.file,question_lookup)
#q39 picking up historical reference for 2020 (q34) when previously wasn't.

saveRDS(question_lookup, "lookups/question_lookup.rds")

#create vectors of percent positive / information questions
percent_positive_questions <- Question_lookup$question[Question_lookup$`Question Type` == "Percent positive" & Question_lookup$question != 'ldp_48hr']
information_questions <- Question_lookup$question[Question_lookup$`Question Type` == "Information"| Question_lookup$question == "q39"]
questions <- Question_lookup$question[(Question_lookup$`Question Type` == "Percent positive"| Question_lookup$`Question Type` == "Information") & Question_lookup$question != 'ldp_48hr']

#check if the same as before
hist.file <- readRDS("lookups/questions.rds")
identical(hist.file,questions)
hist.file <- readRDS("lookups/information_questions.rds")
identical(hist.file,information_questions)
hist.file <- readRDS("lookups/percent_positive_questions.rds")
identical(hist.file,percent_positive_questions)
hist.file <- readRDS("lookups/information_questions_tata.rds")
identical(hist.file,information_questions_tata)

saveRDS(questions, "lookups/questions.rds")
saveRDS(information_questions, "lookups/information_questions.rds")
saveRDS(percent_positive_questions, "lookups/percent_positive_questions.rds")
saveRDS(information_questions_tata, "lookups/information_questions_tata.rds")
