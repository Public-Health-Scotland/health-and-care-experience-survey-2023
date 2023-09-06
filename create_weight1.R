# Name of file: create_weight1.R
#
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content: Create weight1 - probability of non-selection
# 
# Approximate run time: <1 min
# 
# Approximate memory usage: 245mib

#Inputs:
#"output/weights/eligible_pats_by_gp.rds"
#"output/weights/sample_size_by_gp.rds"

#Outputs:
#"output/weights/weight1.rds"

# load in libraries
source("00.set_up_packages.R")
# read in file paths
source("00.setup_file_paths.R")

#to do: 

#Based on 2019/20 SPSS syntax: 05 - Calculate weight 1####
# Match sample size to eligible size
# Eligible practices: 907; sampled: 907.

eligible_pats_by_gp <- readRDS(paste0(data_path,"eligible_pats_by_gp.rds"))
sample_size_by_gp <- readRDS(paste0(data_path,"sample_size_by_gp.rds"))

weight1 <- full_join(eligible_pats_by_gp,sample_size_by_gp,by = c("gp_prac_no"))

#Divide the eligible patients by the sampled patients in each GP Practice to calculate the Weight 1 value.
weight1 <- weight1 %>%
  mutate(gp_wt1 = eligible_pats/sample_pop)

summary(weight1$gp_wt1)
#check max and mean weights compared with last round

#check how new file compares with exisiting file before overwriting 
hist.file <- readRDS(paste0(output_path,"weight1.rds"))
identical(hist.file,weight1)
file.remove (paste0(output_path,"weight1.rds"))

saveRDS(weight1,paste0(output_path,"weight1.rds"))



