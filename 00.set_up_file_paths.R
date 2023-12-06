# Name of file: 00-setup_file_paths.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content: Set up file paths to be used throughout project
# 
# Approximate run time: <1 min
# 
# Approximate memory usage: 245mib

lookup_path   <- if_else(substr(getwd(),2,5) == "conf","/conf/bss/pat-exp-surveys/health-and-care/202324/lookups/","someothernetwork/lookups/")
data_path     <- if_else(substr(getwd(),2,5) == "conf","/conf/bss/pat-exp-surveys/health-and-care/202324/data/","someothernetwork/data/")
output_path   <- if_else(substr(getwd(),2,5) == "conf","/conf/bss/pat-exp-surveys/health-and-care/202324/output/","someothernetwork/output/")
historical_data_path <- if_else(substr(getwd(),2,5) == "conf","/conf/bss/pat-exp-surveys/health-and-care/201920/Tableau/Datafiles/","someothernetwork/output/")


