##################
# Create Directory
##################
make_project_dir <- function(){
    dir_names <- c(
        "00_Data",
        "01_Scripts",
        "02_Business_Understanding",
        "03_Data_Understanding",
        "04_Data_Modelling",
        "05_Evaluation",
        "06_Dashboard_Depolyment"
    )
    dir_create(dir_names)
    
    dir_ls()
}

####################
# Delete Directories
####################
dir_delete()







