################################
# Model_RMSE data extraction   #
#                              #
# Author:                      #     
# Date:                        #
################################


## Libs - Commented out for targets
# library(dplyr)
# library(readr)
# library(sbtools)

## Check that you are in project home directory - should be in project home directory - Commented out for Targets 
# print(getwd())

## Global Variables - Commented out for targets
# downloaded_data_folder <- '1_fetch/out'
# file_name <- 'model_RMSEs.csv'
# sb_id_sciencebase <- '5d925066e4b0c4f70d0d0599'
 

### FUNCTION 1 - download the right data from Sciencebase. This function returns the path to the downloaded data 

data_extraction <- function(filename_output_csv,
                            download_data_location,
                            selected_id_sciencebase,
                            sb_filenames = 'me_RMSE.csv') {
  # print()
  ## Set output path + path
  downloaded_data_path <- file.path(download_data_location,
                                    filename_output_csv)
  
  ## Get the file from Science Base
  sbtools::item_file_download(sb_id = selected_id_sciencebase,
                              names = sb_filenames,
                              destinations = downloaded_data_path,
                              overwrite_file = TRUE)
  print(downloaded_data_path)
  ## Check if it worked - file download and location check check
  if (file.exists(downloaded_data_path) == T) {
    print(paste('Downloaded file exists in', downloaded_data_path))
  } else{
    print(paste('The file does not exist in the designated folder:', downloaded_data_path))
  }
  
  # print(downloaded_data_path)
  return (downloaded_data_path) 
  
}

### RUN ###

# downloaded_data_path <- data_extraction(filename_output_csv = file_name,
#                                         download_data_location = downloaded_data_folder,
#                                         selected_id_sciencebase = sb_id_sciencebase)
