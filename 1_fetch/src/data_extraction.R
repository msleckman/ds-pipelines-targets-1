################################
# Model_RMSE data extraction   #
#                              #
################################

### FUNCTION 1 - download the right data from Sciencebase. This function returns the path to the downloaded data 

data_extraction <- function(filename_output_csv,
                            downloaded_data_location,
                            selected_id_sciencebase,
                            sb_filenames = 'me_RMSE.csv') {
  
  ## Set output path + path
  downloaded_data_path <- file.path(downloaded_data_location,
                                    filename_output_csv)
  
  ## Get the file from Science Base
  sbtools::item_file_download(sb_id = selected_id_sciencebase,
                              names = sb_filenames,
                              destinations = downloaded_data_path,
                              overwrite_file = TRUE)
  
}
