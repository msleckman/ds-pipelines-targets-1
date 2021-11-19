library(targets)

source("data_extraction_ms.R")
source("ModelPlotting_ms.R")

tar_option_set(packages = c("tidyverse", "sbtools", "whisker","readr","dplyr","stringr"))

list(
 # define global vars   
   tar_target(file_name, 'model_RMSEs.csv'),
   tar_target(sb_id_sciencebase, '5d925066e4b0c4f70d0d0599'),
   tar_target(downloaded_data_folder, '1_fetch/out'),
   tar_target(process_out_path, '2_process/out'),
   tar_target(downloaded_data_path, file.path(downloaded_data_folder,file_name)),

 # Get the data from ScienceBase
  tar_target(
    downloaded_data_path,
    data_extraction(filename_output_csv = file_name, 
                    downloaded_data_location = downloaded_data_folder,
                    selected_id_sciencebase = sb_id_sciencebase),
    format = "file"
  ), 

 # Prepare the data for plotting
  tar_target(
    eval_data,
    prep_model_data(output_data_path = process_out_path,
                    data_file_path = downloaded_data_path,
                    data_col_types = 'iccd')
  ),  
 
 # Save processed df
 tar_target(
   model_summary_results_csv,
   write_processed_data(data = eval_data, output_data_location = process_out_path, saved_data_name =  'model_summary_results.csv'),
   format = "file"
 ),
 
 # Create a plot
  tar_target(
    figure_1_png,
    plot_model_data(data = eval_data,
                    output_plot_location = process_out_path,
                    fig_name ='figure1'), 
    format = "file"
  ),

 # Save model diagnostics 
  tar_target(
    model_diagnostic_text_txt,
    render_data_diag(data = eval_data,
                     output_path = process_out_path),
    format = "file"
  )
)

