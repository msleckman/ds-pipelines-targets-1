##########################
# Model_RMSE data Plot   #
#                        #
# Author:                #     
# Date:                  #
##########################


## Libs
library(dplyr)
library(readr)
library(stringr)
library(sbtools)
library(whisker)


## Check that you are in project home directory - should be in project home directory
print(getwd())

## Global Variables:

in_path = '1_fetch/src' 
out_path = '1_fetch/out'
csv_filename_output = 'model_RMSEs.csv'
sb_id_sciencebase = '5d925066e4b0c4f70d0d0599'

### how to run the 4 functions 

# downloaded_data_path <- data_extraction(filename_output_csv = csv_filename_output,
#                                         download_data_location = in_path,
#                                         selected_id_sciencebase = sb_id_sciencebase)
# 
# data <- prep_model_data(downloaded_data_path,
#                         cleaned_data_location = out_path)
# 
# plot_model_data(data,
#                 output_plot_location = out_path)
# 
# render_data_diag(data)
# 

###

### FUNCTION 1 - download the right data from Sciencebase. This function returns the path to the downloaded data 

data_extraction <- function(filename_output_csv,
                            download_data_location,
                            selected_id_sciencebase,
                            sb_filenames = 'me_RMSE.csv') {
  
  ## Set output path + path
  downloaded_data_path <- file.path(download_data_location,
                                    filename_output_csv)
  
  ## Get the file from Science Base
  sbtools::item_file_download(sb_id = selected_id_sciencebase,
                              names = sb_filenames,
                              destinations = downloaded_data_path,
                              overwrite_file = TRUE)
  
  ## Check if it worked - file download and location check check
    if (file.exists(downloaded_data_path) == T) {
    print(paste('Downloaded file exists in', downloaded_data_path))
  } else{
    print(paste('The file does not exist in the designated folder:', downloaded_data_path))
  }
  
  # print(downloaded_data_path)
  return (downloaded_data_path) 

  }


### FUNCTION 2 - Prepare the data for plotting. Entails cleaning the downloading data and saving it to the appropriate output folder. 
### This function returns the clean data, which can then be inputted into the next plot function

# prepare the data for plotting

prep_model_data <- function(downloaded_data_file_path,
                            cleaned_data_location,
                            data_col_types = 'iccd',
                            save_processed_data = TRUE) {
  
  ## read and clean data 
  eval_data <- readr::read_csv(downloaded_data_file_path, col_types = data_col_types) %>%
  filter(str_detect(exper_id, 'similar_[0-9]+')) %>%
  mutate(col = case_when(
    model_type == 'pb' ~ '#1b9e77',
    model_type == 'dl' ~'#d95f02',
    model_type == 'pgdl' ~ '#7570b3'),
    pch = case_when(
    model_type == 'pb' ~ 21,
    model_type == 'dl' ~ 22,
    model_type == 'pgdl' ~ 23),
    n_prof = as.numeric(str_extract(exper_id, '[0-9]+')))

  # save the processed data
  if(save_processed_data == TRUE){
    
    readr::write_csv(eval_data, file = file.path(cleaned_data_location, 'model_summary_results.csv'))
    
  }
  
  return(eval_data)
  
  }

### Function 3 - plots the data processed above and saved it in the appropriate out folder 
### Not returns on this function, as we need nothing out of this function for this script 

plot_model_data <- function(data, output_plot_location, fig_name ='figure1'){
 
   png(file = file.path(output_plot_location, paste0(fig_name, '.png')),
      width = 8, height = 10, res = 200, units = 'in')
  par(omi = c(0,0,0.05,0.05), mai = c(1,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)
  
  plot(NA, NA, xlim = c(2, 1000), ylim = c(4.7, 0.75),
       ylab = "Test RMSE (°C)", xlab = "Training temperature profiles (#)", log = 'x', axes = FALSE)
  
  n_profs <- c(2, 10, 50, 100, 500, 980)
  
  axis(1, at = c(-100, n_profs, 1e10), labels = c("", n_profs, ""), tck = -0.01)
  axis(2, at = seq(0,10), las = 1, tck = -0.01)
  
  # slight horizontal offsets so the markers don't overlap:
  offsets <- data.frame(pgdl = c(0.15, 0.5, 3, 7, 20, 30)) %>%
    mutate(dl = -pgdl, pb = 0, n_prof = n_profs)

  for (mod in c('pb','dl','pgdl')){
    mod_data <- filter(data, model_type == mod)
    mod_profiles <- unique(mod_data$n_prof)
    for (mod_profile in mod_profiles){
      d <- filter(mod_data, n_prof == mod_profile) %>% summarize(y0 = min(rmse), y1 = max(rmse), col = unique(col))
      x_pos <- offsets %>% filter(n_prof == mod_profile) %>% pull(!!mod) + mod_profile
      lines(c(x_pos, x_pos), c(d$y0, d$y1), col = d$col, lwd = 2.5)
    }
    d <- group_by(mod_data, n_prof) %>% summarize(y = mean(rmse), col = unique(col), pch = unique(pch)) %>%
      rename(x = n_prof) %>% arrange(x)
    
    lines(d$x + tail(offsets[[mod]], nrow(d)), d$y, col = d$col[1], lty = 'dashed')
    points(d$x + tail(offsets[[mod]], nrow(d)), d$y, pch = d$pch[1], col = d$col[1], bg = 'white', lwd = 2.5, cex = 1.5)
  
    }

    points(2.2, 0.79, col = '#7570b3', pch = 23, bg = 'white', lwd = 2.5, cex = 1.5)
    text(2.3, 0.80, 'Process-Guided Deep Learning', pos = 4, cex = 1.1)
    
    points(2.2, 0.94, col = '#d95f02', pch = 22, bg = 'white', lwd = 2.5, cex = 1.5)
    text(2.3, 0.95, 'Deep Learning', pos = 4, cex = 1.1)
    
    points(2.2, 1.09, col = '#1b9e77', pch = 21, bg = 'white', lwd = 2.5, cex = 1.5)
    text(2.3, 1.1, 'Process-Based', pos = 4, cex = 1.1)
    
    dev.off()
}

### Function 4 - Render_data_diag takes the cleaned/processed data used for the plot and prints the model diagnostics 
### This function no return

render_data_diag <- function(data, output_path = out_path){
  
# Save the model diagnostics
render_data <- list(pgdl_980mean = filter(data, model_type == 'pgdl', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    dl_980mean = filter(data, model_type == 'dl', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    pb_980mean = filter(data, model_type == 'pb', exper_id == "similar_980") %>% pull(rmse) %>% mean %>% round(2),
                    dl_500mean = filter(data, model_type == 'dl', exper_id == "similar_500") %>% pull(rmse) %>% mean %>% round(2),
                    pb_500mean = filter(data, model_type == 'pb', exper_id == "similar_500") %>% pull(rmse) %>% mean %>% round(2),
                    dl_100mean = filter(data, model_type == 'dl', exper_id == "similar_100") %>% pull(rmse) %>% mean %>% round(2),
                    pb_100mean = filter(data, model_type == 'pb', exper_id == "similar_100") %>% pull(rmse) %>% mean %>% round(2),
                    pgdl_2mean = filter(data, model_type == 'pgdl', exper_id == "similar_2") %>% pull(rmse) %>% mean %>% round(2),
                    pb_2mean = filter(data, model_type == 'pb', exper_id == "similar_2") %>% pull(rmse) %>% mean %>% round(2))

template_1 <- 'resulted in mean RMSEs (means calculated as average of RMSEs from the five dataset iterations) of {{pgdl_980mean}}, {{dl_980mean}}, and {{pb_980mean}}°C for the PGDL, DL, and PB models, respectively.
  The relative performance of DL vs PB depended on the amount of training data. The accuracy of Lake Mendota temperature predictions from the DL was better than PB when trained on 500 profiles 
  ({{dl_500mean}} and {{pb_500mean}}°C, respectively) or more, but worse than PB when training was reduced to 100 profiles ({{dl_100mean}} and {{pb_100mean}}°C respectively) or fewer.
  The PGDL prediction accuracy was more robust compared to PB when only two profiles were provided for training ({{pgdl_2mean}} and {{pb_2mean}}°C, respectively). '

whisker.render(template_1 %>% str_remove_all('\n') %>% str_replace_all('  ', ' '), render_data ) %>% cat(file = file.path(output_path, 'model_diagnostic_text.txt'))


}


### RUN ###

downloaded_data_path <- data_extraction(filename_output_csv = csv_filename_output,
                                        download_data_location = in_path,
                                        selected_id_sciencebase = sb_id_sciencebase)

eval_data = prep_model_data(downloaded_data_file_path = downloaded_data_path,
                            cleaned_data_location = out_path)

Plot_model_data(data = eval_data, output_plot_location = out_path)

render_data_diag(data = eval_data)

