# Download original bruno's Scripts

# 01/02 Script: save_data.R
# save_data.R: https://is.gd/7PhUjd
# https://raw.githubusercontent.com/b-rodrigues/rap4all/master/scripts/save_data.R

# 1.1 Get file name
save_data <- "https://raw.githubusercontent.com/b-rodrigues/rap4all/master/scripts/save_data.R"
file_name <- basename(save_data) 
file_name
# 1.2 Download it to current directory
# downloads to current directory:
download.file(save_data, basename(save_data))


