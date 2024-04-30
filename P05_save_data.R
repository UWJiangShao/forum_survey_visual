source("C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/Program/P01_clean_data_admin.R")


file_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/all_year/hedis_star_all.csv"
write.csv(hedis_star_all, file = file_path, row.names = FALSE)

file_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/all_year/hedis_sk_all.csv"
write.csv(hedis_sk_all, file = file_path, row.names = FALSE)

file_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/all_year/hedis_sp_all.csv"
write.csv(hedis_sp_all, file = file_path, row.names = FALSE)

file_path <- "C:/Users/jiang.shao/Dropbox (UFL)/Forum Slides Making Project/data/admin data/all_year/national_level_all.csv"
write.csv(national_level_all, file = file_path, row.names = FALSE)
