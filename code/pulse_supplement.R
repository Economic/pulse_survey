### DOWNLOAD HH PULSE SURVEY DATA ####
###note: only uncomment if data has not been downloaded to the data folder in the root project
#note: adjust url based on file downloading
#       1. 2020 vs 2021 data year
#       2. 1-9 has leading zero in .zip nomenclature, but not in folder nomenclature
#       3. data dictionary file nomenclature changed (due to revisions): file 12-18 has *_updated.12.2020.xslx tag
pulse_supplement_fun <- function(year, week_x) {
  #download zipped file
  system(paste('wget -N --progress=bar:force', paste0("https://www2.census.gov/programs-surveys/demo/datasets/hhp/",year ,"/wk", week_x,"/HPS_Week", week_x, "_PUF_CSV.zip"), '-P data/'))
  
  #unzip file
  unzip(here(paste0("data/HPS_Week", week_x,"_PUF_CSV.zip")), exdir = here("data"))
  
  #remove zipped file from "suppdata" folder
  unlink(here(paste0("data/HPS_Week", week_x,"_PUF_CSV.zip")))
  
  # move files to better organize data
  file.move(here(paste0("data/pulse", year, "_puf_", week_x,".csv")), here("data/puf_csv"))
  file.move(here(paste0("data/pulse", year, "_repwgt_puf_", week_x,".csv")), here("data/repwgt_puf_csv"))
  file.move(here(paste0("data/pulse", year, "_data.dictionary_CSV_", week_x,".xlsx")), here("data/data_dictionary"))
  
}

# quietly run pulse supplement download function
#note: "~" syntax inside walk () is allowing week_x to be defined dynamically but hold year the same,
#      download every week for each year at a time (walking over two dynamic arguments is much more complicated)
walk(1:21, ~ pulse_supplement_fun(week_x = .x, year = 2020))
walk(22:40, ~ pulse_supplement_fun(week_x = .x, year = 2021))
walk(41:52, ~ pulse_supplement_fun(week_x = .x, year = 2022))
walk(53:58, ~ pulse_supplement_fun(week_x = .x, year = 2023))