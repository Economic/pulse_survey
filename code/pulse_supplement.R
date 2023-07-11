### DOWNLOAD HH PULSE SURVEY DATA ####
###note: only uncomment if data has not been downloaded to the data folder in the root project
#note: adjust url based on file downloading
#       1. 2020 vs 2021 data year
#       2. 1-9 has leading zero in .zip nomenclature, but not in folder nomenclature
#       3. data dictionary file nomenclature changed (due to revisions): file 12-18 has *_updated.12.2020.xslx tag
pulse_supplement_fun <- function(x) {
  #download zipped file
  system(paste('wget -N --progress=bar:force', paste0("https://www2.census.gov/programs-surveys/demo/datasets/hhp/2022/wk",x,"/HPS_Week",x, "_PUF_CSV.zip"), '-P data/'))
  
  #unzip file
  unzip(here(paste0("data/HPS_Week",x,"_PUF_CSV.zip")), exdir = here("data"))
  
  #remove zipped file from "suppdata" folder
  unlink(here(paste0("data/HPS_Week",x,"_PUF_CSV.zip")))
  
  # move files to better organize data
  file.move(here(paste0("data/pulse2022_puf_",x,".csv")), here("data/puf_csv"))
  file.move(here(paste0("data/pulse2022_repwgt_puf_",x,".csv")), here("data/repwgt_puf_csv"))
  file.move(here(paste0("data/pulse2022_data.dictionary_CSV_",x,".xlsx")), here("data/data_dictionary"))
  
}

# quietly run pulse supplement download function

walk(41, pulse_supplement_fun)

