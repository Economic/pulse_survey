# week x-walk
date_xwalk <- read.csv(here("data/date_xwalk.csv")) %>% 
  mutate(stdate = as.POSIXct(stdate), endate = as.POSIXct(endate), id = 1)

cdc_data <- read.csv(here("data/covid_cases_daily.csv")) %>% 
  mutate(date = as.POSIXct(date), id = 1)

covid_cases <- sqldf("SELECT date_xwalk.week, date, new_cases, stdate, endate
  FROM date_xwalk LEFT JOIN cdc_data 
  ON date_xwalk.id = cdc_data.id AND date BETWEEN stdate AND endate") %>% 
  group_by(week) %>% 
  summarise(mean = mean(new_cases, na.rm = TRUE)) %>% 
  left_join(date_xwalk, by = "week") %>% 
  mutate(covid_cases = mean/1000000) %>% 
  select(endate, covid_cases)

# read in pulse survey data for 2020
pulse_data_df1 <- map_dfr(str_pad(1:21, width = 2, pad = 0), ~ read.csv(here(paste0("data/puf_csv/pulse2020_puf_", .x,".csv"))))

pulse_data_df2 <- map_dfr(22:40, ~ read.csv(here(paste0("data/puf_csv/pulse2021_puf_", .x,".csv"))))

pulse_data <- pulse_data_df1 %>% 
  bind_rows(., pulse_data_df2) %>% 
  clean_names() %>% left_join(date_xwalk, by = "week")

# create date df to fill in missing weeks
ts <- seq.POSIXt(as.POSIXlt("2020-05-05"), as.POSIXlt("2021-12-13"), by = "week")
ts <- seq(as.Date(paste0("2020-05-05")), as.Date("2021-12-13"), by = "week")
df <- data.frame(endate = ts)

rsnnowrkrv_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  arrange(endate) %>% 
  mutate(n = n/1000000) %>% 
  add_row(endate = as.Date(paste0("2022-01-10")), n = 8.75) %>% 
  left_join(covid_cases, by = "endate") %>% 
  full_join(df, by = "endate") %>% 
  pivot_longer(cols = c("covid_cases", "n"), names_to = "names", values_to = "values")

write.csv(rsnnowrkrv_df, here("output/rsnnowrkrv_care.csv"))

wbhao_df <- pulse_data  %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  crosstab(endate, wbhao, w = pweight, row = TRUE) %>% 
  select(endate, white, black, hispanic, asian, other)

age_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  mutate(agecat = case_when(
    tbirth_year >= 1998 ~ "18–24",
    tbirth_year < 1998 & tbirth_year >= 1988 ~ "25–34",
    tbirth_year < 1988 & tbirth_year >= 1978 ~ "35–44",
    tbirth_year < 1978 & tbirth_year >= 1968 ~ "45–54",
    tbirth_year < 1968 & tbirth_year >= 1958 ~ "55–64",
    tbirth_year < 1958 ~ "65+")) %>% 
  crosstab(endate, agecat, w = pweight, row = TRUE)

gender_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  mutate(gender = case_when(
    egender == 1 | egenid_birth == 1 ~ "male", egender == 2 | egenid_birth == 2 ~ "female")) %>% 
  crosstab(endate, gender, w = pweight, row = TRUE)

wb <- createWorkbook()

addWorksheet(wb, sheetName = "wbhao")
writeData(wb, x = wbhao_df, sheet = "wbhao", startRow = 1, startCol = 1)

addWorksheet(wb, sheetName = "age")
writeData(wb, x = age_df, sheet = "age", startRow = 1, startCol = 1)

addWorksheet(wb, sheetName = "gender")
writeData(wb, x = gender_df, sheet = "gender", startRow = 1, startCol = 1)

saveWorkbook(wb, here("output/rsnnowrkrv_care_demo.xlsx"), overwrite = TRUE)

rsnnowrkrv_care_black_pop <- pulse_data  %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrk = as.character(rsnnowrk), rsnnowrkrv = as.character(rsnnowrkrv),
    rsnnowrkrv_new = case_when(
      rsnnowrk == "1" | rsnnowrkrv == "1" ~ "I did not want to be employed at this time",
      rsnnowrk %in% c("2", "3") | rsnnowrkrv == "2" ~ "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
      rsnnowrk == "4" | rsnnowrkrv == "3" ~ "I am/was caring for children not in school or daycare",
      rsnnowrk == "5" | rsnnowrkrv == "4" ~ "I am/was caring for an elderly person",
      rsnnowrkrv == "5" ~ "I was concerned about getting or spreading the coronavirus",
      rsnnowrk == "6" | rsnnowrkrv == "6" ~ "I am/was sick (not coronavirus related) or disabled",
      rsnnowrk == "7" | rsnnowrkrv == "7" ~ "I am retired",
      rsnnowrk == "8" | rsnnowrkrv == "8" ~ "I am/was laid off or furloughed due to coronavirus pandemic",
      rsnnowrk == "9" | rsnnowrkrv == "9" ~ "My employer closed temporarily due to the coronavirus pandemic",
      rsnnowrk == "10" | rsnnowrkrv == "10" ~ "My employer went out of business due to the coronavirus pandemic",
      rsnnowrk == "11" | rsnnowrkrv == "11" ~ "I do/did not have transportation to work",
      rsnnowrk == "12" | rsnnowrkrv == "12" ~ "Other",
      rsnnowrk %in% c("-88", "-99") | rsnnowrkrv %in% c("-88", "-99") ~ as.character(NA_real_))) %>% 
  mutate(endate = as.Date(endate)) %>% 
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  filter(wbhao == "black") %>% 
  crosstab(endate, rsnnowrkrv_new) %>% 
  select(endate, "I did not want to be employed at this time", 
         "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
         "I am/was caring for children not in school or daycare", "I am/was caring for an elderly person",
         "I was concerned about getting or spreading the coronavirus", "I am/was sick (not coronavirus related) or disabled",
         "I am retired", "I am/was laid off or furloughed due to coronavirus pandemic", 
         "My employer closed temporarily due to the coronavirus pandemic", "My employer went out of business due to the coronavirus pandemic",
         "I do/did not have transportation to work", "Other", "NA")

rsnnowrkrv_care_black_share <- pulse_data  %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrk = as.character(rsnnowrk), rsnnowrkrv = as.character(rsnnowrkrv),
         rsnnowrkrv_new = case_when(
           rsnnowrk == "1" | rsnnowrkrv == "1" ~ "I did not want to be employed at this time",
           rsnnowrk %in% c("2", "3") | rsnnowrkrv == "2" ~ "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
           rsnnowrk == "4" | rsnnowrkrv == "3" ~ "I am/was caring for children not in school or daycare",
           rsnnowrk == "5" | rsnnowrkrv == "4" ~ "I am/was caring for an elderly person",
           rsnnowrkrv == "5" ~ "I was concerned about getting or spreading the coronavirus",
           rsnnowrk == "6" | rsnnowrkrv == "6" ~ "I am/was sick (not coronavirus related) or disabled",
           rsnnowrk == "7" | rsnnowrkrv == "7" ~ "I am retired",
           rsnnowrk == "8" | rsnnowrkrv == "8" ~ "I am/was laid off or furloughed due to coronavirus pandemic",
           rsnnowrk == "9" | rsnnowrkrv == "9" ~ "My employer closed temporarily due to the coronavirus pandemic",
           rsnnowrk == "10" | rsnnowrkrv == "10" ~ "My employer went out of business due to the coronavirus pandemic",
           rsnnowrk == "11" | rsnnowrkrv == "11" ~ "I do/did not have transportation to work",
           rsnnowrk == "12" | rsnnowrkrv == "12" ~ "Other",
           rsnnowrk %in% c("-88", "-99") | rsnnowrkrv %in% c("-88", "-99") ~ as.character(NA_real_))) %>% 
  mutate(endate = as.Date(endate)) %>% 
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  filter(wbhao == "black") %>% 
  crosstab(endate, rsnnowrkrv_new, w = pweight, row = TRUE) %>% 
  select(endate, "I did not want to be employed at this time", 
         "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
         "I am/was caring for children not in school or daycare", "I am/was caring for an elderly person",
         "I was concerned about getting or spreading the coronavirus", "I am/was sick (not coronavirus related) or disabled",
         "I am retired", "I am/was laid off or furloughed due to coronavirus pandemic", 
         "My employer closed temporarily due to the coronavirus pandemic", "My employer went out of business due to the coronavirus pandemic",
         "I do/did not have transportation to work", "Other", "NA")

wb <- createWorkbook()

addWorksheet(wb, sheetName = "rsnnowrk_black_sample")
addWorksheet(wb, sheetName = "rsnnowrk_black_shares")

writeData(wb, sheet = "rsnnowrk_black_sample", x = rsnnowrkrv_care_black_pop)
writeData(wb, sheet = "rsnnowrk_black_shares", x = rsnnowrkrv_care_black_share)

saveWorkbook(wb, here("output/rsnnowrkrv_black_demo.xlsx"), overwrite = TRUE)