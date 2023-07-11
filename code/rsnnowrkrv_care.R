### PEOPLE NOT AT WORK FOR CARE RELATED REASONS #### 
# REQUEST FROM: MAROKEY SAWO (01/2022)
# week x-walk
date_xwalk <- read.csv(here("data/date_xwalk.csv")) %>% 
  mutate(stdate = as.POSIXct(stdate), endate = as.POSIXct(endate), id = 1)

# import CDC data 
#note: manually downloaded
cdc_data <- read.csv(here("data/covid_cases_daily.csv")) %>% 
  mutate(date = as.POSIXct(date), id = 1)

# merge week number onto CDC data
#note: no straight-forward way, use SQL syntax 
covid_cases <- sqldf("SELECT date_xwalk.week, date, new_cases, stdate, endate
  FROM date_xwalk LEFT JOIN cdc_data 
  ON date_xwalk.id = cdc_data.id AND date BETWEEN stdate AND endate") %>% 
  # pull average across PUF week
  group_by(week) %>% 
  summarise(mean = mean(new_cases, na.rm = TRUE)) %>% 
  left_join(date_xwalk, by = "week") %>% 
  # convert cases to millions
  mutate(covid_cases = mean/1000000) %>% 
  select(endate, covid_cases)

# read in pulse survey data for 2020-2021
pulse_data_df1 <- list_rbind(map(str_pad(1:21, width = 2, pad = 0), ~ read.csv(here(paste0("data/puf_csv/pulse2020_puf_", .x,".csv")))))

pulse_data_df2 <- list_rbind(map(22:40, ~ read.csv(here(paste0("data/puf_csv/pulse2021_puf_", .x,".csv")))))

# append pulse data and restrict to 16+
pulse_data <- pulse_data_df1 %>% 
  bind_rows(., pulse_data_df2) %>% 
  # clean names and crosswalk PUF week number
  clean_names() %>% left_join(date_xwalk, by = "week") %>% 
  # filter 16+
  filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk, # rsnnowrkrv ~ rsnnowrk in prior weeks
    TRUE ~ rsnnowrkrv),
    # assert date class
    endate = as.Date(endate))


#### SUMMARY STATISTICS ####
rsnnowrkrv_care_df <- pulse_data %>% 
  # isolate people not at work for childcare or elderly care responsibilities
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  # perfrom weight pop counts by PUF week
  group_by(endate) %>% tally(wt = pweight) %>% 
  arrange(endate) %>% 
  # convert to millions
  mutate(n = n/1000000) %>% 
  # merge covid cases
  left_join(covid_cases, by = "endate") %>% 
  # write to .csv
  write.csv(here("output/rsnnowrkrv_care.csv"))

### BY DEMOGRAPHICS ####
# add worksheet function 
sheet_fun <- function(data, wb, s) {
  # add worksheet and map data
  addWorksheet(wb, sheetName = paste0(s))
  writeData(wb, sheet = paste0(s), x = data)
  
  # assign column widths and data formatting
  setColWidths(wb, sheet = paste0(s), cols = 2:ncol(data), widths = 15)
  
  # check if date is a column and format the first column as date
  if (any(colnames(data) %in% c("endate"))) {
    # format date
    options("openxlsx.dateFormat" = "mmm-yyyy")
    addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = "DATE"), rows = 2:nrow(data)+1, cols = 1)
  }
}

# set wb object for assignment
rsnnowrkrv_care_demo_wb <- createWorkbook()

# by race/ethnicity
wbhao_df <- pulse_data  %>% 
  # define race/ethnicity variables
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>%
  # isolate people not at work for care related reasons
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  # share of people not at work ... by PUF week across race/ethnicity
  crosstab(endate, wbhao, w = pweight, row = TRUE) %>% 
  select(endate, white, black, hispanic, asian, other) %>% 
  # write to wb sheet
  sheet_fun(wb = rsnnowrkrv_care_demo_wb, s = "wbhao")

# by age
age_df <- pulse_data %>% 
  # isolate people not at work for care related reasons
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  # generate age category variable
  mutate(agecat = case_when(
    tbirth_year >= 1998 ~ "18–24",
    tbirth_year < 1998 & tbirth_year >= 1988 ~ "25–34",
    tbirth_year < 1988 & tbirth_year >= 1978 ~ "35–44",
    tbirth_year < 1978 & tbirth_year >= 1968 ~ "45–54",
    tbirth_year < 1968 & tbirth_year >= 1958 ~ "55–64",
    tbirth_year < 1958 ~ "65+")) %>% 
  # share of people not at work ... by PUF week across age category
  crosstab(endate, agecat, w = pweight, row = TRUE) %>% 
  # write to wb sheet
  sheet_fun(wb = rsnnowrkrv_care_demo_wb, s = "age")

# by gender
gender_df <- pulse_data %>% 
  # isolate people not at work for care related reasons
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(4, 5), rsnnowrkrv %in% c(3, 4))) %>% 
  # generate sex variable using gender binary smh
  mutate(gender = case_when(
    egender == 1 | egenid_birth == 1 ~ "male", egender == 2 | egenid_birth == 2 ~ "female")) %>% 
  # share of people not at work ... by PUF week across sex
  crosstab(endate, gender, w = pweight, row = TRUE) %>% 
  # write to wb sheet
  sheet_fun(wb = rsnnowrkrv_care_demo_wb, s = "gender")

# save wb object to disk
saveWorkbook(wb, here("output/rsnnowrkrv_care_demo.xlsx"), overwrite = TRUE)

### CUT SHARES ACROSS RESPONSE RATE ####
# define wb object
rsnnowrkrv_black_demo_wb <- createWorkbook()

# difficulty reading across wbhao due to sharp differences in overall population, 
# look within race
rsnnowrkrv_care_black <- pulse_data  %>% 
  # define new rsnnowrkrv variable
  #note: combine certain responses together (i.e. not at work Covid-related)
  mutate(rsnnowrkrv_new = case_when(
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
  # define race/ethnicity variable
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  # filter Black workers
  filter(wbhao == "black")

# sample size of reasons not at work for Black workers by PUF week
rsnnowrkrv_care_black_pop <- rsnnowrkrv_care_black %>% 
  crosstab(endate, rsnnowrkrv_new) %>% 
  select(endate, "I did not want to be employed at this time", 
         "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
         "I am/was caring for children not in school or daycare", "I am/was caring for an elderly person",
         "I was concerned about getting or spreading the coronavirus", "I am/was sick (not coronavirus related) or disabled",
         "I am retired", "I am/was laid off or furloughed due to coronavirus pandemic", 
         "My employer closed temporarily due to the coronavirus pandemic", "My employer went out of business due to the coronavirus pandemic",
         "I do/did not have transportation to work", "Other", "NA") %>% 
  sheet_fun(wb = rsnnowrkrv_black_demo_wb, s = "rsnnowrk_black_sample")

# share of Black workers not at work by reasons not at work across PUF week
rsnnowrkrv_care_black_share <- rsnnowrkrv_care_black   %>% 
  crosstab(endate, rsnnowrkrv_new, w = pweight, row = TRUE) %>% 
  select(endate, "I did not want to be employed at this time", 
         "I am/was sick with coronavirus symptoms or was caring for someone who was sick with coronavirus symptoms",
         "I am/was caring for children not in school or daycare", "I am/was caring for an elderly person",
         "I was concerned about getting or spreading the coronavirus", "I am/was sick (not coronavirus related) or disabled",
         "I am retired", "I am/was laid off or furloughed due to coronavirus pandemic", 
         "My employer closed temporarily due to the coronavirus pandemic", "My employer went out of business due to the coronavirus pandemic",
         "I do/did not have transportation to work", "Other", "NA") %>% 
  sheet_fun(wb = rsnnowrkrv_black_demo_wb, s = "rsnnowrk_black_shares")

# write wb to disk
saveWorkbook(wb, here("output/rsnnowrkrv_black_demo.xlsx"), overwrite = TRUE)