#### WHAT TO WATCH ON JOBS DAY: OMICRON WILL WEIGHT HEAVILY ON THE LABOR MARKET ####
# REQUEST FROM: ELISE GOULD ((01/2022)
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

# read in pulse survey data for 2020-2022
#note: the function scaffolding list_rbind(map(...)) with read.csv always user to read multiple csvs and append using rbind
pulse_data_df1 <- list_rbind(map(str_pad(1:21, width = 2, pad = 0), ~ read.csv(here(paste0("data/puf_csv/pulse2020_puf_", .x,".csv")))))

pulse_data_df2 <- list_rbind(map(22:40, ~ read.csv(here(paste0("data/puf_csv/pulse2021_puf_", .x,".csv")))))

pulse_data_df3 <- list_rbind(map(41, ~ read.csv(here(paste0("data/puf_csv/pulse2022_puf_", .x, ".csv")))))

# append pulse data and restrict to 16+
pulse_data <- bind_rows(pulse_data_df1, pulse_data_df2, pulse_data_df3) %>% 
  # clean names and crosswalk PUF week number
  clean_names() %>% left_join(date_xwalk, by = "week") %>% 
  # filter 16+
  filter(tbirth_year <= 2004) %>%
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk, # rsnnowrkrv ~ rsnnowrk in prior weeks
    TRUE ~ rsnnowrkrv),
    # assert date class
    endate = as.Date(endate))

# create date df to fill in missing weeks
#note: used during linear interpolation
ts_df <- data.frame(endate = seq(as.Date(paste0("2020-05-05")), as.Date("2021-12-13"), by = "week"))

#### SUMMARY STATISTICS ####
# weighted pop count of people not working because they have/care for someone with Covid-19
rsnnowrkrv_df <- pulse_data %>% 
  # isolate people not at work for Covid-related reasons
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>% 
  # perform weighted count by week
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  arrange(endate) %>% 
  # convert to millions
  mutate(n = n/1000000) %>%
  # merge covid cases
  left_join(covid_cases, by = "endate") 
  # write to csv
  #write.csv(here("output/rsnnowrkrv.csv"))

# weighted pop of all unemployed
rsnnowrkrv_tot_df <- pulse_data %>% 
  # isolate unemployed
  filter(anywork == 2) %>%
  # perform weighted count
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  # convert to millions
  mutate(n = n/1000000) %>% 
  #full_join(df, by = "endate") %>% 
  arrange(endate) %>% 
  # write to csv
  write.csv(here("output/rsnnowrkrv_tot.csv"))

# weight pop of people not at work "other reason"
rsnnowrkrv_other_df <- pulse_data %>% 
  # isolate those not at work "other reason"
  filter(rsnnowrkrv == 12) %>% 
  # perform weighted pop by week
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  arrange(endate) %>% 
  # convert to millions 
  mutate(n = n/1000000)


#### SCATTER PLOT ####
ggplot(data = rsnnowrkrv_df %>% pivot_longer(cols = c("covid_cases", "n"), names_to = "names", 
                                             values_to = "values"), aes(x = endate, y = values, color = names)) +
  geom_point() +
  ggtitle(paste("People not working because they have or were caring for someone with Covid")) +
  xlab("Week") +
  ylab("Weighted population (millions)") +
  theme(panel.grid.minor = element_blank())


### DATA ADJSTMENTS ####
# linear interpolate missig weeks and map visually
lmRsnnowrkrv_df <- rsnnowrkrv_df %>% 
  # define missing weeks for linear interpolation
  full_join(ts_df, by = "endate") %>% 
  pivot_wider(id_cols = endate, names_from = names, values_from = values)

lmRsnnowrkrv = lm(n ~ covid_cases, data = lmRsnnowrkrv_df)
summary(lmRsnnowrkrv)

ggplot(data = lmRsnnowrkrv_df, aes(x = covid_cases, y = n)) +
  geom_point()


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
rsnnowrkrv_demo_wb <- createWorkbook()

# by race
wbhao_df <- pulse_data  %>% 
  # define race/ethnicity variable
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
  # share of people not at work for reasons due to Covid-1 by PUF week across race/ethnicity
  crosstab(endate, wbhao, w = pweight, row = TRUE) %>% 
  select(endate, white, black, hispanic, asian, other) %>% 
  # write to sheet
  sheet_fun(wb = rsnnowrkrv_demo_wb, s = "wbhao")

# by age
age_df <- pulse_data %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
  # define ade category
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
  sheet_fun(wb = rsnnowrkrv_demo_wb, s = "age")

# by gender
gender_df <- pulse_data %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
  # define sex using gender binary (ugh)
  mutate(gender = case_when(
    egender == 1 | egenid_birth == 1 ~ "male", egender == 2 | egenid_birth == 2 ~ "female")) %>% 
  # share of people not at work ... by PUF week across sex
  crosstab(endate, gender, w = pweight, row = TRUE) %>% 
  # write to wb sheet
  sheet_fun(wb = rsnnowrkrv_wb, s = "gender")

# save workbook to disk
saveWorkbook(wb, here("output/rsnnowrkrv_demo.xlsx"), overwrite = TRUE)
