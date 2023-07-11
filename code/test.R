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

pulse_data_df3 <- map_dfr(41, ~ read.csv(here(paste0("data/puf_csv/pulse2022_puf_", .x, ".csv"))))

pulse_data <- bind_rows(pulse_data_df1, pulse_data_df2, pulse_data_df3) %>% 
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
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>% 
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  arrange(endate) %>% 
  mutate(n = n/1000000) %>% 
  left_join(covid_cases, by = "endate") %>% 
  full_join(df, by = "endate") %>% 
  pivot_longer(cols = c("covid_cases", "n"), names_to = "names", values_to = "values") %>% 
  filter(!is.na(values))

rsnnowrkrv_tot_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(anywork == 2) %>% 
  #select(endate, rsnnowrkrv)
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  full_join(df, by = "endate") %>% 
  arrange(endate) %>% 
  mutate(n = n/1000000)

rsnnowrkrv_other_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(rsnnowrkrv == 12) %>% 
  #select(endate, rsnnowrkrv)
  group_by(endate) %>% 
  tally(wt = pweight) %>% 
  full_join(df, by = "endate") %>% 
  arrange(endate) %>% 
  mutate(n = n/1000000)


write.csv(rsnnowrkrv_df, here("output/rsnnowrkrv.csv"))

write.csv(rsnnowrkrv_tot_df, here("output/rsnnowrkrv_tot.csv"))


ggplot(data = rsnnowrkrv_df, aes(x = endate, y = values, color = names)) +
  geom_point() +
  ggtitle(paste("People not working because they have or were caring for someone with Covid")) +
  xlab("Week") +
  ylab("Weighted population (millions)") +
  theme(panel.grid.minor = element_blank())

ggplot(data = lmRsnnowrkrv_df, aes(x = covid_cases, y = n)) +
  geom_point()

lmRsnnowrkrv_df <- rsnnowrkrv_df %>% 
  pivot_wider(id_cols = endate, names_from = names, values_from = values)

lmRsnnowrkrv = lm(n ~ covid_cases, data = lmRsnnowrkrv_df)
summary(lmRsnnowrkrv)        

wbhao_df <- pulse_data  %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  mutate(wbhao = case_when(
    rhispanic == 2 ~ "hispanic",
    rrace == 1 ~ "white", rrace == 2 ~ "black", rrace == 3 ~ "asian", rrace == 4 ~ "other")) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
  crosstab(endate, wbhao, w = pweight, row = TRUE) %>% 
  select(endate, white, black, hispanic, asian, other)

age_df <- pulse_data %>% 
  dplyr::filter(tbirth_year <= 2004) %>% 
  mutate(rsnnowrkrv = case_when(
    is.na(rsnnowrkrv) ~ rsnnowrk,
    TRUE ~ rsnnowrkrv),
    endate = as.Date(endate)) %>% 
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
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
  filter(ifelse(week <= 27, rsnnowrkrv %in% c(2,3), rsnnowrkrv == 2)) %>%
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

saveWorkbook(wb, here("output/rsnnowrkrv_demo.xlsx"), overwrite = TRUE)
