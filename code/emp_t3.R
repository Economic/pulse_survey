# week x-walk
date_xwalk <- read.csv(here("data/date_xwalk.csv"))

# read in pulse survey data for 2020
pulse_data <- map_dfr(40, ~ read.csv(here(paste0("data/puf_csv/pulse2021_puf_", .x,".csv"))))
  

emp_t3 <- pulse_data %>% 
  clean_names() %>% select(id = scram, week, educ = eeduc, rsnnowrkrv, pweight) %>% 
  mutate(rsnnowrkrv = case_when(
      rsnnowrkrv == 1 ~ "I did not want to be employed at this time",
      rsnnowrkrv == 2 ~ "I am/was sick with coronavirus or caring for someone who was sick with coronavirus symptoms",
      rsnnowrkrv == 3 ~ "I am/was caring for children not in school or daycare",
      rsnnowrkrv == 4 ~ "I am/was caring for an elderly person",
      rsnnowrkrv == 5 ~ "I was concerned about geting or spreading the coronavirus",
      rsnnowrkrv == 6 ~ "I am/was sick (not coronavirus related) or disabled",
      rsnnowrkrv == 7 ~ "I am retired",
      rsnnowrkrv == 8 ~ "I am/was laid off or furloughed due to coronavirus pandemic",
      rsnnowrkrv == 9 ~ "My employer closed temporarily due to the coronavirus pandemic",
      rsnnowrkrv == 10 ~ "My employer went out of business due to the coronavirus pandemic",
      rsnnowrkrv == 11 ~ "I do/did not have transportation to work",
      rsnnowrkrv == 12 ~ "Other",
      rsnnowrkrv == -99 | rsnnowrkrv == -88 ~ "None"),
    educ = case_when(
      educ == 1 ~ "Less than high school",
      educ == 2 ~ "Some high school",
      educ == 3 ~ "High school or equivalent",
      educ == 4 ~ "Some college",
      educ == 5 ~ "Associate's degree",
      educ == 6 ~ "Bachelor's degree",
      educ == 7 ~ "Advanced degree")) %>% 
  group_by(rsnnowrkrv, educ) %>% 
  summarise(sum = sum(pweight, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = "rsnnowrkrv", names_from = "educ", values_from = "sum") %>% 
  select(rsnnowrkrv, "Less than high school", "Some high school", "High school or equivalent", "Some college", 
         "Associate's degree", "Bachelor's degree", "Advanced degree") %>% 
  arrange(match(rsnnowrkrv, c("I did not want to be employed at this time", 
                              "I am/was sick with coronavirus or caring for someone who was sick with coronavirus symptoms", 
                              "I am/was caring for children not in school or daycare", 
                              "I am/was caring for an elderly person", "I was concerned about geting or spreading the coronavirus",
                              "I am/was sick (not coronavirus related) or disabled",
                              "I am retired", "I am/was laid off or furloughed due to coronavirus pandemic", 
                              "My employer closed temporarily due to the coronavirus pandemic", 
                              "My employer went out of business due to the coronavirus pandemic", "I do/did not have transportation to work",
                              "Other", "None")))