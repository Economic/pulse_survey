# Census Pulse Survey
Get Census Household Pulse Survey (HHPS) data

## Data Source
Data is downloaded from [Census Pulse Survey website](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html).

## Downloading the data
`code/pulse_supplement.R` shows how to download multiple weeks across multiple years at once. `pulse_supplement.R` takes advantage of the repetitive .zip naming convention to automate data download.

## Supplementary files
`data/date_xwalk.csv` provides crosswalk that can be used to establish week number.

## Other files
This project was initially written in response to various requests related to HHPS. These are good reference for how to read and append multiple .csv files at a time:
1. `code/emp_t3.R` recreates [HHPS Employment Tables](https://www.census.gov/data/tables/2021/demo/hhp/hhp41.html) [Table 3 for week 41](https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk41/employ3_week41.xlsx).
2. `code/rsnnowrkrv.R` relates to Elise's [WWJD: Omicron will weight heavily on the labor market](https://www.epi.org/blog/what-to-watch-on-jobs-day-omicron-will-weigh-heavily-on-the-labor-market/)
3. `code/rsnnowrkrv_care.R` relates to request from Marokey on the "discussion of who's leaving the labor force b/c of child care issues, specifically Black women." To my knowledge, these numbers were never published.
