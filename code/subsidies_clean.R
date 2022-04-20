# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PREAMBLE, LIBRARIES, AND IMPORT ----
# ______________________________________________________________________________

# Preamble, packages -----------------------------------------------------------
options(scipen=999) # Do not print in scientific notation

library(tidyverse)
library(rlang)
library(readxl)
library(lubridate)
library(zipangu)
library(tsibble)
source("code/0. functions.R")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN NPO SUBSIDY DATA ----
# ______________________________________________________________________________

# Read in all excel files from directory ---------------------------------------
subsidies <- read_dir("data/npo/subsidies", "xlsx", filename = T, skip = 2,
                col_types = "text")
  
# Rename columns from Japanese to English --------------------------------------
subsidies <- subsidies %>% rename(
  granter_ministry = "支出元府省" ,
  project_name = "事業名",
  grantee_detail = "補助金交付先名\r\n",
  grantee = "補助金交付先法人名\r\n（平成24年９月末時点）",
  grantee2 = "補助金交付先法人名\r\n（平成25年8月末時点）",
  grantee3 = "補助金交付先法人名\r\n（平成26年11月末現在）",
  grantee_jcn = "補助金交付先名及び法人番号",
  grant_amount = "交付決定額",
  account_type = "支出元会計区分",
  grant_name = "支出元（目）名称",
  grant_date = "補助金交付決定等に係る支出負担行為ないし意思決定の日",
  npo_type = "公益法人の区分" ,
  npo_type2 = "公益法人の場合",
  admin_division = "国所管、都道府県所管の区分"  ,
  grantee_detail2 = "補助金交付先名",
  grant_amount2 = "交付決定額（円）",
  jcn = "法人番号",
  admin_division2 = "国認定、都道府県認定の区分",
  admin_division3 = "...10"
)

# Translate ministry and agency names
subsidies <- subsidies %>%
  mutate(granter_ministry = case_when(
    grepl("経済産業省", granter_ministry) ~ "METI",
    grepl("防衛省", granter_ministry) ~ "MOD",
    grepl("環境省", granter_ministry) ~ "MOE",
    grepl("原子力規制庁", granter_ministry) ~ "MOE", # Nuclear Regulation Authority
    grepl("財務省", granter_ministry) ~ "MOF", 
    grepl("外務省", granter_ministry) ~ "MOFA", 
    grepl("総務省", granter_ministry) ~ "MIAC", 
    grepl("厚生労働省", granter_ministry) ~ "MHLW", 
    grepl("農林水産省", granter_ministry) ~ "MAFF", 
    grepl("法務省", granter_ministry) ~ "MOJ",
    grepl("国土交通省", granter_ministry) ~ "MLIT",
    grepl("文部科学省", granter_ministry) ~ "MEXT",
    grepl("内閣府", granter_ministry) ~ "CAO",
    grepl("復興庁", granter_ministry) ~ "Reconstruction Agency",
    TRUE ~ "99"
  ))
  
# For one year, Japan Corporate Numbers and organization names are combined ----
# in the same column. Split them.
subsidies <- subsidies %>% mutate(
    jcn = ifelse(is.na(jcn), gsub("[^0-9.]", "", grantee_jcn), jcn),
    grantee = ifelse(is.na(grantee), gsub("法人番号.*", "", grantee_jcn), grantee),
    grantee = sub("\\s+[^ ]+[0-9]$", "", grantee)
  )

# Column names change from year to year so are read in as multiple columns -----
# Combine them in into single columns.
subsidies <- subsidies %>% mutate(
    grantee = coalesce(grantee, grantee2, grantee3),
    grantee_detail = coalesce(grantee_detail, grantee_detail2),
    grant_amount = coalesce(grant_amount, grant_amount2),
    admin_division = coalesce(admin_division, admin_division2, admin_division3),
    npo_type = coalesce(npo_type, npo_type)
  ) %>% 
  # Remove duplicated columns that are now coalesced
  select(-grantee_detail2, -grantee2, -grantee3, -grant_amount2, -grantee_jcn,
         -admin_division2, -npo_type2, -admin_division3)

# Starting H28 there are no separate grantee and grantee_detail columns. -------
# Some organization names have addresses in them. Check for addresses in 
# grantee_detail and remove and add cleaned org name to grantee column. 
subsidies <- subsidies %>%
  mutate(grantee = ifelse(is.na(grantee), grantee_detail, grantee))

# Clean dates ------------------------------------------------------------------
# Pull imperial dates out of date columns
subsidies <- subsidies %>%
  mutate(
    date_j = ifelse(str_detect(grant_date, "H|平成|令和|R"), grant_date, NA),
    grant_date = ifelse(str_detect(grant_date, "H|平成|令和|R"), NA, grant_date),
    
    # Convert Excel string dates to Date format
    grant_date = as.Date(as.numeric(grant_date), origin = "1899-12-30")
    )

# Grantees with subsidies delivered in batches are listed on the same row with multiple dates
# Make each grant an individual record by pulling out records with multiple dates,
# And returning the earliest date
subsidies <- subsidies %>%
  separate(col = date_j, into = c("date1", "date2"), 
           sep = "\\s", extra = "merge", remove = FALSE) %>%
  separate(col = date2, into = c("date3", "date4"),
           sep = "(?=平成)|(?=令和)|(?=H)|(?=R)", extra = "merge", remove = FALSE) %>%
  select(-date2, -date3) %>% rename(date2 = date4) %>%
  separate(col = date2, into = c("date3", "date4"),
           sep = "(?=\\s平成)|(?=\\s令和)|(?=、平成)|(?=、令和)|(?=は平成)|(?=は令和)|(?=はR)|(?=はH)", extra = "merge", remove = FALSE) %>%
  select(-date2) %>% rename(date2 = date3, date3 = date4) %>%
  mutate(
    across(c(date1, date2, date3), ~str_extract(., "H+\\d+.\\d+.\\d+|R+\\d+.\\d+.\\d+|平成+\\d+.\\d+.\\d+|令和+\\d+.\\d+.\\d+|令和元+.\\d+.\\d+")),
    across(c(date1, date2, date3), ~convert_jdate(.)), # Convert cleaned Japanese dates to Ano Domini
    e_date = pmin(date1, date2, date3, na.rm = TRUE),
    grant_date = coalesce(grant_date, e_date))

# Add month and year variables
subsidies <- subsidies %>%
  mutate(
    grant_year = year(grant_date),
    grant_month = yearmonth(grant_date)
  )

# Clean grantee names ----------------------------------------------------------
# Create cleaned grantee name removing prefixes from grantee names
subsidies <- subsidies %>%
  mutate(grantee_clean = gsub(".*法人","", grantee), # Remove NPO signifier
         grantee_clean = gsub("\\s*\\([^\\)]+\\)", "" , grantee_clean), # Remove information in parens
         grantee_clean = str_remove(grantee_clean, "東京都千代田区平河町2-6-3"), # Remove addresses 
         grantee_clean = str_trim(grantee_clean, side = "both") # Remove whitespace
         )

# Clean grant amounts ----------------------------------------------------------
subsidies <- subsidies %>%
  # Need to remove a lot of odd characters and notes from grant amount column
  filter(!is.na(grant_amount)) %>%
  mutate(
    grant_amount = gsub("\\（.*","", grant_amount),
    grant_amount = gsub("\\(.*","", grant_amount),
    grant_amount = str_remove(grant_amount, "円|△|。"),
    grant_amount = str_replace(grant_amount, pattern = ",", replacement = ""),
    grant_amount = gsub(",", "", grant_amount),
    grant_amount = str_trim(grant_amount),
    grant_amount = as.numeric(grant_amount)
  )

# Final data cleaning and prep -------------------------------------------------
subsidies <- subsidies %>% 
  filter(!is.na(grantee) & !is.na(grant_amount)) %>% # Remove NA grantees
  mutate(grant_type = "Subsidy", # Add identifier for subsidies
         grant_amount = as.numeric(gsub("[^0-9.-]", "", grant_amount))) %>% 
  select(granter_ministry, grant_date, grant_month, grant_year, grant_amount, 
         grantee_clean, grantee, grantee_detail, jcn, 
         grant_name, grant_type, npo_type, admin_division, filename)

# Export data to CSV -----------------------------------------------------------
write_csv(subsidies, "data/npo/subsidies_clean.csv")

# Expand into time series dataset ----------------------------------------------
subsidies_ts <- subsidies %>%
  group_by(granter_ministry, grantee_clean, grant_month, grant_type) %>%
  summarize(grant_amount = sum(grant_amount)) %>%
  as_tsibble(key = c(granter_ministry, grantee_clean, grant_type), 
             index = grant_month) %>% 
  fill_gaps(.full = TRUE) %>%
  mutate(grant_amount = ifelse(is.na(grant_amount), 0, grant_amount))




