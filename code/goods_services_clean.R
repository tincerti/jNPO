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
# READ IN AND COMBINE COLUMNS PUBLIC WORKS CONTRACT DATA ----
# ______________________________________________________________________________

# Read in all excel files from directory (NOTE: Author written function) -------
gs <- read_dir("data/goods_services", "xlsx", filename = T, skip = 1,
               col_types = "text")

# Rename columns from Japanese to English --------------------------------------
# Use regex to find patterns in differing columns to pass to coalesce 
granter_ministry = syms(grep("支出元府省|所管府省", names(gs), value = TRUE))
description = syms(c("物品役務等の名称及び数量", "物品役務等の名称\r\n及び数量"))
grantee = syms(grep("相手方法人の名|相手方の法人名", names(gs), value = TRUE))
grantee_detail = syms(grep("契約の相手方の商号又は名称|契約の相手方の商号又は\r\n名称及び住所", names(gs), value = TRUE))
grant_name = syms(grep("契約担当", names(gs), value = TRUE))
amount_est = syms(grep("予定価格", names(gs), value = TRUE))
amount = syms(grep("契約金額", names(gs), value = TRUE))
bidding_type = syms(grep("一般競争入札・指名競争入札の別", names(gs), value = TRUE))
num_bidders = syms(grep("応札・応募者数", names(gs), value = TRUE))
admin_division = syms(grep("都道府県所管の区分|都道府県認定の区分", names(gs), value = TRUE))
grantee_jcn = syms(c("法人番号"))
npo_type = syms(c("公益法人の区分"))
contract_reason = syms(grep("随意契約によることとした会計法令|随意契約によることとした業務方法書又は会計規定等の根拠規定及び理由", names(gs), value = TRUE))
govt_reemployees = syms(grep("再就職の役員の数|再就職の\r\n役員の数", names(gs), value = TRUE))
notes = syms(grep("備　　考|備考|備　考", names(gs), value = TRUE))

# Combine columns based on string matches above
gs <- gs %>%
  mutate(
    description = coalesce(!!! description),
    granter_ministry = coalesce(!!! granter_ministry),
    grantee = coalesce(!!! grantee),
    grantee_detail = coalesce(!!! grantee_detail),
    grant_name = coalesce(!!! grant_name),
    amount_est = coalesce(!!! amount_est),
    amount = coalesce(!!! amount),
    bidding_type = coalesce(!!! bidding_type),
    num_bidders = coalesce(!!! num_bidders),
    admin_division = coalesce(!!! admin_division),
    grantee_jcn = coalesce(!!! grantee_jcn),
    contract_reason = coalesce(!!! contract_reason),
    govt_reemployees = coalesce(!!! govt_reemployees),
    npo_type = coalesce(!!! npo_type),
    notes = coalesce(!!! notes)
  ) %>%
  # Remove coalesced columns
  select(
    -matches("物品役務等の名称|支出元府省|所管府省|相手方法人の名|相手方の法人名"),
    -matches("契約の相手方の商号又は|相手方の商号又は名称及び住所|契約担当"),
    -matches("備　　考|備考|備　考|予定価格|契約金額|一般競争入札・指名競争入札の別"),
    -matches("支出元独立行政法人の名称及び法人番号|都道府県|契約の相手方の法人番号"),
    -matches("支出元独立行政法人の名称|応札・応募者数|公益法人の区分"),
    -matches("随意契約によることとした会計法令|再就職の役員の数"),
    -matches("随意契約によることとした業務方法書又は会計規定等の根拠規定及び理由"), 
    -"再就職の\r\n役員の数", -"法人番号"
  ) %>%
  # Translate columns that don't require coalescing
  rename(
    granter_agency = "支出元独立行政法人",
    grant_date = "契約を締結した日",
    est_actual_ratio = "落札率",
    granter_jcn = "支出元独立行政法人の法人番号"
  )

# Remove NA rows (from notes at end of raw Excel files) ------------------------
gs <- gs %>% filter(!is.na(amount))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN AND TRANSLATE ENTRIES IN GOODS AND SERVICES DATA ----
# ______________________________________________________________________________

# Clean amount (price paid for goods and services) columns ---------------------
gs <- gs %>%
  mutate(
    amount = sub(".*支出実績：", "", amount),
    amount = sub(".*?支払実績総額", "", amount),
    amount = sub("分担契約.*", "", amount),
    amount = gsub(",", "", amount), # Remove commas from numbers
    #amount_test = str_extract(amount, "(\\d)+(?=円)")
    ) %>%
    # Remove nonstandard amounts, such as per person, per book, etc.
  filter(
    !str_detect(amount, "円／人|円/人"), # 5 rows
    # Remove values with @ symbol until it can be determined what this signifies 
    !str_detect(amount, "@|＠"), # 181 rows
    !str_detect(amount, "冊") # 4 rows
    # Remaining: entries with two values, and removing yen symbols
    )

check <- gs %>% select(amount_est, amount)

# Clean grantee column ---------------------------------------------------------
# NPO name column does not always exist distinct from NPO name + address
# Pull address out of NPO where necessary
gs <- gs %>%
  mutate(
    # Use grantee detail column if main grantee column is empty
    grantee_clean = ifelse(is.na(grantee), grantee_detail, grantee),
    grantee_clean = gsub("\\s*\\（[^\\）]+\\）", "" , grantee_clean), # Remove information in parens
    # Extract string to the right of first occurrence of 法人
    grantee_clean = sub(".*?法人", "", grantee_clean), 
    grantee_clean = str_trim(grantee_clean), 
    grantee_detail_clean = str_remove(grantee_clean, ".*法人"),
    grantee_clean = ifelse(is.na(grantee), sub("\\s.*", "", grantee_detail_clean),
                           grantee_clean),
    # Remove NPO type information in parenthesis
    # There are also some firm name acronyms in parens that should also be removed
    grantee_clean = gsub("\\s*\\([^\\)]+\\)","", grantee_clean),
    grantee_clean = str_remove(grantee_clean, "\\(社\\）"),
    # Replace blank space or dash with NA
    grantee_clean = na_if(grantee_clean, ""),
    grantee_clean = ifelse(grantee_clean == "-", NA, grantee_clean),
    # Manual name cleaning
    grantee_clean = case_when(
      grepl("リバーフロント研究所", grantee_clean) ~ "リバーフロント研究所",
      grepl("日本測量調査技術協会", grantee_clean) ~ "日本測量調査技術協会",
      grepl("ｱｼﾞｱ・ｱﾌﾘｶ文化財団", grantee_clean) ~ "アジア・アフリカ文化財団",
      TRUE ~ grantee_clean
    ),
    #grantee_clean = zen2han(grantee_clean),
    grantee_clean = sanitizeZenkaku(grantee_clean),
  ) %>% 
  select(-grantee_detail_clean)

# Clean dates ------------------------------------------------------------------
# Pull imperial dates out of date columns
gs <- gs %>%
  mutate(
    date_j = ifelse(str_detect(grant_date, "H|平成|令和|R"), grant_date, NA),
    date_j = sanitizeZenkaku(date_j), # Dates are a mix of full and half width characters
    date_j = str_remove(date_j, " "), # Some spaces in middle of Japanese date strings
    grant_date = ifelse(str_detect(grant_date, "H|平成|令和|R"), NA, grant_date),
    
    # Convert Excel string dates to Date format
    grant_date = as.Date(as.numeric(grant_date), origin = "1899-12-30")
  )

# Grantees with subsidies delivered in batches are listed on the same row, sometimes with multiple dates.
# Make each grant an individual record by pulling out records with multiple dates,
# and returning the earliest date. Using earliest date because not all records with 
# multiple amounts have multiple dates, so cannot necessarily split rows. 
gs <- gs %>%
  mutate(
    date1 = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", date_j, perl=T), # First date in parens, second outside
    date1 = ifelse(!str_detect(date1, "年"), NA, date1), # Some mismatches, remove those without date
    date2 = gsub("\\([^()]*\\)", "", date_j),
    date2 = str_replace(date2, "\\（[^\\）]*\\）", ""),  # Remove other parenthetical notations in dates
    date2 = str_replace(date2, "\\([^\\）]*\\）", ""),
    date_j = ifelse(!is.na(date1), date1, date2),
    date_j = gsub("[[:space:]]", "", date_j),
    date_j = convert_jdate(date_j),
    grant_date = coalesce(grant_date, date_j)
    ) %>%
  select(-date_j, -date1, -date2)

# Add month and year variables
gs <- gs %>%
  mutate(
    grant_year = year(grant_date),
    grant_month = yearmonth(grant_date)
  )

# Clean grant amounts ----------------------------------------------------------
# Pull out numeric value where multiple values and/or characters listed
gs <- gs %>% mutate(
  amount_clean = amount,
  # Convert column to half-width
  across(c(amount_clean, amount_est), ~sanitizeZenkaku(.)),
  # Remove Yen symbol and convert "-" to 0
  across(c(amount_clean, amount_est), ~str_remove(., "円|円")),
  across(c(amount_clean, amount_est), ~ifelse(. %in% c("－", "-"), 0, .)),
  # convert USD amount to JPY using FOREX on day of grant
  amount_clean = ifelse( 
    str_detect(amount_clean, "US\\$"), 
    as.character(as.numeric(str_extract(amount_clean, "[[:digit:]]+")) * 80.2223), 
    amount_clean),
  # Keep only value before or after total where indicator of total value paid given
  amount_clean = sub('.*（変更）', '', amount_clean),
  amount_clean = sub('（変更後）.*', '', amount_clean),
  amount_clean = sub('.*総額', '', amount_clean),
  amount_clean = sub('.*実績額:', '', amount_clean),
  amount_clean = sub('.*支払実績', '', amount_clean),
  amount_clean = sub('.*計画変更後：', '', amount_clean),
  amount_clean = sub('.*変更後金額:', '', amount_clean),
  amount_clean = sub('.*計画変更後:', '', amount_clean),
  amount_clean = sub('。初回交付決定後の額は.*', '', amount_clean),
  amount_clean = sub('、初回契約金額は.*', '', amount_clean),
  amount_clean = sub('出演者一部休演における変更契約後の契約金額.*', '', amount_clean),
  amount_clean = sub('平成29年10月5日（減額変更契約日。.*', '', amount_clean),
  amount_clean = sub('.*当初契約金額', '', amount_clean), # Pulls number AFTER 当初契約金額 and before 最終契約金額
  amount_clean = sub('①.*', '', amount_clean),
  amount_clean = sub('（予定総価）.*', '', amount_clean),
  amount_clean = sub('税込.*', '', amount_clean),
  amount_clean = sub("\\（変更契約金額.*|\\（変更交付決定後の額|\\(変更契約金額", "", amount_clean),
  amount_clean = str_remove(amount_clean, "（支払実績）|\\(支払実績"),
  amount_clean = str_remove(amount_clean, "（単価契約）")
  )

# Remove odd entries from dataframe
# Can revisit if clarification given by cabinet office
# Note: Removes 0.16% of data
gs <- gs %>%
  filter(
    !str_detect(amount, "月額"), # No way of knowing how many months contract lasted
    !str_detect(amount, "単価"), # No way of knowing how many units purchased
    !str_detect(amount, "会場使用料"), # Same as above (per unit fees)
    !str_detect(amount, "/時間|／時間単位"), # Same as above
    !str_detect(amount, "／シフト|/シフト"), # Same as above (per shift fees)
    !str_detect(amount, "ほか"), # Same as above
    !str_detect(amount, "/件"), # Same as above
    !str_detect(amount, "口座振替済"),
    !str_detect(amount, "計画変更後契約金額"),
    !str_detect(amount, "登記情報提供契約約款に定めた金額"),
    !str_detect(amount, "実施1回あたり"),
    !str_detect(amount, "身体計測"),
    !str_detect(amount, "品目ごとの単価契約"),
    !str_detect(amount, "ＧＭサーベイメータ|GMサ-ベイメ-タ"),
    !str_detect(amount, "1クラス1回|1ｸﾗｽ1回"),
    amount != "）", 
    amount != ")"
  ) %>%
  # Remove unnecessary words and characters from amount column
  mutate(
    amount_est = ifelse(amount_est == "非公表", NA, amount_est),
    amount_clean = str_replace(amount_clean, "予定調達総額", ""),
    amount_clean = str_replace(amount_clean, "支払実績", ""),
    amount_clean = str_replace(amount_clean, "平成31・令和元年度実績額：", ""),
    amount_clean = str_replace(amount_clean, "令和元年12月18日に変更契約 変更後金額：", "")
  )

# Where grants delivered in batches listed with multiple numbers in same cell,
# Combine grant amounts
gs <- gs %>%
  # Perform separations of multiple listed amounts
  separate(amount_clean, c("amount_clean", "amount_clean2"), sep = "\\)") %>%
  separate(amount_clean, c("amount_clean", "amount_clean3"), sep = "\\（|\\(") %>%
  # Remove all remaining non-numeric characters from cleaned amount columns
  mutate(across(c(amount_clean, amount_clean2, amount_clean3),
           ~as.numeric(str_extract(., "-?\\d+")))) %>%
  # Sum across batches
  rowwise() %>%
  mutate(amount_clean = sum(amount_clean, amount_clean2, amount_clean3, na.rm = TRUE)) %>%
  select(-amount_clean2, -amount_clean3)

# Clean bidding information ----------------------------------------------------
unique(gs$num_bidders)
gs <- gs %>%
  mutate(
    # Clean number of bidders column
    num_bidders = as.numeric(case_when(
      num_bidders %in% c("-", "－", "－", "＿") ~ "0",
      num_bidders == "１者" ~ "1",
      num_bidders == "２者" ~ "2",
      TRUE ~ num_bidders
    )), 
    # Add indicator for type of bidding procedure
    competitive_bid = ifelse(str_detect(filename, "2-3|3-3"), 
                             "Competitive", "Negotiated")
  )

# Clean govt re-employment column in NPO data amounts -------------------------
# NOTE: Only exists for non-competitive bid contracts. Therefore not a
# replacement for Amakudata as limited in scope. Also does not clarify dates
# former officials joined the NPO 
gs <- gs %>%
  mutate(
    govt_reemployees = case_when(
      competitive_bid == "Competitive" ~ NA_character_,
      govt_reemployees %in% c("－", "-", "　－", "ー", "―", "‐", "無")  ~ "0",
      is.na(govt_reemployees) & competitive_bid == "Negotiated" ~ "0",
      TRUE ~ govt_reemployees),
    govt_reemployees = str_remove(govt_reemployees, "名")
  )

# Translate ministry and agency names ------------------------------------------
gs <- gs %>%
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
    grepl("内閣", granter_ministry) ~ "CAO",
    grepl("復興庁", granter_ministry) ~ "Reconstruction Agency",
    grepl("宮内庁", granter_ministry) ~ "Imperial Household Agency",
    grepl("警察庁", granter_ministry) ~ "National Police Agency",
    grepl("金融庁", granter_ministry) ~ "Financial Services Agency",
    grepl("消費者庁", granter_ministry) ~ "Consumer Affairs Agency",
    grepl("人事院", granter_ministry) ~ "National Personnel Authority",
    grepl("内閣官房", granter_ministry) ~ "Cabinet secretariate",
    TRUE ~ granter_ministry
  ))

# Clean NPO type column --------------------------------------------------------
gs <- gs %>% mutate(npo_type = case_when(
  
  npo_type == "－" ~ "-99",
  
  grepl("特財", npo_type) & grepl("特社", npo_type) ~ "特財 特社",
  grepl("特財", npo_type) & grepl("公社", npo_type) ~ "特財 公社",
  grepl("特財", npo_type) & grepl("公財", npo_type) ~ "特財 公財",
  grepl("特社", npo_type) & grepl("公財", npo_type) ~ "特社 公財",
  grepl("特社", npo_type) & grepl("公社", npo_type) ~ "特社 公社",
  grepl("公社", npo_type) & grepl("公財", npo_type) ~ "公社 公財",
  
  grepl("特財", npo_type) ~ "特財",
  grepl("特社", npo_type) ~ "特社",
  grepl("公社", npo_type) ~ "公社",
  grepl("公財", npo_type) ~ "公財",

  TRUE ~ npo_type
))

# Final data cleaning, prep, and CSV export ------------------------------------
gs <- gs %>% 
  mutate(
    grant_type = "Goods and Services", # Add identifier for goods and services
    amount = amount_clean # Remove if you wish to keep all original string information in amount column
    ) %>% 
  select(granter_ministry, granter_jcn, grant_date, grant_month, grant_year, 
         amount, amount_est,
         grantee_clean, grantee, grantee_detail, grantee_jcn, 
         grant_name, description, grant_type, npo_type, admin_division,
         num_bidders, competitive_bid, govt_reemployees, filename) %>%
  arrange(grant_date, granter_ministry, grantee_clean)

# Export to CSV
write_csv(gs, "data/goods_services_clean.csv")

# Expand into time series dataset ----------------------------------------------
gs_ts <- gs %>%
  group_by(granter_ministry, grantee_clean, grant_month, grant_type) %>%
  summarize(
    amount = sum(amount),
    amount_est = sum(amount_est)
  ) %>%
  as_tsibble(key = c(granter_ministry, grantee_clean, grant_type), 
             index = grant_month) %>% 
  fill_gaps(.full = TRUE) %>%
  mutate(
    amount = ifelse(is.na(amount), 0, amount),
    amount_est = ifelse(is.na(amount_est), 0, amount_est)
  )

