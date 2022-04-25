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
pw <- read_dir("data/public_works", "xlsx", filename = T, skip = 1,
               col_types = "text")

# Rename columns from Japanese to English --------------------------------------
# Use regex to find patterns in differing columns to pass to coalesce 
granter_ministry = syms(grep("支出元府省|所管府省", names(pw), value = TRUE))
project_desc = syms(c("公共工事の名称、場所、期間及び種別", "物品役務等の名称及び数量", "公共工事の名称、場所、\r\n期間及び種別"))
grantee = syms(c("支出元独立行政法人の名称", grep("相手方法人の名|相手方の法人名|平成25年8月末時点|平成26年11月時点", names(pw), value = TRUE)))
grantee_detail = syms(grep("相手方の商号又は名称及び住所|支出元独立行政法人の名称及び法人番号|契約の相手方の商号又は名称、住所及び法人番号|契約の相手方の商号又は\r\n名称及び住所", names(pw), value = TRUE))
grant_name = syms(grep("契約担当", names(pw), value = TRUE))
contract_amount_est = syms(grep("予定価格", names(pw), value = TRUE))
contract_amount = syms(grep("契約金額", names(pw), value = TRUE))
bidding_type = syms(grep("一般競争入札・指名競争入札の別", names(pw), value = TRUE))
num_bidders = syms(grep("応札・応募者数", names(pw), value = TRUE))
admin_division = syms(grep("都道府県所管の区分|都道府県認定の区分", names(pw), value = TRUE))
grantee_jcn = syms(c("法人番号", "契約の相手方の法人番号"))
npo_type = syms(grep("公益法人の区分", names(pw), value = TRUE))
contract_reason = syms(grep("随意契約によることとした会計法令|随意契約によることとした業務方法書又は会計規定等の根拠規定及び理由", names(pw), value = TRUE))
govt_reemployees = syms(grep("再就職の役員の数", names(pw), value = TRUE))
notes = syms(grep("備　　考|備考|備　考", names(pw), value = TRUE))

# Combine columns based on string matches above
pw <- pw %>%
  mutate(
    project_desc = coalesce(!!! project_desc),
    granter_ministry = coalesce(!!! granter_ministry),
    grantee = coalesce(!!! grantee),
    grantee_detail = coalesce(!!! grantee_detail),
    grant_name = coalesce(!!! grant_name),
    contract_amount_est = coalesce(!!! contract_amount_est),
    contract_amount = coalesce(!!! contract_amount),
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
    -matches("支出元府省|所管府省|相手方法人の名|相手方の法人名"),
    -matches("契約の相手方の商号又は名称|相手方の商号又は名称及び住所|契約担当"),
    -matches("備　　考|備考|備　考|予定価格|契約金額|一般競争入札・指名競争入札の別"),
    -matches("支出元独立行政法人の名称及び法人番号|都道府県|契約の相手方の法人番号"),
    -matches("支出元独立行政法人の名称|応札・応募者数|公益法人の区分"),
    -matches("随意契約によることとした会計法令|再就職の役員の数"),
    -matches("随意契約によることとした業務方法書又は会計規定等の根拠規定及び理由"),
    -"法人番号", -"公共工事の名称、場所、期間及び種別", -"物品役務等の名称及び数量", 
    -"公共工事の名称、場所、\r\n期間及び種別", -"契約の相手方の商号又は\r\n名称及び住所"
  ) %>%
  # Translate columns that don't require coalescing
  rename(
    granter_agency = "支出元独立行政法人",
    grant_date = "契約を締結した日",
    est_actual_ratio = "落札率",
    granter_jcn = "支出元独立行政法人の法人番号"
  )
colnames(pw)

# Remove NA rows (from notes at end of raw Excel files) ------------------------
pw <- pw %>% filter(!is.na(contract_amount))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN AND TRANSLATE ENTRIES IN PUBLIC WORKS DATA ----
# ______________________________________________________________________________

# Clean grantee column ---------------------------------------------------------
# NPO name column does not always exist distinct from NPO name + address
# Pull address out of NPO where necessary
pw <- pw %>%
  mutate(
    grantee_clean = ifelse(is.na(grantee), grantee_detail, grantee),
    grantee_clean = gsub("\\s*\\（[^\\）]+\\）", "" , grantee_clean), # Remove information in parens
    # Extract string to the right of first occurance of 法人
    grantee_clean = sub(".*?法人", "", grantee_clean), 
    grantee_clean = str_trim(grantee_clean), 
    grantee_detail_clean = str_remove(grantee_clean, ".*法人"),
    grantee_clean = ifelse(is.na(grantee), sub("\\s.*", "", grantee_detail_clean),
                                               grantee_clean),
    # Manual name cleaning
    grantee_clean = case_when(
      grepl("リバーフロント研究所", grantee_clean) ~ "リバーフロント研究所",
      grepl("日本測量調査技術協会", grantee_clean) ~ "日本測量調査技術協会",
      TRUE ~ grantee_clean
    )
  ) %>% 
  select(-grantee_detail_clean)

# Format dates as dates --------------------------------------------------------
# Add month and year variables
pw <- pw %>%
  separate(col = grant_date, into = c("grant_date", "date2"),
           sep = "(?=平成)|(?=令和)", extra = "merge", remove = FALSE) %>%
  mutate(
    grant_date = as.Date(as.numeric(grant_date), origin = "1899-12-30"),
    date2 = convert_jdate(date2),
    grant_date = coalesce(grant_date, date2),
    grant_year = year(grant_date),
    grant_month = yearmonth(grant_date)
  ) %>%
  select(-date2)

# Clean grant amounts ----------------------------------------------------------
pw <- pw %>%
  mutate(
    contract_amount = as.numeric(contract_amount),
    contract_amount_est = as.numeric(contract_amount_est)
  )

# Add indicator for type of bidding procedure ----------------------------------
pw <- pw %>%
  mutate(competitive_bid = ifelse(str_detect(filename, "2-1|3-1"), 
                                  "Competitive", "Negotiated"))

# Clean govt re-employement column in NPO data amounts -------------------------
# NOTE: Only exists for non-competitive bid contracts. Therefore not a
# replacement for Amakudata as limited in scope. Also does not clarify dates
# former officials joined the NPO 
pw <- pw %>%
  mutate(
    govt_reemployees = case_when(
      competitive_bid == "Competitive" ~ "-99",
      govt_reemployees == "－" | govt_reemployees == "-" ~ "0",
      is.na(govt_reemployees) & competitive_bid == "Negotiated" ~ "0",
      TRUE ~ govt_reemployees),
    govt_reemployees = str_remove(govt_reemployees, "名")
  )

# Final data cleaning, prep, and CSV export ------------------------------------
pw <- pw %>% 
  mutate(grant_type = "Public Works") %>% # Add identifier for contracts
  select(granter_ministry, granter_jcn, grant_date, grant_month, grant_year, 
         contract_amount, contract_amount_est,
         grantee_clean, grantee, grantee_detail, grantee_jcn, 
         grant_name, grant_type, npo_type, admin_division, filename,
         govt_reemployees, contract_reason) %>%
  arrange(grant_date, granter_ministry, grantee_clean)

# Export to CSV
write_csv(pw, "data/public_works_clean.csv")

# Expand into time series dataset ----------------------------------------------
pw_ts <- pw %>%
  group_by(granter_ministry, grantee_clean, grant_month, grant_type) %>%
  summarize(
    contract_amount = sum(contract_amount),
    contract_amount_est = sum(contract_amount_est)
    ) %>%
  as_tsibble(key = c(granter_ministry, grantee_clean, grant_type), 
             index = grant_month) %>% 
  fill_gaps(.full = TRUE) %>%
  mutate(
    contract_amount = ifelse(is.na(contract_amount), 0, contract_amount),
    contract_amount_est = ifelse(is.na(contract_amount_est), 0, contract_amount_est)
    )

