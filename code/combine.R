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

# Read in cleaned files --------------------------------------------------------
jnpo <- read_dir("data", extension = "csv", delim = ",", filename = FALSE)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ARRANGE AND SORT COLUMNS ----
# ______________________________________________________________________________

# Arrange files
jnpo <- jnpo %>%
  arrange(
    grantee_clean, grant_date, grant_type, granter_ministry, competitive_bid
    ) %>%
  select(
    grantee_clean, grantee_jcn, granter_ministry, granter_jcn, 
    grant_date, grant_month, grant_year, 
    amount, amount_est, competitive_bid, num_bidders, govt_reemployees,
    grant_name, grantee, grantee_detail, grant_type, description, contract_reason,
    npo_type, admin_division, filename
    )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FILL OR REMOVE NA VALUES WHERE APPLICABLE ----
# ______________________________________________________________________________

# Fill NA JCN codes by group (grantee)
jnpo <- jnpo %>%
  group_by(grantee_clean) %>%
  fill(grantee_jcn, .direction = "downup") %>%
  ungroup() %>%
  group_by(granter_ministry)  %>%
  ungroup()

# Label missing indicator -99 to categorical variables where applicable 
jnpo <- jnpo %>%
  mutate(
    competitive_bid = ifelse(grant_type == "Subsidy", -99, competitive_bid),
    npo_type = ifelse(is.na(npo_type), -99, npo_type)
    )

# Remove NA dates and firms
jnpo <- jnpo %>% 
  filter(!is.na(grant_date), !is.na(amount), !is.na(grantee_clean))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FORMAT DATA TYPES ----
# ______________________________________________________________________________

# Format dates
jnpo <- jnpo %>%
  mutate(grant_date = as.Date(grant_date),
         grant_year = year(grant_date), 
         grant_month = yearmonth(grant_date),
         amount = as.numeric(amount))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# EXPORT TO Rdata & CSV ----
# ______________________________________________________________________________

save(jnpo, file = "jnpo.RData")
write_csv(jnpo, "jnpo.csv")



