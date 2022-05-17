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
# FILL NA VALUES WHERE APPLICABLE ----
# ______________________________________________________________________________

# Fill NA JCN codes by group (grantee)
jnpo <- jnpo %>%
  group_by(grantee_clean) %>%
  fill(grantee_jcn, .direction = "downup") %>%
  ungroup() %>%
  group_by(granter_ministry)  %>%
  ungroup()

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# EXPORT TO CSV ----
# ______________________________________________________________________________

write_csv(jnpo, "data/jnpo.csv")



