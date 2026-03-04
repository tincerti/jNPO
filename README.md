# jNPO

jNPO is a longitudinal dataset of all publicly reported subsidies and contracts awarded by the Japanese government to nonprofit organizations (NPOs) from 2011–2025.

The raw source files used to construct this dataset are published by the Cabinet Office and are available at:: https://www.koeki-info.go.jp.

Please cite as: Incerti, Trevor. "jNPO Database." *University of Amsterdam Department of Political Science*.

---

## Project Overview

Japanese government ministries and agencies are required to report subsidies and contracts awarded to nonprofit organizations to the Public Interest Commission of the Cabinet Office. These records are made publicly available.

However:

- The data format varies substantially across years.
- Files are not structured for reproducible data analysis.
- No consolidated time-series (panel) dataset exists covering the full period.

The **jNPO** project provides a cleaned, harmonized, and analysis-ready dataset that compiles all available records from 2011–2025 into standardized formats suitable for research.

---

## Data Description

The dataset includes all reported contracts and subsidies (補助金等) from Japanese government ministries and agencies to NPOs.

Contract data include:

- Competitive bids (競争入札)  
- Negotiated contracts (随意契約)  
- Public works (公共工事)  
- Goods and services (物品役務等)  

For competitive bid contracts, the number of bidders is recorded when available.

---

## Data Files

### 1. Time-Series Panel Dataset (Agency–Month–Grantee Level)

Available as:

- `jNPO.csv`  
- `jNPO.RData`  

This file aggregates data to the **agency × grantee × month** level. For months in which no grant or contract was awarded, `amount = 0`.

#### Variables

- `grantee_clean` — Cleaned name of the NPO (standardized for merging)  
- `grantee_jcn` — Corporate Number (Japan Corporate Number), if available  
- `granter_ministry` — Granting ministry or agency  
- `granter_jcn` — Granting entity Corporate Number, if available  
- `grant_date` — Date of award  
- `grant_month` — Month of award  
- `grant_year` — Year of award  
- `amount` — Contract or subsidy amount (actual awarded amount)  
- `competitive_bid` — Contract type (competitive bid or negotiated)  
- `num_bidders` — Number of bidders (competitive bids only)  
- `grant_name` — Name of the contract or subsidy program  
- `grantee` — Original grantee name (as listed in source files)  
- `grantee_detail` — Grantee name with additional details (e.g., address)  
- `grant_type` — Category (goods and services, public works, or subsidy)  
- `description` — Description of the contract (Japanese; from source files)  
- `contract_reason` — Justification for procurement method, if applicable  
- `amount_est` — Estimated contract price (may differ from awarded amount)  
- `govt_reemployees` — Number of former government officials employed by the NPO  
- `npo_type` — Legal classification of the NPO (e.g., Public Interest Corporation)  
- `admin_division` — Administrative jurisdiction (national or prefectural; currently national only)  
- `filename` — Original Excel filename from the Cabinet Office source  

---

### 2. Transaction-Level Datasets (Agency–Date–Grantee Level)

Located in the `data_clean/` folder:

- `goods_services_clean.csv`  
- `public_works_clean.csv`  
- `subsidies_clean.csv`  

These files contain one observation per subsidy or contract at the date of award, prior to monthly aggregation.

---

## Raw Data

All original (uncleaned) source files are stored in the `data_raw/` directory and subdirectories.

---


## Replication Code

All data were extracted, cleaned, and compiled using:

- **R version 4.3.2**  
- macOS Tahoe 26.1  

Code is located in the `code/` directory.

### File Structure

**0. `functions.R`**  
User-defined functions used throughout the cleaning process.

**1. `goods_services_clean.R`**  
Cleans and compiles government goods and services contracts with NPOs.

**2. `public_works_clean.R`**  
Cleans and compiles government public works contracts with NPOs.

**3. `subsidies_clean.R`**  
Cleans and compiles government subsidies awarded to NPOs.

**4. `combine.R`**  
Merges the cleaned datasets above into the final `jNPO` dataset.
