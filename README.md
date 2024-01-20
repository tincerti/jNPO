# jNPO

Dataset of all subsidies and contracts from the Japanese government to nonprofit organizations (NPOs) from 2011 - present. The raw files used to create the dataset are located at: https://www.koeki-info.go.jp.

All data files are presented in two formats:  

1.  At the agency-date-grantee level. This data contains variables for each subsidy or contract granted by a government ministry or agency for the date the subsidy or contract was issued.

2. In time-series format at the agency-month of grant-grantee level. This format collapses the data to include the total amount of grants given by each agency to each NPO in a given month. For months where no grant was given, the *grant_amount* is 0. 

Please cite as: Incerti, Trevor. "jNPO Database." *University of Amsterdam Department of Political Science*.

### Why?

The Japanese government requires subsidies and/or contracts from the government to nonprofit organizations to be reported to the Public Interest Commission of Cabinet Office and made public. However, the format of the data changes across years, the data are not formatted in a manner conducive to data analysis, and no time series panel format dataset exists that compiles this data over time. 

This project therefore provides a cleaned, analyzable version of this data. 

### Data

Cleaned data can be found in jNPO.csv or jNPO.RData.

Contains data on all contracts and subsidies (補助金等) from the Japanese government to NPOs from 2011 - present. Contract data includes competitive bid (競争入札) and negotiated contracts (随意契約) for public works (公共工事) and goods and services (物品役務等). The number of bidders is provided for all competitive bid contracts. 

Variables provided are as follows:
  - *grantee_clean*: The name of the NPO grantee, cleaned to facilitate ease of merging with other datasets. 
  - *grantee_jcn*: The grantee's  Corporate Number, if available. 
  - *granter_ministry*: The granter ministry or agency
  - *granter_jcn*: The granter's  Corporate Number, if available. 
  - *grant_date*: The date the grant was issued. 
  - *grant_month*: The month the grant was issued. 
  - *grant_year*: The year the grant was issued. 
  - *amount*: The grant amount.
  - *competitive_bid*: The type of contract (i.e., competitive bid or negotiated). 
  - *num_bidders*: The number of bidders on the contract.
  - *grant_name*: The name of the grant
  - *grantee*: The name of the NPO grantee as listed in the raw source files.
  - *grantee_detail*: The name of the NPO grantee and associated details (e.g., address) as listed in the raw source files. 
  - *grant_type*: The ty### Replication Code (relative path "~/code")
  

### Code

Data extracted, cleaned, and compiled using R version 4.3.2 on MacOS Sonoma 14.2.

0. functions.R
- Contains user-written functions used throughout the cleaning code. 
1. goods_services_clean.R
- Cleans and compiles government goods and services contracts with NPOs. 
2. public_works_clean.R
- Cleans and compiles government public works contracts with NPOs. 3. subsidies_clean.R
- Cleans and compiles government subsidies with NPOs. 4. combine.R
- Combines cleaned datasets above into final cleaned jNPO dataset. 
  
