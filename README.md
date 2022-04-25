# jNPO

Dataset of all subsidies and contracts from the Japanese government to nonprofit organizations (NPOs) from 2011 - present. The raw files used to create the dataset are located at: https://www.koeki-info.go.jp.

All data files are presented in two formats:  

1.  At the agency-date-grantee level. This data contains the following variables for each subsidy or contract granted by a government ministry or agency for the date the subsidy or contract was issued.
  - *granter_ministry*: The granter ministry or agency
  - *grant_date*: The grant date. 
  - *grant_amount*: The grant amount
  - *grantee_clean*: The name of the NPO grantee, cleaned to facilitate ease of merging with other datasets. 
  - *grantee*: The name of the NPO grantee as listed in the raw source files.
  - *grantee_detail*: The name of the NPO grantee and associated details (e.g., address) as listed in the raw source files. 
  - *grant_name*: The name of the grant
  - *grant_type*: The type of grant (e.g., subsidy, contract, etc.). 
  - *npo_type*: The type of NPO
  - *admin_division*: The administrative division of the granting agency. 
  - *filename*: The raw filename from https://www.koeki-info.go.jp. 

2. In time-series format at the agency-month of grant-grantee level. This format collapses the information above to include the total amount of grants given by each agency to each NPO in a given month. For months where no grant was given, the *grant_amount* is 0. 

## Subsidies 

Contains data on all subsidies from the Japanese government to NPOs from 2011 - present. 

## Contracts

Contains data on all contracts from the Japanese government to NPOs from 2011 - present. 
