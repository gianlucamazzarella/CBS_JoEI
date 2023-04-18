# Intergenerational Mobility in the Netherlands: Models, Outcomes and Trends

This repository contains all the codes to reproduce the results presented in:

Colagrossi, M., Geraci, A., Mazzarella, G. (2023). Intergenerational Mobility in the Netherlands: Models, Outcomes and Trends. *Journal of Economic Inequality*, forthcoming

Data cannot be shared since they are property of the Centraal Bureau voor de Statistiek, Netherlands. Further information are available at this [Link](https://www.cbs.nl/en-gb/onze-diensten/customised-services-microdata/microdata-conducting-your-own-research/applying-for-access-to-microdata)

You can read and download the paper (with open access) here: https://doi.org/10.1007/s10888-023-09569-7

It includes:
- 8 `Stata` do-files (.do) that start from orginal administrative records and end with the creation of the excel file to be submitted for output check. In particular:
	- **0_globals:** constains the globals pointing to the folders used for the analyses
	- **1_families:** creates genealogical tree of all individuals ever appearing in Dutch municipalities records
	- **2_income:** compute proxies of permanent income starting from annual income microdata
	- **3_education:** highest achieved education and crosswalk with years of education
	- **4_estimates:** bootstrap replications of full population and birth cohorts estimates
	- **5_table2:** bootstrap replication to reproduce the findings of Table 2
	- **6_descriptive_statistics:** information on the samples used
	- **7_excel4output:** creates the final excel file for output check
- An `R` script (**8_post_output_tab_fig.R**) that creates all the tables and figures after output check.
