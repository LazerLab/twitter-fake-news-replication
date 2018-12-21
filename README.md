# Replication Materials Details

[![DOI](https://zenodo.org/badge/162752654.svg)](https://zenodo.org/badge/latestdoi/162752654)

This repository contains public replication materials for the paper "Fake News on Twitter During the 2016 U.S. Presidential Election."

Please note that some of the code in this repository requires access to the restricted data release for this paper. For access to this data, please contact d.lazer@northeastern.edu.  Once you have access, you can simply place the required file into the data directory of the particular result you are replicating to finish the replication.

# Structure of the Repository

Each subfolder within this repository contains replication materials and/or data for a particular result in the paper.  Within each subfolder, code for replication is provided. There is also, within each subfolder, a ```data``` directory where data for that particular analysis is housed. Where this data is restricted, we note this, and note where in the restricted data release this data can be found. Finally, there is an ```img``` file, where any images output by the code will be placed. ***Please note, where these directories are not present, you will need to create them (Github does not allow commits of empty folders) before running analyses***

For each subfolder, the expectation is that the code is run from the top level directory (i.e. this directory) of this repository.

The only exception to this is the ```util``` directory, which contains code utilities leveraged throughout our analyses.

## ```domain_coding```

This directory contains data files only that describe our labeling of websites.  The directory contains three files:

```Domain Codings.xlsx``` - details on manual codings of fake news sites.

```website_alignment_scores.xlsx``` - political alignment scores for websites.

```black_sites.txt``` - all black sites used in the present study.


## ```figure_1```

This folder contains code and data necessary to replicate Figure 1 in the paper.

### Code

The script ```figure_1/gen_fig1_plus_supporting.R``` reproduces the results for Figures 1A, 1B, 1C, 1D in the main body, as well as the supporting material for Figure 1, in Figures S6, S7 and S8.

### Data
The files ```figure_1b_data.csv```, ```figure_1c_data.csv```, and ```figure_1d_data.csv```, provide the data to recreate the ECDFs in Figure 1. ***Note that ```figure_1c_data.csv``` and ```figure_1d_data.csv``` contain individual-level data and can only be found in the restricted access release.*** Each file has 3 columns, a category column (what type of website is this line relevant to?), a ```percentage_of_total``` column (the x-axis of the plots) and a ```cumulative_total```, the y-axis of the plot.

The files ```figure_1/data/daily_counts_exposures.csv```, ```figure_1/data/daily_counts_exposures_guess.csv```, and ```figure_1/data/daily_counts_shares.csv``` contain daily counts of fake news quantities for exposures, exposures according to the list from Guess et al., and shares, respectively.  Columns in each file are as follows:

```
ts_rel_days - Number of days before (negative) or after (positive) election day
red - number of red fake news sites exposed to/shared by
orange - number of orange fake news exposed to/shared by
black - number of black fake news exposed to/shared by (note all sites in the Guess et al. list are considered "black")
N - Total number of shares/exposures on this day
date - date in year-month-day format
pct_red - percentage of all shares/exposures that were to/from red sites
pct_orange - percentage of all shares/exposures that were to/from orange sites
pct_black - percentage of all shares/exposures that were to/from black sites
pct_fake - percentage of all shares/exposures that were to/from any fake news site
```

## Coexposure analysis

### Code
The script ```coexposure_network/analysis.R``` generates Figure 7 from the main text, as well as Figures S10 and S11 in the supplementary.

### Data

The file ```coexposure_network/data/site_to_site_connections.csv``` provides details on each site pair considered in the analysis, using the methods specified in Dianati et al., 2016.  The columns of the data are:

```
site_j - one site relevant to this edge
site_i - the other site relevant to this edge
x - total number of exposures common to the two sites
weight_i, weight_j, q, p - used to compute significance of edge weights under the null model, see paper for details
pval - the p-value of the observed edge under the null
significance - the significance of the edge (the negative logarithm of the p-value)
```

The file ```coexposure_network/data/site_map.tsv``` just cleans up some of the sitenames. The file ```total_exposure_counts.csv``` gives the total number of exposures for each domain, and is just used for sizing for Figure 7.

There are two files used in the script that are not available in this public data release:

-  ```coexposure_network/data/user_to_domain_network.csv``` links an anonymized user ID (```panel_uid```) to a website (```website```) and gives the number of times the user was exposed to that website.  However, as it is individual level data, we do not share it here.
- ```coexposure_network/data/fb_top500_domain_affl.csv``` is taken from the Bakshy et al. Facebook study, and must be requested from those authors. 


## Representativeness (PEW)

### Code
The script ```compare_to_pew/analyze_representativeness_pew.R``` generates Figure S1 from the supplementary.

### Data

There are two files used in the script that are not available in this public data release:

- ```compare_to_pew/data/pew_stats.csv``` contains data drawn directly from [PEW's 2016 Social Media update survey](http://www.pewinternet.org/dataset/march-2016-libraries/).  You will need to register for and then download the CSV release for this survey, and then rename it to ```pew_stats.csv```.

- ```compare_to_pew/data/panel_stats.csv``` contains information on panel members party, sex, race_ethnicity, and age (anonymized). ***This file is contained only in the restricted release***.

