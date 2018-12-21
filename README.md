# Replication Materials Details

This repository contains public replication materials for the paper "Fake News on Twitter During the 2016 U.S. Presidential Election."

Please note that most of the code in this repository requires access to the restricted data release for this paper. For access to this data, please contact d.lazer@northeastern.edu.

# Structure of the Repository

Each subfolder within this repository contains replication materials and/or data for a particular result in the paper.  Within each subfolder, code for replication is provided. There is also, within each subfolder, a ```data``` directory where data for that particular analysis is housed. Where this data is restricted, we note this, and note where in the restricted data release this data can be found. Finally, there is an ```img``` file, where any images output by the code will be placed.

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
