# Replication Materials Details

[![DOI](https://zenodo.org/badge/162752654.svg)](https://doi.org/10.5281/zenodo.2483311)

This repository contains public replication materials for the paper "Fake News on Twitter During the 2016 U.S. Presidential Election."

Please note that some of the code in this repository requires access to the restricted data release for this paper. For access to this data, please contact d.lazer@northeastern.edu.  Once you have access, you can simply place the required file into the data directory of the particular result you are replicating to finish the replication.

# Structure of the Repository

Each subfolder within this repository contains replication materials and/or data for a particular result in the paper.  Within each subfolder, code for replication is provided. There is also, within each subfolder, a ```data``` directory where data for that particular analysis is housed. Where this data is restricted, we note this, and note where in the restricted data release this data can be found. Finally, there is an ```img``` file, where any images output by the code will be placed. ***Please note, where these directories are not present, you will need to create them (Github does not allow commits of empty folders) before running analyses***

For each subfolder, the expectation is that the code is run from the top level directory (i.e. this directory) of this repository.

The only exception to this is the ```util``` directory, which contains code utilities leveraged throughout our analyses.

## ```domains/domain_coding```

This directory contains data files only that describe our labeling of websites.  The directory contains three files:

```Domain Codings.xlsx``` - details on manual codings of fake news sites.

```website_alignment_scores.xlsx``` - political alignment scores for websites.

```black_sites.txt``` - all black sites used in the present study.


## ```figure_1```

This folder contains code and data necessary to replicate Figure 1 in the paper.

### Code

The script ```figure_1/gen_fig1_plus_supporting.R``` reproduces the results for Figures 1A, 1B, 1C, 1D in the main body, as well as the supporting material for Figure 1, in Figures S6, S7 and S8.

### Data
The files ```figure_1b_data.csv```, ```figure_1c_data.csv```, and ```figure_1d_data.csv```, provide the data to recreate the ECDFs in Figure 1. ***Note that these files contain individual-level data and can only be found in the restricted access release.*** Each file has 3 columns, a category column (what type of website is this line relevant to?), a ```percentage_of_total``` column (the x-axis of the plots) and a ```cumulative_total```, the y-axis of the plot.

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

## Exposure, sharing, and sharing per exposure regressions

The analysis in ```exp_share_regressions/exp_share_reg.Rmd``` covers the results that focus on the individual characteristics associated with exposure and sharing of fake news. These are reported in the main body of the paper (sections 'who was exposed to fake news sources?' and 'who shared fake news sources?' as well as in sections S11-13 of the supplamentary materials. 

While the data for replicating this analysis is available under restricted-access, the output of its execution on (de-identified data) is available for easy viewing in html format at ```exp_share_regressions/exp_share_reg.nb.html```.

## ```coexposure_network```


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

The file ```coexposure_network/data/site_map.tsv``` just cleans up some of the sitenames. The file ```coexposure_network/data/total_exposure_counts.csv``` gives the total number of exposures for each domain, and is just used for sizing for Figure 7.

The file ```restricted_data/fb_top500_domain_affl.csv``` is taken from the Bakshy et al. Facebook study, and is available only in the restricted release.  Note that you can still generate network plots without this file. 

Additionally, note that with restricted access, you can construct the coexposure network from raw exposure data, but the results will be slightly different due to k-anonymization.  Refer to ```coexposure_network/analysis.R``` for details.



## ```political_urls/political_classifier_evaluation```

Code and data to replicate Figure S4 in the paper.

### Code
The script ```political_urls/political_classifier_evaluation/figure_s4.R``` generates Figure S4, for the political classifier evaluation.

### Data

***This file is only available under the restricted data release***. 

The file ```restricted_data/political_classifier_eval_data.csv``` gives results of the annotation task, and has the following columns:

```
tweet_date - Twitter formatted, time removed
election - number of MTurk annotators giving "U.S. Election" as the answer
idk - N Turkers giving "I don't know"
other - N Turkers giving "Something else"
politics - N Turkers giving "U.S. politics in general
answer - the final answer. This is a majority vote of turkers in most cases, if there was no agreement, its the decision of someone on our Northeastern team.
is_pol - Did the classifier say it was about politics?
```
## ```panel_accounts/compare_to_pew```

Code and data to replicate Figure S1.

### Code
The script ```panel_accounts/compare_to_pew/analyze_representativeness_pew.R``` generates Figure S1 from the supplementary.

### Data

There are two files used in the script that are not available in this public data release:

- ```restricted_data/pew_stats.csv``` contains data drawn directly from [PEW's 2016 Social Media update survey](http://www.pewinternet.org/dataset/march-2016-libraries/).

- ```restricted_data/panel.tsv``` contains information on panel members (anonymized).

## ```panel_accounts/super_people_analyses```

Code for Figure 2 and Section S.9. 

### Code
- `generate_superspreaders_figure.R` generates Figure 2 under `img/bar.pdf` and `img/bar_exp.pdf`
- `superPeopleAnalyses.R` generates data for Table S3. Its output is included here in `superPeopleAnalyses-out.txt`.

### Data
Both scripts rely on  `panel_with_counts.tsv` from the restricted data release.

## `panel_accounts/`: `profile_validation` and `panel_accounts/outlier_validation`

Code and results for Section S.8. 

### Code
- ```panel_accounts/outlier_validation/accountsToInspect.R``` demonstrates how we selected the 23 outlier accounts and 15 bot-like accounts for manual inspection.
- ```panel_accounts/profile_validation``` demonstrates how we selected 200 matched accounts for annotation. The analysis of annotations takes place in ```03-eval-with-annots.R```, and 
its output is in ```results/03-out.txt```.

### Data
- `panel_accounts/outlier_validation` uses `restricted_data/panel_with_counts.tsv`.
- `panel_accounts/profile_validation` uses individual-level data that is not being released.



## ```analyses/capping_simulation```

Code for generating Figure S14. Requires ```restricted_data``` for its files ```panel.tsv``` and ```urls.tsv.gz```.

- ```cappingSimData.R``` generates the data files ```capping_preserved2.csv``` and ```origSums.Rdata``` (included here).
- ```cappingPlot.R``` uses those files to create the plot ```img/cappingSim4.pdf``` (included here).

