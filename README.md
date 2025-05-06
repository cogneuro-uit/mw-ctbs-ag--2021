# mw-ctbs-ag--2021

Data, analysis and experimental materials for the paper "Reducing mind wandering using continuous theta burst stimulation".

Preprint is available at [PsyArXiv](https://doi.org/10.31234/osf.io/fkx3w).

If you want to use this data/analysis in a research publication, please cite [our paper](https://doi.org/10.31234/osf.io/u5j7s).

~~~{bibtex}
@article{drevland2024,
  author={Ragnhild Nicolaisen Drevland and Steffen Rygg Aasen and Gábor Csifcsák and Matthias Mittner},
  title={Reducing mind wandering using continuous theta burst stimulation},
  year=2024,
  journal={Open Science Framework (OSF)},
  volume=,
  number=,
  doi=https://doi.org/10.31234/osf.io/u5j7s
}
~~~

## Requirements

Analysis are coded in [R](http://r-project.org). The scripts rely on a fair number of R-packages, in addition to [Stan](http://mc-stan.org) for Bayesian analysis. 

## Setup

This repository uses the
[ProjectTemplate](http://projecttemplate.net/) directory layout. 

## Data

The raw data is stored under `data/raw` as a `.csv` file. Demographics data are stored as `demographics.sav`. The `export` folder contains blinding related output, generated from the `make_randomization_list.R`. Lastly, the Bayesian models are stored in `data/` as a `.rdata` file. 

### Preprocessing 

First, load the library (`library(ProjectTemplate)`) and then load the project by `load.project()`. This will munge the raw data to a finished preprocessed output (see below for the behavioural data), load the most relevant packages and other local packages.

~~~
# A tibble: 3,520 × 16
   subj  session block proberound probe1 probe2 probe3      apen     bv logapen logbv zlogapen
   <chr> <chr>   <chr>      <dbl> <ord>  <ord>  <ord>      <dbl>  <dbl>   <dbl> <dbl>    <dbl>
 1 AG001 S1      B0             1 1      2      1      -0.000945 0.0560   0.365 -2.88   -1.60 
 2 AG001 S1      B0             2 1      1      1       0.289    0.0335   0.907 -3.40   -1.06 
 3 AG001 S1      B0             3 1      1      2       0.122    0.0415   0.559 -3.18   -1.41 
 4 AG001 S1      B0             4 1      2      3       0.318    0.175    0.979 -1.74   -0.984
 5 AG001 S1      B0             5 2      3      2       0.399    0.0533   1.22  -2.93   -0.739
 6 AG001 S1      B0             6 2      3      1       0.303    0.0372   0.941 -3.29   -1.02 
 7 AG001 S1      B0             7 3      1      3       0.229    0.0538   0.768 -2.92   -1.20 
 8 AG001 S1      B0             8 1      2      3       0        0.0463   0.367 -3.07   -1.60 
 9 AG001 S1      B0             9 2      3      2       0        0.0345   0.367 -3.37   -1.60 
10 AG001 S1      B0            10 3      1      1       0.192    0.0620   0.690 -2.78   -1.28 
# ℹ 3,510 more rows
# ℹ 4 more variables: zlogbv <dbl>, region <chr>, stimulation <fct>, proberound_prop <dbl>
# ℹ Use `print(n = ...)` to see more rows
~~~

## Analyses

The analyses are located in the `src/` folder as `03_prereg_analysis_ag.R`, and `demographic_descriptives.R`. 
