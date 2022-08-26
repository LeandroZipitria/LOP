# Variety and LOP

R files for the paper "Varieties as a Source of Law of One Price Deviations", published in [**International Economics**](https://doi.org/10.1016/j.inteco.2022.07.001), Volume 172, December 2022, Pages 1-14.

This repository provides all files to create the database on price differences, create the dummies on variety and competition, run descriptive statistics, and all regressions. Files are created for the R program.

--------------------- 

## Create databases

The first four files create the databases (in levels and differences) and are needed for the analysis. \

0-A-create-databases.R: merge price database with product and store information.\
0-B-difference-database.R: create price difference database. \

--------------------- 

## Tables and estimations

1-tables-figures-2020.R: create all tables and figures. \
Tables-10-11-latex.R create the tables based on the information collected in the code 1-tables-figures-2020.R \

**Correlation Regressions**: \
2-regressions-chp4: run all regressions.\

**Causality**: \
First, create the database: chp5-database.R \
Regressions and figures: chp5-figures-regressions.R


---------------------
