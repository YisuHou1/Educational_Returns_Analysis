# Family Income and Educational Returns in China, 2010-2020: Higher Returns Favor the Wealthy, but Gaps Hold Steady

## Overview

This project examines the influence of family income on economic returns to education in China from 2010 to 2020, using data from the China Family Panel Studies (CFPS). The analysis reveals that individuals from wealthier families consistently enjoy higher financial returns from education compared to those from lower-income families, although the disparities remained stable over the decade. The findings underscore persistent inequalities in education’s economic impact and highlight the need for policies to enhance education's role as an equalizing force.


## File Structure

The repo is structured as:

-   `data/simulated_data` contains generated data that mimics the actual CFPS dataset.
-   `data/raw_data` contains the raw data as obtained from CFPS.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains details about LLM chat interactions, sketches, and supporting materials.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, clean, and test data.


## Data source policies
The China Family Panel Studies (CFPS) forbids the distribution of its raw datasets or modified forms despite being an open source dataset. Thus, placeholders were placed in `data/raw_data` and `data/analysis_data`. The CFPS dataset can be downloaded freely at https://cfpsdata.pku.edu.cn/#/home after creating an account. Replacing the placeholder dataset in `data/raw_data` with downloaded SAS datasets from CFPS2010 to CFPS2020 ensures that all scripts will function normally. The actual analysis datasets are stored as parquet files instead of csv files.

## Statement on LLM usage

Aspects of the code were written with the help of the LLM ChatGPT. The text in the paper were written with the help of ChatGPT, and the complete chatlogs can be found in other/llm_usage.
