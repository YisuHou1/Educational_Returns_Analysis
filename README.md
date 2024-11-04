# Starter folder

## Overview

This project analyzes the probability of winning for presidential candidates Kamala Harris and Donald Trump on election day. By examining the aggregated polls in the seven swing states and national polls on candidates' support rate, this project identifies that Harris has a slightly lower chance of winning from electoral votes in swing states. This project also explores the temporal trend of candidates' national support rate to predict the popular vote.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from Five Thirty-Eight.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains details about LLM chat interactions and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, clean, and test data.


## Statement on LLM usage

Aspects of the code were written with the help of the LLM ChatGPT. The text in the paper were written with the help of ChatGPT and Claude 3.5 Sonnet and the complete chatlogs can be found in other/llm_usage. The name of the textfiles in this folder describe the part of the paper for which the chatlog applies to/was used in the production of.
