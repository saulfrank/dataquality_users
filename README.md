# To run data quality script

## Using RStudio

## Run install packages in command line:
install.packages("data.table", repos="http://R-Forge.R-project.org")
install.packages("RJSONIO")
install.packages("phonenumber")
install.packages("sqldf")

## Ensure the following files are in the same folder:
•	Data Cleanse 6.R
•	File_test_run.csv 

## Run the R script: Data Cleanse 6.R

## The output includes
1.	Correcting the incorrect data against the test cases
2.	Creating a data quality map which is a JSON array of what corrections were made
3.	Exception column to flag data that cannot be programmatically corrected and should be fixed at source.

## Generate test data using node 
Install Node

sudo npm install faker
sudo npm install json2csv

Run:
node user_generate.js

This will generate: file.csv

This file is used in file_test_cases.xlsx to create the test cases in file_test_run.csv
