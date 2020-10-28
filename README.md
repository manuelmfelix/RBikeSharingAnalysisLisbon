# RBikeSharingAnalysisLisbon
Analysis on Lisbon's Bike Sharing system "GIRAS" using R and Shiny

Find the app's frontend here: https://manuelfelix.shinyapps.io/Giras_Analysis/

## File Explanation:

### teste.csv
  Represents an instance in time of the information gathered through a week in 2020 (28.09 to 04.10)
  It is used for the Giras Analysis app
  
### sumatorio.csv
  Represents the summarized information of the 7 days of gathered data from the GIRAS API, grouped by day
  It is used for the Giras Analysis app
  
### infoGiras.R
  This R file has the code to gather the information from the GIRAS API, treat this information and write on a SQL data base
  
### data_treatment.R
  This R file has the code to treat the information gathered to afterwards produce the csv files for app.R
  It reades the information written on the SQL database
  
### app.R
  This R file has the code that produces the shiny appshowed above
  
There is a file missing that has the full information for each dock, updated every minute for a full week in 2020, It could not be uploaded due to upload size limitations
This file is was created through the usage of the infoGiras.R and data_treatment.R files and the information gathered through the week mentioned above.
