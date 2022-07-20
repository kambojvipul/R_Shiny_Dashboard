# Interactive Dashboard developed using R Shiny for the Physician Performance

The indicator of the physician's performance needs to be evolved using given datasets of the Physicians & Patients Characteristics, ICU and the physicians rating given by 360 evaluation. In view of the above, the dashboard for the physician performance is hereby developed in R Shiny Application using R programming language. 

# Dataset

Before running of code, kindly download the dataset from github and change their path in App.R code file. 

# Dependencies

install.packages(c("shinydashboard", "ggplot2", "shinydashboardPlus", "shiny", "DT", "corrplot", "PerformanceAnalytics", "wesanderson", "hrbrthemes",
 "viridis", "reshape2"))
 
 # Executing program

runApp(app.R)

# Alternatively, the dashboard can be run straight from the R console:

library(shiny)
runGitHub("penguin-shinydashboard", "gaudet-d")
