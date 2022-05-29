# election-polls-monitoring
The app of strong web scraping background.

To run app on browser, clkick below link:

https://wpiela.shinyapps.io/election_poll_monitoring/

To run locally on your desktop, paste this code to the console in RStudio:

```
req_packages <- c("googlesheets4","shinyBS","tidyverse","shiny","plotly","rvest","shinysurveys")
new_packages <- req_packages[!(req_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

shiny::runGitHub("election-polls-monitoring","wiktorpiela")
```

