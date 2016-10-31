# FlomKart_DemoApp

This is the demo version of Flom_Kart_ShinyApp.
It includes a very small subsample of the full NetCDF files, in order to load the app into shinyapps.io
However, the RNetCDF package currently leads to an error when trying to deploy to shinyapps.io

You can run the app locally with this command:
```r
# If you don't have the R Shiny package installed:
install.packages('shiny')
# Then load Shiny and run from Github:
library(shiny)
runGitHub("FlomKart_DemoApp", "fbaffie")
```
