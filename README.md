# FlomKart_ShinyApp
Code of the Shiny app developed for the FlomKart project

## Running the app 

#### Directly from Github (NOT READY)

I would like to allow running the app directly from Github (with runGitHub) but I first need to add 2 very small NetCDF files to this repo for demonstration purposes.

```R
library(shiny)

runGitHub("FlomKart_ShinyApp", "fbaffie")

```

#### Locally

To have a copy on your computer, first clone the git repository, then download both full NetCDF files and then use `runApp()`:

```R
# First clone the repository with git. If you have cloned it into
# ~/FlomKart_Shiny, go to that directory and download the NetCDF files

download.file('https://www.dropbox.com/s/bqnc49gd8yucdlz/gof.nc?raw=1', destfile="../data/gof.nc", method="auto")
download.file('https://www.dropbox.com/s/d9f84qz3md2f99b/flood_database.nc?raw=1', destfile="../data/flood_database.nc", method="auto")

# Then use runApp().
setwd("~/FlomKart_Shiny/R")
runApp()
```










## Methodology

We read the NetCDF files created by the FlomKart repo and visualize the results via an R Shiny app.
This helps better finding problems in the creation of the NetCDF files and is also a practical way to graphically explore the results.
We will improve the mapping features in order to better understand how floods statistics may be regionalized.

## Filing issues

Please try to follow those guidelines for filing issues:

One issue for one purpose. Don't add more than one bug, feature request, documentation request, question, etc.. on to the same issue.

- If you've found a bug, thanks for reporting!
- If you've a request of some kind, e.g., feature request or documentation request, it'd be much appreciated if you could add **[Request]** at the beginning of the title. This helps us to prioritise easily without having to go through the entire issue.
- If you need support, e.g., installation issues or upgrade issues, please add **[Support]** at the beginning of the title. This helps us to easily identify the most common support issues, and provide solutions in a separate page.
- If you have a general question, add **[Question]** at the beginning of the title.
- If you've an issue that doesn't fall into any of these categories, then it'd be helpful if you could add **[Misc]** to your title.
