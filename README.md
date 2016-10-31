`Warning: This project is still under development and is not fully mature`

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

To have a copy on your computer, first clone the git repository. Then set FlomKart_ShinyApp/R as the working directory, download both full NetCDF files to FlomKart_ShinyApp/data/ and finally use `runApp()` from the working directory:

```R
# First clone the repository with git. You should have cloned it as
# ~/FlomKart_ShinyApp. Set the working directory to the /R sub-folder and download the NetCDF files
setwd("~/FlomKart_Shiny/R")

# Download the data either via the following links, or with R using the code provided below:
# https://www.dropbox.com/s/yyhbts8er96iggb/flood_database.nc?dl=0
# https://www.dropbox.com/s/ou2sxl4hsm0j9rs/gof.nc?dl=0
dl_from_dropbox <- function(x, key) {
     require(RCurl)
     bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                         ssl.verifypeer = FALSE)
     con <- file(paste("../data/", x, sep = ""), open = "wb")
     writeBin(bin, con)
     close(con)
     message(noquote(paste(x, "read into", "~/FlomKart_Shiny/data")))                        
}

dl_from_dropbox("flood_database.nc", "yyhbts8er96iggb")
dl_from_dropbox("gof.nc", "ou2sxl4hsm0j9rs")

# Then use runApp().
runApp()
```

Required packages are specified in global.r and should get installed automatically if missing.

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
