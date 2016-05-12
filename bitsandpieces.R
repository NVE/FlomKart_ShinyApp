## Mainly copied/pasted inspiration for later integration

## EXPERIMENTAL : trying to link selected rows to stations in the map DOESNT WORK WITH THIS VERSION OF DT
# and the dev version of DT did not work on my computer
  output$map4table <- renderLeaflet({
    norway_map4server(input$test.table_rows_selected)
  })

# example use of formattable
df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny", 
           "Hans", "Leo", "John", "Emily", "Lee"), 
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)


formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span",
                    style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span", 
                         style = x ~ style(color = ifelse(x, "green", "red")))
)
)


# I tried to install the development version of DT for the Buttons feature, but it didn't work. maybe for later
# install.packages('devtools')
# devtools::install_github('rstudio/DT')

# example use of DT with highlighting. Looks like a nice way to highlight cells
library('shinydashboard')
header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody(
  DT::dataTableOutput("mtcarsTable")
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    
    output$mtcarsTable <- renderDataTable({
      DT::datatable(datasets::mtcars, 
                    options = list(rowCallback = JS('
                                                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                    // Bold and green cells for conditions
                                                    if (parseFloat(aData[3]) >= 200)
                                                    $("td:eq(3)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[3]) >= 100)
                                                    $("td:eq(3)", nRow).css("background-color", "#9BF59B");
                                                    }')
                    )
                    )
  })
  }
                    )

# shinyjs is installed, it could be interesting

# sparkline to check, could be good complement of DT tables 
# library(devtools)
# install_github('htmlwidgets/sparkline')








