# Server file for Shiny App

# supporting files for the app
source('global.R')  
source('rawdata_plotting.R')
source('mapping.R')
source('main_plotting.R')  # plots for the first "main plots" subtab
source('gof_plotting.R')  # GOF plots
source('rperiods_plotting.R')  # Plots that are a function of return periods (return levels, QS, BS, NT)
source('ui.R')  # User inferface function

# good debuggin tool that stops the browser and lets explore which call cause an error
# options(shiny.error = browser)
# shinyApp(ui, server)  # to run the app


server <- function(session,input, output) {
  
  # To be able to select stations directly on the map (for the first tab) 
  observeEvent(input$map_marker_click, { # update the map markers and view on map clicks
    p <- input$map_marker_click
    leafletProxy("map")
    
    updateSelectInput(session, inputId='station', selected =  p$id, 
                      label = "Pick a station", choices = station$number)
  })
  
  # change the station selection on the first tab when a new station is selected in the rlevels tab
  observeEvent(input$station4rlevels, { 
    updateSelectInput(session, inputId='station', selected = input$station4rlevels, 
                      label = "Pick a station", choices = station$number)
  })
  # and accordingly change the station selection in the rlevels tab when a new station is selected in the main tab
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4rlevels', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for BS
  observeEvent(input$station4bs, { 
    updateSelectInput(session, inputId='station', selected = input$station4bs, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4bs', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for QS
  observeEvent(input$station4qs, { 
    updateSelectInput(session, inputId='station', selected = input$station4qs, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4qs', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for NT
  observeEvent(input$station4nt, { 
    updateSelectInput(session, inputId='station', selected = input$station4nt, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4nt', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for CV
  observeEvent(input$station4cv, { 
    updateSelectInput(session, inputId='station', selected = input$station4cv, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4cv', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for KS
  observeEvent(input$station4ks, { 
    updateSelectInput(session, inputId='station', selected = input$station4ks, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4ks', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for AD
  observeEvent(input$station4ad, { 
    updateSelectInput(session, inputId='station', selected = input$station4ad, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4ad', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  # Same thing for CS
  observeEvent(input$station4cs, { 
    updateSelectInput(session, inputId='station', selected = input$station4cs, 
                      label = "Pick a station", choices = station$number)
  })
  observeEvent(input$station, { 
    updateSelectInput(session, inputId='station4cs', selected = input$station, 
                      label = "Pick a station", choices = station$number)
  })
  
  
  # This conditional use of "observe" updates the random runs selection when the "FULL RECORD" is selected
  observe({
    if(input$length == "FULL RECORD") { 
      output$random.panel <- renderUI({ selectInput(inputId='random', selected =  1, 
                                                    label = "There are no subsamples", choices = 1)
      })  
      #       cat(file="output/app_output.txt", "full record has been selected for station", input$station, "\n") # test of debugging functionality 
      
    } else {
      output$random.panel <- renderUI({ sliderInput("random", 
                                                    "Browse the random runs", 
                                                    value = 25,
                                                    min = 1, 
                                                    max = 50)
      })
    } 
  }) 
  
  ## Conditional use of observe for the special return periods of BS and NT
  observe({
    if(input$coeffvar2plot_ave == "BS" || input$coeffvar2plot_ave == "NT") { 
      output$r.period4coefvar_ave <- renderUI({ selectInput(inputId='r.period4coefvar_ave', selected =  10, 
                                                    label = "Which return period to plot?", choices = rperiods.bs)
      })  

    } else {
      output$r.period4coefvar_ave <- renderUI({ selectInput('r.period4coefvar_ave', selected = 100, label='Which return period to plot?', 
                                                                        choices = return.periods)
      })
    } 
  }) 
  ## Same thing for BS only in the single station CV
  observe({
    if(input$coeffvar2plot == "BS") { 
      output$r.period4coefvar <- renderUI({ selectInput(inputId='r.period4coefvar', selected =  10, 
                                                            label = "Which return period to plot?", choices = rperiods.bs)
      })  
      
    } else {
      output$r.period4coefvar <- renderUI({ selectInput('r.period4coefvar', selected = 100, label='Which return period to plot?', 
                                                            choices = return.periods)
      })
    } 
  }) 
  ## Same thing for BS in the main subtab selection for the table
  observe({
    if(input$gof2table2 == "BS") { 
      output$r.period4table <- renderUI({ selectInput(inputId='r.period4table', selected =  10, 
                                                        label = "Choose a return period for the table", choices = rperiods.bs)
      })  
      
    } else {
      output$r.period4table <- renderUI({ selectInput('r.period4table', selected = 100, label='Choose a return period for the table', 
                                                        choices = return.periods)
      })
    } 
  }) 

  ## Below are reactive variables based on inputs selected in the UI ------------------
  
  old_station.index <- reactive({ station$index[which(station$number == input$station)]  
    # station$index is called in order to go back
    # to the original indexes of the nc file
  })
  new_station.index  <- reactive({ which(station$number == input$station)  
  })
  
  distr.index <- reactive({ which(distr.name == input$distr)
  })
  method.index <- reactive({ which(method.name == input$method)
  })
  random.index <- reactive({ as.numeric(input$random)
  })
  length.index <- reactive({ which(sampling_years_full_record == input$length)
  })
  distr2plot <- reactive({ which(distr.name == input$distr2plot)
  })
  method2plot <- reactive({ which(method.name == input$method2plot)
  })
  param.estimate <- reactive({ var.get.nc(nc, "param.estimate", 
                                          start = c(old_station.index(), distr.index(), method.index(), 1, length.index(), random.index()), 
                                          count = c(1, 1, 1, 3, 1, 1)) 
  }) 
  
  ## Rendering plots of the first tab
  output$qdata_boxplot <- renderPlot({
    qdata_boxplot(as.numeric(input$min_years), as.numeric(input$max_years), as.numeric(input$min_height), as.numeric(input$max_height))
  })
  output$qdata_barplot <- renderPlot({
    qdata_barplot(as.numeric(input$min_years), as.numeric(input$max_years), as.numeric(input$min_height), as.numeric(input$max_height))
  })

  
  ## Rendering of the goodness of fit plots for the related GOF tabs -------------------
  output$plot.ks <- renderPlot({
    plot4server_gof(old_station.index(), "KS")
  })
  
  output$plot.ad <- renderPlot({
    plot4server_gof(old_station.index(), "AD")
  })
  output$plot.cs <- renderPlot({
    plot4server_gof(old_station.index(), "CS")
  })
  output$plot.rlevels <- renderPlot({
    plot4server_gof.rlevels(old_station.index(), "r.levels", input$r.period)
  })
  output$plot.qs <- renderPlot({
    plot4server_gof.rlevels(old_station.index(), "QS", input$r.period4qs)
  })
  output$plot.bs <- renderPlot({
    plot4server_gof.rlevels(old_station.index(), "BS", input$r.period4bs)
  })
  output$plot.nt <- renderPlot({
    plot4server_nt(old_station.index(), "NT", input$r.period4nt)
  })
  
  output$plot.rlevels_coeff <- renderPlot({
    plot4server_rlevels_coeffvar(old_station.index(), input$coeffvar2plot, input$r.period4coefvar)
  })
  output$plot.gof_averaged <- renderPlot({
    plot4server_gof_averaged(input$gof4ave, input$min_years4ave, input$max_years4ave)
  })
  output$plot.rlevels_coeff_averaged <- renderPlot({
    plot4server_rlevels_coeffvar_ave(input$coeffvar2plot_ave, input$r.period4coefvar_ave, input$min_years4coeff_ave, input$max_years4coeff_ave)
  })
  
  
  ## Main plots for the first tab ---------------  
  
  output$main.plot <- renderPlot({
    plot4server(na.omit(Q[old_station.index(), ]), 
                param.estimate(),
                distr = distr.index())
  })
  output$rlevels.plot <- renderPlot({
    plot4server_rlevel(na.omit(Q[old_station.index(), ]), 
                       param.estimate(),
                       distr = distr.index())
  })
  output$cdf.plot <- renderPlot({
    plot4server_cdf(na.omit(Q[old_station.index(), ]), 
                    param.estimate(),
                    distr = distr.index())
  })
  output$qq.plot <- renderPlot({
    plot4server_qq(na.omit(Q[old_station.index(), ]), 
                   param.estimate(),
                   distr = distr.index()) 
  })
  output$map <- renderLeaflet({
    norway_map4server(input$station)
  })
  
  # Formattable tables, early experiments, the 2 visualizations could be switched with a button
  output$gof.table <- renderFormattable({ 
    
    temp.list <- gof_summary(input$gof2table, old_station.index())
    # print(temp.list$max_gof)
    # print(temp.list$min_gof)
    
    formattable(temp.list$gof_table, list(
      # Strangely, the condition didn't work with x == temp.list...
      gum = formatter("span", style = x ~ ifelse(abs(x - temp.list$min_gof) < 0.00001, style(color = "green", font.weight = "bold"),
                                                 ifelse(abs(x - temp.list$max_gof) < 0.00001, style(color = "red", font.weight = "bold"), NA))),
      
      gam = formatter("span", style = x ~ ifelse(abs(x - temp.list$min_gof) < 0.00001, style(color = "green", font.weight = "bold"),
                                                 ifelse(abs(x - temp.list$max_gof) < 0.00001, style(color = "red", font.weight = "bold"), NA))),
      
      gev = formatter("span", style = x ~ ifelse(abs(x - temp.list$min_gof) < 0.00001, style(color = "green", font.weight = "bold"),
                                                 ifelse(abs(x - temp.list$max_gof) < 0.00001, style(color = "red", font.weight = "bold"), NA))),
      
      glo = formatter("span", style = x ~ ifelse(abs(x - temp.list$min_gof) < 0.00001, style(color = "green", font.weight = "bold"),
                                                 ifelse(abs(x - temp.list$max_gof) < 0.00001, style(color = "red", font.weight = "bold"), NA))),
      
      pe3 = formatter("span", style = x ~ ifelse(abs(x - temp.list$min_gof) < 0.00001, style(color = "green", font.weight = "bold"),
                                                 ifelse(abs(x - temp.list$max_gof) < 0.00001, style(color = "red", font.weight = "bold"), NA)))
    ))
    
  })
  
  output$gof.table2 <- renderFormattable({ 
    # This is probably not the most helpful way to format this table.
    formattable(gof_summary_rperiods(input$gof2table2, old_station.index(), input$r.period4table), list(
      gum = color_tile("white", "pink"),
      gam = color_tile("white", "pink"),
      gev = color_tile("white", "pink"),
      glo = color_tile("white", "pink"),
      pe3 = color_tile("white", "pink")
    ))
  })
  
  # Output DT tables to have some excel feature tables at the end of the app
  output$test.table <- DT::renderDataTable({
    datatable(stations.summary.df,
              # extensions = 'Scroller', 
              filter = 'top', 
              options = list(
                # dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), # this was for extensions = 'Buttons' which didn't work
                
                #               deferRender = TRUE,
                #               scrollY = 200,
                #               scroller = TRUE,  
                
                pageLength = 20, autoWidth = TRUE, 
                
                order = list(list(2, 'asc'))
              )
              
              
    )  
    #     %>% formatStyle(
    #     'Station.name',
    #      backgroundColor = styleInterval(3.4, c('white', 'grey'))
    #    )  # Can be used for additional styling
  })

  # Computing a reactive group of stations based on best gof performance
  st_group.indexes <- reactive({ station_group_indexes(input$gof4st_groups, input$distr4st_groups, input$method4st_groups, input$minmax)
  })
  
  # Computing a reactive group of stations based on slection in first tab
  st_group_first_tab.indexes <- reactive({ station_group_indexes_first_tab(as.numeric(input$min_years), as.numeric(input$max_years), 
                                                                           as.numeric(input$min_height), as.numeric(input$max_height))
  })
  
  # Mapping the groups of stations that have same best method and distr
  output$map.groups_from_gof <- renderLeaflet({
    norway_map4groups(st_group.indexes())
    
  })
  
  # Mapping the values of a specific parameter for a specific set of distr/method
  output$map.param_values <- renderLeaflet({
    norway_map4param_values(input$dist4param_maps, input$method4param_maps, input$param4param_maps)
    
  })
  
  # Plotting the histogram of the paramter values (goes along the map above)
  output$param.histo <- renderPlot({ 
    histo4param_values(input$dist4param_maps, input$method4param_maps, input$param4param_maps)
    })
  
  # Mapping common stations for the first tab
  output$map.groups_first.tab <- renderLeaflet({
  norway_map4groups(st_group_first_tab.indexes())
    
  })
  
  
  # Table for the mapped stations
  output$group.table <- DT::renderDataTable({
    datatable(group.dfmaker(st_group.indexes()),
              # extensions = 'Scroller', 
              filter = 'top', 
              options = list(
                pageLength = 20, autoWidth = TRUE, 
                order = list(list(2, 'asc'))
              )
    )
  })
  
}  # end of server function


