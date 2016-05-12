# A shiny app gets called this way: shiny::runApp()
# Libraries
library(shiny)

ui <- navbarPage("Flood frequency analysis",  # cut off:  id = "nav",
                 
                 tabPanel("About", icon = icon("info"),
                          fluidRow(
                            
                            HTML('
                                 <p style="text-align:justify"> This app lets you explore various frequency analysis options for fitting flood data in Norway </p>
                                 
                                 <p style="text-align:justify"> <b> Research questions </b> </p>
                                 
                                 <p style="text-align:justify"> Which distribution gives the best fit to the data? </p>
                                 <p style="text-align:justify"> Which estimation method provides the best fit to the data? </p>
                                 <p style="text-align:justify"> Does the answer depend on local data availability? </p>
                        
                          <p style="text-align:justify"> <big> <b> Probability distributions </b> </big> </p>
                          <div> <dl>
                        <p style="text-align:justify"> Gamma </p>
                        <p style="text-align:justify"> Gumbel </p>
                        <p style="text-align:justify"> GEV: Generalized extreme value </p>
                        <p style="text-align:justify"> GL: Generalized logistics </p>
                        <p style="text-align:justify"> Pearson III </p>
</div> </dl>                          

<p style="text-align:justify"> <b> Estimations methods </b> </p>
                          <p style="text-align:justify"> MLE : maximum likelihood estimation </p>
                          <p style="text-align:justify"> Lmom: Linear moments </p>
                          <p style="text-align:justify"> MOM: Ordinary moments </p>
                        <p style="text-align:justify"> Bayesian estimation </p>'
                                 
                                 
                            )),
                          fluidRow(
                            column(3, wellPanel(
                              selectInput(inputId='min_years', selected = '60', label='Select a minimum number of years', 
                                          choices = c(30,60,90,120))
                            )
                            ),
                            column(3, wellPanel(
                              selectInput(inputId='max_years', selected = '80', label='Select a maximum number of years', 
                                          choices = c(60,90,120,150))
                            )
                            ),
                            column(3, wellPanel(
                              selectInput(inputId='min_height', selected = '0', label='Select the catchment minimum elevation', 
                                          choices = seq(0,2500,500))
                            )
                            ),
                            column(3, wellPanel(
                              selectInput(inputId='max_years', selected = '1000', label='Select the catchment minimum elevation', 
                                          choices = seq(0,2500,500))
                            )
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 # tagList(
                 navbarMenu("Single station analysis", icon = icon("compress"),
                            tabPanel("Main plots",
                                     
                                     fluidRow(
                                       
                                       column(2, wellPanel(actionButton("help_loc_btn", "About this tab", class="btn-block"), br())),
                                       bsModal("modal_loc", "About", "help_loc_btn", size="large",
                                               HTML('
                                                              <p style="text-align:justify"> This app lets you explore various frequency analysis options for fitting flood data in Norway </p>
                                                              
                                                              <p style="text-align:justify"> Our research questions are </p>
                                                              
                                                              <p style="text-align:justify"> Which distribution gives the best fit to the data? </p>
                                                              <p style="text-align:justify"> Which estimation method provides the best fit to the data? </p>
                                                              <p style="text-align:justify"> Does the answer depend on local data availability? </p>'
                                                    
                                                    
                                               )),                                  
                                       
                                       column(2, wellPanel(
                                         selectInput(inputId='distr', selected = 'gamma', label='Select a probability distribution', 
                                                     choices = distr.name)
                                       )
                                       ),
                                       column(2, wellPanel(
                                         selectInput(inputId='method', label = "Select an estimation method", choices = method.name)
                                       )
                                       ),
                                       column(2, wellPanel(
                                         selectInput(inputId='station', selected =  station$number[37], 
                                                     label = "Pick a station", choices = station$number)
                                       )
                                       ),
                                       
                                       column(2, wellPanel(
                                         selectInput(inputId='length', label = "Choose a record length", selected = 30,
                                                     choices = c('FULL RECORD', sampling_years))  # Sampling years should be recalculated by the app
                                         
                                       )
                                       ),
                                       column(2, wellPanel(
                                         uiOutput("random.panel") 
                                       )
                                       ),
                                       bsTooltip("distr", "Gamma and gumbel are 2-parameter distributions, the others use 3", "bottom", options = list(container="body")),
                                       bsTooltip("station", "A station can also be selected by clicking on its marker in the map below", "bottom", options = list(container="body"))
                                     ),
                                     
                                     
                                     fluidRow(
                                       column(3, plotOutput('main.plot')),
                                       column(3, plotOutput('rlevels.plot')),  # plot4server_rlevel 
                                       column(3, plotOutput('cdf.plot')),
                                       column(3, plotOutput('qq.plot')) # plot4server_rlevel
                                     ),  
                                     
                                     fluidRow( 
                                       
                                       column(5, 
                                              h4("Goodness of fit summary"),
                                              formattableOutput("gof.table"),
                                              # uiOutput("gof.table"),  # Switch to this if plotting the interactive table (highlight)
                                              
                                              h4("Summary for return levels, QS or BS"),
                                              formattableOutput("gof.table2")
                                              
                                              
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='gof2table', selected = 'KS', label='Select a goodness of fit measure to summarize', 
                                                     choices = c('CS', 'KS', 'AD')),
                                         
                                         selectInput(inputId='gof2table2', selected = 'r.levels', label='Select a goodness of fit measure to summarize', 
                                                     choices = c('r.levels', 'QS', 'BS')),
                                         
                                         selectInput(inputId='r.period4table', selected = 100, label='Choose a return period for the table', 
                                                     choices = return.periods)
                                         
                                       )
                                       
                                       ),
                                       column(4, leafletOutput('map'))
                                     )  # closing last fluidrow   
                            ),  # closing the first tab
                            
                            tabPanel("Return levels",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period', selected = 100, label='Which return period to plot?', 
                                                     choices = return.periods)
                                      )
                                       ),
                                       plotOutput('plot.rlevels', width = "100%", height = "800px")
                                     )
                            ), # closing tab
                            
                            tabPanel("Quantile score",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period4qs', selected = 100, label='Which return period to plot?', 
                                                     choices = return.periods)
                                       )
                                       ),
                                       plotOutput('plot.qs', width = "100%", height = "800px")
                                     )
                            ), # closing tab
                            
                            tabPanel("Brier score",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period4bs', selected = 10, label='Which return period to plot?', 
                                                     choices = rperiods.bs)
                                       )
                                       ),
                                       plotOutput('plot.bs', width = "100%", height = "800px")
                                     )
                            ), # closing tab
                            tabPanel("NT",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period4nt', selected = 10, label='Which return period to plot?', 
                                                     choices = rperiods.bs)
                                       )
                                       ),
                                       plotOutput('plot.nt', width = "100%", height = "800px")
                                     )
                            ), # closing tab
                            
                            tabPanel("Coefficients of variation",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period4coefvar', selected = 100, label='Which return period to plot?', 
                                                     choices = return.periods)
                                       )
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='coeffvar2plot', selected = 'r.levels', label='Select a goodness of fit measure to summarize', 
                                                     choices = c('r.levels', 'QS', 'BS'))
                                       )),
                                       
                                       plotOutput('plot.rlevels_coeff', width = "100%", height = "800px")
                                     )
                            ), # closing tab
                            tabPanel("Kolmogorov Smirnoff",
                                     fluidRow(
                                       plotOutput('plot.ks', width = "100%", height = "900px")
                                     )
                            ), # closing tab
                            
                            tabPanel("Anderson Darling",
                                     fluidRow(
                                       plotOutput('plot.ad', width = "100%", height = "900px")
                                     )
                            ), # closing tab
                            
                            tabPanel("Chi Square",
                                     fluidRow(
                                       plotOutput('plot.cs', width = "100%", height = "900px")
                                     )
                            ) # closing tab
                 ),  # Closing navbarMenu
                 
                 navbarMenu("Station averaged analysis", icon = icon("expand"),
                            
                            tabPanel("Station-averaged CV",
                                     fluidRow(
                                       column(3, wellPanel(
                                         selectInput(inputId='r.period4coefvar_ave', selected = 100, label='Which return period to plot?', 
                                                     choices = return.periods)
                                       )
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='coeffvar2plot_ave', selected = 'r.levels', label='Select a goodness of fit measure to summarize', 
                                                     choices = c('r.levels', 'QS', 'BS', 'NT'))
                                       )),
                                       
                                       
                                       column(3, wellPanel(
                                         selectInput(inputId='min_years4coeff_ave', selected = '60', label='Select a minimum number of years', 
                                                     choices = c(30,60,90,120))
                                       )
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='max_years4coeff_ave', selected = '80', label='Select a maximum number of years', 
                                                     choices = c(60,90,120,150))
                                       )
                                       )
                                       
                                     ),
                                     fluidRow(        
                                       plotOutput('plot.rlevels_coeff_averaged', width = "100%", height = "800px")
                                     )
                            ), # closing tab  
                            
                            tabPanel("Station-averaged GOF",
                                     fluidRow( 
                                       column(3, wellPanel(
                                         selectInput(inputId='gof4ave', selected = 'KS', label='Select a goodness of fit measure', 
                                                     choices = c('CS', 'KS', 'AD'))
                                       )
                                       ),
                                       column(4, wellPanel(
                                         selectInput(inputId='min_years4ave', selected = '60', label='Select a minimum number of years', 
                                                     choices = c(30,60,90,120))
                                       )
                                       ),
                                       column(4, wellPanel(
                                         selectInput(inputId='max_years4ave', selected = '90', label='Select a maximum number of years', 
                                                     choices = c(60,90,120,150))
                                       )
                                       )
                                       
                                     ),
                                     fluidRow(        
                                       plotOutput('plot.gof_averaged', width = "100%", height = "900px")
                                     )
                                     
                            ) # closing tab
                 ),  # Closing navbarMenu
                 
                 navbarMenu("Mapping tools",
                            
                            tabPanel("Mapping GOF-based groups of stations",  
                                     
                                     fluidRow( 
                                       column(3, wellPanel(
                                         selectInput(inputId='gof4st_groups', selected = 'KS', label='Select a goodness of fit measure', 
                                                     choices = c('CS', 'KS', 'AD'))
                                       )
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='minmax', label = "Want to get min GOF (best perf) or max GOF", choices = c("min", "max")) )
                                       ),
                                       
                                       column(3, wellPanel(
                                         selectInput(inputId='distr4st_groups', selected = 'gamma', label='Select a probability distribution', 
                                                     choices = c(distr.name, "ALL"))
                                       )
                                       ),
                                       column(3, wellPanel(
                                         selectInput(inputId='method4st_groups', label = "Select an estimation method", choices = c(method.name, "ALL")) )
                                         
                                       )
                                     ),
                                     fluidRow(         
                                       column(9, DT::dataTableOutput('group.table')
                                       ),
                                       column(3,
                                              h4("Maps of stations with common best method and distribution"),
                                              leafletOutput('map.groups_from_gof', height = "800px")
                                       )
                                       
                                       
                                     )
                            ), icon = icon("globe") # closing tab
                 ),  # Closing navbarMenu
                 
                 navbarMenu("Table tools",
                            
                            tabPanel("Table summary",  
                                     fluidRow(     
                                       h4("Table"),
                                       DT::dataTableOutput('test.table')
                                     )
                                     #            leafletOutput('map4table') # DOESNT WORK WITH THIS VERSION OF DT
                            ), icon = icon("table") # closing tab
                 ),  # Closing navbarMenu 
                 windowTitle = "Explore Big Mama!"                      
)  # Closing navbarPage

# Closing UI












# to check out from Radiant https://github.com/vnijs/radiant/blob/master/inst/analytics/ui.R
# radiant@rady.ucsd.edu
