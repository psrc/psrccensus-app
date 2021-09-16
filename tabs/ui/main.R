# tab_files <- list.files(path = "tabs/ui/main", full.names = T)
# suppressMessages(lapply(tab_files, source))

input.topic <- selectInput('topic',
                           'Topic',
                           choices = unique(var.df$concept)
)

# table topic field would come from unique values of concept.
# Variable name from variable_description. 
# Data Source  from census_product.
# Year from census_year.

main <- tabPanel("Main",
                 fluidRow(
                   div(class = 'main-control',
                       div(
                         radioButtons("output_type",
                                      label = "Output Type",
                                      choices = list("Counties & Region" = 1,
                                                     "Tract" = 2,
                                                     "Trend" = 3),
                                      selected = 1)
                       ),
                       div(
                         div(class = 'top-selection',
                             input.topic
                         ),
                         div(class = 'bottom-selection',
                             uiOutput('ui_var_name')
                         )
                       ),
                       div(
                         div(class = 'top-selection',
                             uiOutput('ui_dataset')
                         ),
                         div(class = 'bottom-selection',
                             uiOutput('ui_dataset_year')
                         )
                       ),
                    
                       div(
                         actionButton("go",
                                      label = "Enter")
                       )
                   ) # end div main-control
                 ), # end fluidRow
                 fluidRow(
                   column(uiOutput('ui_main_tbl'),
                          width = 6),
                   column(uiOutput('ui_main_vis'),
                          width = 6)
                   
                 )
                 
) # end tabPanel