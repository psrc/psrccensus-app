# tab_files <- list.files(path = "tabs/ui/main", full.names = T)
# suppressMessages(lapply(tab_files, source))

input.outtype <- radioButtons("output_type",
                              label = "Output Type",
                              choices = list("Counties & Region" = 1,
                                             "Tract" = 2,
                                             "Trend" = 3),
                              selected = 1)

input.topic <- selectInput('topic',
                           'Topic',
                           choices = unique(var.df$concept),
                           width = '100%')

# table topic field would come from unique values of concept.
# Variable name from variable_description. 
# Data Source  from census_product.
# Year from census_year.

main.control <- fluidRow(
  div(class = 'main-control',
          div(input.outtype),
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
      div(actionButton("go",
                       label = "Enter"))
      
  ) # end div main-control
) # end fluidRow

vis.section <-  fluidRow(class = 'visual-section',
                         column(tableOutput('main_tbl'),
                                width = 6),
                         column(uiOutput('ui_main_vis'),
                                width = 6)
) # end fluidRow

main <- tabPanel("Main",
                 main.control, 
                 vis.section  
) # end tabPanel