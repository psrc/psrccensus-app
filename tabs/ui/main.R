extra_files <- list.files(path = "tabs/ui/main", full.names = T)
suppressMessages(lapply(extra_files, source))

input.geog <- selectInput('geog_type',
                          label = 'Geography',
                          choices = c('Counties & Region' = 1,
                                      'Tract' = 2,
                                      'MSA' = 3,
                                      'Place' = 4
                                      ),
                          selected = 1)

input.fips <- conditionalPanel(condition = "input.geog_type == 3 | input.geog_type == 4",
                                      textInput("fips", 
                                                label = 'Enter FIPS',
                                                placeholder = '14740, 42660'))

input.vis <- radioButtons('vis_type',
                          label = 'Visual',
                          choices = c('Graph' = 1,
                                      'Map' = 2))

input.trend <- checkboxInput('trend',
                             label = 'Trend')

input.topic <- selectInput('topic',
                           'Topic',
                           choices = vars)

# table topic field would come from unique values of concept.
# Variable name from variable_description. 
# Data Source from census_product.
# Year from census_year.

main.control <- fluidRow(
  div(id = 'mainCtrlBox',
      class = 'background',
      div(id = 'mainCtrlCont',
          class = 'main-control',
          div(
            div(
              input.geog,
              input.fips),
            div(class = 'main-control-grp',
                input.vis,
                input.trend)
          ),
          div(
            div(
              input.topic,
              uiOutput('ui_var_name')
            ),
            div(
              uiOutput('ui_dataset'),
              uiOutput('ui_dataset_year')
            )
          ),
          div(actionButton("go",
                           label = "Enter"))
      ), # end .main-control
      
      # hide/show controls
        actionButton('ctrlBtn',
                     class = 'hide-show-btn',
                     label = fa("angle-double-up", fill = "#318ce7", width = '1rem'))
      
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
                 vis.section)