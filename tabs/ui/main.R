extra_files <- list.files(path = "tabs/ui/main", full.names = T)
suppressMessages(lapply(extra_files, source))


# widgets -----------------------------------------------------------------


input.geog <- selectInput('geog_type',
                          label = 'Geography',
                          choices = c('Counties & Region' = 'county',
                                      'Tract' = 'tract',
                                      'MSA' = 'msa',
                                      'Place' = 'place'
                                      ),
                          selected = 1)

input.vis <- radioButtons('vis_type',
                          label = 'Visual',
                          choices = c('Graph' = 'graph',
                                      'Map' = 'map'))

input.trend <- checkboxInput('trend',
                             label = 'Trend')

input.topic <- selectizeInput('topic',
                              label = 'Topic',
                              choices = NULL,
                              options = list(placeholder = 'Type keyword(s) or code and select'))


# main control ------------------------------------------------------------


# table topic field would come from unique values of concept.
# Variable name from variable_description. 
# Data Source from census_product.
# Year from census_year.

main.control <- fluidRow(
  div(id = 'mainCtrl',
      class = 'background',
      div(id = 'mainCtrlCont',
          class = 'main-control',
          div(class = 'box',
            div(
              input.geog,
              uiOutput('ui_fips')
              ),
            div(class = 'main-control-grp',
                input.vis,
                input.trend)
          ),
          div(class = 'box',
            div(
              input.topic,
              uiOutput('ui_var_name')
            ),
            div(
              uiOutput('ui_dataset'),
              uiOutput('ui_dataset_year')
            )
          ),
          div(downloadButton("download", "Download Data"),
              actionButton("go", "Enter"))
      ), # end .main-control
      
      # hide/show controls
        actionButton('ctrlBtn',
                     class = 'hide-show-btn',
                     label = fa("angle-double-up", fill = "#318ce7", width = '1rem'))
      
  ) # end div main-control
) # end fluidRow


# vis section -------------------------------------------------------------


vis.section <-  fluidRow(class = 'visual-section',
                         column(DTOutput('main_tbl'),
                                width = 6),
                         column(uiOutput('ui_main_vis'),
                                width = 6)
) # end fluidRow


# main tab ----------------------------------------------------------------


main <- tabPanel("Main",
                 main.control,
                 vis.section)