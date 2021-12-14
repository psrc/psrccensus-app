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
                          selected = 1,
                          width = '20rem')

input.vis <- radioButtons('vis_type',
                          label = 'Visual',
                          choices = c('Graph' = 'graph',
                                      'Map' = 'map'),
                          width = '10%')

input.trend <- checkboxInput('trend',
                             label = 'Trend',
                             width = '10%')

input.topic <- selectInput('topic', 
                           'Select Topic', 
                           choices = unique(topic.df$tags))


# main control ------------------------------------------------------------


# table field would come from unique values of concept.
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
              input.topic,
              uiOutput('ui_table')
          ),
          div(class = 'box',
              uiOutput('ui_var_name'),
              uiOutput('ui_var_group_option'),
              uiOutput('ui_ungroup_vars')
          ),
          div(class = 'box',
            uiOutput('ui_dataset'),
            uiOutput('ui_dataset_year')),
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