server <- function(input, output, session) {


# main controls -----------------------------------------------------------

    observeEvent(input$ctrlBtn, {
        
        if(input$ctrlBtn %% 2 == 1){
            hide(id = "mainCtrlCont")
        } else {
            show(id = "mainCtrlCont")
        }
        
    })
    
    output$ui_var_name <- renderUI({
        if(is.null(input$topic)) return(NULL)
        
        selectInput('var_name',
                    'Variable',
                    choices = var_names())
        
    })
    
    output$ui_dataset <- renderUI({
        if(is.null(input$var_name)) return(NULL)
        
        selectInput('dataset',
                    'Dataset',
                    choices = dataset(),
                    width = '20rem')
    })
    
    output$ui_dataset_year <- renderUI({
        if(is.null(input$dataset)) return(NULL)
        
        selectInput('dataset_year',
                    'Year',
                    choices = dataset_year(),
                    width = '20rem')
    })

    ## main control reactives ----
    
    var_names <- reactive({
        if(is.null(input$topic)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$census_table_code == input$topic) 
        
        unique(t$variable_description)
    })
    
    dataset <- reactive({
        if(is.null(input$var_name)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$census_table_code == input$topic & .data$variable_description == input$var_name)
        
        sort(unique(t$census_product))
    })
    
    
    dataset_year <- reactive({
        if(is.null(input$dataset)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$census_table_code == input$topic & .data$variable_description == input$var_name & .data$census_product == input$dataset) 
        
        sort(unique(t$census_year))
    })
    

    ## reactives ----
    
    # main_table <- eventReactive(input$go, {
    #     # generate table
    #     # input$trend
    #     # input$geog_type
    #     # input$topic
    #     # input$var_names
    #     # input$dataset
    #     # input$dataset_year
    # 
    #     # query var.df for # 'census_table_code' 'name'
    # 
    #     if(input$dataset %in% c('ACS1', 'ACS5')) {
    #         # Don't forget FIPS
    #         get_acs_recs(geography = input$geog_type,
    #                      table.names = ,
    #                      years = input$dataset_year,
    #                      acs.type = str_to_lower(input$dataset))
    # 
    #     } else if(input$dataset == 'Decennial') {
    #         # use var.df.dist$census_table_code_pad
    #         get_decennial_recs()
    #     }
    # 
    # 
    # 
    # })
    
    ## render visuals ----
    
    output$main_tbl <- renderTable({
        if(input$go == 0) return()
        
        input$go
        
        if(isolate(input$output_type) == 1) {
            mpg %>% 
                slice(1:20)
            
        } else if(isolate(input$output_type) == 2) {
            billboard %>% 
                select(1:6) %>%
                slice(1:20)
        } else {
            starwars %>% 
                select(1:8) %>% 
                slice(1:20)
        }
        
        
    })
    
    output$ui_main_vis <- renderUI({
        if(input$go == 0) return()
        
        input$go
        
        if(isolate(input$output_type) != 2) {
            plotOutput('main_vis')
        } else if(isolate(input$output_type) == 2) {
            div('A Map',
                leafletOutput('main_map'))
            
        }
    })
    
    output$main_vis <- renderPlot({
        
        ggplot(mpg, aes(x = displ, y = cty, color = class)) +
            geom_point() +
            labs(title = 'A Graph')
        
    })
    
    output$main_map <- renderLeaflet({
        
        leaflet() %>%
            addTiles() %>% 
            addMarkers(lng=-122.33781311125064, lat=47.60469023724731)
        
    })
    
    
    
}

