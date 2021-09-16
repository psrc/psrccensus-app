server <- function(input, output, session) {


# main controls -----------------------------------------------------------

    
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

    ## reactives ----
    
    dataset_year <- reactive({
        if(is.null(input$dataset)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$concept == input$topic & .data$variable_description == input$var_name & .data$census_product == input$dataset) 
        
        unique(t$census_year)
    })
    
    dataset <- reactive({
        if(is.null(input$var_name)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$concept == input$topic & .data$variable_description == input$var_name)
        
        unique(t$census_product)
    })
    
    var_names <- reactive({
        if(is.null(input$topic)) return(NULL)
       
        t <- var.df %>% 
            filter(.data$concept == input$topic) 
        
        unique(t$variable_description)
    })
    
    ## render visuals ----
    
    output$ui_main_tbl <- renderUI({
        
    })
    
    output$ui_main_vis <- renderUI({
        input$go
        
        if(isolate(input$output_type) != 2) {
            plotOutput('main_vis')
        } else if(isolate(input$output_type) == 2) {
            leafletOutput('main_map')
        }
    })
    
    output$main_vis <- renderPlot({
        input$go
        
        ggplot(mpg, aes(x = displ, y = cty, color = class)) +
            geom_point() +
            labs(title = 'A Graph')
    })
    
    output$main_map <- renderLeaflet({
        input$go
        
        leaflet() %>%
            addTiles() %>% 
            addMarkers(lng=-122.33781311125064, lat=47.60469023724731, popup="PSRC")
    })
    
   
    
}

