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
    
    ## render ----
    
    # output$var_table <- renderTable({
    #     # dummy table
    #     var.df %>% 
    #         filter(.data$concept %in% input$topic & 
    #                    .data$variable_description %in% input$var_name & 
    #                    .data$census_product %in% input$dataset & 
    #                    .data$census_year %in% input$dataset_year)
    # })
    
}
    # )
