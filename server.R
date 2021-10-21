server <- function(input, output, session) {


# main controls -----------------------------------------------------------
    
    updateSelectizeInput(
        session, 
        'topic', 
        server = TRUE,
        choices = vars
    )
    
    observeEvent(input$ctrlBtn, {
        
        if(input$ctrlBtn %% 2 == 1) {
            hide(id = "mainCtrlCont")
        } else {
            show(id = "mainCtrlCont")
        }
        
    })
    
    observeEvent(input$ctrlBtn, {
        if(input$ctrlBtn %% 2 == 1) {
            updateActionButton(session, 
                               "ctrlBtn",
                               label = fa("angle-double-down", fill = "#318ce7", width = '1rem'))
        } else {
            updateActionButton(session, 
                               "ctrlBtn",
                               label = fa("angle-double-up", fill = "#318ce7", width = '1rem'))
        }
        
    })
    
    observe({
        if(is.null(input$var_name)) return()
        
        input$var_name
        input$geog_type 
        
        if(input$var_name != 'all' & input$geog_type == 'tract') {
            updateRadioButtons(session, 
                               'vis_type', 
                               label = 'Visual', 
                               choices = c('Map' = 'map'),
                               selected = "map")
        } else if(input$var_name == 'all') {
            disable("vis_type")
        } else {
            updateRadioButtons(session, 
                               'vis_type', 
                               label = 'Visual', 
                               choices = c('Graph' = 'graph', 'Map' = 'map'),
                               selected = "map")
        }
        
    })
    
    output$ui_var_name <- renderUI({
        if(is.null(input$topic)) return(NULL)
        
        selectInput('var_name',
                    'Variable',
                    choices = c('All Variables' = 'all', var_names())
                    )
        
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
        # populate variable dropdown
        if(is.null(input$topic)) return(NULL)
        
        t <- var.df %>% 
            filter(.data$census_table_code == input$topic) 
       
        t.dist <- t %>% 
            select(.data$variable_description, .data$name) %>% 
            distinct()
        
        vars <- t.dist$name
        names(vars) <- t.dist$variable_description
        return(vars)
    })
    
    dataset <- reactive({
        # populate dataset dropdown
        if(is.null(input$var_name)) return(NULL)
     
        t <- var.df %>% 
            filter(.data$census_table_code == input$topic)
        
        sort(unique(t$census_product))
    })
    
    
    dataset_year <- reactive({
        # populate year dropdown
        if(is.null(input$dataset)) return(NULL)

        t <- var.df %>% 
            filter(.data$census_table_code == input$topic & .data$census_product == input$dataset) 
        
        sort(unique(t$census_year))
    })
    

    ## reactives ----
    
    main_table <- eventReactive(input$go, {
        
        # clean and vectorize fips
        if(!is.null(input$fips)) {
            fips <- unlist(str_split(input$fips, ",\\s"))
        } else {
            fips <- NULL
        }
        
        # access Census API
        if(input$dataset %in% c('ACS1', 'ACS5')) {
            incProgress(message = 'Gathering ACS data')
            
            if(input$geog_type == 'place') {
                # psrccensus get_acs_recs() doesn't filter place geogs
                recs <- get_acs_recs(geography = input$geog_type,
                                     table.names = input$topic,
                                     years = as.numeric(input$dataset_year),
                                     acs.type = str_to_lower(input$dataset)) %>% 
                    filter(GEOID %in% fips)
            } else {
                recs <- get_acs_recs(geography = input$geog_type,
                                     table.names = input$topic,
                                     years = as.numeric(input$dataset_year),
                                     FIPS = fips,
                                     acs.type = str_to_lower(input$dataset))
            }
            

        } else if(input$dataset == 'Decennial') {
            incProgress(message = 'Gathering Decennial Census data')
            
            # find the padded table code for Decennial tables 
            dec_tbl_code <- var.df.dist %>% 
                filter(.data$census_table_code == input$topic) %>% 
                pull(.data$census_table_code_pad)
            
            recs <- get_decennial_recs(geography = input$geog_type,
                                       table_codes = dec_tbl_code,
                                       years = as.numeric(input$dataset_year),
                                       fips = fips)
        }
        
        incProgress(amount = .5, message = 'Data gathered')
        
        # filter for variable
        if(input$var_name != 'all') {
            recs <- recs %>%
                filter(.data$variable == input$var_name)
        }
        
        incProgress(amount = .4, message = 'Ready to render data')
        
        return(recs)
    })
    
    ## render visuals ----
    
    output$main_tbl <- renderDT({
        if(input$go == 0) return()
        input$go
        
        withProgress(df <- main_table(),
                     detail = 'This may take a while...')

        if(input$dataset != 'Decennial') {
            hide_cols <- c('concept', 'census_geography', 'acs_type', 'year')
            target <- which(colnames(df) %in% hide_cols)
        } else {
            target <- NULL
        }
        
        datatable(main_table(),
                  options = list(columnDefs = list(list(visible = FALSE, targets = target))))
    })
    

# download data  ----------------------------------------------------------
    
    
    ## Enable/Disable download button ----
    v <- reactiveValues(geog_type = NULL,
                        fips = NULL,
                        vis_type = NULL,
                        topic = NULL,
                        var_name = NULL,
                        dataset = NULL,
                        dataset_year = NULL,
                        go = 0
                        )
    
    observeEvent(input$go, {
        # store values in reactive value list after clicking Enter
        
        v$geog_type <- input$geog_type
        v$fips <- input$fips
        v$vis_type <- input$vis_type
        v$topic <- input$topic
        v$var_name <- input$var_name
        v$dataset <- input$dataset
        v$dataset_year <- input$dataset_year
        v$go <- v$go + 1
    })
    
    observe({
        # disable download button if selection changes
        
        if(v$go == 0 || (v$geog_type != input$geog_type) || (v$fips != input$fips) || (v$vis_type != input$vis_type) ||
           (v$topic != input$topic) || (v$var_name != input$var_name) ||
           (v$dataset != input$dataset) || (v$dataset_year != input$dataset_year)) {
            disable("download")
        } else if(v$go > 0) {
            enable("download")  
        }
    })
    
    output$download <- downloadHandler(
        # download file as excel

        filename = function() {
            paste0(paste(input$dataset, input$topic, input$dataset_year, "by", input$geog_type, sep = '_'), ".xlsx")
        },
        content = function(file) {
            write.xlsx(main_table(), file)
        }
    )
    
    # output$ui_main_vis <- renderUI({
    #     if(input$go == 0) return()
    #     
    #     input$go
    #     
    #     if(isolate(input$output_type) != 2) {
    #         plotOutput('main_vis')
    #     } else if(isolate(input$output_type) == 2) {
    #         div('A Map',
    #             leafletOutput('main_map'))
    #         
    #     }
    # })
    
    # output$main_vis <- renderPlot({
    #     
    #     ggplot(mpg, aes(x = displ, y = cty, color = class)) +
    #         geom_point() +
    #         labs(title = 'A Graph')
    #     
    # })
    
    # output$main_map <- renderLeaflet({
    #     
    #     leaflet() %>%
    #         addTiles() %>% 
    #         addMarkers(lng=-122.33781311125064, lat=47.60469023724731)
    #     
    # })
    
    
    
}

