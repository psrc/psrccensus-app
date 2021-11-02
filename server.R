server <- function(input, output, session) {


# main controls -----------------------------------------------------------
    
    updateSelectizeInput(
        session, 
        'topic', 
        server = TRUE,
        choices = vars
    )
    
    output$ui_fips <- renderUI({
        if(input$geog_type == 'place') {
            selectInput("fips",
                        'Select Places',
                        choices = places,
                        multiple = TRUE)
        } else if(input$geog_type == 'msa') {
            selectInput("fips",
                        'Select MSAs',
                        choices = msas,
                        multiple = TRUE)
        }
    })
    
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
        
        # Visual Type radio buttons
        if(input$var_name != 'all' & input$geog_type == 'tract') {
            updateRadioButtons(session, 
                               'vis_type', 
                               label = 'Visual', 
                               choices = c('Map' = 'map'),
                               selected = "map")
        } else if(input$var_name == 'all') {
            disable("vis_type")
        } else if(input$trend == TRUE) {
            updateRadioButtons(session,
                               'vis_type',
                               label = 'Visual',
                               choices = c('Graph' = 'graph'),
                               selected = "graph")
        } else {
            updateRadioButtons(session, 
                               'vis_type', 
                               label = 'Visual', 
                               choices = c('Graph' = 'graph', 'Map' = 'map'),
                               selected = "graph")
        }
        
        # Trend checkbox
        if(input$geog_type == 'tract' || input$var_name == 'all') {
            # update checkbox first then disable
            updateCheckboxInput(
                session = session,
                'trend',
                label = 'Trend',
                value = FALSE
            )
            disable('trend')
        } else {
            enable('trend')
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
        
        if(input$trend == TRUE) {
            selectInput('dataset_year',
                        'Year',
                        choices = dataset_year(),
                        multiple = TRUE,
                        selected = dataset_year(),
                        width = '20rem')
        } else {
            selectInput('dataset_year',
                        'Year',
                        choices = dataset_year(),
                        width = '20rem')
        }
        
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
        
        col_names <- c('GEOID', 'name')
        
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
            
            recs <- recs %>%
                select({{col_names}}, label, everything())
            
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
            recs <- recs %>%
                select(str_to_upper(col_names), label, everything())
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
    
    show_columns <- eventReactive(input$go, {
        # return a vector of column names to show in DT
        hide_cols <- c('variable', 'concept', 'census_geography', 'acs_type', 'year', 'state')
        df <- main_table()
        
        if(input$dataset != 'Decennial' & input$trend == FALSE) {
            target <- which(colnames(df) %in% hide_cols)
        } else if(input$dataset != 'Decennial' & input$trend == TRUE) {
            hide_cols <- hide_cols[!(hide_cols %in% c('year'))]
            target <- which(colnames(df) %in% hide_cols)
        } else {
            target <- NULL
        }
        
        return(target)
    })
    
    ### map ----
    
    map_feature <- eventReactive(input$go, {
        if(input$geog_type == 'tract') {
            withProgress(feature <- st_read(gdb.nm, tract.layer.name, crs = spn),
                        message = 'Reading in tract feature') 
        }
        return(feature)
    })
    
    map <- eventReactive(input$go, {
        df <- main_table()
        if(input$geog_type == 'tract') {
            withProgress(map_out <- create_tract_map(tract.tbl = df,
                             tract.lyr = map_feature()),
                         message = 'Generating map')
        }
        return(map_out)
    })
    
    ### graph ----
    
    graph <- eventReactive(input$go, {
        
        df <- main_table()
        
        if(input$trend == TRUE & input$vis_type == 'graph') {
            # timeseries graph
            p <- get_time_series(df, input$var_name)

        } else if(input$trend == FALSE & input$vis_type == 'graph') {
            # generic graph
            if(input$dataset != 'Decennial') {
                x_val <- 'name'
                y_val <- 'estimate'
            } else {
                x_val <- 'NAME'
                y_val <- 'value'
            }
            
            if('Region' %in% unique(df[[x_val]])) {
                # ensure 'Region' element is last item in graph
                counties <- c('King County', 'Kitsap County', 'Pierce County', 'Snohomish County', 'Region')
                if(input$dataset == 'Decennial') {
                    counties <- c(paste0(counties[1:4], ", Washington"), 'Region')
                }
                df[[x_val]] <- factor(df[[x_val]], levels = counties)
            } 
           
            p <- ggplot(df, aes_string(x = x_val, y = y_val, fill = x_val)) +
                geom_col() +
                scale_y_continuous(labels = label_comma()) +
                labs(x = NULL,
                     y = str_to_title(y_val),
                     title = names(vars[which(vars == input$topic)]),
                     subtitle = str_replace_all(unique(df$label), '!!', ' > '),
                     source = paste(input$dataset, ", ", input$dataset_year)
                     ) +
                theme(legend.title = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 0.5)) 
            
            if(input$dataset != 'Decennial') {
                # add errorbar where MOE is available (ACS datasets)
                p <- p +
                    geom_errorbar(aes(ymin = estimate + moe, ymax = estimate - moe),
                                  alpha = .5,
                                  width = 0.2,)
            }
        }

        return(p)
    })
    
    output$main_vis <- renderPlot({

        graph()
    })
    
    ## render table visual ----
    
    output$main_tbl <- renderDT({
        if(input$go == 0) return()
        input$go
        
        withProgress(df <- main_table(),
                     detail = 'This may take a while...')
        
        datatable(df,
                  options = list(columnDefs = list(list(visible = FALSE, targets = show_columns()))))
        })
    
    output$ui_main_vis <- renderUI({
        if(input$go == 0) return()
        
        input$go
        
        if(isolate(input$vis_type) == 'map') {
            leafletOutput('main_map')
        } else if(isolate(input$vis_type) != 'map') {
            plotOutput('main_vis')
        }
        
    })
    
    ## render map visual ----
    
    output$main_map <- renderLeaflet({
        map()
    })
    

    

# download data  ----------------------------------------------------------
    
    
    ## enable/disable download button ----
    v <- reactiveValues(geog_type = NULL,
                        # fips = NULL,
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
        # v$fips <- input$fips
        v$vis_type <- input$vis_type
        v$topic <- input$topic
        v$var_name <- input$var_name
        v$dataset <- input$dataset
        v$dataset_year <- input$dataset_year
        v$go <- v$go + 1
    })
    
    observe({
        # disable download button if selection changes
        if(v$go == 0 || (v$geog_type != input$geog_type) || (v$vis_type != input$vis_type) ||
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
    
}

