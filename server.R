server <- function(input, output, session) {


# main controls -----------------------------------------------------------
    
    
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
        } else if(input$trend == TRUE | input$geog_type != 'tract') {
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
    
    output$ui_ungroup_vars <- renderUI({
        if(is.null(input$table)) return(NULL)

        # display checkbox to un-group variables if topic is found in variables_groupings.csv
        if(input$table %in% unique(var_group$table_code)) {
            checkboxInput('var_ungroup',
                          'Ungroup Variables',
                          width = '75%')
        } else {
            NULL
        }

    })

    output$ui_var_group_option <- renderUI({
        if(is.null(input$table)) return(NULL)

        # display if topic is found in variables_groupings.csv
        if(input$table %in% unique(var_group$table_code)) {
            t <- var_group %>%
                filter(table_code == input$table)
            group_options <- unique(t$group_name)
            selectInput('var_group_option',
                        'Grouping Option',
                        choices = group_options,
                        width = '25rem')
        } else {
            NULL
        }

    })
    
    output$ui_table <- renderUI({
        if(is.null(input$topic)) return(NULL)
        
        selectInput('table',
                    'Table',
                    choices = table_names())
    })
    
    output$ui_var_name <- renderUI({
        if(is.null(input$table)) return(NULL)
        if(is.null(input$var_ungroup)) return(NULL)
        
        vars.group <- unique(var_group$table_code)
        
        if(input$table %in% vars.group & input$var_ungroup == TRUE) {
            selectInput('var_name',
                        'Variable',
                        choices = c('All Variables' = 'all', var_names())
            )
        } else if (input$table %in% vars.group & input$var_ungroup == FALSE) {
            t <- var_group %>%
                filter(.data$table_code == input$table & .data$group_name == input$var_group_option)
            
            vars <- unique(t$grouping)
            names(vars) <- vars
            
            selectInput('var_name',
                        'Variable',
                        choices = c('All Variables' = 'all', vars)
            )
        } else {
            selectInput('var_name',
                        'Variable',
                        choices = c('All Variables' = 'all', var_names()),
                        width = '25rem')
        }
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
    
    output$ui_main_vis <- renderUI({
        if(input$go == 0) return()
        
        input$go
        
        if(isolate(input$vis_type) == 'map') {
            leafletOutput('main_map')
        } else if(isolate(input$vis_type) != 'map') {
            plotOutput('main_vis')
        }
        
    })
    

    ## main control reactives ----

    
    table_names <- reactive({
        # populate table dropdown
        if(is.null(input$topic)) return(NULL)
        
        df <- topic.df %>% 
            filter(tags == input$topic) 

        tables <- unique(df$table_code)
        
        return(vars[vars %in% tables])
    })
    
    var_names <- reactive({
        # populate initial variable dropdown
        # see observe({}) for updating var_names with grouped variables
        if(is.null(input$table)) return(NULL)
        
        t <- var.df %>%
            filter(.data$census_table_code == input$table) %>%
            select(.data$variable_description, .data$name) %>%
            distinct()

        vars <- t$name
        names(vars) <- t$variable_description
        return(vars)
    })
    
    dataset <- reactive({
        # populate dataset dropdown
        if(is.null(input$var_name)) return(NULL)
     
        t <- var.df %>% 
            filter(.data$census_table_code == input$table)
        
        sort(unique(t$census_product))
    })
    
    
    dataset_year <- reactive({
        # populate year dropdown
        if(is.null(input$dataset)) return(NULL)

        t <- var.df %>% 
            filter(.data$census_table_code == input$table & .data$census_product == input$dataset) 
        
        sort(unique(t$census_year))
    })
    

    ## reactives ----
    
    ### table ----
    
    table_universe <- eventReactive(input$go, {
        df <- topic.df %>% 
            filter(table_code == input$table) %>% 
            mutate(title = str_replace_all(title, 'IN\\s\\d{4}\\s', "")) %>% 
            distinct(title, universe)
        
        return(list(title = str_to_title(df$title), universe = df$universe))
    })
    
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
                                     table.names = input$table,
                                     years = as.numeric(input$dataset_year),
                                     acs.type = str_to_lower(input$dataset)) %>% 
                    filter(GEOID %in% fips)
            } else {
                recs <- get_acs_recs(geography = input$geog_type,
                                     table.names = input$table,
                                     years = as.numeric(input$dataset_year),
                                     FIPS = fips,
                                     acs.type = str_to_lower(input$dataset))
            }
            
            recs <- recs %>%
                select({{col_names}}, .data$label, everything())
            
        } else if(input$dataset == 'Decennial') {
            incProgress(message = 'Gathering Decennial Census data')
            
            # find the padded table code for Decennial tables 
            dec_tbl_code <- var.df.dist %>% 
                filter(.data$census_table_code == input$table) %>% 
                pull(.data$census_table_code_pad)
            
            recs <- get_decennial_recs(geography = input$geog_type,
                                       table_codes = dec_tbl_code,
                                       years = as.numeric(input$dataset_year),
                                       fips = fips)
            recs <- recs %>%
                select(str_to_upper(col_names), .data$label, everything())
        }
        
        incProgress(amount = .5, message = 'Data gathered')
        
        #### table variables grouped ----
        if(input$table %in% unique(var_group$table_code) & input$var_ungroup == FALSE) {
            recs <- group_recs(recs, input$var_group_option) # removed variable, GEOID, label column
            recs <- recs %>% 
                mutate(across(where(is.factor), as.character)) %>% 
                mutate(group_chr = as.character(grouping))
            # if(input$table == 'B01001') {
            #     recs$grouping <- as.character(recs$grouping)
            #     recs$grouping_fac <- recs$grouping
            #     
            #     age_groups <- c('Total', 'Total Female', 'Total Male', '0 to 4 years', '5 to 17 years', '18 to 64 years',
            #                     '65 to 84 years', '85 years and over')
            #     
            #     recs$grouping_fac <- factor(recs$grouping_fac, levels = age_groups)
            #     recs <- recs %>% arrange(name, grouping_fac)
            # }
        }
        
        #### filter for variable ----
        if(input$var_name != 'all' & !(input$table %in% unique(var_group$table_code)) |
           (input$var_name != 'all' & input$var_ungroup == TRUE)) {
            recs <- recs %>%
                filter(.data$variable == input$var_name)
        } else if(input$var_name != 'all' & (input$table %in% unique(var_group$table_code)& input$var_ungroup == FALSE)) {
            recs <- recs %>%
                filter(.data$group_chr == input$var_name)
            print(recs)
                # filter(.data$grouping == input$var_name)
        }
        
        incProgress(amount = .4, message = 'Ready to render data')
        
        return(recs)
    })
    
    hide_columns <- eventReactive(input$go, {
        # return a vector of column names to hide in DT
        hide_cols <- c('variable', 'concept', 'census_geography', 'acs_type', 'year', 'state', 'group_chr')
        df <- main_table()
        
        if(input$dataset != 'Decennial' & input$trend == FALSE) {
            target <- which(colnames(df) %in% hide_cols)
        } else if(input$dataset != 'Decennial' & input$trend == TRUE) {
            hide_cols <- hide_cols[!(hide_cols %in% c('year'))]
            target <- which(colnames(df) %in% hide_cols)
        } else if(input$dataset == 'Decennial') {
            target <- which(colnames(df) %in% hide_cols)
        }
        
        return(target)
    })
    
    ### map ----
    
    map_feature <- eventReactive(input$go, {
        
        layer <- switch(input$geog_type,
                        'tract' = "tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson")
        
        withProgress(feature <- st_read(paste0(arc.root, layer)), message = 'Reading in spatial feature') 

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
        
        if(input$trend == TRUE & input$vis_type == 'graph' & input$var_name != 'all') {
            # timeseries graph
            p <- get_time_series(df, input$var_name)

        } else if(input$trend == FALSE & input$vis_type == 'graph' & input$var_name != 'all') {
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
           
            # subtitle for grouped/ungrouped data ---- 
            if('label' %in% colnames(df)) {
                plot_subtitle <- str_replace_all(unique(df$label), '!!', ' > ')
            } else {
                plot_subtitle <- unique(df$grouping)
            }
            
            geog_choices = c('Counties & Region' = 'county',
                             'Tract' = 'tract',
                             'Metropolitan Statistical Area (MSA)' = 'msa',
                             'Place' = 'place')
            
            p <- ggplot(df, aes_string(x = x_val, y = y_val, fill = x_val)) +
                geom_col() +
                scale_y_continuous(labels = label_comma()) +
                labs(x = names(geog_choices[which(geog_choices == input$geog_type)]),
                     y = str_to_title(y_val),
                     title = names(vars[which(vars == input$table)]),
                     subtitle = plot_subtitle, 
                     caption = paste0('Source: ', input$dataset, ", ", input$dataset_year)
                     ) +
                theme(legend.title = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank()
                      ) 
            
            if(input$dataset != 'Decennial') {
                # add errorbar where MOE is available (ACS datasets)
                p <- p +
                    geom_errorbar(aes(ymin = estimate + moe, ymax = estimate - moe),
                                  alpha = .5,
                                  width = 0.2)
            }
        }

        return(p)
    })
    
    ## render plot visual ----
    
    output$main_vis <- renderPlot({
        graph()
    })
    
    ## render table visual ----
    
    output$main_tbl <- renderDT({
        if(input$go == 0) return()
        input$go
        
        withProgress(df <- main_table(),
                     detail = 'This may take a while...')
        
        # clean column names
        col_names <- c()
        for(i in 1:length(colnames(df))) {
            if(colnames(df)[i] %in% c('GEOID', 'moe')) {
                e <- str_to_upper(colnames(df)[i])
            } else if(colnames(df)[i] %in% hide_columns()){
                e <- colnames(df)[i]
            } else {
                e <- str_replace_all(colnames(df)[i], "_", " ")
                e <- str_to_title(e)
            }
            ifelse(is.null(col_names), col_names <- e, col_names <- c(col_names, e))
        }
        
        tbl.u <- table_universe()

        datatable(df,
                  caption = HTML(paste0('Table: ', isolate(input$table), ' ', tbl.u$title, '<br/> Universe: ', tbl.u$universe)),
                  colnames = col_names,
                  options = list(columnDefs = list(list(visible = FALSE, targets = hide_columns()))),
                  extensions = 'Responsive')
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
                        table = NULL,
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
        v$table <- input$table
        v$var_name <- input$var_name
        v$dataset <- input$dataset
        v$dataset_year <- input$dataset_year
        v$go <- v$go + 1
    })
    
    observe({
        # disable download button if selection changes
        if(v$go == 0 || (v$geog_type != input$geog_type) || (v$vis_type != input$vis_type) ||
           (v$table != input$table) || (v$var_name != input$var_name) ||
           (v$dataset != input$dataset) || (v$dataset_year != input$dataset_year)) {
            disable("download")
        } else if(v$go > 0) {
            enable("download")
        }

    })
    
    output$download <- downloadHandler(
        # download file as excel

        filename = function() {
            paste0(paste(input$dataset, input$table, input$dataset_year, "by", input$geog_type, sep = '_'), ".xlsx")
        },
        content = function(file) {
            write.xlsx(main_table(), file)
        }
    )
    
}

