ui  <- fluidPage(
    # bslib previewer doesn't work!
    # theme = bs_theme(primary = "#FF00F3", 
    #                  heading_font = font_google("Pacifico"), 
    #                  spacer = "0rem"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    navbarPage(
        title = 'PSRC Census Tool',
        
        # tabs --------------------------------------------------------------------
        main,
        about
        
    )#,
    # tableOutput('var_table')
    
) # end fluidPage

