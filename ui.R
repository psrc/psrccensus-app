ui  <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "js-script.js")
        ),
    navbarPage(
        title = 'PSRC Census Tool',
        
        # tabs --------------------------------------------------------------------
        main,
        about
        
    )
) # end fluidPage

