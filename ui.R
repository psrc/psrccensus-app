ui  <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "js-script.js"),
        tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Lato&family=Quicksand&family=Roboto&display=swap")
        ),
    
    navbarPage(
        title = 'PSRC Census Explorer',
        
        # tabs --------------------------------------------------------------------
        main,
        about
        
    )
) # end fluidPage

