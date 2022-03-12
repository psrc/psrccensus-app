ui  <- fluidPage(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src = "js-script.js"),
        tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Lato&family=Quicksand&family=Roboto&display=swap"),
        tags$style(type = "text/css", "#main_map {height: calc(100vh - 320px) !important;}")
        ),
    
    navbarPage(
        title = 'PSRC Census Explorer',
        
        # tabs --------------------------------------------------------------------
        main,
        ref,
        about
        
    )
) # end fluidPage

