ref.topics.table <- fluidRow(column(12, DTOutput('table_opts')))

ref <- tabPanel('Reference',
                includeMarkdown('md/ref.md'),
                ref.topics.table)


