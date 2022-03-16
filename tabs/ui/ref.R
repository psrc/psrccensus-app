ref.topics.table <- fluidRow(column(10, DTOutput('table_opts'), offset = 1))

ref <- tabPanel('Reference',
                includeMarkdown('md/ref.md'),
                ref.topics.table)


