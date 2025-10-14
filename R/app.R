#' browse the data on Bioc usage of OSN
#' @import shiny
#' @export
browse_osndat = function() {
 tab = gettab()
 p = process_tab(tab)
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("OSN usage as of 12 Oct 2025"),
    selectInput("topic", "topic", p$names),
    numericInput("maxrec", "maxNrec", 500, min=100, max=50000, step=1000)
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("data", verbatimTextOutput("sz"), DT::dataTableOutput("basic")),
     tabPanel("summaries", DT::dataTableOutput("probed")),
     tabPanel("about", helpText("The data are output of rclone lsl --max-depth 5 applied to the Bioconductor OSN bucket"))
     )
    )
   )
  )
server = function(input, output) {
  output$sz = renderText({
   sprintf("number of elements: %d", nrow(p$split[[input$topic]]))
  })
  output$basic = DT::renderDataTable({
   head(as.data.frame(p$split[[input$topic]]),input$maxrec)
  })
  output$probed = DT::renderDataTable({
   input$maxrec
   suppressWarnings(probe(p, input$topic))
  })
 }
 runApp(list(ui=ui, server=server))
}
