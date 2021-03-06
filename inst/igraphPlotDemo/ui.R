shiny::shinyUI(fluidPage(
  titlePanel("igraph plot demo"),
  fluidRow(
    column(3, wellPanel(
            selectInput(inputId="GraphType", label="Graph Type",
                        choices=c("Full Graph",
                                  "Empty Graph",
                                  "Star Graph",
                                  "Lattice Graph",
                                  "Ring Graph",
                                  "Tree Graph",
                                  "Erdos-Renyi",
                                  "Watts-Strogatz",
                                  "Adjacency Matrix",
                                  "Adjacency Matrix Excel",
                                  "Barabasi-Albert"), selected="Full Graph"),
                      uiOutput("GraphTypeUI")
            )
    ),
    column(9, wellPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot", wellPanel(
                    fluidRow(
                      column(12, wellPanel(
                        plotOutput("graphPlot", height=450)
                      ))
                    ),
                    fluidRow(
                      column(4, wellPanel(
                        radioButtons(inputId="PlotLayout", label="Plot Layout",
                                     choices=c("Auto"
                                               ,"Random"
                                               ,"Circle"
                                               ,"Sphere"
                                               ,"Fruchterman Reingold"
                                               ,"Kamada Kawai"
                                               ,"Drl"
                                               ,"Reingold Tilford"
                                               ,"Lgl"
                                               ,"Graphout"), selected="Auto")
                      )),
                      column(4, wellPanel(
                        checkboxInput(inputId = "showNodeName", label = "Show Vertex Label",  value = TRUE),
                        sliderInput(inputId = "vertexSize", label = "Vertex Size",  value = 15, min=1, max=100),
                        sliderInput(inputId = "arrowSize", label = "Arrow Size",  value = 0, min=0, max=20)
                      )),
                      column(4, wellPanel(
                        downloadButton('downloadPlot', 'Download Plot in pdf')
                      ))
                    )
                  )),
                  tabPanel("Adjacency Matrix", tableOutput("AdjMatrix")),
                  tabPanel("Centralities", tableOutput("Centralities"))
      )
    ))
  )
))
