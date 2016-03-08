## I still keep load these two package for stand alone shiny apps
library(shiny)
library(igraph)
library(readxl)

shinyServer(function(input, output) {
  output$GraphTypeUI <- renderUI({
    if (is.null(input$GraphType))
      return()
    switch(input$GraphType,
           "Full Graph" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                            checkboxInput(inputId="isDirected", label="directed", value=FALSE),
                            checkboxInput(inputId="isLoops", label="loops", value=FALSE)),
           "Empty Graph" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                             checkboxInput("isDirected", "directed", value=TRUE)),
           "Star Graph" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                            selectInput(inputId = "GraphMode",
                                        label = "Graph Mode",
                                        choices = c("undirected"="undirected"
                                                  ,"in"="in"
                                                  ,"out"="out"
                                                  ,"mutual"="mutual"),
                                        selected = "mutual")),
           "Lattice Graph" = wellPanel(numericInput("dimGraph", "Dimension of Lattice", value = 2),
                                       numericInput("lengthGraph", "Length of Lattice", value = 5),
                                       checkboxInput("isDirected", "directed", value=FALSE),
                                       checkboxInput("isMutual", "mutual", value=FALSE),
                                       checkboxInput("isCircular", "circular", value=TRUE)),
           "Ring Graph" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                            checkboxInput("isDirected", "directed", value=FALSE),
                            checkboxInput("isMutual", "mutual", value=FALSE),
                            checkboxInput("isCircular", "circular", value=TRUE)),
           "Tree Graph" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                                    numericInput("nChild", "Number of childern", value = 2, max=10),
                                    selectInput(inputId="GraphMode", label="Graph Mode", choices=c("undirected","in","out"), selected="undirected")),
           "Erdos-Renyi" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                                      numericInput("pNode", "Link probability", value = 0.3, max=1),
                                      checkboxInput("isDirected", "directed", value=FALSE),
                                      checkboxInput(inputId="isLoops", label="loops", value=FALSE)),
           "Watts-Strogatz" =  wellPanel(numericInput("dimNode", "Dim of the lattice", value = 1, max=2),
                                          numericInput("sizeNode", "Size in dimension", value = 50),
                                          numericInput("neiNode", "Neighborhood", value = 3),
                                          numericInput("pNode", "Wire probability", value = 0.05, max=1),
                                          checkboxInput("isMultiple", "multiple", value=FALSE),
                                          checkboxInput(inputId="isLoops", label="loops", value=FALSE)),
           "Barabasi-Albert" = wellPanel(numericInput("nNode", "Number of vertices", value = 10, max=100),
                                          numericInput("powerGraph", "Preferential attachment power", value = 1),
                                          checkboxInput("isDirected", "directed", value=FALSE)),
           "Adjacency Matrix" = wellPanel(fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('header', 'Header', TRUE),
                                          radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                                          radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"')),
           "Adjacency Matrix Excel" = wellPanel(fileInput('file2', 'Choose xlsx File', accept=c('.xlsx')),
                                          checkboxInput('header', 'Header', TRUE))
          )
  })

  realTimeGraph <- reactive({
    if (is.null(input$GraphType))
      return(graph.empty())
    # cat(paste0('creating ', input$GraphType,'\n'))
    if (input$GraphType == "Adjacency Matrix Excel" || input$GraphType == "Adjacency Matrix")
      print(input$file1$datapath)
    # Assign graph according graph type
    g <- switch(input$GraphType,
                "Full Graph" = graph.full(n=input$nNode, directed=input$isDirected, loops=input$isLoops),
                "Empty Graph" = graph.empty(n=input$nNode, directed=input$isDirected),
                "Star Graph" = make_star(n=input$nNode, mode=input$GraphMode),
                "Lattice Graph" = graph.lattice(length = input$lengthGraph, dim = input$dimGraph, directed=input$isDirected, mutual=input$isMutual, circular=input$isCircular),
                "Ring Graph" = graph.ring(n=input$nNode, directed=input$isDirected, mutual=input$isMutual, circular=input$isCircular),
                "Tree Graph" = make_tree(n=input$nNode, children=input$nChild, mode=input$GraphMode),
                "Erdos-Renyi" = erdos.renyi.game(n=input$nNode, p=input$pNode, directed=input$isDirected, loops=input$isLoops),
                "Watts-Strogatz" = watts.strogatz.game(dim=input$dimNode, size=input$sizeNode, nei=input$nei$Node, p=input$pNode, multiple=input$isMultiple, loops=input$isLoops),
                "Barabasi-Albert" = barabasi.game(n=input$nNode, power=input$powerGraph, directed=input$isDirected),
                "Adjacency Matrix" = graph_from_adjacency_matrix(as.matrix(read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)), weighted=TRUE),
                "Adjacency Matrix Excel" = {inFile <- input$file2;
                                            if(is.null(inFile))
                                              return(NULL)
                                            file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
                                            graph_from_adjacency_matrix(as.matrix(readxl::read_excel(paste0(inFile$datapath, ".xlsx"), col_names = input$header)), weighted=TRUE)}
                )
    # cat(paste0('returning ', input$GraphType,'\n'))
    return(g)
  })

  plotGraph <- function(){
    g <- realTimeGraph()
    # Adjust graph layout
    plotlayout <- switch(input$PlotLayout,
                      "Auto"=layout.auto(g),
                      "Random"=layout.random(g),
                      "Circle"=layout.circle(g),
                      "Sphere"=layout.sphere(g),
                      "Fruchterman Reingold"=layout.fruchterman.reingold(g),
                      "Kamada Kawai"=layout.kamada.kawai(g),
                      "Drl"=layout.drl(g),
                      "Reingold Tilford"=layout.reingold.tilford(g),
                      "Lgl"=layout.lgl(g),
                      "Graphopt"=layout.graphopt(g)
                      )
    # Show node name or not
    if(!input$showNodeName){
      V(g)$label = ""
    }

    # Adjust vertex size according user input
    V(g)$size = input$vertexSize

    # Adjust arrow size according user input
    E(g)$arrow.size = input$arrowSize/10

    # Adjust edge width according weight (if the graph is weighted)
    if( is.weighted(g)){
      E(g)$width = 5 * E(g)$weight/max(E(g)$weight)
    }

    # To avoid plot without boundary error
    if(vcount(g) > 0){
      plot(g, layout=plotlayout)
    }
  }

  output$graphPlot <- renderPlot({
    # plotGraph()
    suppressWarnings(plotGraph())
  })

  ## Download pdf
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('NetworkGraph',format(Sys.time(),"%Y%m%d_%H%M%S"),'.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      print(suppressWarnings(plotGraph()))
      dev.off()
    }
  )

  ## Output of Adjacency Matrix Panel
  calculateAdjMatrix <- function(){
    g=realTimeGraph()
    if(is.weighted(g)){
      adjmat <- as(as_adjacency_matrix(g, attr='weight'),"matrix")
    }else{
      adjmat <- as(as_adjacency_matrix(g),"matrix")
    }
    return(adjmat)
  }
  output$AdjMatrix <- renderTable(calculateAdjMatrix())

  # output$AdjMatrix <- renderTable(as(as_adjacency_matrix(realTimeGraph(), attr='weight'),"matrix"))

  ## Output of Centrality Panel
  calculateCentrality <- function(){
    Centralities <- list()
    Centralities$Alpha <- as(alpha.centrality(realTimeGraph()),"matrix")
    Centralities$Bon <- as(bonpow(realTimeGraph()),"matrix")
    Centralities$Closeness <- as(closeness(realTimeGraph()),"matrix")
    Centralities$Evcent <- as(evcent(realTimeGraph())$vector,"matrix")
    Centralities$Kleinberg <- as(authority.score(realTimeGraph())$vector,"matrix")
    Centralities$PageRank <- as(page.rank(realTimeGraph())$vector,"matrix")
    Centralities$Betweenness <- as(betweenness(realTimeGraph()),"matrix")
    return(as.data.frame(Centralities))
  }
  output$Centralities <- renderTable(as(calculateCentrality(),"matrix"))
})
