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
      return(igraph::make_empty_graph())
    if (input$GraphType == "Adjacency Matrix Excel" || input$GraphType == "Adjacency Matrix")
      print(input$file1$datapath)
    # Assign graph according graph type
    g <- switch(input$GraphType,
                "Full Graph" = igraph::make_full_graph(n=input$nNode, directed=input$isDirected, loops=input$isLoops),
                "Empty Graph" = igraph::make_empty_graph(n=input$nNode, directed=input$isDirected),
                "Star Graph" = igraph::make_star(n=input$nNode, mode=input$GraphMode),
                "Lattice Graph" = igraph::make_lattice(length = input$lengthGraph, dim = input$dimGraph, directed=input$isDirected, mutual=input$isMutual, circular=input$isCircular),
                "Ring Graph" = igraph::make_ring(n=input$nNode, directed=input$isDirected, mutual=input$isMutual, circular=input$isCircular),
                "Tree Graph" = igraph::make_tree(n=input$nNode, children=input$nChild, mode=input$GraphMode),
                "Erdos-Renyi" = igraph::sample_gnp(n=input$nNode, p=input$pNode, directed=input$isDirected, loops=input$isLoops),
                "Watts-Strogatz" = igraph::sample_smallworld(dim=input$dimNode, size=input$sizeNode, nei=input$neiNode, p=input$pNode, multiple=input$isMultiple, loops=input$isLoops),
                "Barabasi-Albert" = igraph::sample_pa(n=input$nNode, power=input$powerGraph, directed=input$isDirected),
                "Adjacency Matrix" = igraph::graph_from_adjacency_matrix(as.matrix(read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote)), weighted=TRUE),
                "Adjacency Matrix Excel" = {inFile <- input$file2;
                                            if(is.null(inFile))
                                              return(NULL)
                                            file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
                                            igraph::graph_from_adjacency_matrix(as.matrix(readxl::read_excel(paste0(inFile$datapath, ".xlsx"), col_names = input$header)), weighted=TRUE)}
                )
    return(g)
  })

  plotGraph <- function(){
    g <- realTimeGraph()
    # Adjust graph layout
    plotlayout <- switch(input$PlotLayout,
                      "Auto"=igraph::layout_nicely(g),
                      "Random"=igraph::layout_randomly(g),
                      "Circle"=igraph::layout_in_circle(g),
                      "Sphere"=igraph::layout_on_sphere(g),
                      "Fruchterman Reingold"=igraph::layout_with_fr(g),
                      "Kamada Kawai"=igraph::layout_with_kk(g),
                      "Drl"=igraph::layout_with_drl(g),
                      "Reingold Tilford"=igraph::layout_as_tree(g),
                      "Lgl"=igraph::layout_with_lgl(g),
                      "Graphopt"=igraph::layout_with_graphopt(g)
                      )
    # Show node name or not
    if(!input$showNodeName){
      igraph::V(g)$label = ""
    }

    # Adjust vertex size according user input
    igraph::V(g)$size = input$vertexSize

    # Adjust arrow size according user input
    igraph::E(g)$arrow.size = input$arrowSize/10

    # Adjust edge width according weight (if the graph is weighted)
    if(igraph::is_weighted(g)){
      igraph::E(g)$width = 5 * igraph::E(g)$weight/max(igraph::E(g)$weight)
    }

    # To avoid plot without boundary error
    if(igraph::vcount(g) > 0){
      plot(g, layout=plotlayout)
    }
  }

  output$graphPlot <- renderPlot({
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
    if(igraph::is_weighted(g)){
      adjmat <- as(igraph::as_adjacency_matrix(g, attr='weight'),"matrix")
    }else{
      adjmat <- as(igraph::as_adjacency_matrix(g),"matrix")
    }
    return(adjmat)
  }
  output$AdjMatrix <- renderTable(calculateAdjMatrix())

  ## Output of Centrality Panel
  calculateCentrality <- function(){
    Centralities <- list()
    Centralities$Alpha <- as(igraph::alpha_centrality(realTimeGraph()),"matrix")
    Centralities$Power <- as(igraph::power_centrality(realTimeGraph()),"matrix")
    Centralities$Closeness <- as(igraph::closeness(realTimeGraph()),"matrix")
    Centralities$Eigenvector <- as(igraph::eigen_centrality(realTimeGraph())$vector,"matrix")
    Centralities$Authority <- as(igraph::authority_score(realTimeGraph())$vector,"matrix")
    Centralities$PageRank <- as(igraph::page_rank(realTimeGraph())$vector,"matrix")
    Centralities$Betweenness <- as(igraph::betweenness(realTimeGraph()),"matrix")
    return(as.data.frame(Centralities))
  }
  output$Centralities <- renderTable(as(calculateCentrality(),"matrix"))
})
