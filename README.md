## Synopsis

This project, igraphinshiny, is a Shiny apps to demonstrate the `plot` function in the igraph package.

## Motivation

When I first learned graph theory (date back to 2005), I always looked at the adjacency matrix to understand the graph because I didn't know how to draw different kind of graph fast. I was always wondering how to visualize the graph easily so I can understand how the adjacency matrix really looks.

Now, I am working in econometric modelling support team in International Monetary Fund, I often get requests from economists (who don't have experience with R or graph theory or both of them) about how to visualize their directed trading data (which equivallent to he adjacency matrix in graph theory). 

I love to use R, so I always tell them you can use the `igraph` package in R so you can visualize your network chart easily. However, for users who are familiar with R but no backgroud with graph theory, they will find out that these graph theory jargon in igraph manual is hard to understand. For users who are familiar with graph theory but no background with R, you will find that it's impossible to make you understand how to use igraph in short period of time. For users who are not familiar with both R and graph theory, they just give up. At the end of day, I just create network charts for them. 

This is why I create this shiny app to demonstrate the plot function in igraph. I pre configure several common graph structure so users can easily play with.

The goal of `igraphinshiny` is to help people without knowledge of graph theory or R can easily learn graph theory.

[This app is also hosted on shinyapps.io](https://mlee2.shinyapps.io/igraph/)

## Installation
`devtools::install_github('mingjerli/igraphinshiny')`

## Loading the package
`library(igraphinshiny)`

## How to use
`plotDemo()`

## Contributors

Ming-Jer Lee (mingjerli@gmail.com )

## License

GPL (>=2)
