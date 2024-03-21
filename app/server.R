# Import packages
library(shiny)
library(igraph)

# Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
load("heros.Rdata")
load("paired_matching_scores.Rdata")
nodes = hero
edges = paired_matching_scores[, c(1,2,4)]
colnames(edges) = c("from", "to", "score")
edges$from = as.vector(edges$from)
edges$to = as.vector(edges$to)

# Define server logic required
shinyServer(function(input, output) {
  
    # Matching Hero Graph
    output$egonetwork = renderPlot({
        
        max = input$Maximum
        Hero_edges = edges[(edges$from==input$Hero| edges$to==input$Hero),]
        if(nrow(Hero_edges)==0){
            plot(1, 1, axes=F, type = "n", xlab = "", ylab = "",
                 main = "This Hero does not have matching record.")
        }else{
            Hero_edges = Hero_edges[order(Hero_edges$score, decreasing = T),]
            if(nrow(Hero_edges)>=max){
                Hero_edges = Hero_edges[c(1:max),]
            }
            igraph_hero = graph.data.frame(d = Hero_edges, directed = F)
            V(igraph_hero)$color = "gold"
            V(igraph_hero)$color[(V(igraph_hero)$name==input$Hero)] = "red"
            weight = Hero_edges$score
            weight = weight - median(weight)
            weight = exp(weight)/(1+exp(weight))
            weight = weight-min(weight)
            weight = weight/max(weight)
            
            plot(igraph_hero, 
                 vertex.color = V(igraph_hero)$color,
                 vertex.size = 40,
                 vertex.pch = 2,
                 vertex.frame.color = NA,
                 vertex.label.color = "black",
                 vertex.label.dist = 0,
                 vertex.label.cex = 1.2,
                 vertex.label = V(igraph_hero)$name,
                 edge.color = rgb(1-weight/2-0.1,
                                  1-weight/2-0.1,
                                  1-weight/2-0.1),
                 edge.width = (weight+0.2)*10)
            legend("bottomleft",legend = "Note: \ndarker and wider edge indicates \nrelatively higher matching score", bty = "n", horiz = T)
        }

    })
    
    # Matching Hero Rank
    output$Rank = renderDataTable({
        
        Hero_edges = edges[(edges$from==input$Hero| edges$to==input$Hero),]
        if(nrow(Hero_edges)==0){
            df = data.frame(Rank = "This Hero does not have matching record.",
                            Hero = NA,
                            Score = NA)
            df
        }else{
            Hero_edges = Hero_edges[order(Hero_edges$score, decreasing = T),]
            matching_hero = Hero_edges$from
            matching_hero[which(matching_hero==input$Hero)] = Hero_edges$to[which(matching_hero==input$Hero)]
            df = data.frame(Rank = c(1:length(matching_hero)),
                            Hero = matching_hero,
                            Score = Hero_edges$score)
            
            df  
        }
        

    })
})
