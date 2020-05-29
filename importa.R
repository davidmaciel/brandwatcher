read_bw <- function(file, 
                       cols = c("Url", 
                                "Twitter Retweet of", "Sentiment")){
  assertive::assert_is_character(file, "stop")
  assertive::assert_is_character(cols, "stop")
  data.table::fread(
    file,
    sep = ",",
    skip = 6,
    header = T,
    encoding = "UTF-8",
    select = cols)
}

extrai_nos <- function(str){
  
  pat <- "http://twitter.com/(.+)/statuses"
  stringr::str_match(str, pat)[,2]
}

faz_rede <- function(dt, source, target, simplify = T) {
  assertive::assert_is_data.table(dt)
  dt<-dt[, .("source" = extrai_nos(get(source)),
             "target" = extrai_nos(get(target)),
             Sentiment)][!is.na(source) &
                           !is.na(target)][, weight := ifelse(Sentiment == "positive",
                                                              1,
                                                              ifelse(Sentiment == "negative",-1, 0))][, .(weight = sum(weight)), by = .(source, target)]
  
  if(simplify == T){
    grafo <- igraph::graph_from_data_frame(dt, directed = T, 
                                           vertices = unique(c(
                                             unique(dt$source), unique(dt$target)))) 
    
    igraph::simplify(grafo, edge.attr.comb = "ignore")
  } else{
    igraph::graph_from_data_frame(dt, directed = T, 
                                  vertices = unique(c(
                                    unique(dt$source), unique(dt$target))))
  }
  
}

per_99 <-function(grafo){
  V(grafo)$grau <- igraph::degree(grafo, mode = "total")
  ind <- quantile(V(grafo)$grau, probs = 0.99)
  print(ind)
  V(grafo)$name[grau > ind]
}

graph_summary <- function(grafo){
  size <- vcount(grafo)
  dens <- graph.density(grafo)
  cluster_avg <- mean(transitivity(grafo, type = "local"), na.rm = T)
  mean_dist <- mean_distance(grafo)
  diameter <- diameter(grafo)
  round(c("size" = size,
          "density" = dens,
          "cluster_coef" = cluster_avg,
          "mean_dist" = mean_dist,
          "diameter" = diameter),4)
}


two_step_reach <- function(grafo, v){
  size <- igraph::vcount(grafo)
  two <- igraph::ego_size(grafo, order = 2, nodes = v)
  c("2step" = (two-1), "prop" = (two-1)/size) %>% 
    round(5)
}


two_step_effic <- function(grafo, v){
  one <- igraph::ego_size(grafo, order = 1, nodes = v) 
  two <- igraph::ego_size(grafo, order = 2, nodes = v) - one
  one <- one - 1
  two/one
}

cluster_coef <- function(grafo, v, undirected = F){
  ego_neig <- igraph::make_ego_graph(grafo, 
                                     order = 1,
                                     nodes = v)
  ego_neig <- ego_neig[[1]]
  ego_neig <- igraph::delete.vertices(ego_neig, v = v) 
  if(undirected == T) {
    ego_neig <- as.undirected(ego_neig)
  }
  graph.density(grafo)
}