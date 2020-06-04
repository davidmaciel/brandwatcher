library(data.table)
library(tidyverse)
library(igraph)

read_bw <- function(file, cols){
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

nodes_retweet <- function(file, source = "Url",
                          attr = c("Twitter Followers",
                                         "Twitter Following",
                                         "Twitter Tweets")){

  dt <- read_bw(file, c(source, attr))
  dt <- dt[,name := extrai_nos(get(source))][
    ,!source, with = FALSE][
      ,c("name", attr), with = F] %>%
  unique()
  if(all(attr == c("Twitter Followers",
               "Twitter Following",
               "Twitter Tweets")) ){
    dt <- summarise_nodes(dt)
  }
  else{
    warning("Sumarisar atributos antes de criar grafo")
  }
  dt

}

edges_retweet <- function(file, source = "Twitter Retweet of",
                          target = "Url", attr = "Sentiment"){
  vars <- c(source, target, attr)
  edges <- read_bw(file, c(source, target, attr))
  edges[,
        `:=`(from = extrai_nos(get(source)),
             to = extrai_nos(get(target)))][
               !is.na(from) & !is.na(to),
               c("from","to",attr), with = F] %>%
  unique()

}

summarise_nodes <- function(nodes){
  nodes[,.(
    "followers" = max(`Twitter Followers`),
    "following" = max(`Twitter Following`),
    "tweets" = sum(`Twitter Tweets`)), by = name]
}

add_edge_nodes <- function(nodes,edges){
  names <- data.table(name = unique(c(edges$from, edges$to )))
  setkey(nodes, name)
  setkey(names, name)
  left_join(nodes, names, by = "name")
  merge(nodes, names, all = T)
}

faz_rede <- function(nodes, edges){
  igraph::graph_from_data_frame(edges, vertices = nodes) %>%
    igraph::simplify(remove.multiple = F)
}

per_99 <-function(grafo, mode = c("out","in","total")){
  mode <- match.arg(mode)
  V(grafo)$grau <- igraph::degree(grafo, mode = mode)
  ind <- quantile(V(grafo)$grau, probs = 0.99)
  print(ind)
  V(grafo)[grau > ind]$name %>% sort()
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

two_step_reach <- function(grafo, v, scale = c("normal","prop")){
  scale <- match.arg(scale)
  size <- igraph::vcount(grafo)
  two <- igraph::ego_size(grafo, order = 2, nodes = v)
  if(scale == "normal"){
    two-1
  } else {
    (two-1)/size
  }
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
  graph.density(ego_neig)
}












