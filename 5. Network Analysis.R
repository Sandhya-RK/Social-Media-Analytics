#Networks of users
library(vosonSML)
class(final.df) <- c(class(final.df),"datasource","twitter")
class(final.df)

actorGraph <- final.df[] %>%      # tweets data table
  Create("actor") %>%             # Actor graph 
  Graph()                         # igraph network graph

source("graphHelpFunctions.R")
get_igraph_attrs(actorGraph)
V(actorGraph)$name %>% head(4)
V(actorGraph)$screen_name %>% head(4)
V(actorGraph)$label %>% head(4)
E(actorGraph)$edge_type %>% as.factor() %>% levels
actorGraph.simplyfied = simplify.actor.network(actorGraph, remove.loops = TRUE, delete.zero.degree = TRUE)
grep("^layout_with_.*[^[sugiyama]]*", ls("package:igraph"), value = TRUE) %>%  print

plot.actor.Graph(actorGraph.simplyfied, 
                 vertex.label = NA, 
                 layout = layout_with_kk)
top.ranked.users(actorGraph.simplyfied)[1:15]
named.users = top.ranked.users(actorGraph.simplyfied)[1:15]
actorGraph.named = label.user.network(actorGraph.simplyfied,
                                      named.users)
plot.actor.Graph(actorGraph.named,layout = layout_with_kk)