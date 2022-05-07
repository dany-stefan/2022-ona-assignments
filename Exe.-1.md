Exercise 1
================

## Code

    library("tidyverse")
    library("igraph")
    csv = read_csv('/Users/danystefan/Documents/01 McGill University/01 MMA/01 Summer 2022/ORGB 672/Assignments/Exe. 1/Connections.csv')
    csv
    View(csv)
    attach(csv)
    csv %>% drop_na(Company)

Counting

    count = csv %>% count(Company, sort=TRUE)
    count

![](/Users/danystefan/Documents/01%20McGill%20University/01%20MMA/01%20Summer%202022/ORGB%20672/Assignments/Exe.%201/count.png)

    csv$last_initial <- substr(csv$`Last Name`, 1, 1)
    csv$node_label <- paste(csv$`First Name`, csv$last_initial)
    nodes <- csv %>% distinct(node_label)
    View(csv)
    nodes <- nodes %>% rowid_to_column('id')
    nodes
    copy <- csv
    colnames(copy) <- paste(colnames(copy), "2", sep="_")
    cross <- tidyr::crossing(csv, copy, .name_repair="minimal")
    edges <- filter(cross, cross$Company == cross$Company_2 & cross$node_label != cross$node_label_2)
    edges <- edges %>% select(node_label, Company, node_label_2, Company_2)
    edges <- edges %>% 
      left_join(nodes, by = c("node_label" = "node_label")) %>% 
      rename(node_1 = id)
    edges <- edges %>% 
      left_join(nodes, by = c("node_label_2" = "node_label")) %>% 
      rename(node_2 = id)

    edges <- select(edges, node_1, node_2)
    edges

![](/Users/danystefan/Documents/01%20McGill%20University/01%20MMA/01%20Summer%202022/ORGB%20672/Assignments/Exe.%201/edges.png)

Graph Network

    library("tidygraph")
    library("ggraph")
    network <- tbl_graph(nodes=nodes, edges=edges, directed=FALSE)
    network

![](/Users/danystefan/Documents/01%20McGill%20University/01%20MMA/01%20Summer%202022/ORGB%20672/Assignments/Exe.%201/network.png)

    ggraph(network) + geom_edge_link() + geom_node_point() + theme_graph()

![](/Users/danystefan/Documents/01%20McGill%20University/01%20MMA/01%20Summer%202022/ORGB%20672/Assignments/Exe.%201/graph.png)
