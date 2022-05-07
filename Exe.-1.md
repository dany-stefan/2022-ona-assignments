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
