#install.packages("arules")
#install.packages("DT")
#install.packages("plotly")
#install.packages("arulesViz")
#install.packages("visNetwork")
#install.packages("igraph")
#install.packages("kableExtra")

# Load packages

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
# Load data files
order_products_train <- fread('C:/Users/JAINHEMANT/Desktop/order_products.csv')
products <- fread('C:/Users/JAINHEMANT/Desktop/Coursera Courses/products.csv')
# Create Shopping basket:
order_baskets=order_products_train %>% 
  inner_join(products, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# Create transaction data
transactions <- as(order_baskets$basket, "transactions")
inspect(transactions[1])

# Implementing Apriori Algorithm
rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.25))

# Remove redundant rule    
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]
head(rules_dt,5)

library("RColorBrewer")
arules::itemFrequencyPlot(transactions,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)")

plotly_arules(rules)

sel <- plot(rules, measure=c("support", "lift"), 
            shading = "confidence",
            interactive = TRUE)

subrules2 <- head(sort(rules, by="confidence"),20)
ig <- plot( subrules2, method="graph", control=list(type="items") )
ig_df <- get.data.frame( ig, what = "both" )
nodesv %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )




