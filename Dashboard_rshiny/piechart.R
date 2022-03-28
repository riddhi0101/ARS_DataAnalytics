library(dplyr)
library(plotly)

df = read.csv("/Users/riddhib/Desktop/ReuseStore/ARS_DataAnalytics/Data/clean_entire.csv")
df$Date = as.Date(df$Date)
df$Category = as.factor(df$Category)
options = levels(df$Category)

time_start = as.Date("2021-10-01")
time_end = as.Date("2021-10-29")

cat = NULL

if (is.null(cat)) {
    subset_df = df %>% filter(Date >= time_start, Date <= time_end) %>% count(Category)
    title = 'Category Breakdown of All Items Sold'
    fig <- plot_ly(subset_df, labels = ~Category, values = ~n, type = 'pie')
    fig <- fig %>% layout(title = title,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
}else{
    print('here')
    title = paste('Items sold in ', cat)
    subset_df = df %>% filter(Date >= time_start, Date <= time_end, Category == cat) %>% count(Item)
    fig <- plot_ly(subset_df, labels = ~Item, values = ~n, type = 'pie')
    fig <- fig %>% layout(title = title,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
}





