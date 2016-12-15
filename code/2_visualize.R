# function to save plot variable
# sythax: savePlot(plotVariable, plotName)

savePlot <- function(sPlot, sName) {
  png(sName)
  print(sPlot)
  cat("\014")
    cat('Saved to ', getwd())
}

# function to save plot image
# sythax: saveImg(plotVariable, plotName)

saveImg <- function(sName, sPlot){
  sName <- paste0(sName,'.png')
  ggsave(filename=sName, plot=sPlot)
  cat("\014")
  cat('Saved to ', getwd())
    }

# 1. VISUALIZATION

library(ggplot2)
library(gridExtra)

# plot 1 - relation between sales and profit

temp <- salesTotalRevenue
temp$warehouseName <- NULL
temp <- aggregate(cbind(temp$amount, temp$profitAmount)~ temp$monthYear, data=temp, FUN = sum) 
names(temp)[1]='monthYear'
names(temp)[2]='amount'
names(temp)[3]='profit'

p1 <- ggplot(temp, aes(x = temp$amount, y = temp$profit)) + 
  geom_point() + 
  geom_smooth() + 
  labs(x="Sales in EUR", y = "Profit in EUR", title = 'Revenue by profit in EUR') +
    theme(
    axis.title.x = element_text(face="bold", size=10, color = 'red'),
    axis.title.y = element_text(face="bold", size=10, color = 'blue'),
    plot.title = element_text(face="bold", color = "black", size=12)
)

# plot 2 - sales and profit by store

p2 <- ggplot(salesTotalRevenue, aes(x=amount, y = profitAmount, color=factor(warehouseName))) +
  geom_smooth() + labs(fill = 'Store') +
  labs(x="Sales in EUR", y = "Profit in EUR", title = 'Revenue and profit by stores') +
  theme(
    axis.title.x = element_text(face="bold", size=10, color = 'red'),
    axis.title.y = element_text(face="bold", size=10, color = 'blue'),
    plot.title = element_text(face="bold", color = "black", size=12)
  ) 

# plot 3 - variations of plot 2 with lines

p3 <- ggplot(salesTotalRevenue, aes(x=amount, y = profitAmount, color=factor(warehouseName))) +
  geom_smooth() + labs(fill = 'Store') +
  labs(x="Sales in EUR", y = "Profit in EUR", title = 'Revenue and profit by stores') +
  theme(
    axis.title.x = element_text(face="bold", size=10, color = 'red'),
    axis.title.y = element_text(face="bold", size=10, color = 'blue'),
    plot.title = element_text(face="bold", color = "black", size=12)
  ) 

p31 <- p1 + geom_point(color='blue') + geom_line()
p32 <- p1 + geom_smooth(method='lm', se=TRUE)
p33 <- p1 + geom_point() + geom_vline(xintercept = 20000, color='red')

p34 <- ggplot(salesTotalRevenue, aes(x = amount, y = profitAmount))+ 
  geom_line(size=1, aes(color=factor(warehouseName)))

# plot 4 - sales by category   

# creating temporary data frame with aggregate sales of main categories

temp <- salesByCategory
temp$monthYear <- NULL
temp <- aggregate(temp$amount, by=list(salesByCategory$categoryName), FUN = sum) 
names(temp)[1]='categoryName'
names(temp)[2]='amount'
temp <- temp[order(-temp$amount),]
temp <- head(temp, 10)

p4 <- ggplot(temp, aes(x = categoryName, y = amount, fill = categoryName)) + 
      geom_bar(stat='identity')+ 
  labs(x="Category", y = "Sales in EUR", title = 'Sales by category in EUR') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete(name = "Category")

# plot 5 - sales by category in time

temp1 <- subset(salesByCategory, salesByCategory$categoryName==temp$categoryName)

p5 <- ggplot(temp1, aes(x=monthYear, y=amount, colour=categoryName, group=categoryName)) + 
  geom_line() +  
  labs(x="Time in months", y = "Sales in EUR", title = 'Sales by category in time') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Category")

# plot 6 - barplot total sales in time

p6 <- ggplot(salesBetween, aes(x = monthYear, y = amount, fill=warehouseName)) + 
  geom_bar(stat='identity') +
  labs(x="Time in months", y = "Sales in EUR", title = 'Total sales by store in time') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete(name = "Store")

# plot 7 - facet plot with store as a factor

p7 <- ggplot(salesByWarehouse, aes(x=monthYear, y=amount, colour = warehouseName)) +
  geom_point() + 
  facet_grid(warehouseName~.) +
 # scale_x_date(breaks = 'month', labels='monthYear') +
  xlab('Month') +
  ylab('EUR') + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Store")

cat("\014")
