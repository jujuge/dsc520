library(dplyr)
library(date)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(gridExtra)
library(lubridate)
library(corrplot)
library(corrgram)
library(reshape2)

df <- read.csv("supermarket_sales - Sheet1.csv")
head(df)
str(df)

retail$Date <- gsub('/', '-', retail$Date)
head(df)

cleanData <- df[,-1]
cleanData$Date <- as.Date(cleanData$Date, "%m/%d/%y")
year(cleanData$Date) <- 2019
sapply(dcleanData, function(x) sum(is.na(x)))
summary(cleanData)

str(cleanData)

new = cleanData[['Unit.price', 'Total', 'Rating', 'gross.income']].copy()
new<- cleanData[, c("Unit.price", "Total", "Rating", 'gross.income')]
print(head(df1))
cleanData[is.na(df1)] <- 0
corrplot(cor(new))

salesByDay <- data.frame(xtabs(formula=Total~Date, data=cleanData))
salesByDay$Date <- as.Date(salesByDay$Date)

ggplot(data = salesByDay, mapping = aes(x = Date, y = Freq))+ geom_line()+ylab("Sales By Day")

ggplot(data = cleanData, mapping = aes(x = Branch, y = Rating)) + geom_boxplot(notch = TRUE, mapping = aes(fill = Branch)) +
  theme_linedraw() + ggtitle("Branch x Ratings") + xlab("Branch")+ ylab("Rating")+
  geom_hline(mapping = aes(yintercept = 7.1), linetype = "dashed") + geom_hline(mapping = aes(yintercept = 6.7), linetype = "dashed") +
  geom_text(mapping = aes(x = "A", y = 7.5, label = "Yangon")) + geom_text(mapping = aes(x = "B", y = 7.5, label = "Mandalay")) + geom_text(mapping = aes(x = "C", y = 7.5, label = "Naypyitaw")) +
  scale_y_continuous(breaks = c(4,5.3,5.6,6.7,7.1,8.2,8.5,10))

p1 <- ggplot(data = cleanData, mapping = aes(x = Branch)) + geom_bar(mapping = aes(fill = Customer.type)) + 
  theme_linedraw() + ggtitle("Customers by Gender") + xlab("Branch") + ylab("Number of Customers") 
p1 + facet_wrap(cleanData$Gender) + scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) + labs(caption = "Female(Members) - 80/85/96 \n Male(Members) - 87/80/73")

p2 <- ggplot(data = data_removed, mapping = aes(x = Branch)) + geom_bar(mapping = aes(fill = Customer.type)) + 
  theme_linedraw() + ggtitle("Payment Mode")+ xlab("Branch") + ylab("Number of Customers") 
p2 + facet_wrap(data_removed$Payment) + scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) + labs(caption = "Cash(Members) - 56/53/59 \n CreditCard(Members) - 49/63/60 \n Ewallet(Members) - 62/49/50")

A <- df %>% filter(Branch == "A")
total_A <- data.frame(xtabs(formula = Total~Date, data = A))
total_A$Date <- as.Date(total_A$Date)
B <- data_removed %>% filter(Branch == "B")
total_B <- data.frame(xtabs(formula = Total~Date, data = B))
total_B$Date <- as.Date(total_B$Date)
C <- data_removed %>% filter(Branch == "C")
total_C <- data.frame(xtabs(formula = Total~Date, data = C))
total_C$Date <- as.Date(total_C$Date)
#total sales x branch
pl1 <- ggplot(data = total_A, mapping = aes(x = Date, y = Freq))+ geom_line()+
  theme_linedraw()+ ggtitle("Total Sales per day in Branch A")+ xlab("Date")+ ylab("Total Sales Per Day")
pl2 <- ggplot(data = total_B, mapping = aes(x = Date, y = Freq))+ geom_line()+
  theme_linedraw()+ ggtitle("Total Sales per day in Branch B")+ xlab("Date")+ ylab("Total Sales Per Day")
pl3 <- ggplot(data = total_C, mapping = aes(x = Date, y = Freq))+ geom_line()+
  theme_linedraw()+ ggtitle("TTotal Sales per day in Branch C")+ xlab("Date")+ ylab("Total Sales Per Day")

grid.arrange(pl1, pl2, pl3)
#customer satisfaction ratings/density
ggplot(data=cleanData) +
  geom_density(mapping=aes(x=Rating, fill=Branch), alpha=.5)
#Sales based on catagory
ggplot(data=cleanData, mapping=aes(x=Product.line, y=Total, color=Branch))+
  geom_boxplot()+
  coord_flip()

ggplot (data=cleanData)+
  geom_bar(mapping=aes(x=Branch, fill=Product.line), position="dodge") +
  coord_flip()
x <- cleanData$Branch
y <- cleanData$Product.line
z <- cleanData$Rating
a <- cleanData$Gender
b<-cleanData$Quantity
newReg <- lm(x ~ y, data = cleanData, subset = 2:4)
print(newReg)

newReg1 <- lm(x ~ z, data = cleanData, subset = 2:4)
print(newReg1)

newReg2 <- lm(a ~ b, data = cleanData, subset = 2:4)
print(newReg1)



