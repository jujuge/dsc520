library(ggplot2)
library(class)
library(caTools)
b <- read.csv("binary-classifier-data.csv")
t <- read.csv("trinary-classifier-data.csv")

ggplot(data = b, aes(x = x, y = y, color = label)) + geom_point() +
  ggtitle("Binary Classifier")
ggplot(data = t, aes(x = x, y = y, color = label)) + geom_point() +
  ggtitle("Trinary Classifier")

x = NULL
y = NULL
count = 1
for(i in -40:330){
  for(j in -20:330){
    x[count] <- i / 3
    y[count] <- j / 3
    count <- count + 1
  }
}
newDf <- data.frame(x, y)
newDf$predicted.values <- knn(b[2:3], newDf, bi$label, k = 59)

ggplot() + 
  geom_point(data = newDf, 
             aes(x = x, 
                 y = y, 
                 color = predicted.values),
             shape = 20, 
             size = 0.5) + 
  geom_point(data = b, 
             aes(x = x, 
                 y = y, 
                 fill = label), 
             colour = "blue", 
             size = 1) + 
  theme_bw()

error <- NULL
predicted <- NULL
set.seed(520)
k.values <- 1:101
sample <- sample.split(b$label, SplitRatio = 0.7 )
train <- subset(b, sample == TRUE)
test <- subset(b, sample == FALSE)