#intro to machine learning
#Justin Hanlon
#2020-10-30
library(ggplot2)
df <- read.csv("clustering-data.csv")
ggplot(data = df, aes(x = x, y = y)) + 
  geom_point(size = 0.3) + 
  theme_bw()

for(i in 2:8){
  set.seed(1)
  df <- read.csv("clustering-data.csv")
  df.cluster <- kmeans(df, i)
  df$cluster <- as.factor(df.cluster$cluster)
  p <- ggplot(data = df, 
              aes(x = x, 
                  y = y, 
                  color = cluster)) + 
    geom_point(size = 0.8) + 
    geom_point(data = as.data.frame(df.cluster$centers), 
               color = "blue", 
               shape = 18, 
               size = 4) 
  print(p)
}
  #K means is unsupervised so there is nothing to compare it to
  
  set.seed(1)
  errors <- NULL
  counter <- 0
  ks <- 1:20
  for( i in ks){
    df <- read.csv("clustering-data.csv")
    df.cluster <- kmeans(df, centers = i)
    df$cluster <- as.factor(df.cluster$cluster)
    df$x.dist <- df.cluster$centers[df$cluster,"x"] - df$x
    df$y.dist <- df.cluster$centers[df$cluster,"y"] - df$y
    df$tot.dist <- sqrt((df$x.dist ** 2) + (df$y.dist ** 2))
    errors[pos] <- mean(df$tot.dist)
    counter <- counter + 1
  }
  error.df <- data.frame(ks,errors)
  ggplot(data = error.df, aes(x = ks, y = errors)) + 
    geom_line(color = "blue") + 
    ggtitle("Average of Distance Residuals") + 
    xlab("K-value") + 
    ylab("Error") +
    geom_point()
