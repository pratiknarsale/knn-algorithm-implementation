# K Nearest Neighbors implementation
data = iris

row_labels = data[,5]

data$Species <- as.numeric(data$Species)

data[,1:4] <- scale(data[,1:4])

set.seed(123)

size <- floor(0.8 *  nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = size)

train_labels <- data[train_ind, 5]

data_train <- data[train_ind,1:4]
data_test <- data[-train_ind,1:4]


data_test_labels <- row_labels[-train_ind]

library(class)

predictions <- knn(train = data_train,
                   test = data_test,
                   k= round(sqrt(nrow(data_train))))

plot_predictions <- data.frame(
  data_test$Sepal.Length,
  data_test$Sepal.Width,
  data_test$Petal.Length,
  data_test$Petal.Width,
  predicted = predictions)

colnames(plot_predictions) <- c("Sepal.Length",
                                "Sepal.Width",
                                "Petal.Length",
                                "Petal.Width",
                                'predicted')

library(ggplot2)

library(plyr)

require(gridExtra)

p1 <- ggplot(plot_predictions, aes(Petal.Length, Petal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_text(aes(label=data_test_labels),hjust=1, vjust=2) +
  ggtitle("Predicted relationship between Petal Length and Width") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


p2 <- ggplot(plot_predictions, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_text(aes(label=data_test_labels),hjust=1, vjust=2) +
  ggtitle("Predicted relationship between Sepal Length and Sepal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol=2)