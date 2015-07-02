library(randomForest)
library(ggplot2)

data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)



training_data = read.table('/home/ipseg/Desktop/waples/RAD_allele_depth/data/chinook.training.txt', sep = "\t", header = TRUE)
unknown_data = read.table('/home/ipseg/Desktop/waples/RAD_allele_depth/data/chinook.unknown.txt', sep = "\t", header = TRUE)

training_data$Duplicate = as.factor(training_data$Duplicate)

loci.rf <- randomForest(factor(Duplicate) ~ depth_per_het + abs_z,
                        data = training_data, ntree=200, mtry=2,  importance=TRUE, proximity = TRUE)

loci.rf

plot(loci.rf)
plot(margin(loci.rf))
#MDSplot(loci.rf, training_data$Duplicate)



getTree(loci.rf, labelVar=TRUE)

predict.rf <- predict(loci.rf, unknown_data)

unknown_data$prediction <- factor(predict.rf)
unknown_data$prediction <- as.factor(unknown_data$prediction)



ggplot() + 
  geom_point(data = training_data, aes(x= depth_per_het, y = z, color = Duplicate), alpha =.5)

ggplot() +   
  geom_point(data = unknown_data,  aes(x= depth_per_het, y = z, color = prediction), alpha =.5)



importance(loci.rf)

names(loci.rf)
loci.rf$importance



loci.rf <- randomForest(data = training_data, 
                        xtest = training_data, 
                        ytest = , )