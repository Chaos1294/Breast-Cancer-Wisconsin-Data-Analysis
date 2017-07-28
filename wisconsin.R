library(corrplot)
library(ggplot2)
library(grid)
library(gridExtra)
library(pROC)
library(mlbench)
library(caret)
library(klaR)
library(MASS)
library(resample)
library(nnet)

cancer.data <- read.csv("data.csv", header = TRUE, sep = ",")
cancer.data <- cancer.data[,c(-33)]
str(cancer.data)
summary(cancer.data)

#### CORRELATION MATRIX ####

corr.mat <- cor(cancer.data[,3:ncol(cancer.data)])
corrplot(corr.mat)

#### BOXPLOTS ####

box.radiusmean <- ggplot(cancer.data, aes(x = "", y = radius_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Radius.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.radiusmean)

box.texturemean <- ggplot(cancer.data, aes(x = "", y = texture_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Texture.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.texturemean)

box.perimetermean <- ggplot(cancer.data, aes(x = "", y = perimeter_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Perimeter.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.perimetermean)

box.areamean <- ggplot(cancer.data, aes(x = "", y = area_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Area.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.areamean)

box.smoothnessmean <- ggplot(cancer.data, aes(x = "", y = smoothness_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Smoothness.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.smoothnessmean)

box.compactnessmean <- ggplot(cancer.data, aes(x = "", y = compactness_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Compactness.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.compactnessmean)

box.concavitymean <- ggplot(cancer.data, aes(x = "", y = concavity_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Concavity.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.concavitymean)

box.concavepointsmean <- ggplot(cancer.data, aes(x = "", y = concave.points_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("ConcavePoints.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.concavepointsmean)

box.fractaldimensionmean <- ggplot(cancer.data, aes(x = "", y = fractal_dimension_mean)) + 
  stat_boxplot(geom ='errorbar')+ ggtitle("Fractal.dimension.Mean_Boxplot")+
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2) #+ theme(axis.title.x = element_blank())
print(box.fractaldimensionmean)

###### ARRANGING BOXPLOTS #######
box_imp <- grid.arrange(box.radiusmean,box.texturemean,box.perimetermean,box.areamean,box.smoothnessmean,
                        box.compactnessmean,box.concavitymean,box.concavepointsmean,box.fractaldimensionmean,
                        top=textGrob("Boxplots", gp=gpar(fontsize=15,font=1)))

#### HISTOGRAMS ####
hist_radius.mean <- ggplot(data=cancer.data, aes(cancer.data$radius_mean)) + geom_histogram(breaks=seq(1, 30, by =1),col="red",fill="orange")
hist_texture.mean <- ggplot(data=cancer.data, aes(cancer.data$texture_mean)) + geom_histogram(breaks=seq(1, 40, by =1),col="red",fill="orange")
hist_perimeter.mean <- ggplot(data=cancer.data, aes(cancer.data$perimeter_mean)) + geom_histogram(breaks=seq(1, 200, by =2),col="red",fill="orange")
hist_area.mean <- ggplot(data=cancer.data, aes(cancer.data$area_mean)) + geom_histogram(breaks=seq(100, 2600, by =5),col="red",fill="orange")
hist_smoothness.mean <- ggplot(data=cancer.data, aes(cancer.data$smoothness_mean)) + geom_histogram(breaks=seq(0, 0.20, by =0.01),col="red",fill="orange")
hist_compactness.mean <- ggplot(data=cancer.data, aes(cancer.data$compactness_mean)) + geom_histogram(breaks=seq(0, 0.40, by =0.01),col="red",fill="orange")
hist_concavity.mean <- ggplot(data=cancer.data, aes(cancer.data$concavity_mean)) + geom_histogram(breaks=seq(0, 0.50, by =0.01),col="red",fill="orange")
hist_concavity.points.mean <- ggplot(data=cancer.data, aes(cancer.data$concave.points_mean)) + geom_histogram(breaks=seq(0, 0.20, by =0.01),col="red",fill="orange")
hist_fractal.dimension.mean <- ggplot(data=cancer.data, aes(cancer.data$fractal_dimension_mean)) + geom_histogram(breaks=seq(0, 0.1, by =0.01),col="red",fill="orange")
hist_symmetry.mean <- ggplot(data=cancer.data, aes(cancer.data$symmetry_mean)) + geom_histogram(breaks=seq(0, 0.20, by =0.01),col="red",fill="orange")

hist_imp <- grid.arrange(hist_radius.mean,hist_texture.mean,hist_perimeter.mean,
                         hist_area.mean,hist_smoothness.mean,hist_compactness.mean,
                         hist_concavity.mean,hist_concavity.points.mean,
                         hist_fractal.dimension.mean,hist_symmetry.mean,
                         top=textGrob("Histograms", gp=gpar(fontsize=15,font=1)))


#### FEATURE IMPORTANCE #####

set.seed(7)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diagnosis~., data=cancer.data, method="lvq", preProcess="scale", trControl=control)
model
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#### QQ PLOTS ####

qq_perimeter.worst <- qqnorm(cancer.data$perimeter_worst, main = "Perimeter Worst")
qq_radius.worst <- qqnorm(cancer.data$radius_worst, main = "Radius Worst")
qq_area.worst <- qqnorm(cancer.data$area_worst, main = "Area Worst")
qq_concave.points.worst <- qqnorm(cancer.data$concave.points_worst, main = "Concave Points Worst")
qq_concave.points.mean <- qqnorm(cancer.data$concave.points_mean, main = "Concave Points Mean")
qq_perimeter.mean <- qqnorm(cancer.data$perimeter_mean, main = "Perimeter Mean")
qq_area.mean <- qqnorm(cancer.data$area_mean, main = "Area Mean")
qq_concavity.mean <- qqnorm(cancer.data$concavity_mean, main = "Concavity Mean")
qq_radius.mean <- qqnorm(cancer.data$radius_mean, main = "Radius Mean")
qq_area.se <- qqnorm(cancer.data$area_se, main = "Area SE")
qqimp <- grid.arrange(qq_perimeter.worst,qq_radius.worst,qq_area.worst,
                      qq_concave.points.worst,qq_concave.points.mean,qq_perimeter.mean,
                      qq_area.mean,qq_concavity.mean,
                      qq_radius.mean, top=textGrob("QQ Plots", gp=gpar(fontsize=15,font=1)))


##### RANDOM FOREST #####

cancer.rf <- train(diagnosis~.,
                  train.data,
                  method="ranger",
                  metric="ROC",
                  #tuneLength=10,
                  #tuneGrid = expand.grid(mtry = c(2, 3, 6)),
                  preProcess = c('center', 'scale'),
                  trControl=fitControl)
cancer.rf
pred_rf <- predict(cancer.rf, test.data)
cm_rf <- confusionMatrix(pred_rf, test.data$diagnosis, positive = "M")
cm_rf

pred_prob_rf <- predict(cancer.rf, test.data, type="prob")
roc_rf <- roc(test.data$diagnosis, pred_prob_rf$M)
plot(roc_rf)

##### K-NEAREST NEIGHBOR #####

cancer.knn <- train(diagnosis~.,
                   train.data,
                   method="knn",
                   metric="ROC",
                   preProcess = c('center', 'scale'),
                   tuneLength=10,
                   trControl=fitControl)

pred_knn <- predict(cancer.knn, test.data)
cm_knn <- confusionMatrix(pred_knn, test.data$diagnosis, positive = "M")
cm_knn

pred_prob_knn <- predict(cancer.knn, test.data, type="prob")
roc_knn <- roc(test.data$diagnosis, pred_prob_knn$M)
plot(roc_knn)

##### NAIVE BAYES #####

cancer.nb <- train(diagnosis~.,
                  train.data,
                  method="nb",
                  metric="ROC",
                  preProcess=c('center', 'scale'),
                  trace=FALSE,
                  trControl=fitControl)

pred_nb <- predict(cancer.nb, test.data)
cm_nb <- confusionMatrix(pred_nb, test.data$diagnosis, positive = "M")
cm_nb

pred_prob_nb <- predict(cancer.nb, test.data, type="prob")
roc_nb <- roc(test.data$diagnosis, pred_prob_nb$M)
plot(roc_nb)

model.list <- list(RF=cancer.rf, KNN=cancer.knn,NB=cancer.nb)
resamples <- resamples(model.list)
model.cor <- modelCor(resamples)
corrplot(model.cor)
bwplot(resamples, metric="ROC")

### CONFUSION MATRICES COMPARISON ###
cm_list1 <- list(RF=cm_rf,KNN = cm_knn, NB=cm_nb)
cm_list_results1 <- sapply(cm_list1, function(x) x$byClass)
cm_list_results1

cm_results_max1 <- apply(cm_list_results1, 1, which.is.max)
output_report1 <- data.frame(metric=names(cm_results_max1), 
                            best_model=colnames(cm_list_results1)[cm_results_max1],
                            value=mapply(function(x,y) {cm_list_results1[x,y]}, 
                                         names(cm_results_max1), 
                                         cm_results_max1))
rownames(output_report1) <- NULL
output_report1