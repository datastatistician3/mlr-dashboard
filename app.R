rm(list = ls(all =TRUE))

mdls <- list('svmLinear'='svmLinear',
             'svmPoly'='svmPoly',
             'Neural Network'='nnet',
             'randomForest'='rf',
             'k-NN'='knn',
             'Naive Bayes'='nb'
             # 'GLM'='glm'
             )
#multinom

mdli <- list(
  'Regression'=c(T,T,T,T,T,F),
  'Classification'=c(T,T,T,T,T,T)
)  

reg.mdls <- mdls[mdli[['Regression']]]
cls.mdls <- mdls[mdli[['Classification']]]


datasets <- list(
  'iris'=iris,
  'cars'=mtcars,
  'diamonds'= ggplot2::diamonds,
  'Boston'=MASS::Boston
  # 'leaf'=leaf
  # 'midwest'=data.frame(midwest),
  # 'mpg'=data.frame(mpg),
  # 'msleep'=data.frame(msleep),
  # 'txhousing'=data.frame(txhousing)
)

model_types <- c("Regression", "Classification")
library(mlbench)
data("Sonar")

rawdata <- iris

# yvar <- rawdata$V14
yvar <- rawdata$Species
xvars <- rawdata[,c("Sepal.Length", "Sepal.Width" , "Petal.Length", "Petal.Width")]
testsize <- 0.80

# if(is.null(yvar)||yvar=='')
#   return(NULL)

# extract y and X from raw data
y <- yvar
X <-  xvars

yi <- !is.na(y)
Xi <- complete.cases(X)
    
df2 <- cbind(y,X)[yi&Xi,]

# c <- class(df2$y)
# lvls <- length(unique(df2$y))
# if(lvls<10|(c!='numeric'&c!='integer')){
#   modelType <<-'Classification'
#   df2$y <- factor(df2$y)
# } else {
#   modelType <<-'Regression'
#   # if(input$chk_logY){df2$y <- log(df2$y+0.1)}
# }

trainIndex <- caret::createDataPartition(df2$y,
                                p = testsize,
                                list = FALSE,
                                times = 1)
dataTrain <<- df2[ trainIndex,]
dataTest  <<- df2[-trainIndex,]

# mdls <- mdls[['svmLinear']]

rdo_CVtype <- 3
    
fitControl <- caret::trainControl(method = "cv",savePredictions = T,
                           number = as.integer(rdo_CVtype),
                           summaryFunction = defaultSummary)

tuneParams <- list(
  'svmLinear'=data.frame(C=c(0.01,0.1,1)),
  'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1)),
  'nnet'=expand.grid(size=c(1,3,5),decay=c(0.01,0.1,1)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(1,3,5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1))
  # 'glm'=NULL#data.frame()
)

trainArgs <- list(
  'svmLinear'= list(form=y ~ .,
                   data = dataTrain,
                   preProcess = c('scale','center'),
                   method = 'svmLinear',
                   trControl = fitControl,
                   tuneGrid=tuneParams[['svmLinear']]),
  'svmPoly'= list(form=y ~ .,
                  data = dataTrain,
                  preProcess = c('scale','center'),
                  method = 'svmPoly',
                  trControl = fitControl,
                  tuneGrid=tuneParams[['svmPoly']]),
  'nnet'= list(form=y ~ .,
              data = dataTrain,
              preProcess = c('scale','center'),
              method = 'nnet',
              trControl = fitControl,
              tuneGrid=tuneParams[['nnet']],
              linout=T),
  'rf'= list(form=y ~ .,
            data = dataTrain,
            preProcess = c('scale','center'),
            method = 'rf',
            trControl = fitControl,
            tuneGrid=tuneParams[['rf']],
            ntree=1e3),
  'knn'= list(form=y ~ .,
             data = dataTrain,
             preProcess = c('scale','center'),
             method = 'knn',
             trControl = fitControl,
             tuneGrid=tuneParams[['knn']]),
  'nb'= list(form=y ~ .,
            data = dataTrain,
            preProcess = c('scale','center'),
            method = 'nb',
            trControl = fitControl,
            tuneGrid=tuneParams[['nb']])
  # 'glm'= list(form=y ~ .,
  #            data = dataTrain,
  #            preProcess = c('scale','center'),
  #            method = 'glm',
  #            trControl = fitControl,
  #            tuneGrid=tuneParams[['glm']])
  # 'gam'= list(form=y ~ .,
  #            data = dataTrain,
  #            preProcess = c('scale','center'),
  #            method = 'gam',
  #            trControl = fitControl,
  #            tuneGrid=tuneParams[['gam']])
)

library(caret)

c <- "Regression1"
if (c == 'Regression') {
  reg_results <- lapply(reg.mdls,function(m){
  do.call('train',trainArgs[[m]])
})

names(reg_results) <- reg.mdls
tuned_model <<- reg_results
#Regression
library(magrittr)

fit_names <- c("RMSE", "Rsquared", "MAE","RMSESD","RsquaredSD","MAESD")

list_df <- (purrr::map(tuned_model, 'results'))

train_fit_summary <- list_df %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>% 
  dplyr::mutate(rank = dplyr::dense_rank(Rsquared)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(rank, dplyr::everything())

best_pred_df <- (purrr::map(tuned_model, 'bestTune')) %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.), sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) 

pred_names <- c('pred','obs', 'rowIndex','Resample')
list_pred_df <- (purrr::map(tuned_model, 'pred'))

train_pred_summary <- list_pred_df %>%
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>%
  dplyr::select(name, dplyr::everything()) %>%
  tidyr::unite_('Model', names(.)[!names(.) %in% pred_names], sep='-', remove=T) %>%
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>%
  dplyr::inner_join(best_pred_df, by = "Model")

list_cols <- (lapply(tuned_model[names(tuned_model)],predict.train,dataTest)) %>% data.frame()

list_cols$y <- dataTest$y

best_pred_df$model_name <- grep(pattern = "[a-z]+|[A-Z]+$", unlist(stringr::str_split(string = best_pred_df$Model, pattern = "-")),value = T)

df_predicted <- tidyr::gather(do.call(cbind.data.frame, list_cols), model_name, predicted, -y) %>% 
  dplyr::inner_join(best_pred_df, by = c("model_name")) %>% 
  dplyr::group_by(model_name) %>% 
  dplyr::mutate(r_square = round(sum((predicted - mean(y))**2) / sum((y - mean(y))**2),4),
                   rmse = round(sqrt(mean((y-predicted)^2)),4)) %>% 
  dplyr::ungroup()

} else {
  class_results <- list()

  class_models <- function(model, tuneParam){
    res <- caret::train(y ~., 
                        dataTrain, 
                        method = model, 
                        preProcess = c('scale','center'), 
                        trControl=fitControl, 
                        tuneGrid = tuneParam)
    return(res)
}

  for (i in 1:sum(mdli$Classification)){
    class_results[[i]] <- class_models(model = cls.mdls[[i]], tuneParam = tuneParams[[i]])
  }

  names(class_results) <- cls.mdls
  tuned_model <<- class_results
  
library(magrittr)

fit_names <- c("Accuracy", "Kappa", "AccuracySD", "KappaSD")

list_df_class <- (purrr::map(tuned_model, 'results'))

train_fit_summary <- list_df_class %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>% 
  dplyr::mutate(rank = dplyr::dense_rank(Accuracy)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(rank, dplyr::everything())

best_pred_df <- (purrr::map(tuned_model, 'bestTune')) %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.), sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) 


pred_names <- c('pred','obs', 'rowIndex','Resample')
list_pred_df <- (purrr::map(tuned_model, 'pred'))

train_pred_summary <- list_pred_df %>%
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>%
  dplyr::select(name, dplyr::everything()) %>%
  tidyr::unite_('Model', names(.)[!names(.) %in% pred_names], sep='-', remove=T) %>%
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>%
  dplyr::inner_join(best_pred_df, by = "Model")

list_cols <- (lapply(tuned_model[names(tuned_model)],predict.train,dataTest)) %>% data.frame()

list_cols$y <- dataTest$y

best_pred_df$model_name <- grep(pattern = "[a-z]+|[A-Z]+]$", unlist(stringr::str_split(string = best_pred_df$Model, pattern = "-|none|TRUE")),value = T)

df_predicted <- tidyr::gather(do.call(cbind.data.frame, list_cols), model_name, predicted, -y) %>% 
  dplyr::inner_join(best_pred_df, by = c("model_name")) %>% 
  dplyr::mutate(predicted = as.factor(predicted)) %>% 
  dplyr::group_by(model_name) %>% 
  dplyr::mutate(Accuracy = round(sum(predicted == y)/length(y),4),
                Kappa    = round(vcd::Kappa(table(predicted,y))$Unweighted['value']),4) %>% 
  dplyr::ungroup() %>% as.data.frame()
}





# 
#   class_results <- list()
#   
#     class_models <- function(model, tuneParam){
#       res <- caret::train(y ~., 
#                           dataTrain, 
#                           method = model, 
#                           preProcess = c('scale','center'), 
#                           trControl=fitControl, 
#                           tuneGrid = tuneParam)
#       return(res)
#   }
#   
#     for (i in 1:length(mdls)){
#       class_results[[i]] <- class_models(model = mdls[[i]], tuneParam = tuneParams[[i]])
#     }
#   
#     names(class_results) <- mdls
#     tuned_model0 <- class_results
#   }
#     tuned_model <<- tuned_model0

# confusion_matrix <- caret::confusionMatrix(df_predicted$predicted, df_predicted$y, positive="R", mode="everything")




# 
# c <- apply(mtcars,1,raster::modal)
# s1 <- sum(c==dataTest$y)/nrow(dataTest)
# s2 <- vcd::Kappa(table(c, dataTest$y))$Unweighted[1]
# 
# 
# 
# sum(df_predicted$predicted == df_predicted$y)/nrow(df_predicted)
# 
# irr::kappa2(cbind(matrix(df_predicted$predicted), matrix(df_predicted$y)))
# 
# vcd::Kappa(table(df_predicted$predicted,df_predicted$y))$Unweighted['value']
# 
# 





### Classification
library(magrittr)

fit_names <- c("RMSE", "Rsquared", "MAE","RMSESD","RsquaredSD","MAESD")

list_df <- (purrr::map(tuned_model, 'results'))

train_fit_summary <- list_df %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>% 
  dplyr::mutate(rank = dplyr::dense_rank(Rsquared)) %>% 
  dplyr::arrange(rank) %>% 
  dplyr::select(rank, dplyr::everything())


best_pred_df <- (purrr::map(tuned_model, 'bestTune')) %>% 
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>% 
  dplyr::select(name, dplyr::everything()) %>% 
  tidyr::unite_('Model', names(.), sep='-', remove=T) %>% 
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) 


pred_names <- c('pred','obs', 'rowIndex','Resample')
list_pred_df <- (purrr::map(tuned_model, 'pred'))

train_pred_summary <- list_pred_df %>%
  purrr::map2_df(names(tuned_model),~dplyr::mutate(.x,name=.y)) %>%
  dplyr::select(name, dplyr::everything()) %>%
  tidyr::unite_('Model', names(.)[!names(.) %in% pred_names], sep='-', remove=T) %>%
  dplyr::mutate(Model = stringr::str_replace_all(Model, "-NA|-NA-|NA-|NA", "")) %>%
  dplyr::inner_join(best_pred_df, by = "Model")

list_cols <- (lapply(tuned_model[names(tuned_model)],predict.train,dataTest)) %>% data.frame()

list_cols$y <- dataTest$y

best_pred_df$model_name <- grep(pattern = "[a-z]+|[A-Z]+$", unlist(stringr::str_split(string = best_pred_df$Model, pattern = "-")),value = T)

df_predicted <- tidyr::gather(do.call(cbind.data.frame, list_cols), model_name, predicted, -y) %>% 
  dplyr::inner_join(best_pred_df, by = c("model_name")) %>% 
  dplyr::group_by(model_name) %>% 
  dplyr::mutate(r_square = sum((predicted - mean(y))**2) / sum((y - mean(y))**2),
                rmse = sqrt(mean((y-predicted)^2))) %>% 
  dplyr::ungroup()


#####################



TabularManifest::histogram_discrete(mtcars, 'am', main_title = "Disctribution of ", x_title = capitalize_each_word(mpg))

TabularManifest::create_manifest_explore_univariate()



Somya::capitalize_each_word

library(ggplot2)
mtcars$am <- as.factor(mtcars$am)

if (class(mtcars$mpg) != "numeric") {
  GGally::ggpairs(mtcars, mapping = aes(color = am), columns = c("am", "drat", "wt"))
} else {
  GGally::ggpairs(mtcars)
}

GGally::ggscatmat(mtcars, columns = 2:4, color="am", alpha=0.8)


tidyr::gather(mtcars, kfsdfey, vadfdslue)

mtcars$am <- as.numeric(mtcars$am)
df <- mtcars %>% 
  tidyr::gather(key, value) %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarize(
     "Minimum"     = min(value, na.rm = TRUE),
     "Maximum"     = max(value, na.rm = TRUE),
     "Mean"        = mean(value, na.rm = TRUE),
     "No. of Obs." = sum(!is.na(value), na.rm = TRUE),
     "Std. Dev."   = round(sd(value, na.rm = TRUE),2),
     "Range"       = paste0("(", Minimum, ",", Maximum, ")")
  ) %>% 
  dplyr::rename("Features" = "key")
     # "Summary"     = sprintf("%.2f (%.2f, %d)", st_mean, st_std, st_count)) %>%
# dplyr::select(-st_mean,-st_count,-st_std, -st_min, -st_max)


mean(mtcars$mpg)

if (ncol(mtcars)*nrow(mtcars) != nrow(df_tidied)) {
  
}

if (class(input$yvar) != 'numeric') {
  mtcars %>% 
  tidyr::gather(key, value) %>% 
  dplyr::group_by_(key, input$yvar) %>% 
  dplyr::summarize(
     "Minimum"     = min(value, na.rm = TRUE),
     "Maximum"     = max(value, na.rm = TRUE),
     "Mean"        = mean(value, na.rm = TRUE),
     "No. of Obs." = sum(!is.na(value), na.rm = TRUE),
     "Std. Dev."   = round(sd(value, na.rm = TRUE),2),
     "Range"       = paste0("(", Minimum, ",", Maximum, ")")
  ) %>% 
  dplyr::rename("Features" = "key")
  
} else {
  mtcars %>% 
  tidyr::gather(key, value) %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarize(
     "Minimum"     = min(value, na.rm = TRUE),
     "Maximum"     = max(value, na.rm = TRUE),
     "Mean"        = mean(value, na.rm = TRUE),
     "No. of Obs." = sum(!is.na(value), na.rm = TRUE),
     "Std. Dev."   = round(sd(value, na.rm = TRUE),2),
     "Range"       = paste0("(", Minimum, ",", Maximum, ")")
  ) %>% 
  dplyr::rename("Features" = "key")
}


 mtcars %>% 
  tidyr::gather(key, value, -"am") %>% 
  dplyr::group_by(key, )


 ds <-iris

 
   if (class(ds[,'Species']) == 'factor') {
    ds %>%
    tidyr::gather(key, value, -'Species') %>%  #), -input$yvar) %>%
    dplyr::group_by_(.dots = 'Species', 'key') %>%
    dplyr::summarize(
       "Minimum"     = min(value, na.rm = TRUE),
       "Maximum"     = max(value, na.rm = TRUE),
       "Mean"        = mean(value, na.rm = TRUE),
       "No. of Obs." = sum(!is.na(value), na.rm = TRUE),
       "Std. Dev."   = round(sd(value, na.rm = TRUE),2),
       "Range"       = paste0("(", Minimum, " , ", Maximum, ")")
    ) %>%
    dplyr::rename("Features" = "key")

  } else {
    ds %>%
    dplyr::select(-'Species') %>% 
    tidyr::gather(key, value) %>%
    dplyr::group_by(key) %>%
    dplyr::summarize(
       "Minimum"     = min(value, na.rm = TRUE),
       "Maximum"     = max(value, na.rm = TRUE),
       "Mean"        = mean(value, na.rm = TRUE),
       "No. of Obs." = sum(!is.na(value), na.rm = TRUE),
       "Std. Dev."   = round(sd(value, na.rm = TRUE),2),
       "Range"       = paste0("(", Minimum, " , ", Maximum, ")")
    ) %>%
    dplyr::rename("Features" = "key")
  }
 
 
 
 
 
 
 
 

# renderPlot({
#     
#     rf <- randomForest::randomForest(y~.,dataTrain)
#     vi <- as.data.frame(randomForest::varImpPlot(rf))
#     vi$Feature <- row.names(vi)
#     names(vi)[1] <- 'Score'
#     vi$Feature <- factor(vi$Feature,levels=vi$Feature[order(vi$Score)])
#   str(vi)  
#     ggplot(vi,aes(x=Feature,y=Score))+
#       geom_bar(stat='identity',fill="#5EECC6")+
#       coord_flip()+
#       xlab('')+
#       ylab('Relative Importance Score')
#     
#   })







# c <- apply(df_preds['svmLinear'],1,mean)
# s1 <- 1 - mean((dataTest$y-c)^2)/mean((dataTest$y-mean(dataTest$y))^2)
# s2 <- sqrt(mean((dataTest$y-c)^2))
# 
# 
# sum((df_predicted$predicted - mean(df_predicted$yy))**2) / sum((df_predicted$yy - mean(df_predicted$yy))**2)
# 
# 
# sqrt(mean(df_predicted$yy- df_predicted$predicted)**2)
#  
# 
# sum(-7.925,-2.825,7.675,3.075,-7.925,-2.825,7.675,3.075,-7.925,-2.825,7.675,3.075,-7.925,-2.825,7.675,3.075,-7.925,-2.825,7.675,3.075)
# knnFit <- caret::train(mpg ~ ., data = mtcars, method = "svmLinear",
#                        trControl = trainControl(method = "cv"))
# 
# rdaFit <- train(mpg ~ ., data = mtcars, method = "svmPoly",
#                 trControl = trainControl(method = "cv"))
# 
# bothModels <- list(svmlinear = knnFit,
#                    svmpoly = rdaFit)
# 
# extractPrediction(bothModels, testX = mtcars[1:5, -1])
# 0.63+extractProb(bothModels, testX = mtcars[1:10, -1])
# 









# knnFit <- caret::train(Species ~ ., data = iris, method = "knn",
#                 trControl = trainControl(method = "cv"))
# 
# rdaFit <- train(Species ~ ., data = iris, method = "rda",
#                 trControl = trainControl(method = "cv"))
# 
# predict(knnFit)
# predict(knnFit, type = "prob")
# 
# bothModels <- list(knn = knnFit,
#                    tree = rdaFit)
# 
# 
# predict.train(knnFit, testX = iris[1:10, -5])
# predict(bothModels)
# 
# extractPrediction(bothModels, testX = iris[1:10, -5])
# extractProb(bothModels, testX = iris[1:10, -5])
# 


 
# 
# 
# get_fits <- list()
# one_fit <- list()
# 
# for (i in names(tune)){
#   one_fit <- tune[[i]]$results
#   # two_fit <- one_fit[(ncol(one_fit)-3):ncol(one_fit)]
#   tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T)
#   get_fits <- rbind(get_fits,one_fit)
# }
# 
# 
# 
# 
# df %>% 
#   dplyr::select(model, dplyr::everything()) %>% 
#   tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T)
# 
# names(tune)
# 
# 
# 
# 
# 
# one_fit <- tune[['svmLinear']]$results
# # two_fit <- one_fit[(ncol(one_fit)-3):ncol(one_fit)]
# one_fit %>% 
# tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T)
# 
# get_fits <- rbind(get_fits,one_fit)
# 
# 
# 
# 
# names(tune)
# 
# tune %>% 
#   map('')
# 
# purrr::map(tune, 'results') %>% 
#   dplyr::mutate_(mtry = map(.,2))
#   
#   
# purrr::map2_df(tune, names(tune), ~ dplyr::mutate(.x, ID = .y))
#   
#   dplyr::mutate( Model = purrr::map(tidyr::unite_(names(.)[!names(.) %in% fit_names], sep='-', remove=T)))
# 
# 
# 
# list_df <- (purrr::map(tune, 'results'))
# 
# 
# 
# library(tidyverse)
# df <- data_frame(one = rep("hey", 10), two = seq(1:10), etc = "etc")
# 
# list_df <- list(df, df, df[1:7,], df, df[1:4,])
# dfnames <- c("first", "second", "third", "fourth", "fifth")
# 
# list_df %>% map2_df(names(tune),~mutate(.x,name=.y)) %>% 
#   dplyr::select(name, everything()) %>% 
#   tidyr::unite_('Model', names(.)[!names(.) %in% fit_names], sep='-', remove=T) %>% 
#   dplyr::mutate(Model = str_replace_all(Model, "-NA|-NA-|NA-|NA", ""))
# 
# 
# purrr::map(tune, 'results') 
# 
# 
# %>% 
#   purrr::map2_df(names(tune), ~mutate(.x, name = .y))
# 
# 
# 
# fruits <- c("oneapple-NA", "twopears-NA-NA", "three-NA-bananas")
# str_replace_all(fruits, "-NA|-NA-|NA-|NA", "")
# 
# 
# 
# tune$svmLinear$results
# df <- tune$svmPoly$results
# df$model <- names(tune)[1]
# 
# 
# 
# 
# 
# tune$`Neural Network`$results
# df <- tune$`k-NN`$results
# df[1:(ncol(df)-4)]
# 
# df[(ncol(df)-3):ncol(df)]
# 
# 
# library(magrittr)
# 
# getRes <- function(i){
#     name <- names(tune)[i]
#     res <- tune[[i]]$results
#     df <- res[(ncol(res)-3):ncol(res)]
#     apply(res,1,function(r) paste(r[1:(ncol(res)-4)],collapse = '-')) %>% 
#       paste(name,.,sep='-') -> model
#     cbind.data.frame(model,df,name=name[[1]],stringsAsFactors =F)
#   }
#     
#     df <- plyr::ldply(1:length(tune),getRes)
# 
# do.call(caret::train(mdls, trainArgs[[1]]))
# 
# caret::train(trainArgs[[1]], mdls)
# 
# 
# library(caret)
# do.call('train',trainArgs[[1]])
# 
# do.call('caret::train()', trainArgs[[1]])
# 
# 
# 
# 
# names(tune) <- mdls
# CVtune <<- tune