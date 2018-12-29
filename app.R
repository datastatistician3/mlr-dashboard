mdls <- list('svmLinear'='svmLinear',
             'svmPoly'='svmPoly',
             'Neural Network'='nnet',
             'randomForest'='rf',
             'k-NN'='knn',
             'Naive Bayes'='nb',
             'GLM'='glm',
             'GAM'='gam')

mdli <- list(
  'Regression'=c(T,T,T,T,T,F,T,F),
  'Classification'=c(T,T,T,T,T,T,F,F)
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
rawdata <- mtcars

yvar <- rawdata$mpg
xvars <- rawdata[,c('am', 'vs', 'hp', 'carb', 'wt')]
testsize <- 0.80

# if(is.null(yvar)||yvar=='')
#   return(NULL)

# extract y and X from raw data
y <- yvar
X <-  xvars

yi <- !is.na(y)
Xi <- complete.cases(X)
    
df2 <- cbind(y,X)[yi&Xi,]

c <- class(df2$y)
lvls <- length(unique(df2$y))
if(lvls<10|(c!='numeric'&c!='integer')){
  modelType <<-'Classification'
  df2$y <- factor(df2$y)
} else {
  modelType <<-'Regression'
  # if(input$chk_logY){df2$y <- log(df2$y+0.1)}
}

trainIndex <- caret::createDataPartition(df2$y,
                                p = testsize,
                                list = FALSE,
                                times = 1)
dataTrain <<- df2[ trainIndex,]
dataTest  <<- df2[-trainIndex,]

# mdls <- mdls[['svmLinear']]

rdo_CVtype <- 3
    
fitControl <- caret::trainControl(method = "cv",savePredictions = T,
                           number = as.integer(rdo_CVtype))

tuneParams <- list(
  'svmLinear'=data.frame(C=c(0.01,0.1,1)),
  'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1)),
  'nnet'=expand.grid(size=c(1,3,5),decay=c(0.01,0.1,1)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(1,3,5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1)),
  'glm'=NULL#data.frame()
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
            tuneGrid=tuneParams[['nb']]),
  'glm'= list(form=y ~ .,
             data = dataTrain,
             preProcess = c('scale','center'),
             method = 'glm',
             trControl = fitControl,
             tuneGrid=NULL),
  'gam'= list(form=y ~ .,
             data = dataTrain,
             preProcess = c('scale','center'),
             method = 'gam',
             trControl = fitControl)
)

tune <- lapply(mdls,function(m){
  do.call('train',trainArgs[[m]])
})

do.call(caret::train(mdls, trainArgs[[1]]))

caret::train(trainArgs[[1]], mdls)


library(caret)
do.call('train',trainArgs[[1]])

do.call('caret::train()', trainArgs[[1]])




names(tune) <- mdls
CVtune <<- tune