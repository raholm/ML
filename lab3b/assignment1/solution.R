## ---- assign1-init
library(pamr)

data <- read.csv("../data/data.csv", sep=";", header=TRUE,
                 stringsAsFactors=FALSE, encoding="latin1")
rownames(data) <- 1:nrow(data)

train_idx <- sample(nrow(data), size=floor(nrow(data) * 7 / 10))
train <- data[train_idx,]
test <- data[-train_idx,]

x <- t(train[, -ncol(data)])
y <- train[, ncol(data)]

x_test <- t(test[, -ncol(data)])
y_test <- test[, ncol(data)]
## ---- end-of-assign1-init

## 1
## ---- assign1-1
mydata <- list(x=x, y=as.factor(y), geneid=as.character(1:nrow(x)), genenames=rownames(x))
model <- pamr.train(mydata, threshold=seq(0,4, 0.1))

pamr.plotcen(model, mydata, threshold=1)
pamr.plotcen(model, mydata, threshold=2.5)

a <- pamr.listgenes(model, mydata, threshold=2.5)
cat(paste(colnames(data)[as.numeric(a[,1])], collapse='\n' ) )
cvmodel <- pamr.cv(model, mydata)

print(cvmodel)
pamr.plotcv(cvmodel)

sum(pamr.predict(model, x_test, threshold=1) == y_test) / length(y_test)
## ---- end-of-assign1-1

## 2

## 3
