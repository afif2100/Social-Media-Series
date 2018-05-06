library(RTextTools)
library(e1071)

#input data train
positif = readLines("train/posTr.csv")
negatif = readLines("train/negTr.csv")
hvz.tr = c(positif, negatif)
length(hvz.tr)
str(hvz.tr)

#input data test
positiftes = readLines("train/posTr.csv")
negatiftes = readLines("train/negTs.csv")
hvz.ts = c(positiftes, negatiftes)
length(hvz.ts)

#menyatukan data
hvz.all = c(hvz.tr,hvz.ts)
length(hvz.all)
str(hvz.all)


#### memberi label sepanjang variabel positif dan negatif
sentiment = c(rep("positif", length(positif) ), 
              rep("negatif", length(negatif)))
sentiment_test = c(rep("positif", length(positiftes) ), 
                   rep("negatif", length(negatiftes)))

sentiment_all = as.factor(c(sentiment, sentiment_test))
str(sentiment_all)
length(sentiment_all)


## membuat matriks document terms
mat = create_matrix(hvz.all, removeStopwords = FALSE, 
                    removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
mat = as.matrix(mat)
#View(mat)

#melihat panjang data
n=length(hvz.all)
n-100


## membuat container text
container <- create_container(mat, sentiment_all, trainSize=1:599,testSize= 600:n, virgin=FALSE)

## train model
model.svm <- train_model(container, 'SVM',kernel='linear')
model.dt <- train_model(container, 'TREE',kernel='linear')
model.ba <- train_model(container, 'BAGGING',kernel='linear')
model.maxent <- train_model(container, 'MAXENT',kernel='linear')

## meramalkan obyek
results.svm <- classify_model(container, model.svm)
results.dt <- classify_model(container, model.dt)
results.ba <- classify_model(container, model.ba)
results.maxent <- classify_model(container, model.maxent)


## membuat tabel akurasi
table(as.character(sentiment_all[600:n]), as.character(results.svm[,1]))
table(as.character(sentiment_all[600:n]), as.character(results.dt[,1]))
table(as.character(sentiment_all[600:n]), as.character(results.ba[,1]))
table(as.character(sentiment_all[600:n]), as.character(results.maxent[,1]))


## melihat akurasi
recall_accuracy(sentiment_all[600:n], results.svm[,1])
recall_accuracy(sentiment_all[600:n], results.dt[,1])
recall_accuracy(sentiment_all[600:n], results.ba[,1])
recall_accuracy(sentiment_all[600:n], results.maxent[,1])

c(results.svm[2,1],hvz.all[600])
results.maxent[2,1]
