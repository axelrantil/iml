library(readr)
crx <- read_csv("~/Projects/iml/exam_exercises/tenta1/crx.csv")

n=dim(crx)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
train=crx[id,]
test=crx[-id,]

library(tree)
treefit <- tree(Class~.,data=train)
plot(treefit)
text(treefit)

train = train[-2,]
treefit <- tree(Class~.,data=train)
plot(treefit)
text(treefit)