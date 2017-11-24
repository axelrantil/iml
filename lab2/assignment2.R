library(readxl)
creditscoring <- read_excel("~/Projects/iml/lab2/creditscoring.xls")

n=dim(creditscoring)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=creditscoring[id,]

testval=creditscoring[-id,]
n=dim(testval)[1]
id=sample(1:n, floor(n*0.5))
test=testval[id,]
val=testval[-id,]

