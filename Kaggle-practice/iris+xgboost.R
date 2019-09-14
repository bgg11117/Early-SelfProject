library(xgboost)
iris2 = iris
# 增加一個 factor 變數當作解釋變數, 否則 iris 資料檔的
# 解釋變數都是數值變數, 不符合實務上常見的資料形態
iris2$id = factor(sample(c("A","B"),150,replace=T))
library(Matrix)
# 把所有解釋變數都轉為矩陣型態
xdata = sparse.model.matrix(Species ~.-1, data = iris2)
#ydata = sparse.model.matrix(Species ~., data = iris2)

# Species 分類數目
m = nlevels(iris2$Species)
# 把目標變數 Species 三個分類轉成 0,1,2. 必須從 0 開始
Y = as.integer(iris2$Species) - 1
set.seed(12345)
# xgboost 參數設定
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m)
result = xgboost(param=param, data=xdata, label=Y, nrounds=20)
#result1 = xgboost(param=param, data=ydata, label=Y, nrounds=20)

# 計算預測值
Ypred = predict(result,xdata)
#Ypred1 = predict(result1,ydata)

Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(iris2$Species)
Ypred = levels(iris2$Species)[max.col(Ypred)]
# 混淆矩陣
t0 = table(iris2$Species,Ypred)
t0
# 預測正確率
sum(diag(t0))/sum(t0)
# 解釋變數重要性
imp = xgb.importance(names(iris2[,-5]),model=result)
#imp1 = xgb.importance(names(xdata@Dimnames[[2]]),model=result)
print(imp)

#library(Ckmeans.1d.dp)
xgb.plot.importance(imp)

library(DiagrammeR)
xgb.plot.tree(feature_names=names(iris[,-5]),model=result, n_first_tree=2)
