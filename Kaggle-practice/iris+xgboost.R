library(xgboost)
iris2 = iris
# �W�[�@�� factor �ܼƷ��@�����ܼ�, �_�h iris ����ɪ�
# �����ܼƳ��O�ƭ��ܼ�, ���ŦX��ȤW�`������ƧκA
iris2$id = factor(sample(c("A","B"),150,replace=T))
library(Matrix)
# ��Ҧ������ܼƳ��ର�x�}���A
xdata = sparse.model.matrix(Species ~.-1, data = iris2)
#ydata = sparse.model.matrix(Species ~., data = iris2)

# Species �����ƥ�
m = nlevels(iris2$Species)
# ��ؼ��ܼ� Species �T�Ӥ����ন 0,1,2. �����q 0 �}�l
Y = as.integer(iris2$Species) - 1
set.seed(12345)
# xgboost �ѼƳ]�w
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m)
result = xgboost(param=param, data=xdata, label=Y, nrounds=20)
#result1 = xgboost(param=param, data=ydata, label=Y, nrounds=20)

# �p��w����
Ypred = predict(result,xdata)
#Ypred1 = predict(result1,ydata)

Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(iris2$Species)
Ypred = levels(iris2$Species)[max.col(Ypred)]
# �V�c�x�}
t0 = table(iris2$Species,Ypred)
t0
# �w�����T�v
sum(diag(t0))/sum(t0)
# �����ܼƭ��n��
imp = xgb.importance(names(iris2[,-5]),model=result)
#imp1 = xgb.importance(names(xdata@Dimnames[[2]]),model=result)
print(imp)

#library(Ckmeans.1d.dp)
xgb.plot.importance(imp)

library(DiagrammeR)
xgb.plot.tree(feature_names=names(iris[,-5]),model=result, n_first_tree=2)