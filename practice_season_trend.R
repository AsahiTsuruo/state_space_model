#　季節とトレンド

library(dlm)

data <- log(AirPassengers)
plot(data, type="o", lwd=2)

#　レベルも傾きも、季節変動も、システムノイズ0 観測誤差のみ
#　Step1 
#　モデル作成のための関数を作る
build.4 <- function(theta){
  dlmModPoly(order=2,　dV=exp(theta[1])　,dW=c(0,0))　+
    dlmModSeas(fr=12,　dW=c(0,rep(0,10)),　dV=0)
}

#　Step2
#　MLEでパラメタ推定。
fit.4 <- dlmMLE(
  data,
  parm=dlmMLE(data,parm=c(1),build.4,method="Nelder-Mead")$par,
  build.4,
  method="BFGS"
)
fit.4

# 推定されたパラメタを使ってモデルを作り直す
DLM.4 <- build.4(fit.4$par)
DLM.4
DLM.4$V
DLM.4$F


# Step3
# カルマンフィルター
Filt.4 <- dlmFilter(data, DLM.4)

Filt.4$m

# Step4
# スムージング
Smooth.4 <- dlmSmooth(Filt.4)


# plot

plot(data, col=1, type="o", lwd=1)
lines(dropFirst(Filt.4$m)[, 1] + dropFirst(Filt.4$m)[, 3], col=2, lwd=2)
lines(dropFirst(Smooth.4$s)[, 1] + dropFirst(Smooth.4$s)[, 3], col=4, lwd=2)
legend("bottomright", pch=c(1,NA,NA),
       col=c(1,2,4), lwd=c(1,2,2), legend=c("data","Filter","Smooth"))

par(mfrow=c(3, 1))
#　元データ
plot(data, col=1, type="o", main="data")
#　レベル＋トレンド成分
plot(dropFirst(Smooth.4$s)[, 1], col=4, main="level+trend")
#季節成分
plot(dropFirst(Smooth.4$s)[, 3], col=4, main="seasonal")
par(mfrow=c(1, 1))

#=======================================================================
# システムノイズもパラメタ推定
#　さらに、1959年以降を予測する

#　1959年以降を切り落とす
test.data <- window(data,end=c(1958, 12))
test.data
#　Step1 
#　モデル作成のための関数を作る
build.5<-function(theta){
  dlmModPoly(order=2,dV=exp(theta[1]), dW=c(exp(theta[2]), exp(theta[3])) ) +
    dlmModSeas(fr=12, dW=c(theta[4], rep(0,10)), dV=0)
}

#　Step2
#　MLEでパラメタ推定。
fit.5 <- dlmMLE(
  test.data,
  parm=dlmMLE(test.data,parm=c(0,1,1,1),build.5,method="Nelder-Mead")$par,
  build.5,
  method="BFGS"
)
fit.5

# 推定されたパラメタを使ってモデルを作り直す
DLM.5 <- build.5(fit.5$par)
DLM.5

# Step3
# カルマンフィルター
Filt.5 <- dlmFilter(test.data, DLM.5)

# Step4
# スムージング
Smooth.5 <- dlmSmooth(Filt.5)

# plot

plot(test.data,col=1,type="o")
lines(dropFirst(Filt.5$m)[,1] + dropFirst(Filt.5$m)[,3], col=2)
lines(dropFirst(Smooth.5$s)[,1] + dropFirst(Smooth.5$s)[,3], col=4)
legend(
  "bottomright", pch=c(1,NA,NA),
  col=c(1,2,4), lwd=1, legend=c("data","Filter","Smooth")
)


par(mfrow=c(3,1))
#　元データ
plot(test.data,col=1,type="o", main="data")
#　レベル＋トレンド成分
plot(dropFirst(Smooth.5$s)[,1],col=4, main="level+trend")
#　季節成分
plot(dropFirst(Smooth.5$s)[,3],col=4, main="seasonal")
par(mfrow=c(1,1))


#　予測
Fore <- dlmForecast(Filt.5, nAhead=24, sampleNew=5)

#　予測の答え合わせ
plot(data, type="o")
lines(dropFirst(Smooth.5$s)[,1]+dropFirst(Smooth.5$s)[,3],col=4)
lines(Fore$f,col=2,lwd=2)
legend("bottomright",pch=c(1,NA),col=c(1,2),lwd=c(1,2),legend=c("実測値","予測値"))

# 乱数を使って予測
Line <- function(x){
  lines(x, col=8, type="o")
}

plot(window(test.data,start=c(1957,1)), xlim=c(1957,1961), ylim=c(5.7,6.5), type="o")
lapply(Fore$newObs, Line)
lines(window(data,start=c(1959,1)), col=1,lwd=2)
lines(Fore$f, col=2)
legend("topleft", pch=c(NA,NA,1), col=c(1,2,8), lwd=c(2,1,1), legend=c("実測値","予測値","乱数を使った予測値"))
