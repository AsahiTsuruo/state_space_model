# ローカルレベルモデル
library(dlm)

build.2 <- function(theta){
  dlmModPoly(order=1, dV=exp(theta[1]), dW=exp(theta[2]), m0=exp(theta[3]))
}

#　Step2
#　MLEでパラメタ推定。
fit.2 <- dlmMLE(
  Nile,
  parm=dlmMLE(Nile, parm=c(1,1,10), build.2,method="Nelder-Mead")$par,build.2,
  method="BFGS"
)
fit.2

# 推定されたパラメタを使ってモデルを作り直す
DLM.2 <- build.2(fit.2$par)
DLM.2

# Step3
# カルマンフィルター

Nile.Filt.2 <- dlmFilter(Nile, DLM.2)
Nile.Filt.2$m

# Step4
# スムージング

Nile.Smooth.2 <- dlmSmooth(Nile.Filt.2)

# plot

plot(Nile, col=8, type="o")
lines(Nile.Filt.2$m, col=2, lwd=2)
lines(Nile.Smooth.2$s, col=4, lwd=2)
legend("bottomleft", pch=c(1,NA,NA), col=c(8,2,4), lty=1, lwd=c(1,2,2), legend=c("Nile", "Filter", "Smooth"))



#=====================
# 状態の分散が０
#=====================

#　Step1 
#　モデル作成のための関数を作る
build.3 <- function(theta){
  dlmModPoly(order=1, dV=exp(theta[1]), dW=0,m0=exp(theta[2]))
}

#　Step2
#　MLEでパラメタ推定。

fit.3 <- dlmMLE(
  Nile,
  parm=dlmMLE(Nile,parm=c(1,7),build.3,method="Nelder-Mead")$par,
  build.3,
  method="BFGS"
)
fit.3

# 推定されたパラメタを使ってモデルを作り直す

DLM.3<-build.3(fit.3$par)
DLM.3
DLM.3$m0
DLM.3$V
DLM.3$W


# Step3
# カルマンフィルター

Nile.Filt.3 <- dlmFilter(Nile, DLM.3)
Nile.Filt.3$m

# Step4
# スムージング

Nile.Smooth.3 <- dlmSmooth(Nile.Filt.3)
Nile.Smooth.3$s

# plot

plot(Nile, col=8, type="o", main="スムージングの結果")
lines(Nile.Smooth.3$s, col=4)


#　lmとの比較
lm.model <- lm(Nile ~ 1)
summary(lm.model)

lm.model$coef
Nile.Smooth.3$s

mean(Nile)