# パラメタの設定
# サンプルサイズ
N <- 500
# 観測誤差の標準偏差
observationErrorSD <- 20
# 過程誤差の標準偏差
processErrorSD <- 10
# 係数の過程誤差の標準偏差
coefErrorSD <- 0.5

# 各種シミュレーションデータの生成
# 説明変数をシミュレーションで作る
set.seed(1)
explanatory <- rnorm(n=N, mean=10, sd=10)

# 各種シミュレーションデータの作成
# slopeのシミュレーション
set.seed(12)
slope <- cumsum(rnorm(n=N, mean=0, sd=coefErrorSD)) + 10
plot(slope, main="時間によるslopeの変化", xlab="day", type="l")

# 各種シミュレーションデータの作成

# interceptのシミュレーション
set.seed(3)
intercept <- cumsum(rnorm(n=N, mean=0, sd=processErrorSD))
plot(intercept, main="時間によるinterceptの変化", xlab="day",type="l")


# 各種シミュレーションデータの作成

# responseのシミュレーション
set.seed(4)
response <- intercept + explanatory*slope + rnorm(n=N, mean=0, sd=observationErrorSD)
plot(response, main="responseのシミュレーション結果", xlab="day",type="l")


# パッケージの読み込み
library(dlm)

# モデルの推定

#　Step1 モデルの型を決める
buildDlmReg <- function(theta){
  dlmModReg(
    X=explanatory , 
    dV=exp(theta[1]), 
    dW=c(exp(theta[2]), exp(theta[3]))
  )
}

# モデルの推定
#　Step2　パラメタ推定
fitDlmReg <- dlmMLE(
  response, 
  parm=c(2, 1, 1), 
  buildDlmReg,
  method = "SANN"
)

# モデルの推定
# 推定された分散を使って、モデルを組みなおす
modDlmReg <- buildDlmReg(fitDlmReg$par)

# カルマンフィルター
filterDlmReg <- dlmFilter(response, modDlmReg)
# スムージング
smoothDlmReg <- dlmSmooth(filterDlmReg)

# ==================================================
# 推定結果の図示と確認
# ==================================================

#　推定されたresponseの状態
estimatedLevel <- dropFirst(smoothDlmReg$s)[,1] + explanatory*dropFirst(smoothDlmReg$s)[,2]

# 図示

#　元データ
plot(response, col=1, main="response", pch="。", xlab="day")

# 推定された状態
lines(estimatedLevel, col=4)

# 観測誤差の入っていない、正しい値
lines(intercept + explanatory*slope, col=2)

# 凡例
legend("topleft", legend=c("スムージングの結果","正しい値"), lty=1, col=c(4,2))

# 図示
#　推定された傾き
plot(dropFirst(filterDlmReg$m)[,2], type="l", ylim=c(0, 15), col=2, xlab="day", ylab="slope")
lines(dropFirst(smoothDlmReg$s)[,2], type="l", ylim=c(0, 15), col=4)

# 正しい傾き
lines(slope, col=1)

# 凡例
legend("topleft", legend=c("フィルタリングの結果","スムージングの結果","正しい値"), lty=1, col=c(2,4,1))