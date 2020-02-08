cat( "\014" )
# Clear plots
if( dev.cur() > 1 ) dev.off()
# Clear global workspace
rm( list = ls( envir = globalenv() ), envir = globalenv() )

intercept <- 0
model_int <- SSModel(Nile~SSMtrend(1, Q = 1496) + SSMcustom(Z = 0, T = 1, Q = 0, a1 = intercept, P1inf = 1), H = 15099)

model_int$T
model_int$T[1,2,1] <- 1
model_int$T

out <- KFS(model_int)

set.seed(1)
x1 <- rnorm(100)
x2 <- rnorm(100)
b1 <- 1 + cumsum(rnorm(100,sd = 1))
b2 <- 2 + cumsum(rnorm(100,sd = 0.1))
y <- 1 + b1*x1 + b2*x2 + rnorm(100,sd = 0.1)
q <- diag(NA,2)
q[2,2] <- 0
a1 = matrix(1,nrow = 2)
model <- SSModel(y~SSMregression(~x1+x2,Q=q,remove.intercept = TRUE,state_names = c("b1","b2")),H=0)
model
fit <- fitSSM(model,inits=c(0,0,0),method="BFGS")

model <- fit$model
model$Q
model$H
out <- KFS(model)

ts.plot(out$alphahat[,2],out$alphahat[,3],b1,b2,col = 1:4)
out$alphahat