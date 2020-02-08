cat( "\014" )
# Clear plots
if( dev.cur() > 1 ) dev.off()
# Clear global workspace
rm( list = ls( envir = globalenv() ), envir = globalenv() )

library(KFAS)
library(ggplot2)

nile_train <- window(Nile, end = 1950)
nile_train[41:60] <- NA

build_kfas <- SSModel(
  H = NA,
  nile_train ~ SSMtrend(degree = 1, Q = NA)
)

fit_kfas <- fitSSM(build_kfas, init=c(1,1))

result_kfas <- KFS(
  fit_kfas$model,
  filtering = c("state","mean"),
  smoothing = c("state","mean")
)

print(fit_kfas$model$H)
print(fit_kfas$model$Q)

mu_filter_kfas <- result_kfas$a[-1]
mu_smooth_kfas <- result_kfas$alphahat

df_filter <- data.frame(
  y = as.numeric(Nile[1:80]),
  time = 1871:1950,
  mu_filter = mu_filter_kfas
)

ggplot(data=df_filter, aes(x=time,y=y)) + 
  labs(title="filtered eitimation") + 
  geom_point(alpha=0.6) + 
  geom_line(aes(y = mu_filter),size=1.2)
mu_smooth_kfas