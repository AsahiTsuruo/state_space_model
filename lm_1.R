cat( "\014" )
if( dev.cur() > 1 ) dev.off()
rm( list = ls( envir = globalenv() ), envir = globalenv() )


d1 <- read.table("/Users/asahi-ts/Documents/state_space_model/UKdriversKSI.txt",skip=1)
colnames(d1) <- "logKSI"
d1 <- ts(log(d1),start = c(1969),frequency = 12)
#print(d1)
t <- 1:nrow(d1)
#print(t)
#summary(lm(d1~t))
#lm(d1~t)

x <- read.table("/Users/asahi-ts/Documents/state_space_model/logUKpetrolprice.txt",skip=1)
x <- ts(x,start=c(1969),frequency = 12)
#x
lm(d1~x)
matplot(cbind(d1,predict(lm(d1~x),newdata=x)),type='l',lty=1,lwd=2)