jonesmodel <- function(acc,eventyear){
deltasales <- subset(acc$sales,acc$year>head(acc$year,n=1)) - subset(acc$sales,acc$year<tail(acc$year,n=1))
deltasales <- data.frame(deltasales)
acc <- acc[c(-1),]
names(deltasales) <- c("delta.sales")
acc <- cbind(acc,deltasales)
accruals <- data.frame(acc$net.income-acc$ex.revenue+acc$ex.loss-acc$cf)
names(accruals) <- c("accruals")
acc <- cbind(acc,accruals)
accbefore <- subset(acc,acc$year<eventyear)
accafter <- subset(acc,acc$year>=eventyear)
jones <- lm(accbefore$accruals ~ accbefore$delta.sales+accbefore$tangible.assets)
intce <- jones$coefficients["(Intercept)"]
salcoef <- jones$coefficients["accbefore$delta.sales"]
tangcoef <- jones$coefficients["accbefore$tangible.assets"]
nda <- acc$delta.sales*salcoef+acc$tangible.assets+intce
ndadata <- data.frame(nda)
names(ndadata) <- c("NDA")
acc <- cbind(acc,ndadata)
da <- data.frame(acc$accruals-nda)
names(da) <- c("discreationary")
acc <- cbind(acc,da)
dabefore <- subset(acc$discreationary,acc$year<eventtime)
daafter <- subset(acc$discreationary,acc$year>=eventtime)
hp1ttest <- t.test(daafter, alternative = c("two.sided"),mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
hp1pvalue <- data.frame(hp1ttest$p.value)
acc <- cbind(acc,hp1pvalue)
print(acc)
}