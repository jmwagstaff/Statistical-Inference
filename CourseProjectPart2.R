## Course Project Part 2.
## The Effect of Vitamin C on Tooth Growth in Guinea Pigs

data(ToothGrowth)
summary(ToothGrowth)
str(ToothGrowth)


dat<-ToothGrowth
dat$dose<-as.factor(dat$dose) # turn dose into factor variable
# First we split the data into groups by dose, so that we can compare method VS and OJ
dat05<-subset(dat, dose %in% c(0.5)) 
dat1<-subset(dat, dose %in% c(1))
dat2<-subset(dat, dose %in% c(2))

## we'll do an independent group t confidence interval.
# The Null hypothesis is that the difference in the means between the two
# groups is zero. 

## group dose 0.5
rbind(
        t.test(len~supp, paired=F, var.equal=T, data=dat05)$conf,
        t.test(len~supp, paired=F, var.equal=F, data=dat05)$conf
)

## group dose 1
rbind(
        t.test(len~supp, paired=F, var.equal=T, data=dat1)$conf,
        t.test(len~supp, paired=F, var.equal=F, data=dat1)$conf
)

## group dose 2
rbind(
        t.test(len~supp, paired=F, var.equal=T, data=dat2)$conf,
        t.test(len~supp, paired=F, var.equal=F, data=dat2)$conf
)

## pvalue summary table
pvalsumm<-data.frame(dose=c(0.5,1,2),supp=c("OJ-VC","OJ-VC","OJ-VC"),
                     pValues=signif(c(t.test(len~supp, paired=F, var.equal=F, data=dat05)$p.value,
                               t.test(len~supp, paired=F, var.equal=F, data=dat1)$p.value,
                               t.test(len~supp, paired=F, var.equal=F, data=dat2)$p.value),
                               3))
pvalsumm

## for pvalues smaller than alpha+5%, we take as significant

## for small dose levels (0.5 and 1), the delivery method has a significant 
# affect on the tooth growth length. In particular, the method ....is more 
# affective. Whereas for large dose (2), the delivery method has no effect i.e.
# both delivery methods are equaly affective.

## next we subset by OJ method, and compare dose groups
datOJ05_1<-subset(dat, supp %in% c("OJ") & dose %in% c("0.5","1"))
datOJ05_2<-subset(dat, supp %in% c("OJ") & dose %in% c("0.5","2"))
datOJ1_2<-subset(dat, supp %in% c("OJ") & dose %in% c("1","2"))
## group supp OJ, 
rbind(
        t.test(len~dose, paired=F, var.equal=F, data=datOJ05_1)$conf,
        t.test(len~dose, paired=F, var.equal=F, data=datOJ05_2)$conf,
        t.test(len~dose, paired=F, var.equal=F, data=datOJ1_2)$conf
)

## next we subset by VC method, and compare dose groups
datVC05_1<-subset(dat, supp %in% c("VC") & dose %in% c("0.5","1"))
datVC05_2<-subset(dat, supp %in% c("VC") & dose %in% c("0.5","2"))
datVC1_2<-subset(dat, supp %in% c("VC") & dose %in% c("1","2"))
## group supp VC, 
rbind(
        t.test(len~dose, paired=F, var.equal=F, data=datVC05_1)$conf,
        t.test(len~dose, paired=F, var.equal=F, data=datVC05_2)$conf,
        t.test(len~dose, paired=F, var.equal=F, data=datVC1_2)$conf
)
t.test(len~dose, paired=F, var.equal=F, data=datVC05_1)$pvalue

pvalsumm2<-data.frame(supp=c("OJ","OJ","OJ","VC","VC","VC"),
                      dose=c("0.5-1","0.5-2","1-2","0.5-1","0.5-2","1-2"),
                     pValues=signif(c(t.test(len~dose, paired=F, var.equal=F, data=datOJ05_1)$p.value,
                               t.test(len~dose, paired=F, var.equal=F, data=datOJ05_2)$p.value,
                               t.test(len~dose, paired=F, var.equal=F, data=datOJ1_2)$p.value,
                               t.test(len~dose, paired=F, var.equal=F, data=datVC05_1)$p.value,
                               t.test(len~dose, paired=F, var.equal=F, data=datVC05_2)$p.value,
                               t.test(len~dose, paired=F, var.equal=F, data=datVC1_2)$p.value)
                               ,3))
pvalsumm2

## we are performing many hypothesis tests...we should correct for that 