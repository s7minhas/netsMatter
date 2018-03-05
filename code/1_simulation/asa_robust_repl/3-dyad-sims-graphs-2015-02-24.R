rm(list=ls())
# Evaluate cross sectional case

resUp.20 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-20.csv")
resUp.50 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-50.csv")
resUp.100 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-100.csv")
resUp.150 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-150.csv")

pdf(file="~/Dropbox/dyadic-variance/pa-submission/replication-files/conv-pic.pdf", height=4, width=8)

par(mfrow=c(1,2))
plot(c(.5,4.5),c(0,1.15), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[0]~(cross~section)))
axis(1, 1:4, c(20,50,100, 150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, resUp.20[,3], add=T, axes=F, outline=F)
boxplot(at=2-.1, resUp.50[,3], add=T, axes=F, outline=F)
boxplot(at=3-.1, resUp.100[,3], add=T, axes=F, outline=F)
boxplot(at=4-.1, resUp.150[,3], add=T, axes=F, outline=F)

boxplot(at=1.1, resUp.20[,5], add=T, axes=F, outline=F, border="gray")
boxplot(at=2.1, resUp.50[,5], add=T, axes=F, outline=F, border="gray")
boxplot(at=3.1, resUp.100[,5], add=T, axes=F, outline=F, border="gray")
boxplot(at=4.1, resUp.150[,5], add=T, axes=F, outline=F, border="gray")

points(1,sd(resUp.20[,1]), pch=18, cex=2)
points(2,sd(resUp.50[,1]), pch=18, cex=2)
points(3,sd(resUp.100[,1]), pch=18, cex=2)
points(4,sd(resUp.150[,1]), pch=18, cex=2)


legend(1, 1.15, pch=c(18,0,0), 
		col=c("black","black","gray"), 
		legend=c("True s.e.","Dyadic-cluster robust s.e. est.","Naive het. robust s.e. est."),
		bty="n", pt.cex=c(2,2,2), cex=.75)

plot(c(.5,4.5),c(0,.6), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[1]~(cross~section)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, resUp.20[,4], add=T, axes=F, outline=F)
boxplot(at=2-.1, resUp.50[,4], add=T, axes=F, outline=F)
boxplot(at=3-.1, resUp.100[,4], add=T, axes=F, outline=F)
boxplot(at=4-.1, resUp.150[,4], add=T, axes=F, outline=F)

boxplot(at=1.1, resUp.20[,6], add=T, axes=F, outline=F, border="gray")
boxplot(at=2.1, resUp.50[,6], add=T, axes=F, outline=F, border="gray")
boxplot(at=3.1, resUp.100[,6], add=T, axes=F, outline=F, border="gray")
boxplot(at=4.1, resUp.150[,6], add=T, axes=F, outline=F, border="gray")

points(1,sd(resUp.20[,2]), pch=18, cex=2)
points(2,sd(resUp.50[,2]), pch=18, cex=2)
points(3,sd(resUp.100[,2]), pch=18, cex=2)
points(4,sd(resUp.150[,2]), pch=18, cex=2)

legend(1, .6, pch=c(18,0,0), 
		col=c("black","black","gray"), 
		legend=c("True s.e.","Dyadic-cluster robust s.e. est.","Naive het. robust s.e. est."),
		bty="n", pt.cex=c(2,2,2), cex=.75)


dev.off()


# Evaluate repeated observations case

resUp.20 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-rep-20.csv")
resUp.50 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-rep-50.csv")
resUp.100 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-rep-100.csv")
resUp.150 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-rep-150.csv")

pdf(file="~/Dropbox/dyadic-variance/pa-submission/replication-files/conv-pic-rep.pdf", height=4, width=8)

par(mfrow=c(1,2))
plot(c(.5,4.5),c(0,1.15), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[0]~(repeated~T==2)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, resUp.20[,3], add=T, axes=F, outline=F)
boxplot(at=2-.1, resUp.50[,3], add=T, axes=F, outline=F)
boxplot(at=3-.1, resUp.100[,3], add=T, axes=F, outline=F)
boxplot(at=4-.1, resUp.150[,3], add=T, axes=F, outline=F)

boxplot(at=1.1, resUp.20[,7], add=T, axes=F, outline=F, border="gray")
boxplot(at=2.1, resUp.50[,7], add=T, axes=F, outline=F, border="gray")
boxplot(at=3.1, resUp.100[,7], add=T, axes=F, outline=F, border="gray")
boxplot(at=4.1, resUp.150[,7], add=T, axes=F, outline=F, border="gray")

points(1,sd(resUp.20[,1]), pch=18, cex=2)
points(2,sd(resUp.50[,1]), pch=18, cex=2)
points(3,sd(resUp.100[,1]), pch=18, cex=2)
points(4,sd(resUp.150[,1], na.rm=T), pch=18, cex=2)

legend(1, 1.15, pch=c(18,0,0), 
		col=c("black","black","gray"), 
		legend=c("True s.e.","Dyadic-cluster robust s.e. est.","Naive cluster robust s.e. est."),
		bty="n", pt.cex=c(2,2,2), cex=.75)

plot(c(.5,4.5),c(0,.6), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[1]~(repeated~T==2)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, resUp.20[,4], add=T, axes=F, outline=F)
boxplot(at=2-.1, resUp.50[,4], add=T, axes=F, outline=F)
boxplot(at=3-.1, resUp.100[,4], add=T, axes=F, outline=F)
boxplot(at=4-.1, resUp.150[,4], add=T, axes=F, outline=F)

boxplot(at=1.1, resUp.20[,8], add=T, axes=F, outline=F, border="gray")
boxplot(at=2.1, resUp.50[,8], add=T, axes=F, outline=F, border="gray")
boxplot(at=3.1, resUp.100[,8], add=T, axes=F, outline=F, border="gray")
boxplot(at=4.1, resUp.150[,8], add=T, axes=F, outline=F, border="gray")

points(1,sd(resUp.20[,2]), pch=18, cex=2)
points(2,sd(resUp.50[,2]), pch=18, cex=2)
points(3,sd(resUp.100[,2]), pch=18, cex=2)
points(4,sd(resUp.150[,2], na.rm=T), pch=18, cex=2)

legend(1, .6, pch=c(18,0,0), 
		col=c("black","black","gray"), 
		legend=c("True s.e.","Dyadic-cluster robust s.e. est.","Naive cluster robust s.e. est."),
		bty="n", pt.cex=c(2,2,2), cex=.75)


dev.off()



# Normsl Shocks with RE comparison
norm.20 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-20.csv")
norm.50 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-50.csv")
norm.100 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-100.csv")
norm.150 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-150.csv")

pdf(file="~/Dropbox/dyadic-variance/pa-submission/replication-files/normal-shocks.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(c(.5,4.5),c(0,1.15), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[0]~(cross~section)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, norm.20[,grep("sehat.1", names(norm.20))], 
			add=T, axes=F, outline=F)
boxplot(at=2-.1, norm.50[,grep("sehat.1", names(norm.50))], 
			add=T, axes=F, outline=F)
boxplot(at=3-.1, norm.100[,grep("sehat.1", names(norm.100))], 
			add=T, axes=F, outline=F)
boxplot(at=4-.1, norm.150[,grep("sehat.1", names(norm.150))], 
			add=T, axes=F, outline=F)			


boxplot(at=1+.1, norm.20[,grep("seHC.1", names(norm.20))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=2+.1, norm.50[,grep("seHC.1", names(norm.50))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=3+.1, norm.100[,grep("seHC.1", names(norm.100))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=4+.1, norm.150[,grep("seHC.1", names(norm.150))], 
			add=T, axes=F, outline=F, border="red")			

points(1, sd(norm.20[,grep("bhat.1", names(norm.20))], na.rm=T), 
		pch=18, cex=2)
points(2, sd(norm.50[,grep("bhat.1", names(norm.50))], na.rm=T), 
		pch=18, cex=2)
points(3, sd(norm.100[,grep("bhat.1", names(norm.100))], na.rm=T), 
		pch=18, cex=2)
points(4, sd(norm.150[,grep("bhat.1", names(norm.150))], na.rm=T), 
		pch=18, cex=2)		

boxplot(at=1+.2, norm.20[,grep("seRE.1", names(norm.20))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=2+.2, norm.50[,grep("seRE.1", names(norm.50))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=3+.2, norm.100[,grep("seRE.1", names(norm.100))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=4+.2, norm.150[,grep("seRE.1", names(norm.150))], 
			add=T, axes=F, outline=F, border="blue")		

points(1.2, sd(norm.20[,grep("bRE.1", names(norm.20))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(2.2, sd(norm.50[,grep("bRE.1", names(norm.50))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(3.2, sd(norm.100[,grep("bRE.1", names(norm.100))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(4.2, sd(norm.150[,grep("bRE.1", names(norm.150))], na.rm=T), 
		pch=18, cex=2, col="blue")		


legend(1, 1.15, pch=c(18,18,0,0,0), 
		col=c("black","blue","black","red","blue"), 
		legend=c("True OLS s.e.","True RE s.e.",
				"Dyadic-cluster robust s.e. est.",
				"Naive cluster robust s.e. est.",
				"RE s.e. est."),
		bty="n", pt.cex=c(2,2,2,2,2), cex=.75)


plot(c(.5,4.5),c(0,1.15), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[1]~(cross~section)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, norm.20[,grep("sehat.2", names(norm.20))], 
			add=T, axes=F, outline=F)
boxplot(at=2-.1, norm.50[,grep("sehat.2", names(norm.50))], 
			add=T, axes=F, outline=F)
boxplot(at=3-.1, norm.100[,grep("sehat.2", names(norm.100))], 
			add=T, axes=F, outline=F)
boxplot(at=4-.1, norm.150[,grep("sehat.2", names(norm.150))], 
			add=T, axes=F, outline=F)			


boxplot(at=1+.1, norm.20[,grep("seHC.2", names(norm.20))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=2+.1, norm.50[,grep("seHC.2", names(norm.50))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=3+.1, norm.100[,grep("seHC.2", names(norm.100))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=4+.1, norm.150[,grep("seHC.2", names(norm.150))], 
			add=T, axes=F, outline=F, border="red")			

points(1, sd(norm.20[,grep("bhat.2", names(norm.20))], na.rm=T), 
		pch=18, cex=2)
points(2, sd(norm.50[,grep("bhat.2", names(norm.50))], na.rm=T), 
		pch=18, cex=2)
points(3, sd(norm.100[,grep("bhat.2", names(norm.100))], na.rm=T), 
		pch=18, cex=2)
points(4, sd(norm.150[,grep("bhat.2", names(norm.150))], na.rm=T), 
		pch=18, cex=2)		

boxplot(at=1+.2, norm.20[,grep("seRE.2", names(norm.20))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=2+.2, norm.50[,grep("seRE.2", names(norm.50))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=3+.2, norm.100[,grep("seRE.2", names(norm.100))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=4+.2, norm.150[,grep("seRE.2", names(norm.150))], 
			add=T, axes=F, outline=F, border="blue")		

points(1.2, sd(norm.20[,grep("bRE.2", names(norm.20))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(2.2, sd(norm.50[,grep("bRE.2", names(norm.50))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(3.2, sd(norm.100[,grep("bRE.2", names(norm.100))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(4.2, sd(norm.150[,grep("bRE.2", names(norm.150))], na.rm=T), 
		pch=18, cex=2, col="blue")		


legend(1, 1.15, pch=c(18,18,0,0,0), 
		col=c("black","blue","black","red","blue"), 
		legend=c("True OLS s.e.","True RE s.e.",
				"Dyadic-cluster robust s.e. est.",
				"Naive cluster robust s.e. est.",
				"RE s.e. est."),
		bty="n", pt.cex=c(2,2,2,2,2), cex=.75)
dev.off()

# Misspecification

miss.20 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-misspec-20.csv")
miss.50 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-misspec-50.csv")
miss.100 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-misspec-100.csv")
miss.150 <- read.csv("~/Dropbox/dyadic-variance/pa-submission/replication-files/simout-misspec-150.csv")

pdf(file="~/Dropbox/dyadic-variance/pa-submission/replication-files/misspec.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(c(.5,4.5),c(0,1), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[0]~(cross~section)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, miss.20[,grep("sehat.irreg.1", names(miss.20))], 
			add=T, axes=F, outline=F)
boxplot(at=2-.1, miss.50[,grep("sehat.irreg.1", names(miss.50))], 
			add=T, axes=F, outline=F)
boxplot(at=3-.1, miss.100[,grep("sehat.irreg.1", names(miss.100))], 
			add=T, axes=F, outline=F)
boxplot(at=4-.1, miss.150[,grep("sehat.irreg.1", names(miss.150))], 
			add=T, axes=F, outline=F)			


boxplot(at=1+.1, miss.20[,grep("seHC.irreg.1", names(miss.20))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=2+.1, miss.50[,grep("seHC.irreg.1", names(miss.50))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=3+.1, miss.100[,grep("seHC.irreg.1", names(miss.100))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=4+.1, miss.150[,grep("seHC.irreg.1", names(miss.150))], 
			add=T, axes=F, outline=F, border="red")			

points(1, sd(miss.20[,grep("bhat.irreg.1", names(miss.20))], na.rm=T), 
		pch=18, cex=2)
points(2, sd(miss.50[,grep("bhat.irreg.1", names(miss.50))], na.rm=T), 
		pch=18, cex=2)
points(3, sd(miss.100[,grep("bhat.irreg.1", names(miss.100))], na.rm=T), 
		pch=18, cex=2)
points(4, sd(miss.150[,grep("bhat.irreg.1", names(miss.150))], na.rm=T), 
		pch=18, cex=2)		

boxplot(at=1+.2, miss.20[,grep("seRE.irreg.1", names(miss.20))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=2+.2, miss.50[,grep("seRE.irreg.1", names(miss.50))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=3+.2, miss.100[,grep("seRE.irreg.1", names(miss.100))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=4+.2, miss.150[,grep("seRE.irreg.1", names(miss.150))], 
			add=T, axes=F, outline=F, border="blue")		

points(1.2, sd(miss.20[,grep("bre.irreg.1", names(miss.20))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(2.2, sd(miss.50[,grep("bre.irreg.1", names(miss.50))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(3.2, sd(miss.100[,grep("bre.irreg.1", names(miss.100))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(4.2, sd(miss.150[,grep("bre.irreg.1", names(miss.150))], na.rm=T), 
		pch=18, cex=2, col="blue")		


legend(1, 1, pch=c(18,18,0,0,0), 
		col=c("black","blue","black","red","blue"), 
		legend=c("True OLS s.e.","True RE s.e.",
				"Dyadic-cluster robust s.e. est.",
				"Naive cluster robust s.e. est.",
				"RE s.e. est."),
		bty="n", pt.cex=c(2,2,2,2,2), cex=.75)


plot(c(.5,4.5),c(0,.6), type="n", 
	axes=F, xlab="N", ylab="Standard error",
	main=bquote(s.e.~hat(beta)[1]~(cross~section)))
axis(1, 1:4, c(20,50,100,150))
axis(2, seq(0,1.2,.2))
box()

boxplot(at=1-.1, miss.20[,grep("sehat.irreg.2", names(miss.20))], 
			add=T, axes=F, outline=F)
boxplot(at=2-.1, miss.50[,grep("sehat.irreg.2", names(miss.50))], 
			add=T, axes=F, outline=F)
boxplot(at=3-.1, miss.100[,grep("sehat.irreg.2", names(miss.100))], 
			add=T, axes=F, outline=F)
boxplot(at=4-.1, miss.150[,grep("sehat.irreg.2", names(miss.150))], 
			add=T, axes=F, outline=F)			


boxplot(at=1+.1, miss.20[,grep("seHC.irreg.2", names(miss.20))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=2+.1, miss.50[,grep("seHC.irreg.2", names(miss.50))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=3+.1, miss.100[,grep("seHC.irreg.2", names(miss.100))], 
			add=T, axes=F, outline=F, border="red")
boxplot(at=4+.1, miss.150[,grep("seHC.irreg.2", names(miss.150))], 
			add=T, axes=F, outline=F, border="red")			

points(1, sd(miss.20[,grep("bhat.irreg.2", names(miss.20))], na.rm=T), 
		pch=18, cex=2)
points(2, sd(miss.50[,grep("bhat.irreg.2", names(miss.50))], na.rm=T), 
		pch=18, cex=2)
points(3, sd(miss.100[,grep("bhat.irreg.2", names(miss.100))], na.rm=T), 
		pch=18, cex=2)
points(4, sd(miss.150[,grep("bhat.irreg.2", names(miss.150))], na.rm=T), 
		pch=18, cex=2)		

boxplot(at=1+.2, miss.20[,grep("seRE.irreg.2", names(miss.20))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=2+.2, miss.50[,grep("seRE.irreg.2", names(miss.50))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=3+.2, miss.100[,grep("seRE.irreg.2", names(miss.100))], 
			add=T, axes=F, outline=F, border="blue")
boxplot(at=4+.2, miss.150[,grep("seRE.irreg.2", names(miss.150))], 
			add=T, axes=F, outline=F, border="blue")		

points(1.2, sd(miss.20[,grep("bre.irreg.2", names(miss.20))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(2.2, sd(miss.50[,grep("bre.irreg.2", names(miss.50))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(3.2, sd(miss.100[,grep("bre.irreg.2", names(miss.100))], na.rm=T), 
		pch=18, cex=2, col="blue")
points(4.2, sd(miss.150[,grep("bre.irreg.2", names(miss.150))], na.rm=T), 
		pch=18, cex=2, col="blue")		


legend(1, .6, pch=c(18,18,0,0,0), 
		col=c("black","blue","black","red","blue"), 
		legend=c("True OLS s.e.","True RE s.e.",
				"Dyadic-cluster robust s.e. est.",
				"Naive cluster robust s.e. est.",
				"RE s.e. est."),
		bty="n", pt.cex=c(2,2,2,2,2), cex=.75)
dev.off()

# Misspec graph


N <- 50
index <- 1:N
P <- choose(N,2)
dyads <-  as.matrix(apply(t(combn(index, 2)), 1,
					function(x)paste(x[1],
					x[2],sep="-")))

dyad.mat <- t(apply(dyads, 1, function(x)unlist(strsplit(x,"-"))))
a <- rnorm(N)
X <- rnorm(N)

# Misspec

dX <- da <- NA
for(i in 1:P){
da[i] <- sum(a[as.numeric(dyad.mat[i,])])
dX[i] <- abs(diff(X[as.numeric(dyad.mat[i,])]))
}

dY <- da + dX + .25*dX^2 + rnorm(P)
pdf(file="~/Dropbox/dyadic-variance/pa-submission/replication-files/misspec-scatterplot.pdf", height=6, width=8)
plot(dX, dY, pch=19, cex=.5, xlab="|Xi-Xj|", ylab="Y")
abline(lm(dY~dX), col="red")
dX.ord <- dX[order(dX)]
dY.ord <- dY[order(dX)]
points(dX.ord, predict(lm(dY.ord~dX.ord+I(dX.ord^2))), type="l", col="blue")
dev.off()
