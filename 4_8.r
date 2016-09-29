set.seed(1)
data1 = scan("C:/Users/STD/Downloads/data/data/menchild30bach.dat")
data2 = scan("C:/Users/STD/Downloads/data/data/menchild30nobach.dat")
sy1 = sum(data1) ; n1 = length(data1)
sy2 = sum(data2) ; n2 = length(data2)
a = 2 ; b = 1
theta1.mc = rgamma(5000, sy1+a,n1+b)
theta2.mc = rgamma(5000, sy2+a,n2+b)
y1.mc=rpois(5000,theta1.mc)
y2.mc=rpois(5000,theta2.mc)
ds= 0:11

##4.8(a)
par(mfrow=c(2,1))
plot(ds,(table(c(y1.mc,ds))-1)/length(y1.mc), type="h", lwd=3)
plot(ds,(table(c(y2.mc,ds))-1)/length(y2.mc), type="h", lwd=3)

##4.8(b)
diff_theta = theta1.mc-theta2.mc
diff_y = y1.mc - y2.mc
quantile(diff_theta, c(0.025,0.975))
quantile(diff_y, c(0.025,0.975))

##4.8(c)
par(mfrow=c(1,1))
data.support = seq(0,11,length = 100)
plot(ds,(table(c(data2,ds))-1)/length(data2), type = 'h')
points(data.support,dpois(data.support,1.4),col="gray",lwd=2, type = "h")

##4.8(d)
mc0 = c()
mc1 = c()
for (i in 1:length(theta2.mc)){
  y2d.mc = rpois(218,theta2.mc[i])
  mc0[i] = length(y2d.mc[y2d.mc == 0])
  mc1[i] = length(y2d.mc[y2d.mc == 1])
}
plot(mc0,mc1, ylim = c(45,105))
points(length(data2[data2==0]),length(data2[data2==1]), col = 'red')
