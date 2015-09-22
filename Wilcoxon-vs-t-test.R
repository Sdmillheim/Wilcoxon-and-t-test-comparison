###### n=6 ###### t.test ######
### runs 1,200,000 t-tests
### compares 14,400,000 data points
n=6
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpt6=c()
for (i in 1:120){
 for (j in 1:10000){
     p=t.test(a[j,],c(b[j,]+s[i]))$p.value
 results[j,i]=as.numeric(p<0.05)}
proportionexpt6[i]=(sum(results[,i])/10000)}

###### n=6 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 14,400,000 data points
n=6
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpw6=c()
for (i in 1:120){
 for (j in 1:10000){
     p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
 results[j,i]=as.numeric(p<0.05)}
 proportionexpw6[i]=(sum(results[,i])/10000)}

###### n=18 ###### t.test ######
### runs 1,200,000 t-tests
### compares 60,000,000 data points
n=18
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpt18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionexpt18[i]=(sum(results[,i])/10000)}

###### n=18 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 60,000,000 data points
n=18
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpw18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionexpw18[i]=(sum(results[,i])/10000)}

###### n=54 ###### t.test ######
### runs 1,200,000 t-tests
### compares 120,000,000 data points
n=54
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpt54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionexpt54[i]=(sum(results[,i])/10000)}

###### n=54 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 120,000,000 data points
n=54
a=matrix(rexp(10000*n),10000,n)
b=matrix(rexp(10000*n),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionexpw54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionexpw54[i]=(sum(results[,i])/10000)}

###### plotting ######
mean_shift=s
plot(mean_shift,proportionexpt6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionexpw6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionexpt18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionexpw18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionexpt54,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionexpw54,ylim=c(0,1))

datae=data.frame(proportionexpt6,proportionexpw6,proportionexpt18,proportionexpw18,proportionexpt54,proportionexpw54)
datae$mean_shift=s
datae$power=seq(0,1,1/119)

myplote = ggplot(datae,aes(x=mean_shift,y=power_exponential))
myplote = myplote + geom_line(aes(y = proportionexpt6), data = datae,linetype=2,colour="green",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw6), data = datae,linetype=1,colour="green",size=1)
myplote = myplote + geom_line(aes(y = proportionexpt18), data = datae,linetype=2,colour="blue",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw18), data = datae,linetype=1,colour="blue",size=1)
myplote = myplote + geom_line(aes(y = proportionexpt54), data = datae,linetype=2,colour="red",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw54), data = datae,linetype=1,colour="red",size=1)
myplote


########### Normal Dist #############

###### n=6 ###### t.test ######
### runs 1,200,000 t-tests
### compares 14,400,000 data points
n=6
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormt6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormt6[i]=(sum(results[,i])/10000)}

###### n=6 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 14,400,000 data points
n=6
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormw6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormw6[i]=(sum(results[,i])/10000)}

###### n=18 ###### t.test ######
### runs 1,200,000 t-tests
### compares 60,000,000 data points
n=18
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormt18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormt18[i]=(sum(results[,i])/10000)}

###### n=18 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 60,000,000 data points
n=18
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormw18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormw18[i]=(sum(results[,i])/10000)}

###### n=54 ###### t.test ######
### runs 1,200,000 t-tests
### compares 120,000,000 data points
n=54
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormt54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormt54[i]=(sum(results[,i])/10000)}

###### n=54 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 120,000,000 data points
n=54
a=matrix(rnorm(10000*n,0,1),10000,n)
b=matrix(rnorm(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionnormw54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionnormw54[i]=(sum(results[,i])/10000)}

###### plotting ######
mean_shift=s[-201]
plot(mean_shift,proportionnormt6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionnormw6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionnormt18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionnormw18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionnormt54,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionnormw54,ylim=c(0,1))

myplotn = ggplot(datae,aes(x=mean_shift,y=power_normal))
myplotn = myplotn + geom_line(aes(y = proportionnormt6), data = datae,linetype=2,colour="green",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw6), data = datae,linetype=1,colour="green",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormt18), data = datae,linetype=2,colour="blue",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw18), data = datae,linetype=1,colour="blue",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormt54), data = datae,linetype=2,colour="red",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw54), data = datae,linetype=1,colour="red",size=1)
myplotn


########### Uniform Dist #############

###### n=6 ###### t.test ######
### runs 1,200,000 t-tests
### compares 14,400,000 data points
n=6
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunift6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunift6[i]=(sum(results[,i])/10000)}

###### n=6 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 14,400,000 data points
n=6
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunifw6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunifw6[i]=(sum(results[,i])/10000)}

###### n=18 ###### t.test ######
### runs 1,200,000 t-tests
### compares 60,000,000 data points
n=18
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunift18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunift18[i]=(sum(results[,i])/10000)}

###### n=18 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 60,000,000 data points
n=18
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunifw18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunifw18[i]=(sum(results[,i])/10000)}

###### n=54 ###### t.test ######
### runs 1,200,000 t-tests
### compares 120,000,000 data points
n=54
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunift54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunift54[i]=(sum(results[,i])/10000)}

###### n=54 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 120,000,000 data points
n=54
a=matrix(runif(10000*n,0,1),10000,n)
b=matrix(runif(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportionunifw54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportionunifw54[i]=(sum(results[,i])/10000)}

###### plotting ######
mean_shift=s
plot(mean_shift,proportionunift6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionunifw6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionunift18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionunifw18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionunift54,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportionunifw54,ylim=c(0,1))

myplotu = ggplot(datae,aes(x=mean_shift,y=power_uniform))
myplotu = myplotu + geom_line(aes(y = proportionunift6), data = datae,linetype=2,colour="green",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw6), data = datae,linetype=1,colour="green",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunift18), data = datae,linetype=2,colour="blue",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw18), data = datae,linetype=1,colour="blue",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunift54), data = datae,linetype=2,colour="red",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw54), data = datae,linetype=1,colour="red",size=1)
myplotu

########### Cauchy Dist #############

###### n=6 ###### t.test ######
### runs 1,200,000 t-tests
### compares 14,400,000 data points
n=6
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncaucht6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncaucht6[i]=(sum(results[,i])/10000)}

###### n=6 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 14,400,000 data points
n=6
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncauchw6=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncauchw6[i]=(sum(results[,i])/10000)}

###### n=18 ###### t.test ######
### runs 1,200,000 t-tests
### compares 60,000,000 data points
n=18
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncaucht18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncaucht18[i]=(sum(results[,i])/10000)}

###### n=18 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 60,000,000 data points
n=18
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncauchw18=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncauchw18[i]=(sum(results[,i])/10000)}

###### n=54 ###### t.test ######
### runs 1,200,000 t-tests
### compares 120,000,000 data points
n=54
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncaucht54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=t.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncaucht54[i]=(sum(results[,i])/10000)}

###### n=54 ##### Wilcox.test ######
### runs 1,200,000 Wilcox tests
### compares 120,000,000 data points
n=54
a=matrix(rcauchy(10000*n,0,1),10000,n)
b=matrix(rcauchy(10000*n,0,1),10000,n)
s=seq(0.025,3,0.025)
results=matrix(0,10000,120)
proportioncauchw54=c()
for (i in 1:120){
  for (j in 1:10000){
    p=wilcox.test(a[j,],c(b[j,]+s[i]))$p.value
    results[j,i]=as.numeric(p<0.05)}
  proportioncauchw54[i]=(sum(results[,i])/10000)}

###### plotting ######
mean_shift=s
plot(mean_shift,proportioncaucht6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportioncauchw6,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportioncaucht18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportioncauchw18,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportioncaucht54,ylim=c(0,1))
par(new=TRUE)
plot(mean_shift,proportioncauchw54,ylim=c(0,1))

myplotc = ggplot(datae,aes(x=mean_shift,y=power_cauchy))
myplotc = myplotc + geom_line(aes(y = proportioncaucht6), data = datae,linetype=2,colour="green",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw6), data = datae,linetype=1,colour="green",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncaucht18), data = datae,linetype=2,colour="blue",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw18), data = datae,linetype=1,colour="blue",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncaucht54), data = datae,linetype=2,colour="red",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw54), data = datae,linetype=1,colour="red",size=1)
myplotc

####### graphing ##########
myplotn = ggplot(datae,aes(x=mean_shift,y=power_normal))
myplotn = myplotn + geom_line(aes(y = proportionnormt6), data = datae,linetype=1,colour="dark green",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw6), data = datae,linetype=2,colour="dark green",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormt18), data = datae,linetype=1,colour="dark blue",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw18), data = datae,linetype=2,colour="dark blue",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormt54), data = datae,linetype=1,colour="black",size=1)
myplotn = myplotn + geom_line(aes(y = proportionnormw54), data = datae,linetype=2,colour="black",size=1)
myplotn

myplotu = ggplot(datae,aes(x=mean_shift,y=power_uniform))
myplotu = myplotu + geom_line(aes(y = proportionunift6), data = datae,linetype=1,colour="dark green",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw6), data = datae,linetype=2,colour="dark green",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunift18), data = datae,linetype=1,colour="dark blue",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw18), data = datae,linetype=2,colour="dark blue",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunift54), data = datae,linetype=1,colour="black",size=1)
myplotu = myplotu + geom_line(aes(y = proportionunifw54), data = datae,linetype=2,colour="black",size=1)
myplotu

myplote = ggplot(datae,aes(x=mean_shift,y=power_exponential))
myplote = myplote + geom_line(aes(y = proportionexpt6), data = datae,linetype=1,colour="dark green",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw6), data = datae,linetype=2,colour="dark green",size=1)
myplote = myplote + geom_line(aes(y = proportionexpt18), data = datae,linetype=1,colour="dark blue",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw18), data = datae,linetype=2,colour="dark blue",size=1)
myplote = myplote + geom_line(aes(y = proportionexpt54), data = datae,linetype=1,colour="black",size=1)
myplote = myplote + geom_line(aes(y = proportionexpw54), data = datae,linetype=2,colour="black",size=1)
myplote

myplotc = ggplot(datae,aes(x=mean_shift,y=power_cauchy))
myplotc = myplotc + geom_line(aes(y = proportioncaucht6), data = datae,linetype=1,colour="dark green",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw6), data = datae,linetype=2,colour="dark green",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncaucht18), data = datae,linetype=1,colour="dark blue",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw18), data = datae,linetype=2,colour="dark blue",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncaucht54), data = datae,linetype=1,colour="black",size=1)
myplotc = myplotc + geom_line(aes(y = proportioncauchw54), data = datae,linetype=2,colour="black",size=1)
myplotc = myplotc + scale_fill_discrete(guide="legend")
myplotc

Cauchy="Cauchy"
myplotc = ggplot(data = datae, aes(x = mean_shift,y=power,title=Cauchy)) +
  geom_line(aes(y = proportioncaucht6, colour = "proportioncaucht6"),size=1) +
  geom_line(aes(y = proportioncauchw6, colour = "proportioncauchw6"),linetype=2,size=1) +
  geom_line(aes(y = proportioncaucht18, colour = "proportioncaucht18"),size=1) +
  geom_line(aes(y = proportioncauchw18, colour = "proportioncauchw18"),linetype=2,size=1) +
  geom_line(aes(y = proportioncaucht54, colour = "proportioncaucht54"),size=1) +
  geom_line(aes(y = proportioncauchw54, colour = "proportioncauchw54"),linetype=2,size=1) +
  scale_colour_manual("", 
                      breaks = c("proportioncaucht6", "proportioncauchw6", "proportioncaucht18","proportioncauchw18","proportioncaucht54","proportioncauchw54"),
                      values = c("dark blue", "black", "dark green", "dark blue","black","dark green")) 
myplotc

########################################################################

Normal="Normal Distribution"
myplotn = ggplot(data = datae, aes(x = mean_shift,y=power,title=Normal)) +
  geom_line(aes(y = proportionnormt6, colour = "t-test, n=6"),size=1) +
  geom_line(aes(y = proportionnormw6, colour = "Wilcoxon, n=6"),linetype=2,size=1) +
  geom_line(aes(y = proportionnormt18, colour = "t-test, n=18"),size=1) +
  geom_line(aes(y = proportionnormw18, colour = "Wilcoxon, n=18"),linetype=2,size=1) +
  geom_line(aes(y = proportionnormt54, colour = "t-test, n=54"),size=1) +
  geom_line(aes(y = proportionnormw54, colour = "Wilcoxon, n=54"),linetype=2,size=1) +
  scale_colour_manual("", 
                      breaks = c("t-test, n=6", "Wilcoxon, n=6", "t-test, n=18","Wilcoxon, n=18","t-test, n=54","Wilcoxon, n=54"),
                      values = c("dark blue", "black", "dark green", "dark blue","black","dark green")) 
myplotn

Uniform="Uniform Distribution"
myplotu = ggplot(data = datae, aes(x = mean_shift,y=power,title=Uniform)) +
  geom_line(aes(y = proportionunift6, colour = "t-test, n=6"),size=1) +
  geom_line(aes(y = proportionunifw6, colour = "Wilcoxon, n=6"),linetype=2,size=1) +
  geom_line(aes(y = proportionunift18, colour = "t-test, n=18"),size=1) +
  geom_line(aes(y = proportionunifw18, colour = "Wilcoxon, n=18"),linetype=2,size=1) +
  geom_line(aes(y = proportionunift54, colour = "t-test, n=54"),size=1) +
  geom_line(aes(y = proportionunifw54, colour = "Wilcoxon, n=54"),linetype=2,size=1) +
  scale_colour_manual("", 
                      breaks = c("t-test, n=6", "Wilcoxon, n=6", "t-test, n=18","Wilcoxon, n=18","t-test, n=54","Wilcoxon, n=54"),
                      values = c("dark blue", "black", "dark green", "dark blue","black","dark green")) 
myplotu

Exponential="Exponential Distribution"
myplote = ggplot(data = datae, aes(x = mean_shift,y=power,title=Exponential)) +
  geom_line(aes(y = proportionexpt6, colour = "t-test, n=6"),size=1) +
  geom_line(aes(y = proportionexpw6, colour = "Wilcoxon, n=6"),linetype=2,size=1) +
  geom_line(aes(y = proportionexpt18, colour = "t-test, n=18"),size=1) +
  geom_line(aes(y = proportionexpw18, colour = "Wilcoxon, n=18"),linetype=2,size=1) +
  geom_line(aes(y = proportionexpt54, colour = "t-test, n=54"),size=1) +
  geom_line(aes(y = proportionexpw54, colour = "Wilcoxon, n=54"),linetype=2,size=1) +
  scale_colour_manual("", 
                      breaks = c("t-test, n=6", "Wilcoxon, n=6", "t-test, n=18","Wilcoxon, n=18","t-test, n=54","Wilcoxon, n=54"),
                      values = c("dark blue", "black", "dark green", "dark blue","black","dark green")) 
myplote

Cauchy="Cauchy Distribution"
myplotc = ggplot(data = datae, aes(x = mean_shift,y=power,title=Cauchy)) +
  geom_line(aes(y = proportioncaucht6, colour = "t-test, n=6"),size=1) +
  geom_line(aes(y = proportioncauchw6, colour = "Wilcoxon, n=6"),linetype=2,size=1) +
  geom_line(aes(y = proportioncaucht18, colour = "t-test, n=18"),size=1) +
  geom_line(aes(y = proportioncauchw18, colour = "Wilcoxon, n=18"),linetype=2,size=1) +
  geom_line(aes(y = proportioncaucht54, colour = "t-test, n=54"),size=1) +
  geom_line(aes(y = proportioncauchw54, colour = "Wilcoxon, n=54"),linetype=2,size=1) +
  scale_colour_manual("", 
                      breaks = c("t-test, n=6", "Wilcoxon, n=6", "t-test, n=18","Wilcoxon, n=18","t-test, n=54","Wilcoxon, n=54"),
                      values = c("dark blue", "black", "dark green", "dark blue","black","dark green")) 
myplotc
