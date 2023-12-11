
##Team PROJECT for MATH 6397 

##Installing and loading necessary packages
#install.packages("caret")
library(caret)
#install.packages('fields')
library(fields)
library(ggplot2)
library(lattice)
library(geoR)


##Loading the dataset of Argo location 2
getwd()
setwd("D:/1data science/6397 spatial statistics/team poject")


load("argo_ref_loc2.rda")
argo_2_data <- do.call(data.frame, argo_ref_loc2)


# check for missing or null values 
colSums(is.na(argo_2_data))
dim(argo_2_data)
summary(argo_2_data)
str(argo_2_data)

#filter out pressure from 0<depth<=600 data
# Consider Pressure to filter based on depth
data <- subset(argo_2_data, Pressure >= 0 & Pressure <= 600)

#attach(data)

#1.spatial domain description for spatial domain,
#Latitude and longitude domain locations,
#negative values indicate for southern latitudes and western longitudes
hist(data$Latitude,include.lowest = TRUE,breaks = 50)
summary(data$Latitude)

# Positive values indicate eastern longitude
eastern_lon<-data[data$Longitude>=0,]
hist(eastern_lon$Longitude,main ='Eastern longitude Range',breaks = 30)
summary(eastern_lon$Longitude)

# Negative values indicate western longitude
western_lon<-data[data$Longitude<=0,]
hist(western_lon$Longitude,main ='Western longitude Range',breaks=30 )
summary(western_lon$Longitude)

#Conversion of negative longitude to positive by +360
data$Longitude <- ifelse(data$Longitude < 0 & data$Longitude> -180, data$Longitude + 360, data$Longitude)
summary(data$Longitude)
hist(data$Longitude,breaks=30,xlim=range(175,195),xlab = 'Longitude',main='Histogram of Longitude')

float_names<-unique(data$ProfileNumber)   # we get 50 unique values
data$ProfileNumber<-as.factor(data$ProfileNumber)

##set aside 5 floats, which should be taken from training set

set.seed(222)
set_aside_5float<-sample(float_names,5)
set_aside_5float
test<-data[data$ProfileNumber%in%set_aside_5float,]
train<-data[!(data$ProfileNumber%in%set_aside_5float),]

# Locations of training and testing sites are shown on spatial maps
# scatter plot
quilt.plot(train$Longitude,train$Latitude,train$SalinityResiduals)
title(main='Spatial map for training sites salinity')
quilt.plot(test$Longitude,test$Latitude,test$SalinityResiduals)
title(main='Spatial map for test sites salinity')


#2.Data Description 
# Create a histogram of temperature and salinity
hist(data$TemperatureResiduals, 
     main = "Histogram of Temperature",
     xlab = "Temperature ",
     ylab = "Frequency",
     col = "yellow",          # Color of bars
     border = "black",      # Color of bar borders
     breaks = 20)           # Number of bins
boxplot(data$TemperatureResiduals,main = 'Boxplot of temperature')
summary(data$TemperatureResiduals)

hist(data$SalinityResiduals,
     main = "Histogram of Salinity ",
     xlab = "Salinity ",
     ylab = "Frequency",
     col = "pink",          # Color of bars
     border = "black",      # Color of bar borders
     breaks = 30)           # Number of bins

boxplot(data$SalinityResiduals,main = "Boxplot of Salinity")
summary(data$SalinityResiduals)

# Create a plot for Temperature vs. Salinity

# histogram of pressure
hist(data$Pressure,main='Histogram of Depth',breaks = 30,xlab = 'Pressure')
summary(data$Pressure)
boxplot(data$Pressure,main = "Boxplot of Depth")


#spatial map of temperature
quilt.plot(data$Longitude,data$Latitude,data$TemperatureResiduals,add.legend = TRUE)
title(main='Spatial Map of Temperature')

#spatial map of salinity
quilt.plot(data$Longitude,data$Latitude,data$SalinityResiduals,add.legend = TRUE)
title(main='Spatial Map of Salinity')


# line plot
tem_line<-ggplot(data=data,aes(x=TemperatureResiduals,y=Pressure,group= ProfileNumber))+
  geom_path(colour=data$ProfileNumber)+
  labs(title = "Temperature", x = "Temperature", y = "Depth") +  # Labels and title
  theme_minimal()  # Apply a minimal theme (you can choose other themes)
tem_line

sal_line<-ggplot(data=data,aes(x=SalinityResiduals,y=Pressure,group= ProfileNumber))+
  geom_path(colour=data$ProfileNumber,size=0.5)+
  labs(title = "Salinity", x = "Salinity", y = "Depth") +  # Labels and title
  theme_minimal()  # Apply a minimal theme (you can choose other themes)
sal_line

##3. covariate plot where salinity is dependent variable,temperature is covariate
plot(data$SalinityResiduals, 
     data$TemperatureResiduals,
     main = "Temperature  vs. Salinity ",
     xlab = "Salinity ",
     ylab = "Temperature ",
     col = "black",      # Color of points
     cex = 1.0)          # Point size
#other covariates
plot(data$Latitude,data$SalinityResiduals,main = 'Latitude & Salinity ',xlab = "Latitude",ylab = 'Salinity')
plot(data$Longitude,data$SalinityResiduals,main='Longitude & Salinity',xlab='longitude',ylab='Salinity')


#4. linear regression on training dataset
lm1<-lm(train$SalinityResiduals~train$TemperatureResiduals)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

#plot of original training data
par(mfrow=c(1,2))
quilt.plot(train$Longitude,train$Latitude,train$SalinityResiduals,add.legend = TRUE)
title(main='Spatial Map of training Salinity')
#plot spatial map of linear regression residual
quilt.plot(train$Longitude,train$Latitude,lm1$residuals)
title(main='Spatial Map of residuals')

par(mfrow=c(1,1))
hist(lm1$residuals,breaks = 30,main = 'Histogram of regression residuals')

############################################part1 end#######################################################

 


#####################################team report part2  9/29/2023

#1. working with residual, plot variogram

loc<-matrix(cbind(train$Longitude,train$Latitude),ncol = 2) # 1226 training locations
loc.test<-matrix(cbind(test$Longitude,test$Latitude),ncol=2) #131 test locations

par(mfrow=c(1,1))
look1<-vgram(loc,lm1$residuals,lon.lat = T,type="variogram")
plot(look1,pch=19,main='Variogram with residuals') #variogram with default bin size ,covariance parameter information come from here

#plot(look1$d,look1$vgram,main='variogram with with residuals point cloud') #variogram cloud

#try different variograma
par(mfrow=c(1,2))
brak11=seq(0,1250,by=50)
look11<-vgram(loc,lm1$residuals,lon.lat = T,type="variogram",breaks=brak11)
plot(look11,pch=19,breaks=brak11,main='variogram with with residuals with bin size of 26',xlim=range(0,1200))

brak12=seq(0,1250,by=120)
look12<-vgram(loc,lm1$residuals,lon.lat = T,type="variogram",breaks=brak12)
plot(look12,pch=19,breaks=brak12,main='variogram with with residuals with bin size of 11',xlim=range(0,1200))
par(mfrow=c(1,1))
?vgram

#boxplot of variogram
brak2=seq(0,1300,by=100)
boxplotVGram(look1)
plot(look1,add=T,type='p',col='red')
getVGMean(look1, breaks =brak2)


#2. original data variogram compare with variogram on residuals
par(mfrow=c(1,2))
look2<-vgram(loc,train$SalinityResiduals,lon.lat = T,type="variogram")
plot(look2,pch=19,main='variogram on original data')

look1<-vgram(loc,lm1$residuals,lon.lat = T,type="variogram")
plot(look1,pch=19,main='Variogram with residuals') #shows more dependency
par(mfrow=c(1,1))



#3 initial kriging on test data(5 set aside floats)

#covariance matrix, exponential function, use guess value from variogram (variance, range,nugget)
#first guess from first variogram plot : 
# variance=0.0062(we get from 0.0206-0.0144),range=930, nugget=0.0144

D1=rdist.earth(loc,loc)  #1226 by 1226
S=exp(-D1/930)*0.0062    #1226 by 1226
diag(S)=diag(S)+0.0143 # nugget

D2=rdist.earth(loc,loc.test) #1226 by 131 
K=exp(-D2/930)*0.0062 

##3.use simple kriging on residual since working on residual which mean = 0 

lambda=solve(S) %*% K #simple krigging
dim(lambda) #1226 x 131 values
colSums(lambda) #should be 1 or close to 1

krig_value<-t(lambda) %*% lm1$residuals  #krigging on residuals
dim(krig_value)

# b0+b1*x ,b0 and b1 is from  regression model, x is test $ temperature,
# this is basically regression on test, results is same as predict(model, newdata)function
summary(lm1)
test.reg<-lm1$coefficients[1] + lm1$coefficients[2] * test$TemperatureResiduals
length(test.reg)

# add krigging value on the test.reg, got predicted test salinity
pred.test<-krig_value+test.reg

#summary all the results
result<-data.frame('krig value'=round(pred.test,4),
                    'true test sali'=round(test$SalinityResiduals,4),
                    "krig vs true_difference"=round(krig_value - test$SalinityResiduals, 4),
                    'regression predict'=round(test.reg,4)
                   )
result

##quantify the results and difference
# compare 3 MSE, 
#MSE1 is predicted value vs true test
#MSE2 is simple regression predict vs true test

MSE1<-mean((pred.test-test$SalinityResiduals)^2)
MSE2<-mean((test.reg-test$SalinityResiduals)^2)

MSE1
RMSE1=sqrt(MSE1)
MSE2
RMSE2=sqrt(MSE2)
RMSE2
MAE1<-mean(abs(pred.test-test$SalinityResiduals))
MAE2<-mean(abs(test.reg-test$SalinityResiduals))

######################################part 2 end


########################team report part3  10/27/2023

#install.packages(sp)


library(geoR)
library(sp)#plot

#part 1.estimate covariance parameters with OLS 
# we use geoR package OLS 

loc<-cbind(train$Longitude,train$Latitude) # 1226 training locations
par(mfrow=c(1,1))
plot(loc,main="training data locations")

?variog
vg=variog(coords = loc,data = lm1$residuals, max.dist = 15)#compute variogram


?variofit
fit1=variofit(vg,weights="equal",cov.model = 'matern',fix.nugget = FALSE,fix.kappa = FALSE)#ols
#"matern", "exponential", "gaussian",

fit1
summary(fit1)

plot(vg)
lines(fit1)
title(main="Empirical and theoretical variogram")

fit1$nugget #nugget
fit1$cov.pars #variance and range 
fit1$kappa  #smoothness

#use GLS equation to get the dated mean parameter BETA,see Lec11 notes page11
res=lm1$residuals

?Matern
S= Matern(D1,range=fit1$cov.pars[2],smoothness =fit1$kappa,phi = fit1$cov.pars[1])#cov matrix
diag(S)=diag(S)+fit1$nugget
dim(S)

M=cbind(rep(1,length(loc[,1])),train$TemperatureResiduals)# M is 1226 by 2 
Z=train$SalinityResiduals

Beta= solve(t(M)%*% solve(S)%*% M ) %*% t(M) %*% solve(S) %*% Z #GLS function to get mean parameters b0 and b1
Beta


#part 2. use MLE AND REML and report parameters

###jittere the location with + runif(r)
set.seed(123)
train$Latitude_j=train$Latitude +runif(length(train$Latitude),min = -0.2, max = 0.2)
train$Longitude_j=train$Longitude + runif(length(train$Longitude),min = -0.2, max = 0.2)

loc_j<-cbind(train$Longitude_j,train$Latitude_j)
v1=train$TemperatureResiduals

#MLE 
?likfit
fit.mle.j=likfit(coords = cbind(train$Longitude_j,train$Latitude_j),data=train$SalinityResiduals,
                 cov.model = 'matern',kappa=1.3,fix.kappa=TRUE, fix.nugget = FALSE,
                 ini.cov.pars = c(1,0.1),trend = trend.spatial(~v1))
fit.mle.j
summary(fit.mle.j) 

#REML
fit.reml.j=likfit(coords=loc_j, data=train$SalinityResiduals,ini.cov.pars=c(1,0.1),
                fix.kappa=TRUE,fix.nugget = FALSE, cov.model='matern',kappa=1.3,
                lik.method="REML",trend=trend.spatial(~v1))
fit.reml.j
#
summary(fit.reml.j)


###########################part3 , krigging results between 3 pairs
# create matrics for kriging 
M=cbind(rep(1,length(loc[,1])),train$TemperatureResiduals)# M is 1226 by 2 
m=rbind(rep(1,length(loc.test[,1])),test$TemperatureResiduals) #m is 131 by 2 

D1=rdist.earth(loc,loc) 
d1=rdist.earth(loc, loc.test) #known data and test location
d1.test=rdist.earth(loc.test,loc.test) # test location



# (1) OLS estimates krigin, do simple kriging because mean of 0
fit1$nugget #nugget
fit1$cov.pars #[1]variance and [2]range 
fit1$kappa  #smoothness

S= Matern(D1,range=fit1$cov.pars[2],smoothness =fit1$kappa,phi = fit1$cov.pars[1])#cov matrix
diag(S)=diag(S)+fit1$nugget

k1=fit1$cov.pars[1]*Matern(d1/fit1$cov.pars[2], smoothness=fit1$kappa) #1226 by 131
k0=fit1$cov.pars[1]*Matern(d1.test/fit1$cov.pars[2], smoothness=fit1$kappa)#131 by 131
diag(k0)=diag(k0)+fit1$nugget

lamba.ols.simple=solve(S)%*% k1 # simple kriging lambda 
colSums(lamba.ols.simple)

z.Krig.ols=t(lamba.ols.simple) %*% train$SalinityResiduals # kriging  value of residuals

# add regression to kriging value same as report 2 did 
summary(lm1)
test.reg<-lm1$coefficients[1] + lm1$coefficients[2] * test$TemperatureResiduals
length(test.reg)

# add krigging value to  the test.reg, got predicted test salinity from ols 
pred.test.ols<-z.Krig.ols + test.reg

#rmse and mae of OLS estimated krig value
rmse.ols=sqrt(mean((pred.test.ols - test$SalinityResiduals)^2))
rmse.ols
mae.ols=mean(abs(pred.test.ols - test$SalinityResiduals))
mae.ols




# (2)use  MLE and REML estimated value to do universal krige
# Kriging on MLE Estimates fit.mle.j
summary(fit.mle.j)
fit.mle.j

?Matern
S2=Matern(D1, range=fit.mle.j$phi, smoothness=fit.mle.j$kappa, phi = fit.mle.j$sigmasq) #1226 by 1226
diag(S2)=diag(S2)+fit.mle.j$tausq

k2=Matern(d1,range=fit.mle.j$cov.pars[2],smoothness = fit.mle.j$kappa,phi=fit.mle.j$cov.pars[1])
k20=Matern(d1.test,range=fit.mle.j$cov.pars[2],smoothness = fit.mle.j$kappa,phi=fit.mle.j$cov.pars[1])
diag(k20)=diag(k20)+fit.mle.j$nugget

# Kriging weight calculation #universal kriging
lambda.mle= ( solve(S2) - solve(S2) %*% M %*% solve( t(M) %*% solve(S2) %*% M)%*% t(M) %*% solve(S2)) %*% k2 + solve(S2) %*% M %*% solve( t(M) %*% solve(S2) %*% M) %*% (m)  
colSums(lambda.mle)
z.Krig.mle=t(lambda.mle) %*% train$SalinityResidual #get krig value,universal kriging already deal with mean estimates, no need to deal with beta

rmse.mle=sqrt(mean((z.Krig.mle - test$SalinityResiduals)^2))
mae.mle=mean(abs(z.Krig.mle-test$SalinityResiduals))
rmse.mle
mae.mle

#kriging on REML estimates fit.reml.j
summary(fit.reml.j)
S3=Matern(D1, range=fit.reml.j$cov.pars[2],smoothness=fit.reml.j$kappa,phi=fit.reml.j$cov.pars[1]) #1226 by 1226
diag(S3)=diag(S3)+fit.reml.j$nugget

k3=Matern(d1, range=fit.reml.j$cov.pars[2],smoothness=fit.reml.j$kappa,phi=fit.reml.j$cov.pars[1]) #1226 by 1226
k30=Matern(d1.test, range=fit.reml.j$cov.pars[2],smoothness=fit.reml.j$kappa,phi=fit.reml.j$cov.pars[1]) #1226 by 1226
diag(k30)=diag(k30)+fit.reml.j$nugget

# Kriging weight calculation #universal kriging
lambda.reml= ( solve(S3) - solve(S3) %*% M %*% solve( t(M) %*% solve(S3) %*% M)%*% t(M) %*% solve(S3)) %*% k3 + solve(S3) %*% M %*% solve( t(M) %*% solve(S3) %*% M) %*% (m)  
colSums(lambda.reml)
z.Krig.reml=t(lambda.reml) %*% train$SalinityResidual #get krig value,universal kriging already deal with mean estimates, no need to deal with beta

rmse.reml=sqrt(mean((z.Krig.reml - test$SalinityResiduals)^2))
mae.reml=mean(abs(z.Krig.reml-test$SalinityResiduals))

# plot krigging value spatial maps for 3 method 
par(mfrow=c(1,1))
quilt.plot(loc.test,pred.test.ols,col=larry.colors(),main="spatial map of total prediction (ols+regression) from OLS")
quilt.plot(loc.test,z.Krig.mle,col=larry.colors(),main="spatial map of prediction from MLE")
quilt.plot(loc.test,z.Krig.reml,col=larry.colors(),main="spatial map of prediction from REML")
quilt.plot(loc.test,test$SalinityResiduals,col=larry.colors(),main="spatial map of true test salinity")




#part4 : MSE from each set of estimation
#MSE1 is predicted value from report2  vs true test #COMPARE rmse and mae from report2 
RMSE1=sqrt(MSE1)
MAE1<-mean(abs(pred.test-test$SalinityResiduals))

results3<-data.frame(
  'method'=c("RMSE","MAE"),
  'ols'=c(rmse.ols,mae.ols),
  'MLE'=c(rmse.mle,mae.mle),
  'REML'=c(rmse.reml,mae.reml),
  'report2'=c(RMSE1,MAE1)
)
results3
# ols estimates got the best results 

# spatial map of krigging values correponding spatial map of  mse lect 8 mse of kriging 

#ols


z.Krig.ols # kriging on residual 
#gamma1=m- t(M) %*% solve(S) %*% k1 #gamma1 is 0 since  on residual,simple kriging
MSE.ols= k0-t(k1) %*% solve(S) %*% k1 #+t(gamma1) %*% solve(t(M) %*% solve(S) %*% M) %*% gamma1
diag(MSE.ols)
par(mfrow=c(1,2))
quilt.plot(loc.test,z.Krig.ols,col=larry.colors(),main="map of kriging residual_OLS")
quilt.plot(loc.test,diag(MSE.ols),col=larry.colors(),main="map of mse of residual_ols")


# plot for reml and mle krigging value and mse

#z.Krig.mle is mle krig value 
gamma2=m- t(M) %*% solve(S2) %*% k2
MSE.mle=k20-t(k2) %*% solve(S2) %*% k2 +t(gamma2) %*% solve(t(M) %*% solve(S2) %*% M) %*% gamma2
diag(MSE.mle)

 #reml
gamma3=m- t(M) %*% solve(S3) %*% k3
MSE.reml=k30-t(k3) %*% solve(S3) %*% k3 +t(gamma3) %*% solve(t(M) %*% solve(S3) %*% M) %*% gamma3
diag(MSE.reml)

par(mfrow=c(1,2))
quilt.plot(loc.test,diag(MSE.mle),col=larry.colors(),main="map of MSE_mle")
quilt.plot(loc.test,diag(MSE.reml),col=larry.colors(),main="map of MSE_reml")











################################################
##########################################################           team report part 4  locally stationary model

#part1. identify subregions.
quilt.plot(data$Longitude,data$Latitude,data$SalinityResiduals,add.legend = TRUE)
title(main='Spatial Map of Salinity')
abline(a=0,b=0)  

#split upper and lower region based on latitude values, for both train and test set.
train_up=train[train$Latitude>0,]
train_lo=train[train$Latitude<0,]

test_up=test[test$Latitude>0,]
test_lo=test[test$Latitude<0,]


#part2. fit locally stationary model with 2 estimated methods: ols and MLE ,estimate covariance and mean parameters

# estimate covariance parameters with OLS 

#(1)upper region

loc_up<-cbind(train_up$Longitude,train_up$Latitude) # 553 upper training locations
plot(loc_up,main="training data upper subregion locations")

lm_up<-lm(train_up$SalinityResiduals~train_up$TemperatureResiduals,data = train_up)#regression on train_up
summary(lm_up)


#?variog #geoR package OLS 
vg_up=variog(coords = loc_up,data =lm_up$residuals, max.dist = 15)#compute variogram

?variofit
fit_up=variofit(vg_up,weights="equal",cov.model = 'matern',fix.nugget = FALSE,fix.kappa = FALSE,ini.cov.pars=c(0.1,1))#ols

fit_up
summary(fit_up)



#(2)lower region
loc_lo<-cbind(train_lo$Longitude,train_lo$Latitude) # 673 lower training locations
plot(loc_lo,main="training data lower subregion locations")

lm_lo<-lm(train_lo$SalinityResiduals~train_lo$TemperatureResiduals,data = train_lo)#regression on train_lo
summary(lm_lo)

vg_lo=variog(coords = loc_lo,data = lm_lo$residuals, max.dist = 15)#compute variogram

fit_lo=variofit(vg_lo,weights="equal",cov.model = 'matern',fix.nugget = FALSE,fix.kappa = FALSE)#ols

fit_lo
summary(fit_lo)




########## mean parameters of b0 and b1 OLS

#mean prameters from ols by GLS
#ols_up
ols_up_b0=coefficients(lm_up)[1]
ols_up_b1=coefficients(lm_up)[2]

#ols_lo
ols_lo_b0=coefficients(lm_lo)[1]
ols_lo_b1=coefficients(lm_lo)[2]






# estimate covariance parameters with MLE

#MLE for upper 
?likfit
set.seed(123)
train_up$Latitude_j=train_up$Latitude +runif(length(train_up$Latitude),min = -0.2, max = 0.2)
train_up$Longitude_j=train_up$Longitude + runif(length(train_up$Longitude),min = -0.2, max = 0.2)

loc_j_up<-cbind(train_up$Longitude_j,train_up$Latitude_j)
v_up=train_up$TemperatureResiduals

fit.mle.up=likfit(coords =loc_j_up,data=train_up$SalinityResiduals,
                 cov.model = 'matern',kappa=1.3,fix.kappa=TRUE, fix.nugget = FALSE,
                 ini.cov.pars = c(1,0.1),trend = trend.spatial(~v_up))
fit.mle.up
summary(fit.mle.up)
fit.mle.up$AIC # -1031.723
fit.mle.up$kappa


#MLE for lower
train_lo$Latitude_j=train_lo$Latitude +runif(length(train_lo$Latitude),min = -0.5, max = 0.2)
train_lo$Longitude_j=train_lo$Longitude + runif(length(train_lo$Longitude),min = -0.5, max = 0.2)

loc_j_lo<-cbind(train_lo$Longitude_j,train_lo$Latitude_j)
v_lo=train_lo$TemperatureResiduals

fit.mle.lo=likfit(coords =loc_j_lo,data=train_lo$SalinityResiduals,
                  cov.model = 'matern',kappa=1.3,fix.kappa=FALSE, fix.nugget = FALSE,
                  ini.cov.pars = c(1,0.1),trend = trend.spatial(~v_lo))
fit.mle.lo
summary(fit.mle.lo)

fit.mle.lo$AIC #-765.3543
fit.mle.j$AIC

#beta mean parameters from MLE get from likfit()
fit.mle.up$beta
fit.mle.lo$beta

#part3  report estimated parameters (mean and covariance parameters) for all sub-regions.
results4=data.frame(method=c("b0","b1","alpha","beta","delta","nu"),
  ols_up=c(ols_up_b0,ols_up_b1,fit_up$cov.pars[1] ,fit_up$cov.pars[2], fit_up$nugget,fit_up$kappa),
  ols_lo=c(ols_lo_b0,ols_lo_b1,fit_lo$cov.pars[1],fit_lo$cov.pars[2],fit_lo$nugget,fit_lo$kappa),
  mle_up=c(fit.mle.up$beta[1],fit.mle.up$beta[2],fit.mle.up$cov.pars[1],fit.mle.up$cov.pars[2],fit.mle.up$nugget,fit.mle.up$kappa),
  mle_lo=c(fit.mle.lo$beta[1],fit.mle.lo$beta[2],fit.mle.lo$cov.pars[1],fit.mle.lo$cov.pars[2],fit.mle.lo$nugget,fit.mle.lo$kappa))
results4=(as.data.frame(t(results4)))
results4



# part4. compare MLE from report 3 in terms of AIC
fit.mle.j$AIC     #AIC from report3 which does not process locally stationary model
fit.mle.up$AIC 
fit.mle.lo$AIC #-765.3543
fit.mle.loc.total=fit.mle.up$AIC+fit.mle.lo$AIC
fit.mle.loc.total
# the report 3 model has AIC -1715.54, and locally stationary model in total have AIC -1793.797 which is lower than isotropica model.



# #####################  part 5, perform kriging for each fits
# test upper : 65; test lower: 66
#train upper :553, lower 673

# following 2 kriging function to get krig lambda coeffciients 
krig=function(par,loc_up,loc_test_up,x0,y0,x1,y1){
  alpha=par[1]
  beta=par[2]
  nu=par[3] #smothness
  delta=par[4] #nugget
  
  D=rdist(loc_up)
  d=rdist(loc_up,loc_test_up)
  d0=rdist(loc_test_up)
  
  M=cbind(rep(1,length(x0)),x0)
  m=cbind(rep(1,length(x1)),x1)
  
  S=Matern(D,range =beta,phi = alpha,smoothness =nu)
  diag(S)=diag(S)+delta
  
  k=Matern(d,range = beta,phi = alpha, smoothness=nu)
  k0=Matern(d0,range =beta, phi = alpha, smoothness=nu)
  diag(k0)=diag(k0)+delta
  
  lambda= ( solve(S) - solve(S) %*% M %*% solve( t(M) %*% solve(S) %*% M)%*% t(M) %*% solve(S)) %*% k + solve(S) %*% M %*% solve( t(M) %*% solve(S) %*% M) %*% t(m)  
  return(lambda)
}

sim_krig=function(par,loc_up,loc_test_up){
  alpha=par[1]
  beta=par[2]
  nu=par[3] #smothness
  delta=par[4] #nugget
  
  D=rdist(loc_up)
  d=rdist(loc_up,loc_test_up)

  S=Matern(D,range =beta,phi = alpha,smoothness =nu)
  diag(S)=diag(S)+delta
  
  k=Matern(d,range = beta,phi = alpha, smoothness=nu)

  lamba=solve(S)%*% k # simple kriging lambda 
  return(lamba)
}


#loc_test divided into upper and lower region

loc_test_up=cbind(test_up$Longitude,test_up$Latitude)
loc_test_lo=cbind(test_lo$Longitude,test_lo$Latitude)

#ols upper
#par=c(alpha, beta,nu,delta)
par=c( 0.001472014 , 1.017093780, 1.009058000,0.008567619) # from ols upper region fit_up
krig1=sim_krig(par,loc_up,loc_test_up)
colSums(krig1)
dim(krig1)

#ols lower
par=c(0.010040950,  8.283924766,  1.000019396,  0.018887461  )
krig2=sim_krig(par,loc_lo,loc_test_lo)
colSums(krig2)
dim(krig2)


#MLE upper universal kriging
par=c( 0.001568973 , 0.036776674, 1.3 ,0.008283706 )# fix kappa=1.3
krig3=krig(par,loc_up,loc_test_up,train_up$TemperatureResiduals,train_up$SalinityResiduals,test_up$TemperatureResiduals,test_up$SalinityResiduals)
colSums(krig3)
dim(krig3)

#MLE lower
par=c(0.003207724,  0.213418083 ,1.993562217, 0.016965301  )
krig4=krig(par,loc_lo,loc_test_lo,train_lo$TemperatureResiduals,train_lo$SalinityResiduals,test_lo$TemperatureResiduals,test_lo$SalinityResiduals)
colSums(krig4)
dim(krig4)





#######################################################
# kriging accuracy  RMSE and MAE compare OLS AND MLE
#1.ols upper
res_ols_upper=t(krig1) %*% train_up$SalinityResiduals # kriging  value of residuals

#mean part, add regression up_mean into up_residual 
mean_up= ols_up_b0+ols_up_b1*test_up$TemperatureResiduals
krig_ols_up=mean_up+res_ols_upper

########################################

#2.ols lower
res_ols_lo=t(krig2)%*%train_lo$SalinityResiduals

#add lo_mean into lo_residual 
mean_lo= ols_lo_b0+ols_lo_b1*test_lo$TemperatureResiduals
krig_ols_lo=mean_lo+res_ols_lo

#############################

#combine ols upper and lower predicted results together
krig.ols.t=rep(NA,length(test$TemperatureResiduals)) #create vector with NA 
krig.ols.t[test$Latitude>0]=krig_ols_up  # for index which test lat>0, assign value up
krig.ols.t[test$Latitude<0]=krig_ols_lo
################################

#comPUTE RMSE and MAE
RMSE.ols.local=sqrt(mean((krig.ols.t-test$SalinityResiduals)^2))
RMSE.ols.local
MAE.ols.local=mean(abs(krig.ols.t-test$SalinityResiduals))
MAE.ols.local


###############################
#3.mle upper
krig_mle_up=t(krig3)%*%train_up$SalinityResiduals

#4,mle lower
krig_mle_lo=t(krig4)%*%train_lo$SalinityResiduals

#combine mle together and compute RMSE AND MAE
krig.mle.t=rep(NA,length(test$TemperatureResiduals))
krig.mle.t[test$Latitude>0]=krig_mle_up
krig.mle.t[test$Latitude<0]=krig_mle_lo

RMSE.mle.local=sqrt(mean((krig.mle.t-test$SalinityResiduals)^2))
RMSE.mle.local
MAE.mle.local=mean(abs(krig.mle.t-test$SalinityResiduals))
MAE.mle.local

#results
results5<-data.frame(
  'method'=c("RMSE","MAE"),
  'ols_local_stationary'=c(RMSE.ols.local,MAE.ols.local),
  'MLE_local_stationary'=c(RMSE.mle.local,MAE.mle.local),
  'ols_report3'=c(rmse.ols,mae.ols),
  'MLE_report3'=c(rmse.mle,mae.mle),
  'REML_report3'=c(rmse.reml,mae.reml),
  'report2'=c(RMSE1,MAE1)
)
results5

#locally staionary results RMSE slightly higher than report3, MAE slightly lower than report3.

