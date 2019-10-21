"
This was code I used for a Statistics class while I was analyzing economics data on Okun's law. 
I also included the final report to help show how I was using this code. While not in the report,
I wanted to do some time series analysis to show that I have hadsome experience with this so at the 
end of the file I perform a basic ARIMA analysis of U3. For a briefer summary of the project than what
is included in the final report, I was looking at Okun'slaw which states how unemployment and GDP move together
and exploring what happens if we use alternate measures of unemployment. The data is time series and we did
not cover that inthis statistics class so I did not do anything to correct for this in the initial project, despite
it definitely needing it.
"

library(ggplot2)
library(readxl)
library(faraway)
library(scales)

#Calls in data
Okunsdata <- read_excel("Documents/Okun's law.xlsx")
Okunsdata<-as.data.frame(Okunsdata)

#Plots Unemployment over time
ggplot(data=Okunsdata)+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U1,color="U1"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U1,color="U1"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U2,color="U2"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U2,color="U2"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U3,color="U3"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U3,color="U3"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U4,color="U4"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U4,color="U4"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U5,color="U5"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U5,color="U5"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$U6,color="U6"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$U6,color="U6"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$Natural,color="Natural Rate"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$Natural,col="Natural Rate"))+
  scale_colour_manual(values=c("black","red","blue","green","yellow","pink","white"))+
  labs(x="Date",y="Unemployment Rates")

#Plots potential and current GDP over time. (The 1.0526 is to adjust for inflation as the potential data is chained in 2009 dollars instead of 2012)
ggplot(data=Okunsdata)+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$POT*1.0526,color="Potential"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$POT*1.0526,color="Potential"))+
  geom_point(aes(x=Okunsdata$DATE, y=Okunsdata$REAL,color="Current"))+
  geom_line(aes(x=Okunsdata$DATE, y=Okunsdata$REAL,color="Current"))+
  scale_colour_manual(values=c("black","red"))+
  labs(x="Date",y="Output")

# Gives the plots for the regression for each alternate measure of unemployment with Short Run Output
for(i in 1:6){
  uvals<-Okunsdata[181:278,i+6] #picks the measure of unemployment
  reg<-summary(lm(Okunsdata$SHORTGDP[181:278]~uvals)) #Does the regression (181 is the start of all variables being tracked)
  #Adds in the caption containing data
  caption<-c(paste("Short Run Output~",round(reg$coefficients[1,1],2),round(reg$coefficients[2,1],2),"*U",i,
             "\n95% CI of Slope: (",round(reg$coefficients[2,1]+qt(0.025,96)*reg$coefficients[2,2],2),", ",round(reg$coefficients[2,1]+qt(0.975,96)*reg$coefficients[2,2],2),")",
             "\nP-value~",scientific(reg$coefficients[2,4], digits = 3),
             "\nR-squared=",round(reg$r.squared,2),
             "\nRMSE=",round(reg$sigma,2),sep=""))
  #Makes the plot
  print(ggplot()+
    geom_point(aes(x=uvals,y=Okunsdata$SHORTGDP[181:278]))+
    labs(x=paste("U",i,sep=""),y="Short run Ouput")+
    geom_abline(aes(intercept=reg$coefficients[1,1],slope=reg$coefficients[2,1]))+
    geom_label(aes(x=max(uvals)*0.8+min(uvals)*0.2,y=0.3,label=caption),size=4.5)+
    theme(text = element_text(size=15)))
}

#Calculates the deltas
Okunsdata$delta2[181:278]<-Okunsdata$U2[181:278]-Okunsdata$U1[181:278]
Okunsdata$delta3[181:278]<-Okunsdata$U3[181:278]-Okunsdata$U2[181:278]
Okunsdata$delta4[181:278]<-Okunsdata$U4[181:278]-Okunsdata$U3[181:278]
Okunsdata$delta5[181:278]<-Okunsdata$U5[181:278]-Okunsdata$U4[181:278]
Okunsdata$delta6[181:278]<-Okunsdata$U6[181:278]-Okunsdata$U5[181:278]

#Regression for the deltas
summary(lm(Okunsdata$SHORTGDP[181:278]~Okunsdata$U1[181:278]+Okunsdata$delta2[181:278]+Okunsdata$delta3[181:278]+Okunsdata$delta4[181:278]+Okunsdata$delta5[181:278]+Okunsdata$delta6[181:278]))

#Calculates thhe VIF
vif(lm(Okunsdata$SHORTGDP[181:278]~Okunsdata$U1[181:278]+Okunsdata$delta2[181:278]+Okunsdata$delta3[181:278]+Okunsdata$delta4[181:278]+Okunsdata$delta5[181:278]+Okunsdata$delta6[181:278]))

#Runs the BIC's
Uregfull <- lm(SHORTGDP[181:278]~Okunsdata$U1[181:278]+Okunsdata$U2[181:278]+Okunsdata$U3[181:278]+Okunsdata$U4[181:278]+Okunsdata$U5[181:278]+Okunsdata$U6[181:278],data=Okunsdata)
backBIC <- step(Uregfull,direction="backward", data=bridge, k=log(n))
Uregmin <- lm(SHORTGDP[181:278]~1,data=Okunsdata)
forwardBIC <- step(Uregmin,scope=list(lower=~1, 
                                        upper=~Okunsdata$U1[181:278]+Okunsdata$U2[181:278]+Okunsdata$U3[181:278]+Okunsdata$U4[181:278]+Okunsdata$U5[181:278]+Okunsdata$U6[181:278]),
                   direction="forward", k=log(n),data=Okunsdata)

#Final output
summary(lm(Okunsdata$SHORTGDP[181:278] ~ Okunsdata$U2[181:278] + Okunsdata$U1[181:278] + 
     Okunsdata$U6[181:278]))

#Predictions
Okunsdata$predictions[181:278]<-predict(lm(Okunsdata$SHORTGDP[181:278] ~ Okunsdata$U2[181:278] + Okunsdata$U1[181:278] + 
             Okunsdata$U6[181:278]))

#VIF
vif(lm(Okunsdata$SHORTGDP[181:278] ~ Okunsdata$U2[181:278] + Okunsdata$U1[181:278] + 
        Okunsdata$U6[181:278]))

#Predicted vs Actual
ggplot()+
  geom_point(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$SHORTGDP[181:278],color="Short Run Output"))+
  geom_line(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$SHORTGDP[181:278],color="Short Run Output"))+
  geom_point(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$predictions[181:278],color="Predicted"))+
  geom_line(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$predictions[181:278],color="Predicted"))+
  scale_colour_manual(values=c("black","red"))+
  labs(x="Date",y="Short Run Output")

#Residuals
ggplot()+
  geom_point(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$SHORTGDP[181:278]-Okunsdata$predictions[181:278]))+
  geom_line(aes(x=Okunsdata$DATE[181:278], y=Okunsdata$SHORTGDP[181:278]-Okunsdata$predictions[181:278]))+
  labs(x="Date",y="Residuals")

#Shapiro test
shapiro.test(Okunsdata$SHORTGDP[181:278]-Okunsdata$predictions[181:278])

#Autocorrelation of Residuals
restplus1<-Okunsdata$SHORTGDP[182:278]-Okunsdata$predictions[182:278]
rest<-Okunsdata$SHORTGDP[181:277]-Okunsdata$predictions[181:277]
reg<-summary(lm(restplus1~rest))
caption<-c(paste("Res. in time t+1~",round(reg$coefficients[1,1],2),"+",round(reg$coefficients[2,1],2),"*Res. in time t",
                 "\n95% CI of Slope: (",round(reg$coefficients[2,1]+qt(0.025,96)*reg$coefficients[2,2],2),", ",round(reg$coefficients[2,1]+qt(0.975,96)*reg$coefficients[2,2],2),")",
                 "\nP-value~",scientific(reg$coefficients[2,4], digits = 3),
                 "\nR-squared=",round(reg$r.squared,2),
                 "\nRMSE=",round(reg$sigma,2),sep=""))

#Plots autocorrelation
ggplot()+
  geom_point(aes(x=rest, y=restplus1))+
  labs(x="Residual in Time t",y="Residual in Time t+1")+
  geom_abline(aes(intercept=reg$coefficients[1,1],slope=reg$coefficients[2,1]))+
  geom_label(aes(x=min(rest)*0.8+max(rest)*0.2,y=0.85,label=caption),size=4.5)

#Okun's law over time
func1<-function(start,finish){
  reg<-summary(lm(Okunsdata$SHORTGDP[start:finish]~Okunsdata$deltaU[start:finish]))
  caption<-c(paste("Short Run Output~",round(reg$coefficients[1,1],2),round(reg$coefficients[2,1],2),"*(U3-UNat.)",
                   "\n95% CI of Slope: (",round(reg$coefficients[2,1]+qt(0.025,96)*reg$coefficients[2,2],2),", ",round(reg$coefficients[2,1]+qt(0.975,96)*reg$coefficients[2,2],2),")",
                   "\nP-value~",scientific(reg$coefficients[2,4], digits = 3),
                   "\nR-squared=",round(reg$r.squared,2),
                   "\nRMSE=",round(reg$sigma,2),sep=""))
  print(ggplot()+
          geom_point(aes(x=Okunsdata$deltaU[start:finish],y=Okunsdata$SHORTGDP[start:finish]))+
          labs(x=paste("Difference between U3 and Natural Rate from ",round(start/4+1949,0)," to ",round(finish/4+1949,0),sep=""),y="Short run Ouput")+
          geom_abline(aes(intercept=reg$coefficients[1,1],slope=reg$coefficients[2,1]))+
          geom_label(aes(x=max(Okunsdata$deltaU[start:finish])*0.75+min(Okunsdata$deltaU[start:finish])*0.25,y=max(Okunsdata$SHORTGDP[start:finish])*0.85+min(Okunsdata$SHORTGDP[start:finish])*0.15,label=caption),size=4.5)+
          theme(text = element_text(size=15)))
}

#Picks the time periods
func1(1,278)
func1(1,50)
func1(51,152)
func1(153,278)

#Time Series Analysis
library(tseries)
library(forecast)
Udata<-Okunsdata$U3
adf.test(Udata) #Running a Dickey-Fuller test for stationarity we see that we should take a difference to make it stationary
plot(Udata) #Further illustration of non-stationarity
Udatadiff<-diff(Udata,differences=1) 
adf.test(Udatadiff) #Here we actually end up with a worse value for stationarity, but when we plot it, it seems much better so we decide to stick with it
plot(Udatadiff)
acf(Udata) #we now take the ACF and PACF plots to see the autocorrelation. This suggests an arima of 0,1,2.
pacf(Udata)
arima(Udata,c(0,1,2))
fit<-auto.arima(Udata,max.p=20,max.q=20) #We also use auto.arima to try to find a bettermodel and it appears that we can use some of the ar terms once we take into account the ma.
plot(forecast(fit,h=20)) #Here we see our forecast for the unemployment rate over the next 5 years. It expects it to hover around 3.7 which is what it has done since I orginally downloaded the data(we have gone through 5 of the 20 forecasted quarters).
#However we do see that it has a very unrealistic CI as we will never hit negative unemployment. This shows how it is important to have an inutition behind our modeling instead of blindly applying statistical techniques.