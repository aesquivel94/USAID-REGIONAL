#  _____          _  _                                                _                           _  _    _     
# |  __ \        (_)| |                                              | |                         (_)| |  | |    
# | |  | |  __ _  _ | | _   _    __ _   ___  _ __    ___  _ __  __ _ | |_  ___   _ __  __      __ _ | |_ | |__  
# | |  | | / _` || || || | | |  / _` | / _ \| '_ \  / _ \| '__|/ _` || __|/ _ \ | '__| \ \ /\ / /| || __|| '_ \ 
# | |__| || (_| || || || |_| | | (_| ||  __/| | | ||  __/| |  | (_| || |_| (_) || |     \ V  V / | || |_ | | | |
# |_____/  \__,_||_||_| \__, |  \__, | \___||_| |_| \___||_|   \__,_| \__|\___/ |_|      \_/\_/  |_| \__||_| |_|
#                        __/ |   __/ |                                                                          
#                       |___/   |___/                                                                           
#            __  __               _                        _             _             
#           |  \/  |             | |                      | |           (_)            
#           | \  / |  __ _  _ __ | | __ ___ __   __   ___ | |__    __ _  _  _ __   ___ 
#           | |\/| | / _` || '__|| |/ // _ \\ \ / /  / __|| '_ \  / _` || || '_ \ / __|
#           | |  | || (_| || |   |   <| (_) |\ V /  | (__ | | | || (_| || || | | |\__ \
#           |_|  |_| \__,_||_|   |_|\_\\___/  \_/    \___||_| |_| \__,_||_||_| |_||___/




#Loading observed data
setwd("C:/Users/lllanos/Desktop/Markov_forecast to daily")
data_daily=read.csv("jagual_huila.csv",header = T)

#Exploring the data with some plots
xax=seq(as.Date(paste("1980/1/1",sep="")), as.Date(paste("2014/12/31",sep="")), "days") #Definir periodo que se desea analizar
xax1=seq(as.Date(paste("1980/1/1",sep="")), as.Date(paste("2014/12/31",sep="")), "month") #Definir periodo que se desea analizar
plot(x=xax,y=data_daily[,4],type="l",ylab="Rain (mm)",xlab="",main="Daily data")
#axis.Date(1, at = xax1, las=2)
plot(x=xax1,data_monthly[,3],type="l",ylab="Rain (mm)",xlab="",main="Monthly data")




#Calculate cumulative rain per month
data_monthly=aggregate(data_daily[,4],list(data_daily$month,data_daily$year),sum,na.rm=T) #Daily to monthly



#Define the trimester to forecast
to_forecast=data_monthly[data_monthly$Group.1==1,]

#Calculate the years with above, normal and below rain (terciles)
terciles=quantile(to_forecast[,3],probs = c(0.3333,0.6666,1))
years_below=to_forecast[which(to_forecast[,3]<=terciles[1]),2]
years_normal=to_forecast[which(to_forecast[,3]>terciles[1] & to_forecast[,3]<=terciles[2]),2]
years_above=to_forecast[which(to_forecast[,3]>terciles[2]),2]


#Filter the daily data for the trimester to forecast in each tercile  
daily_below=data_daily[data_daily$month==unique(to_forecast[,1]) & data_daily$year %in% years_below,]
daily_normal=data_daily[data_daily$month==unique(to_forecast[,1]) & data_daily$year %in% years_normal,]
daily_above=data_daily[data_daily$month==unique(to_forecast[,1]) & data_daily$year %in% years_above,]



#########################################################################################################

#Pass the rain data to binary (0-dry / 1-Wet)
daily_below$bin [daily_below[,4]<=0.5]<-0 #Should we consider dry days with rain less than 0.5mm or 1mm?
daily_below$bin [daily_below[,4]>0.5]<-1


daily_normal$bin [daily_normal[,4]<=0.5]<-0 
daily_normal$bin [daily_normal[,4]>0]<-1


daily_above$bin [daily_above[,4]<=0.5]<-0 
daily_above$bin [daily_above[,4]>0]<-1



#Calculate table of frequency today and yesterday (y=yesterday t=today 0=dry 1=rain)

################################
#Table for BELOW and each month
################################
#Month 1
y0_t0=matrix(NA,31,length(years_below))
y1_t1=matrix(NA,31,length(years_below))
y1_t0=matrix(NA,31,length(years_below))
y0_t1=matrix(NA,31,length(years_below))

for(j in 1:length(years_below)) {
  
  daily_below_y=daily_below[daily_below$year==years_below[j],]
  
  for( i in 1:(nrow(daily_below_y)-1)){
    
    yest=daily_below_y[i,5]
    today=daily_below_y[i+1,5]
    
    if(yest==0 & today==0) y0_t0[i,j]=1
    if(yest==0 & today==1) y0_t1[i,j]=1
    if(yest==1 & today==1) y1_t1[i,j]=1
    if(yest==1 & today==0) y1_t0[i,j]=1
    
  }
}

freq_below=matrix(NA,2,2,dimnames =list(c("Yest_0","Yest_1"),c("Today_0","Today_1")))
freq_below[1,1]=sum(y0_t0,na.rm=T)
freq_below[1,2]=sum(y0_t1,na.rm=T)
freq_below[2,1]=sum(y1_t1,na.rm=T)
freq_below[2,2]=sum(y1_t0,na.rm=T)



################################
#Table for NORMAL and each month
################################
#Month 1
y0_t0=matrix(NA,31,length(years_normal))
y1_t1=matrix(NA,31,length(years_normal))
y1_t0=matrix(NA,31,length(years_normal))
y0_t1=matrix(NA,31,length(years_normal))

for(j in 1:length(years_normal)) {
  
  daily_normal_y=daily_normal[daily_normal$year==years_normal[j],]
  
  for( i in 1:(nrow(daily_normal_y)-1)){
    
    yest=daily_normal_y[i,5]
    today=daily_normal_y[i+1,5]
    
    if(yest==0 & today==0) y0_t0[i,j]=1
    if(yest==0 & today==1) y0_t1[i,j]=1
    if(yest==1 & today==1) y1_t1[i,j]=1
    if(yest==1 & today==0) y1_t0[i,j]=1
    
  }
}

freq_normal=matrix(NA,2,2,dimnames =list(c("Yest_0","Yest_1"),c("Today_0","Today_1")))
freq_normal[1,1]=sum(y0_t0,na.rm=T)
freq_normal[1,2]=sum(y0_t1,na.rm=T)
freq_normal[2,1]=sum(y1_t1,na.rm=T)
freq_normal[2,2]=sum(y1_t0,na.rm=T)



################################
#Table for ABOVE and each month
################################
#Month 1
y0_t0=matrix(NA,31,length(years_above))
y1_t1=matrix(NA,31,length(years_above))
y1_t0=matrix(NA,31,length(years_above))
y0_t1=matrix(NA,31,length(years_above))

for(j in 1:length(years_above)) {
  
  daily_above_y=daily_above[daily_above$year==years_above[j],]
  
  for( i in 1:(nrow(daily_above_y)-1)){
    
    yest=daily_above_y[i,5]
    today=daily_above_y[i+1,5]
    
    if(yest==0 & today==0) y0_t0[i,j]=1
    if(yest==0 & today==1) y0_t1[i,j]=1
    if(yest==1 & today==1) y1_t1[i,j]=1
    if(yest==1 & today==0) y1_t0[i,j]=1
  }
}

freq_above=matrix(NA,2,2,dimnames =list(c("Yest_0","Yest_1"),c("Today_0","Today_1")))
freq_above[1,1]=sum(y0_t0,na.rm=T)
freq_above[1,2]=sum(y0_t1,na.rm=T)
freq_above[2,1]=sum(y1_t1,na.rm=T)
freq_above[2,2]=sum(y1_t0,na.rm=T)


########################################################## 
#Generate synthetic serie  
##########################################################

################################
#Table for ABOVE and each month
################################

months_size=c(31,28,31,30,31,30,31,31,30,31,30,31)

names_tables=expand.grid("freq_",c("below","normal","above"))
names_tables=apply(names_tables, 1, paste, collapse = "")
#names_tables=sort(names_tables)

month.numb=1
month.names=month.name[month.numb]
size=months_size[month.numb[1]]

#Function to generate syntetic serie of 1 and 0 for the first trimester
syn_generator=function(tables,size){
 
  syn=rep(NA,sum(size))
  table_p=eval(parse(text =tables[1]))
  for(i in 1:sum(size)){
    seed=runif(1,0,1)
    
    
    if(i==1) {
      prob=apply(table_p,2,sum)/sum(table_p)
      syn[i]=ifelse(seed>prob[1],1,0)
    }else{
      
      if(syn[i-1]==0){
        prob=table_p[1,]/sum(table_p[1,])
        syn[i]=ifelse(seed>prob[1],1,0)
      }else{
        prob=table_p[2,]/sum(table_p[2,])
        syn[i]=ifelse(seed>prob[1],1,0)
      }
    }
    
  }
  
  return(syn)
}

dates1=seq(as.Date(paste("2014/",month.numb[1],"/1",sep="")), as.Date(paste("2014/",month.numb[1],"/",size[1],sep="")), "days") #Definir periodo que se desea analizar
dates=format(dates1,"%B")


syn_below=syn_generator(names_tables[1],size)
syn_normal=syn_generator(names_tables[2],size)
syn_above=syn_generator(names_tables[3],size)

num_sim=100
for(sim in 1:num_sim){
  syn_below=cbind(syn_below,syn_generator(names_tables[1],size))
  syn_normal=cbind(syn_normal,syn_generator(names_tables[2],size))
  syn_above=cbind(syn_above,syn_generator(names_tables[3],size))
  
}

sum.rain.days=apply(syn_below,2,sum)
to.rm=quantile(sum.rain.days,c(0.1,0.9))
which(sum.rain.days<=to.rm[1])
which(sum.rain.days>=to.rm[2])




rain_below=daily_below[daily_below[,5]==1,4]
rain_normal=daily_normal[daily_normal[,5]==1,4]
rain_above=daily_above[daily_above[,5]==1,4]

library(fitdistrplus)

names_rain=expand.grid("rain_",c("below","normal","above"))
names_rain=apply(names_rain, 1, paste, collapse = "")
#names_rain=sort(names_rain)

#Function to fit a PDF to rainy days
fit_distr=function(x){
  
  month=1
  data=eval(parse(text =x))
  

  fit=fitdist(data,distr = "lnorm")
  fit1=fitdist(data,distr = "gamma")
  fit2=fitdist(data,distr = "weibull")
  
  
  
  png(width =1000 ,height =1000 ,paste("fit",substring(x,6,nchar(x)),month.names[month],".png",sep="_"),pointsize = 20)
  par(mfrow = c(2, 2))
  plot.legend <- c("Lognormal", "Gamma","Weibull")
  
  denscomp(list(fit, fit1, fit2), legendtext = plot.legend,xlab = "Rain (mm)")
  qqcomp(list(fit, fit1, fit2), legendtext = plot.legend)
  cdfcomp(list(fit, fit1, fit2), legendtext = plot.legend,xlab = "Rain (mm)")
  ppcomp(list(fit, fit1, fit2), legendtext = plot.legend)
  
  mtext(paste("Fitting a distribution for days with rain",substring(x,6,nchar(x)),"in",month.names[month],sep=" "), side = 3, line = -1, outer = TRUE,font=2)
  dev.off()
  
  tests=gofstat(list(fit,fit1,fit2),fitnames=c("lnorm","gamma","weibull"))
  model.choose=cbind("Anderson"=names(which.min(tests$ad)),"Cramer-von"=names(which.min(tests$cvm)),"BIC"=names(which.min(tests$bic)),"AIC"=names(which.min(tests$aic)))
  
  
  y=table(model.choose)
  pos=length(which(y==max(y)))
  if(pos==4 || pos==2){
    final=model.choose[1]
  }else{
    final=names(which(y==max(y)))
  }
  
  if(final=="lnorm") param=fit$estimate
  if(final=="gamma") param=fit1$estimate
  if(final=="weibull") param=fit2$estimate
  
  
  to.return=c(x,final,param)
  return(to.return) 
}

models.fit=lapply(1:length(names_rain),function(i) {fit_distr(names_rain[i])})
models.fit1=data.frame(do.call("rbind",models.fit))
models.fit1[,3]=as.numeric(as.character(models.fit1[,3]))
models.fit1[,4]=as.numeric(as.character(models.fit1[,4]))


distr.final_below=models.fit1[grep("below",models.fit1[,1]),]
distr.final_normal=models.fit1[grep("normal",models.fit1[,1]),]
distr.final_above=models.fit1[grep("above",models.fit1[,1]),]

##################################################
#Generate amounts of rain for the syntetics series
##################################################

rain_amount=function(distr,syn){
 
    pos=which(syn==1)
    
    for(k in 1:length(pos)){
      seed=runif(1,0,0.96)
      #if(seed>=0.95) seed=0.95
      
      if(distr[1,2]=="lnorm") amt=qlnorm(seed,distr[1,3],distr[1,4])
      if(distr[1,2]=="gamma") amt=qgamma(seed,distr[1,3],distr[1,4])
      if(distr[1,2]=="weibull") amt=qweibull(seed,distr[1,3],distr[1,4])
      
      syn[pos[k]]=amt
    }
    
  
  return(syn)
}

rain_above_syn=apply(syn_above,2,rain_amount,distr=distr.final_above)
rain_normal_syn=apply(syn_normal,2,rain_amount,distr=distr.final_normal)
rain_below_syn=apply(syn_below,2,rain_amount,distr=distr.final_below)


daily_below$month2=rep(dates1,length(years_below))
daily_normal$month2=rep(dates1,length(years_normal))
daily_above$month2=rep(dates1,length(years_above))

library(ggplot2)
b=ggplot(daily_below,aes(x=month2,y=JaguaLa,colour=as.factor(year)))+geom_line()+ theme_bw()+ylab(" ")+
  xlab("")+scale_y_continuous("",seq(0,100,20),seq(0,100,20),seq(0,100,20),limits=c(0,max(rain_above_syn,rain_normal_syn)))+ labs(title = "Rain Below",colour=" ")+
  theme(legend.position = c(.92, .6))

n=ggplot(daily_normal,aes(x=month2,y=JaguaLa,colour=as.factor(year)))+geom_line()+ theme_bw()+ylab(" ")+
  xlab("")+scale_y_continuous("",seq(0,100,20),seq(0,100,20),seq(0,100,20),limits=c(0,max(rain_above_syn,rain_normal_syn)))+ labs(title = "Rain Normal",colour=" ")+
  theme(legend.position = c(.92, .6))

a=ggplot(daily_above,aes(x=month2,y=JaguaLa,colour=as.factor(year)))+geom_line()+ theme_bw()+ylab(" ")+
  xlab("")+scale_y_continuous("",seq(0,100,20),seq(0,100,20),seq(0,100,20),limits=c(0,max(rain_above_syn,rain_normal_syn)))+ labs(title = "Rain Above",colour=" ")+
  theme(legend.position = c(.92, .6))

library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
x11()
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(b, vp = vplayout(1, 1))
print(n, vp = vplayout(1, 2))
print(a, vp = vplayout(1, 3))


x11()
par(mfrow=c(1,3))
plot(x=dates1,rain_below_syn[,1],type="l",ylim=c(0,max(rain_above_syn,rain_normal_syn)),main="Rain Below",ylab="",xlab="")
for (i in 2:num_sim){
  
  lines(x=dates1,rain_below_syn[,i],type="l")
}
plot(x=dates1,rain_normal_syn[,1],type="l",ylim=c(0,max(rain_above_syn,rain_normal_syn)),main="Rain Normal",ylab="",xlab="")
for (i in 2:num_sim){
  
  lines(x=dates1,rain_normal_syn[,i],type="l")
}

plot(x=dates1,rain_above_syn[,1],type="l",ylim=c(0,max(rain_above_syn,rain_normal_syn)),,main="Rain Above",ylab="",xlab="")
for (i in 2:num_sim){
  
  lines(x=dates1,rain_above_syn[,i],type="l")
}

monthly_above=aggregate(rain_above_syn,list(dates),sum)
monthly_normal=aggregate(rain_normal_syn,list(dates),sum)
monthly_below=aggregate(rain_below_syn,list(dates),sum)
names(monthly_normal)[2]="V1"
names(monthly_above)[2]="V1"
names(monthly_below)[2]="V1"


all_m=cbind("cat"=c(rep("monthly_below",1),rep("monthly_normal",1),rep("monthly_above",1)),rbind(monthly_below,monthly_normal,monthly_above))
library(reshape2)
library(ggplot2)
all_m1=melt(all_m,id.vars = c("cat","Group.1"))
all_m1$cat=factor(all_m1$cat,levels = c("monthly_below","monthly_normal","monthly_above"))
x11()
ggplot(all_m1,aes(x=Group.1,y=value,fill=cat))+geom_boxplot()+ylab("")+xlab("")+theme_bw()+ylim(0,300)
#ggplot(all_m1,aes(x=Group.1,y=value,fill=cat))+hist()+ylab("")+xlab("")+theme_bw()+ylim(0,300)

#ggplot(all_m1,aes(x=cat,y=value,colour=Group.1))+geom_boxplot()



monthly_above.o=aggregate(daily_above[,4],list(daily_above$month,daily_above$year),sum)
monthly_normal.o=aggregate(daily_normal[,4],list(daily_normal$month,daily_normal$year),sum)
monthly_below.o=aggregate(daily_below[,4],list(daily_below$month,daily_below$year),sum)
names(monthly_normal)[2]="V1"
names(monthly_above)[2]="V1"
names(monthly_below)[2]="V1"


all_m=cbind("cat"=c(rep("monthly_below",12),rep("monthly_normal",11),rep("monthly_above",12)),rbind(monthly_below.o,monthly_normal.o,monthly_above.o))
library(reshape2)
library(ggplot2)
#all_m1=melt(all_m,id.vars = c("cat","Group.1"))
all_m$cat=factor(all_m$cat,levels = c("monthly_below","monthly_normal","monthly_above"))
x11()
ggplot(all_m,aes(x=as.factor(Group.1),y=x,fill=cat))+geom_boxplot()+ylab("")+xlab("")+theme_bw()+ylim(0,300)
