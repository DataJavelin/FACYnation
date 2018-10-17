## code to read in monthly sensitivities and analyse their relationship with the underlying climate
rm(list=ls())
## read in sensitivity file
library(RColorBrewer)
library(moments)
require(expm, lib.loc="/home/h06/epope/R_progs/packages")
require(plot3D, lib.loc="/home/h06/epope/R_progs/packages")
library(mclust)
library(fpc)
library(scatterplot3d)
library(copula)
require(VineCopula, lib.loc="/home/h06/epope/R_progs/packages")
require(rgl, lib.loc="/home/h06/epope/R_progs/packages")
require(pvclust, lib.loc="/home/h06/epope/R_progs/packages")
require(sfsmisc)
library(car)

do_copula = 0

crop = "Wheat"
## if wheat, we need to specify whether it's spring or winter
crop_type = "Winter"
##
ave_method = "median"
## number of bins for temperature and precipitation
l = 10
## options are: "Winter (Wheat)", "Spring (Wheat, Maize, Soybean)", "Summer (Maize)", "Early (Rice)", "Mid (Rice)", "Late (Rice)", "NULL" (wheat - specified national values)
## Or specify particular regions
sens_data = read.csv(paste(crop,"_", crop_type,"_",ave_method,"_sensitivity_stats.csv", sep=""), header = TRUE,sep = ",", stringsAsFactors = FALSE)
## wheat
#sens_data = read.csv("Wheat_Spring_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
#sens_data = read.csv("Wheat_Winter_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
## Maize
#sens_data = read.csv("Maize_Spring_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
#sens_data = read.csv("Maize_Summer_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
## Soybean
#sens_data = read.csv("Soybean_Spring_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
## Rice
#sens_data = read.csv("Rice_Early_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
#sens_data = read.csv("Rice_Mid_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
#sens_data = read.csv("Rice_Late_sensitivity_stats.csv", header = TRUE,sep = ",", stringsAsFactors = FALSE)
dir = "/net/home/h06/epope/climate_security/python_code/HCCP/hccp_output/"

region = unique(sens_data$Region)
cntry = sens_data$Country
cntry_set = c("UK", "France", "Germany", "Ireland", "Netherlands", "Denmark", "Poland")
#c("USA", "China")
#c("UK", "France", "Germany", "Ireland", "Netherlands", "Denmark", "Poland")
time_set = 0.5

if (cntry_set != "ALL") {
  cinds = which(is.finite(match(cntry,cntry_set)))
} else {
  cinds = 1:length(sens_data$T_clim)
}

tinds = which(sens_data$fr >= time_set)
#minds = intersect(cinds,tinds)
minds = cinds

## 
t_clim = sens_data$T_clim[minds]
p_clim = sens_data$P_clim[minds]
## fraction of time through growing season
fract = sens_data$fr[minds]

tp_model = lm(p_clim ~ t_clim)

grad = tp_model$coefficients[2]

pal = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd"))

st = sens_data$Temp_sens_scaled[minds]
sp = sens_data$Precip_sens_scaled[minds]

br = 10
colort = pal(br)[as.numeric(cut(st,breaks = br))]
colorp = pal(br)[as.numeric(cut(sp,breaks = br))]

## See if we can extract information about both temperature/precipitation and proportion throught the growing season 
t_seq <- seq(min(t_clim), max(t_clim), length.out=l)
p_seq <- seq(min(p_clim), max(p_clim), length.out=l)
frac_seq = seq(0, 1, length.out=l)

gridt = mesh(t_seq,frac_seq)
gridp = mesh(p_seq,frac_seq)
st_mean = array(0,c((length(t_seq)-1), (length(frac_seq)-1)))
sp_mean = array(0,c((length(p_seq)-1), (length(frac_seq)-1)))
st_stderr = array(0,c((length(t_seq)-1), (length(frac_seq)-1)))
sp_stderr = array(0,c((length(p_seq)-1), (length(frac_seq)-1)))

for (i in 1:(length(t_seq)-1)) {
  for (j in 1:(length(frac_seq)-1)) {
    t_hi = gridt$x[(i+1),1]
    t_lo = gridt$x[i,1]
    p_hi = gridp$x[(i+1),1]
    p_lo = gridp$x[i,1]
    f_hi = gridt$y[1,(j+1)]
    f_lo = gridt$y[1,j]
    finds = which(fract >= f_lo & fract < f_hi)
    pinds = which(p_clim >= p_lo & p_clim < p_hi)
    tinds = which(t_clim >= t_lo & t_clim < t_hi)
    overlapt = intersect(tinds,finds)
    overlapp = intersect(pinds,finds)
    if (length(overlapt) == 0) {
      st_mean[i,j] = 0
      st_stderr[i,j] = 0
    } 
    if (length(overlapp) == 0) {
      sp_mean[i,j] = 0
      sp_stderr[i,j] = 0 
    } 
    if (all(is.na(st[overlapt])) == TRUE) {
      st_mean[i,j] = 0
      st_stderr[i,j] = 0 
    } else {
      st_mean[i,j] = median(st[overlapt], na.rm=TRUE)
      st_stderr[i,j] = sd(st[overlapt], na.rm=TRUE)/sqrt(length(is.finite(st[overlapt])))
    }
    if (all(is.na(sp[overlapp])) == TRUE) {
      sp_mean[i,j] = 0
      sp_stderr[i,j] = 0
    } else {
      sp_mean[i,j] = median(sp[overlapp], na.rm=TRUE)
      sp_stderr[i,j] = sd(sp[overlapp], na.rm=TRUE)/sqrt(length(is.finite(sp[overlapp])))     
    }
  }
}

#dev.new()
#filled.contour(z = st_mean, x = gridt$x[1:(length(t_seq)-1),1],y = gridt$y[1,1:(length(frac_seq)-1)], 
#xlab = "Temperature", ylab = "Proportion through growing season", main = "Temperature Sensitivity",
#color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))

#dev.new()
#scatter2D(t_clim, fract, st)

#dev.new()
#filled.contour(z = sp_mean, x = gridp$x[1:(length(p_seq)-1),1],y = gridp$y[1,1:(length(frac_seq)-1)], 
#xlab = "Precipitation", ylab = "Proportion through growing season", main = "Precipitation Sensitivity",
#color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))

#dev.new()
#scatter2D(p_clim, fract, sp)
#browser()

temp.class <- cut(t_clim, t_seq, include.lowest = TRUE)
temp.mean = tapply(t_clim, temp.class, mean)

precip.class <- cut(p_clim, p_seq, include.lowest = TRUE)
precip.mean = tapply(p_clim, precip.class, mean)

frac.class = cut(fract, frac_seq, include.lowest = TRUE)
frac.mean = tapply(fract, frac.class, mean)

t.mean = tapply(t_clim, frac.class, mean)
p.mean = tapply(p_clim, frac.class, mean)

st.midt <- tapply(st, temp.class, median, na.rm=TRUE)
st.sdt = tapply(st, temp.class, sd, na.rm=TRUE)
st.midp <- tapply(st, precip.class, median, na.rm=TRUE)
st.sdp = tapply(st, precip.class, sd, na.rm=TRUE)

st.midf <- tapply(st, frac.class, median, na.rm=TRUE)
st.sdf = tapply(st, frac.class, sd, na.rm=TRUE)
sp.midf <- tapply(sp, frac.class, median, na.rm=TRUE)
sp.sdf = tapply(sp, frac.class, sd, na.rm=TRUE)

sp.midp <- tapply(sp, precip.class, median, na.rm=TRUE)
sp.sdp = tapply(sp, precip.class, sd, na.rm=TRUE)
sp.midt <- tapply(sp, temp.class, median, na.rm=TRUE)
sp.sdt = tapply(sp, temp.class, sd, na.rm=TRUE)

difft = c(0,diff(t.mean))

#dev.new()
#plot(frac.mean, t.mean, type='l')

#dev.new()
#plot(frac.mean, p.mean, type='l')

#dev.new()
#plot(frac.mean, difft*st.midf)

#dev.new()
#plot(t.mean, st.midf)

#dev.new()
#plot(temp.mean, st.midt)

#dev.new()
#plot(frac.mean, st.midf, type='l')

#dev.new()
#plot(frac.mean, sp.midf, type='l')

#browser()

ttab = table(temp.class, st)
tcounts = apply(ttab, 1, sum)
st.sderrt = sp.sdt/sqrt(tcounts)
ptab = table(precip.class, sp)
pcounts = apply(ptab, 1, sum)
sp.sderrp = sp.sdp/sqrt(pcounts)

st.sderrt[which(!is.finite(st.sderrt))] = 0.0
sp.sderrp[which(!is.finite(sp.sderrp))] = 0.0





colort = pal(br)[as.numeric(cut(10.0*st.midt,breaks = br))]

#mean_model = lm(precip.mean ~ temp.mean)

cum_sp = numeric(0)
cum_st = numeric(0)
cum_st_stderr = numeric(0)
cum_sp_stderr = numeric(0)

## Create the temperautre and precip increments. Estimate the first increment (dT0, dP0)
dT0 = temp.mean[1] - min(t_clim)
dP0 = precip.mean[1] - min(p_clim)
dT = c(dT0, diff(temp.mean))
dP = c(dP0, diff(precip.mean))

for (i in 1:length(sp.midp)) {
  cum_st[i] = sum(st.midt[1:i]*dT[1:i], na.rm=TRUE)
  cum_sp[i] = sum(sp.midp[1:i]*dP[1:i], na.rm=TRUE)
  cum_st_stderr[i] = sqrt(sum((st.sderrt[1:i]*dT[1:i])^2, na.rm=TRUE))
  cum_sp_stderr[i] = sqrt(sum((sp.sderrp[1:i]*dP[1:i])^2, na.rm=TRUE))
}

png(paste(crop,"_",crop_type,"_",ave_method,"_",cntry_set,"_monthly.png", sep=""), width=7.25,height=7.25,units="in",res=300)
par(mfrow=c(2,1),oma=c(0,0,2,0))
#text(0.5,0.5,paste(crop,"-",crop_type, " response functions", sep=""),cex=2,font=2)
plot(temp.mean, cum_st, type='l', xlab=expression(Temperature~(degree*C)), ylab="Yield rate of change (per month)")
arrows(temp.mean, cum_st-cum_st_stderr, temp.mean, cum_st+cum_st_stderr, length=0.05, angle=90, code=3)
abline(h=0, col="blue")
#text(0.1, 0.0,paste(crop,"-",crop_type, " response functions", sep=""),cex=2,font=2)
#lines(temp.mean, predict(tlo), col='red')
plot(precip.mean, cum_sp, type='l', xlab="Mean monthly precipitation (mm)", ylab="Yield rate of change (per month)")
arrows(precip.mean, cum_sp-cum_sp_stderr, precip.mean, cum_sp+cum_sp_stderr, length=0.05, angle=90, code=3)
abline(h=0, col="blue")
#lines(precip.mean[which(is.finite(cum_sp))], predict(plo)[which(is.finite(cum_sp))], col='red')
title(paste(crop," (",crop_type, ") normalised monthly yield response functions", sep=""), outer=TRUE) 
dev.off()

## resample to explore the response function for daily mean temperatures.
ndraws = 50
res = 50
xdim = length(temp.mean)*res
temp = array(0,c(xdim,ndraws))
#t_inc = array(0,c(length(temp.mean),ndraws))
xdim = length(temp.mean)*res
t_response_err = array(0,c(xdim,ndraws))
precip = array(0,c(xdim,ndraws))
p_response_err = array(0,c(xdim,ndraws))

ydotspline_t = spline(temp.mean,cum_st, xout = seq(from=min(t_seq), to=max(t_seq), length.out=res*length(temp.mean)))
ydotspline_tmax = spline(temp.mean,cum_st+cum_st_stderr, xout = seq(from=min(t_seq), to=max(t_seq), length.out=res*length(temp.mean)))
ydotspline_tmin = spline(temp.mean,cum_st-cum_st_stderr, xout = seq(from=min(t_seq), to=max(t_seq), length.out=res*length(temp.mean)))
ydotspline_tdelta = ydotspline_tmax$y - ydotspline_tmin$y

ydotspline_p = spline(precip.mean,cum_sp, xout = seq(from=min(p_seq), to=max(p_seq), length.out=res*length(precip.mean)))
ydotspline_pmax = spline(precip.mean,cum_sp+cum_sp_stderr, xout = seq(from=min(p_seq), to=max(p_seq), length.out=res*length(precip.mean)))
ydotspline_pmin = spline(precip.mean,cum_sp-cum_sp_stderr, xout = seq(from=min(p_seq), to=max(p_seq), length.out=res*length(precip.mean)))
ydotspline_pdelta = ydotspline_pmax$y - ydotspline_pmin$y

#for (i in 1:length(ydotspline_t$x)){ 
#  t_response_err[i,] = rnorm(ndraws, mean=ydotspline_t$y[i] , sd = 0.5*ydotspline_tdelta[i])
#  p_response_err[i,] = rnorm(ndraws, mean=ydotspline_p$y[i] , sd = 0.5*ydotspline_pdelta[i])
#
#}

#ydotspline_tclim = spline(temp.mean,st.midt, xout = t_clim)
#ydotspline_tclim = spline(temp.mean,cum_st, xout = t_clim)
#delta_st = st - ydotspline_tclim$y
#delta_st = st - c(0,diff(ydotspline_tclim$y))
#dev.new()
#plot(ydotspline_tclim$x, ydotspline_tclim$y)
#dst.midt <- tapply(delta_st, precip.class, median, na.rm=TRUE)



#dev.new()
#plot(precip.mean, dst.midt, type='l')



#browser()

#dev.new()
#plot(precip.mean, sp.sderrp/sp.midp)

#dev.new()
#plot(t_clim, delta_st)

#dev.new()
#plot(ydotspline_t$x, t_response_err[,1], xlab="Monthly mean temperature", ylab="Growth rate")
#for (k in 1:res) {
#  points(ydotspline_t$x, t_response_err[,k])
#}
#lines(ydotspline_t$x, ydotspline_t$y, col='blue')
#abline(h=0, col="red")

#dev.new()
#plot(ydotspline_p$x, p_response_err[,1], xlab="Monthly mean precipitation", ylab="Growth rate")
#for (k in 1:res) {
#  points(ydotspline_p$x, p_response_err[,k])
#}
#lines(ydotspline_p$x, ydotspline_p$y, col='blue')
#abline(h=0, col="red")


#png(paste(crop,"_",crop_type,"_",ave_method,"_",cntry_set,"_daily.png", sep=""), width=7.25,height=7.25,units="in",res=300)
#par(mfrow=c(2,1),oma=c(0,0,2,0))
##text(0.5,0.5,paste(crop,"-",crop_type, " response functions", sep=""),cex=2,font=2)
#plot(temp.mean, st.midt*temp.mean, type='l', xlab=expression(Temperature~(degree*C)), ylab=expression(Temperature~response~(1/degree*C)))
#arrows(temp.mean, cum_st-cum_st_stderr, temp.mean, cum_st+cum_st_stderr, length=0.05, angle=90, code=3)
#abline(h=0, col="blue")
#text(0.1, 0.0,paste(crop,"-",crop_type, " response functions", sep=""),cex=2,font=2)
#lines(temp.mean, predict(tlo), col='red')
#plot(precip.mean, sp.midt*precip.mean, type='l', xlab="Mean precipitation (mm)", ylab="Precipitation response (1/mm)")
#arrows(precip.mean, cum_sp-cum_sp_stderr, precip.mean, cum_sp+cum_sp_stderr, length=0.05, angle=90, code=3)
#abline(h=0, col="blue")
#lines(precip.mean[which(is.finite(cum_sp))], predict(plo)[which(is.finite(cum_sp))], col='red')
#title(paste(crop," (",crop_type, ") normalised daily yield response functions", sep=""), outer=TRUE) 
#dev.off()

## create data frame x = month; y = region
region_sub = unique(sens_data$Region[cinds])
mons = unique(sens_data$Month)

tsens_array = array(NA, c(length(region_sub),length(mons)))
psens_array = array(NA, c(length(region_sub),length(mons)))

rownames(tsens_array) = region_sub
colnames(tsens_array) = mons
rownames(psens_array) = region_sub
colnames(psens_array) = mons

for (r in 1:length(region_sub)) {
  for (m in 1:12){
    rinds = which(sens_data$Region == region_sub[r])
    minds = which(sens_data$Month == mons[m])
    overlap = intersect(rinds, minds)
    tsens_array[r,m] = sens_data$Temp_sens_scaled[overlap]
    psens_array[r,m] = sens_data$Precip_sens_scaled[overlap]
  }
}

#fitt = pvclust(t(tsens_array), method.hclust="ward.D",method.dist="euclidean")
#fitp = pvclust(t(psens_array), method.hclust="ward.D",method.dist="euclidean")
#png(paste(crop,"_",crop_type,"_",ave_method,"_",cntry_set,"_temp_dendro.png", sep=""), width=7.25,height=7.25,units="in",res=300)
#plot(fitt)
#pvrect(fitt, alpha=.95) 
#dev.off()

#png(paste(crop,"_",crop_type,"_",ave_method,"_",cntry_set,"_precip_dendro.png", sep=""), width=7.25,height=7.25,units="in",res=300)
#plot(fitp)
#pvrect(fitp, alpha=.95) 

##============================================================
## grid sensitivities to generate p(dy/dT, vs T); p(dy/dP vs P)
## Use these to generate the spread for y vs T; y vs P
## Can we then generate y vs T and P? 
##============================================================

st_seq = seq(floor(5*min(st, na.rm=T))/5, ceiling(5*max(st, na.rm=T))/5, length.out=9) 
sp_seq = seq(floor(10*min(sp, na.rm=T))/10, ceiling(10*max(sp, na.rm=T))/10, length.out=9) 

cond_prob_st_t = array(0, c(length(t_seq)-1, length(st_seq)-1))
cond_prob_st_t_norm = array(0, c(length(t_seq)-1, length(st_seq)-1))
cond_prob_sp_p = array(0, c(length(p_seq)-1, length(sp_seq)-1))
cond_prob_sp_p_norm = array(0, c(length(p_seq)-1, length(sp_seq)-1))

cond_prob_p_t = array(0, c(length(t_seq)-1, length(p_seq)-1))
cond_prob_p_t_norm = array(0, c(length(t_seq)-1, length(p_seq)-1))



n_samples = 10000

boot_t = array(0, c(length(t_seq)-1, n_samples))
boot_p = array(0, c(length(p_seq)-1, n_samples))

##=================================================================
## st
##=================================================================
for (j in 1:(length(t_seq)-1)) {
  t_inds = which(t_clim >= t_seq[j] & t_clim < t_seq[(j+1)])  
  real_inds = which(is.finite(st[t_inds]))
  if (length(real_inds) > 0) {
    boot_t[j,] = sample(st[t_inds][real_inds], n_samples, replace = TRUE)
  } else {
    boot_t[j,] = 0.0
  }  
  for (i in 1:(length(st_seq)-1)) {
    st_inds =  which(st >= st_seq[i] & st < st_seq[(i+1)]) 
    t_st_inds = intersect(t_inds,st_inds)
    sts = st[t_st_inds]
    cond_prob_st_t[j,i] = length(which(is.finite(sts)))
  }
}

for (j in 1:(length(t_seq)-1)) {
  col_sum = sum(cond_prob_st_t[j,])
  if (col_sum != 0.0) {
    cond_prob_st_t_norm[j,] = cond_prob_st_t[j,]/col_sum
  } else {
    cond_prob_st_t_norm[j,] = 0.0
  }
}

cum_cond_prob_st_t = t(apply(cond_prob_st_t_norm, 1, cumsum))
med_st_t = numeric(0)
for (j in 1:(length(t_seq)-1)) {
  med_ind = which(cum_cond_prob_st_t[j,] >= 0.5)
  if (length(med_ind) > 0) {  
    med_st_t[j] = (st_seq[min(med_ind)]+ st_seq[min(med_ind)+1])/2.0
# + sp_seq[min(med_ind)+1] + sp_seq[min(med_ind)-1])/3.0
  } else {
    med_st_t[j] = 0.0
  }
}

t_axis = diff(t_seq)[1] + t_seq[1:(length(t_seq)-1)]
## resample and sum to get ydot and distribution.
ydot_t_boot = apply(boot_t*diff(t_seq)[1], 2, cumsum)
ydot_t_boot_quant = apply(ydot_t_boot, 1, quantile, probs = c(0.05,0.1,0.5,0.9,0.95),  na.rm = TRUE)

dev.new()
plot(0, pch='',xlab='Temperature',ylab='Temperature response', xlim=c(min(t_axis),max(t_axis)), ylim=c(min(ydot_t_boot_quant),max(ydot_t_boot_quant)))
lines(t_axis, ydot_t_boot_quant[4,],lty=2, col='blue')
lines(t_axis, ydot_t_boot_quant[3,], col='black')
lines(t_axis, ydot_t_boot_quant[2,], col='blue',lty=2)
abline(h=0, col='red')

ydot_t_seq = seq(floor(5*min(ydot_t_boot, na.rm=T))/5, ceiling(5*max(ydot_t_boot, na.rm=T))/5, length.out=8) 
cond_prob_yt_t = array(0, c(length(t_seq)-1, length(ydot_t_seq)-1))
cond_prob_yt_t_norm = array(0, c(length(t_seq)-1, length(ydot_t_seq)-1))

cond_prob_yt_t_shift = array(0, c(length(t_seq)-1, length(ydot_t_seq)-1))
cond_prob_yt_t_shift_norm = array(0, c(length(t_seq)-1, length(ydot_t_seq)-1))


for (j in 1:(length(t_seq)-1)) {
  t_inds = which(t_clim >= t_seq[j] & t_clim < t_seq[(j+1)])
  for (i in 1:(length(ydot_t_seq)-1)) {
    yt_inds =  which(ydot_t_boot[j,] >= ydot_t_seq[i] & ydot_t_boot[j,] < ydot_t_seq[(i+1)]) 
    yt_t_inds = intersect(t_inds,yt_inds)
    yts = ydot_t_boot[j,yt_t_inds]
    cond_prob_yt_t[j,i] = length(which(is.finite(yts)))
  }  
}

for (j in 1:(length(t_seq)-1)) {
  col_sum = sum(cond_prob_yt_t[j,])
  if (col_sum != 0.0) {
    cond_prob_yt_t_norm[j,] = cond_prob_yt_t[j,]/col_sum
  } else {
    cond_prob_yt_t_norm[j,] = 0.0
  }
}

##=================================================================
## sp
##=================================================================

for (j in 1:(length(p_seq)-1)) {
  p_inds = which(p_clim >= p_seq[j] & p_clim < p_seq[(j+1)])  
  real_inds = which(is.finite(sp[p_inds]))
  if (length(real_inds) > 0) {
    boot_p[j,] = sample(sp[p_inds][real_inds], n_samples, replace = TRUE)
  } else {
    boot_p[j,] = 0.0
  }  
  for (i in 1:(length(sp_seq)-1)) {
    sp_inds =  which(st >= sp_seq[i] & st < sp_seq[(i+1)]) 
    p_sp_inds = intersect(p_inds,sp_inds)
    sps = sp[p_sp_inds]
    cond_prob_sp_p[j,i] = length(which(is.finite(sps)))
  }
}


for (j in 1:(length(p_seq)-1)) {
  col_sum = sum(cond_prob_sp_p[j,])
  if (col_sum != 0.0) {
    cond_prob_sp_p_norm[j,] = cond_prob_sp_p[j,]/col_sum
  } else {
    cond_prob_sp_p_norm[j,] = 0.0
  }
}

cum_cond_prob_sp_p = t(apply(cond_prob_sp_p_norm, 1, cumsum))
med_sp_p = numeric(0)
for (j in 1:(length(p_seq)-1)) {
  med_ind = which(cum_cond_prob_sp_p[j,] >= 0.5)
  if (length(med_ind) > 0) {  
    med_sp_p[j] = (sp_seq[min(med_ind)]+ sp_seq[min(med_ind)+1])/2.0
# + sp_seq[min(med_ind)+1] + sp_seq[min(med_ind)-1])/3.0
  } else {
    med_sp_p[j] = 0.0
  }
}

p_axis = diff(p_seq)[1] + p_seq[1:(length(p_seq)-1)]
## resample and sum to get ydot and distribution.
ydot_p_boot = apply(boot_p*diff(p_seq)[1], 2, cumsum)
ydot_p_boot_quant = apply(ydot_p_boot, 1, quantile, probs = c(0.05,0.1,0.5,0.9,0.95),  na.rm = TRUE)

dev.new()
plot(0, pch='',xlab='Precipitation',ylab='Precipitation response', xlim=c(min(p_axis),max(p_axis)), ylim=c(min(ydot_p_boot_quant),max(ydot_p_boot_quant)))
lines(p_axis, ydot_p_boot_quant[4,],lty=2, col='blue')
lines(p_axis, ydot_p_boot_quant[3,], col='black')
lines(p_axis, ydot_p_boot_quant[2,], col='blue',lty=2)
abline(h=0, col='red')

ydot_p_seq = seq(floor(5*min(ydot_p_boot, na.rm=T))/5, ceiling(5*max(ydot_p_boot, na.rm=T))/5, length.out=8) 
cond_prob_yp_p = array(0, c(length(p_seq)-1, length(ydot_p_seq)-1))
cond_prob_yp_p_norm = array(0, c(length(p_seq)-1, length(ydot_p_seq)-1))

for (j in 1:(length(p_seq)-1)) {
  p_inds = which(p_clim >= p_seq[j] & p_clim < p_seq[(j+1)])
  for (i in 1:(length(ydot_p_seq)-1)) {
    yp_inds =  which(ydot_p_boot[j,] >= ydot_p_seq[i] & ydot_p_boot[j,] < ydot_p_seq[(i+1)]) 
    yp_p_inds = intersect(p_inds,yp_inds)
    yps = ydot_p_boot[j,yp_p_inds]
    cond_prob_yp_p[j,i] = length(which(is.finite(yps)))
  }  
}

for (j in 1:(length(p_seq)-1)) {
  col_sum = sum(cond_prob_yp_p[j,])
  if (col_sum != 0.0) {
    cond_prob_yp_p_norm[j,] = cond_prob_yp_p[j,]/col_sum
  } else {
    cond_prob_yp_p_norm[j,] = 0.0
  }
}

yt_axis = ydot_t_seq[1:(length(ydot_t_seq)-1)]
yp_axis = ydot_p_seq[1:(length(ydot_p_seq)-1)]

dev.new()
filled.contour(z = cond_prob_yt_t_norm, x = t_axis,y = yt_axis, color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd"))) 

dev.new()
filled.contour(z = cond_prob_yp_p_norm, x = p_axis,y = yp_axis, color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd"))) 

#dev.new()
#filled.contour(z = cond_prob_st_t, x = t_seq[1:(length(t_seq)-1)],y = st_seq[1:(length(st_seq)-1)], 
#xlab = "Temperature", ylab = "Sensitivity", main = "P(ST | T)",
#color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))

#dev.new()
#filled.contour(z = cond_prob_sp_p, x = p_seq[1:(length(p_seq)-1)],y = sp_seq[1:(length(sp_seq)-1)], 
#xlab = "Precip", ylab = "Sensitivity", main = "P(SP | P)",
#color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))

#browser()

yt_area = integrate.xy(t_axis, ydot_t_boot_quant[3,])
yp_area = integrate.xy(p_axis, ydot_p_boot_quant[3,])

cat("=====================================","\n")
cat("Area under ydot(T) =", yt_area, "\n")
cat("Area under ydot(P) =", yp_area, "\n")
cat("=====================================","\n")

## generate condition distribution: p(T | P)


tmodel = lm(p_clim~t_clim)

cinterval = predict(tmodel, interval="c")

sig = 0.5*(cinterval[,"upr"] - cinterval[,"lwr"])

dev.new()
plot(t_clim, p_clim)
lines(t_clim, cinterval[,"lwr"], col='red')
lines(t_clim, cinterval[,"upr"], col='red')

#n_samples_tp = 10000
for (j in 1:(length(t_seq)-1)) {
#  precip = pnorm(tmodel[1]+t_seq[j]*tmodel[2], )
  t_inds = which(t_clim >= t_seq[j] & t_clim < t_seq[(j+1)])  
  mean_sig = mean(sig[t_inds])
  mean_t = mean(t_clim[t_inds])
  precip = rnorm(10000, mean_t*tmodel$coef[2] + tmodel$coef[1], mean_sig)  
  for (i in 1:(length(p_seq)-1)) {
    p_inds =  which(precip >= p_seq[i] & precip < p_seq[(i+1)]) 
#    pt_inds = intersect(t_inds,p_inds)
#    sts = st[t_st_inds]
    cond_prob_p_t[j,i] = length(which(is.finite(p_inds)))
  }
}

for (j in 1:(length(t_seq)-1)) {
  col_sum = sum(cond_prob_p_t[j,])
  if (col_sum != 0.0) {
    cond_prob_p_t_norm[j,] = cond_prob_p_t[j,]/col_sum
  } else {
    cond_prob_p_t_norm[j,] = 0.0
  }
}

dev.new()
filled.contour(z = cond_prob_p_t_norm, x = t_axis,y = p_axis, color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))

## change variable
## simple method
#for (k in 1:X){
#  for (j in 1:(length(t_seq)-1)) {
    
#  }
#}

for (k in 1:(length(ydot_p_seq)-1)){
  for (j in 1:(length(p_seq)-1)) {
#    for (i in 1:(length(p_seq)-1)) {
    print(k)
    print(j)
#    print(cond_prob_yp_p_norm[j,k])
#    print(cond_prob_p_t_norm[j,])
    print((cond_prob_yp_p_norm[j,k]*cond_prob_p_t_norm[j,]))
    cond_prob_yt_t_shift[j,k] = sum(cond_prob_yp_p_norm[j,k]*cond_prob_p_t_norm[j,])
  }
}

for (j in 1:(length(t_seq)-1)) {
  col_sum = sum(cond_prob_yt_t_shift[j,])
  if (col_sum != 0.0) {
    cond_prob_yt_t_shift_norm[j,] = cond_prob_yt_t_shift[j,]/col_sum
  } else {
    cond_prob_yt_t_shift_norm[j,] = 0.0
  }
}

dev.new()
filled.contour(z = cond_prob_yt_t_shift_norm, x = t_axis,y = yt_axis, color = colorRampPalette(brewer.pal(n = 8, name = "YlOrRd")))
##==============================================

if (do_copula == 1) {

u <- pobs(as.matrix(cbind(t_clim, p_clim)))[,1]
v <- pobs(as.matrix(cbind(t_clim, p_clim)))[,2]
selectedCopula <- BiCopSelect(u,v,familyset=NA)
#selectedCopula
#browser()
t.cop <- frankCopula(dim=2)
set.seed(500)
m <- pobs(as.matrix(cbind(t_clim, p_clim)))
fit <- fitCopula(t.cop,m,method='ml')
#coef(fit)
#frankCopula(dim = 2, param = 4.42)
rho <- coef(fit)[1]
#df <- coef(fit)[2]
#dev.new()
#persp(frankCopula(dim=2,rho),dCopula)

u <- rCopula(3965,frankCopula(dim=2,rho))
#dev.new()
#plot(u[,1],u[,2],pch='.',col='blue')
cor(u,method='spearman')

#dev.new()
#plot(t_clim, p_clim)

t_mu = mean(t_clim)
t_sd = sd(t_clim)
p_mu = mean(p_clim)
p_sd = sd(p_clim)

copula_dist <- mvdc(copula=frankCopula(rho,dim=2), margins=c("norm","norm"),
                    paramMargins=list(list(mean=t_mu, sd=t_sd),
                                      list(mean=p_mu, sd=p_sd)))
#gf <- gofCopula(frankCopula(dim = 2), as.matrix(cbind(t_clim,p_clim)), N = 50)

sim <- rMvdc(1000, copula_dist)

plot(t_clim,p_clim,main='Temp/precip')
points(sim[,1],sim[,2],col='red')
legend('bottomright',c('Observed','Simulated'),col=c('black','red'),pch=21)

dev.new()
plot(t_clim, st)

re = which(is.finite(st))

out = smooth.spline(t_clim[re],st[re])

ys = cumsum(out$y)

dev.new()
plot(t_clim, st)
lines(out)

dev.new()
plot(out$x, ys)
}