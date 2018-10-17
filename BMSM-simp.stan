data {
int<lower=0> n_regions;
int<lower=0> n_years;
real d_temp[n_regions,n_years,12];
real d_yields[n_regions,n_years];
}

parameters {
real s_temp[n_regions,12];
}

//transformed parameters {

//vector[n_region] sigma[n_regions];

//for (n in 1:n_regions){
//sigma[n]=sqrt(sum(square(s_temp[n])*variance(d_temp[n])));

//}


//}

model {
real tmp;
for (n in 1:n_regions){
for (m in 1:12){
s_temp[n,m] ~normal(0.0,100.0);
}
}
for (n in 1:n_regions){
for (y in 1:n_years){
tmp=0.0;
for (m in 1:12){
tmp=tmp+s_temp[n,m]*d_temp[n,y,m];
}
d_yields[n,y]~normal(tmp,1.0);
}
}
}


generated quantities {
real d_yields_pred[n_regions,n_years];
real tmp;
for (n in 1:n_regions){
for (y in 1:n_years){
tmp=0.0;
for (m in 1:12){
tmp=tmp+s_temp[n,m]*d_temp[n,y,m];
}
d_yields_pred[n,y]=normal_rng(tmp,1.0);
}
}
}