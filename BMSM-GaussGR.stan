functions {

real yield(real[] temp, real mu, real sigma, real norm){
    real dy[6];
    for (i in 1:6){
    dy[i]=norm*exp(-0.5*square((temp[i]-mu)/sigma));
    }
    return sum(dy);
}
}

data {
int<lower=0> n_regions;
int<lower=0> n_years;
real d_temp[n_regions,n_years,6];
real d_yields[n_regions,n_years];
int n_gf;
real temp[n_gf];
}



parameters {
real mu;
real<lower=0.0> sigma;
real<lower=0.0> norm;

}

model {
real tmp;
mu ~ normal(20,5);
sigma ~ normal(5,1);
norm ~ normal(1,3);
for (n in 1:n_regions){
for (y in 1:n_years){
d_yields[n,y]~normal(yield(d_temp[n,y,:],mu, sigma, norm),1.0);
}}
}


generated quantities {
real fdy[n_gf];
real pred_yields[n_regions,n_years];
for (i in 1:n_gf){
fdy[i]=norm*exp(-0.5*square((temp[i]-mu)/sigma));
 }
for (n in 1:n_regions){
for (y in 1:n_years){
pred_yields[n,y]=normal_rng(yield(d_temp[n,y,:],mu, sigma, norm),1.0);
}
}}