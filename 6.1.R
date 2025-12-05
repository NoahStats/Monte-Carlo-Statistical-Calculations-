library(tidyverse)
set.seed(1234)
theta0 = 2.0
x = rnorm(1) + theta0
M = 1e3

# MH with a proposal dist Q = N(x,1)
prop = rnorm(M) + x 
u = runif(M)
theta_vec = numeric(M)
theta = rnorm(1)
theta_vec[1] = theta 
for(i in 2:M){
    if(u[i] <= (1+theta^2)/(1+prop[i]^2)){
        theta = prop[i]
    }
    theta_vec[i] = theta
}

data.fr = data.frame(x = 1:M, theta = theta_vec)

index = (1:M)[diff(theta_vec)==0]
data.fr = data.frame(x = index, theta = theta_vec[index])

ggplot(data.fr, mapping = aes(x = x, y = theta))+geom_line(col = 'blue') + geom_point(aes(x = x, y = theta), data = data.fr, 
col = 'black', size = 2, alpha = 0.5) + theme_bw()

