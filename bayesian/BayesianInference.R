library(Rcpp)

# ESTIMATION OF A MEAN
data(iris)

# PARAMETRIC FREQUENTIST WITH MAXIMUM
# Suppose X1,..X150 is a sample from X~N(mu,sigma2) and that 
# want to estimate theta = (mu, sigma2). 
# Suppose we want to know the mean of sepal length
samp <- iris$Sepal.Width
hist(samp)
N <- length(samp)


# APPROACH 1) POINTWISE ESTIMATORS AND PIVOTAL QUANTITIES
est.mean <- sum(samp)/N # mean(samp)
est.sd <- sqrt(sum((samp-est.mean)^2)/(N-1)) # var(samp)

# We can prove that (sqrt(N)/est.sd)(bar(X) -  mu) ~ t-STUDENT_(N-1) degrees.
# So we can make CONFIDENCE INTERVALS using the quantiles of a t-student
# NOTE: THESE ESTIMATES ONLY WORK I WE ASSUME WE HAVE A NORMAL DISTRIBUTION!!!!!!!!
alpha <- .05
intervals <- c(est.mean - est.sd*qt(df=N-1, alpha/2, lower.tail=FALSE)/sqrt(N),
               est.mean + est.sd*qt(df=N-1, alpha/2, lower.tail=FALSE)/sqrt(N))
intervals # OF THE ESTIMATOOOOOOOR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# APPROACH 2) MAXIMUM LIKELIHOOD
# REMARKS: 
# a) The same will work for ANY model
# b) Model=choice of likelihood function
# c) It is better to work with LOG-likelihoods.
# d) for the likelihood we will only need PROPORTIONAL to, not the exact one,
#    so may forget any constant.
cppFunction('
            double lkh(NumericVector X, NumericVector theta){
            int m=X.size();
            double lkh=0;
            for (int i=0; i<m; i++){
            lkh += -.5*pow((X[i]-theta[0])/theta[1],2)-log(theta[1]);
            }
            return lkh;
            }            
            ')
guess <- c(3, 1) # puede ser casi lo que sea
params <- optim(guess, function(t) -lkh(samp, t))$par
params
adjusted.dens <- function(x) dnorm(x, mean=params[1], sd=params[2])
hist(samp, prob=TRUE, ylim=c(0,1.5), main="Frequentist maximum likelihood estimation", col="lightgreen")
plot(adjusted.dens, xlim=c(2,4), add=TRUE, col="darkblue", lwd=2)
intervals2 <- qnorm(c(alpha/2, 1-alpha/2), mean=params[1], sd=params[2]/sqrt(N))
intervals2



# BAYESIAN APPROACH

# Suppose some researchers believes from previous studies
# that the mean and variance must be more less behaves as follows
# mu ~ NORMAL(mean=6, sd=4) 
# sigma ~ Gamma(rate=5, shape=1)  
library(ggplot2)
prior.mean <- function(x) dnorm(x, 3, .2)
prior.sd <- function(x) dgamma(x, 5, 40)
plot(prior.mean, col="darkblue", xlim=c(-5,18), lwd="2", main="Prior for mean", ylab="density")
plot(prior.sd , col="darkred", xlim=c(0,1), lwd="2", main="Prior for standard deviation", ylab="density")
# Here the wide variance in the mean reflects the uncertainty.
# For stupidity reason suppose independent.

# OUR DATA WILL BE IN MATRIX FORM!!! MANDATORY!!!! IF YOU DON'T LIKE IT
# YOU CAN USE A WRAPPER FUNCTION IN R THAT CALLS THE C FUNCTIONS AND TRANSFORMS
# A VECTOR OR A DATAFRAME INTO A NUMERICAL MATRIX. BUT THIS WAY WE CAN 
# GENERALISE THIS EXAMPLE IT TO REGRESSIONS AND LARGE DATA!
data <- matrix(iris$Sepal.Width, ncol=1)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)
y <- iris$Sepal.Length
x <- iris[c('Sepal.Width','Petal.Length','Petal.Width')]
# 1)
cppFunction('
            double objdens(NumericMatrix X, NumericVector y, NumericVector theta, double sigma){
            double lkh, logprior, yhat;
            int m=X.nrow(), p=X.ncol();
            NumericVector aux(m);
            // Compute loglikelihood
            lkh=0;
            for (int i=0; i<m; i++){
            aux = X(i,_)*theta;
            yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
            lkh += -.5/pow(sigma,2)*pow(y[i] - yhat,2);
            }
            // Compute logprior
            logprior = 0.0;
            for(int j=0; j<p; j++){
            logprior += R::dnorm(theta[j], 0.0, 100, true); // Aquí la inicial!!
            }
            logprior += R::dgamma(sigma, 5.0, 0.01, true);
            // Log of target density
            return lkh + logprior;
            }')
objdens(as.matrix(x), y, 1:3, 1)

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta, double sigma){
            int nparam = theta.size();
            double jump = 0.1; 
            NumericVector newtheta(nparam+1);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            newtheta[nparam] = R::rnorm(sigma, jump);
            if(newtheta[nparam] <= 0){
            newtheta[nparam] = 0.0001;
            }
            return newtheta;
            }')
proposal(c(1,2,3), 1)


# 3) METROPOLIS

source("BayesianMH.cpp")

nsim <- 1000
init <- rep(0,ncol(x)+1)
sigma_init <- 1
#MHBayes(20, init, objdens, proposal, data)
mh.samp <- MHBayesLinReg(nsim, init, sigma_init, objdens, proposal, cbind(1,as.matrix(x)), y)
estims <- mh.samp$theta
estims_sigma <- mh.samp$sigma
str(mh.samp)

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
# burnin and subsampling
burnin <- 100
estim <- estims[-(1:burnin), ]
thinning <- .9 # meaning we'll keep 75% of observations to reduce autocorrelation
# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]
#acf(estims[ , 1])
#acf(estims[ , 2]) 


# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE

hist(estims[ ,1], prob=TRUE, xlim=c(2.5,3.5), breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the Mean") # posterior distribution of mean
plot(prior.mean, xlim=c(2.5,3.5), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

mean(estims[ ,1]) # approx. mean-value of the posterior of mean
mean(estims[ ,2]) # approx. mean-value of the posterior of standard deviation

# CERTAINTY INTERVALS
intervals3 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
intervals3
quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) # ALSO FOR SSSDDDD

# COMPARISON OF ALL RESULTS
meanestims <- c(est.mean, params[1], mean(estims[ ,1]))
sdestims <- c(est.sd, params[2], mean(estims[ ,2]))
intmeanlow <- c(intervals[1], intervals2[1], intervals3[1])
intmeanhigh <- c(intervals[2], intervals2[2], intervals3[2])
Comparison <- data.frame(meanestims, sdestims, intmeanlow, intmeanhigh)
row.names(Comparison) <- c("Pivot", "Likelihood", "Bayesian")
Comparison
