/*
#-------------------------------------------------------------------------------#
#                Hierarchical Bayesian Media Mix Model                          #
#                                (HBMMM)                                        #
#-------------------------------------------------------------------------------#

Purpose
--------------------------
To develop a template STAN code to estimate HBMMM, which includes
 (1) heterogeneity in media effects across geos,
 (2) in-sample and out-of-sample estimation/prediction, and 
 (3) evaluation of model performance.


HBMMM Structure
--------------------------
1. Model
  (1) Geo-Level Model
    - y_geo = beta_base_geo + X_media*beta_media_geo + X_control*beta_control_geo + error
    - Note: 
      - error ~ N(0, sigma_y^2)
      - the model parameters, theta (=(beta_base, beta_media, beta_control)) are assumed to vary by geo.
    - Pooling:  
      - Complete pooling: theta is the same across geos.
      - No pooling: theta varies by geo but are not related across geos
      - Partial pooling: theta varies by geo and are related across geos

  (2) Geo-Level Heterogeneity
    - beta_base_geo    = Z_geo*gamma_base + error,   error ~ N(0, sigma_base^2)
    - beta_media_geo   = exp(Z_geo*gamma_media + error),   ln(error) ~ N(0, sigma_media^2)
    - beta_control_geo = Z_geo*gamma_control + error, error ~ N(0, sigma_control^2)
      - where Z_geo are geo-level covariates.


2. Data
  (1) Dimensions
    (a) num_obs    : # of observations
    (b) num_geo    : # of geos
    (c) num_media  : # of media variables
    (d) num_control: # of control variables
    (e) num_grp_var: # of geo-level covariates
    (f) num_lag    : # of max lags
    
  (2) Data
    (a) y: 
      - the response variable
      - a (num_obs) vector
    (b) X_media: 
      - the media variables
      - a (num_obs, num_media) matrix when num_lag = 0
      - a (num_obs, num_media, num_lag) array when num_lag > 0
    (c) X_control: 
      - the control variables
      - a (num_obs, num_control) matrix
    (d) Z_geo: 
      - the geo-level covariates
      - a (num_geo, num_grp_var) matrix
    (e) id_geo: the geo-id vector
      - a (num_obs) vector
      

3. Parameters
  (1) Geo-Level Parameters
    (a) beta_base: 
      - the intercept or the baseline response
      - 'beta_base' varies by geo.
      - a (num_geo) vector
    (b) beta_media:
      - the media parameters or the effects of media variables
      - 'beta_media' varies by geo and media
      - a (num_geo, num_media) matrix
    (c) beta_control:
      - the control parameters or the effects of control variables
      - 'beta_control' varies by geo and control variable
      - a (num_geo, num_control) matrix
    
  (2) Population-Level Parameters
    (a) Heterogeneity:
      - Heterogeneity is captured by geo-level covariate, 'Z_geo' and various 'sigma' parameters.
      - When 'Z_geo' = 1, 'gamma' captures the average effect across geos.
    (b) The Intercept:
      - 'gamma_base' measures the average baseline response for any given 'Z_geo'
        - i.e. 'Z_geo*gamma_base', where 'gamma_base' is a (num_grp_var) vector.
      - 'sigma_base' measures the degree of heterogeneity in the baseline response across geos.
        - 'sigma_base' is a scalar
    (c) The Media Effect:
      - 'gamma_media' measures the average media effect for any given 'Z_geo'
        - i.e. 'Z_geo*gamma_media', where 'gamma_media' is a (num_grp_var, num_media) matrix.
      - 'sigma_media' measures the degree of heterogeneity in the media effect across geos.
        - 'sigma_media' is a (num_media) vector when medias are assumed to be independent.
        - Otherwise, 'sigma_media' is a (num_media, num_media) matrix.
    (d) The Control Effect:
      - 'gamma_control' measures the average control effect for any given 'Z_geo'
        - i.e. 'Z_geo*gamma_control', where 'gamma_control' is a (num_grp_var, num_control) matrix.
      - 'sigma_control' measures the degree of heterogeneity in the control effect across geos.
        - 'sigma_control' is a (num_control) vector when controls are assumed to be independent.
        - Otherwies, 'sigma_control' is a (num_control, num_control) matrix.
    (e) Carryover Effect:
      - alpha: the retention rate of media effect
      - L    : the maximum duration of carryover effect
      - Carryover effect is assumed to be the same across geos, but vary across medias.
    (f) Shape Effect:
      - K: the saturation parameter
      - S: the slope parameter
      - Shape effect is assumed to be the same across geos, but vary across medias.

      
References
--------------------------
  - Chan, D. & Perry, M. (2017). Challenges and opportunities in media mix modeling. research.google.com.
  - Gelman, A. & Hill, J. (2006). Data analysis using regression and multilevel/hierarchical models (1st ed.). 
        Cambridge University Press.
  - Jin, Y., Wang, Y., Sun, Y., Chan, D. & Koehler, J. (2017). Bayesian methods for media mix modeling with 
        carryover and shape effects. research.google.com.
  - Rossi, P. E., Allenby, G. M., & McCulloch, R. (2005). Bayesian statistics and marketing. 
        Chichester, England: Wiley-Interscience.
  - Sun, Y., Wang, Y., Jin, Y., Chan, D. & Koehler, J. (2017). Geo-level bayesian hierarchical media mix modeling. 
        research.google.com.
  - Stan Development Team. (2020). Stan User's Guide. https://mc-stan.org/users/documentation/
  - Tellis, G. J. (2006). Modeling marketing mix. In R. Grover & M. Vriens (Eds.), Handbook of marketing research: 
        uses, misuses, and future advances (pp. 506-522). Thousand Oaks, CA.
  - Wang, Y., Jin, Y., Sun, Y., Chan, D. & Koehler, J. (2017). A hierarchical bayesian approach to improve 
        media mix models using category data. research.google.com. 
  - https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
  - https://jrnold.github.io/bayesian_notes/introduction-to-stan-and-linear-regression.html
  - https://mc-stan.org/docs/2_23/stan-users-guide/optimization-chapter.html
  - https://mc-stan.org/docs/2_23/stan-users-guide/parallelization-chapter.html
  - https://discourse.mc-stan.org/t/non-centered-parameterization-on-variance-parameter/584


# Author: Taesun Kim
# Last Revision: 07/07/2020

*/



functions {
  // the Adstock Function for Carryover Effect
  real Adstock(vector t, vector w) {
    return dot_product(t, w) / sum(w);
    } 
  
  
  // the Hill Function for Response Effect
  real Hill(real t, real K, real S) {
    return 1 / (1 + (t / K)^(-S));
  }
}



data {
  // Dimensions
  int<lower=1> num_obs;                            // # of observations
  int<lower=1> num_geo;                            // # of geos
  int<lower=1> num_media;                          // # of media variables
  int<lower=1> num_control;                        // # of control variables
  int<lower=1> num_grp_var;                        // # of geo-level covariates
  int<lower=1> num_lag;                            // # of max lags  


  // Data
  vector<lower=0>[num_obs] y;                      // the response variable
  vector[num_lag] X_media[num_obs, num_media];     // the media variables
  matrix[num_obs, num_control] X_control;          // the control variables
  matrix[num_geo, num_grp_var] Z_geo;              // the geo-level covariates
  int<lower=1, upper=num_geo> id_geo[num_obs];     // the geo-id vector  


  // Priors
  row_vector[num_media] prior_location;
  vector[num_media] prior_spread;
 
}



transformed data {


}



parameters {
  // Geo-Level Raw Parameters
  vector[num_geo] beta_base_raw;                   // the intercept by geo
  matrix[num_geo, num_media] beta_media_raw;       // the media parameters by geo-media
  matrix[num_geo, num_control] beta_control_raw;   // the control parameters by geo-control
 
 
  // Population-Level Parameters
  vector[num_grp_var] gamma_base; 
  matrix<upper=0.4>[num_grp_var, num_media] gamma_media; //exp(0.4) = 1.491825
  matrix[num_grp_var, num_control] gamma_control;
  vector<lower=0.5,upper=0.9>[num_media] K;        // the saturation parameter
  vector<lower=1.5,upper=3.5>[num_media] S;        // the slope parameter
  vector<lower=0.2,upper=0.75>[num_media] alpha;   // the retention rate of media effect


  // Standard Deviations
  real<lower=0> sigma_y;                           // the standard deviation of y
  real<lower=0> sigma_base;                        // the standard deviation of beta_base
  real<lower=0> sigma_media;                       // the standard deviation of beta_media
  real<lower=0> sigma_control;                     // the standard deviation of beta_control
}



transformed parameters {
  // Geo-Level Parameters
  vector[num_geo] beta_base;                       // the intercept by geo
  matrix[num_geo, num_media] beta_media;           // the media parameters by geo-media
  matrix[num_geo, num_control] beta_control;       // the control parameters by geo-control


  // The Mean of Each Observation
  vector[num_obs] mu_y;

  
  // Carryover and Shape Effects
  //vector<lower=0>[num_lag] weights;
  //Specifying weights as a matrix rather than a vector
  matrix<lower=0>[num_media,num_lag] weights;
  //vector<lower=0>[num_lag] weight_vec;
  real X_Adstock;
  matrix[num_obs, num_media] X_Hill;


  // Note: due to the positive constraint of media impact on sales,
  // log(beta_media) = Z_geo*gamma_media + sigma_media --> beta_media = exp(Z_geo*gamma_media + sigma_media)
  // Use `prior_location` as lower bounds for media parameters
  for (geo in 1:num_geo) { // How geo-level parameters be genearated.
    beta_base[geo]      = Z_geo[geo,]*gamma_base + sigma_base*beta_base_raw[geo];
    beta_media[geo, ]   = prior_location + exp(Z_geo[geo,]*gamma_media + sigma_media*beta_media_raw[geo, ]);
    beta_control[geo, ] = Z_geo[geo,]*gamma_control + sigma_control*beta_control_raw[geo, ];
  }
  
  //As the lags is not dependent on each observation, refactoring the code to reduce the number of computations and loops
  for (media in 1:num_media) {
      for (lag in 1:num_lag){
        weights[media,lag] = pow(alpha[media], (lag - 1));
      }
   }


  // X_media is transformed via (1) Adstock and (2) Hill functions.
  for (obs in 1:num_obs) {// How each observation be genearated.
    for (media in 1:num_media) {
      //extracting the weight vector from the matrix by media
      X_Adstock = Adstock(X_media[obs, media], weights[media,]');
      X_Hill[obs, media] = Hill(X_Adstock, K[media], S[media]);
    }
    mu_y[obs] = beta_base[id_geo[obs]] + X_Hill[obs,]*beta_media[id_geo[obs],]' + 
                  X_control[obs,]*beta_control[id_geo[obs],]';
  }
}





model {
  // Likelihood
  y ~ normal(mu_y, sigma_y);

  
  // Priors
  // Prior values will change later.
  sigma_y ~ normal(0, 1);
  
  alpha ~ beta(3, 3);
  S ~ gamma(1, 0.5);
  K ~ beta(2, 2);

  beta_base_raw ~ normal(0, 1);
  for (geo in 1:num_geo) {
    beta_media_raw[geo,] ~ normal(0, 1);
    beta_control_raw[geo,] ~ normal(0, 1);
  }

  
  // Priors for Hyper-Parameters
  gamma_base ~ normal(0, 1);                // Intercept
  to_vector(gamma_media) ~ normal(-0.7, 3); // exp(-1.2) = 0.3011942
  to_vector(gamma_control) ~ normal(0, 1);
  
  sigma_base ~ normal(0, 1);
  sigma_media ~ normal(0, 2);
  sigma_control ~ normal(0, 1);  
}



generated quantities {
  //Model Estimation and Prediction
  vector[num_obs] y_fitted;               // fitted y
  vector[num_obs] y_pred;                 // predicted y
  vector[num_obs] log_lik;                // log-likelihood
  
  // Note:
  // beta_media[geo, ] = exp(Z_geo[geo,]*gamma_media + sigma_media*beta_media_raw[geo, ]);
  // --> log(beta_media) ~ normal(Z_geo*gamma_media + sigma_media)
  matrix[num_grp_var, num_media] exp_gamma_media;
  exp_gamma_media = exp(gamma_media);
  
  // Compute fit meterics
  for (obs in 1:num_obs) {     
    y_fitted[obs] = mu_y[obs];
    y_pred[obs]   = normal_rng(y_fitted[obs], sigma_y);
    log_lik[obs]  = normal_lpdf(y[obs] | mu_y[obs], sigma_y);
  }
}
