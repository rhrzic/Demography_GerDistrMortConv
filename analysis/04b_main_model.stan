data {
    int A;                         // number of single-year ages
    int G;                         // number of age groups
    int R;                         // number of regions
    int K;                         // number of TOPALS offsets per schedule

    matrix[A,K] B;                 // spline basis fns for TOPALS

    int n1;                        // rows in S1 
    int n2;                        // rows in S2

    matrix[n1,R] S1;               // spatial basis fns ( {iid N(0,1)} %*% Sj) -> level-j hier eff)
    matrix[n2,R] S2;               // spatial basis fns ( {iid N(0,1)} %*% Sj) -> level-j hier eff)

    matrix[G,A] W;                 // weigthing matrix to convert single-year mort rates to grp avgs

    matrix[A,2] lambda_star;       // standard schedules: A x 2 (male,female)
    
    matrix[A,R] dstar;             // expected M-F difference in log mx by (age, region)


/*--------
Stan syntax means that identically-organized arrays of 
integers and reals need different-looking declarations. 
In both cases below, varname[s][g,r] will mean
(sex s, age group g, region r).  
In R, the input array in both cases is 2 x G x R
--------*/

    int D[2,G,R];                // deaths: 2-array of deaths by age group,region
    matrix[G,R]  N[2];           // exposure: 2-array of person-yrs by age group,region
}

parameters {
  matrix[K,n1] eps1[2];   // deep spatial params: 2-array of (R-1) x K matrices
  matrix[K,n2] eps2[2];   // deep spatial params: 2-array of (R-1) x K matrices

  vector[K]     mu[2];    // global means for alphas, by component and sex: 2-array of 1xK vectors
  
  real<lower=0> sigma_hier[2];   // sds of hier spatial effects
  real<lower=0> sigma_sex;       // sd of sex-difference prior 
}

transformed parameters {
  matrix[K,R] alpha[2];    // TOPALS offsets: 2-array of K x R matrices
  matrix[A,R] lambda[2];   // logmx rates: 2-array of A x R matrices
  matrix[G,R] log_Dhat[2]; // log expected deaths: 2-array of G x R matrices
  {

  matrix[G,R] logM[2];     // (log) Mg group-avgd rates: 2-array of A x R matrices
  
  for (s in 1:2) {
      alpha[s]  = rep_matrix(mu[s], R) + sigma_hier[1] * eps1[s] * S1
                                       + sigma_hier[2] * eps2[s] * S2;  // K x R

      lambda[s] = rep_matrix(lambda_star[,s], R) + B * alpha[s];        // A x R 
      
      logM[s]      = log(W * exp(lambda[s]) );    // G x R
 
  
      log_Dhat[s] = log(N[s]) + logM[s]; 
      
    } // for s 
  }
} // transformed parameters

model {
  matrix[A,R]   sex_diffs;
  matrix[K-1,R] alpha_diffs[2];   // 2-array of alpha(2:K)-alpha(1:(K-1)) for each region

/*----------------------
PRIORS
-----------------------*/
  sigma_sex  ~ normal(0, .10);
  sigma_hier ~ normal(0, .20);

//--- deep parameters
  for (s in 1:2) {
    to_vector(eps1[s]) ~ normal(0,1);
    to_vector(eps2[s]) ~ normal(0,1);
  }

//--- smoothing (alpha-diff) prior

  for (s in 1:2) {
    alpha_diffs[s] = alpha[s][2:K,] - alpha[s][1:(K-1),];

    to_vector(alpha_diffs[s]) ~ normal(0, 0.7071);
  }

//--- sex-difference prior

  sex_diffs = ( lambda[1] - lambda[2] ) - dstar;  

  to_vector(sex_diffs) ~ normal(0, sigma_sex);

/*----------------------
LIKELIHOOD
to_array_1d( ) 
   converts int[...]  in ROW major order
   converts real[...] in ROW major order
   converts matrix    in COL major order !!!
so the transpose symbol (') on log_Dhat is CRITICAL
-----------------------*/

  for (s in 1:2) {
    to_array_1d(D[s]) ~ poisson_log( to_array_1d( log_Dhat[s]') ) ;
  }


}
