
# Version 6.1-4

- The `dPO`, `pPO` and `qPO` are updated so the length of y is equal to the length of `mu`

- the functions `test_continuous_gamlss_dist()` and `test_discrete_gamlss_dist()` are added to the package for checking distributions but the function not have help files. 

# Version 6.1-3

* `BCT` the `BCPE` and the `BCCG` have new `d`, `p` and `q` functions

*  the `q` functions for all distributions are updated so the limits are defined properly for example for the BEINF we have;

     --   `q[abs(p-0)<1e-15] <- 0`
     
     --   `q[abs(p-1)<1e-15] <- Inf`
     
     --   `q[p <  0] <- NaN`
     
     --   `q[p >  1] <- NaN`.
          
   

# Version 6.1-2

* The GitHub repository is now hosted under the new `gamlss-dev` organization:
  <https://github.com/gamlss-dev/gamlss/>.


# Version 6.1-1

* The package is now hosted on GitHub at
  <https://github.com/mstasinopoulos/GAMLSS-Distibutions/>.

* Add an S3 class `GAMLSS` and corresponding methods encompassing all
  distributions from the `gamlss.dist` package using the workflow from the
  [distributions3](https://CRAN.R-project.org/package=distributions3) package
  (contributed by [Achim Zeileis](https://www.zeileis.org/)). The idea is that
  from fitted `gamlss` model objects predicted probability distributions
  can be obtained for which moments (mean, variance, etc.), probabilities,
  quantiles, etc. can be obtained with corresponding generic functions. See
  [useR! 2022 presentation](https://www.zeileis.org/news/user2022/) by
  Zeileis, Lang, and Hayes for an overview of the `distributions3` package.
