# Version 6.1-0

* Add an S3 class `GAMLSS` and corresponding methods encompassing all
  distributions from the `gamlss.dist` package using the workflow from the
  [distributions3](https://CRAN.R-project.org/package=distributions3) package
  (contributed by [Achim Zeileis](https://www.zeileis.org/)). The idea is that
  from fitted `gamlss` model objects predicted probability distributions
  can be obtained for which moments (mean, variance, etc.), probabilities,
  quantiles, etc. can be obtained with corresponding generic functions. See
  [useR! 2022 presentation](https://www.zeileis.org/news/user2022/) by
  Zeileis, Lang, and Hayes for an overview of the `distributions3` package.
