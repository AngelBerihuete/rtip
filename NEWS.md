rtip 1.1.1
----------------------------------------------------------------

* Improvement: testGL and testTIP functions have the significance level
  `alpha` as argument.

* Fixed/Improvement:
  - Upper and Lower bounds for the critical value for jointly 
  testing equality and inequality are calculated efficiently using
  indications in (David & Palm). (Thanks to a referee in 
  the process of submission to Rjournal!)
  
  - setupDataset will not calculate wHX040. This variable will be 
  calculated in every indicator.


rtip 1.1.0
----------------------------------------------------------------

* Fixed:
  - In order to uniform the argument names, now the argument `samp`
  is renamed to `samplesize`.

* Improvement: The function `setupDataset` has been changed substantially:
  - the optional argument `ppp` (the purchasing power parity) has been
    changed by `pppr` (purchasing power parity rate), and must be provided by 
    the user in case of use. Default value is `NULL`. The reason to do this is
    because the purchasing power parity was based on a fixed file using 2008-2014
    period, which is obsolete for some data analysis.
  - the `region` argument accepts a character string with the name(s)
    of the region(s) in the country, thus making it possible to study macro regions.
    The default value (`NULL`) will use all the regions in the country.
  - The argument `deflac` is renamed conveniently to `deflator`.

* Improvement: the user can specify the name of the variables in the call of
  the functions (with default values corresponding to EU-SILC names).

* Improvement: the `ci` argument accepts a scalar or vector containing the
  confidence level(s) of the required confidence interval(s).

* Improvement: TIP and Lorenz curves have the `samplesize` parameter. If
  `samplesize='complete'` then the complete dataset will be used instead of
  quantiles.


rtip 1.0.0
----------------------------------------------------------------

First release of the package.
