rtip 1.1.0
----------------------------------------------------------------

* Fixed:
  - In order to uniform the argument names, now the argument `samp`
  is renamed to `samplesize`.

* Improvement: The function `setupDataset` has been changed substantialy:
  - the argument `ppp` (the purchasing power parity) has been
    changed by `pppr` (purchasing power parity rate) in the `setupDataset`
    function, and must be provided by the user in case of use. Default is
    `NULL`. The reason to do this is because `ppp` was based on a fixed file
    using 2008-2014 period, which is obsolete for some data analysis.
  - the `region` argument accepts a character or verctor string with the name(s)
    of the region(s) in the country. The default value (`NULL`) will use all the
    regions in the country.
  - The argument `deflac` is renamed to `deflator`.

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
