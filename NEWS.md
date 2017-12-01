rtip 1.1.0
----------------------------------------------------------------

* Improvement: the TIP and Lorenz curves have the `samplesize` parameter. If
  `samplesize='complete'` then the complete dataset will be used instead of 
  quantiles.

* Improvement: the parameter `ppp` (the purchasing power parity) has been changed by `pppr` 
  (purchasing power parity rate) in the `setupDataset` function, 
  and must be provided by the researcher in case of use. Default is `NULL`.
  The reason to do this is because `ppp` was based on a fixed file using 2008-2014 period, which
  is obsolete for some data analysis.
  
* Help: template to load your own dataset at GitHubGist https://gist.github.com/AngelBerihuete/b5cf089d4beac3adb1b004dfc7ccbb5c

* Help: template to comparing macro-regions at GitHubGist https://gist.github.com/AngelBerihuete/02324aa2a110c61118d728861535563a

rtip 1.0.0
----------------------------------------------------------------

First release of the package.
