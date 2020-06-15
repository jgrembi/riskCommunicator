## Test environments
* local OS macOS Catalina install, R 4.0.0
* win-builder (devel and release)

## R CMD check results
For local OS there were no ERRORs or WARNINGs or NOTEs. 
For win-builder there were no ERRORs or WARNINGs.

There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jessica Grembi <jess.grembi@gmail.com>'
  New submission
  Version contains large components (0.0.0.9000)
  Possibly mis-spelled words in DESCRIPTION:
  confounder (14:282)
  
  Confounder is an epidemiologically-relevant term applicable to our package description.

** running examples for arch 'i386' ... [25s] NOTE
  Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
  plot.gComp 11.25   0.04   11.28

** running examples for arch 'x64' ... [26s] NOTE
  Examples with CPU (user + system) or elapsed time > 10s
              user system elapsed
  plot.gComp 11.84   0.04    11.9  

  The plot.gComp function is intended to validate that the bootstrap results are normally distributed.  Thus, we need to run ~100 bootstraps to show a normal distribution. This check passes on macOS Catalina in <5s and is close to the <10s recommended for Windows so we hope to keep it as is. If needed, can reduce the number of bootstraps executed in the example prior to plotting. 
  


## Downstream dependencies
There are currently no downstream dependencies for this package