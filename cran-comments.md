## Resubmission
This is a resubmission. In this version I have:

* Reduced the DESCRIPTION title to 58 characters.

* Added references to the Description field of the DESCRIPTION file.

## Test environments
* local OS macOS Catalina install, R 4.0.0
* win-builder (devel and release)
* rhub (debian-gcc-devel, linux-x86_64-centos6-epel-rdt)

## R CMD check results
For local OS and linux-x86_64-centos6-epel-rdt there were no ERRORs or WARNINGs or NOTEs. 
For win-builder and debian-gcc-devel there were no ERRORs or WARNINGs.

For win-builder there was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jessica Grembi <jess.grembi@gmail.com>'
  New submission
  Possibly mis-spelled words in DESCRIPTION:
  confounder (14:282)
  
  Confounder is an epidemiologically-relevant term applicable to our package description.


For debian-gcc-devel there were 2 NOTEs:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jessica Grembi <jess.grembi@gmail.com>’
  New submission
  Possibly mis-spelled words in DESCRIPTION:
  confounder (15:282)

  Confounder is an epidemiologically-relevant term applicable to our package description.
  
* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
  plot.gComp  8.704 0.016   8.723
 

  The plot.gComp function is intended to validate that the bootstrap results are normally distributed.  Thus,   we need to run sufficient bootstraps to show a normal distribution, which we were able to accomplish with 60   bootstraps. This check passes on macOS Catalina in <5s and is close to the <10s recommended for Windows so    we hope to keep it as is. If needed, can reduce the number of bootstraps further or include `\donttest{}`     for this example. 
  


## Downstream dependencies
There are currently no downstream dependencies for this package