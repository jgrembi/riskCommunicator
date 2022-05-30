## New submission/Resubmission of a package that was archived
In this version I have:

* Added a summary method for class `gComp` objects

* Added additional error checks for the the new summary and print methods

* Fixed errors leading to vignette not building under various test environments (reason that package was removed from CRAN and archived)


## Test environments
* local OS macOS Catalina install, R 4.2.0
* rhub(ubuntu-gcc-release, fedora-clang-devel, windows-x86_64-devel)

## R CMD check results
For local OS, there were no ERRORs or WARNINGs or NOTEs.
For all others, there were 2 NOTEs:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jessica Grembi <jess.grembi@gmail.com>'

  New submission

  Package was archived on CRAN

  Possibly misspelled words in DESCRIPTION:
    Ahern (14:431)
    Galea (14:451)
    Snowden (14:490)
    Westreich (14:555)
    al (14:568)
    confounder (14:282)
    et (14:565)
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.1002/sim.5316
      From: DESCRIPTION
    Interpretable (2:34)
      Status: Service Unavailable
      Message: 503
        
        
  I have verified the spelling for each word and the DOI is correct.  Confounder is an epidemiologically-relevant term applicable to our package description, the remainder of potential mis-spellings are author names from references.      
        
* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
              user system elapsed
  plot.gComp 10.78   0.41   11.19    
    
    
  The plot.gComp function is intended to validate that the bootstrap results are normally distributed.  Thus, we need to run sufficient bootstraps to show a normal distribution, which we were able to accomplish with 60 bootstraps. This check passes on macOS Catalina in <5s and is close to the <10s recommended for Windows so we hope to keep it as is. If needed, can reduce the number of bootstraps further or include `\donttest{}` for this example. 
    
For windows-x86-64-devel there was one additional NOTE:
* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'





# Previous cran-comments
## Resubmission
This is a resubmission. In this version I have:

* Added functionality to use negative binomial distribution for overdispersed count/rate outcomes.

* Changed order of operations for calculating risk difference which resulted in changes to previous estimates at the hundredths decimal place.

* Added additional error checks to prevent users from inputting incorrect data structures.


## Test environments
* local OS macOS Catalina install, R 4.1.2
* win-builder (devel and release)
* rhub (debian-gcc-devel, ubuntu-gcc-devel, ubuntu-gcc-release, linux-x86_64-rocker-gcc-san)

## R CMD check results
For local OS, win-builder, debian-gcc-devel, ubuntu-gcc-devel, ubuntu-gcc-release, and linux-x86_64-rocker-gcc-san there were no ERRORs or WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package


## Resubmission
This is a resubmission. In this version I have:
* Reduced the DESCRIPTION title to 58 characters.
* Added references to the Description field of the DESCRIPTION file.

## Test environments
* local OS macOS Catalina install, R 4.0.0
* win-builder (devel and release)
* rhub (debian-gcc-devel, linux-x86_64-centos-epel-rdt)

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
 

  The plot.gComp function is intended to validate that the bootstrap results are normally distributed.  Thus, we need to run sufficient bootstraps to show a normal distribution, which we were able to accomplish with 60 bootstraps. This check passes on macOS Catalina in <5s and is close to the <10s recommended for Windows so we hope to keep it as is. If needed, can reduce the number of bootstraps further or include `\donttest{}` for this example. 
  

## Downstream dependencies
There are currently no downstream dependencies for this package