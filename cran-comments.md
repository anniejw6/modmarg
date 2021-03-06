# Resubmission

This is a resubmission, responding to feedback from Gregor Seyer on initial submission below. 

Gregor wrote:

> Please do not modify the user's global environment or the user's home
filespace in your examples or vignettes by deleting objects
rm(list = ls())

This is now fixed.

> Have the issues why your package was archived been fixed?
Please explain this in the submission comments.

This package was archived because it was not updated to comply with the 
change in R 4.0.0 to use a stringsAsFactors = FALSE default. This change 
caused tests expecting factors in data.frames to fail on encountering strings.

This has been resolved by specifying that the expected output should be 
data.frames with characters, and has been kept backwards-compatible by 
specifying stringsAsFactors = FALSE where data.frames are created.

# Version 0.9.6

## Test environments

* local OS X install, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

There was 1 NOTE:

Maintainer: 'Annie Wang <anniejw6@gmail.com>'
New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Stata (11:43)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-04-09 as check problems were not
    corrected despite reminders.

This package was previously accepted, but I did not get around to updating
it with the latest R release. That is now remedied.

Stata (referencing the statistical software) is spelled correctly, see, e.g.,
https://www.stata.com.

## Downstream Dependencies

There are currently no downstream dependencies for this package.
