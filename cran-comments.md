## R CMD check results

0 errors \| 0 warnings \| 1 notes

-   This is a new release.

## Resubmission

This is a resubmission of the 'midr' package. In this version, I have addressed the previous comments as follows:

-   The `par.midr()` function has been corrected. It now uses `par(no.readonly = TRUE)` to exclude read-only graphical parameters from its return value. This addresses the NOTE regarding changes to `par()` settings.

<!-- -->

-    The `Description` field in the DESCRIPTION file has been revised to ensure the package name 'midr' is properly quoted, as per CRAN policy.

Thank you for your time and consideration.

Best regards,\
Ryoichi Asashiba [aut, cre]
