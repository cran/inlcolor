# inlcolor

[![USGS
Category](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)
[![CRAN
Version](https://www.r-pkg.org/badges/version/inlcolor)](https://CRAN.R-project.org/package=inlcolor)
[![pipeline
status](https://code.usgs.gov/inl/inlcolor/badges/main/pipeline.svg)](https://code.usgs.gov/inl/inlcolor/-/commits/main)
[![coverage
report](https://code.usgs.gov/inl/inlcolor/badges/main/coverage.svg)](https://code.usgs.gov/inl/inlcolor/-/commits/main)

## Description

The [R](https://www.r-project.org/) package **inlcolor** provides access
to a variety of color schemes. Used to support packages and scripts
written by researchers at the United States Geological Survey (USGS)
Idaho National Laboratory Project Office
([INLPO](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)).
Key features of this package are (1) a simple and consistent API, (2)
support for qualitative, diverging, and sequential color schemes, (3)
support for combining color pallets using a hinge location, (4)
simulates color blindness, and (5) has few dependencies. The goal of
**inlcolor** is to be a single comprehensive collection of color schemes
for maps and graphs in INLPO publications.

![](man/figures/colors.png)

## Installation

The current release is available on
[CRAN](https://CRAN.R-project.org/package=inlcolor), which you can
install using the following command:

``` r
install.packages("inlcolor")
```

To install the development version, you need to clone the repository and
build from source, or run:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_gitlab(
  repo = "inl/inlcolor@develop",
  auth_token = Sys.getenv("GITLAB_PAT"),
  host = "code.usgs.gov",
  dependencies = TRUE
)
```

## Usage

The package provides two main functions for interacting with color
palettes:

-   `get_colors` is used to create a vector of `n` colors from
    qualitative, diverging, and sequential color schemes; and
-   `set_hinge` is used to set a *hinge* location within a color
    palette.

A hinge is a dramatic change in the colors at a location appropriate for
your data (such as, at sea level). Examples are given in the package
help pages. To access these documents, run:

``` r
library("inlcolor")
help(package = "inlcolor")
```

## Authors

Jason C. Fisher ([ORCID iD
0000-0001-9032-8912](https://orcid.org/0000-0001-9032-8912))

## Point of Contact

Jason C. Fisher (<jfisher@usgs.gov>)

## Suggested Citation

To cite **inlcolor** in publications, please use:

Fisher, J.C., 2023, inlcolor—Color palettes for the U.S. Geological
Survey Idaho National Laboratory Project Office: U.S. Geological Survey
software release, R package, Reston, Va.,
<https://doi.org/10.5066/P93BDACR>.

## Contributing

We welcome your contributions and suggestions for how to make these
materials more useful to the community. Please feel free to comment on
the [issue tracker](https://code.usgs.gov/inl/inlcolor/-/issues) or open
a [merge request](https://code.usgs.gov/inl/inlcolor/-/merge_requests)
to contribute.

## Code of Conduct

All contributions to- and interactions surrounding- this project will
abide by the [USGS Code of Scientific
Conduct](https://www.usgs.gov/office-of-science-quality-and-integrity/fundamental-science-practices).

<!-- Embedded References -->

## Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.

Any use of trade, product, or firm names is for descriptive purposes
only and does not imply endorsement by the U.S. Government.

## License

Unless otherwise noted, this project is in the public domain in the
United States because it contains materials that originally came from
the United States Geological Survey, an agency of the United States
Department of Interior. For more information, see the official USGS
copyright policy at
[copyrights-and-credits](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits).

Additionally, we waive copyright and related rights in the work
worldwide through the CC0 1.0 Universal public domain dedication.

#### CC0 1.0 Universal Summary

This is a human-readable summary of the [Legal Code (read the full
text)](https://creativecommons.org/publicdomain/zero/1.0/legalcode).

##### No Copyright

The person who associated a work with this deed has dedicated the work
to the public domain by waiving all of his or her rights to the work
worldwide under copyright law, including all related and neighboring
rights, to the extent allowed by law.

You can copy, modify, distribute and perform the work, even for
commercial purposes, all without asking permission.

##### Other Information

In no way are the patent or trademark rights of any person affected by
CC0, nor are the rights that other persons may have in the work or in
how the work is used, such as publicity or privacy rights.

Unless expressly stated otherwise, the person who associated a work with
this deed makes no warranties about the work, and disclaims liability
for all uses of the work, to the fullest extent permitted by applicable
law. When using or citing the work, you should not imply endorsement by
the author or the affirmer.

<!-- Embedded References -->

## Support

The Idaho National Laboratory Project Office of the USGS supports the
development and maintenance of **inlcolor**. Resources are available
primarily for maintenance and responding to user questions. Priorities
on the development of new features are determined by the development
team.

## Additional Publication Details

Additional metadata about this publication, not found in other parts of
the page is in this table.

<!--html_preserve-->
<table>
<tbody>
<tr>
<th scope="row">
Publication type
</th>
<td>
Formal R language package
</td>
</tr>
<tr>
<th scope="row">
DOI
</th>
<td>
10.5066/P93BDACR
</td>
</tr>
<tr>
<th scope="row">
Year published
</th>
<td>
2023
</td>
</tr>
<tr>
<th scope="row">
Year of version
</th>
<td>
2023
</td>
</tr>
<tr>
<th scope="row">
Version
</th>
<td>
1.0.2
</td>
</tr>
<tr>
<th scope="row">
IPDS
</th>
<td>
IP-150749
</td>
</tr>
</tbody>
</table>

<cr><!--/html_preserve-->

<!-- Embedded References -->
