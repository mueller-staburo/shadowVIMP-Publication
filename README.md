
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shadowVIMP - Simulation Study

<!-- badges: start -->
<!-- badges: end -->

This repository contains the code for the simulation study conducted in
\[PAPER TITLE\]. The most important files in this repository are:

1.  File `01_sim.R` specifies the data simulation scenarios along with
    the methods for performing feature selection.
2.  File `02_unpack_resuts.R` gathers the results obtained in the first
    file and computes performance measures for all scenarios.
3.  File `03_alzheimer` performs feature selection using the shadowVIMP
    method on the Alzheimer dataset from the `AppliedPredictiveModeling`
    package.

The below table contains the details of the data simulation designs and
feature selection methods that are specified in the `01_sim.R` file.

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center;">
Data Simulation Design
</th>
<th style="text-align:center;">
Method
</th>
<th style="text-align:center;">
Setting Name in `01_sim.R`
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;vertical-align: middle !important;width: 20em; " rowspan="5">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 50, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
shadowVIMP without pre-selection
</td>
<td style="text-align:center;">
evaluatesetting1
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting2
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting3
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting4
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Janitza, Celik, and Boulesteix (2018) with 10.000 trees
</td>
<td style="text-align:center;">
evaluatesetting5
</td>
</tr>
<tr>
<td style="text-align:center;vertical-align: middle !important;width: 20em; " rowspan="6">
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
shadowVIMP without pre-selection
</td>
<td style="text-align:center;">
evaluatesetting6
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting7
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting8
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting9
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting10
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting11
</td>
</tr>
<tr>
<td style="text-align:center;vertical-align: middle !important;width: 20em; " rowspan="5">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 10, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
shadowVIMP without pre-selection
</td>
<td style="text-align:center;">
evaluatesetting12
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting13
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting14
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting15
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Janitza, Celik, and Boulesteix (2018) with 10.000 trees
</td>
<td style="text-align:center;">
evaluatesetting16
</td>
</tr>
<tr>
<td style="text-align:center;vertical-align: middle !important;width: 20em; " rowspan="6">
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
shadowVIMP without pre-selection
</td>
<td style="text-align:center;">
evaluatesetting17
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting18
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting19
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting20
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting21
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting22
</td>
</tr>
<tr>
<td style="text-align:center;vertical-align: middle !important;width: 20em; " rowspan="6">
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
shadowVIMP without pre-selection
</td>
<td style="text-align:center;">
evaluatesetting23
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting24
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting25
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting26
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting27
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting28
</td>
</tr>
</tbody>
</table>

The remainder of the repository has the following structure:

- Folder `code_rfvarse` stores the numeric results of all analyzed data
  simulation designs and feature selection algorithms, grouped into
  subfolders according to the data simulation design.
- Folder `functions` contains the code for all custom functions used in
  this study.
- The “xls” files store performance measures for both the proposed
  method and established methods, as created in the `01_sim.R` file.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-degenhardt2019evaluation" class="csl-entry">

Degenhardt, Frauke, Stephan Seifert, and Silke Szymczak. 2019.
“Evaluation of Variable Selection Methods for Random Forests and Omics
Data Sets.” *Briefings in Bioinformatics* 20 (2): 492–503.

</div>

<div id="ref-friedman1991multivariate" class="csl-entry">

Friedman, Jerome H. 1991. “Multivariate Adaptive Regression Splines.”
*The Annals of Statistics* 19 (1): 1–67.

</div>

<div id="ref-hapfelmeier2023package" class="csl-entry">

Hapfelmeier, Alexander, and Roman Hornung. 2023. “Package ‘Rfvimptest’.”

</div>

<div id="ref-hapfelmeier2023efficient" class="csl-entry">

Hapfelmeier, Alexander, Roman Hornung, and Bernhard Haller. 2023.
“Efficient Permutation Testing of Variable Importance Measures by the
Example of Random Forests.” *Computational Statistics & Data Analysis*
181: 107689.

</div>

<div id="ref-janitza2018computationally" class="csl-entry">

Janitza, Silke, Ender Celik, and Anne-Laure Boulesteix. 2018. “A
Computationally Fast Variable Importance Test for Random Forests for
High-Dimensional Data.” *Advances in Data Analysis and Classification*
12 (4): 885–915.

</div>

<div id="ref-kursa2010boruta" class="csl-entry">

Kursa, Miron B, Aleksander Jankowski, and Witold R Rudnicki. 2010.
“Boruta–a System for Feature Selection.” *Fundamenta Informaticae* 101
(4): 271–85.

</div>

<div id="ref-kursa2010feature" class="csl-entry">

Kursa, Miron B, and Witold R Rudnicki. 2010. “Feature Selection with the
Boruta Package.” *Journal of Statistical Software* 36: 1–13.

</div>

<div id="ref-nicodemus2010behaviour" class="csl-entry">

Nicodemus, Kristin K, James D Malley, Carolin Strobl, and Andreas
Ziegler. 2010. “The Behaviour of Random Forest Permutation-Based
Variable Importance Measures Under Predictor Correlation.” *BMC
Bioinformatics* 11: 1–13.

</div>

<div id="ref-strobl2007bias" class="csl-entry">

Strobl, Carolin, Anne-Laure Boulesteix, Achim Zeileis, and Torsten
Hothorn. 2007. “Bias in Random Forest Variable Importance Measures:
Illustrations, Sources and a Solution.” *BMC Bioinformatics* 8: 1–21.

</div>

</div>
