
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shadowVIMP - Simulation Study

<!-- badges: start -->
<!-- badges: end -->

This repository contains the code for the simulation study conducted in
**“shadowVIMP: Permutation-based multiple testing-controlled variable
selection”**. The most important files in this repository are:

1.  File `01_sim.R` specifies the data simulation scenarios along with
    the methods for performing feature selection.
2.  File `02_unpack_resuts.R` gathers the results obtained in the first
    file and computes performance measures for all scenarios.
3.  File `03_alzheimer.R` performs feature selection using the
    shadowVIMP method on the Alzheimer dataset from the
    `AppliedPredictiveModeling` package.

The remainder of the repository is organized as follows:

- **functions/** contains all custom R functions used in this study:
  - `data_simulation_functions.R` implements the various data-generation
    designs used (see table below).
  - `vim_perm_sim.R`, `add_test_results.R` and `vim_perm_sim_wrapper.R`
    form the implementation of the proposed shadowVIMP method as used in
    this study. Since then, the implementation was ported into a
    publicly available R-package on CRAN: `shadowVIMP` package (Mueller
    and Miluch 2025), which we recommend using.
  - All the other scripts in this folder contain technical helper
    functions.
- **results/** stores every output presented in the paper:
  - **figures/** stores graphics included in the manuscript.
  - **intermediate_results/** has subfolders named by simulation
    scenario that contain per-method RDS result files. Two summary
    `.xlsx` files aggregate performance metrics for the proposed and
    comparison methods. The file `alzheimer.rds` contains results from
    `03_alzheimer.R`.
  - **logs/** contains text files capturing session information from
    `01_sim.R`, `02_unpack_resuts.R` and `03_alzheimer.R`.
  - **tables/** stores tables used in the publication.
  - **`reference.bib`** - BibTeX entries for all papers cited in this
    repository.

Below is a table summarizing the data simulation designs and feature
selection methods that are used in the `01_sim.R` file.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
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
<td style="text-align:center;width: 20em; ">
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
Degenhardt, Seifert, and Szymczak (2019) with a group size of 50, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting2
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 50, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting3
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 50, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting4
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 50, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Method from Janitza, Celik, and Boulesteix (2018) with 10.000 trees
</td>
<td style="text-align:center;">
evaluatesetting5
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
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
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting7
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting8
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting9
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting10
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Friedman (1991) with 100 observations
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting11
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
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
Degenhardt, Seifert, and Szymczak (2019) with a group size of 10, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting13
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 10, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting14
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 10, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting15
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Degenhardt, Seifert, and Szymczak (2019) with a group size of 10, 100
observations, and a total of 5.000 covariates.
</td>
<td style="text-align:center;">
Method from Janitza, Celik, and Boulesteix (2018) with 10.000 trees
</td>
<td style="text-align:center;">
evaluatesetting16
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
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
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting18
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting19
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting20
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting21
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
Strobl et al. (2007) with 100 observations
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting22
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
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
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
shadowVIMP with pre-selection
</td>
<td style="text-align:center;">
evaluatesetting24
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
Boruta algorithm with 10.000 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting25
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
Boruta algorithm with 500 trees Kursa and Rudnicki (2010)
</td>
<td style="text-align:center;">
evaluatesetting26
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 10.000 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting27
</td>
</tr>
<tr>
<td style="text-align:center;width: 20em; ">
The null case of the design Nicodemus et al. (2010)
</td>
<td style="text-align:center;">
Method from Hapfelmeier, Hornung, and Haller (2023) with 500 trees,
implemented in Hapfelmeier and Hornung (2023)
</td>
<td style="text-align:center;">
evaluatesetting28
</td>
</tr>
</tbody>
</table>

Computational specifications:

Program and Platform:

- Program: R, versions 4.2.2 and 4.2.1.
- The raw results of the simulation study were obtained on a Linux
  cluster, and the evaluation of the raw results to produce the final
  results as well as the conduction of the real data analyses was
  performed on Windows.
- Below is the output of the R command sessionInfo() on the Linux
  machine and on the Windows machine. The output specifies which R
  packages and versions of those packages were used to generate the raw
  results and to evaluate them.

sessionInfo() of the Linux machine (`01_sim.R`):

    #> ```r
    #> > sessionInfo()
    #> R version 4.2.2 Patched (2022-11-10 r83330)
    #> Platform: x86_64-pc-linux-gnu (64-bit)
    #> Running under: Debian GNU/Linux 12 (bookworm)
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.21.so
    #> 
    #> locale:
    #>  [1] LC_CTYPE=de_DE.UTF-8       LC_NUMERIC=C               LC_TIME=de_DE.UTF-8        LC_COLLATE=de_DE.UTF-8    
    #>  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=de_DE.UTF-8    LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> attached base packages:
    #>  [1] stats4    grid      parallel  stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] stringr_1.5.1     data.table_1.16.4 tidyr_1.3.1       permimp_1.0-2     party_1.3-11      strucchange_1.5-3
    #>  [7] sandwich_3.0-2    zoo_1.8-11        modeltools_0.2-23 mvtnorm_1.3-3     MASS_7.3-58.2     Hmisc_4.8-0      
    #> [13] ggplot2_3.5.1     Formula_1.2-5     survival_3.5-3    lattice_0.20-45   mlbench_2.1-3     Boruta_8.0.0     
    #> [19] ranger_0.14.1     dplyr_1.1.3       doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] Rcpp_1.0.14          deldir_1.0-6         png_0.1-8            digest_0.6.37        R6_2.6.1            
    #>  [6] backports_1.5.0      pillar_1.10.1        rlang_1.1.1          multcomp_1.4-22      rstudioapi_0.14     
    #> [11] rpart_4.1.19         Matrix_1.5-3         checkmate_2.3.2      splines_4.2.2        foreign_0.8-84      
    #> [16] htmlwidgets_1.6.1    munsell_0.5.1        compiler_4.2.2       xfun_0.37            pkgconfig_2.0.3     
    #> [21] base64enc_0.1-3      libcoin_1.0-9        htmltools_0.5.4      nnet_7.3-18          tidyselect_1.2.1    
    #> [26] tibble_3.2.1         gridExtra_2.3        htmlTable_2.4.1      coin_1.4-2           matrixStats_0.63.0  
    #> [31] codetools_0.2-19     randomForest_4.7-1.1 withr_3.0.2          gtable_0.3.6         lifecycle_1.0.4     
    #> [36] magrittr_2.0.3       scales_1.3.0         cli_3.6.1            stringi_1.8.4        latticeExtra_0.6-30 
    #> [41] generics_0.1.3       vctrs_0.6.3          TH.data_1.1-1        RColorBrewer_1.1-3   tools_4.2.2         
    #> [46] interp_1.1-3         glue_1.8.0           purrr_1.0.4          jpeg_0.1-10          fastmap_1.1.1       
    #> [51] colorspace_2.1-1     cluster_2.1.4        knitr_1.42
    #> 
    #> ```

sessionInfo() of the Windows machine (`02_unpack_results.R`):

    #> ```r
    #> R version 4.2.1 (2022-06-23 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows Server x64 (build 17763)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
    #> [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.1252    
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] stringr_1.5.1 foreach_1.5.2 dplyr_1.1.3  
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] pillar_1.9.0      compiler_4.2.1    base64enc_0.1-3   iterators_1.0.14  tools_4.2.1      
    #>  [6] digest_0.6.31     rpart_4.1.16      evaluate_0.23     checkmate_2.1.0   lifecycle_1.0.4  
    #> [11] tibble_3.2.1      gtable_0.3.6      htmlTable_2.4.1   pkgconfig_2.0.3   rlang_1.1.2      
    #> [16] cli_3.6.0         rstudioapi_0.14   writexl_1.4.2     xfun_0.41         fastmap_1.1.0    
    #> [21] gridExtra_2.3     knitr_1.45        withr_3.0.2       cluster_2.1.3     htmlwidgets_1.6.1
    #> [26] generics_0.1.3    vctrs_0.6.4       grid_4.2.1        nnet_7.3-17       tidyselect_1.2.1 
    #> [31] glue_1.6.2        data.table_1.14.6 R6_2.5.1          fansi_1.0.3       rmarkdown_2.19   
    #> [36] foreign_0.8-82    Formula_1.2-5     purrr_1.0.1       tidyr_1.3.0       ggplot2_3.5.1    
    #> [41] magrittr_2.0.3    MASS_7.3-60       htmltools_0.5.4   backports_1.5.0   Hmisc_5.1-0      
    #> [46] scales_1.3.0      codetools_0.2-18  colorspace_2.1-1  mlbench_2.1-3     utf8_1.2.2       
    #> [51] stringi_1.7.12    munsell_0.5.1
    #> 
    #> ```

sessionInfo() of the Windows machine (`03_alzheimer.R`):

    #> ```r
    #> R version 4.2.1 (2022-06-23 ucrt)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows Server x64 (build 17763)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.1252    
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] forcats_1.0.0                   purrr_1.0.1                     readr_2.1.4                     tidyr_1.3.0                     tibble_3.2.1                   
    #>  [6] tidyverse_1.3.2                 AppliedPredictiveModeling_1.1-7 ggplot2_3.5.1                   stringr_1.5.1                   foreach_1.5.2                  
    #> [11] dplyr_1.1.3                    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] fs_1.5.2            lubridate_1.9.2     httr_1.4.4          tools_4.2.1         backports_1.5.0     utf8_1.2.2          R6_2.5.1            rpart_4.1.16       
    #>  [9] Hmisc_5.1-0         DBI_1.1.3           colorspace_2.1-1    nnet_7.3-17         withr_3.0.2         tidyselect_1.2.1    gridExtra_2.3       compiler_4.2.1     
    #> [17] textshaping_0.3.6   cli_3.6.0           rvest_1.0.3         htmlTable_2.4.1     xml2_1.3.3          labeling_0.4.3      scales_1.3.0        checkmate_2.1.0    
    #> [25] systemfonts_1.1.0   digest_0.6.31       foreign_0.8-82      rmarkdown_2.19      base64enc_0.1-3     pkgconfig_2.0.3     htmltools_0.5.4     plotrix_3.8-2      
    #> [33] dbplyr_2.3.0        fastmap_1.1.0       htmlwidgets_1.6.1   rlang_1.1.2         readxl_1.4.2        rstudioapi_0.14     farver_2.1.2        generics_0.1.3     
    #> [41] jsonlite_1.8.4      googlesheets4_1.0.1 CORElearn_1.57.3    magrittr_2.0.3      Formula_1.2-5       Matrix_1.5-3        Rcpp_1.0.12         munsell_0.5.1      
    #> [49] fansi_1.0.3         lifecycle_1.0.4     stringi_1.7.12      MASS_7.3-60         plyr_1.8.8          grid_4.2.1          crayon_1.5.2        lattice_0.20-45    
    #> [57] haven_2.5.1         hms_1.1.2           knitr_1.45          pillar_1.9.0        ranger_0.14.1       reshape2_1.4.4      codetools_0.2-18    reprex_2.0.2       
    #> [65] glue_1.6.2          evaluate_0.23       rpart.plot_3.1.1    data.table_1.14.6   modelr_0.1.11       mlbench_2.1-3       vctrs_0.6.4         tzdb_0.3.0         
    #> [73] cellranger_1.1.0    gtable_0.3.6        assertthat_0.2.1    xfun_0.41           broom_1.0.7         viridisLite_0.4.2   ragg_1.2.5          googledrive_2.0.0  
    #> [81] gargle_1.3.0        iterators_1.0.14    ellipse_0.4.3       writexl_1.4.2       cluster_2.1.3       timechange_0.2.0    ellipsis_0.3.2
    #> 
    #> ```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

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

<div id="ref-shadowVIMP" class="csl-entry">

Mueller, Tim, and Oktawia Miluch. 2025. *shadowVIMP: Covariate Selection
Based on VIMP Permutation-Like Testing*.
<https://github.com/OktawiaStaburo/shadowVIMP>.

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
