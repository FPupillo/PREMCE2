#!/bin/bash

# script to run parmeter recovery for different models

#simulatedmodels="simulate_RescorlaWagner_simple"

#fittedmodels="RW_simple_repar"

#SBATCH --partition=test
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --mem-per-cpu=1024
#SBATCH --time=00:30:00
#SBATCH --no-requeue
#SBATCH --mail-type=ALL


# go to the home directory
cd /home/brainimage/pupillo

# initialize spack
. spack/share/spack/setup-env.sh

# load the modules
# load an r version compatible with r-stan
module load r-4.2.0-gcc-8.2.0-ucuppxd
module load r-rcppparallel-5.1.5-gcc-8.2.0-ghjdkwe
module load r-ggplot2-3.3.6-gcc-8.2.0-3h7p2jk
module load r-gtable-0.3.0-gcc-8.2.0-oiahwyn
module load r-rlang-1.0.2-gcc-8.2.0-hsey6qk
module load r-scales-1.2.0-gcc-8.2.0-3mbmnf7
module load r-r6-2.5.1-gcc-8.2.0-wn6h5wj
module load r-lifecycle-1.0.1-gcc-8.2.0-xm3lykm
module load r-ellipsis-0.3.2-gcc-8.2.0-dzwwmsu
module load r-colorspace-2.0-3-gcc-8.2.0-jmtkddj
module load r-tibble-3.1.7-gcc-8.2.0-mb3opi6
module load r-glue-1.6.2-gcc-8.2.0-7udjfss
module load r-ggplot2-3.3.6-gcc-8.2.0-3h7p2jk
module load r-magrittr-2.0.3-gcc-8.2.0-h26qkx2
module load r-pillar-1.7.0-gcc-8.2.0-oqzmsag
module load r-crayon-1.5.1-gcc-8.2.0-fb4zfdq
module load r-fansi-1.0.3-gcc-8.2.0-pewdxkf
module load r-utf8-1.2.2-gcc-8.2.0-w332d5d
module load r-vctrs-0.4.1-gcc-8.2.0-o43fm3h
module load r-pkgconfig-2.0.3-gcc-8.2.0-w4bzfin
module load r-withr-2.5.0-gcc-8.2.0-2y5652d
module load r-inline-0.3.19-gcc-8.2.0-fhrrhj7
module load r-pkgbuild-1.3.1-gcc-8.2.0-rtd5q5g
module load r-callr-3.7.0-gcc-8.2.0-hrwt474
module load r-munsell-0.5.0-gcc-8.2.0-g74mwwq
module load r-backports-1.4.1-gcc-8.2.0-7blfzp6
module load r-bh-1.78.0-0-gcc-8.2.0-fm4n2fg
module load r-brio-1.1.3-gcc-8.2.0-jasewnd
module load r-checkmate-2.1.0-gcc-8.2.0-zsvs73j
module load r-cli-3.3.0-gcc-8.2.0-mcsvu7f
module load r-colorspace-2.0-3-gcc-8.2.0-jmtkddj
module load r-desc-1.4.1-gcc-8.2.0-p3becyv
module load r-diffobj-0.3.5-gcc-8.2.0-6dabpem
module load r-digest-0.6.29-gcc-8.2.0-imlr4dr
module load r-ellipsis-0.3.2-gcc-8.2.0-dzwwmsu
module load r-evaluate-0.15-gcc-8.2.0-fidnxfe
module load r-farver-2.1.0-gcc-8.2.0-s5dxi37
module load r-ggplot2-3.3.6-gcc-8.2.0-3h7p2jk
module load r-gridextra-2.3-gcc-8.2.0-jog3xg2
module load r-gtable-0.3.0-gcc-8.2.0-oiahwyn
module load r-isoband-0.2.5-gcc-8.2.0-glu4n2a
module load r-jsonlite-1.8.0-gcc-8.2.0-fsbp4nf
module load r-labeling-0.4.2-gcc-8.2.0-kmloglz
module load r-lattice-0.20-45-gcc-8.2.0-it7dlx3
module load r-lifecycle-1.0.1-gcc-8.2.0-xm3lykm
module load r-loo-2.5.1-gcc-8.2.0-tzh6rkq
module load r-magrittr-2.0.3-gcc-8.2.0-h26qkx2
module load r-mass-7.3-57-gcc-8.2.0-wtg7a7p
module load r-matrix-1.4-1-gcc-8.2.0-bslt7dc
module load r-matrixstats-0.62.0-gcc-8.2.0-bu45np2
module load r-mgcv-1.8-40-gcc-8.2.0-v47eaoh
module load r-munsell-0.5.0-gcc-8.2.0-g74mwwq
module load r-nlme-3.1-157-gcc-8.2.0-wihqccx
module load r-pkgload-1.2.4-gcc-8.2.0-37b3upy
module load r-praise-1.0.0-gcc-8.2.0-k3kyemg
module load r-prettyunits-1.1.1-gcc-8.2.0-lx4t4ha
module load r-processx-3.5.3-gcc-8.2.0-qzwgddg
module load r-ps-1.7.0-gcc-8.2.0-gkp6jvb
module load r-r6-2.5.1-gcc-8.2.0-wn6h5wj
module load r-rcolorbrewer-1.1-3-gcc-8.2.0-llwaapt
module load r-rcpp-1.0.8.3-gcc-8.2.0-dvsrrlc
module load r-rcppeigen-0.3.3.9.2-gcc-8.2.0-ndmvmp2
module load r-rcppparallel-5.1.5-gcc-8.2.0-ghjdkwe
module load r-rematch2-2.1.2-gcc-8.2.0-pwilh4g
module load r-rlang-1.0.2-gcc-8.2.0-hsey6qk
module load r-rprojroot-2.0.3-gcc-8.2.0-xwsms6j
module load r-rstan-2.21.5-gcc-8.2.0-xrkms52
module load r-rstudioapi-0.13-gcc-8.2.0-5wegx45
module load r-scales-1.2.0-gcc-8.2.0-3mbmnf7
module load r-stanheaders-2.21.0-7-gcc-8.2.0-san2g5p
module load r-testthat-3.1.4-gcc-8.2.0-mzuwecg
module load r-tibble-3.1.7-gcc-8.2.0-mb3opi6
module load rust-1.60.0-gcc-8.2.0-476lcv4
module load r-viridislite-0.4.0-gcc-8.2.0-bnhndc6
module load r-waldo-0.4.0-gcc-8.2.0-zatebds

# go back to the scratch directory
cd /scratch/brainimage/pupillo/PREMCE2

# first, simulate with RW simple and fit with simple
Rscript parameter_recovery/parameter_recovery.R "simulate_HMM" "HMM_repar"