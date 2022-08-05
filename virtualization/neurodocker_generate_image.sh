#!/bin/sh

set -e

# The function below will generate the Dockerfile required to build the docker image.
generate() {
  docker run --rm repronim/neurodocker:0.7.0 generate "$1" \
    --base=ubuntu:22.04 \
    --pkg-manager=apt \
    --install "software-properties-common g++ clang-13 r-base r-base-dev \
               littler gpgv2 r-cran-rstan libcurl4 libcurl4-openssl-dev cmake" \
    --copy install_R_env.r /opt/install_R_env.r \
    --run-bash 'Rscript /opt/install_R_env.r' \
    --user=jovyan \
    --workdir='/home/jovyan' \
    --entrypoint "/neurodocker/startup.sh Rscript" 
  }

# run the function and parse/save the output to a Dockerfile
generate docker > Dockerfile

# build the docker image locally
docker build -t rstan_lisco .

# save the docker image to a local file that can be shared 
docker save rstan_lisco | gzip > rstan_lisco.tar.gz

# convert the docker image to a local singularity image that can be shared
docker run --privileged -t --rm \
            -v /var/run/docker.sock:/var/run/docker.sock \
            -v $PWD:/output \
            singularityware/docker2singularity \
            rstan_lisco