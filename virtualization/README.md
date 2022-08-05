# How to use the docker/singularity image

## 1. What's included here?
------------------------

- `neurodocker_generate_images.sh`

    This script generates the docker and singularity images for the
    Francesco-Rstan project using neurodocker. Ie it specifies the base
    OS and software to be installed. This will be written to a Dockerfile.
    It will then build the docker image locally, save it to a local file and 
    subsequently convert it to a singularity image. 
    The last parts can be commented out.

- `install_R_env.r`

    This script installs the R environment and packages required for the
    Francesco-Rstan project. This includes the setup of C-related aspects.
    Importantly, this script is copied into the docker image during the build
    so that I can be run within the virtual environment during the build.

- `Dockerfile`

    The Dockerfile created via neurodocker_generate_images.sh and used to build
    the docker image.

- `rstan_lisco.tar.gz`

    The built docker image, saved as a tar.gz file. This can be shared with other
    folks and utilized via the "docker load command".

- `rstan_lisco-2022-08-04-0cee7182d6c0.simg`

    The built docker image converted to a singularity image, saved as a .simg file. 
    This can be shared with other folks and utilized via singularity.

## 2. How to use this?


The image is currently setup to automatically run the "`Rscript`" command upon startup. This means that it expects an `R script`, e.g. `parameter_recovery.R`, as input which will then be run. To make this work, the path to the respective `code` needs to be specified and mapped accordingly. Here's an general example:

```
docker run -it --rm -v path/to/the/code:/code \
                       Rstan_lisco /code/parameter_recovery.R "simulate_HMM" "HMM_repar"
```

where `path/to/the/code` is the `path` to the `code` that should be available inside the `docker/singularity image` and parameter_recovery.R is a `script` that is stored inside this `path`. This will result in the `code` being run inside the `docker/singularity image`.

Here's a more specific example based on my setup:

```
docker run -it --rm -v /Users/peerherholz/Desktop/francesco-rstan:/code \
                        Rstan_lisco /code/parameter_recovery.R "simulate_HMM" "HMM_repar"
```

Here, all relevant `code` is stored in the folder "`francesco-rstan`" on my `desktop`. With `singularity` this should roughly look like this:

```
singularity run -B /Users/peerherholz/Desktop/francesco-rstan:/code \
                    Rstan_lisco /code/parameter_recovery.R "simulate_HMM" "HMM_repar"
```


## 3. What to look out for?


- `automatic Rscript execution vs. interactive exploration/testing`

    The image is currently setup to automatically run the "Rscript" command upon startup. Thus, if you want to play around with/inside the environment you would need to adapt the command as follows:

    ```
    docker run -it --rm -v path/to/the/code:/code --entrypoint bin/bash Rstan_lisco 
    ```

    which will then bring you into a classic bash shell inside your image so that you can work interactively with it. Please note that this most likely won't work on the `HPC`.

- `paths, paths, paths & mounts/mapping`

    As mentioned above, you need to specify the `path` to the `code` that should be available inside the `docker/singularity image` and `map` it accordingly. Based on how the project is setup with its subfolders and functions that are sourced, you would need to adapt your code and functions respectively. More precisely, you need to prepend the mapped path to your source and other functions (at least this was the case on my end). Here's an example:

    If you run the `image` as follows, mapping the `code` outside the `image` to a directory called "`/code`" inside the `image`:   

    ```
    docker run -it --rm -v path/to/the/code:/code \
                        Rstan_lisco /code/parameter_recovery.R "simulate_HMM" "HMM_repar"
    ```

    then the respective parts of your `R code` would need to changed from this:

    ```
    source("helper_functions/taskSim.R")
    ```

    to this:

    ```
    source("/code/helper_functions/taskSim.R")
    ```

    The same holds true for all other sections where you specify `paths`, e.g. `outputs`, etc. .

- `versions, versions, versions`

    Please make sure to utilize **specific versions** of the software that you are using. Specifically, the `R-packages`, as things might become rather unreproducible and the `image` `build process` might
    not work in the future (because of version issues).

## 4. Further questions?

Please feel free to reach out via email or `issues` in this repository.