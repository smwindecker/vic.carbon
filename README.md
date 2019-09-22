## Repository for drivers of Victorian freshwater carbon project

## Reproducing analysis 

You can reproduce the analysis, figures, tables, and manuscript for this project by running the code in the `analysis.R` file. To aid in the long-term reproducibility of this work, we have created a Docker image to enable others to reproduce these results using the same software and versions we used to conduct the original analysis. You can access a container hosted by [binder](mybinder.org) by clicking on the following badge: 

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/smwindecker/vic.carbon/master?urlpath=rstudio)

Alternatively, you can use docker on your local machine. Instructions at bottom on page. 

## Directories

* `ms/`: Manuscript file as well as other files to compile document including .bib files for bibliography.
* `R/`: R code to reproduce analysis, tables, and figures.
* `figs/`: Figures produced in the R scripts. 
* `data-raw/`: Raw data for analysis.
* `img/`: Images made with other tools to illustrate ideas. 

## License

This work is licensed under the [MIT license](https://opensource.org/licenses/MIT).

## Reproducing work with docker

### 1. Clone the repository on Github
First off, run the following line of code in the terminal to clone the project repository. 
```
git clone https://github.com/smwindecker/vic.carbon /home/vic.carbon
```

Since this repo contains large data files (>= 50MB), we leverage a Git plugin for large file storage that needs to be installed before you can clone this repo. You can download the plugin [`git-lfs`](https://git-lfs.github.com) using command `git lfs install` in the console.

### 2. Set up Docker

You'll need to install [docker](https://www.docker.com/get-docker), and then pull the image we have created from [dockerhub](dockerhub.com). You can do this in two ways: 

1. Pull the docker image we have already created by running the following in the terminal:

```
docker pull smwindecker/vic.carbon
```

### 3. Run the container

We will run the container in a new RStudio session and mount your local directory `/Users/path/to/vic.carbon` (you'll have to put your respective path here), into the container at `/home/vic.carbon`. Any results produced in the container will be automatically saved onto your local directory, so you can play with the results, data, and figures outside the docker container later.

*For Mac & Linux users*
```
docker run -v /Users/path/to/vic.carbon/:/home/vic.carbon:/home/rstudio -p 8787:8787 smwindecker/vic.carbon:latest
```

*For Windows users*
```
docker run -v c:\path\to\vic.carbon\:/home/vic.carbon:/home/rstudio -p 8787:8787 smwindecker/vic.carbon:latest
```

Access Rstudio within docker by opening your web browser and going to `localhost:8787/`. Username and password are both `rstudio`.

## Docker Image metadata

| Docker Hub Build Status and URL                                | Image Size
| :-----------------------------------------                     | :--------------
| [good](https://registry.hub.docker.com/u/smwindecker/vic.carbon/)  | [![Layers and Size](https://images.microbadger.com/badges/image/smwindecker/vic.carbon.svg)](https://registry.hub.docker.com/u/smwindecker/vic.carbon/)

Special thank you to [James Camac](https://github.com/jscamac) for assistance in setting up this reproducible workflow. 

## Problems?

If you have any problems getting the workflow to run please create an [issue](https://github.com/smwindecker/vic.carbon/issues) and I will endevour to remedy it ASAP.

