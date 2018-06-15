
![logo](/img/violins/pokemon_logo.png?raw=true "Title")
***
## Overview
**| Violin Plots | Split Violins | Bean Plots | Pirate plots |**

This is a quick blog and git repo to help you create violin type plots using R in a few different ways using the Pokemon dataset.


1) **Bean Plots with `geom_violin`**; 
  Simple bean/violin plots with individual data points using ggplot2. Including
  demonstration on how to change the order and colour of groups.
  
2) **Pirate Plots with `YaRrr` package**; 
  Example of the YaRrr package for RDI type plots with IQR. \n Pirate plots have been coined "RDI plots" (Raw data, Descriptive statistics and Inferential statistics). You can download the yarrr package from [github](www.github.com/ndphillips/yarrr), which also has some great [documentation](https://bookdown.org/ndphillips/YaRrr/) from the talented Dr. Nathaniel Phillips.
  
3) **Split Violin Plot Function** ; 
 A function I have built upon to make split violin plots using ggplot2 with lots of customisation potential for those less fluent with ggplot2; customisation such as; 
  - change colours
  - change order on plots
  - adding summary counts
  - setting a theme

### Why use violin/bean/RDI plots? 
The median, mean and IQR aren’t always enough to understand a dataset. Violin/bean type plots are becoming a more popular way of visualising data over common bar and boxplot. The main advantage with these types of plots is that anomalies in the data are easier to visualise; including multimodal distributions and duplicate values. See these great resources on the detailed advantages of violin/bean plots.
[Beanplot package](https://cran.r-project.org/web/packages/beanplot/vignettes/beanplot.pdf); documentation; not used in this blog post, but also includes a split beanplot function


## Install
Prerequisites:

* [Download and install Git](https://git-scm.com/downloads); for git install
* [Download and install R](https://www.r-project.org/)
* [Download and install Rstudio](https://www.rstudio.com/)

You can choose from one of the following methods to install scripts and this markdown file:

#### Install with Git
Install on your computer using Git with the terminal

1. [clone](https://github.com/sourcethemes/academic-kickstart#fork-destination-box) the *Violin plots* repository with Git: 

        mkdir violin_plots 
        git clone https://github.com/bakertina/violin_plots.git violin_plots
        cd /violin_plots/

#### Install with ZIP
Install on your computer by downloading the ZIP files

1. [Download](https://github.com/bakertina/violin_plots) and extract *Violin plots*


***
