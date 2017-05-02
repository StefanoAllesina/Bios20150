# Bios20150

Material for the ecology portion of the class BIOS 20150 *How Can We Understand the Biosphere?*, taught at the University of Chicago in the Spring Quarter of 2017. The material was prepared by [yours truly](http://allesinalab.uchicago.edu/?page_id=2).

This repository contains slides, data and readings for each of the four themes we are going to explore in class:

- **Theme 1: Growth**
- **Theme 2: Tipping Points**
- **Theme 3: The Plague**
- **Theme 4: Interactions**

## Downloading the material

There are two options:
- If you're familiar with `git`, simply `clone` the repository 
(if you're not familiar with `git`, but want to learn, here's a [short tutorial](http://goo.gl/H1qfgr)) 

- If you don't want to bother with `git`, you can download a [zip file](https://github.com/StefanoAllesina/Bios20150/archive/master.zip) containing all the material. 
The only downside is that whenever the material is updated, you will need to download the zip file again :-(

## Browsing the material

You can also browse the material online:
### Here are the slides: 
- Theme 1 [RMarkDown](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_1/theme_1.Rmd) [Summary](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_1/theme_1_summary.pdf)
- Theme 2 [RMarkDown](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_2/theme_2.Rmd) [Summary](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_2/theme_2_summary.pdf)
- Theme 3 [RMarkDown](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_3/theme_3.Rmd) Summary
- Theme 4 [RMarkDown](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_4/theme_4.Rmd) Summary

The folder for each theme also contains the data and readings.

## Computable slides

The slides are in a special format, called `RMarkDown`. Basically, they can be "knitted" in `RStudio` to produce a dynamic `html` file, so that we can do real-time calculations and graphics. 

One cool feature, is that all the code for generating the figures, simulating the models, etc., can be run in `R` with a simple copy and paste. See for example the code for [Theme 2](https://github.com/StefanoAllesina/Bios20150/blob/master/theme_2/theme_2.Rmd).

To be able to knit the slides and run the code in `RStudio`, you need to install a number of packages, including `knitr`, `dplyr`, `ggplot2`, `reshape2`, `tidyr`, `deSolve`. Installing a package in `RStudio` [is quite easy](https://www.youtube.com/watch?v=u1r5XTqrCTQ).
