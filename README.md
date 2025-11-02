# Assignment B1 – Making a Function in R

## Overview

This repository contains all files for **Assignment B1** of the UBC STAT545B course.  
The goal of this assignment is to **design, document, and test a custom R function** following tidyverse and reproducible workflow standards.


## Files Included

 - *Assignment B1.Rmd*
 - *Assignment B1.md*
 - *README.md*

## What I Did

1. **Created a function – `summarize_by_group()`**  
   - Purpose: To summarize how many observations belong to each category of a selected variable,  
     and calculate each category’s proportion in the total dataset.  
   - Built using the **tidyverse** style for clarity and reproducibility.  
   - Handles:
     - Missing values (`na.rm = TRUE/FALSE`)
     - Empty datasets (`nrow = 0`)
     - Input validation (checks data type and variable existence)

2. **Documented the function**  
   - Used **roxygen2** syntax with `@param`, `@return`, and description tags.  
   - Clear and concise naming conventions following tidyverse guidelines.

3. **Tested the function**  
   - Used **testthat** package with **three non-redundant tests**



[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/ZfRSGEKK)
