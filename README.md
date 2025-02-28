
# Task Assignment Script

## Purpose

This script assigns tasks to workers based on their scores while respecting maximum SGSE weight limits for each worker.

## Description

The script uses a task assignment function to maximize scores while ensuring that each worker does not exceed their maximum SGSE weight. The function takes into account the scores and maximum SGSE weights for each worker, and assigns tasks accordingly.

## Usage

To use this script, simply execute the `assign_tasks` function with the required input parameters:

* `input_filepath`: the file path to the input Excel file containing task scores and maximum SGSE weights
* `num_workers`: the number of workers to assign tasks to
* `scores_sheet`: the name of the sheet in the input Excel file containing task scores
* `max_sgse_sheet`: the name of the sheet in the input Excel file containing maximum SGSE weights
* `score_adjust_coef`: the score adjustment coefficient to use when assigning tasks

### Example

```r
assign_tasks('manual_input.xlsx', 5, "scores", "max_sgse", 0)
```

This will assign tasks to 5 workers based on the scores and maximum SGSE weights in the `manual_input.xlsx` file.

## Requirements

* R programming language
* `tidyverse` package
* `openxlsx` package

## Installation

To install the required packages, install `librarian` only:

```r
install.packages("librarian")
```

