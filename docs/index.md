# rmdmooodle R pckage

Based on RStudio [RMarkdown](https://rmarkdown.rstudio.com/) (`Rmd`) 

* Build a Moodle exam (moodle exam xml file) 
* Build a question set in file system

and mix both concepts.

## Install

```r
install.packages("devtools")
library(devtools)
install_github("jpcaveiro/rmdmoodle")
```

## Moodle exam

It's an `Rmd` file built in RStudio.

```
----
title:
----

# Question 1 - Descriptive Statistics - CLOZE

\`\`\`{r}
#some calculations
\`\`\`

## Variant 1

A student can see this variant

## Variant 2

Another student can see this variant

# Question 2 - Inference in one variable - ESSAY


## Variant unique

All students see this question.
```


## Question set in file system

It's an `Rmd` file built in RStudio.

Each Rmd as the following structure:

```
----
title:
----

# Code

\`\`\`{r}
#some calculations
\`\`\`

# Scenary CLOZE

Some text describing the scenary.

# Item 1 about the scenary

Some question text describing the scenary.

# Item 2 about the scenary

Some question text describing the scenary.

# Item 3 about the scenary

Some question text describing the scenary.

```

