
|rmdmoodle| documentation
=====================================


|rmdmoodle| is for (a) authoring moodle exams and (b) build question banks. Both are based on single `Rmarkdown <https://rmarkdown.rstudio.com/>`_ (`rmd`) files edited in RStudio and converted to `XML moodle <https://docs.moodle.org/401/en/Moodle_XML_format>`_ for importing in `moodle`. 

.. code:: R

   install.packages("devtools")
   library(devtools)
   install_github("jpcaveiro/rmdmoodle")




.. toctree::
   :maxdepth: 2
   :caption: Contents:

   moodle
   devel


.. usar o mermaid::




Rmd as a moodle exam
-----------------------



Explicit variants

It's an `Rmd` file built in RStudio.

.. code::

   ----
   title:
   ----

   # Question 1 - Descriptive Statistics - CLOZE

   ```{r}
   set.seed(10)
   datasample <- rnorm(8, mean=5, sd=19)
   ```

   ## Variant 1

   Consider the sample

   `r paste0(datasample, sep = '  ')`

   What is the mean of this sample?

   ## Variant 2

   Another student can see this variant


   # Question 2 - Inference in one variable - ESSAY


   ## Variant unique

   All students see this question.



Question set in file system
----------------------------


It's an `Rmd` file built in RStudio.

Each Rmd as the following structure:


.. code:: markdown

   ----
   title:
   ----

   # Code

   ```{r}
   #some calculations
   ```

   # Scenary CLOZE

   Some text describing the scenary.

   # Item 1 about the scenary

   Some question text describing the scenary.

   # Item 2 about the scenary

   Some question text describing the scenary.

   # Item 3 about the scenary

   Some question text describing the scenary.



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
