.. rmdmoodle documentation master file, created by
   sphinx-quickstart on Tue Mar 21 15:58:54 2023.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to rmdmoodle's documentation!
=====================================

`rmdmoodle` is for authoring moodle exams (and question banks) based on a single Rmd file edited RStudio (possibly splitting in several files). Or, in toher words, this package is to convert 

* a `Rmarkdown <https://rmarkdown.rstudio.com/>`_ (`rmd`) to `XML moodle <https://docs.moodle.org/401/en/Moodle_XML_format>`_ for importing in `moodle`. 


The following **mind set** is important in order to not waste time:

* there is some gratification in making an exam with the same contextual plot ("enredo", "trama"), meaning same dataset with several interestering and diverse variables;
* moodle exams are able to choose a question in a bank;
* warning: two students sit side by side could have the same random question.



.. toctree::
   :maxdepth: 2
   :caption: Contents:

   starting
   moodle
   rpackage
   sphinxdoc


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
