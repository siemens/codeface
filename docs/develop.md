# Notes on Codeface development

## Wiki Ressources
* Codeface is developed as an open source project; since the system is complex
   to set up, the preferred way of using it is via vagrant (installation hints
   are given in the [wiki](https://github.com/siemens/codeface/wiki/Runnning-codeface-with-Vagrant).
* The wiki also contains a [style guide](https://github.com/siemens/codeface/wiki/Style-Guide), and
   [guidelines](https://github.com/siemens/codeface/wiki/How-to-structure-commits) on how to
  structure commits.

## Generating HTML Documentation
The code base includes some (albeit limited) documentation of the internal functions,
classes etc.

* To generate the Sphinx documentation for the codeface python classes, go 
  to `/vagrant` and run:

        python setup.py build_sphinx 

The resulting documentation is found in `/vagrant/build/sphinx/html`
* To generate the python HTML documentation, run `python setup.py`. 
