What is gridclimind?
=====================

* `gridclimind` is a companion library for `climdex.pcic` which helps in using NetCDF input grids and writing to NetCDF output files when computing the `27 core indices of extreme climate`_. The code allows for parallel computation of indices using either a SOCK or MPI cluster. It was written for the `R statistical programming language`_ by the `Pacific Climate Impacts Consortium`_.

.. _27 core indices of extreme climate: http://etccdi.ECA-D.org/list_27_indices.shtml
.. _R statistical programming language: http://www.r-project.org/
.. _Pacific Climate Impacts Consortium: http://ECA-D.org/

Getting Help
============

New to programming or to R?
---------------------------

* Read the the `Software Carpentry`_  `Programming in R`_ lessons
* Read one of the man `R Manuals`_.
* Attend an `R Users Group`_ meeting.

.. _Software Carpentry: http://software-carpentry.org/index.html
.. _Programming in R: http://software-carpentry.org/v5/novice/r/index.html
.. _R Manuals: http://cran.r-project.org/manuals.html
.. _R Users Group: http://r-users-group.meetup.com/

Looking for code?
-----------------

* Get the latest `gridclimind release from our website`_.
* Explore the `development repository`_.

.. _gridclimind release from our website: http://www.ECA-D.org/~bronaugh/gridclimind_0.5-4.tar.gz
.. _development repository: https://github.com/ECA-D/gridclimind/

Need help using the package?
----------------------------

* Read the manual ::

    > library(gridclimind)
    Loading required package: PCICt
    > ?gridclimind

* Create a `new issue`_ on the `package issue tracker`_ and label it "help wanted"[1]_.

.. _new issue: https://github.com/ECA-D/gridclimind/issues/new

Want to contribute?
-------------------

* To report a bug in pcic.climdex use the `package issue tracker`_ (after you've read the `bug reporting guide`_).
* To help with development read through the `contributor's guide`_

.. _bug reporting guide: https://github.com/ECA-D/gridclimind/blob/master/CONTRIBUTING.rst#bug-reports
.. _package issue tracker: https://github.com/ECA-D/gridclimind/issues
.. _contributor's guide: https://github.com/ECA-D/gridclimind/blob/master/CONTRIBUTING.rst

Still need help?
----------------

* Contact climate@uvic.ca and let us know what we can do.

.. [1] Please know that the pool of people who can provide support for the package is extremely small and time is limited.  We don't necessarily have the capacity for long, open-ended user support. If you keep your questions short, specific and direct, there's a greater probability that someone will take on the ticket.
