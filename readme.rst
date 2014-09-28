packages-ng
===========

The next generation Gentoo package browser / search utility.

<insert screenshot here>

Docs
----

* `Original discussion (private) <https://docs.google.com/document/d/1-w9hBi0Ae-cvN4JYiaa4ZD5jmcmprtluvIZfglPSAsM/>`_

* `Project plan (private) <https://docs.google.com/document/d/1uzSeft-KzMActMYmNt4MUMXxTRWD9jzzrBHLK-MbnSs/>`_

Projects we aim to replace/combine
----------------------------------

* `gstats <git://anongit.gentoo.org/gstats>`_
* `gentoo-smolt <git://git.goodpoint.de/smolt-gentoo.git>`_
* `gentoostats 1 <https://github.com/vikraman/gentoostats>`_
* `gentoostats 2 <https://github.com/gg7/gentoostats>`_
* packages 1
* `packages 2 <git://anongit.gentoo.org/packages>`_
* `gentoo-packages <https://github.com/bacher09/gentoo-packages>`_

Building
--------

.. code ::

    $ cabal sandbox init                               # Initialise the sandbox
    $ cabal install --only-dep --jobs --enable-tests   # Install dependencies into the sandbox
    $ cabal build                                      # Build packages-ng inside the sandbox
    $ cabal test --show-details=streaming

License
-------

`GPL 2 <LICENSE>`_
