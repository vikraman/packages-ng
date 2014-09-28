Workflow
========

Initial setup
-------------

.. code ::

    $ . ./env.sh
    $ ./drop-and-create.sh
    $ cat migrations/*.sql | psql --quiet --no-psqlrc --single-transaction

Updating
--------

.. code ::

    $ . ./env.sh
    $ find migrations -type f -newer migrations/x-current.sql -print0 | sort -z | xargs -0 cat | psql --quiet --no-psqlrc --single-transaction

Working with Postgres
---------------------

.. code ::

    $ . ./env.sh
    $ psql
