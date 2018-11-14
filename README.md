cpns
====

This package contains the implementation of the
Curry Port Name Server (CPNS) and related libraries.

The CPNS is used to implement libraries which supports
network connections over ports with symbolic names.

To implement a connection to a port with symbolic name `pn`,
CPNS (running on the remote machine of `pn`) is asked
to get the physical socket number of this port.
In order to connect to CPNS from any machine in the world,
the CPNS demon always listens at the port `Network.CPNS.cpnsSocket`,
which is defined as `8769`.

The executable `curry-cpnsd` installed by this package
is the CPNS demon. It understands the following commands:

* Start the CPNS demon on the local machine:

    > curry-cpnsd start

  Usually, this is automatically done when a new port is registered.

* Terminate the CPNS demon on the local machine:

    > curry-cpnsd stop

  Usually, this is not necessary...

* Show status information (memory and currently registered ports):

    > curry-cpnsd status

  Exits with error message and status if CPNS demon is not running.

* Show the complete log file:

    > curry-cpnsd log


Auxiliary files (locations defined in `Network.CPNS.Config`):
-------------------------------------------------------------

/tmp/Curry_CPNSD.log : the log file of CPNSD


Required commands:
------------------

The implementation uses the following system commands:

    touch
    chmod
    ps
    nohup
    lockfile-create
    lockfile-remove
