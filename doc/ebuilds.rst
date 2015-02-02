Ebuilds
=======

Obtaining metadata
------------------

There are roughly 2 different ways to obtain an ebuild's metadata:

* Parsing
* Code execution

Parsing
~~~~~~~

Direct ebuild parsing may seem simple, but has the following disadvantages:

* Can't handle anything non-trivial (e.g. variables set in eclasses, if-else
  statements, etc.)
* Will be a bitch to maintain as EAPIs change

Code execution
~~~~~~~~~~~~~~

Here are some of the dangers of arbitrary code execution:

* DoS attacks (e.g. fork bombs, memory exhaustion, etc). This can be solved with
  cgroup limits
* Malicious (outbound) network activity. This can be solved with iptables and/or
  namespaces
* Privilege escalation

How does Portage handle the metadata phase?
-------------------------------------------

Here's a superficial explanation that's probably wrong:

#. Variables such as P/PF/PN/PV/etc. get set to their correct values.

   See `doebuild_environment()` in `pym/portage/package/ebuild/doebuild.py`

#. EAPI gets parsed from the ebuild with a regex.

   See `pym/portage/__init__.py / _parse_eapi_ebuild_head()`

#. EbuildMetadataPhase opens the ebuild, configures pipes/file handles, handles
   return codes, etc.

   See `pym/_emerge/EbuildMetadataPhase.py`

#. `doebuild()` gets called with `phase='depend'`.

   See `pym/portage/package/ebuild/doebuild.py`

#. `_spawn_phase()` creates an `EbuildPhase` object are runs it.

   See `pym/_emerge/EbuildPhase.py`

#. `ebuild.sh` gets spawned and echoes the relevant variables.

   See `bin/ebuild.sh`, search for 'auxdbkeys='.

The code is non-trivial and frequently surprising (at least for me).

Things to research
------------------

* `libbash <http://wiki.gentoo.org/wiki/Project:Libbash>`_
* eix's ebuild parser
* paludis/pkgcore's approaches to metadata handling
