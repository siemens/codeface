Integration scripts
===================

Common scripts used by Vagrant and Travis CI to provision the VM required for
integration testing. The working directory for all scripts must be the codeface root directory.

The scripts are meant to be run in a Ubuntu 12.04 LTS 64bit environment.

Travis CI
---------
No further steps required.

Vagrant
-------
`vagrant up` Create a new VM and execute all provision scripts. If the VM already
exists, provisioning is skipped and the existing VM is started instead.

`vagrant ssh` Open a SSH session with the VM. Codeface is per default mounted
from the host to `/vagrant` and owned by the user `vagrant`.

`vagrant halt` Stop a running VM.

`vagrant destroy` Destroy the VM and delete from file system.