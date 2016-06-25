# Analysing your first project with codeface
1. Create Basic analysis structure

	cd
	mkdir git-repos
	mkdir res
	cd git-repos
	git clone git://git.qemu.org/qemu.git

2. Start the ID service

	cd /vagrant/id_service
	node id_service.js ../codeface.conf > /tmp/id.log&

3. Run a project analysis

	cd $HOME
	codeface run -c /vagrant/codeface.conf -p /vagrant/conf/qemu.conf ~/res ~/git-repos/

Specify `codeface -j<N>` to use N cores and speed up processing.

4. Run the web frontend
* First time setup: Run
	cd /vagrant; bash shiny-server-pack.sh
* Start the frontend web server
	cd /vagrant; ./shiny-server.sh
* Access the web frontend via [http://localhost:8081](http://localhost:8081)
