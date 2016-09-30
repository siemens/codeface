# Web server setup 
There are two options to set up an instance of the web frontend server:

* Using a self-contained tarball prepared on a machine with proper 
  internet connection (to be deployed on machines without network 
  access or behind restrictive corporate firewalls):
  Run `bash shiny-server-pack.sh`, copy the resulting 
  `shiny-server-pack.tar.gz` to the destination machine and unpack 
  it into $CFDIR. Start the server with `shiny-server.sh`. 

* Global installation: Run 

        sudo -E npm install -g \
        https://github.com/JohannesEbke/shiny-server/archive/no-su.tar.gz 

  to install shiny server (respectively the customised version which 
  supports operation without root privileges) into the global 
  node package repo. Start with 

        shiny-server shiny-server.config 

  in `$CFDIR`. 

  In the default configuration, the web frontend is available 
  on [http://localhost:8081/](http://localhost:8081/). Notice that this port is by default forwarded to the
  host machine for vagrant based installations. 
