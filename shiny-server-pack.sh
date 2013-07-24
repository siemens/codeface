# This script sets up a self-contained version of the shiny server,
# downloading a dedicated version of the latest node.js and then
# installing the modified shiny-server from
# https://github.com/JohannesEbke/shiny-server/tree/no-su
# with all dependencies in the node.js directory.
# It also creates a shiny-server-pack.tar.gz for distribution.

# This script must be run on a server where HTTP and HTTPS connections
# to the internet are possible. For installation on other systems,
# copy the shiny-server-pack.tar.gz and untar it in the prosoda project
# directory.

export ARCH=x64
export NODEVER=0.10.13
export NODEVERSION=node-v${NODEVER}-linux-$ARCH

# --------
set -e
set -u
if [[ ! -e $NODEVERSION.tar.gz ]]; then
  wget http://nodejs.org/dist/v$NODEVER/${NODEVERSION}.tar.gz
fi

#rm -rf $NODEVERSION
tar xzf ${NODEVERSION}.tar.gz
$NODEVERSION/bin/npm install -g https://github.com/JohannesEbke/shiny-server/archive/no-su.tar.gz
tar czf shiny-server-pack.tar.gz $NODEVERSION shiny-server-pack.sh
