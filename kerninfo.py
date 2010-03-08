# Subsystem characterisations for various Unix kernels
# (following kerninfo.pm from the Perl scripts)

# Subsystems are characterised by a dictionary with the subsys
# categories as keys. Each value is a list of all directories that comprise 
# the subsystem. None must be used if the subsystem does
# not exist, but this fact is of importance (for instance in a comparison
# of similarly structured systems)

# TODO: For all kernels except Linux, the block layer is missing!

subsysDescrLinux = {
    "core" : ["kernel", "ipc", "virt", "security" ], 
    "drivers" : ["drivers", "sound"], 
    "net" : ["net"],
    "mm" : ["mm"], 
    "block": ["block"],
    "library" : ["lib"],
    "headers" : ["include"],
    "arch" : ["arch"], 
    "security" : ["crypto", "security"], 
    "contrib" : None, # External contributions to Linux remain external 
    "scripts" : ["scripts"], 
    "fs" : ["fs"], 
    "virt" : ["virt"], 
    "docs" : ["Documentation"] }

