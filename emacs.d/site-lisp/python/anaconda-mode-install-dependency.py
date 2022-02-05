# anaconda-mode depends on:
# service_factory
# jedi

# by default, anaconda-mode will install them from pypi you into ~/.emacs.d/anaconda-mode/0.1.6/.

# If the dependencies are already installed, avoid installing again by adding the following empty
# directories to the directory stored in (anaconda-mode-server-directory) (typically
# ~/.emacs.d/anaconda-mode/<version>)

# manual method with easy_install.
# The downloaded files from pypi were placed into '/home/user/anaconda_mode_pkg/'

import os, sys
from setuptools.command import easy_install
version = "0.1.6"
directory = os.path.join("/home/user/.emacs.d/anaconda-mode/", version)
sys.path.append(directory)
local_package_dir = '/home/user/anaconda_mode_pkg/'
easy_install.main(['-d', directory,            # install package to DIR
                   '-S', directory,            # list of directories where .pth files work
                   '-a',                       # Always copy all needed packages to install dir
                   '-Z',                       # don't install as a zipfile, no matter what
                   '-f', local_package_dir,    # additional URL(s) to search for packages
                   'anaconda_mode==' + version])
