#! /bin/sh -x -u -e
#
# Get latest version of org-mode in ~/git/orgmode.org/org-mode
#

WHERE="${HOME}/.emacs.d"
mkdir -p ${WHERE}
cd ${WHERE}

# pull for first time if we don't have it

if [ ! -d ${WHERE}/org-mode-ox-odt ]; then
    git clone git://orgmode.org/org-mode-ox-odt.git
else
    # get updates, if any
    cd ${WHERE}/org-mode-ox-odt
    git pull
fi

# build the autoloads
cd ${WHERE}/org-mode-ox-odt
make
make autoloads

#(add-to-list 'load-path "~/.emacs.d/org-mode-ox-odt/lisp/")
