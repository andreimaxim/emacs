# Emacs Configuration

## Setup

### Install the packages required for compiling Emacs from source:

```shell
$ sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev \
    libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo autoconf libjansson4 \
    libjansson-dev libgccjit0 libgccjit-10-dev gcc-10 g++-10
```

### Install additional packages, required by the configuration:


```shell
$ sudo apt install ripgrep
```

### Install packages for Windows

In the WSL2 distribution, add the full theme to remove the error messages:

```shell
$ sudo apt install adwaita-icon-theme-full
```

Then install win32yank (most likely through a package manager like Chocolatey).


## Installation

Setup the default compiler to point to gcc-10:

```shell
$ export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
```

Clone the Emacs repository:

```shell
$ git clone git://git.sv.gnu.org/emacs.git
```

```shell
$ cd emacs
```

Check out the branch for v29, if it's not already checked out:

```shell
$ git checkout -b emacs-29
```

If the directory is already cloned, reset its state:

```shell
$ git clean -fdx
```

Configure Emacs with the required features:

```shell
$ ./autogen.sh
$ ./configure --with-native-compilation --with-json --with-rsvg --with-xml2 --with-pgtk
```

Build, using as many cores as possible:

```shell
$ make --jobs=$(nproc)
```

Finally, install Emacs locally:

```shell
$ sudo make install
```

