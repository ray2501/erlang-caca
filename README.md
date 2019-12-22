erlang-caca
=====

It is an Erlang bindings for [libcaca](http://caca.zoy.org/wiki/libcaca).
Now this project is at an early development stage.


Build
-----
To build erlang-caca you need a C compiler and `make`, and the development 
packages for libcaca. Below are the commands to install these on several 
operating systems.

Debian/Ubuntu

    sudo apt install -y build-essential libcaca-dev

openSUSE

    sudo zypper install -y libcaca-devel gcc make

How to build this package:

    $ rebar3 compile

