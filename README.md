# To download pre-built maps

There are several listed in [the Unrest topic on
minecraftforum](http://).


# To download the map generator

## Windows

**Important: You must have 64-bit Windows with 64-bit Java installed
to run the Unrest map generator.**

[download unrest_Win64.zip (contains "unrest1.exe")](http://)


## Mac

This should work on any recent version of Mac OS X.

[download unrest_Mac.zip (contains "Unrest map generator")](http://)

The application is not signed, so you will need to right click it and
select Open on Lion or Mountain Lion.  Because the source code is
freely available, anyone can inspect it to confirm there is no
malware, and confirm that it builds to the same jar contained in this
app bundle, but if you are not comfortable running an unsigned
application, you may run from the source code (see instructions
below), or just download pre-generated maps (see link above).


## Linux and all other platforms with Java

Again, you need a 64-bit OS and 64-bit Java.

[download unrest_Other.jar.gz](http://)

In a shell, go to the directory containing the downloaded file and run
the following:

    $ gunzip unrest_Other.jar.gz
    $ java -Xmx4g unrest_Other.jar

# To get the source code and run it:

1. [Install leiningen](http://leiningen.org/)
2. [Install git](http://git-scm.com/) if you don't have it (or
   download from github and skip to step 4)
3. Run `git clone https://github.com/shiftniner/unrest.git` to clone
   the unrest git repository
4. Run the ./run.sh script (Linux/Mac) or .\run.bat batch file
   (Windows)

