# .......................................................................
#
#   $Id: admin.bashrc,v 1.1 2005-12-04 20:58:09 hute37 Exp $
#
#   These bash functions will help managing Sourceforge project. You need:
#
#	bash        (Unix)  http://www.fsf.org/directory/bash.html
#		    (Win32) http://www.cygwin.com/
#	Perl 5.4+   (Unix)  http://www.perl.org
#		    (Win32) http://www.ativestate.com or Cygwin perl
#	t2html.pl   Perl program to convert text -> HTML
#		    http://www.cpan.org/modules/by-authors/id/J/JA/JARIAALTO/
#
#   This file is of interest only for the Admin or Co-Developer of
#   project:
#
#	http://sourceforge.net/projects/tiny-tools
#	http://tiny-tools.sourceforge.net/
#
#   Include this file to your $HOME/.bashrc and make the necessary
#   modifications
#
#	SF_TINY_TOOLS_USER=<sourceforge-login-name>
#	SF_TINY_TOOLS_USER_NAME="FirstName LastName"
#	SF_TINY_TOOLS_EMAIL=<email address>
#	SF_TINY_TOOLS_ROOT=~/cvs-projects/tiny-tools
#
#	source ~/cvs-projects/tiny-tools/bin/admin.bashrc
#
# .......................................................................

function sfttinit ()
{
    local id="sfttinit"

    local url=http://tiny-tools.sourceforge.net/

    SF_TINY_TOOLS_HTML_TARGET=${SF_TINY_TOOLS_HTML_TARGET:-$url}
    SF_TINY_TOOLS_KWD=${SF_TINY_TOOLS_KWD:-"Emacs, library, package"}
    SF_TINY_TOOLS_DESC=${SF_TINY_TOOLS_DESC:-"Emacs documentation"}
    SF_TINY_TOOLS_TITLE=${SF_TINY_TOOLS_TITLE:-$SF_TINY_TOOLS_DESC}
    SF_TINY_TOOLS_ROOT=${SF_TINY_TOOLS_ROOT:-"."}

    if [ "$SF_TINY_TOOLS_USER" = "" ]; then
       echo "$id: Identity SF_TINY_TOOLS_USER unknown."
    fi


    if [ "$SF_TINY_TOOLS_USER_NAME" = "" ]; then
       echo "$id: Identity SF_TINY_TOOLS_USER_NAME unknown."
    fi

    if [ "$SF_TINY_TOOLS_EMAIL" = "" ]; then
       echo "$id: Address SF_TINY_TOOLS_EMAIL unknown."
    fi
}



function sfttdate ()
{
    date "+%Y.%m%d"
}

function sfttfilesizeAwk ()
{
    # This was old implementation. Found better bash solution.
    ls -la $1 | awk '{print $5}'
}


function sfttfilesize ()
{
    #	put line into array ( .. )

    local line
    line=($(ls -l "$1"))

    #	Read 4th element from array
    #	-rw-r--r--    1 root     None         4989 Aug  5 23:37 file

    echo ${line[4]}
}


function sftt_lispdoc ()
{
    local id="sftt_lispdoc"

    #	Rip all documentation from lisp files
    #	and update tiny-tools.txt

    local dir=$SF_TINY_TOOLS_ROOT/lisp/tiny
    local out=$SF_TINY_TOOLS_ROOT/doc/txt/emacs-tiny-tools

    local out1=${out}-part1.src
    local out2=${out}-part2.src
    local final=${out}.txt

    (
	cd $dir
	perl -S ripdoc.pl $(ls *.el | sort) > $out2
	cat $out1 $out2 > $final
    )

    echo "$id: $final done."
}


function sfttscp ()
{
    local id="sfttscp"

    #	To upload file to project, call from shell prompt
    #
    #	    bash$ sfttscp <FILE>

    local sfuser=$SF_TINY_TOOLS_USER
    local sfproject=t/ti/tiny-tools

    if [ "$sfuser" = "" ]; then
	echo "$id: SF_TINY_TOOLS_USER is not set."
	return;
    fi

    scp $* $sfuser@shell.sourceforge.net:/home/groups/$sfproject/htdocs/
}

function sftthtml ()
{
    local id="sftthtml"

    #	To generate HTML documentation located in /doc directory, call
    #
    #	    bash$ sftthtml <FILE.txt>
    #
    #	To generate Frame based documentation
    #
    #	    bash$ sftthtml <FILE.txt> --html-frame
    #
    #	For simple page, like README.txt
    #
    #	    bash$ sftthtml <FILE.txt> --as-is

    local input="$1"
    shift

    if [ "$input" = "" ]; then
        echo "id: usage is FILE [html-options]"
        return
    fi


    if [ ! -f "$input" ]; then
	echo "$id: No file found [$input]"
	return
    fi


    local opt=$*

    echo "$id: Htmlizing $input $opt"

    #	perl -S  will work both in Unix and Win32. The -S causes
    #	perl to search $PATH
    #
    #	This option was removed
    #
    #              --button-top $SF_TINY_TOOLS_HTML_TARGET


    perl -S t2html.pl                                               \
	  $opt                                                      \
	  --title  "$SF_TINY_TOOLS_TITLE"                           \
	  --author "$SF_TINY_TOOLS_USER_NAME"                       \
	  --email  "$SF_TINY_TOOLS_EMAIL"                           \
	  --meta-keywords "$SF_TINY_TOOLS_KWD"                      \
	  --meta-description "$SF_TINY_TOOLS_DESC"                  \
	  --Out                                                     \
	  $input

    if [ -d "../../html/"  ]; then
	mv *.html ../../html/
    elif [ -d "../html/"  ]; then
	mv *.html ../html/
    else
	echo "$id: Can't move generated HTML to html/"
    fi

}

function sftthtmlall ()
{
    local id="sftthtmlall"

    #	loop all *.txt files and generate HTML
    #	If filesize if bigger than 15K, generate Framed HTML page.

    local dir=$SF_TINY_TOOLS_ROOT/doc/txt

    (
	cd $dir || return
	echo "$id: Source dir " $(pwd)

	for file in *.txt;
	do

	    local opt=""

	    local size=$(sfttfilesize $file)

	    if [ $size -gt 15000 ]; then
	       opt=--html-frame
	    fi

	    #	Some files's NAME tags must be numbered sequentially

	    if [ $file = "emacs-tiny-tools.txt" ]; then
		opt="$opt --name-uniq"
	    fi

	    sftthtml $file "$opt"

	 done
    )

    echo "$id: done."

}

function sftt_linkcheck ()
{
    # Check if the URL links are valid

    local id="sftt_linkcheck"
    local cache="--Link-cache /tmp/sftt_link.cache"
    local cmd="perl -S t2html.pl --Link-check-single $cache --quiet"

    cd $SF_TINY_TOOLS_ROOT/doc/txt;

    for file in *.txt;
    do
	echo "$id: $cmd $SF_TINY_TOOLS_ROOT/doc/txt/$file"
	$cmd $file
    done

}


function sftt_release ()
{
    local id="sftt_release"

    #	TYPE is tgz  bz2  or zip

    local type=$1

    local opt=-9
    local cmd=""
    local ext1=""
    local ext2=""


    case $type in
     tar.gz|tgz|gz) type=tar
		    ext1=.tar
		    ext2=.gz
		    cmd=gzip
		    ;;
	 bz|bz2)    type=tar
		    ext1=.tar
		    ext2=.gz
		    cmd=bzip2
		    ;;
	    zip)    type=zip
		    ext1=.zip
		    opt="-9 -r"
		    cmd=zip
		    ;;
	     *)	    echo "$id: ERROR, unknown release type [tgz|bz2|zip]"
		    return
		    ;;
    esac


    local dir=/tmp

    if [ ! -d $dir ]; then
	echo "$id: Can't make release. No directory [$dir]"
	return
    fi

    if [ ! -d "$SF_TINY_TOOLS_ROOT" ]; then
	echo "$id: No SF_TINY_TOOLS_ROOT [$SF_TINY_TOOLS_ROOT]"
	return
    fi


    local base=emacs-tiny-tools
    local ver=$(sfttdate)
    local tar="$base-$ver$ext1"
    local file="$base-$ver$ext1$ext2"

    if [ -f $dir/$file ]; then
	echo "$id: Removing old archive $dir/$file"
	( rm $dir/$file )
    fi

    (


	local todir=$base-$ver
	local tmp=$dir/$todir

	if [ -d $tmp ]; then
	    echo "$id: Removing old archive directory $tmp"
	    rm -rf $tmp
	fi


	cp -r $SF_TINY_TOOLS_ROOT $dir/$todir

	cd $dir

	if [ "$type" = "tar" ]; then

	    find $todir -type f                     \
		\( -name "*[#~]*"                   \
		   -o -name ".*[#~]"                \
		   -o -name ".#*"                   \
                   -o -name "*elc"                  \
                   -o -name "*tar"                  \
                   -o -name "*gz"                   \
                   -o -name "*bz2"                  \
                   -o -name .cvsignore              \
		\) -prune                           \
		-o -type d \( -name CVS \) -prune   \
		-o -type f -print                   \
		| xargs tar cvf $dir/$tar

	    echo "$id: Running $cmd $opt $dir/$tar"

	    $cmd $opt $dir/$tar

	elif [ $type = "zip" ]; then

	    $cmd $opt $dir/$file $todir		\
	    -x  \*/CVS/\* CVS/\*		\
	        .cvsignore \*/.cvsignore	\
		\*.zip \*.gz \*.elc \*.tar	\
		\*.gz \*.bz2			\
		.#\* \*.#\*			\
		\*~ \*#

	fi

	echo "$id: Made release $dir/$file"
	ls -l $dir/$file
    )

}


dummy=$(sfttinit)			# Run initializer


export SF_TINY_TOOLS_HTML_TARGET
export SF_TINY_TOOLS_KWD
export SF_TINY_TOOLS_DESC
export SF_TINY_TOOLS_TITLE
export SF_TINY_TOOLS_ROOT


# End of file
