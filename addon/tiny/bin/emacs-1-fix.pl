#!/usr/local/bin/perl
#
# @(#) emacs-1-fix.pl -- Fix Emacs Lisp package first line to standard format
# @(#) $Id: emacs-1-fix.pl,v 1.1 2005-12-04 20:58:09 hute37 Exp $
#
#   File id
#
#	Copyright (C)	2000 Jari Aalto
#	Created:	2000-01
#	Keywords:	Perl text conversion
#	PerlVer:	5.004
#
#	$Contactid: jari.aalto@poboxes.com $
#	$Docid: 2001-12-10 Jari Aalto $
#
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License as
#	published by the Free Software Foundation; either version 2 of
#	the License, or (at your option) any later version.
#
#	This program is distributed in the hope that it will be useful, but
#	WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#	General Public License for more details.
#
#	You should have received a copy of the GNU General Public License along
#	with this program; if not, write to the Free Software Foundation,
#	Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#   Documentation
#
#	Fix first line string in Emacs files to following format:
#
#	    ;;; @(#) file.el --- One line description string
#
#	The attraction of this would be that there would be a simple means
#	of indexing all of the packages that are supplied with Emacs.
#
#   End


use strict;
use English;

sub Main ()
{
    my $id = "emacs-1-fix.pl";


    # .......................................... expand command line ...

    my @files;

    for ( @ARGV )
    {
	#	Win32 can't expand "*". We must do it here.
	#	Grep only FILES, not directories.

	push @files, grep { -f and /\.el$/ } glob $ARG;
    }



    local ( *FILE, $ARG );
    my    ( @content, $file);

    for $file ( @files )
    {

        # ..................................................... read ...

	unless ( open FILE, $file )
	{
	    print "$id: Cannot open $file\n";
	}
	else
	{
	    binmode FILE;
	    @content = <FILE>; close FILE;
	}

        # ...................................................... fix ...
	# fix first 2 lines.

	my $count;

	for ( @content )
	{
	    s/^;+/;;;/;
	    s/(\s+)-+(\s+)/ --- /;
	    $count++;
	    last if $count == 2;
	}

        # .............................................. replace old ...

	print $content[0];

	unless ( open FILE, ">$file" )
	{
	    print "$id: Cannot open for writing $file\n";
	}
	else
	{
	    binmode FILE;
	    print FILE @content; close FILE;
	}
    }
}

Main();

0;
__END__
