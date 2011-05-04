#!/usr/local/bin/perl
#
# @(#) copyright-update.pl -- Update copyright year
# @(#) $Id: copyright-update.pl,v 1.1 2005-12-04 20:58:09 hute37 Exp $
#
#   File id
#
#	Copyright (C)	2000-2002 <jari.aalto@poboxes.com>
#	Created:	2000-01
#	Keywords:	Perl text conversion
#	PerlVer:	5.004
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
#	This program will update the year part of the copyright line.
#
#	    Copyright (C)	2000-2001
#
#       =>
#
#           Copyright (C)	2000-2002
#
#	The Copyright year can be passed as command line option. If no
#       option is given, current year is used.
#
#   End


use strict;
use English;
use Getopt::Long;
use autouse 'Pod::Text' => qw( pod2text );


    my $LIB = "copyright-update.pl";

    use vars qw ( $VERSION );

    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
    #   So that it puts the tardist number in format YYYY.MMDD
    #   The REAL version number is defined later
    #
    #   The following variable is updated by Emacs setup whenever
    #   this file is saved. See Emacs module tinymy.el where this
    #   feature is implemented, available at
    #   http//tiny-tools.sourceforge.net

    $VERSION = '2001.1225';



=pod

=head1 NAME

copyright-update.pl - Update Copyright year information

=head1 README

This program will update the copyright year information for
given files. The year is current year unless passed with --year YEAR
option.

   % perl copyright-update.pl --verbose --test --year 2002 *

Only files that contain lines

   Copyright (C)	2000-2001
   Copyright: (C)	2000-2001

Are updated. The format must be exatly as show above; Different
amount of spaces is permitted, but the YEAR-YEAR must be kept
together in files.

=cut

sub Help ()
{
    my $id   = "$LIB.Help";
    my $msg  = shift;  # optional arg, why are we here...
    my $type = shift;  # optional arg, type

    pod2text $PROGRAM_NAME;

    exit 1;
}


sub Year ()
{
    my $id = "$LIB.Year";
    1900 + (localtime time())[5];
}


sub HandleCommandLineArgs ()
{
    my $id = "$LIB.HandleCommandLineArgs";

    use vars qw
    (
        $YEAR
        $test
        $verb
    );

    Getopt::Long::config( qw
    (
        require_order
        no_ignore_case
        no_ignore_case_always
    ));

    my ( $help );

    GetOptions      # Getopt::Long
    (
          "year=i"   => \$YEAR
        , "help"     => \$help
        , "test"     => \$test
        , "verbose"  => \$verb
    );

    $help and  Help();

    $YEAR = Year()  unless defined $YEAR;

    unless ( $YEAR =~ /^\d{4}$/ )
    {
	die "$id: --year must be four digits [$YEAR]";
    }


    $verb = 1  if $test;

}

sub Main ()
{
    my $id = "$LIB.Main";

    HandleCommandLineArgs();

    unless ( @ARGV )
    {
	die "What files to change? See --help.";
    }

    # .......................................... expand command line ...

    my @files;

    for ( @ARGV )
    {
	#	Win32 can't expand "*". We must do it here.
	#	Grep only FILES, not directories.

	push @files, grep { -f  } glob $ARG;
    }



    local ( *FILE, $ARG );

    for my $file ( @files )
    {

        # ..................................................... read ...

	unless ( open FILE, "< $file" )
	{
	    print "$id: Cannot open $file\n";
	}
	else
	{
	    binmode FILE;
	    $ARG = join '', <FILE>; close FILE;
	}

	my $yy      = '\d{4}';
	my $lead    = 'Copyright:?[ \t]+\([Cc]\)[ \t]+' . $yy . '-';

	#  If we find the LEAD, then check if YEAR is different
	#  and finally do substitution.
	#
	#  If everything went ok, replace file.

	my $y;

	if ( /$lead($yy)/i
	     and  ($y = $1) ne $YEAR
	     and  s/($lead)($yy)/$1$YEAR/gi
	   )
	{
	    my $msg = "Changed";

	    $test  and  $msg = "Would change";
	    $verb  and  print "${msg} $file $y => $YEAR\n";
	    $test  and  next;

	    unless ( open FILE, ">$file" )
	    {
		print "$id: Cannot open for writing $file\n";
	    }
	    else
	    {
		binmode FILE;
		print FILE $ARG;
		close FILE;
	    }

	}

    }
}

Main();

0;
__END__
