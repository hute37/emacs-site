#!/usr/local/bin/perl
'di';
'ig00';
#
#
# Program to do a DOS like directory tree
#
# By Brian Blackmore
#

=pod

if ( $ARGV[1] )    
{
    $dir = &resolve($ARGV[1],$ENV{'PWD'});
} 
else    
{
    $dir =  &resolve(".",$ENV{'PWD'});
}

=cut

$dir = $ARGV[0] || ".";

print "$dir\n";


&tree($dir,"");

# This function resolves a pathname into its shortest version
# Removing any references to the directory . , any references
# to // , any references to directory/.. and any final /
sub resolve {
    local($file,$direct) = @_;

    $_ = $file;
    m#^/# || s#^#$direct/#;
    while (s#/\.?/#/# || s#/[^/]+/\.\./#/# || s#/\.?$##){   }
    $_ = '/'    if ($_ eq "");
    return $_;
}

# This function does all the work, it scans a directory and
# then prints out the files in each directory in a pretty format
# Note: It is recursive
sub tree    {
    local($dir,$level) = @_;
    local(@files) = ();

    if (!opendir(DIRECT,"$dir"))    {
        warn "Could not open directory $dir, bailing out\n";
        return;
    }
    @files = readdir(DIRECT);
    while ($name = shift @files)    {
        if ($name =~ /^\.\.?$/)    {    # Skip . && ..
        } else    {
            &resolve($name,$dir);
            if (-d $_ )    {
                $newname = $_;
                if (-l $newname)
                {    # Do not follow symlinks
                    $newname = readlink($_);
                    print "$level+--$name -> $newname\n";
                } elsif (-r _ && -x _)    {
                    # We must be able to enter a directory in order to tree it
                    print "$level+--$name/\n";
                    if (@files)    {
                        &tree($newname,"$level|  ");
                    } else    {
                        &tree($newname,"$level   ");
                    }
                } else    {
                    print "$level\--$name/ (unreadable)\n";
                }
            }
        }
    }
}


##############################################################################

	# These next few lines are legal in both Perl and nroff.

.00;			# finish .ig

'di			\" finish diversion--previous line must be blank
.nr nl 0-1		\" fake up transition to first page again
.nr % 0			\" start at page 1
'; __END__ ############# From here on it's a standard manual page ############
.TH DDIR 1 "March 9, 1994"
.AT 3
.SH NAME
ddir \- Program to do a DOS like directory tree
.SH SYNOPSIS
.B ddir
.SH DESCRIPTION
.I Ddir
Prints out only directories, not files while recursing down from the
current directory. This is quite slow program to use...
.SH ENVIRONMENT
No environment variables are used.
.SH FILES
None.
.SH AUTHOR
Brian Blackmore
.SH "SEE ALSO"
.SH DIAGNOSTICS
.SH BUGS
.ex
