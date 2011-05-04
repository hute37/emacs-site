#!/usr/bin/perl -w
use strict;

my $lib_dir = "/usr/lib/emacsen-common";
my $var_dir = "/var/lib/emacsen-common";

my $dry_run = 0;

require $lib_dir . "/generate-install-list";

my $all_pkgs = installed_add_on_packages_list();
my @ordered_pkg_list = generate_add_on_install_list($all_pkgs);
print "  " . join("\n  ", @ordered_pkg_list);


  open DEPENDS, ">depend-list" or
    die " Couldn't open depend-list for write.";
  print DEPENDS join("\n", @ordered_pkg_list);
  close DEPENDS;


sub x_generate_add_on_install_list {

  my($packages_to_sort) = @_;

  my @sorted_pkgs = reorder_add_on_packages($packages_to_sort,
                                            installed_add_on_packages_list());

  return(@sorted_pkgs);
}

sub z_generate_add_on_install_list {

  if (! -f "${var_dir}/depend-list")
    {
      my @sorted_pkgs = x_generate_add_on_install_list();
      return(@sorted_pkgs);
    }

  open DEPENDS, "<${var_dir}/depend-list" or
    die "Couldn't open ${var_dir}/depend-list";
  my @sorted_pkgs = <DEPENDS>;
  close DEPENDS;
  chomp @sorted_pkgs;
  return(@sorted_pkgs);
}


