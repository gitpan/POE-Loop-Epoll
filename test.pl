#!/usr/bin/perl -w
# $Id: test.pl,v 1.2 2004/11/22 18:05:18 rcaputo Exp $

use strict;

use Test::Harness;
use File::Find;
use File::Spec;

my @test_files = gather_test_files();
die "*** Can't find test files" unless @test_files;

runtests(@test_files);
exit;

# Build a list of all the tests to run.

sub gather_test_files {
  my %test_files;

  find(
    sub {
      return unless -f;
      $test_files{File::Spec->catfile($File::Find::dir, $_)} = 1;
    },
    'tests',
  );

  return sort keys %test_files;
}
