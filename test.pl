#!/usr/bin/perl -w
# $Id: test.pl 1774 2005-04-22 20:27:23Z rcaputo $

use strict;

use lib qw(./mylib);
use Test::Harness;
use File::Find;
use File::Spec;

### Some early setup.

# Makefile.PL does this.  Why don't we?
$ENV{PERL_DL_NONLAZY} = 1;

### Run the tests.

my @test_files = gather_test_files();
die "*** Can't find test files" unless @test_files;

# Various test filtering thingies.

# Stop the tests at one that tends to dump core.
#my $x = @test_files;
#while (@test_files) {
#  last if $test_files[-1] eq "tests/30_loops/50_tk/ses_session.t";
#  pop @test_files;
#}

# Run the tests for a particular event loop.
# @test_files = grep /50_tk/, @test_files;

runtests(@test_files);
exit;

# Build a list of all the tests to run.

sub gather_test_files {
  my %test_files;

  find(
    sub {
      return unless -f;
      return unless /(\.t|\.pm)$/;
      $test_files{File::Spec->catfile($File::Find::dir, $_)} = 1;
    },
    'tests',
  );

  return sort keys %test_files;
}
