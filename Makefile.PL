use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

if ($^O ne "linux") {
    print STDERR "POE::Loop::Epoll only works on Linux. Sorry.\n";
    exit(1);
}

# test to see if we can epoll
my $epfd = eval { syscall(254, 100); };
if ($@ or $epfd < 0) {
    die "epoll only works on 2.5.66 or newer, but it looks like you've got something\n" .
      "older. If this is wrong, please contact the author.\n";
}

my $prompt = qq|
POE's test suite requires a functional network.
However, we can skip those tests requiring network
access if you would like.

Would you like to skip the network tests?

|;

$prompt .= "(Any text other than Y or y will be taken as a no)";

my $ret = "n";
if (grep /^--default$/, @ARGV) {
  print $prompt, " [n] n\n\n";
}
else {
  $ret = prompt($prompt, "n");
}

my $marker = 'run_network_tests';

if($ret =~ /^Y$/i) {
    unlink $marker if $marker;
} else {
    open(TOUCH,"+>$marker") and close TOUCH;
}


WriteMakefile(
    NAME              => 'POE::Loop::Epoll',
    VERSION_FROM      => 'lib/POE/Loop/Epoll.pm', # finds $VERSION
    PREREQ_PM         => { POE => 0.29 }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/POE/Loop/Epoll.pm', # retrieve abstract from module
       AUTHOR         => 'Paul Visscher <paulv@canonical.org>') : ()),
);
