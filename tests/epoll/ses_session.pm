#!/usr/bin/perl -w
# $Id: ses_session.pm,v 1.7 2004/11/24 22:25:25 rcaputo Exp $

# Tests basic compilation and events.

use strict;

use lib qw(./mylib ../mylib ../lib ./lib);
use TestSetup;

BEGIN {
  sub POE::Kernel::ASSERT_DEFAULT () { 1 }
  sub POE::Kernel::TRACE_DEFAULT  () { 1 }
  sub POE::Kernel::TRACE_FILENAME () { "./test-output.err" }
}

test_setup(44);

use POE qw(Loop::Epoll);

### Test parameters and results.

my $machine_count  = 10;
my $event_count    = 5;
my $sigalrm_caught = 0;
my $sigpipe_caught = 0;
my $sender_count   = 0;
my $got_heap_count = 0;
my $default_count  = 0;
my $get_active_session_within = 0;
my $get_active_session_before = 0;
my $get_active_session_after  = 0;
my $get_active_session_heap   = 0;

die "machine count must be even" if $machine_count & 1;

### Status registers for each state machine instance.

my ( @completions, @objpack );

#------------------------------------------------------------------------------
# Define a simple state machine.

sub task_start {
  my ($kernel, $session, $heap, $id) = @_[KERNEL, SESSION, HEAP, ARG0];
  $heap->{count} = 0;
  $kernel->yield( count => $id );
}

sub task_run {
  my ($kernel, $session, $heap, $id) = @_[KERNEL, SESSION, HEAP, ARG0];

  $sender_count++ if $_[SENDER] == $session;

  if ($heap->{count} & 1) {
    $kernel->yield( bogus => $id ); # _default
  }
  else {
    $kernel->post( $session, bogus => $id ); # _default
  }

  if ( $kernel->call( $session, next_count => $id ) < $event_count ) {

    if ($heap->{count} & 1) {
      $kernel->yield( count => $id );
    }
    else {
      $kernel->post( $session, count => $id );
    }

  }
  else {
    $heap->{id} = $id;
  }
}

sub task_default {
  return 0 if $_[ARG0] eq '_signal'; # ignore signals
  $default_count++ if $_[STATE] eq '_default';
}

sub task_next_count {
  my ($kernel, $session, $heap, $id) = @_[KERNEL, SESSION, HEAP, ARG0];
  ++$heap->{count};
}

sub task_stop {
  $completions[$_[HEAP]->{id}] = $_[HEAP]->{count};
  $got_heap_count++ if (
    defined($_[HEAP]->{got_heap}) and
    $_[HEAP]->{got_heap} == $_[HEAP]->{id}
  );
}

#------------------------------------------------------------------------------
# Test simple signals.

# Spawn a quick state machine to test signals.  This is a classic
# example of inline states being just that: inline anonymous coderefs.
# It makes quick hacks quicker!
POE::Session->create(
  inline_states => {
    _start => sub {
      $_[HEAP]->{kills_to_go} = $event_count;
      $_[KERNEL]->sig( ALRM => 'sigalrm_target' );
      $_[KERNEL]->sig( PIPE => 'sigpipe_target' );
      $_[KERNEL]->delay( fire_signals => 0.5 );
    },
    fire_signals => sub {
      if ($_[HEAP]->{kills_to_go}--) {
        $_[KERNEL]->delay( fire_signals => 0.5 );
        if ($^O eq 'MSWin32') {
          $_[KERNEL]->signal( $_[KERNEL], 'ALRM' );
          $_[KERNEL]->signal( $_[KERNEL], 'PIPE' );
        }
        else {
          kill ALRM => $$;
          kill PIPE => $$;
        }
      }
      # One last timer so the session lingers long enough to catch
      # the final signal.
      else {
        $_[KERNEL]->delay( nonexistent_state => 1 );
      }
    },
    sigalrm_target => sub {
      $sigalrm_caught++ if $_[ARG0] eq 'ALRM';
      $_[KERNEL]->sig_handled();
    },
    sigpipe_target => sub {
      $sigpipe_caught++ if $_[ARG0] eq 'PIPE';
      $_[KERNEL]->sig_handled();
    },
  }
);

# Spawn ten state machines.
for (my $i=0; $i<$machine_count; $i++) {

  # Odd instances, try POE::Session->create
  if ($i & 1) {
    POE::Session->create(
      inline_states => {
        _start     => \&task_start,
        _stop      => \&task_stop,
        count      => \&task_run,
        next_count => \&task_next_count,
        _default   => \&task_default,
      },
      args => [ $i ],
      heap => { got_heap => $i },
    );
  }

  # Even instances, try POE::Session->new
  else {
    POE::Session->new (
      _start     => \&task_start,
      _stop      => \&task_stop,
      count      => \&task_run,
      next_count => \&task_next_count,
      [ $i ],
    );
  }
}

#------------------------------------------------------------------------------
# Simple client/server sessions using events as inter-session
# communications.  Tests postbacks, too.

POE::Session->create(
  inline_states => {
    _start => sub {
      $_[KERNEL]->alias_set( 'server' );
      $_[HEAP]->{response} = 0;
    },
    sync_query => sub {
      $_[ARG0]->( ++$_[HEAP]->{response} );
    },
    query => sub {
      $_[ARG0]->( ++$_[HEAP]->{response} );
    },
  },
);

# A simple client session.  It requests five counts and then stops.
# Its magic is that it passes a postback for the response.

my $postback_test = 1;
my $callback_test = 1;

POE::Session->create(
  inline_states => {
    _start => sub {
      $_[KERNEL]->yield( 'query' );
      $_[HEAP]->{cookie} = 0;
    },
    query => sub {
      $_[KERNEL]->post(
        server =>
        query  => $_[SESSION]->postback(response => ++$_[HEAP]->{cookie})
      );
      $_[HEAP]->{sync_called_back} = 0;
      $_[KERNEL]->call(
        server     =>
        sync_query =>
        $_[SESSION]->callback(sync_response => ++$_[HEAP]->{cookie})
      );
      $callback_test = 0 unless $_[HEAP]->{sync_called_back};
    },
    sync_response => sub {
      my ($req, $rsp) = ($_[ARG0]->[0], $_[ARG1]->[0] + 1);
      $callback_test = 0 unless $req == $rsp;
      $_[HEAP]->{sync_called_back} = 1;
    },
    response => sub {
      my ($req, $rsp) = ($_[ARG0]->[0], $_[ARG1]->[0] - 1);
      $postback_test = 0 unless $req == $rsp;
      if ($_[HEAP]->{cookie} < 5) {
        $_[KERNEL]->yield( 'query' );
      }
    },
    _stop => sub {
      $get_active_session_within = (
        $_[KERNEL]->get_active_session() == $_[SESSION]
      );
      $get_active_session_heap = (
        $_[KERNEL]->get_active_session()->get_heap() == $_[HEAP]
      );
    },
  }
);

#------------------------------------------------------------------------------
# Unmapped package session.

package UnmappedPackage;
use POE::Session; # for constants

sub _start {
  $_[KERNEL]->yield( 'count' );
  $_[HEAP]->{count} = 0;
  $_[HEAP]->{id} = $_[ARG0];
}

sub count {
  return unless $_[OBJECT] eq __PACKAGE__;
  $_[KERNEL]->yield( 'count' ) if ++$_[HEAP]->{count} < $event_count;
}

sub _stop {
  $objpack[$_[HEAP]->{id}] = $_[HEAP]->{count};
}

#------------------------------------------------------------------------------
# Unmapped object session.

package UnmappedObject;
use POE::Session; # for constants

# Trivial constructor.
sub new { bless [ ], shift; }

sub _start {
  $_[KERNEL]->yield( 'count' );
  $_[HEAP]->{count} = 0;
  $_[HEAP]->{id} = $_[ARG0];
}

sub count {
  return unless ref($_[OBJECT]) eq __PACKAGE__;
  $_[KERNEL]->yield( 'count' ) if ++$_[HEAP]->{count} < $event_count;
}

sub _stop {
  $objpack[$_[HEAP]->{id}] = $_[HEAP]->{count};
}

#------------------------------------------------------------------------------
# Unmapped package session.

package MappedPackage;
use POE::Session; # for constants

sub my_start {
  $_[KERNEL]->yield( 'count' );
  $_[HEAP]->{count} = 0;
  $_[HEAP]->{id} = $_[ARG0];
}

sub my_count {
  return unless $_[OBJECT] eq __PACKAGE__;
  $_[KERNEL]->yield( 'count' ) if ++$_[HEAP]->{count} < $event_count;
}

sub my_stop {
  $objpack[$_[HEAP]->{id}] = $_[HEAP]->{count};
}

#------------------------------------------------------------------------------
# Unmapped object session.

package MappedObject;
use POE::Session; # for constants

# Trivial constructor.
sub new { bless [ ], shift; }

sub my_start {
  $_[KERNEL]->yield( 'count' );
  $_[HEAP]->{count} = 0;
  $_[HEAP]->{id} = $_[ARG0];
}

sub my_count {
  return unless ref($_[OBJECT]) eq __PACKAGE__;
  $_[KERNEL]->yield( 'count' ) if ++$_[HEAP]->{count} < $event_count;
}

sub my_stop {
  $objpack[$_[HEAP]->{id}] = $_[HEAP]->{count};
}

#------------------------------------------------------------------------------
# Test the Package and Object sessions.

package main;

# New style (create) object session without event to method name map.
POE::Session->create(
  object_states => [
    UnmappedObject->new() => [ '_start', 'count', '_stop' ],
  ],
  args => [ 0 ],
);

# New style (create) object session with event to method name map.
POE::Session->create(
  object_states => [
    MappedObject->new => {
      _start => 'my_start',
      count  => 'my_count',
      _stop  => 'my_stop',
    },
  ],
  args => [ 1 ],
);

# Old style (new) object session without event to method name map.
POE::Session->new(
  [ 2 ],
  UnmappedObject->new => [ '_start', 'count', '_stop' ],
);

# Old style (new) object session with event to method name map.
POE::Session->new(
  [ 3 ],
  MappedObject->new => {
    _start => 'my_start',
    count  => 'my_count',
    _stop  => 'my_stop',
  },
);

# New style (create) package session without event to method name map.
POE::Session->create(
  package_states => [
    UnmappedPackage => [ '_start', 'count', '_stop' ],
  ],
  args => [ 4 ],
);

# New style (create) package session with event to method name map.
POE::Session->create(
  package_states => [
    MappedPackage => {
      _start => 'my_start',
      count  => 'my_count',
      _stop  => 'my_stop',
    },
  ],
  args => [ 5 ],
);

# Old style (new) package session without event to method name map.
POE::Session->new(
  [ 6 ],
  UnmappedPackage => [ '_start', 'count', '_stop' ],
);

# Old style (new) package session with event to method name map.
POE::Session->new(
  [ 7 ],
  MappedPackage => {
    _start => 'my_start',
    count  => 'my_count',
    _stop  => 'my_stop',
  },
);

#------------------------------------------------------------------------------
# Main loop.

$get_active_session_before = $poe_kernel->get_active_session() == $poe_kernel;
POE::Kernel->run();
$get_active_session_after = $poe_kernel->get_active_session() == $poe_kernel;

#------------------------------------------------------------------------------
# Final tests.

# Now make sure they've run.
for (my $i=0; $i<$machine_count; $i++) {
  print 'not ' unless $completions[$i] == $event_count;
  print 'ok ', $i+1, "\n";
}

# Were all the signals caught?
if ($^O eq 'MSWin32' or $^O eq 'MacOS') {
  print "ok 11 # skipped: $^O does not support signals.\n";
  print "ok 12 # skipped: $^O does not support signals.\n";
}
else {
  print 'not ' unless $sigalrm_caught == $event_count;
  print "ok 11\n";

  print 'not ' unless $sigpipe_caught == $event_count;
  print "ok 12\n";
}

# Did the postbacks work?
print 'not ' unless $postback_test;
print "ok 13\n";

print 'not ' unless $callback_test;
print "ok 14\n";

# Were the various get_active_session() calls correct?
print 'not ' unless $get_active_session_within;
print "ok 15\n";

print 'not ' unless $get_active_session_before;
print "ok 16\n";

print 'not ' unless $get_active_session_after;
print "ok 17\n";

# Was the get_heap() call correct?
print 'not ' unless $get_active_session_heap;
print "ok 18\n";

# Gratuitous tests to appease the coverage gods.
print 'not ' unless (
  ARG1 == ARG0+1 and ARG2 == ARG1+1 and ARG3 == ARG2+1 and
  ARG4 == ARG3+1 and ARG5 == ARG4+1 and ARG6 == ARG5+1 and
  ARG7 == ARG6+1 and ARG8 == ARG7+1 and ARG9 == ARG8+1
);
print "ok 19\n";

print 'not ' unless $sender_count == $machine_count * $event_count;
print "ok 20\n";

print 'not ' unless $default_count == ($machine_count * $event_count) / 2;
print "ok 21\n";

print 'not ' unless $got_heap_count == $machine_count / 2;
print "ok 22\n";

# Object/package sessions.
for (0..7) {
  print 'not ' unless $objpack[$_] == $event_count;
  print 'ok ', $_ + 23, "\n";
}

my $sessions_destroyed = 0;
my $objects_destroyed = 0;
my $stop_called = 0;
my $parent_called = 0;
my $child_called = 0;

package POE::MySession;

use vars qw(@ISA);

use POE::Session;
@ISA = qw(POE::Session);

sub DESTROY {
  $_[0]->SUPER::DESTROY;
  $sessions_destroyed++;
}

package MyObject;

sub new { bless {} }
sub DESTROY { $objects_destroyed++ }

package main;

POE::MySession->new(
  _start => sub {
    $_[HEAP]->{object} = MyObject->new;
    POE::MySession->new(
      _start => sub {
        $_[HEAP]->{object} = MyObject->new;
        POE::MySession->new(
          _start => sub {
            $_[HEAP]->{object} = MyObject->new;
            POE::MySession->new(
              _start => sub {
                $_[HEAP]->{object} = MyObject->new;
                $_[KERNEL]->delay(nonexistent => 3600);
                $_[KERNEL]->alias_set('test4');
              },
              _parent => sub {
                $parent_called++;
              },
              _child => sub { }, # To shush ASSERT
              _stop => sub {
                $stop_called++;
              },
            ),
            $_[KERNEL]->delay(nonexistent => 3600);
            $_[KERNEL]->alias_set('test3');
          },
          _parent => sub {
            $parent_called++;
          },
          _child => sub {
            $child_called++ if $_[ARG0] eq 'lose';
          },
          _stop => sub {
            $stop_called++;
          },
        ),
        $_[KERNEL]->delay(nonexistent => 3600);
        $_[KERNEL]->alias_set('test2');
      },
      _parent => sub {
        $parent_called++;
      },
      _child => sub {
        $child_called++ if $_[ARG0] eq 'lose';
      },
      _stop => sub {
        $stop_called++;
      },
    ),
    $_[KERNEL]->delay(nonexistent => 3600);
    $_[KERNEL]->alias_set('test1');
    $_[KERNEL]->yield("stop");
  },
  _parent => sub {
    $parent_called++;
  },
  _child => sub {
    $child_called++ if $_[ARG0] eq 'lose';
  },
  _stop => sub {
    $stop_called++;
  },
  stop => sub {
    POE::Kernel->stop();

    my $expected;
    if ($] >= 5.004 and $] < 5.005) {
      warn(
        "# Note: Perl 5.004-ish appears to leak sessions.\n",
        "#       Consider upgrading to Perl 5.005_04 or beyond.\n",
      );
      $expected = 0;
    }
    else {
      $expected = 3;
    }

    print 'not ' unless $sessions_destroyed == $expected;
    print "ok 31 # dest $sessions_destroyed sessions (expected $expected)\n";

    # 5.004 and 5.005 have some nasty gc issues. Near as I can tell,
    # data inside the heap is surviving the session DESTROY. This
    # isnt possible in a sane and normal world. So if this is giving
    # you fits, please consider upgrading perl to at least 5.6.1.
    my $expected;
    if($] >= 5.006 or ($] >= 5.004 and $] < 5.005)) {
      $expected = 3;
    } else {
      $expected = 2;
    }
    print 'not ' unless $objects_destroyed == $expected;
    print "ok 32 # dest $objects_destroyed objects (expected $expected)\n";
  }
);

$poe_kernel->run;
print 'not ' unless $stop_called == 0;
print "ok 33\n";
print 'not ' unless $child_called == 0;
print "ok 34\n";
print 'not ' unless $parent_called == 0;
print "ok 35\n";

my $expected;
if ($] >= 5.004 and $] < 5.005) {
  warn(
    "# Note: Perl 5.004-ish appears to leak sessions.\n",
    "#       Consider upgrading to Perl 5.005_04 or beyond.\n",
  );
  $expected = 0;
}
else {
  $expected = 4;
}

print 'not ' unless $sessions_destroyed == $expected;
print "ok 36 # dest $sessions_destroyed sessions (expected $expected)\n";

# 5.004 and 5.005 have some nasty gc issues. Near as I can tell,
# data inside the heap is surviving the session DESTROY. This
# isnt possible in a sane and normal world. So if this is giving
# you fits, please consider upgrading perl to at least 5.6.1.
my $expected;
if($] >= '5.006') {
  $expected = 4;
}
elsif ($] == 5.005_04) {
  $expected = 3;
}
else {
  $expected = 4;
}

print "not " unless $objects_destroyed == $expected;
print "ok 37 # dest $objects_destroyed objects (expected $expected)\n";

# This simple session just makes sure we can start another Session and
# another Kernel.  If all goes well, it'll dispatch some events and
# exit normally.

# The restart test dumps core when using Tk with Perl 5.8.0 and
# beyond, but only if they're built without threading support.  It
# happens consistently in a pure Tk test case.  It happens
# consistently in POE's "make test" suite.  It doesn't happen at all
# when running the test by hand.
#
# http://rt.cpan.org/Ticket/Display.html?id=8588 is tracking the Tk
# test case.  Wish us luck there.
#
# Meanwhile, these tests will be skipped under Tk if Perl is 5.8.0 or
# beyond, and it's not built for threading.

use Config;
if (
  $] >= 5.008 and
  exists $INC{"Tk.pm"} and
  !$Config{useithreads}
) {
  foreach (38..43) {
    print(
      "not ok $_ # Skip: Restarting Tk dumps core in single-threaded perl $]\n"
    );
  }
}
else {
  POE::Session->create(
    options => { trace => 1, default => 1, debug => 1 },
    inline_states => {
      _start => sub {
        print "ok 38\n";
        $_[KERNEL]->yield("woot");
        $_[KERNEL]->delay(narf => 1);
      },
      woot => sub {
        print "ok 40\n";
      },
      narf => sub {
        print "ok 41\n";
      },
      _stop => sub {
        print "ok 42\n";
      },
    }
  );

  print "ok 39\n";
  POE::Kernel->run();
  print "ok 43\n";

  if (defined($INC{'POE/Loop/Epoll.pm'}) == 1) {
      print "ok 44\n";
  } else {
      print "not ok 44\n";
  }
}

1;