# Empty package to appease perl.
package POE::Loop::Epoll;

our $VERSION = 0.01;

# Include common signal handling.
use POE::Loop::PerlSignals;

# Everything plugs into POE::Kernel;
package POE::Kernel;

use strict;

BEGIN {
    if ($^O ne "linux") {
        warn "Epoll doesn't work on anything but Linux.  Falling back to IO::Select.\n";
        require POE::Loop::Select;
        POE::Loop::Select->import();
        die "not really dying";
    }

}
sub MINIMUM_EPOLL_TIMEOUT () { 0 }

use Errno qw(EINPROGRESS EWOULDBLOCK EINTR);

# Allow $^T to change without affecting our internals.
my $start_time = $^T;

my %epoll_fd_masks;
my $epfd = undef;

# for epoll definitions:
our $HAVE_SYSCALL_PH = eval {
    require 'syscall.ph'; 1
} || eval {
    require 'sys/syscall.ph'; 1
};

# Explicitly define the poll constants, as either one set or the other
# won't be loaded. They're also badly implemented in IO::Epoll:
# The IO::Epoll module is buggy in that it doesn't export constants
# efficiently (at least as of 0.01), so doing constants ourselves saves
# 13% of the user CPU time.

use constant EPOLLIN       => 1;
use constant EPOLLOUT      => 4;
use constant EPOLLERR      => 8;
use constant EPOLLHUP      => 16;
use constant EPOLLRDBAND   => 128;
use constant EPOLL_CTL_ADD => 1;
use constant EPOLL_CTL_DEL => 2;
use constant EPOLL_CTL_MOD => 3;

# linux-ix86 defaults
our $SYS_epoll_create = eval { &SYS_epoll_create } || 254; 
our $SYS_epoll_ctl = eval { &SYS_epoll_ctl } || 255;
our $SYS_epoll_wait = eval { &SYS_epoll_wait } || 256;

# args: size
sub epoll_create {
    my $epfd = eval { syscall($SYS_epoll_create, $_[0]) };
    return -1 if $@;
    return $epfd;
}

# ARGS: (epfd, op, fd, events)
sub epoll_ctl {
    syscall(
            $SYS_epoll_ctl,
            $_[0]+0,            # epfd
            $_[1]+0,            # operation
            $_[2]+0,            # fd
            pack("LLL", $_[3], $_[2]) # events
           );
}

# ARGS: (epfd, maxevents, timeout, arrayref)
#  arrayref: values modified to be [$fd, $event]
our $epoll_wait_events;
our $epoll_wait_size = 0;

sub epoll_wait {
    # resize our static buffer if requested size is bigger than we've ever done
    if ($_[1] > $epoll_wait_size) {
        $epoll_wait_size = $_[1];
        $epoll_wait_events = pack("LLL") x $epoll_wait_size;
    }
    my $ct = syscall($SYS_epoll_wait,
                     $_[0]+0,
                     $epoll_wait_events,
                     $_[1]+0,
                     $_[2]+0);

    for ($_ = 0; $_ < $ct; $_++) {
        @{$_[3]->[$_]}[1,0] = unpack("LL",
                                     substr($epoll_wait_events, 12*$_, 8)
                                    );
    }

    return $ct;
}

#------------------------------------------------------------------------------
# Loop construction and destruction.

sub loop_initialize {
    my $self = shift;
    %epoll_fd_masks = ();

    # create an epoll fd "dimensioned for _size_ descriptors". The size
    # isn't a maximum, it's just a hint to the kernel.
    $epfd = epoll_create(100);
    die "No Epoll support" if $epfd < 0;
}

sub loop_finalize {
    # does nothing
}

#------------------------------------------------------------------------------
# Signal handler maintenance functions.

sub loop_attach_uidestroy {
    # does nothing
}

#------------------------------------------------------------------------------
# Maintain time watchers.

sub loop_resume_time_watcher {
    # does nothing
}

sub loop_reset_time_watcher {
    # does nothing
}

sub loop_pause_time_watcher {
    # does nothing
}

#------------------------------------------------------------------------------
# Maintain filehandle watchers.

sub loop_watch_filehandle {
    my ($self, $handle, $mode) = @_;
    my $fileno = fileno($handle);

    loop_initialize() unless $epfd;

    my $type;
    $type = EPOLLIN if $mode == MODE_RD;
    $type = EPOLLOUT if $mode == MODE_WR;
    $type = EPOLLRDBAND if $mode == MODE_EX;

    my $current = $epoll_fd_masks{$fileno} || 0;
    my $new = $current | $type;

    #   if (TRACE_FILES) {
    #     POE::Kernel::_warn(
    #       sprintf(
    #         "<fh> Watch $fileno: " .
    #         "Current mask: 0x%02X - including 0x%02X = 0x%02X\n",
    #         $current, $type, $new
    #       )
    #     );
    #   }

    if ($epoll_fd_masks{$fileno}) {
        epoll_ctl($epfd, EPOLL_CTL_MOD, $fileno, $new);
    } else {
        epoll_ctl($epfd, EPOLL_CTL_ADD, $fileno, $new);
    }

    $epoll_fd_masks{$fileno} = $new;
}

sub loop_ignore_filehandle {
    my ($self, $handle, $mode) = @_;
    my $fileno = fileno($handle);

    loop_initialize() unless $epfd;

    my $type;
    $type = EPOLLIN if $mode == MODE_RD;
    $type = EPOLLOUT if $mode == MODE_WR;
    $type = EPOLLRDBAND if $mode == MODE_EX;

    my $current = $epoll_fd_masks{$fileno} || 0;
    my $new = $current & ~$type;

    #   if (TRACE_FILES) {
    #     POE::Kernel::_warn(
    #       sprintf(
    #         "<fh> Ignore $fileno: " .
    #         ": Current mask: 0x%02X - removing 0x%02X = 0x%02X\n",
    #         $current, $type, $new
    #       )
    #     );
    #   }

    if ($epoll_fd_masks{$fileno}) {
        if ($new) {
            epoll_ctl($epfd, EPOLL_CTL_MOD, $fileno, $new);
            $epoll_fd_masks{$fileno} = $new;
        } else {
            epoll_ctl($epfd, EPOLL_CTL_DEL, $fileno, 0);
            delete $epoll_fd_masks{$fileno};
        }
    } else {
        epoll_ctl($epfd, EPOLL_CTL_ADD, $fileno, $new);
        $epoll_fd_masks{$fileno} = $new;
    }

}

*loop_pause_filehandle = \&loop_ignore_filehandle;
*loop_resume_filehandle = \&loop_watch_filehandle;

#------------------------------------------------------------------------------
# The event loop itself.

sub loop_do_timeslice {
    my $self = shift;

    # Check for a hung kernel.
    $self->_test_if_kernel_is_idle();

    # Set the poll timeout based on current queue conditions.  If there
    # are FIFO events, then the poll timeout is zero and move on.
    # Otherwise set the poll timeout until the next pending event, if
    # there are any.  If nothing is waiting, set the timeout for some
    # constant number of seconds.

    my $now = time();
    my $timeout = $self->get_next_event_time();

    if (defined $timeout) {
        $timeout -= $now;
        $timeout = MINIMUM_EPOLL_TIMEOUT if $timeout < MINIMUM_EPOLL_TIMEOUT;
    } else {
        $timeout = 3600;
    }

    if (TRACE_EVENTS) {
        POE::Kernel::_warn(
                           '<ev> Kernel::run() iterating.  ' .
                           sprintf(
                                   "now(%.4f) timeout(%.4f) then(%.4f)\n",
                                   $now-$start_time, $timeout, ($now-$start_time)+$timeout
                                  )
                          );
    }

    if (TRACE_FILES) {
        foreach (sort { $a<=>$b} keys %epoll_fd_masks) {
            my @types;
            push @types, "plain-file"        if -f;
            push @types, "directory"         if -d;
            push @types, "symlink"           if -l;
            push @types, "pipe"              if -p;
            push @types, "socket"            if -S;
            push @types, "block-special"     if -b;
            push @types, "character-special" if -c;
            push @types, "tty"               if -t;
            my @modes;
            my $flags = $epoll_fd_masks{$_};
            push @modes, 'r' if $flags & (EPOLLIN     | EPOLLHUP | EPOLLERR);
            push @modes, 'w' if $flags & (EPOLLOUT    | EPOLLHUP | EPOLLERR);
            push @modes, 'x' if $flags & (EPOLLHUP | EPOLLERR);
            POE::Kernel::_warn(
                               "<fh> file descriptor $_ = modes(@modes) types(@types)\n"
                              );
        }
    }

    if ($timeout or %epoll_fd_masks) {

        # There are filehandles to poll, so do so.

        if (%epoll_fd_masks) {
            # Check filehandles, or wait for a period of time to elapse.
            my @events;
            my $e_timeout = int($timeout * 1000);

            my $hits = epoll_wait($epfd, 1000, $e_timeout, \@events);

            # If epoll has seen filehandle activity, then gather up the
            # active filehandles and synchronously dispatch events to the
            # appropriate handlers.

            if ($hits > 0) {
                my (@rd, @wr, @ex);

                foreach my $event (@events) {
                    my ($fd, $state) = @$event;

                    push(@rd, $fd) if $state & ( EPOLLIN | EPOLLHUP | EPOLLERR );
                    push(@wr, $fd) if $state & ( EPOLLOUT | EPOLLHUP | EPOLLERR );
                    push(@ex, $fd) if $state & ( EPOLLRDBAND | EPOLLHUP | EPOLLERR );
                }

                @rd and $self->_data_handle_enqueue_ready(MODE_RD, @rd);
                @wr and $self->_data_handle_enqueue_ready(MODE_WR, @wr);
                @ex and $self->_data_handle_enqueue_ready(MODE_EX, @ex);
                            
            }
        }

        # No filehandles to poll on.  Try to sleep instead.  Use sleep()
        # itself on MSWin32.  Use a dummy four-argument select() everywhere
        # else.

        else {
            select(undef, undef, undef, $timeout);
        }
    }

    if (TRACE_STATISTICS) {
        $self->_data_stat_add('idle_seconds', time() - $now);
    }

    # Dispatch whatever events are due.
    $self->_data_ev_dispatch_due();
}

### Run for as long as there are sessions to service.

sub loop_run {
    my $self = shift;
    while ($self->_data_ses_count()) {
        $self->loop_do_timeslice();
    }
}

sub loop_halt {
    # does nothing
}

1;

__END__

=head1 NAME

POE::Loop::Epoll - a bridge that supports epoll from POE

=head1 SYNOPSIS

See L<POE::Loop>.

=head1 DESCRIPTION

This class is an implementation of the abstract POE::Loop interface.
It follows POE::Loop's public interface exactly.  Therefore, please
see L<POE::Loop> for its documentation.

=head1 LIMITATIONS

epoll is Linux specific. It only works on Linux versions greater than
2.5.44.

=head1 BUGS

This module make break everything. It probably hasn't been adequately
tested. You have been warned.

If it's been some significant period of time (I'll let you decide what
"significant period of time" means) since this module was uploaded,
everything is probably fine. Or I've been hit by a bus.

=head1 SEE ALSO

L<POE>, L<POE::Loop>, L<epoll(2)>, L<epoll_create(2)>, L<epoll_ctl(2)>, L<epoll_wait(2)>

=head1 AUTHORS & LICENSING

This module was cleaned up by Paul Visscher. It was heavily based on
code written by Brad Fitzpatrick. The test code was borrowed from the
POE test suite and modified to test POE::Loop::Epoll.

Please see L<POE> for more information about authors, contributors,
and POE's licensing.

=cut
