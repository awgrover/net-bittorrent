package t::10000_by_class::Net::BitTorrent::BEP23::Compact;
our $PACKAGE=__PACKAGE__;

# For mocking 'send' :

use strict;
use warnings;
our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
use Test::More;
use parent 'Test::Class';
use lib '../../../../lib', 'lib';
use 5.010.000;
use Test::Mock::Method;
use Carp;
use Data::Dumper;
use Net::BitTorrent::Protocol::BEP23::Compact qw(compact_ipv4 uncompact_ipv4);

my @HP = map { [ sprintf('%s.%s.%s.%s', $_,$_,$_,$_), $_ ] } (0..255);

sub t100_compact_sanity : Tests {
  foreach (@HP) {
    my $compact = compact_ipv4 $_;
    is length($compact),6,"Compacted length for ".join(":",@$_);
    }
  }

sub t150_ip_patterns : Tests {
  foreach (qw(001.02.0.000) ) {
    eval {compact_ipv4 [$_,1]};
    is $@,"","Tolerated pattern for ip $_";
    }
  }

sub t200_round_trip_each : Tests {
  foreach (@HP) {
    my $compact = compact_ipv4 $_;
    my @un = uncompact_ipv4($compact);
    ok( scalar(@un), "Got something uncompacted for ".join(":",@$_))
    and is_deeply( (uncompact_ipv4($compact))[0], $_);
    }
  }

sub t300_round_trip_2 : Tests {
  my $size = 32; # make sure we test all the control chars
  my $compact = compact_ipv4 @HP[0..$size];
  my @un = uncompact_ipv4($compact);
  my $str = join(", ", map {join(":",@$_)} @HP[0..$size]);
  ok( scalar(@un), "Got something uncompacted for $str")
  and is_deeply( [uncompact_ipv4($compact)], [@HP[0..$size]], "Round trip a list $str");
  }

sub t400_compact_ip_errors : Tests {
  foreach ('1', '300.0.0.0', '127.0.0.1.4', '127.a.0.1', '127.0.0.1.') {
    eval {compact_ipv4 [$_,1]};
    isnt $@,"","Throws for ip $_";
    }
  }

sub t400_compact_port_errors : Tests {
  foreach (-1, 65536, 65537) {
    eval {compact_ipv4 ['127.0.0.1',$_]};
    isnt $@,"","Throws for port $_";
    }
  }

$PACKAGE->runtests() if !caller;
1;
