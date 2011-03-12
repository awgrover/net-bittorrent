package t::10000_by_class::Net::BitTorrent::Protocol::BEP05::Node;
our $PACKAGE=__PACKAGE__;

# For mocking 'send' :
BEGIN { *{CORE::GLOBAL::send} = sub { CORE::send $_[0],$_[1],$_[2] } }

use Time::HiRes;
use Carp;
sub main::check_slow {
  my $msg = shift if !ref $_[0];
  # warn "Watching ",join(',',(caller(1))[1,2]);
  my $start = Time::HiRes::time;
  my $rez = $_[0]->();
  my $elapsed = Time::HiRes::time - $start;
  if ($elapsed > .01) {
    warn "SLOW $msg, $elapsed, at ",join(',',caller(1));
    }
  $rez;
  }

use strict;
use warnings;
our $MAJOR = 0; our $MINOR = 1; our $DEV = 1; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
use Test::More;
use parent 'Test::Class';
use lib '../../../../lib', 'lib';
use 5.010.000;
use Test::Moose;
use AnyEvent::Impl::Perl;   # Timing is different than with EV. Go figure.
use AnyEvent;
use Test::Mock::Method;
use Carp;
use Data::Dumper;
use SmartMatch::Sugar;
use Net::BitTorrent::Protocol::BEP05::Packets::Render;
use Net::BitTorrent::DHT;

  use Net::BitTorrent::Network::Utility;
  # Prevent dns lookup, in testing all addresses are ip's
  # Do this early, before "sockaddr" is imported into someone else
  no warnings 'redefine';
  sub Net::BitTorrent::Network::Utility::sockaddr($$){ 
    my $rez = AnyEvent::Socket::pack_sockaddr( $_[1], AnyEvent::Socket::parse_address($_[0]) );
    # warn "Resolved $_[0]:$_[1] to ...";
    confess "provide udp4_host, and upd6_host, in dht->new" if $_[0] eq '0.0.0.0';
    $rez;
    }

use constant Class => 'Net::BitTorrent::Protocol::BEP05::Node';
my @AE_hold; # to hold refs

sub module_loads_and_can_new : Tests(startup => no_plan) {
    my $self = shift;

    use_ok Class;
    can_ok Class, 'new';
}

my %DefaultMocks = (
  'Net::BitTorrent::Protocol::BEP05::Node::expire_request' => sub {},
  'Net::BitTorrent::Protocol::BEP05::Node::ping' => sub {},
  'Net::BitTorrent::Protocol::BEP05::Node::send' => sub {length($_[1])},
  );

sub setup : Test(setup) {
    my $self = shift;

    # mock 'send' so we don't do real io
    mock_core('send', sub {
        my ($sock, $packet, $flags, $to) = @_;
        # warn "Core::Send called with ".sockaddr2ip(getsockname($sock))." ".length($packet);
        return length($packet);
        });

    # Capture timer events so we can force them
    AE->mock('timer', \&AnyEvent::Impl::Perl::Mock::timer);
    # This runs our mocked timer events
    AnyEvent::Impl::Perl->mock('one_event', \&AnyEvent::Impl::Perl::Mock::one_event);

    # ensure that we don't sleep during one_event
    @AE_hold = AE::idle sub {}; # the first call to AE::idle has to provide explicit sub. weird.

    while (my ($method, $sub) = each %DefaultMocks) {
      my ($package,$method_name) = $method =~ /(.+)::([^:]+$)/;
      $package->mock($method_name, $sub);
      }
}

sub teardown : Test(teardown) {
  @AE_hold = ();
}

sub knows_protocol : Tests {
  my $node = Class->new(host=>'127.0.0.1', port => 12);
  ok !$node->is_ipv6,"Not ipv6";

  $node = Class->new(host=>'::0', port => 12);
  ok $node->is_ipv6,"Not ipv6";
  }

sub expiring_counts_as_fail : Tests {
  Net::BitTorrent::Protocol::BEP05::Node->unmock('expire_request');
  Net::BitTorrent::Protocol::BEP05::Node->mock('inc_fail')->once;
  my $node = Class->new(host=>'127.0.0.1', port => 12);
  $node->add_request('transid', {});
  is keys %{ $node->outstanding_requests }, 1, "1 request";
  ok $node->get_request('transid'), "request in there";
  $node->expire_request('transid');
  is $node->get_request('transid'), undef, "request gone";
  }

sub requests_timeout : Tests {
  Net::BitTorrent::Protocol::BEP05::Node->unmock('expire_request');

  my $trans = "trans9";
  Net::BitTorrent::Protocol::BEP05::Node->mock('inc_fail');
  my $cv = AE::cv;
  Net::BitTorrent::Protocol::BEP05::Node->mock('expire_request', sub {$cv->send})
    ->with(1=>$trans)
    ->once;
  my $node = Class->new(host=>'127.0.0.1', port => 12);
  $node->add_request($trans, {});
  my $timeout = AE::unmocked_timer( 5,0,sub{ die "got stuck waiting for expire" } );
  $cv->recv;
  }

sub ping_themselves : Tests {
  Net::BitTorrent::Protocol::BEP05::Node->unmock('ping');
  # ping is setup for initial and repeating
  my $cv = AE::cv; $cv->begin for (1..3);
  Net::BitTorrent::Protocol::BEP05::Node->mock('ping', sub {$cv->end})->twice;

  my $node = Class->new(host=>'127.0.0.1', port => 12);

  my $timeout = AE::unmocked_timer( 5,0,sub{ die "got stuck waiting for expire" } );
  $cv->recv;
  }

sub go_inactive : Tests {
  my $node = Class->new(host=>'127.0.0.1', port => 12);

  ok !$node->active, "new node is inactive";

  $node->touch;
  ok $node->active, "just touched is active";

  $node->_set_seen( time - 60 * 60);
  ok !$node->active, "old node is inactive";
  }

sub expects_replies : Tests {
  my $dht = Net::BitTorrent::DHT->new(
    nodeid => '12345', 
    udp4_host => '127.0.0.1', 
    udp6_host => '::0');
  my $node = Class->new(
    host=>'127.0.0.1', 
    port => 12, 
    routing_table=>$dht->ipv4_routing_table,
    dht => $dht,
    );

  Net::BitTorrent::Protocol::BEP05::Node->unmock('ping');
  my %requests = (
    ping => [],
    find_node => [Bit::Vector->new_Dec(160,'12345')],
    get_peers => [Bit::Vector->new_Dec(160,'12345')],
    announce_peer => [Bit::Vector->new_Dec(160,'12345'), 446],
    );

  Net::BitTorrent::Protocol::BEP05::Node->mock('has_announce_peer_token_in')->returns(sub {1});

  while (my ($type, $args) = each %requests) {
    $node->$type(@$args);
    ok ((grep {$_->{'type'} eq $type} values %{$node->outstanding_requests}), "expecting reply: $type");
    warn (join(", ",map {$_->{'type'}} values %{$node->outstanding_requests}));
    }
  }

use Bit::Vector; # we add to it
package Bit::Vector;
# Provide smartmatch, semantics is 'eq'. convert numberish things to a bit:vector for comparison
use Bit::Vector::Overload;
use Scalar::Util 'blessed';
use overload '~~' => 'bv_smart';

sub bv_smart {
    my ($self,$operand,$reverse) = @_;
    ($self,$operand) = ($operand,$self) if $reverse;

    if (blessed($operand) && $operand->isa('Bit::Vector')) {
        $self eq $operand;
        }
    # We don't try to convert the $operand
    elsif (ref($operand)) {
        return $operand ~~ $self;
        # sadly, we can't call SUPER::~~
        die "Don't know how to do ~~ for ".ref($self)." ~~ ".(ref($operand) || $operand);
        }
    elsif ($operand =~ /^\d+$/) {
        my $other = $self->Shadow;
        $other->from_Dec($operand);
        $self eq $other;
        }
    elsif ($operand =~ /^0x/) {
        my $other = $self->Shadow;
        $other->from_Hex($operand);
        $self eq $other;
        }
    elsif ($operand =~ /^0b/) {
        # bit vector doesn't tolerate 0bxxxx for binary strings
        my $other = $self->Shadow;
        $other->from_Bin(substr $operand, 2);
        $self eq $other;
        }
    else {
        my $rez = eval { $self eq $operand; };
        if ($@) {
            my $x = index($@, "Bit::Vector::bv_smart: index out of range in overloaded 'cmp' operator")==0;
            if ($x==0) {
                die "You tried to do a smartmatch of ".ref($self)." ~~ ".$operand.". But, that Bit::Vector's -> Configuration apparently had 'input = bit indices', and $operand doesn't look like decimal, hex or binary, so $operand was considered an indice: ".$self->Configuration;
                }
            else {
                die $@."::: $x";
                }
            }
        $rez;
        }
            

    }

$PACKAGE->runtests() if !caller;
1;
