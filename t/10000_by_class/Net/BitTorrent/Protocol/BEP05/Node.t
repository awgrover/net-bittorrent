package t::10000_by_class::Net::BitTorrent::Protocol::BEP05::Node;
our $PACKAGE=__PACKAGE__;

# For mocking 'send' :
BEGIN { *{CORE::GLOBAL::send} = sub { CORE::send $_[0],$_[1],$_[2] } }

use strict;
use warnings;
our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
use Test::More;
use parent 'Test::Class';
use lib '../../../../lib', 'lib';
use 5.010.000;
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
  Net::BitTorrent::Protocol::BEP05::Node->mock('_get_announce_peer_token_in')->returns(sub {'x'});

  while (my ($type, $args) = each %requests) {
    $node->$type(@$args);
    ok ((grep {$_->{'type'} eq $type} values %{$node->outstanding_requests}), "expecting reply: $type");
    }
  }

sub sends_reply : Tests {
  my $dht = Net::BitTorrent::DHT->new(
    nodeid => 0x12345, 
    udp4_host => '127.0.0.1', 
    udp6_host => '::0',
    );
  my $node = $dht->find_or_add_node(['127.0.0.3', 736]);
  $node->_nodeid(0x6a45);

  Net::BitTorrent::Protocol::BEP05::Node->unmock('send');

  my %tests = (
    _reply_ping => { 
      args => [], 
      test => sub { $dht->nodeid ~~ "0x".$_[0]->{'alive'} } 
      },
    _reply_find_node => { 
      args => [Bit::Vector->new_Hex(160, 0x555)], 
      test => sub { defined($_[0]->{'said'}) && [$node->host, $node->port] ~~ $_[0]->{'node'} } 
      },
    _reply_get_peers => { 
      args => [Bit::Vector->new_Hex(160, 0x634)], 
      # this is the "i don't know" response
      test => sub { defined($_[0]->{'said'}) && [$node->host, $node->port] ~~ $_[0]->{'node'} } 
      },
    _reply_announce_peer => { 
      args => [Bit::Vector->new_Hex(160, 0x634), {token => 'x'}], 
      # we signal error since we never actually have any peers
      test => sub { defined($_[0]->{'error'}->{'203'}) } 
      }, 
    );

  while (my ($method, $info) = each %tests) {
    Net::BitTorrent::Protocol::BEP05::Node->mock('send')
      ->with(
        0 => $node,
        1 => sub {
          my ($packet) = @_;
          my %rendered = render_packet($packet);
          # warn "Testing for $method...";
          # warn "\tdht ",$dht->nodeid, " node ",$node->nodeid;
          # warn "\tPacket for $method: ",Dumper(\%rendered); 
          my $rez = unpack('H*', $method) eq $rendered{'transaction_id'} && $info->{'test'}->(\%rendered);
          # warn "\tmatched? $rez";
          $rez;
          })
      ->once
      ->returns(sub{ ok 1,"Sent packet for $method"});
    }
  Net::BitTorrent::Protocol::BEP05::Node->mock('send')->never;

  while (my ($method, $info) = each %tests) {
    # warn "TRY $method";
    $node->$method($method, @{$info->{'args'}});
    }
  }

$PACKAGE->runtests() if !caller;
1;
