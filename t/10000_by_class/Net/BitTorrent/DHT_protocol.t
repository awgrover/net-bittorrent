package t::10000_by_class::Net::BitTorrent::DHT_protocol;
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
use t::10000_by_class::Net::BitTorrent::Test_Utility;
use SmartMatch::Sugar;

use Net::BitTorrent::Network::Utility;
# Prevent dns lookup: in testing all addresses are ip's
# Do this early, before "sockaddr" is imported into someone else
no warnings 'redefine';
sub Net::BitTorrent::Network::Utility::sockaddr($$){ 
  my $rez = AnyEvent::Socket::pack_sockaddr( $_[1], AnyEvent::Socket::parse_address($_[0]) );
  # warn "Resolved $_[0]:$_[1] to ...";
  confess "init udp4_host, and upd6_host, in dht->new" if $_[0] eq '0.0.0.0';
  $rez;
  }
use warnings 'redefine';

use Net::BitTorrent::DHT;
use Net::BitTorrent::Protocol::BEP05::Packets::Render;
use Net::BitTorrent::Protocol::BEP03::Bencode qw(bdecode);

use constant DHT => 'Net::BitTorrent::DHT';
use constant NODE => 'Net::BitTorrent::Protocol::BEP05::Node';
our ($DHT, $Resource_Id_To_Query);
my @AE_hold; # to hold refs

my %DefaultMocks = (
  # 'Net::BitTorrent::Protocol::BEP05::Node::expire_request' => sub {},
  );

sub setup_mock_core {
    # mock 'send' so we don't do real io
    mock_core('send', sub {
        my ($sock, $packet, $flags, $to) = @_;
        warn "Core::Send called with ".sockaddr2ip(getsockname($sock))." ".length($packet);
        return length($packet);
        });
    }
sub setup : Test(setup) {

    my $self = shift;

    $Resource_Id_To_Query = Bit::Vector->new_Dec(160, 0xFAC9BED);

    $DHT = DHT->new( nodeid => 0x12345, udp4_host => '127.0.0.1', udp6_host => '::0' );

    # Capture timer events so we can force them
    AE->mock('timer', \&AnyEvent::Impl::Perl::Mock::timer);
    # This runs our mocked timer events, starting at 0
    $AnyEvent::Impl::Perl::Mock::PsuedoTime = 0;
    AnyEvent::Impl::Perl->mock('one_event', \&AnyEvent::Impl::Perl::Mock::one_event);

    # ensure that we don't sleep during one_event
    push @AE_hold, AE::idle sub {}; # the first call to AE::idle has to provide explicit sub. weird.

    # Watch for getting stuck
    push @AE_hold, AE::unmocked_timer( 5,0,sub{ die "Timed out in AE loop" } );

    while (my ($method, $sub) = each %DefaultMocks) {
      my ($package,$method_name) = $method =~ /(.+)::([^:]+$)/;
      $package->mock($method_name, $sub);
      }
}

sub teardown : Test(teardown) {
  warn "On Teardown:";
  Test::Mock::Method::dump_mocks(); # debug
  @AE_hold = ();
}

sub near_node {
  my ($resource_id, %args) = @_;
  my ($cvs, $in, $caller_plus, $cvs_count) = @args{qw(cond_var in caller_plus cond_var_count)};
  $caller_plus //= 0;
  state $ct = 1; 
  $ct++;
  # 8 entries per MSB (bucket) so we will keep all of them
  my $mask = (8 << int( $ct / 8)) + ($ct % 8);

  # ip/port doesn't matter, but unique
  my @ipport = ("127.0.0.$ct", 456+$ct);

  my $node;
  $node = $DHT->find_or_add_node( \@ipport ) ;

  # warn "New node ",$ipport[0]," ",$DHT->nodeid," = ", $DHT->nodeid->to_Dec ^ $mask;
  $node->_nodeid( Bit::Vector->new_Dec(160, $DHT->nodeid->to_Dec ^ $mask ) );
  $node->routing_table->del_node($node) if ! $in;

  # should send "get_peers" to them
  # warn "Expect get_peers on ",$ipport[0]," with ",$resource_id->to_Hex if $cvs;
  NODE->mock('get_peers', at => 1 + $caller_plus)
    ->with(0 => $node, 1 => $resource_id)
    ->set_cond_var($cvs, $cvs_count)
    ->at_least_once
    # pass through to real get_peers
    ->returns(sub{warn "Called get_peers(".$_[1]->to_Hex.") to $node"; $_->(@_)})
    if $cvs;

  $node;
  }

sub rt_node {
  # generate a node and put it in our routing table
  # and expect a get_peers() on it
  my ($resource_id, $cvs, $do_cvs_count) = @_;

  my $node = near_node($resource_id, cond_var => $cvs, in => 1, caller_plus => 1, cond_var_count => defined($do_cvs_count));

  $node;
  }

sub get_peers_dead_nodes : Tests {
  # Some nodes won't answer, so they get asked multiple times

  my @packets_sent;
  my @nodes; # in our routing-table
  my @get_peers_called;

  push @nodes, rt_node($Resource_Id_To_Query, \@get_peers_called, 'count');
  $get_peers_called[-1]->begin; $get_peers_called[-1]->begin; # twice

  # we only see the first send, because it is rate throttled against real time()
  expect_send(to => $nodes[-1], type => 'get_peers', for => $Resource_Id_To_Query)
    ->set_cond_var(\@packets_sent) 
    ->at_least_once;

  setup_mock_core(); # catch all

  my $request = $DHT->get_peers( $Resource_Id_To_Query, sub { ok 0, "Shouldn't complete get_peers" }); 

  ($_->ready || $_->recv) for (@get_peers_called, @packets_sent);
  }

sub get_peers_have_closer : Tests {

  my @packets_sent;
  my @nodes; # in our routing-table
  my @get_peers_called;
  my @packets_received;

  # Some nodes reveal closer nodes
  
  # These nodes are known by our nodes, but aren't in our routing table
  my @closer_holders;
  push @closer_holders, near_node($Resource_Id_To_Query) for (1..2);


  push @packets_received, my $sentCV = AE::cv;

  # warn "Has closer:";
  for (1..2) {
    push @nodes, rt_node($Resource_Id_To_Query, \@get_peers_called);
    $sentCV->begin;

    expect_send(to => $nodes[-1], type => 'get_peers', for => $Resource_Id_To_Query, 
      reply => sub {
        my ($decoded) = @_;
        # we reply with the closer_holders
        my $trans = pack "C*", map {hex($_)} $decoded->{'transaction_id'} =~ /([a-z0-9]{2})/ig;
        # warn "sent trans $trans:",$decoded->{'transaction_id'},"!";
        $sentCV->end;
        Net::BitTorrent::Protocol::BEP05::Packets::build_dht_reply_get_peers(
          $trans,
          $decoded->{'for'},
          [], # no values
          Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
            map { [$_->host, $_->port] } @closer_holders
            ),
          "ticket".$nodes[-1]->host.":".$nodes[-1]->port
          )
      })
      ->once
      ->set_cond_var(\@packets_sent)
    }

  setup_mock_core(); # catch all

  is $DHT->ipv4_routing_table->count_nodes, scalar(@nodes), "Sanity: nodes in routing table ".@nodes;

  NODE->mock('get_peers', sub {$_->(@_)});
  my $request = $DHT->get_peers( $Resource_Id_To_Query, sub { ok 0, "Shouldn't complete get_peers" }); 

  ($_->ready || $_->recv) for (@get_peers_called, @packets_sent, @packets_received);

  ok $DHT->ipv4_routing_table->find_node_by_sockaddr( Net::BitTorrent::Network::Utility::sockaddr($_->host, $_->port)), "In routing table $_" for @closer_holders;
  }

sub get_peers_closer_till_hit : Tests {
  # Should issue get_peers to discovered nodes till it finds the actual node.

  my @nodes;
  my @closer_holders;
  my @get_peers_called;
  my @packets_received;
  my @packets_sent;

  # our start node, will get a "get_peers"
  push @nodes, rt_node($Resource_Id_To_Query, \@get_peers_called);

  # our closer node
  push @closer_holders, near_node($Resource_Id_To_Query);

  # our "it" node
  push @closer_holders, near_node($Resource_Id_To_Query);

  # the start node will add the closer-node
  push @packets_received, my $sentCV = AE::cv;
  expect_send(to => $nodes[-1], type => 'get_peers', for => $Resource_Id_To_Query, 
    reply => sub {
      my ($decoded) = @_;
      # warn "sent!";
      $sentCV->send;
      # we reply with the closer_holders
      Net::BitTorrent::Protocol::BEP05::Packets::build_dht_reply_get_peers(
        $decoded->{'transaction_id'},
        $decoded->{'for'},
        [], # no values
        Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
          map { [$_->host, $_->port] } $closer_holders[0]
          ),
        "ticket".$nodes[-1]->host.":".$nodes[-1]->port
        )
    })
      ->once
      ->set_cond_var(\@packets_sent)
  }

sub get_peers : Tests {
  # To find peers: (get_peers), who owns resource x?
  # Known nodes closest to Query
  # Ask those nodes: xxx
  # If HIT ("values), done
  # and/or, should have got "closer" nodes
  #   if new closer nodes
  #     ask "closer" nodes again as above
  #   else done
  # Finally,
  # Tell closest nodes about me (announce_peer) (only if we have torrent!)
  # Return peer-address if we got a hit

  my @packets_sent;
  my @get_peers_called;
  my @nodes; # in our routing-table
  my @packets_received;


  # These are the resource-holder nodes, they get returned by somebody below
  my @resource_holder;
  push @resource_holder, near_node($Resource_Id_To_Query) for (1..2);
  # FIXME: @resource_holder should not see have get_peers() called on it?
  # FIXME: test 2nd order: have node, it knows other node, which is holder

  # Some nodes know the resource_holder

  my $holderSentCV = AE::cv;
  push @packets_received, $holderSentCV;
  push @nodes, rt_node($Resource_Id_To_Query, \@get_peers_called); # FIXME: not doing get_peers()! why?
  expect_send(to => $nodes[-1], type => 'get_peers', for => $Resource_Id_To_Query, 
    reply_wait => 10,
    reply => sub {
      my ($decoded) = @_;
      # warn "sent!";
      $holderSentCV->send;
      Net::BitTorrent::Protocol::BEP05::Packets::build_dht_reply_get_peers(
        $decoded->{'transaction_id'},
        $decoded->{'for'},
        [ map { 
          Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4( [$_->host, $_->port] )
          } @resource_holder
          ],
        # "token" for next announce_peer
        "ticket".$nodes[-1]->host.":".$nodes[-1]->port,
        [], # no nodes
        );
    })
    ->once;

  # FIXME: announce?

  # other sends act like they work
  expect_send(to => $_) for @resource_holder; # FIXME: what other sends?
  mock_core('send', sub { length($_[1]) });

  # Sanity on nodes in routing table
  is $DHT->ipv4_routing_table->count_nodes, scalar(@nodes), "Sanity: nodes in routing table ".@nodes;

  my $request = $DHT->get_peers( $Resource_Id_To_Query, sub { ok 0, "Shouldn't complete get_peers" }); 

  # AnyEvent::Impl::Perl::one_event() while $AnyEvent::Impl::Perl::Mock::PsuedoTime < 120;
  ($_->ready || $_->recv) for (@get_peers_called, @packets_received, $holderSentCV);

  # The protocol says that we give up when there are no closer nodes


  }

sub expect_send {
  # to=>$node, type=>$packet_type, other_decoded_keys=>$value, reply => sub{}
  my @non_test = qw(to reply type reply_wait); # things that aren't test::more'd
  my %args = @_;
  my $mocked = mock_core('send');
  if ($args{'to'}) {
      $mocked->with(
        3 => sub{
          my ($to) = @_;
          my @to = sockaddr2ip($to);
          $to[0] eq $args{'to'}->host;
          },
        1 => sub{
          my ($packet) = @_;
          my $hit = 1;
          my %decoded = render_packet($packet);

          # warn "Saw send of packet to ",$args{'to'}->host," ",$decoded{'type'},explain(\%decoded)," ";
          # warn "  ",explain(bdecode($packet)) if $decoded{'type'} eq 'bad?';
          $hit &&= !($decoded{'type'} eq 'bad?' && fail("Expected known packet type, saw '${decoded{'type'}}'"));
          
          if ($args{'type'}) {
            # warn "  Was type ",$decoded{'type'};
            $hit &&= $decoded{'type'} eq $args{'type'};
            }

          $hit;
          },
        );
      }

  $mocked->returns(sub{
    state @transaction_ids;
    my ($sock, $packet, $flags, $to) = @_;

    my %decoded = render_packet($packet);

    subtest "expected send ${decoded{'type'}}", sub {
      ok $decoded{'transaction_id'}, "${decoded{'type'}} has transaction id";
      ok !( @transaction_ids ~~ $decoded{'transaction_id'}), "Don't repeat transaction id ".$decoded{'transaction_id'};
      # warn "seen ",explain(\@transaction_ids);
      push @transaction_ids, $decoded{'transaction_id'};

      my $hit = 1;
      while (my ($k, $v) = each %args) {
        next if @non_test ~~ $k;
        # warn "type of $k ",ref($v);
        $hit = $hit and cmp_ok $decoded{$k},'~~',$v,"${decoded{'type'}} packet has: $k => $v";
        }

      if ($hit) {
        if ($args{'reply'}) {
          # warn "add timer to run the 'reply' sub of ${decoded{'type'}} ",$args{'to'}->host,":",$args{'to'}->port;
          push @AE_hold, AE::timer($args{'reply_wait'} || 1,0,sub{
            # warn "Timer fired to run the 'reply' sub of ${decoded{'type'}} ",$args{'to'}->host,":",$args{'to'}->port;
            $DHT->_on_udp4_in(undef, undef, $args{'to'}->host, $args{'to'}->port, $args{'reply'}->(\%decoded), undef); 
            });
          }
        }
      done_testing;
      };
    length($packet);
    });

  return $mocked;
  }


$PACKAGE->runtests() if !caller;
1;
