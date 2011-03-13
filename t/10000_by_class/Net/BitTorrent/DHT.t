package t::10000_by_class::Net::BitTorrent::DHT;
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

use constant Class => 'Net::BitTorrent::DHT';
my @AE_hold; # to hold refs

sub module_loads_and_can_new : Tests(startup => no_plan) {
    my $self = shift;

    use_ok Class;
    can_ok Class, 'new';
}

my %DefaultMocks = (
  'Net::BitTorrent::Protocol::BEP05::Node::expire_request' => sub {},
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

sub nodeid_auto_and_explicit : Tests {
    my $self = shift;
    my $pig = new_ok(Class, [(udp4_host => '127.0.0.1', udp6_host => '::0')], 'decoy NB client');
    ok($pig->nodeid, 'nodeid is defined');
    isa_ok $pig->nodeid, 'Bit::Vector';

    my $dht = Class->new( nodeid => '12345', udp4_host => '127.0.0.1', udp6_host => '::0');
    ok $dht->nodeid, 'nodeid is defined';
    is $dht->nodeid->to_Dec, '12345', "nodeid can be set on new";
}

sub talks_to_boot_nodes_at_new : Tests {
    my $tester=shift;

    my $saw_find_node = undef;

    use Net::BitTorrent::Protocol::BEP05::Node;
    Net::BitTorrent::Protocol::BEP05::Node->mock( 'find_node', sub{
        my $self = shift;
        my ($target) = @_;
        isa_ok $self, 'Net::BitTorrent::Protocol::BEP05::Node';
        (isa_ok $target, 'Bit::Vector') && do {
            if ($self->dht->nodeid ne $target) {
                warn "Saw target node ".ref($target)." $target vs ",ref($self->dht->nodeid);
                return;
                }
            };
        is $self->dht->nodeid, $target, 'advertised dht';
        $saw_find_node=1;
        } )
        ->with( 0 => inv_isa('Net::BitTorrent::Protocol::BEP05::Node'), 1 => inv_isa('Bit::Vector'));

    my $dht = Class->new( boot_nodes => [ ['10.5.98.12',1234], ['10.23.8.55',7899] ], udp4_host => '127.0.0.1', udp6_host => '::0' );
    is $saw_find_node, 1, 'saw advertisement'
    }

sub sockaddr2ip {
    my ($port, $packed) = Net::BitTorrent::Network::Utility::unpack_sockaddr($_[0]);
    my $ip = Net::BitTorrent::Network::Utility::paddr2ip($packed);
    return ($ip, $port);
    }

sub sends_ping_to_boot_nodes_at_new : Tests {
    my $tester = shift;
    my @test_to_addrs = (['10.5.98.12',1234], ['FC12:3456:6789::', 1256]);
    my @cv;

    unmock_core('send');
    foreach my $of_interest (@test_to_addrs) {
      mock_core('send')
        ->at_least_once
        ->with(3 => sub{
          my ($to) = @_;
          my @to = sockaddr2ip($to);
          $to[0] eq $of_interest->[0];
          })
        ->set_cond_var(\@cv)
        ->returns(sub{
          my ($sock, $packet, $flags, $to) = @_;
          my @d_to = sockaddr2ip($to);
          is_deeply \@d_to, $of_interest, 'sending to expected address';

          my @from = sockaddr2ip(getsockname($sock));
          like $from[0],qr/^(127\.0\.0\.1|::0?)+$/, 'sending from localhost '.$from[0];

          my %decoded = render_packet($packet);
          ok $decoded{'ask'}, "asking someone";
          is $decoded{'ask'}, $decoded{'them'}, "whom and about is same ".$decoded{'ask'};
          });
        }
    mock_core('send', sub {length($_[1])});

    Net::BitTorrent::Protocol::BEP05::RoutingTable->mock('del_node', sub {confess "DEL_NODE"});
    Net::BitTorrent::Protocol::BEP05::Node->mock('_build_ping_timer', sub {{}});
    # Net::BitTorrent::Protocol::BEP05::Node->mock('find_node', sub {{}});
    subtest "Test ping to each boot_node", sub {
        # need the host(s) or you get funny behavior in find_node()
        my $dht = Class->new( boot_nodes => \@test_to_addrs, udp4_host => '127.0.0.1', udp6_host => '::0' );

        $_->recv for @cv;
        # AnyEvent::Impl::Perl::one_event for (1..20);

        done_testing;
        }
    }

sub listens_at_new : Tests {
    my $tester=shift;
    use Net::BitTorrent::Network::Utility;
    state $ip6=0;

    Net::BitTorrent::Network::Utility
        ->mock('server')
        ->with(0 => qr/:/, 4 => 'udp' )
        ->once
        ;
    Net::BitTorrent::Network::Utility
        ->mock('server')
        ->with(0 => qr/\./, 4 => 'udp' )
        ->once
        ;

    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');
    }

sub adds_nodes_to_correct_table : Tests {
    my $tester = shift;

    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');

    ok($dht->ipv6_routing_table, "has ipv6 routing table") or return;
    ok($dht->ipv4_routing_table, "has ipv4 routing table") or return;

    my @test_to_addrs = (['10.5.98.12',1234], ['FC12:3456:6789::', 1256]);
    Net::BitTorrent::Protocol::BEP05::RoutingTable->mock('add_node', sub { undef})
        ->once
        ->with(0 => $dht->ipv4_routing_table, 1 => $test_to_addrs[0]);
    Net::BitTorrent::Protocol::BEP05::RoutingTable->mock('add_node', sub { undef})
        ->once
        ->with(0 => $dht->ipv6_routing_table, 1 => $test_to_addrs[1]);
    foreach my $ipp (@test_to_addrs) {
        $dht->find_or_add_node($ipp);
        }
    }

sub get_peers_calls_node_get_peers : Tests {
    my $tester=shift;

    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0') ;

    my $of_interest = Bit::Vector->new_Dec(160,'12345');
    $dht->ipv4_routing_table->add_node(['127.0.0.1', $dht->port])->_nodeid($of_interest);
    Net::BitTorrent::Protocol::BEP05::RoutingTable->mock('del_node',sub { confess "DELETER"});

    # timer blows up at end of block
    my $timer = AE::unmocked_timer( 5,0,sub{ die "got stuck in get_peers (should run immediately in eventloop)" } );

    my $cv = AE::cv;
    Net::BitTorrent::Protocol::BEP05::Node->mock('get_peers', sub { $cv->send; undef})
        ->once
        ->with( 1 => $of_interest );
    my $w2 = $dht->get_peers($of_interest, sub {fail "shouldn't call this"});
    ok(!$cv->ready,"Doesn't call get_peers till event loop");
    $cv->recv;
    }

sub announce_peer_calls_node_announce_peer_later : Tests {
    my $tester=shift;

    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');
    $dht->ipv4_routing_table->add_node(['127.0.0.2', $dht->port])->_nodeid(0x91234);

    my @of_interest = (Bit::Vector->new_Dec(160,'12345'), 4567);
    my $cv = AE::cv;
    # announce_peer is inside a AE::Timer
    # Mock at least one AE::Timer, where it repeats {
    # Within (called by, caused by) an AE::Time (which repeats), expect
    # simulate an AE::Timer (which repeats) for
    # (eventually) Expect an AE::Timer (which repeats) for
    # AE::Timer, with repeat, expects xxx::xxx
    # within AE::Timer, expect ....
    # expect xxxx, within ae::timer
    # AE::Timer->will_expect xx::xx->...
    # xx::xx will be called by an ae::timer that repeats
    # I expect xx::xx will be called by the firing of an ae::timer (with repeats)
    #   turns on mock interceptions of ae::timer, and one_event
    #   otherwise, should be transparent
    # I expect my timer to be fired 2 times..

    # We expect this to be fired by a timer initially, and repeatedly
    Net::BitTorrent::Protocol::BEP05::Node->mock('announce_peer', sub { $cv->send; undef})
        ->at_least_twice
        ->with( 1 => $of_interest[0], 2 => $of_interest[1] );

    # disable these
    Net::BitTorrent::Protocol::BEP05::Node->mock('find_node');

    my $w2 = $dht->announce_peer(@of_interest, sub {fail "shouldn't call this"});

    ok(!$cv->ready,"Doesn't call get_peers till event loop");

    AnyEvent::Impl::Perl::one_event for (1..10); # while (!$cv->ready);
    }

sub find_node_causes_node_find_node : Tests {
    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');
    my $sender = ['127.0.0.2', $dht->port];
    my $sender_nodeid = Bit::Vector->new_Dec(160,0x91234);
    $dht->ipv4_routing_table->add_node($sender)->_nodeid($sender_nodeid);

    my $cv = AE::cv;
    # will be called via a timer
    Net::BitTorrent::Protocol::BEP05::Node->mock('find_node')
      ->with(0 => sub{
          my $self=shift;
          $cv->send;
          $self->nodeid ~~ $sender_nodeid;
          })
      ->at_least_once;
    $dht->find_node( Bit::Vector->new_Dec(160, 0x7654) );
    $cv->recv;
    }

sub incoming_udp_discards_bad_packets : Tests {

  my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');

  # bad packets don't
  my @bad_source = ( '127.0.0.3', 888 );
  Net::BitTorrent::DHT->mock('find_or_add_node')
    ->with(1 => \@bad_source)
    ->never;

  # Since I mock _on_udp6_in, the $sock, $sockaddr, and $flags can be dumy
  my $bad_data = "bad data";
  for my $prot ((4,6)) {
    my $oner = "_on_udp${prot}_in";
    $dht->$oner( undef, undef, @bad_source, $bad_data, undef );
    }

  is $dht->_recv_invalid_count, 2, 'saw the 2 bad packet';
  is $dht->_recv_invalid_length, 2*length($bad_data), 'saw the right amount of bad data';
  }

sub incoming_udp_adds_to_routing : Tests {

  subtest "incoming_udp_adds_to_routing", sub {
    my $dht = Class->new(udp4_host => '127.0.0.1', udp6_host => '::0');
    # Good packets do
    my @good_source = ( '127.0.0.4', 4441 );
    my $message = Net::BitTorrent::Protocol::BEP05::Packets::build_dht_query_ping("trans1",
                                        pack('H*', $dht->nodeid->to_Hex)
                                        );
    Net::BitTorrent::DHT->mock('find_or_add_node')
      ->with(1 => \@good_source)
      ->twice
      ->returns(sub{ Net::BitTorrent::Protocol::BEP05::Node->new(host=> '127.0.0.9', port=>1, routing_table=>$dht->ipv4_routing_table)});

    for my $prot ((4,6)) {
      my $oner = "_on_udp${prot}_in";
      ok 1,"Calling $oner";
      $dht->$oner( undef, undef, @good_source, $message, undef );
      }
    
    # remove these 2 when the todo is done
      is $dht->_recv_requests_count, 1, 'saw the 2 good messages';
      is $dht->_recv_requests_length,length($message), 'saw the right amount of data';
    TODO : {
      local $TODO = 'udp6 handling not implemented';
      is $dht->_recv_requests_count, 2, 'saw the 2 good messages';
      is $dht->_recv_requests_length, 2*length($message), 'saw the right amount of data';
      }

    done_testing;
    };
  ok 1,"OUT";
  }

sub ip_filter : Tests {
  TODO : {
    local $TODO = "Write tests for the ip_filtering and filtering in send";
    ok 0, "Write tests for the ip_filtering and filtering in send";
    }
  }

sub packet_handling : Tests {
  TODO : {
    local $TODO = "Write tests for the packet_handling";
    ok 0, "Write tests for the packet_handling";
    }
  }

$PACKAGE->runtests() if !caller;
1;
