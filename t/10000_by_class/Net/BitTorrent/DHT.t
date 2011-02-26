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
use Test::Moose;
use AnyEvent::Impl::Perl;   # Timing is different than with EV. Go figure.
use AnyEvent;
use Test::Mock::Method;
use Carp;
use Net::BitTorrent::Protocol::BEP05::Packets::Render;
use Data::Dumper;
use SmartMatch::Sugar;

use constant Class => 'Net::BitTorrent::DHT';

sub module_loads_and_can_new : Tests(startup => no_plan) {
    my $self = shift;

    use_ok Class;
    can_ok Class, 'new';
}

sub setup : Test(setup) {
    my $self = shift;
}

sub teardown : Test(teardown) {
}

sub nodeid_auto_and_explicit : Tests {
    my $self = shift;
    my $pig = new_ok(Class, [], 'decoy NB client');
    ok($pig->nodeid, 'nodeid is defined');
    isa_ok $pig->nodeid, 'Bit::Vector';

    my $dht = Class->new( nodeid => '12345');
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

    my $dht = Class->new( boot_nodes => [ ['10.5.98.12',1234], ['10.23.8.55',7899] ] );
    is $saw_find_node, 1, 'saw advertisement'
    }

sub sockaddr2ip {
    my ($port, $packed) = Net::BitTorrent::Network::Utility::unpack_sockaddr(@_);
    my $ip = Net::BitTorrent::Network::Utility::paddr2ip($packed);
    return ($ip, $port);
    }

sub sends_ping_to_boot_nodes_at_new : Tests {
    my $tester = shift;
    # Net::BitTorrent::DHT->mock( 'send' )->once;
    my @test_to_addrs = (['10.5.98.12',1234], ['FC12:3456:6789::', 1256]);
    my $dht;
    my @sent_ping;
    mock_core('send', sub {
        my ($sock, $packet, $flags, $to) = @_;

        subtest "Seeing a 'send'", sub {

            my @to = sockaddr2ip($to);
            my @hit = grep { $to[0] eq $_->[0] } @test_to_addrs;

            ok(@hit, "sending to expected address ".join(' : ',@{$hit[0]})) or return;

            is_deeply \@to, $hit[0], 'sending to expected address';

            my @from = sockaddr2ip(getsockname($sock));

            like $from[0],qr/^[.0:]+$/, 'sending from localhost '.$from[0];

            my %decoded = render_packet($packet);
            ok $decoded{'ask'}, "asking someone";
            is $decoded{'ask'}, $decoded{'them'}, "whom and about is same ".$decoded{'ask'};

            push @sent_ping, $to[0] if $decoded{'ask'};

            done_testing;
            };
        });
    subtest "Test ping to each boot_node", sub {
        $dht = Class->new( boot_nodes => \@test_to_addrs );
        is_deeply [sort @sent_ping], [sort map {$_->[0]} @test_to_addrs], "sent a ping to the boot_nodes";
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

    my $dht = Class->new;
    }

sub adds_nodes_to_correct_table : Tests {
    my $tester = shift;

    my $dht = Class->new;

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

    # doesn't fire unless you keep the ref
    my $dht = Class->new;
    $dht->ipv4_routing_table->add_node(['127.0.0.1', $dht->port])->_nodeid('1234');

    my $of_interest = Bit::Vector->new_Dec(160,'12345');
    my $cv = AE::cv;
    my $not_yet = 0;
    Net::BitTorrent::Protocol::BEP05::Node->mock('get_peers', sub { $cv->send; $not_yet = 1; undef})
        ->once
        ->with( 1 => $of_interest );
    my $w2 = $dht->get_peers($of_interest, sub {fail "shouldn't call this"});
    ok(!$not_yet,"Doesn't call get_peers till event loop");
    $cv->recv;
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
