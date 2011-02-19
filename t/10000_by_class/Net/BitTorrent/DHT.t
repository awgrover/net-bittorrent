package t::10000_by_class::Net::BitTorrent::DHT;

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
use Test::Mock::Moose;
use Carp;
use Net::BitTorrent::Protocol::BEP05::Packets::Render;
use Data::Dumper;

our %RestoreFromMock;

sub class {'Net::BitTorrent::DHT'}

sub startup : Tests(startup => no_plan) {
    my $self = shift;

    use_ok $self->class;
    can_ok $self->class, 'new';
}

sub setup : Test(setup) {
    my $self = shift;
}

sub teardown : Test(teardown) {
}

sub nodeid : Tests {
    my $self = shift;
    my $pig = new_ok($self->class, [], 'decoy NB client');
    ok($pig->nodeid, 'nodeid is defined');
    isa_ok $pig->nodeid, 'Bit::Vector';

    my $dht = $self->class->new( nodeid => '12345');
    ok $dht->nodeid, 'nodeid is defined';
    is $dht->nodeid->to_Dec, '12345', "nodeid can be set on new";
}

sub talks_to_boot_nodes : Tests {
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
        } );

    my $dht = $tester->class->new( boot_nodes => [ ['10.5.98.12',1234], ['10.23.8.55',7899] ] );
    is $saw_find_node, 1, 'saw advertisement'
    }

sub sockaddr2ip {
    my ($port, $packed) = Net::BitTorrent::Network::Utility::unpack_sockaddr(@_);
    my $ip = Net::BitTorrent::Network::Utility::paddr2ip($packed);
    return ($ip, $port);
    }

sub sends_data : Tests {
    my $tester = shift;
    # Net::BitTorrent::DHT->mock( 'send' )->once;
    my @test_to_addrs = (['10.5.98.12',1234], ['FC12:3456:6789::', 1256]);
    my $dht;
    my @sent_ping;
    mock_core('send', sub {
        my ($sock, $packet, $flags, $to) = @_;

        warn "in mocked send";
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
        $dht = $tester->class->new( boot_nodes => \@test_to_addrs );
        is_deeply [sort @sent_ping], [sort map {$_->[0]} @test_to_addrs], "sent a ping to the boot_nodes";
        done_testing;
        }
    }

sub listens : Tests {
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

    my $dht = $tester->class->new;
    }
 
__PACKAGE__->runtests() if !caller;
1;
