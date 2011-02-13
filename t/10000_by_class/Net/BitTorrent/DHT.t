package t::10000_by_class::Net::BitTorrent::DHT;

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

our %RestoreFromMock;

sub class {'Net::BitTorrent::DHT'}

sub startup : Tests(startup => no_plan) {
    my $self = shift;

    use_ok $self->class;
    can_ok $self->class, 'new';
    $self->{'dht'}
        = new_ok($self->class, [], 'decoy NB client');
}

sub setup : Test(setup) {
    my $self = shift;
}

sub teardown : Test(teardown) {
}

sub nodeid : Tests {
    my $pig = shift->{dht};
    ok($pig->nodeid, 'nodeid is defined');
    isa_ok $pig->nodeid, 'Bit::Vector';
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

    sub listens : Tests {
        my $tester=shift;
        use Net::BitTorrent::Network::Utility;
        state $ip6=0;
        Net::BitTorrent::Network::Utility
            ->mock('server', sub {confess "Called twice" if ++$ip6 > 1; undef},)
            ->with(0 => qr/:/, 4 => 'udp' )
            ->once
            #->returns('Net::BitTorrent::Network::Utility');
            ;
        Net::BitTorrent::Network::Utility
            ->mock('server')
            ->with(0 => qr/\./, 4 => 'udp' )
            ->once
            #->returns('Net::BitTorrent::Network::Utility');
            ;

        my $dht = $tester->class->new;
        }
 
__PACKAGE__->runtests() if !caller;
1;
