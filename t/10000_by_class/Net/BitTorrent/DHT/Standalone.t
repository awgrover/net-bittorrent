package t::10000_by_class::Net::BitTorrent::DHT::Standalone;
{
    use strict;
    use warnings;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent/DHT.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::DHT';
    use Test::More;
    use Test::Moose;

    #
    sub new_args {
        my $t = shift;
        [port => [1337 .. 3339, 0],
         boot_nodes =>
             [['router.utorrent.com', 6881], ['router.bittorrent.com', 6881]],
         on_listen_failure => sub {
             my ($s, $a) = @_;
             diag $a->{'message'};
             $t->{'cv'}->send;
         },
         on_listen_success => sub {
             my ($s, $a) = @_;
             diag $a->{'message'};
             }
        ];
    }

    sub check_role : Test( 9 ) {
        my $self = shift;
        does_ok $self->{'dht'}, 'Net::BitTorrent::DHT::Standalone';
        has_attribute_ok $self->{'dht'}, $_ for qw[port
            udp6 udp6_sock udp6_host
            udp4 udp4_sock udp4_host ];
        ok !$self->{'dht'}->has_client,
            '... standalone dht nodes have no client';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
