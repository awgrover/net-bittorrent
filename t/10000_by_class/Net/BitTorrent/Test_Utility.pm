package t::10000_by_class::Net::BitTorrent::Test_Utility;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(sockaddr2ip);
use strict; use warnings; no warnings 'uninitialized';

sub sockaddr2ip {
    my ($port, $packed) = Net::BitTorrent::Network::Utility::unpack_sockaddr($_[0]);
    my $ip = Net::BitTorrent::Network::Utility::paddr2ip($packed);
    return ($ip, $port);
    }

1;
