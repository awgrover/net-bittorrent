package Net::BitTorrent::Protocol::BEP23::Compact;
{
    use strict;
    use warnings;
    use Carp qw[carp confess];
    use Fcntl ':flock';
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[compact_ipv4 uncompact_ipv4];
    %EXPORT_TAGS = (all => [@EXPORT_OK], bencode => [@EXPORT_OK]);

    sub compact_ipv4 {
        my (@peers) = @_;
        my $return;
    PEER: for my $peer (@peers) {
            next if not $peer;
            my ($ip, $port) = @$peer;

            if ($port < 0 || $port >= 2**16) {
                carp 'Port number beyond ephemeral range: ' . $port;
                next PEER;
            }
            elsif (my @octets = $ip =~ /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/) {
              foreach (@octets) { if ($_ > 255) {carp "IPv4 octect out of range $ip"; next PEER }}
              $return .= pack 'C4n', @octets, int $port;
              }
            else {   
              carp 'Invalid IPv4 address: ' . $ip;
            }
        }
        confess "No peers compacted" if !$return; 
        return $return;
    }

    sub uncompact_ipv4 {
      my @h_p = unpack "(C4n)*", $_[0];
      my @rez;
      while (@h_p) {
        push @rez, [sprintf('%s.%s.%s.%s', splice @h_p,0,4), shift @h_p];
        }
      @rez;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP23::Compact - Utility functions for BEP32: Tracker Returns Compact Peer Lists

=head1 Importing From Net::BitTorrent::Protocol::BEP23::Compact

By default, nothing is exported.

You may import any of the following or use one or more of these tag:

=over

=item C<:all>

Imports the tracker response-related functions
L<compact|/"compact_ipv4 ( LIST )"> and
L<uncompact|/"uncompact_ipv4 ( STRING )">.

=back

=head1 Functions

=over

=item C<compact_ipv4 ( LIST )>

Compacts a list of IPv4:port strings into a single string.

A compact peer is 6 bytes; the first four bytes are the host (in network byte
order), the last two bytes are the port (again, in network byte order).

=item C<uncompact_ipv4 ( STRING )>

Inflates a compacted string of peers and returns a list of IPv4:port strings.

=back

=head1 See Also

=over

=item BEP 32: Tracker Returns Compact Peer Lists

http://bittorrent.org/beps/bep_0023.html

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
