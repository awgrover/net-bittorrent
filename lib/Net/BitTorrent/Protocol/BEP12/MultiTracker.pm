package Net::BitTorrent::Protocol::BEP12::MultiTracker;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use Carp qw[carp];
    use List::Util qw[shuffle];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:tracker :bencode];
    has 'torrent' => (isa       => 'Net::BitTorrent::Torrent',
                      is        => 'ro',
                      weak_ref  => 1,
                      writer    => '_torrent',
                      predicate => 'has_torrent',
                      handles   => {client => 'client'}
    );
    has 'tiers' => (
                traits  => ['Array'],
                isa     => 'NBTypes::Tracker::Tier',
                is      => 'rw',
                coerce  => 1,
                default => sub { [] },
                handles => {_push_tier => 'push', _shuffle_tiers => 'shuffle'}
    );
    my $tier_constraint;

    sub add_tier {
        my ($self, $urls) = @_;
        $tier_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                    'NBTypes::Tracker::Tier');
        $self->_push_tier($tier_constraint->coerce($urls));
        $self->_shuffle_tiers;
    }

    has 'quests' => (
        isa => 'ArrayRef',
        is => 'ro',
        traits => ['Array'],
        default => sub {[]},
        handles => {
            'add_quest' => 'push'
        }
    );

    sub announce {
        my ($self, $event) = @_;
        my %args = (info_hash  => $self->torrent->info_hash->to_Hex,
                    peer_id    => $self->client->peer_id,
                    port       => $self->client->port,
                    uploaded   => $self->torrent->uploaded,
                    downloaded => $self->torrent->downloaded,
                    left       => $self->torrent->left
        );
        $args{'info_hash'} =~ s|(..)|\%$1|g;
        $self->add_quest($_->[0]->announce($event, \%args, sub {
            use Data::Dump;
            ddx \@_;
        } )) for @{$self->tiers};
    }

    sub scrape {
        my ($self) = @_;
        $_->[0]->scrape() for @{$self->tiers};
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP12 - Multitracker Metadata Extension

=head1 See Also

=over

=item BEP 12:

http://bittorrent.org/beps/bep_0012.html

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
