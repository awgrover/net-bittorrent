package Net::BitTorrent::Torrent;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../lib';
    use Net::BitTorrent::Types qw[:torrent];

    # Meat
    use lib '../../';
    has 'client' => (
        isa      => 'Maybe[Net::BitTorrent]',
        is       => 'rw',
        weak_ref => 1,
        init_arg => 'Client',
        trigger  => sub {
            my ($self, $client) = @_;

            # XXX - make sure the new client knows who I am
            #$self->queue;
            warn 'TODO: Start trackers!';
            $self->tracker->announce('start');
        }
    );
    has 'error' => (is       => 'rw',
                    isa      => 'Str',
                    init_arg => undef
    );
    has 'storage' => (is         => 'ro',
                      required   => 1,
                      isa        => 'Net::BitTorrent::Storage',
                      lazy_build => 1,
                      builder    => '_build_storage',
                      init_arg   => 'Storage',
                      handles    => {
                                  size => 'size',
                                  read => 'read'
                      }
    );

    sub _build_storage {
        require Net::BitTorrent::Storage;
        Net::BitTorrent::Storage->new(Torrent => $_[0]);
    }
    for my $direction (qw[up down]) {
        has $direction
            . 'loaded' => (
                         is      => 'ro',
                         isa     => 'Int',
                         traits  => ['Counter'],
                         handles => {'inc_' . $direction . 'loaded' => 'inc'},
                         default => 0
            );
    }

    sub left {
        my ($self) = @_;
        require List::Util;
        return $self->piece_length
            * List::Util::sum(
                            split('', unpack('b*', ($self->wanted() || ''))));
    }
    # Quick methods
    my $pieces_per_hashcheck = 10;    # Max block of pieces in single call

    sub hashcheck {    # Range is split up into $pieces_per_hashcheck blocks
        my ($self, $range) = @_;
        $range
            = defined $range
            ? ref $range
                ? $range
                : [$range]
            : [0 .. $self->piece_count - 1];
        if (scalar @$range <= $pieces_per_hashcheck) {
            $self->_clear_have();
            for my $index (@$range) {
                my $piece = $self->read($index);
                next if !$piece || !$$piece;
                require Digest::SHA;
                $self->have->Bit_On($index)
                    if Digest::SHA::sha1($$piece) eq
                        substr($self->pieces, ($index * 20), 20);
            }
        }
        else {
            my $cv = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, @ranges, @this_range, $coderef);
            push @ranges, [splice(@$range, 0, $pieces_per_hashcheck, ())]
                while @$range;
            $coderef = sub {
                shift @watchers if @watchers;
                @this_range = shift @ranges;
                $self->hashcheck(@this_range);
                push @watchers,
                    AE::idle(@ranges ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
        return 1;
    }


    #
    with 'Net::BitTorrent::Protocol::BEP03::Metadata';
    no Moose;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

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
