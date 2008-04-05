package Net::BitTorrent::Session::Peer::Request;
use strict;
use warnings;
{

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Carp qw[carp];
    {
        my ( %peer, %index, %offset, %length, %timestamp );

        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if (     defined $args->{q[peer]}
                 and defined $args->{q[index]}
                 and defined $args->{q[offset]}
                 and defined $args->{q[length]} )
            {
                $self =
                    bless \sprintf( q[R I:%d:O:%d:L:%d],
                                $args->{q[index]}, $args->{q[offset]},
                                $args->{q[length]} ),
                    $class;
                $peer{$self}      = $args->{q[peer]};
                $index{$self}     = $args->{q[index]};
                $offset{$self}    = $args->{q[offset]};
                $length{$self}    = $args->{q[length]};
                $timestamp{$self} = time;
            }
            return $self;
        }
        sub peer    { my ($self) = @_; return $peer{$self} }
        sub session { my ($self) = @_; return $peer{$self}->session }

        sub client {
            my ($self) = @_;
            return $peer{$self}->session->client;
        }
        sub index     { my ($self) = @_; return $index{$self} }
        sub offset    { my ($self) = @_; return $offset{$self} }
        sub length    { my ($self) = @_; return $length{$self} }
        sub timestamp { my ($self) = @_; return $timestamp{$self} }

        sub piece {
            my ($self) = @_;
            return $peer{$self}->session->pieces->[ $index{$self} ];
        }

        sub _build_packet_args {    # unused
            my ($self) = @_;
            return ( index  => $index{$self},
                     offset => $offset{$self},
                     length => $length{$self},
                     data   => $self->_read
            );
        }

        sub _read {
            my ($self) = @_;
            return $self->piece->_read( $offset{$self},
                                        $length{$self} );
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my $dump = $self . q[ [TODO]];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my ($self) = @_;
            delete $peer{$self};
            delete $index{$self};
            delete $offset{$self};
            delete $length{$self};
            delete $timestamp{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Peer::Request - Incoming request

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Peer::Request> object.  This
constructor should not be used directly.

=back

=head1 METHODS

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Peer::Request> object's data structure.
If called in void context, the structure is printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
request.

=item C<index ( )>

Returns the zero based index of the related
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object.

=item C<length ( )>

Returns the amount of data the peer requested.

=item C<offset ( )>

Returns the offset of data the peer requested.

=item C<peer ( )>

Returns the
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer>
object related to this request.

=item C<piece ( )>

Returns the
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object related to this request.

=item C<session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this request.

=item C<timestamp ( )>

Returns the time when the request was made.

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - L<http://sankorobinson.com/>

CPAN ID: SANKO

ProperNoun on Freenode

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See
L<http://www.perl.com/perl/misc/Artistic.html> or the F<LICENSE> file
included with this module.

All POD documentation is covered by the Creative Commons
Attribution-Noncommercial-Share Alike 3.0 License
(L<http://creativecommons.org/licenses/by-nc-sa/3.0/us/>).

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
