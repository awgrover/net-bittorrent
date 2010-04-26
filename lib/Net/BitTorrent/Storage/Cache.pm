package Net::BitTorrent::Storage::Cache;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    use File::Spec::Functions qw[catfile splitpath catpath];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    extends 'Net::BitTorrent::Storage::Node';
    has 'storage' => (is       => 'ro',
                      isa      => 'Net::BitTorrent::Storage',
                      init_arg => 'Storage',
                      required => 1
    );
    has 'path' => (
        is         => 'ro',
        isa        => 'ArrayRef',
        lazy_build => 1,
        builder    => '_build_path',
        init_arg   => 'Path',
        trigger    => sub {
            my ($self, $new, $old);
            $self->close;
            unlink catpath @{$old};
        }
    );

    sub _build_path {
        ['~' . substr($_[0]->storage->torrent->infohash, 0, 7) . '.dat'];
    }
    has 'packets' => (traits  => ['Hash'],
                      is      => 'ro',
                      isa     => 'HashRef[Torrent::Cache::Packet]',
                      default => sub { {} },
                      handles => {_add_packet => 'set',
                                  _get_packet => 'get',
                                  _del_packet => 'delete',
                                  is_empty    => 'is_empty',
                                  size        => 'count'
                      }
    );

    sub add_packet ($$$$) {
        my ($self, $index, $offset, $data) = @_;
        $self->close() if defined $self->open && $self->open ne 'wo';
        $self->open('wo') || return;
        my $_offset = -s $self->filehandle;
        $self->write($_offset, $data) || die $!;
        return $self->_add_packet(
                          $index . '|' . $offset => [$_offset, length $data]);
    }

    sub get_packet ($$;$) {
        my ($self, $index, $offset) = @_;
        $offset //= 0;
        my $where = $self->_get_packet($index . '|' . $offset);
        return if !defined $where;
        $self->close() if defined $self->open && $self->open ne 'ro';
        $self->open('ro') || return;
        return $self->read(@$where);
    }

    sub del_packet ($$;$) {
        my ($self, $index, $offset) = @_;
        $offset //= 0;
        my $where = $self->_del_packet($index . '|' . $offset);
        return if !defined $where;
        $self->close() if defined $self->open && $self->open ne 'ro';
        $self->open('ro') || return;
        my $data = $self->read(0, -s $self->filehandle);
        substr($data, $where->[0], $where->[1], '');
        $self->close();
        rename catfile(@{$self->path}), catfile(@{$self->path}) . '.old';
        $self->open('wo') || return;
        return $self->write(0, $data);
    }
    sub _resume { return $_->packets }
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
