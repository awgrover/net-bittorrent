package Net::BitTorrent::Storage::File;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use File::Spec::Functions qw[catfile splitpath catpath];
    use File::Path qw[make_path];
    use Fcntl qw[/O_/ /SEEK/ :flock];
    has 'path' => (is       => 'rw',
                   isa      => 'ArrayRef[Str]',
                   required => 1,
                   init_arg => 'Path',
                   trigger  => sub { $_[0]->close }
    );
    has 'index' => (is       => 'ro',
                    isa      => 'Maybe[Int]',
                    required => 1,
                    init_arg => 'Index'
    );
    has 'length' => (is       => 'ro',
                     isa      => 'Int',
                     required => 1,
                     init_arg => 'Length'
    );
    has 'offset' => (is       => 'ro',
                     isa      => 'Int',
                     default  => 0,
                     init_arg => 'Offset'
    );
    has 'priority' => (is       => 'rw',
                       isa      => subtype(as 'Int' => as enum([0 .. 3])),
                       default  => 2,
                       init_arg => 'Priority'
    );

    #
    has 'filehandle' => (is  => 'rw',
                         isa => 'Maybe[GlobRef]');
    enum 'File::Open::Permission' => qw[ro wo];
    has 'open' => (
        is      => 'rw',
        isa     => 'Maybe[File::Open::Permission]',
        trigger => sub {
            my ($self, $new_mode, $old_mode) = @_;
            if (defined $new_mode) {
                my $path = catfile @{$self->path};
                my ($vol, $dirs, $file) = splitpath($path);
                make_path(catpath $vol, $dirs, '') if $new_mode ne 'ro';
                my $_mode = $new_mode eq 'ro' ? O_RDONLY : O_WRONLY;
                sysopen(my ($FH),
                        $path,
                        $_mode | (($_mode &= O_WRONLY)
                                  ? O_CREAT
                                  : 0
                        )
                    )
                    || return !$self->close();
                $self->filehandle($FH);
                flock $self->filehandle,
                    $new_mode eq 'ro' ? LOCK_SH : LOCK_EX;
            }
            else {
                if ($self->filehandle) {
                    flock $self->filehandle, LOCK_UN;
                    close $self->filehandle;
                }
                $self->filehandle(undef);
            }
        }
    );
    sub close () { !shift->open(undef); }

    sub read ($;$$) {
        my ($self, $offset, $length) = @_;
        return if !$self->open;
        return if $self->open ne 'ro';
        $offset //= 0;
        $length //= $self->length - $offset;
        return
            if $length + $offset > $self->length
                && !$self->isa('Net::BitTorrent::Storage::Cache');
        truncate $self->filehandle, $offset
            if $offset + $length > -s $self->filehandle;
        sysseek $self->filehandle, $offset, SEEK_SET;   # Set correct position
        my $real_length = sysread $self->filehandle, my ($data), $length;
        return if $real_length != $length;
        return $data;
    }

    sub write ($$$) {
        my ($self, $offset, $data) = @_;
        return if !$self->open;
        return if $self->open ne 'wo';
        $offset //= 0;
        return
            if length($data) + $offset > $self->length
                && !$self->isa('Net::BitTorrent::Storage::Cache');
        truncate $self->filehandle, $offset
            if $offset + length $data > -s $self->filehandle;
        sysseek $self->filehandle, $offset, SEEK_SET;   # Set correct position
        return syswrite $self->filehandle, $data, length($data);
    }
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
