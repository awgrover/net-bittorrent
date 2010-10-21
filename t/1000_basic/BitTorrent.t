package t::Net::BitTorrent;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use parent 'Test::Class';
    use Test::Moose;
    use Test::Exception;

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load local modules
    use lib '../../../../../../lib', 'lib';
    use Net::BitTorrent;

    #
    sub class     {'Net::BitTorrent'}

    sub init_args { }

    #
    sub build : Test( startup => 1 ) {
        my $s = shift;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];


    }


    sub peer_id : Test( 1 ) {
        my $s = shift;
        like $s->{'m'}->peer_id, qr[^NB\d{3}[SU]-.{13}$];
    }

    sub class_can : Test( 0 ) {
        my $s = shift;
        #can_ok $s->{'m'}, $_ for qw[size as_string];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 1 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_
            for
            qw[peer_id];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    #
    __PACKAGE__->runtests() if !caller;
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
