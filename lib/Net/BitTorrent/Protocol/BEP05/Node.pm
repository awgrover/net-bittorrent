package Net::BitTorrent::Protocol::BEP05::Node;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use AnyEvent;
    use Carp;
    use Net::BitTorrent::Types qw[NBTypes::DHT::NodeID];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use 5.010.000;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    use overload '""' => sub { my $self=shift; sprintf "<%s %s:%d>",(overload::StrVal($self) =~ /^Net::BitTorrent::Protocol::(.+)/)[0],$self->host, $self->port };

    use constant GOOD_UNTIL => 15 * 60; # in seconds
    use constant GET_PEERS_RETRY => 15; # seconds

    #
    sub BUILD {1}

    #
    has 'port' => (isa => 'Int', is => 'ro', required => 1);
    has 'host' => (isa => 'Str', is => 'ro', required => 1);
    has 'sockaddr' =>
        (isa => 'Str', is => 'ro', required => 1, lazy_build => 1);

    sub _build_sockaddr {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::sockaddr($_[0]->host, $_[0]->port);
    }

    # just a cached predicate
    has 'is_ipv6' => (isa => 'Bool', is => 'ro', lazy_build => 1);

    sub _build_is_ipv6 { length shift->sockaddr == 28 }

    for my $dir (qw[in out]) {
        has "announce_peer_token_$dir"
             => (isa     => 'HashRef[Str]',
                       is      => 'ro',
                       traits  => ['Hash'],
                       handles => {
                               "_set_announce_peer_token_$dir" => 'set',
                               "_get_announce_peer_token_$dir" => 'get',
                               "_del_announce_peer_token_$dir" => 'delete',
                               "has_announce_peer_token_$dir" => 'defined'
                       },
                       default => sub { {} }
            );
    }

    # 'v' is optional "version" of the DHT implementation, from the incoming bencode data
    has 'v' =>
        (isa => 'Str', is => 'ro', writer => '_v', predicate => 'has_v');
    has 'bucket' => (isa       => 'Net::BitTorrent::Protocol::BEP05::Bucket',
                     is        => 'ro',
                     writer    => 'assign_bucket',
                     weak_ref  => 1,
                     predicate => 'has_bucket'
    );
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      predicate  => 'has_routing_table',
                      writer     => '_routing_table',
                      weak_ref   => 1,
                      handles    => [qw[send dht tracker]]
    );
    around 'send' => sub {
        my ($code, $self, $packet, $reply) = @_;
        $code->($self, $self, $packet, !!$reply); # this looks redundant
    };
    has 'nodeid' => (isa       => 'NBTypes::DHT::NodeID',
                     is        => 'ro',
                     writer    => '_nodeid',
                     predicate => 'has_nodeid',
                     coerce    => 1
    );
    after '_nodeid' => sub {
        confess "You must provide a routing_table for a Node if you set the nodeid" if !$_[0]->routing_table;
        $_[0]->routing_table->assign_node($_[0]);
        $_[0]->routing_table->del_node($_[0]) if !$_[0]->has_bucket;
    };
    has 'outstanding_requests' => (isa     => 'HashRef[HashRef]',
                                   is      => 'ro',
                                   traits  => ['Hash'],
                                   handles => {add_request    => 'set',
                                               get_request    => 'get',
                                               del_request    => 'delete',
                                               expire_request => 'delete',
                                               is_expecting   => 'defined'
                                   },
                                   init_arg => undef,
                                   default  => sub { {} }
    );
    after 'expire_request' => sub { shift->inc_fail };
    around 'add_request' => sub {
        my ($code, $self, $tid, $args) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        $args->{'timeout'} //= AE::timer(
            20, 0,
            sub {
                $self->expire_request($tid) if $self;    # May ((poof)) $self
            }
        );
        $code->($self, $tid, $args);
    };
    has 'ping_timer' => (
         isa      => 'Ref',                               # ArrayRef|EV::Timer
         builder  => '_build_ping_timer',
         is       => 'ro',
         init_arg => undef,
         writer   => '_ping_timer'
    );

    sub _build_ping_timer {
        my ($self) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        AE::timer(60 * 10, 60 * 10, sub { $self->ping if $self });
    }
    has 'seen' => (
        isa        => 'Int',
        is         => 'ro',
        init_arg   => undef,
        writer     => '_set_seen',
        handles    => {
            touch  => sub { shift->_set_seen(time) },
            active => sub {
                my $seen = shift->seen;
                return $seen && (time - $seen <= GOOD_UNTIL);
                }
        }
    );
    for my $type (qw[get_peers find_node announce_peer]) {
        has 'prev_'
            . $type => (isa        => 'HashRef[Int]',
                        is         => 'ro',
                        lazy_build => 1,
                        builder    => '_build_prev_X',
                        init_arg   => undef,
                        traits     => ['Hash'],
                        handles    => {
                                    'get_prev_' . $type     => 'get',
                                    'set_prev_' . $type     => 'set',
                                    'defined_prev_' . $type => 'defined'
                        }
            );
    }
    sub _build_prev_X { {} }
    after 'BUILD' => sub {
        my ($self) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        $self->_ping_timer(AE::timer(rand(30), 0, sub { $self && $self->ping }));
    };
    has 'birth' => (is         => 'ro',
                    isa        => 'Int',
                    init_arg   => undef,
                    lazy_build => 1
    );
    sub _build_birth {time}

    sub ping {
        my ($self) = @_;
        state $tid = 'a';
        my $packet = build_dht_query_ping('p_' . $tid,
                                      pack('H*', $self->dht->nodeid->to_Hex));
        my $sent = $self->send($packet);
        return $self->inc_fail() if !$sent;
        $self->add_request('p_' . $tid, {type => 'ping'});
        $tid++;
    }

    sub _reply_ping {
        my ($self, $tid) = @_;
        my $packet = build_dht_reply_ping($tid,
                                      pack('H*', $self->dht->nodeid->to_Hex));
        my $sent = $self->send($packet, 1);
        $self->inc_fail() if !$sent;
        return $sent;
    }

    sub find_node {
        my ($self, $target) = @_;
        return
            if $self->defined_prev_find_node($target->to_Hex)
                && $self->get_prev_find_node($target->to_Hex)
                > time - (GOOD_UNTIL - 60); # to avoid a race condition
        state $tid = 'a';
        my $packet =
            build_dht_query_find_node('fn_' . $tid,
                                      pack('H*', $self->dht->nodeid->to_Hex),
                                      pack('H*', $target->to_Hex)
            );
        my $sent = $self->send($packet);
        return $self->inc_fail() if !$sent;
        $self->add_request('fn_' . $tid,
                           {type => 'find_node', target => $target});
        $tid++;
        $self->set_prev_find_node($target->to_Hex, time);
    }

    sub _reply_find_node {
        my ($self, $tid, $target) = @_;
        require Net::BitTorrent::Protocol::BEP23::Compact;
        my $nodes
            = Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                 map { [$_->host, $_->port] }
                     @{$self->routing_table->nearest_bucket($target)->nodes});
        return if !$nodes;
        my $packet =
            build_dht_reply_find_node($tid, pack('H*', $target->to_Hex),
                                      $nodes);
        my $sent = $self->send($packet, 1);
        $self->inc_fail() if !$sent;
        return $sent;
    }

    sub get_peers {
        my ($self, $info_hash) = @_;
        # don't do it again within 15 seconds
        # warn "get_peers to ",$info_hash->to_Hex," last was ",$self->defined_prev_get_peers($info_hash->to_Hex) && ((time - $self->get_prev_get_peers($info_hash->to_Hex)) / 60)," minutes ago";
        return
            if $self->defined_prev_get_peers($info_hash->to_Hex)
                && $self->get_prev_get_peers($info_hash->to_Hex)
                > time - (GET_PEERS_RETRY - 1);
        state $tid = 'a';
        my $packet =
            build_dht_query_get_peers('gp_' . $tid,
                                      pack('H*', $self->dht->nodeid->to_Hex),
                                      pack('H*', $info_hash->to_Hex)
            );
        my $sent = $self->send($packet);
        return $self->inc_fail() if !$sent;
        $self->add_request('gp_' . $tid,
                           {type => 'get_peers', info_hash => $info_hash});
        $tid++;
        $self->set_prev_get_peers($info_hash->to_Hex, time);
    }

    sub _reply_get_peers {
        my ($self, $tid, $id) = @_;
        if (!$self->has_announce_peer_token_out($id->to_Hex)) {
            state $announce_peer_token = 'aa';
            $announce_peer_token = 'aa' if length $announce_peer_token == 3;
            $self->_set_announce_peer_token_out($id->to_Hex,
                                                $announce_peer_token++);
        }
        require Net::BitTorrent::Protocol::BEP23::Compact;
        my $nodes
            = Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                     map { [$_->host, $_->port] }
                         @{$self->routing_table->nearest_bucket($id)->nodes});
        my @values = grep { defined $_ } map {
            Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                                                           [$_->[0], $_->[1]])
        } @{$self->tracker->get_peers($id) || []};
        return if (!@values && !$nodes);
        my $packet =
            build_dht_reply_get_peers($tid,
                                      $id->to_Hex,
                                      \@values,
                                      $nodes,
                                      $self->_get_announce_peer_token_out(
                                                                   $id->to_Hex
                                      )
            );
        my $sent = $self->send($packet, 1);
        $self->inc_fail() if !$sent;
        return $sent;
    }

    sub announce_peer {
        my ($self, $info_hash, $port) = @_;
        return
            if $self->defined_prev_announce_peer($info_hash->to_Hex)
                && $self->get_prev_announce_peer($info_hash->to_Hex)
                > time - (60 * 30);
        return if !$self->has_announce_peer_token_in($info_hash->to_Hex);
        state $tid = 'a';
        my $packet =
            build_dht_query_announce_peer(
                       'an_' . $tid,
                       pack('H*', $self->dht->nodeid->to_Hex),
                       pack('H*', $info_hash->to_Hex),
                       $self->_get_announce_peer_token_in($info_hash->to_Hex),
                       $port
            );
        my $sent = $self->send($packet);
        return $self->inc_fail() if !$sent;
        $self->add_request('an_' . $tid,
                          {type => 'announce_peer', info_hash => $info_hash});
        $tid++;
        $self->set_prev_announce_peer($info_hash->to_Hex, time);
    }

    sub _reply_announce_peer {
        my ($self, $tid, $info_hash, $a_ref) = @_;
        my $packet;
        if ((!$self->has_announce_peer_token_out($info_hash->to_Hex))
            || ($self->_get_announce_peer_token_out($info_hash->to_Hex) ne
                $a_ref->{'token'})
            )
        {   $packet =
                build_dht_reply_error($tid,
                                      [203,
                                       'Incorrect write token in announce_peer'
                                      ]
                );
        }
        elsif (!$self->tracker->add_peer(
                                   $info_hash, [$self->host, $a_ref->{'port'}]
               )
            )
        {   $packet = build_dht_reply_error($tid,
                                      [202, 'Failed to add peer to tracker']);
        }
        else {
            $packet = build_dht_reply_announce_peer($tid,
                                      pack('H*', $self->dht->nodeid->to_Hex));
        }
        my $sent = $self->send($packet, 1);
        $self->inc_fail() if !$sent;
        return $sent;
    }
    has 'fail' => (
        isa      => 'Int',
        traits   => ['Counter'],
        default  => 0,
        is       => 'ro',
        handles  => {inc_fail => 'inc'},
        init_arg => undef,
        trigger  => sub {
            my ($self, $new, $old) = @_;
            $self->routing_table->del_node($self)
                if $new == ($self->has_bucket ? 5 : 1);
        }
    );
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP05::Node - A single node in a DHT routing table

=head1 Description

TODO

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
