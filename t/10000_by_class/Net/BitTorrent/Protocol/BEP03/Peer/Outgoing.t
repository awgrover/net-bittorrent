package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
{
    use strict;
    use warnings;
    use AnyEvent;
    use AnyEvent::Socket qw[tcp_server];
    use AnyEvent::Handle;
    use Test::Most;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent;
    use Net::BitTorrent::Torrent;
    $|++;

    sub new_args {
        my $s = shift;
        -f $s->torrent ? last : chdir '..' for 0 .. 15;
        $s->{'client'} = Net::BitTorrent->new();
        $s->{'torrent'} = Net::BitTorrent::Torrent->new(path => $s->torrent);
        $s->{'client'}->add_torrent($s->{'torrent'});
        (connect => [$s->{'host'}, $s->{'port'}],
         client  => $s->{'client'},
         torrent => $s->{'torrent'}
        );
    }

    # Handshake data
    sub reserved  { "\0" x 8 }
    sub torrent   {'t/90000_data/95000_torrents/95003_miniswarm.torrent'}
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}
    sub peer_id   {'This ain\'t a peer_id'}

    # Callbacks
    sub on_peer_disconnect {
        my ($s, $a) = @_;
        use Data::Dump;
        ddx $a;
        is $a->{'peer'}->handshake, 0, 'disconnect mid-handshake';

        # Regression test
        my $match
            = '127\.0\.0\.1:\d+ \('
            . substr(peer_id(), 0, 20)
            . '\) disconnect: Bad info_hash \(We are not serving '
            . sprintf(info_hash, 0, 40) . '\)';
        like $a->{'message'}, qr[^$match],
            'peer disconnected (unknown torrent)';
    }

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Outgoing'}

    # Events
    sub _100_handshake_ : Test( 3 ) {
        my $s = shift;
        use Data::Dump;
        return 'TODO';
        syswrite $s->{'fh'},
            build_handshake($s->reserved, $s->info_hash, $s->peer_id);
        AnyEvent->one_event for 1 .. 100;
        ddx $s->{'peer'};
        sysread $s->{'fh'}, my ($text), 1024;
        warn $text;
    }

    # AnyEvent
    sub _00000_init : Test( startup ) {
        my $s = shift;
        note 'Adding condvar for later use...';
        $s->{'cv'} = AE::cv();
        $s->{'cv'}->begin(sub { $s->{'cv'}->send });
        note '...which will timeout in 30s';
        $s->{'to'}
            = AE::timer(30, 0, sub { diag 'Timeout!'; $s->{'cv'}->send });
    }

    sub wait : Test( shutdown => no_plan ) {
        my $s = shift;
        $s->{'cv'}->end;
        $s->{'cv'}->recv;
    }

    # Setup/teardown
    sub startup : Test( startup => 3 ) {
        my $s = shift;
        use_ok $s->class;
        can_ok $s->class, 'new';
        $s->{'peer'} = new_ok $s->class, [$s->new_args];
        explain 'New peer looks like... ', $s->{'peer'};
        $s->{'expect'} = [qw[handshake bitfield]];
    }

    sub setup : Test( setup ) {
        my $s = shift;
    }

    sub shutdown : Test( shutdown ) {
    }

    sub __dispatch {
        {handshake => sub {
             plan tests => 10;
             my $s = shift;
             ok $s->{'handle'}->rbuf, 'read handshake packet';
             ok length $s->{'handle'}->rbuf >= 68,
                 'handshake was >= 68 bytes';
             my $p = parse_packet(\$s->{'handle'}->rbuf);
             is ref $p, 'HASH', 'packet parses to hashref';
             is $p->{'type'},           -1, 'fake handshake type';
             is $p->{'packet_length'},  68, 'parsed packet was 68 bytes';
             is $p->{'payload_length'}, 48, 'parsed payload was 48 bytes';
             is scalar @{$p->{'payload'}}, 3, 'parsed payload has 3 elements';
             is length $p->{'payload'}[0], 8, 'reserved is eight bytes';
             like $p->{'payload'}[1], qr[^[A-F\d]{40}$]i,        'info_hash';
             like $p->{'payload'}[2], qr[^NB\d\d\d[SU]-.{13}+$], 'peer_id';

             # Next step
             $s->{'handle'}->push_write(
                   build_handshake($s->reserved, $s->info_hash, $s->peer_id));
             $s->{'handle'}->push_read(
                 sub {
                     AnyEvent->one_event;
                     subtest 'post handshake', sub {
                         plan tests => 4;
                         ok $s->{'peer'}->_has_torrent,
                             '...->torrent is defined';
                         is $s->{'peer'}->torrent->info_hash->to_Hex,
                             $s->info_hash,
                             '...->torrent->info_hash->to_Hex is correct';
                         is $s->{'peer'}->peer_id, $s->peer_id,
                             '...->peer_id is correct';
                         is $s->{'peer'}->pieces->to_Enum, '',
                             'initial value for ...->pieces->to_Enum is correct';
                     };
                     1;
                 }
             );
         },
         bitfield => sub {
             plan tests => 2;
             my $s = shift;
             is length $s->{'handle'}->rbuf, 6, 'read 6 bytes from peer';
             is_deeply parse_packet(\$s->{'handle'}->rbuf),
                 {packet_length  => 6,
                  payload        => "\0",
                  payload_length => 1,
                  type           => 5
                 },
                 'bitfield is correct';

             # Next step
             $s->{'handle'}->push_write(build_bitfield(pack 'B*', '10'));
             $s->{'handle'}->push_read(
                 sub {
                     AnyEvent->one_event for 1 .. 10;
                     subtest 'post bitfield', sub {
                         plan tests => 2;
                         is $s->{'peer'}->pieces->to_Enum, '0',
                             'new value for ...->pieces->to_Enum is correct';
                         ok $s->{'peer'}->interesting,
                             'peer is now interested in us';
                     };
                     1;
                 }
             );
             }
        };
    }

    sub _9000_open_socket : Test( startup => 0 ) {
        my $s = shift;
        $s->{'cv'}->begin;
        $s->{'socket'} = tcp_server '127.0.0.1', 0, sub {
            my ($fh, $host, $port) = @_;
            $s->{'fh'} = $fh;
            $s->{'handle'} = AnyEvent::Handle->new(
                fh      => $fh,
                on_read => sub {
                    @{$s->{'expect'}}
                        ? subtest $s->{'expect'}->[0], sub {
                        $s->{'cv'}->begin;
                        $s->__dispatch->{shift @{$s->{'expect'}}}->($s);
                        $s->{'cv'}->end;
                        }
                        : explain 'No idea what to do with this packet: ',
                        $s->{'handle'}->rbuf;

                    #subtest 'read handshake', sub { $s->_read_handshake; 1 }
                },
                on_write => sub {...}
            );

            #
            #subtest 'write handshake' => sub { $s->_write_handshake };
            #subtest 'read bitfield'   => sub { $s->_read_bitfield };
            #subtest 'write bitfield'  => sub { $s->_write_bitfield };
            #subtest 'read interested' => sub { $s->_read_interested };
            #subtest 'write unchoke'   => sub { $s->_write_unchoke };
            #$s->{'cv'}->end;
            }, sub {
            my ($state, $host, $port) = @_;
            $s->{'host'} = $host;
            $s->{'port'} = $port;
            1;
            }
    }

    sub _read_interested {
        plan tests => 2;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is sysread($s->{'fh'}, my ($x), 1024), 5, 'read 6 bytes from peer';
        is_deeply parse_packet(\$x),
            {packet_length  => 5,
             payload_length => 0,
             type           => 2
            },
            'interested is correct';
        $s->{'cv'}->end;
    }

    sub _write_unchoke {
        plan tests => 2;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is syswrite($s->{'fh'}, build_unchoke), 5,
            'wrote 5 byte unchoke to peer';
        AnyEvent->one_event for 1 .. 10;
        ok !$s->{'peer'}->remote_choked, 'peer is now unchoked by us';
        $s->{'cv'}->end;
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller
}
1;
