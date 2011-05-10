package Net::BitTorrent::Protocol::BEP05::Packets::Render;
# cf http://www.bittorrent.org/beps/bep_0005.html
use feature 'switch';
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(render_packet);

use Net::BitTorrent::Protocol::BEP03::Bencode qw(bdecode);
use Net::BitTorrent::Protocol::BEP23::Compact qw(uncompact_ipv4);

use Data::Dumper;

sub render_packet {
        my ($packet) = @_;
        # returns a string or a struct
        # The struct should have a 'type' key
        my %Type = ( q => 'query', r => 'response', e => 'error' );

        my $decoded_string = "";

        my $packet_data = bdecode($packet);
        if (!defined $packet_data->{'t'}) {
            $decoded{'type'} = "bad?";
            $decoded_string = "bad: ".unpack('H*', $packet_data)." c: ".join("",unpack('C*', $packet_data))." got: ".Dumper($packet_data);
            $decoded_string = "bad: ".unpack('H*', $packet)." c: ".join("",unpack('C*', $packet))." got: ".Dumper($packet_data);
            }
        else {
            $decoded{'transaction_id'} = unpack('H*', $packet_data->{'t'});
            $decoded_string .= $decoded{'transaction_id'}."[".$packet_data->{'t'}."]";

            given ($Type{$packet_data->{'y'}}) {
                when ('error') {
                  $decoded_string .= 'error(';
                  $decoded_string .= $packet_data->{'e'}->[0].":".$packet_data->{'e'}->[1];

                  $decoded{'type'} = 'error';
                  $decoded{'error'} = { $packet_data->{'e'}->[0] => $packet_data->{'e'}->[1] };

                  $decoded_string .= ')';
                  }
                when ('query') {
                    $decoded_string .= $decoded{'transaction_id'}.": ";
                    my $decoded_a = $packet_data->{'a'};
                    $decoded{'ask'} = unpack('H40', $decoded_a->{'id'});
                    given ( $packet_data->{'q'} ) {
                        # $decoded_string .= $decoded{'ask'}."->" if $decoded{'ask'} ne unpack('H40', $self->nodeid);
                        $decoded_string .= "$_(";
                        $decoded{'type'} = $_;

                        when ('ping') {
                            $decoded{'node_id'} = unpack('H40', $packet_data->{'a'}->{'id'});
                            $decoded_string .= $decoded{'node_id'};
                            }
                        when ('find_node') {
                            $decoded{'them'} = unpack('H40', $decoded_a->{'target'});
                            $decoded_string .= $decoded{'them'};
                            }
                        when ('get_peers') {
                            $decoded{'for'} = unpack('H40',$decoded_a->{'info_hash'});
                            $decoded_string .= "for: ".unpack('H40',$decoded_a->{'info_hash'});
                            }
                        when ('announce_peer') {
                          $decoded{'node_id'} = unpack('H40', $decoded_a->{'id'});
                          $decoded{'sha1'} = unpack('H40', $decoded_a->{'info_hash'});
                          $decoded_string .= "tell: ".$decoded{'node_id'}." has: ".$decoded{'sha1'};
                          }
                        default {
                            $decoded{'args'} = length($packet_data->{'a'}).'bytes';
                            $decoded_string .= $decoded{'args'};
                            }
                        }
                    $decoded_string .= ")";
                    }
                when ('response') {
                    my $outstanding = undef; #$outstanding_p{refaddr $self}{ $packet_data->{'t'} };
                    
                    my $decoded_r = $packet_data->{'r'};

                    # alive => $id
                    # closer => @nodes, said => $id (both get_peers and find_node)
                    # node => $node, said => $id
                    # peers => @values, said $id ( ip/port...)

                    $decoded{'said'} = unpack('H40',$decoded_r->{'id'});
                    $decoded_string .= $decoded{'said'}."->response(";

                    my $is_ping = 1;

                    # find_node, or get_peers:ask_others
                    if (my $nodes = $decoded_r->{'nodes'}) {
                        my @nodes = uncompact_ipv4($nodes);
                        warn "decoded 'nodes' to ",Dumper(\@nodes);
                        if (scalar(@nodes) > 1) {
                            $decoded{'type'} = 'found_nodes_closer';
                            $decoded{'closer'} = @nodes;
                            $decoded_string .= "closer: ".join(", ", map {$_->[0].":".$_->[1]} @nodes); use Data::Dumper;
                            }
                        else {
                            $decoded{'type'} = 'found_node';
                            $decoded{'node'} = $nodes[0];
                            $decoded_string .= "node: ".join(":",@{$nodes[0]});
                            }
                        $is_ping = 0;
                        }
                    # get_peers
                    if (my $peers = $decoded_r->{'values'}) {
                        $decoded{'type'} = 'got_peers';
                        $decoded{'peers'} = [map {uncompact_ipv4($_)} @$peers];
                        $decoded_string .= ", peers: ".join(",",map {join ":", uncompact_ipv4($_)} @$peers);
                        $is_ping = 0;
                        }
                    # ping
                    if ($is_ping) {
                        $decoded{'type'} = 'pong';
                        $decoded{'alive'} = delete $decoded{'said'};
                        $decoded_string .= "alive"
                        }
                        
                    $decoded_string .= ")";
                    }
                default {
                    $decoded{'type'} = $_;
                    $decoded_string .= 'something else';
                    }
                }
            }
        wantarray
            ? %decoded
            : $decoded_string
            ;
        };
1;
