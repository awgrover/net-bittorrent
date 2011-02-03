package t::10000_by_class::Net::BitTorrent::Protocol::BEP05::Bucket;

use strict;
use warnings;

use Test::More;
use parent 'Test::Class';
use 5.010.000;

use Net::BitTorrent::Protocol::BEP05::RoutingTable;
use Net::BitTorrent::DHT;
use Net::BitTorrent::Protocol::BEP05::Node;

my ($Class) = __PACKAGE__ =~ /^t::10000_by_class::(.+)/;

my $RoutingTable;
my $Bucket;

sub setup : Test(setup) {
  $RoutingTable = Net::BitTorrent::Protocol::BEP05::RoutingTable->new(dht => Net::BitTorrent::DHT->new);
  $Bucket = Net::BitTorrent::Protocol::BEP05::Bucket->new(routing_table => $RoutingTable);
  }

sub new_node {
  my ($host,$port,$nodeid) = @_;
  $nodeid = '1234567890abcdef' if !defined $nodeid;
  $nodeid = Bit::Vector->new_Hex(160, $nodeid) if !ref $nodeid;
  Net::BitTorrent::Protocol::BEP05::Node->new( host => $host, port => $port, nodeid => $nodeid);
  }

sub startup : Tests(startup => no_plan) {
  use_ok $Class;
  }

sub add_once : Tests { 

  # sanity
  is $Bucket->count_nodes, 0;
  is $Bucket->count_backup_nodes, 0;

  my @n1 = ( 'somehost', 1234);
  my $node1 = new_node(@n1);

  # only once for the same object
  $Bucket->add_node($node1);
  $Bucket->add_node($node1);
  is $Bucket->count_nodes, 1;
  is $Bucket->count_backup_nodes, 0;

  # only once for the same nodeid
  my $node2 = new_node(@n1);
  $Bucket->add_node($node2);
  is $Bucket->count_nodes, 1;
  is $Bucket->count_backup_nodes, 0;
  }
  
sub once_overflow : Tests {
  # sanity
  is $Bucket->count_nodes, 0;
  is $Bucket->count_backup_nodes, 0;

  # force nodes to overflow (we don't know what K is)
  my $ct = 0;
  while ($Bucket->count_backup_nodes == 0) {
    $ct ++;
    $Bucket->add_node(new_node('somehost', $ct, $ct));
    }
  is $Bucket->count_nodes, $ct-1;
  is $Bucket->count_backup_nodes, 1;

  my @n1 = ('otherhost', 4567, 4567);
  my $node1 = new_node(@n1);
  $Bucket->add_node($node1);
  is $Bucket->count_backup_nodes, 2;
  $Bucket->add_node($node1);
  is $Bucket->count_backup_nodes, 2;
  $Bucket->add_node(new_node(@n1));
  is $Bucket->count_backup_nodes, 2;
  }

sub once_backup_nodes : Tests {
  my @n1 = ('otherhost', 4567);
  my $node1 = new_node(@n1);
  $Bucket->add_backup_node($node1);
  $Bucket->add_backup_node($node1);
  is $Bucket->count_backup_nodes, 1;
  $Bucket->add_node(new_node(@n1));
  is $Bucket->count_backup_nodes, 1;
  }

sub once_both : Tests {
  # "once" applies to both backup & regular list
  my $node1 = new_node('otherhost', 4567);
  $Bucket->add_node($node1);
  $Bucket->add_backup_node($node1);
  is $Bucket->count_backup_nodes, 0;

  my $node2 = new_node('somehost', 1234, 1234);
  $Bucket->add_backup_node($node2);
  $Bucket->add_node($node2);
  is $Bucket->count_nodes, 1;
  is $Bucket->count_backup_nodes, 1;
  }

__PACKAGE__->runtests() if !caller;
1;
