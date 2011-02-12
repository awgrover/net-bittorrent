package t::10000_by_class::Net::BitTorrent::DHT;
{
    use strict;
    use warnings;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use Test::More;
    use parent 'Test::Class';
    use lib '../../../../lib', 'lib';
    use 5.010.000;
    use Test::Moose;
    use AnyEvent::Impl::Perl;   # Timing is different than with EV. Go figure.
    use AnyEvent;

    our %RestoreFromMock;

    #
    sub class {'Net::BitTorrent::DHT'}

    sub new_args {
        my $t = shift;
        [                       #port              => [1337 .. 1339, 0],
        ];
    }

    #
    sub startup : Tests(startup => no_plan) {
        my $self = shift;

        use Moose::Object;
        %Moose::Object::RestoreFromMock = ();
        Moose::Object->meta->add_method( mock => 
            sub {
                my $class =shift;
                my ($name, $sub) = @_;

                my $meta = $class->meta;
                $Moose::Object::RestoreFromMock{$class}->{$name} = $meta->get_method($name);
                $meta->add_around_method_modifier($name, $sub);
                }
            );

        use_ok $self->class;
        can_ok $self->class, 'new';
        explain $self->new_args;
        $self->{'dht'}
            = new_ok($self->class, $self->new_args, 'decoy NB client');
    }

    sub setup : Test(setup) {
        my $self = shift;
    }

    sub teardown : Test(teardown) {
        while (my ($package, $replaced) = each %Moose::Object::RestoreFromMock) {
            while (my ($sub, $was) = each %$replaced) {
                # print "Revert ${package}::$sub to something $was\n";
                my $meta = $package->meta;
                $meta->remove_method($sub);
                $meta->add_method($sub,$was);
            }
        }
    }

    sub nodeid : Test(no_plan) {
        my $pig = shift->{dht};
        ok($pig->nodeid, 'nodeid is defined');
        isa_ok $pig->nodeid, 'Bit::Vector';
    }

    sub talks_to_boot_nodes : Test(no_plan) {
        my $tester=shift;

        my $saw_find_node = undef;

        use Net::BitTorrent::Protocol::BEP05::Node;
        Net::BitTorrent::Protocol::BEP05::Node->mock( 'find_node', sub{ 
            my $super=shift;
            my $self = shift;
            my ($target) = @_;
            isa_ok $self, 'Net::BitTorrent::Protocol::BEP05::Node';
            (isa_ok $target, 'Bit::Vector') && do {
                if ($self->dht->nodeid ne $target) {
                    warn "Saw target node ".ref($target)." $target vs ",ref($self->dht->nodeid);
                    return;
                    }
                };
            is $self->dht->nodeid, $target, 'advertised dht';
            $saw_find_node=1;
            } );

        my $dht = $tester->class->new( boot_nodes => [ ['10.5.98.12',1234], ['10.23.8.55',7899] ] );
        is $saw_find_node, 1, 'saw advertisement'

        #substitute a test for an existing method
        # all other calls should fail
        # calls to certain methods let us do tests
        # certain methods must be called
        }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
