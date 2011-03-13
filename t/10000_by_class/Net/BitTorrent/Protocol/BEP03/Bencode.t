# shamelessly copied from Aristotle's bencode
# https://github.com/ap/Bencode
# http://search.cpan.org/~aristotle/Bencode/

use 5.10.0;
use strict; use warnings; no warnings 'uninitialized';


use Test::More;
use lib '../../../../lib', 'lib';

use Net::BitTorrent::Protocol::BEP03::Bencode qw( bencode bdecode );

my @test = (
'i4e' => 4,
'i0e' => 0,
'i-10e' => -10,
'i12345678901234567890e' => '12345678901234567890',
'0:' => '',
'3:abc' => 'abc',
# '10:1234567890' => \'1234567890', # Aristotle's has special case for string-ref
'i1234567890e' => '1234567890', # perl can't tell string from number
'le' => [],
'li1ei2ei3ee' => [ 1, 2, 3 ],
'll5:Alice3:Bobeli2ei3eee' => [ [ 'Alice', 'Bob' ], [ 2, 3 ] ],
'de' => {},
'd3:agei25e4:eyes4:bluee' => { 'age' => 25, 'eyes' => 'blue' },
'd8:spam.mp3d6:author5:Alice6:lengthi100000eee' => { 'spam.mp3' => { 'author' => 'Alice', 'length' => 100000 } },
'd1:Xli1ee1:Yi0ee' => { X=>[ 1 ], Y => 0 },
'ld1:Xi1e1:Yi0eei2ee' => [ { X => 1, Y => 0}, 2 ],
"3:a
b" => "a\nb",
# This is safe, because the decode actually treats everything as a string
# and we don't do anything to trigger number-behavior in our test
'i555555555555555555555555555559999e' => '555555555555555555555555555559999',
# Try screwing with the delimiters
'3:1:A' => '1:A',
'16:d1:Xli1ee1:Yi0ee' => 'd1:Xli1ee1:Yi0ee',
);

# These don't decode to the same value
my @encode_only = (
'0:' => undef, # maybe not strictly kosher
);

# These don't encode back to the bencode
my @decode_only = (
# Test with a large "number" that perl would choke on
'33:555555555555555555555555555559999' => '555555555555555555555555555559999',
'i9ex' => '9', # we ignore trailing stuff, wrong, should fail
'd1:Xi1e' => { X => 1 }, # we just stop if stuff is missing, this is wrong actually
'i9ei10e' => '9',
'0000:' => '',
'i9e' => '9',
);

my @wont_encode = (
# unlikely, but we assert that these aren't handled
sub {1},
bless( {},"Bob"),
\*STDERR,
\'stringref',
);

my @wont_decode = (
'99:ab',
'a',
'1',
'ie',
);

use constant 'FUZZ_DECODES', 40;
use constant 'FUZZ_ENCODES', 40;

plan tests => @test * 1.5 + @encode_only/2 + @decode_only/2 + @wont_encode + @wont_decode + FUZZ_DECODES + FUZZ_ENCODES;

while ( my ( $frozen, $thawed ) = splice @test, 0, 2) {
    my $to = bencode ($thawed);
    is( $to, $frozen, "encode $thawed to $frozen" );
    is_deeply bdecode($to), $thawed, "round trip through $to" ;
    is bencode(bdecode($to)), $to, "round trip through & back $to " ;
    }
while ( my ( $frozen, $thawed ) = splice @encode_only, 0, 2) {
    is bencode($thawed), $frozen, "encode $thawed to $frozen";
    }
while ( my ( $frozen, $thawed ) = splice @decode_only, 0, 2) {
    is_deeply bdecode($frozen), $thawed, "decode from $frozen to $thawed";
    }
foreach  my $thawed (@wont_encode) {
    # undef might be better to signal "not encoded"
    is bencode($thawed), '', "can't encode $thawed";
    }
foreach  my $frozen (@wont_decode) {
    is bdecode($frozen), undef, "can't decode $frozen ".bdecode($frozen);
    }

# fuzz test
# I wanted to test ^D and null, but that signals EOF to the test-harness if I print it.
my @fuzz_chars = (' ', qw(: i e d l 0 1 2 A B));
for (1..FUZZ_DECODES) {
    my $bad = join('',map { $fuzz_chars[rand $#fuzz_chars]} (1..(rand 20)+1));
    my $got = eval { bdecode($bad) };
    is $@, '', "Junk shouldn't crash decode: $bad";
    }

use Data::Dumper;

for (1..FUZZ_ENCODES) {
    my $more;
    $more = rand_struct($more) for (1..10);
    is_deeply( bdecode(bencode($more)), $more, "Fuzz structure")
        || warn "Failed Structure was ".Dumper($more)." ";
    }

our $Rand_Depth = 0;
sub rand_struct {
    my ($sofar) = @_;
    local $Rand_Depth;
    return undef if $Rand_Depth++ > 9;

    given (int(rand(9))) {
        when (0) {
            $sofar = $sofar ? { A => $sofar } : {};
            }
        when (1) {
            $sofar = $sofar ? [ $sofar ] : [];
            }
        when ([(2..8)]) {
            if (!defined $sofar) {
                $sofar = int(rand(2)) ? 'A' : '1';
                }

            if ($sofar =~ /^\d+$/) {
                $sofar .= int(rand(10)) for (1..rand(5)+1);
                }
            elsif (ref($sofar) eq 'ARRAY') {
                push @$sofar, rand_struct();
                }
            elsif (ref($sofar) eq 'HASH') {
                $sofar->{chr(ord('A') + rand(26))} = rand_struct();
                }
            else {
                $sofar .= chr(ord('A') + rand(26)) for (1..rand(5)+1);
                }
            }
        }
    $sofar;
    }
