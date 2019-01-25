package presetdeck;
use strictures 2;
use parent 'Parse::Binary';
use constant;
use parsehelper qw' string header ';

constant->import(
    FORMAT => header(),
    map {
        my $deck = $_;
        (
            "deck_${_}_id"         => "L",
            "deck_${_}_name"       => string,
            "deck_${_}_ship_count" => "L",
            map +( "deck_${deck}_ship_${_}_name" => string ), 1 .. 6,
          )
    } 1 .. 19
);

1;

