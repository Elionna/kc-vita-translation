package presetship;
use strictures 2;
use parent 'Parse::Binary';
use constant;
use parsehelper qw' string header ';

constant->import(
    FORMAT => header(),
    map +( "ship_${_}_id" => "L", "ship_${_}_name" => string, "ship_${_}_pad" => "a24" ), 1 .. 39
);

1;

