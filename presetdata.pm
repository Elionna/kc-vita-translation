package presetdata;
use strictures 2;
use parent 'Parse::Binary';
use constant;
use parsehelper qw' string header ';

constant->import(
    FORMAT => header(),    #
    map +( "preset_${_}_id" => "L", "preset_${_}_name" => string, "preset_${_}_pad" => "a172" ), 1 .. 20
);
1;
