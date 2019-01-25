package motionship;
use strictures 2;
use parent 'Parse::Binary';
use constant;
use parsehelper 'string';

constant->import(
    FORMAT => (
        "id"   => "L",
        "name" => "L/a x!4",
        "pad"  => "a16",
    )
);

1;

