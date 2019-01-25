package motionlist;
use strictures 2;
use parent 'Parse::Binary';
use constant;
use parsehelper qw' string header ';
use motionship;

constant->import( FORMAT => header(), motionship => [ 'L L/a x!4 a16', '*', 3 ] );

1;

