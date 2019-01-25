package label;
use strictures 2;
use parent 'Parse::Binary';
use parsehelper qw' string ';
use constant FORMAT => (
    magic  => 'a136',
    width  => 'L',
    height => 'L',
    eh     => 'a48',
    text   => string(),
    tail   => 'a*',
);
1;
