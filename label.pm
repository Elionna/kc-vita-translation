package label;
use strictures 2;
use parent 'Parse::Binary';
use constant FORMAT => (
    magic  => 'a136',
    width  => 'L',
    height => 'L',
    eh     => 'a48',
    length => 'L',
    text   => 'a{$length}',
    pad    => 'a{($length % 4) ? 4 - ($length % 4) : 0}',
    tail   => 'a*',
);
1;
