package label;
use strictures 2;
use parent 'Parse::Binary';
use parsehelper qw' string ';
use constant FORMAT => (
    magic         => 'a136',
    width         => 'L',
    height        => 'L',
    eh            => 'a48',
    text          => string(),
    font_size     => 'L',
    font_style    => 'L',
    eh2           => 'a116',
    shrink_to_fit => 'L',
    eh3           => 'a12',
    multi_line    => 'L',
);
1;
