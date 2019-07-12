package countdown;

use Mu;
use strictures;
use Time::HiRes 'time';

ro "total";
ro _start_time     => default => sub { time };
rw current         => default => 0;
rw _last_time_left => default => 0;
lazy minimum_step => sub { 0.05 };

sub update_and_return { shift->update_count_and_return( undef, shift ) }

sub update_count_and_return { shift->update(shift); return shift; }

sub update {
    my ( $self, $done_count ) = @_;
    $self->current( $self->current + ( $done_count || 1 ) );
    my $elapsed    = time - $self->_start_time;
    my $total_time = $self->total * $elapsed / $self->current;
    my $left       = $total_time - $elapsed;
    return if $self->_last_time_left and abs( $self->_last_time_left - $left ) < $self->minimum_step;
    printf "\r% 20s", sprintf "%.2f s", $left;
    $self->_last_time_left($left);
    return;
}

1;
