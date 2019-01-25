package parsehelper;
use 5.020;
use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw( header string );

sub header {
    (
        game_object_file_id => 'L',
        game_object_path_id => 'Q',
        enabled             => 'L',
        script_file_id      => 'L',
        script_path_id      => 'Q',
        object_name         => "L/a x!4",
        sheet_count         => 'L',
        sheet_name          => "L/a x!4",
        obj_count           => 'L',
    );
}

sub string { "L/a x!4" }

sub pad4 { ( $_[0] % 4 ) ? 4 - ( $_[0] % 4 ) : 0 }

1;

