use 5.010;
use strictures 2;
use IO::All -binary;
use Capture::Tiny 'capture';
use Time::HiRes 'time';

run();

# this whole thing works a bit oddly. since asset files are interlinked, we need
# the resources.resource file despite never unpacking it, and we also can only
# delete things after unpacking.

sub run {
    my $target = "../kc_original_unpack/repatch/PCSG00684/Media";
    io($target)->mkpath;
    chdir "../kc_original_unpack/repatch/PCSG00684/Media";
    my @files = io("../../../../kc_original/repatch/PCSG00684/Media/")->all_files;
    $_->copy( "./" . $_->filename ) for @files;
    my $unity_ex = io("../../../../unity_tools/UnityEX.exe")->absolute->pathname;
    for my $file ( grep !/resources\.resource/, io(".")->all_files ) {
        say "unpacking file $file'";
        my ( $out, $err, $res ) = capture { system qq["$unity_ex" export $file] };
        warn "\n$out" if $out;
        warn "\n$err" if $err;
    }
    io($_)->unlink for @files;
    my $has_find = -e "c:/cygwin/bin/find.exe";
    say "has find: $has_find";
    my @list = $has_find ? split /\n/, `c:/cygwin/bin/find "." -type f`    #
      :                    io(".")->All_Files;
    @list = grep /\.(gobj|4|dds|fsb|snd|[\d]+|script|txt|shader|ani|obj|cbm|mesh|xml)$/, @list;
    say "deleting";

    my $ctd = countdown->new( total => scalar @list );
    $|++;
    while (@list) {
        my @sublist = splice @list, 0, ( @list >= 100 ) ? 100 : @list;
        unlink $_ for @sublist;
        $ctd->update( scalar @sublist );
    }
    return;
}
