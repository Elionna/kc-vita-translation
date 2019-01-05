use 5.020;
use strictures 2;
use IO::All -binary;
use Capture::Tiny 'capture';

$|++;
run();

sub plain_or_asset {
    my ( $dir, $file ) = @_;
    print "  $file";
    $file = "$dir/$file" if $dir;
    if ( !-f $file ) {
        $file .= ".assets";
        print ".assets";
    }
    die "couldn't find $file" if !-f $file;
    return $file;
}

sub run {
    my $cwd        = io(".")->absolute->pathname;
    my $patch_dir  = "../kc_original_unpack_modded";
    my $src_dir    = "../kc_original";
    my $target_dir = "../kc_translation_mod_candidate";
    my $media_dir  = "$target_dir/Media";
    my $asset_dir  = "$media_dir/Unity_Assets_Files";
    my $unity_ex   = io("../unity_tools/UnityEX.exe")->absolute->pathname;
    my $verbose    = grep /^-v$/, @ARGV;

    say "deleting candidate dir";
    io($target_dir)->rmtree if -e $target_dir;

    say "prepped, copying patches";
    my @patch_files = grep $_ !~ /\.git/, io($patch_dir)->All_Files;
    for my $patch (@patch_files) {
        my @file_parts = split /\/|\\/, $patch;
        $file_parts[1] = "kc_translation_mod_candidate";
        my $target = join "/", @file_parts;
        io( io->file($target)->filepath )->mkpath;
        $patch->copy($target);
    }

    say "copying target files";
    my $src_media_dir = "$src_dir/Media";
    my @targets = map $_->filename, io($asset_dir)->all;
    io( plain_or_asset $src_media_dir, $_ )->copy($media_dir) for @targets;

    say "\napplying patches to target files";
    chdir $media_dir;
    for my $target (@targets) {
        $target = plain_or_asset undef, $target;
        my ( $out, $err, $res ) = capture { system qq["$unity_ex" import $target] };
        warn "\n$out" if !$verbose and $out !~ /^import\n\n.*\n/;
        say "\n$out" if $verbose;
    }
    chdir $cwd;

    say "\ndeleting patches";
    io($asset_dir)->rmtree;

    my $sub_path = "repatch/PCSG00684/";
    say "moving to $sub_path";
    io("$target_dir/$sub_path/")->mkpath;
    io($media_dir)->rename("$target_dir/$sub_path/Media");

    say "done";
    return;
}
