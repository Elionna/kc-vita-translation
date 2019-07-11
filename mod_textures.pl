use 5.020;
use strictures 2;
use IO::All -binary;
use Capture::Tiny 'capture';

$|++;
run();

sub _age($) {
    eval { ( stat $_[0] )[9] } || 0;
}

sub run {
    say "copying and converting images";
    my $target_path = "../kc_original_unpack_modded/repatch/PCSG00684/Media/Unity_Assets_Files";
    my @images = grep /\.png$/, io("en/Unity_Assets_Files")->All_Files;
    my ( $processed, %known ) = (0);
    for my $image (@images) {
        my @file_parts = split /\/|\\/, $image;
        my $source_tex = "../kc_original_unpack/repatch/PCSG00684/Media/Unity_Assets_Files/$file_parts[2]/$file_parts[4]";
        my $target_tex = "$target_path/$file_parts[2]/$file_parts[4]";
        $_ =~ s/\.png$// for $source_tex, $target_tex;
        die "source texture missing: '$source_tex'" if not -e $source_tex;
        $known{$target_tex}++;
        next if -e $target_tex and _age $target_tex > _age $image->name;

        $processed++;
        print "  " . $image->filename;
        io( io->file($target_tex)->filepath )->mkpath;
        io->file($source_tex)->copy($target_tex);
        my $cmd = qq[UnityTexTool/UnityTexTool-x64.exe -c -i "$image" -o "$target_tex"];
        my ( $out, $err, $res ) = capture { system $cmd };
        die "texture conversion error:\n$cmd\n$out\n$err\n$res" if $err or $res;
        die "weird texture conversion result:\n$cmd\n$out\n$err\n$res" if $out !~ /File Created\.\.\./;
    }
    say sprintf "\n  skipped %s texes that were newer than their translated source png", @images - $processed;
    my @found = grep /\.tex$/, io($target_path)->All_Files;
    s/\\/\//g for @found;
    my @unknown = grep !$known{$_}, @found;
    if (@unknown) {
        say join "\n  ", "  deleting unknown texture files:", @unknown;
        io($_)->unlink for @unknown;
    }
    say "done";
    return;
}
