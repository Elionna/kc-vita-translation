use 5.020;
use strictures 2;
use IO::All -binary;
use Capture::Tiny 'capture';

$|++;
run();

sub run {
    say "copying translated unmodded files";
    for my $set (    #
        [ "png", "en/sce_sys",                   "sce_sys/" ],
        [ "png", "en/sce_sys/livearea/contents", "sce_sys/livearea/contents/" ],
      )
    {
        my ( $ext, $src, $tgt ) = $set->@*;
        if(!-d $src) {
            warn "$src doesn't exist\n";
            next;
        }
        my @files = grep /\.$ext$/, io($src)->all_files;
        for my $file (@files) {
            die "didn't find file '$file' in original game" if !-e "../kc_original/repatch/PCSG00684/$tgt" . $file->filename;
            my $target_file = "../kc_original_unpack_modded/repatch/PCSG00684/$tgt" . $file->filename;
            io( io->file($target_file)->filepath )->mkpath;
            $file->copy($target_file);
        }
    }
    say "done";
}
