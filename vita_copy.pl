use 5.020;
use strictures 2;
use IO::All -binary;
use Digest::SHA1 'sha1_base64';

$|++;
run();

sub run {
    say "copying";
    my $vita_dir = "e:/rePatch/PCSG00684/Media";
    my $cand_dir = "../kc_translation_mod_candidate/rePatch/PCSG00684";

    io($vita_dir)->mkpath;
    my @vita_files = grep !/checksums/, io($vita_dir)->All_Files;
    for my $file (@vita_files) {
        my @file_parts = split /\/|\\/, $file;
        shift @file_parts for 1 .. 3;
        my $cand_file = join "/", $cand_dir, @file_parts;
        next if -e $cand_file;
        say "file '$file' on vita doesn't exist in candidate dir, deleting";
        unlink $file;
    }

    my $sums_file = "$vita_dir/checksums";
    my %known_sums = -e $sums_file ? split /\n|\t/, io($sums_file)->all : ();
    my %sums;

    say "checksumming files";
    my @files = reverse sort { $a->size <=> $b->size } io($cand_dir)->All_Files;
    my @to_copy;
    for my $file (@files) {
        print "$file ";
        my @file_parts = split /\/|\\/, $file;
        shift @file_parts for 1 .. 5;
        my $path = join "/", @file_parts;
        $sums{$path} = sha1_base64 $file->all;
        if ( $known_sums{$path} and $sums{$path} eq $known_sums{$path} ) {
            say " already on vita, skip";
            next;
        }
        say " new!";
        push @to_copy, [ $file, "$vita_dir/$path" ];
    }

    say "copying files";
    for my $file (@to_copy) {
        ( $file, my $path ) = $file->@*;
        say $file;
        io( io->file($path)->filepath )->mkpath;
        $file->copy($path);
    }

    io($sums_file)->print( join "\n", map "$_\t$sums{$_}", sort keys %sums );

    say "done";
    $|++;
    print "\7";
    sleep 1;
    print "\7";
    sleep 1;
    print "\7";
    return;
}
