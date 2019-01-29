use 5.020;
use strictures 2;

$|++;
run();

sub call {
    my ($script) = @_;
    system qq[perl $script] and exit;
}

sub run {
    $|++;
    say "you might want to run this to enable kanji: chcp 65001";
    say "your terminal's font will need to support kanji too (e.g. MS Gothic, MS Mincho)\n";

    my @opt = @ARGV;
    @ARGV = ();
    call "translate_utf8_binary.pl @opt";
    call "modify_fonts_and_inject.pl";
    call "mod_textures.pl";
    call "copy_unmodded_files.pl";
    call "import_files_to_assets.pl";
    try_copy();

    say "all done";
    return;
}

sub try_copy {
    print "WARNING: \7 This will delete all unknown files in ux0:/rePatch/PCSG00684/Media, backup if you have anything there.\nReady to copy? [y]";
    my $in .= <>;    # i have no idea why i need to do this twice
    $in .= <>;
    $in =~ s/[\r\n]//g;
    return if $in and $in !~ /^y/;
    call "vita_copy.pl";
    return;
}
