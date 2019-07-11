use 5.010;
use strictures 2;
use IO::All -binary;
use Capture::Tiny 'capture';
use Digest::SHA1 'sha1_hex';

run();

sub _clean_and_report {
    my ( $out, $err, $res, $target ) = @_;
    -d $target ? io($target)->rmtree : io($target)->unlink;
    return "error:\nout:\n$out\nerr:\n$err\nres: $res\n";
}

sub prep_font {
    my ( $ff, $prepped_path, %font ) = @_;

    say "preparing font from source with empty PUA glyphs and guaranteed space glyph";
    my ( $out, $err, $res ) = capture { system(qq["$ff" -script prepare_new_font.ff "$font{src}" "$prepped_path"]) };
    $err =~ s/^Copyright.*\n(^ .*\n)+//m;
    $err =~ s/^The following table.*ignored.*\n(^ .*\n)+//m;
    $err =~ s/^Warning:.*\n(^ .*\n)+//mg;
    $err =~ s/^This font contains both.*\n(^ .*\n)+//m;
    $err =~ s/^.*SEAC-like endchar operator is deprecated for Type2\n//mg;
    $err =~ s/^Subtable status not filled in .*\n*//mg;
    $err =~ s/^The glyph named .* is mapped to .*\n  But its name indicates it should be mapped to .*\n*//mg;                  # research
    $err =~ s/^Bad sfd file. Glyph .* has width .* even though it should be\n  bound to the width of .* which is .*\n*//mg;    # research
    $out =~ s/^log:.*\n//mg;
    die _clean_and_report $out, $err, $res, $prepped_path if $out or $err or $res;
    return;
}

sub _age ($) {
    eval { ( stat $_[0] )[9] } || 0;
}

sub make_font {
    my ( $ff, %font ) = @_;

    my $work_base    = "../fonts/" . sha1_hex $font{src};
    my $prepped_path = "$work_base.sfdir";
    $prepped_path =~ s/\.ttf//;
    prep_font $ff, $prepped_path, %font if !-e $prepped_path;

    my $temp_target = "$work_base.otf";
    return $temp_target if _age $temp_target > _age "font_mod_character_pairs";

    say "modding font with multi-glyphs";
    my $mod_dir = "$work_base\_mod.sfdir";
    my ( $out, $err, $res ) = capture { system(qq[python font_mod_run.py "$font{name}" "$prepped_path" "$mod_dir"]) };
    die _clean_and_report $out, $err, $res, $mod_dir if $out or $err or $res;

    say "generating font file from mod dir";
    my ( $out2, $err2, $res2 ) = capture { system(qq["$ff" -script generate_font_from_dir.ff "$mod_dir" "$temp_target"]) };
    $err2 =~ s/^Copyright.*\n(^ .*\n)+//m;
    $err2 =~ s/^Lookup sub table.*is too big.*\n//m;
    $err2 =~ s/^Lookup .* has an\n(^  .*\n)+//m;
    die _clean_and_report $out2, $err2, $res2, $temp_target if $out2 or $err2 or $res2;

    return $temp_target;
}

sub mod_font {
    my ( $ff, $prepped, %font ) = @_;
    say "processing font '$font{name}'";

    my $target_body = "$font{target_path}/$font{name}";
    if ( _age "$target_body.ttf" > _age "font_mod_character_pairs" ) {
        say "font was generated after last change to char tuples, skipping\n";
        return;
    }

    my $modded_font = $prepped->{ $font{src} } ||= make_font $ff, %font;

    # the extension here is for compatibility. the fonts in kc are OTF, but
    # have the extension ttf, so we generate the replacements to match
    say "fixing final extension";
    io( $font{target_path} )->mkpath;
    io($modded_font)->copy("$target_body.ttf");

    say "done\n";
    return;
}

sub run {
    my $ff        = "C:/Program Files (x86)/FontForgeBuilds/bin/fontforge.exe";
    my $t_body    = "Media/Unity_Assets_Files";
    my $t_root    = "../kc_original_unpack_modded/repatch/PCSG00684/$t_body";
    my $font_root = "../kc_original_unpack/repatch/PCSG00684/$t_body";

    # ume is used to make texts ingame more narrow
    my @fonts = (
        { name => "A-OTF-UDShinGoPro-Regular",              target_path => "$t_root/sharedassets3",  src => "fonts/ume-pgs4.ttf" },
        { name => "A-OTF-ShinGoPro-Regular",                target_path => "$t_root/sharedassets2",  src => "fonts/ume-pgs4.ttf" },
        { name => "CenturyGothicStd",                       target_path => "$t_root/resources",      src => "fonts/ume-pgs4.ttf" },
        { name => "A-OTF-ShinGoPro-Regular-For-ScrollList", target_path => "$t_root/sharedassets5",  src => "fonts/ume-pgs4.ttf" },
        { name => "AxisStd-Regular_1",                      target_path => "$t_root/sharedassets5",  src => "fonts/ume-pgs4.ttf" },
        { name => "A-OTF-RyuminPr5-ExBold",                 target_path => "$t_root/sharedassets11", src => "fonts/ume-pgs4.ttf" },
        { name => "Century Gothic",                         target_path => "$t_root/sharedassets26", src => "fonts/ume-pgs4.ttf" },
    );
    io("../fonts")->mkdir if !-d "../fonts";
    my %prepped;
    mod_font $ff, \%prepped, $_->%* for @fonts;
    say "done with fonts";
    return;
}
