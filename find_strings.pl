use 5.020;
use strictures 2;
use IO::All -binary;
use List::Util qw'sum uniq min';
use Encode qw' decode encode ';
use Tk;
use Tk::Font;
use utf8;
use B 'perlstring';
use lib '.';
use binary_translations;

run();

sub utf8_asset_files {
    my $has_find = -e "c:/cygwin/bin/find.exe";
    say "has find: $has_find";
    my $src_dir = "../kc_original_unpack/repatch/PCSG00684/Media/Unity_Assets_Files/";
    my @list = $has_find ? split /\n/, `c:/cygwin/bin/find "$src_dir" -type f`    #
      : io($src_dir)->All_Files;
    @list = grep !/\.(tex|dds(_\d)*|mat|gobj|shader|txt|xml|ttf|amtc|ani|avatar|cbm|flr|fsb|mesh|obj|physmat2D|rtex|script|snd|[0-9]+)$/, @list;
    @list = map +( ref $_ ? $_ : io($_) ), @list;
    @list = map +{ file => $_, filename => $_->filename, fileparts => [ split /\/|\\/, $_ ], enc => "UTF-8" }, @list;
    $_->{fileid} = join "/", @{ $_->{fileparts} }[ 4 .. $#{ $_->{fileparts} } ] for @list;
    return @list;
}

sub run {
    $|++;
    binmode STDOUT, ":encoding(UTF-8)";
    binmode STDERR, ":encoding(UTF-8)";

    say "prepping dictionary";
    my %tr = binary_translations->data;

    my %chars    = map +( $_ => 1 ), map split( //, $_ ), keys %tr;
    my @chars    = sort keys %chars;
    my @qchars   = map quotemeta, @chars;
    my $chars    = quotemeta join "", sort keys %chars;
    my $chars_re = qr/[\p{Hiragana}\p{Katakana}\p{Han}\r\n0-9a-zA-Z$chars]+/;
    my $jp       = qr/[\p{Hiragana}\p{Katakana}\p{Han}]+/;

    say "grabbing file list";
    my @list = utf8_asset_files;
    @list = sort { lc $a->{fileid} cmp lc $b->{fileid} } @list;

    say "scanning files for hits";
    my %found;
    for my $file (@list) {
        my $content = decode "UTF-8", $file->{file}->all;
        my @hits = ( $content =~ /($chars_re$jp$chars_re|$chars_re$jp|$jp$chars_re)/g );
        for my $hit (@hits) {
            next if $tr{$hit};
            $found{$hit}++;
        }
    }

    say "saving hits";

    my $f = sub {
        $_ =~ s/\n/\\n/g;
        $_ =~ s/\r/\\r/g;
        $_ =~ s/"/\\"/g;
        $_;
    };
    my $out = join "\n", map qq[        "$_" => {},], map $f->(), sort keys %found;
    io("unknown_hits.pl")->print( encode "UTF-8", $out );

    return;
}
