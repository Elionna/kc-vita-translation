use 5.020;
use strictures 2;
use IO::All -binary;
use List::Util qw'sum uniq min';
use Encode qw' decode encode ';
use JSON::MaybeXS qw' decode_json encode_json ';
use Tk;
use Tk::Font;
use utf8;
use lib '.';
use binary_translations;

run();

sub saynl {
    my ($msg) = @_;
    $msg ||= $_;
    $msg =~ s/\n/\\n/g;
    $msg =~ s/\r/\\r/g;
    say $msg;
}

sub map_str_to_multi_chars {
    my ( $tr, $enc, $length_target, $used, $glyphmap_cache, $prepared ) = @_;
    saynl "mapping: ($length_target) '$tr'";
    my %rev_prep = reverse $prepared->%*;
    my @glyphs   = sort { length $a <=> length $b } sort keys $prepared->%*;
    my @parts    = split /(?<=\])|(?=\[)/, $tr;

    my $e = sub { encode $enc, join "", @_ };
    my $l = sub { length $e->(@_) };
    return $e->(@parts) if $l->(@parts) == $length_target;

    my $try = $glyphmap_cache->{$enc}{$tr};
    if ( $try and my @maps = keys $try->%* ) {
        for my $map (@maps) {
            my @map_parts = split /\|/, $map;
            my @filtered_map_parts = map +( $_ !~ /^\$(.*$)/ ? $_ : $prepared->{$1} ? $prepared->{$1} : $_ ), @map_parts;
            next if grep /^\$/, @filtered_map_parts;
            next if $l->(@filtered_map_parts) != $length_target;
            say "using cached mapping";
            $used->{ $rev_prep{$_} }++ for grep defined $rev_prep{$_}, @filtered_map_parts;
            return $e->(@filtered_map_parts);
        }
    }

    my @tasks = ( \@parts );
    my @result;

    my ( %seen, @failed, %closest );
    eval {
        # the lists made before each loop aren't faster, but easier to debug
        while (@tasks) {
            my @parts = shift(@tasks)->@*;
            $seen{ $e->(@parts) }++;
            die "tried for too long, add more things to the font mod pairs" if 80_000 == keys %seen;

            my $l2   = $l->(@parts);
            my $diff = $l2 - $length_target;
            if ( not $diff ) {
                @result = @parts;
                last;
            }

            my $fail = "length in encoding $enc: $length_target -> $l2 : " . join "|", map +( $rev_prep{$_} ? "\$$rev_prep{$_}" : $_ ), @parts;
            push @{ $closest{ abs $diff } }, $fail;
            push @failed, $fail;

            #if ( @failed > 100 ) {
            #    my $closest_diff = min keys %closest;
            #    my @closest = map "closest: $_", $closest{$closest_diff}->@* if $closest_diff;
            #    saynl for @failed, @closest;
            #    @failed = ();
            #}

            my $length_current = $l->(@parts);
            my $need_to_shrink = $length_current - $length_target;
            next if $need_to_shrink < 0 and $enc eq "UTF-16LE";
            my @to_process = grep +( $parts[$_] !~ /^\[/ and not $rev_prep{ $parts[$_] } ), 0 .. $#parts;
            for my $i (@to_process) {
                my $part = $parts[$i];
                my @matches = grep index( $part, $_ ) != -1, @glyphs;
                if ( $need_to_shrink > 0 ) {
                    my $size_limit =
                        ( $need_to_shrink > 85 ) ? 8
                      : ( $need_to_shrink > 75 ) ? 7
                      : ( $need_to_shrink > 65 ) ? 6
                      : ( $need_to_shrink > 55 ) ? 5
                      : ( $need_to_shrink > 35 ) ? 4
                      : ( $need_to_shrink > 25 ) ? 3
                      : ( $need_to_shrink > 15 ) ? 2
                      :                            1;
                    my @sorted_matches;
                    while ($size_limit) {
                        push @sorted_matches, grep length > $size_limit, @matches;
                        $size_limit--;
                    }
                    @matches = uniq @sorted_matches;
                    1;
                }
                my @next_tasks = map {
                    my ( $p1, $p2 ) = split /\Q$_\E/, $part, 2;
                    my @parts2 = @parts;
                    splice @parts2, $i, 1, grep length, $p1, $prepared->{$_}, $p2;
                    $seen{ $e->(@parts2) } ? () : \@parts2;
                } @matches;
                unshift @tasks, @next_tasks;
            }
        }
    };

    say "attempts: " . keys %seen;

    my @mapped       = @result;
    my $closest_diff = min keys %closest;
    push @failed, map "closest: $_", $closest{$closest_diff}->@* if $closest_diff;
    @mapped = @parts if not @mapped;
    $used->{ $rev_prep{$_} }++ for grep defined $rev_prep{$_}, @mapped;
    my $raw = join "|", map +( $rev_prep{$_} ? "\$$rev_prep{$_}" : $_ ), @mapped;
    return ( $e->(@mapped), $raw, uniq @failed );
}

sub map_tr_to_multi_chars {
    my ( $jp, $enc, $obj, $used, $glyphmap_cache, %prepared ) = @_;
    $DB::single = $DB::single = 1 if $obj->{tr} eq "Fusou";
    my $target_length = length encode $enc, $jp;
    my ( $tr, $raw, @failed ) = map_str_to_multi_chars( $obj->{tr}, $enc, $target_length, $used, $glyphmap_cache, \%prepared );
    my $l_tr = length $tr;
    if ( $target_length != $l_tr ) {
        my @msg = ( "length wanted: $target_length", @failed, "translation '$jp' ($target_length) => '$obj->{tr}' ($l_tr) doesn't match in length" );
        saynl for @msg;
        return @msg;
    }
    if ($raw) {
        $glyphmap_cache->{$enc}{ $obj->{tr} }{$raw} = 1;
        io("glyphmap.cache")->utf8->print( JSON::MaybeXS->new( pretty => 1, canonical => 1 )->encode($glyphmap_cache) );
    }
    $obj->{tr_mapped}{$enc} = $tr;
    return;
}

sub trim_nl {
    my ($s) = @_;
    $s =~ s/[\r\n]//g;
    return $s;
}

sub add_mapped {
    my ( $dictionary, $enc, $used, $glyphmap_cache, %mapping ) = @_;
    return map map_tr_to_multi_chars( $_, $enc, $dictionary->{$_}, $used, $glyphmap_cache, %mapping ),    #
      reverse sort { length $dictionary->{$a}{tr} <=> length $dictionary->{$b}{tr} }
      grep length $dictionary->{$_}{tr},
      sort keys $dictionary->%*;
}

sub get_hits {
    my ( $content, $jp, $enc ) = @_;
    my @hits;
    my $pos = 0;
    while ( ( my $hit = index $content, encode( $enc, $jp ), $pos ) != -1 ) {
        push @hits, $hit;
        $pos = $hit + 1;
    }
    return @hits;
}

sub check_for_null_bracketing {    # looks at the following char as utf8 one to make scanning easier
    my ( $content, $jp, $enc, $hit ) = @_;
    return if $enc eq "UTF-16LE";
    my $decode_content = decode $enc, substr $content, $hit;
    my $pre = ord substr $content, $hit - 1, 1;
    my $post = ord substr $decode_content, length $jp, 1;
    my %acceptable = map +( $_, 1 ), ( 0, 0x1, 0x4, 0x6, 0x7, 0x8, 0xB, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0xFFFD );
    return ( $pre == 0 and $acceptable{$post} );
}

sub utf8_asset_files {
    my $has_find = -e "c:/cygwin/bin/find.exe";
    say "has find: $has_find";
    my $src_dir = "../kc_original_unpack/Media/Unity_Assets_Files/";
    my @list = $has_find ? split /\n/, `c:/cygwin/bin/find "$src_dir" -type f`    #
      : io($src_dir)->All_Files;
    @list = grep !/\.(tex|dds(_\d)*|mat|gobj|shader|txt|xml|ttf|amtc|ani|avatar|cbm|flr|fsb|mesh|obj|physmat2D|rtex|script|snd|[0-9]+)$/, @list;
    @list = map +( ref $_ ? $_ : io($_) ), @list;
    @list = map +{ file => $_, filename => $_->filename, fileparts => [ split /\/|\\/, $_ ], enc => "UTF-8" }, @list;
    $_->{fileid} = join "/", @{ $_->{fileparts} }[ 4 .. $#{ $_->{fileparts} } ] for @list;
    return @list;
}

sub report_near_miss {
    my ( $file_hit, $hit, $enc, $jp, $content ) = @_;
    my $mod = $hit - ( $hit % 16 );
    my ( $offset, $extract ) = (0);
    while ( $offset < 3 ) {
        $extract = decode $enc, substr $content, $hit - 16 + $offset, 28 + 2 * length encode $enc, $jp;
        last if $extract =~ /\Q$jp\E/;
        $offset++;
    }
    my ($ords) = map "[$_]", join "|", map uc sprintf( "%x", ord ), split //, $extract;
    my $msg = sprintf "hit '%s' %08x %08x not marked skipped or ok, please verify %s in >%s< %s", $file_hit, $mod, $hit, $jp, $extract, $ords;

    # need to remain: newlines: A D, jp space: 3000
    $msg =~ s/\x{$_}/■/g
      for 0 .. 8,
      qw( B C E F 10 11 12 13 15 17 18 19 1A 1B 1C 1D 1E 14 600 900 300 500 B00 1D00 D00 1700 800 1500 1900 F00 700 1B00 1D00 1F00 1300 1100 2000 2100 2300 2500 2700 2900 2A00 2B00 2D00 3200 321E 3428 3C3D 3D00 3E30 3F00 4300 4900 4C30 4D00 661A 7B00 7D00 FFFD   );
    saynl $msg;
}

sub duplicate_check {
    my %seen;
    $seen{$_}++ for binary_translations->data;
    my @duplicates = grep $seen{$_} > 1, keys %seen;
    die "following keys are duplicate in dictionary: @duplicates" if @duplicates;
    return;
}

sub run {
    $|++;
    binmode STDOUT, ":encoding(UTF-8)";
    binmode STDERR, ":encoding(UTF-8)";
    my $do_blank = grep /--blank/, @ARGV;

    say "prepping dictionary";

    duplicate_check;
    my %tr = binary_translations->data;
    $tr{$_}{tr} //= "" for grep !defined $tr{$_}{tr}, sort keys %tr;
    my @tr_keys = reverse sort { length $a <=> length $b } sort keys %tr;
    my $tr_body = join "#", map $tr{$_}{tr}, keys %tr;

    my @pairs = grep length $_, map split( /\|/, $_ ), map trim_nl($_), io("font_mod_character_pairs")->getlines;
    @pairs = ( @pairs, map ucfirst, @pairs );
    say "" . @pairs;
    @pairs = grep $tr_body =~ /\Q$_\E/, @pairs;
    say "" . @pairs;

    my %mapping = do {
        my $unicode = 0xE000;
        map +( $pairs[$_] => chr( $unicode + $_ ) ), 0 .. $#pairs;
    };

    my ( $mw, $font_name ) = ( MainWindow->new, "Ume P Gothic S4" );
    my $font = $mw->fontCreate( "test", -family => $font_name, -size => 18 );
    my %what = $font->actual;
    die "didn't create right font, but: $what{-family}" if $what{-family} ne $font_name;
    for my $jp (@tr_keys) {
        $tr{$jp}{width}       = $font->measure($jp);
        $tr{$jp}{width_tr}    = $font->measure( $tr{$jp}{tr} );
        $tr{$jp}{width_ratio} = sprintf "%.2f", $tr{$jp}{width_tr} / $tr{$jp}{width};
    }
    for my $jp ( reverse sort { $tr{$a}{width_ratio} <=> $tr{$b}{width_ratio} } sort keys %tr ) {
        next if $tr{$jp}{width_ratio} <= 1;
        my $msg = " $tr{$jp}{width_ratio} = $tr{$jp}{width} : $tr{$jp}{width_tr} -- $jp #-# $tr{$jp}{width_ratio} = $tr{$jp}{width} : $tr{$jp}{width_tr} -- $tr{$jp}{tr}";
        $msg =~ s/#-#/\n/g;
        saynl $msg;
    }
    print "\n";

    my $g = "glyphmap.cache";
    my %glyphmap_cache = -e $g ? JSON::MaybeXS->new( pretty => 1 )->decode( io($g)->utf8->all )->%* : ();

    my %used;
    my @too_long = map add_mapped( \%tr, $_, \%used, \%glyphmap_cache, %mapping ), "UTF-16LE", "UTF-8";
    my @unused = grep !$used{$_}, keys %mapping;
    say "following tuples unused: @unused\nfollowing tuples used: '" . ( join "|", sort keys %used ) . "'\n" if @unused;
    die "\n" if @too_long;

    if ($do_blank) {
        for my $enc ( "UTF-16LE", "UTF-8" ) {
            $tr{$_}{tr_mapped}{$enc} = "\0" x length encode $enc, $_ for keys %tr;
        }
    }

    # this converts any single string ok/skip entries into arrays, or fills in empty arrays if there's none
    for my $entry ( values %tr ) {
        $entry->{$_} = !defined $entry->{$_} ? [] : !ref $entry->{$_} ? [ $entry->{$_} ] : $entry->{$_} for qw( ok skip );
    }

    say "grabbing file list";
    my @list = utf8_asset_files;
    push @list, {    #
        file      => io("../kc_original/Media/Managed/Assembly-CSharp.dll"),
        filename  => "Assembly-CSharp.dll",
        fileparts => [ split /\/|\\/, "../kc_original/Media/Managed/Assembly-CSharp.dll" ],
        enc       => [ "UTF-16LE", "UTF-8" ],
        fileid    => "a-csharp",
    };
    @list = sort { lc $a->{fileid} cmp lc $b->{fileid} } @list;
    say "prepped";
    my %found;
    my %unmatched;
    my %hit;

    for my $file (@list) {
        my $content = $file->{file}->all;
        my $f_enc   = $file->{enc};
        my $found;
        for my $enc ( ref $f_enc ? $f_enc->@* : $f_enc ) {
            for my $jp (@tr_keys) {
                next unless    #
                  my @hits = get_hits $content, $jp, $enc;
                my %obj = $tr{$jp}->%*;
                for my $hit (@hits) {
                    my $file_hit = "$file->{fileid} $hit";
                    next if grep $file_hit eq $_, $obj{skip}->@*;
                    $hit{$jp}++;
                    if ( !grep $file_hit eq $_, $obj{ok}->@* and !check_for_null_bracketing $content, $jp, $enc, $hit ) {
                        $unmatched{$jp}++;
                        report_near_miss $file_hit, $hit, $enc, $jp, $content;
                        next;
                    }
                    next if !$do_blank and !length $obj{tr};
                    substr( $content, $hit, length $_ ) = $_ for $obj{tr_mapped}{$enc};
                    $found++;
                    $found{$jp}++;
                }
            }
        }
        my @file_parts = $file->{fileparts}->@*;
        $file_parts[1] = "kc_original_unpack_modded";
        my $target = join "/", @file_parts;
        if ( !$found ) {
            io($target)->unlink if -e $target;
            next;
        }
        io( io->file($target)->filepath )->mkpath;
        io($target)->print($content);
    }

    my @maybe = map sprintf( "  %-" . ( 30 - length $_ ) . "s %-30s hit x %3s, nomatch x %3s, match x %3s", $_, $tr{$_}{tr}, $hit{$_}, $unmatched{$_}, $hit{$_} - $unmatched{$_} ),
      reverse sort { length $a <=> length $b } sort keys %unmatched;
    my @nowhere = map sprintf( "  %-" . ( 30 - length $_ ) . "s $tr{$_}{tr}", $_ ), grep +( !$found{$_} and !$unmatched{$_} ), @tr_keys;
    saynl for "", "strings not always identified confidently:", @maybe, "", "strings found nowhere:", @nowhere;

    say "\ndone";
    return;
}
