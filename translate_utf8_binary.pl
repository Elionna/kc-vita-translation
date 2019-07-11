use 5.020;
use strictures 2;
use IO::All -binary;
use List::Util qw' sum uniq min max ';
use Encode qw' decode encode ';
use JSON::MaybeXS qw' decode_json encode_json ';
use Tk;
use Tk::Font;
use utf8;
use Array::Split 'split_into';
use Getopt::Long::Descriptive;
use XML::LibXML;
use lib '.';
use binary_translations;
use label;
use presetdata;
use motionlist;
use presetdeck;
use presetship;

=head1 DESCRIPTION

This is a mess. I'm sorry. I was in a hurry. Still a mess. :(

=cut

my $jp_qr = qr/[\p{Hiragana}\p{Katakana}\p{Han}]/;

run();

sub filter_nl {
    my (@msgs) = @_;
    @msgs = ( $_ ? $_ : () ) if !@msgs;
    for (@msgs) {
        s/\n/\\n/g;
        s/\r/\\r/g;
        s/\t/\\t/g;
        s/\x7/\\7/g;
    }
    return @msgs;
}

sub saynl   { say   for filter_nl @_; return }
sub printnl { print for filter_nl @_; return }

sub matches_for_part {
    my ( $part, $need_to_shrink, $enc, @glyphs ) = @_;
    my @matches = grep index( $part, $_ ) != -1, @glyphs;
    return @matches if $need_to_shrink <= 0;
    my $size_limit =
        ( $need_to_shrink > 41 ) ? 11
      : ( $need_to_shrink > 39 ) ? 10
      : ( $need_to_shrink > 36 ) ? 9
      : ( $need_to_shrink > 33 ) ? 8
      : ( $need_to_shrink > 30 ) ? 7
      : ( $need_to_shrink > 27 ) ? 6
      : ( $need_to_shrink > 24 ) ? 5
      : ( $need_to_shrink > 21 ) ? 4
      : ( $need_to_shrink > 18 ) ? 3
      : ( $need_to_shrink > 15 ) ? 2
      :                            1;
    my @sorted_matches;
    while ($size_limit) {
        push @sorted_matches, grep length > $size_limit, @matches;
        $size_limit--;
    }
    my $min = !$need_to_shrink ? 0 : ( $enc eq "UTF-16LE" ) ? 1 : 2;
    @sorted_matches = grep length > $min, @sorted_matches;
    return uniq @sorted_matches;
}

sub tasks_for_matches {
    my ( $match, $part, $prepared, $i, $e, $seen, @parts ) = @_;
    my ( $p1, $p2 ) = split /\Q$match\E/, $part, 2;
    my @parts2 = @parts;
    splice @parts2, $i, 1, grep length, $p1, $prepared->{$match}, $p2;
    my @tasks = $seen->{ $e->(@parts2) } ? () : ( \@parts2 );
    return @tasks;
}

sub map_str_to_multi_chars {
    my ( $tr, $enc, $length_target, $used, $glyphmap_cache, $prepared, $verbose ) = @_;
    my $intro = "mapping: ($length_target) '$tr' ";
    my $delayed_intro = $verbose ? "" : $intro;
    printnl $intro if $verbose;
    my %rev_prep = reverse $prepared->%*;
    my @glyphs = sort { length $a <=> length $b } sort keys $prepared->%*;

    my $e = sub { encode $enc, join "", @_ };
    my $l = sub { length $e->(@_) };
    if ( $enc eq "UTF-16LE" and $l->($tr) < $length_target ) {
        while ( $l->($tr) < $length_target ) {
            $tr .= "\x{200B}";
        }
    }
    my @parts = split /(?<=\])|(?=\[)|(?<=\})|(?=\{)/, $tr;
    if ( $l->(@parts) == $length_target ) {
        saynl $delayed_intro. "using translation";
        return $e->(@parts);
    }

    my $try = $glyphmap_cache->{$enc}{$tr};
    if ( $try and my @maps = keys $try->%* ) {
        for my $map (@maps) {
            my @map_parts = split /\|/, $map;
            my @filtered_map_parts = map +( $_ !~ /^\$(.*$)/ ? $_ : $prepared->{$1} ? $prepared->{$1} : $_ ), @map_parts;
            next if grep /^\$/, @filtered_map_parts;
            next if $l->(@filtered_map_parts) != $length_target;
            say "using cached mapping" if $verbose;
            $used->{ $rev_prep{$_} }++ for grep defined $rev_prep{$_}, @filtered_map_parts;
            return $e->(@filtered_map_parts);
        }
    }

    my @tasks = ( \@parts );
    my @result;

    my ( %seen, @failed, %closest, %panic_reported );
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
            push $closest{ abs $diff }->@*, $fail;
            unshift @failed, $fail;

            if ( @failed > 200 ) {
                my $closest_diff = min keys %closest;
                my @closest = map "closest: $_", $closest{$closest_diff}->@* if $closest_diff;
                @closest = grep !$panic_reported{$_}++, @closest;
                saynl for @closest;
            }

            my $length_current = $l->(@parts);
            my $need_to_shrink = $length_current - $length_target;
            next if $need_to_shrink < 0 and $enc eq "UTF-16LE";
            my @to_process = grep +( $parts[$_] !~ /^[\[\{]/ and not $rev_prep{ $parts[$_] } ), 0 .. $#parts;
            @to_process = sort { length $parts[$a] <=> length $parts[$b] } @to_process;
            for my $i (@to_process) {
                my $part       = $parts[$i];
                my @matches    = matches_for_part( $part, $need_to_shrink, $enc, @glyphs );
                my @next_tasks = map tasks_for_matches( $_, $part, $prepared, $i, $e, \%seen, @parts ), @matches;
                unshift @tasks, @next_tasks;
            }
        }
    };

    say "attempts: " . keys %seen;

    my @mapped       = @result;
    my $closest_diff = min keys %closest;
    my @closest      = $closest_diff ? map "closest: $_", $closest{$closest_diff}->@* : ();
    @failed = @closest ? ( @closest, ( $verbose ? @failed : () ) ) : @failed;
    @mapped = @parts if not @mapped;
    $used->{ $rev_prep{$_} }++ for grep defined $rev_prep{$_}, @mapped;
    my $raw = join "|", map +( $rev_prep{$_} ? "\$$rev_prep{$_}" : $_ ), @mapped;
    return ( $e->(@mapped), $raw, uniq reverse @failed );
}

sub map_tr_to_multi_chars {
    my ( $jp, $enc, $obj, $used, $glyphmap_cache, $verbose, %prepared ) = @_;
    my $target_length = length encode $enc, $jp;
    my ( $tr, $raw, @failed ) = map_str_to_multi_chars( $obj->{tr}, $enc, $target_length, $used, $glyphmap_cache, \%prepared, $verbose );
    my $l_tr = length $tr;
    if ( $target_length != $l_tr ) {
        my @msg = ( "length wanted: $target_length", @failed, "translation '$jp' ($target_length) => '$obj->{tr}' ($l_tr) doesn't match in length" );
        saynl @msg;
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
    my ( $dictionary, $enc, $used, $glyphmap_cache, $verbose, %mapping ) = @_;
    return map map_tr_to_multi_chars( $_, $enc, $dictionary->{$_}, $used, $glyphmap_cache, $verbose, %mapping ),    #
      reverse sort { length $dictionary->{$a}{tr} <=> length $dictionary->{$b}{tr} }
      grep length $dictionary->{$_}{tr},
      sort keys $dictionary->%*;
}

sub tr_in_enc {
    my ( $tr, $enc, $used, $glyphmap_cache, $verbose, %mapping ) = @_;
    map_tr_to_multi_chars( $tr->{orig}, $enc, $tr, $used, $glyphmap_cache, $verbose, %mapping ) if !defined $tr->{tr_mapped}{$enc};
    die "check log above, there was a translation that couldn't be matched\n" if !defined $tr->{tr_mapped}{$enc};
    return $tr->{tr_mapped}{$enc};
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

sub squares_for_mojibake {
    my ($msg) = @_;

    # need to remain: newlines: A D, jp space: 3000
    $msg =~ s/\x{$_}/■/g
      for 0 .. 8,
      qw( B C E F 10 11 12 13 15 17 18 19 1A 1B 1C 1D 1E 14 600 900 300 500 B00 C00 1D00 D00 1700 800 1500 1900 F00 700 70C 1B00 1D00 1F00 1300 1100 2000 2100 2300 2500 2700 2900 2A00 2B00 2D00 3100 3200 321E 3300 3428 3500 3700 3900 3B00 3C3D 3D00 3E30 3F00 4100 4300 4900 4C30 4D00 4F00 5100 6200 661A 6C00 6D6F 6E00 7000 7500 7900 7B00 7D00 8000 8580 8780 8980 9600 E00A FFFD   );
    return $msg;
}

sub check_for_null_bracketing {
    my ( $content, $jp, $enc, $hit, $file ) = @_;
    my $translation_length = length encode $enc, $jp;
    if ( $file->{filename} eq "Assembly-CSharp.dll" and $enc eq "UTF-16LE" ) {
        my $length = unpack 'C', substr $content, $hit - 1, 1;
        my $c = ord substr $content, $hit + $length - 1, 1;
        my $sample = decode $enc, substr $content, $hit, $length;

        # sometimes the sizes match exactly, but sometimes the string also has a
        # zero byte added. if the size matches exactly there is never a divider
        # after it, just the size indicator and the next string.
        return 1 if $length == $translation_length or ( !$c and $translation_length == $length - 1 );
        my $msg = squares_for_mojibake "wanted $translation_length for '$jp', header indicated $length, $c: '$sample'";
        saynl $msg if $length;

        #saynl $msg if $length and !$c and $sample =~ $jp_qr and $msg !~ /■|■/;
        return;
    }
    if ( $file->{filename} eq "Assembly-CSharp.dll" and $enc eq "UTF-8" ) {
        my $length = unpack 'C', substr $content, $hit - 1, 1;
        my $sample = decode $enc, substr $content, $hit, $length;
        return 1 if $length == $translation_length;
        my $msg = squares_for_mojibake "wanted $translation_length for '$jp', header indicated $length: '$sample'";
        saynl $msg if $length;

        #saynl $msg if $length and $sample =~ $jp_qr and $msg !~ /■|■/;
    }
    if ( $file->{filename} ne "Assembly-CSharp.dll" and $enc eq "UTF-8" ) {
        my $length = unpack 'L', substr $content, $hit - 4, 4;
        return 1 if $length and $length == $translation_length;
        saynl squares_for_mojibake $_ for split /#-#/, sprintf "wanted      % 4s for '$jp'#-#header gave % 4s     '%s'", $translation_length, $length, decode $enc, substr $content, $hit, $length;
    }
    my $decode_content = decode $enc, substr $content, $hit;
    my $pre = ord substr $content, $hit - 1, 1;
    my $post = ord substr $decode_content, length $jp, 1;
    my %acceptable = map +( $_, 1 ), ( 0, 0x1, 0x4, 0x6, 0x7, 0x8, 0xB, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0xFFFD );
    return ( $pre == 0 and $acceptable{$post} );
}

sub utf8_asset_files {
    my ($src_dir) = @_;
    my $has_find = -e "c:/cygwin/bin/find.exe";
    say "has find: $has_find";
    my @list = $has_find ? split /\n/, `c:/cygwin/bin/find "$src_dir" -type f`    #
      :                    io($src_dir)->All_Files;
    @list = grep !/\.(tex|dds(_\d)*|mat|gobj|shader|txt|ttf|amtc|ani|avatar|cbm|flr|fsb|mesh|obj|physmat2D|rtex|script|snd|[0-9]+)$/, @list;
    @list = map +( ref $_ ? $_ : io($_) ), @list;
    @list = map +{ file => $_, filename => $_->filename, fileparts => [ split /\/|\\/, $_ ], enc => "UTF-8", ext => $_->ext }, @list;
    $_->{fileid} = join "/", @{ $_->{fileparts} }[ 4 .. $#{ $_->{fileparts} } ] for @list;
    return @list;
}

sub report_near_miss {
    my ( $file_hit, $hit, $enc, $jp, $content, $is_a_hit ) = @_;
    my $mod = $hit - ( $hit % 16 );
    my ( $offset, $extract ) = (0);
    while ( $offset < 3 ) {
        $extract = decode $enc, substr $content, $hit - 32 + $offset, 16 + 28 + 2 * length encode $enc, $jp;
        last if $extract =~ /\Q$jp\E/;
        $offset++;
    }
    my ($ords) = map "[$_]", join "|", map uc sprintf( "%x", ord ), split //, $extract;
    my $msg = sprintf "hit '%s' %08x %08x" . ( $is_a_hit ? "" : " not marked skipped or ok" ) . ", please verify %s in >%s< %s", $file_hit, $mod, $hit, $jp, $extract, $ords;
    saynl squares_for_mojibake $msg;
}

sub duplicate_check {
    my %seen;
    $seen{$_}++ for binary_translations->data;
    my @duplicates = grep $seen{$_} > 1, keys %seen;
    die "following keys are duplicate in dictionary: @duplicates" if @duplicates;
    return;
}

sub half { my $p = shift; ( split_into 2, @_ )[$p]->@* }

sub store_file_as_modded {
    my ( $file, $found, $content ) = @_;
    my @file_parts = $file->{fileparts}->@*;
    $file_parts[1] = "kc_original_unpack_modded";
    my $target = join "/", @file_parts;
    if ( !$found ) {
        io($target)->unlink if -e $target;
        return;
    }
    io( io->file($target)->filepath )->mkpath;
    io($target)->print($content);
    return;
}

sub handle_file_as_asset {
    my ( $do_blank, $report_matches, $found, $untranslated, $ignored, $file, %tr ) = @_;
    my ($extension) = ( $file->{filename} =~ /\.(-[0-9]+)$/ );
    my $pack        = $file->{fileparts}[-2];
    my %known       = (
        level2         => { -7  => "label" },
        level3         => { -5  => "label" },
        level4         => { -15 => "label" },
        level5         => { -6  => "label" },
        level6         => { -3  => "label" },
        level7         => { -5  => "label" },
        level8         => { -7  => "label" },
        level9         => { -5  => "label" },
        level11        => { -5  => "label" },
        level12        => { -6  => "label" },
        level13        => { -6  => "label" },
        level16        => { -7  => "label" },
        level17        => { -6  => "label" },
        level18        => { -6  => "label" },
        level19        => { -4  => "label" },
        level20        => { -8  => "label" },
        level21        => { -6  => "label" },
        level22        => { -4  => "label" },
        level23        => { -6  => "label" },
        level24        => { -6  => "label" },
        level25        => { -6  => "label" },
        sharedassets3  => { -2  => "label" },
        sharedassets5  => { -4  => "label" },
        sharedassets6  => { -5  => "label" },
        sharedassets7  => { -3  => "label" },
        sharedassets8  => { -8  => "label" },
        sharedassets10 => { -4  => "label" },
        sharedassets11 => { -18 => "label" },
        sharedassets17 => { -5  => "label" },
        resources      => {
            -10 => "label",
            -2  => "motionlist",    # Entity_MotionList 00001.-2 48605
            -3  => "presetdata",    # Entity_PresetData 00001.-3 48606
            -4  => "presetdeck",    # Entity_PresetDeck 00001.-4 48607
            -5  => "presetship",    # Entity_PresetShip 00001.-5 48608
        },
    );
    return 1 if $extension and $extension =~ /^-[0-9]+$/ and !$known{$pack}{$extension};    # unknown asset monobehaviors very unlikely to contain text
    return if !$extension or !$known{$pack}{$extension};                                    # other unknown stuff might contain something?
    my $type = $known{$pack}{$extension};
    return 1 if $type eq "skip";                                                            # we might wanna process these later
    my %known_types = map +( $_ => 1 ), qw( label motionlist presetdata presetdeck presetship );
    return $known_types{$type}
      ? handle_file_as_type( $type, $file, $found, $untranslated, $ignored, $do_blank, $report_matches, %tr )
      : die "unknown type $type";
}

sub presetdeck_meths {
    my ( $i, $obj ) = @_;
    my $objmeth = "deck_${i}_ship_count";
    return ( "deck_${i}_name", map "deck_${i}_ship_${_}_name", 1 .. $obj->$objmeth );
}

sub value_pairs_for_type {
    my ( $type, $file ) = @_;
    my $obj = $file->{obj} ||= $type->new( $file->{file} );
    return
        $type eq "motionlist" ? map [ name => $_ ], $obj->field_children("motionship")
      : $type eq "presetdata" ? map [ "preset_${_}_name" => $obj ], 1 .. $obj->obj_count
      : $type eq "presetship" ? map [ "ship_${_}_name", $obj ], 1 .. $obj->obj_count
      : $type eq "presetdeck" ? map [ $_, $obj ], map presetdeck_meths( $_, $obj ), 1 .. $obj->obj_count
      : $type eq "label" ? [ text => $obj ]
      :                    die "unknown type $type";
}

sub handle_file_as_type {
    my ( $type, $file, @args ) = @_;
    return if !grep try_and_translate_binparse_value( $_->[0], $_->[1], $file, @args ), value_pairs_for_type $type => $file;
    $file->{obj}->refresh;
    store_file_as_modded $file, 1, $file->{obj}->dump;
    return 1;
}

sub try_and_translate_binparse_value {
    my ( $meth, $obj, $file, $found, $untranslated, $ignored, $report_matches, $do_blank, %trs ) = @_;

    my $text = decode "UTF-8", $obj->$meth;
    return if !length $text;
    my $id = max - 1, keys %{ $found->{$text}{ $file->{file} } ||= {} };
    $found->{$text}{ $file->{file} }{ $id + 1 } = ( $trs{$text} || {} )->{tr};
    return $text !~ $jp_qr ? !++$ignored->{$text} : saynl "unable to find translation for: '$text' in: '$file->{file}'" unless    #
      my $tr = $trs{$text};
    return if $tr->{no_tr};
    return $untranslated->{$text}++ ? () : saynl "not yet translated in: '$file->{file}': '$text'"
      if not $tr->{tr} and not $do_blank;

    my $new_text = encode "UTF-8", $tr->{tr};
    my $set = "Set$meth";
    $obj->$set($new_text);
    saynl sprintf "binary parse result %-26s %10s : %-60s -> %-60s", "'$file->{filename}'", "", "'$text'", "'$tr->{tr}'" if $report_matches;
    return 1;
}

sub maybe_dual_length {
    my ( $content_ref, $offset ) = @_;

    my $length = ord substr $content_ref->$*, $offset, 1;
    $offset++;
    return ( $offset, $length ) if $length < 128;

    $length = ( $length - 128 ) * 256 + ord substr $content_ref->$*, $offset;
    $offset++;
    return ( $offset, $length );
}

sub parse_csharp {
    my ( $content_ref, $tr_in_enc, $do_blank, $report_matches, $found, $untranslated, $ignored, $file, %tr ) = @_;

    my $offset = 4095716;
    for ( 1 .. 4 ) {
        my $length = index substr( $content_ref->$*, $offset ), "\0";
        my $text = decode "UTF-8", substr $content_ref->$*, $offset, $length;
        try_replace_csharp( "UTF-8", $offset, $length, $text, @_ );
        $offset += $length + 1;
    }

    $offset = 4472589;
    while (1) {
        ( $offset, my $length ) = maybe_dual_length $content_ref, $offset;
        last if !$length;
        my $text = decode "UTF-16LE", substr $content_ref->$*, $offset, $length;
        try_replace_csharp( "UTF-16LE", $offset, $length, $text, @_ );
        $offset += $length;
    }

    $offset = 4681951;
    while (1) {
        ( $offset, my $length ) = maybe_dual_length $content_ref, $offset;
        last if !$length;
        my $bytes = substr $content_ref->$*, $offset, $length;
        my $next = $offset + $length;

        if ( $bytes =~ /^\x01\x00[\x00\x01\x02]/ ) { }
        elsif ( $bytes =~ /^\x01\x00/ ) {
            $offset += 2;
            while ( $offset < $next ) {
                ( $offset, my $slength ) = maybe_dual_length $content_ref, $offset;
                next if !$slength or $slength > ( $next - $offset );
                my $text = decode "UTF-8", substr $content_ref->$*, $offset, $slength;
                try_replace_csharp( "UTF-8", $offset, $slength, $text, @_ )
                  if $text !~ /\x00/;
                $offset += $slength;
            }
        }
        else {
            my $text = decode "UTF-16LE", $bytes;
            try_replace_csharp( "UTF-16LE", $offset, $length, $text, @_ )
              if $offset < 4699960 and $text !~ /[\x00\x02\x03\x08\x13\x1E\x81]/;
        }
        $offset = $next;
    }

    store_file_as_modded $file, 1, $content_ref->$*;

    return $content_ref->$*;
}

sub try_replace_csharp {
    my ( $enc, $offset, $length, $text, $content_ref, $tr_in_enc, $do_blank, $report_matches, $found, $untranslated, $ignored, $file, %trs ) = @_;
    $found->{$text}{ $file->{file} }{$offset} = ( $trs{$text} || {} )->{tr};
    return $text !~ $jp_qr ? ++$ignored->{$text} : !saynl "unable to find translation for: '$text' in: '$file->{file}'" unless    #
      my $tr = $trs{$text};
    return if $tr->{no_tr};
    return $untranslated->{$text}++ ? 1 : !saynl "not yet translated in: '$file->{file}': '$text'"
      if not $tr->{tr} and not $do_blank;
    my $new_text = $do_blank ? ( "\0" x $length ) : $tr_in_enc->( $tr, $enc );
    my $new_length = length $new_text;
    $new_text .= "\0" if $enc eq "UTF-16LE" and $new_length + 1 eq $length;
    $new_length = length $new_text;
    die "new text doesn't match $new_length != $length" if $new_length != $length;
    substr( $content_ref->$*, $offset, $length ) = $new_text;
    saynl sprintf "binary parse result %-26s %10s : %-60s -> %-60s", "'$file->{filename}'", $offset, "'$text'", "'$tr->{tr}'" if $report_matches;
    return;
}

sub translate_xml_string {
    my ( $text, @args ) = @_;
    return _translate_xml_string( $text, @args ) // $text;
}

sub _translate_xml_string {
    my ( $text, $file, $node, $do_blank, $report_matches, $found, $untranslated, %trs ) = @_;

    my $tr = $trs{$text};
    $found->{$text}{ $file->{file} }{$node} = ( $tr || {} )->{tr};
    if ( !$tr ) {
        saynl "unable to find translation for: '$text' in: '$file->{file}'";
        return;
    }

    return if $tr->{no_tr};

    if ( not $tr->{tr} and not $do_blank ) {
        saynl "not yet translated in: '$file->{file}': '$text'" if !$untranslated->{$text}++;
        return;
    }

    saynl sprintf "binary parse result %-26s %10s : %-60s -> %-60s", "'$file->{filename}'", $node, "'$text'", "'$tr->{tr}'" if $report_matches;
    return $tr->{tr};
}

sub handle_file_as_xml {
    my ( $do_blank, $report_matches, $found, $untranslated, $ignored, $file, %trs ) = @_;
    return if $file->{ext} ne "xml";
    return if $file->{file} !~ /StreamingAssets/;

    my %known = map +( "mst_$_.xml" => 1 ), qw( bgm bgm_jukebox furniture
      maparea mapinfo mission2 payitem payitemtext quest ship shiptext
      ship_class slotitem slotitem_equiptype stype useitem );
    return if !$known{ $file->{filename} };

    if ( $file->{filename} eq "mst_bgm.xml" ) {
        my $xml = XML::LibXML->load_xml( string => io( $file->{file} )->all );
        my @todo = $xml->findnodes("//bgm_record/text()");
        for my $i ( 0 .. $#todo ) {
            my $todo = $todo[$i];
            my ( $head, $text ) = split /,/, $todo->data;
            $text = translate_xml_string( $text, $file, $i, $do_blank, $report_matches, $found, $untranslated, %trs );
            $todo->setData( join ",", $head, $text );
        }
        store_file_as_modded( $file, 1, $xml->toString );
        return 1;
    }

    if ( $file->{filename} eq "mst_bgm_jukebox.xml" ) {
        my $xml = XML::LibXML->load_xml( string => io( $file->{file} )->all );
        my @todo = $xml->findnodes("//Jukebox_record/text()");
        for my $i ( 0 .. $#todo ) {
            my $todo = $todo[$i];
            my ( $head, @text ) = split /,/, $todo->data;
            $text[$_] = translate_xml_string( $text[$_], $file, $i, $do_blank, $report_matches, $found, $untranslated, %trs ) for 0, 1;
            $todo->setData( join ",", $head, @text );
        }
        store_file_as_modded( $file, 1, $xml->toString );
        return 1;
    }

    my %tags = map ref($_) ? $_ : "mst_$_.xml",    #
      furniture          => ["Title"],
      maparea            => ["Name"],
      mapinfo            => [qw( Infotext Name Opetext )],
      mission2           => [qw( Details Name )],
      payitem            => ["Name"],
      payitemtext        => ["Description"],
      quest              => [qw( Details Name )],
      ship               => [qw( Yomi Name )],
      ship_class         => ["Name"],
      shiptext           => [qw( Getmes Sinfo )],
      slotitem           => ["Name"],
      slotitem_equiptype => ["Name"],
      stype              => ["Name"],
      useitem            => ["Name"];

    if ( my $tags = $tags{ $file->{filename} } ) {
        my $xml = XML::LibXML->load_xml( string => io( $file->{file} )->all );
        my @todo = map $xml->findnodes("//$_/text()"), $tags->@*;
        $todo[$_]->setData( translate_xml_string( $todo[$_]->data, $file, $_, $do_blank, $report_matches, $found, $untranslated, %trs ) )    #
          for 0 .. $#todo;
        store_file_as_modded( $file, 1, $xml->toString );
        return 1;
    }

    die "how did we get here with $file->{file} ?";

    return;
}

sub run {
    `chcp 65001`;
    $|++;
    binmode STDOUT, ":encoding(UTF-8)";
    binmode STDERR, ":encoding(UTF-8)";

    my ( $opt, $usage ) = describe_options(
        'perl %c %o',
        [ 'prepare_maps|p',   "prepare mappings in both encodings for all translations before starting" ],
        [ 'do_blank|d',       "process all strings with blanked-out translations to find untranslated strings" ],
        [ 'filter_pairs|f',   "generate additional glyph pairs to find better matches (only for testing)" ],
        [ 'report_matches|m', "report all matches" ],
        [ 'report_ignored|i', "report ignored strings" ],
        [
            'bisect|b=s',
            "help drill down to problem translations by repeatedly halving the list of translations to apply. "
              . "expects a sequence of 0s and 1s, where the number indicates whether the top or bottom half "
              . "of the list is to be left over after the halving step. examples: 0, 111101, 000101110",
            { default => "" }
        ],
        [ 'verbose|v', "be a bit more chatty" ],
        [],
        [ 'help', "print usage message and exit", { shortcircuit => 1 } ],
    );
    my ( $do_blank, $filter_pairs, $bisect, $report_matches, $verbose, $report_ignored, $prepare_maps ) =
      @{$opt}{qw( do_blank filter_pairs bisect report_matches verbose report_ignored prepare_maps )};

    say $usage->text;
    exit if $opt->help;
    sleep 1;

    say "prepping dictionary";

    duplicate_check;
    my %tr = binary_translations->data;
    $tr{$_}{orig} = $_ for keys %tr;
    delete $tr{$_} for grep +( $tr{$_}{tr_tex} and !$tr{$_}{tr} ), keys %tr;
    $tr{$_}{tr} //= "" for grep !defined $tr{$_}{tr}, sort keys %tr;
    my @tr_keys = reverse sort { length $a <=> length $b } sort keys %tr;

    my @pairs = grep length $_, map split( /\|/, $_ ), map trim_nl($_), io("font_mod_character_pairs")->getlines;
    if ($filter_pairs) {
        @pairs = ( @pairs, map ucfirst, @pairs );
        say "" . @pairs;
        my $tr_body = join "#", map $tr{$_}{tr}, keys %tr;
        @pairs = grep $tr_body =~ /\Q$_\E/, @pairs;
        say "" . @pairs;
    }

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
    my $tr_in_enc = sub { tr_in_enc( @_, \%used, \%glyphmap_cache, $verbose, %mapping ) };
    if ($prepare_maps) {
        my @too_long = map add_mapped( \%tr, $_, \%used, \%glyphmap_cache, $verbose, %mapping ), "UTF-16LE", "UTF-8";
        my @unused = grep !$used{$_}, keys %mapping;
        say "following tuples unused: @unused\nfollowing tuples used: '" . ( join "|", sort keys %used ) . "'\n" if @unused;
        die "check log above, there was a translation that couldn't be matched\n" if @too_long;
    }

    if ($do_blank) {
        for my $enc ( "UTF-16LE", "UTF-8" ) {
            $tr{$_}{tr_mapped}{$enc} = "\0" x length encode $enc, $_ for keys %tr;
        }
        delete $tr{$_}{no_tr} for keys %tr;
    }

    # this converts any single string ok/skip entries into arrays, or fills in empty arrays if there's none
    for my $entry ( values %tr ) {
        $entry->{$_} = !defined $entry->{$_} ? [] : !ref $entry->{$_} ? [ $entry->{$_} ] : $entry->{$_} for qw( ok skip );
    }

    my @bisections = split //, $bisect;
    @tr_keys = half $_, @tr_keys for @bisections;
    saynl !@bisections ? () : (    #
        "bisected with path ' @bisections ' to " . @tr_keys . " translations",
        "",
        @tr_keys < 120 ? map sprintf( "% 3s: '$tr_keys[$_]' : '$tr{$tr_keys[$_]}{tr}'", $_ ), 0 .. $#tr_keys : ""
    );
    my %allowed_tr_keys = map +( $_ => 1 ), @tr_keys;
    delete $tr{$_} for grep !$allowed_tr_keys{$_}, keys %tr;

    say "grabbing file list";
    my @list = (
        utf8_asset_files("../kc_original_unpack/Media/Unity_Assets_Files/"),
        utf8_asset_files("../kc_original/Media/StreamingAssets/Xml/tables/master/"),
        {                          #
            file      => io("../kc_original/Media/Managed/Assembly-CSharp.dll"),
            filename  => "Assembly-CSharp.dll",
            fileparts => [ split /\/|\\/, "../kc_original/Media/Managed/Assembly-CSharp.dll" ],
            enc       => [ "UTF-16LE", "UTF-8" ],
            fileid    => "a-csharp",
            ext       => "dll",
        },
    );
    @list = sort { lc $a->{fileid} cmp lc $b->{fileid} } @list;
    io($_)->unlink for grep !/\.(tex|ttf)$/, io("../kc_original_unpack_modded/Media")->All_Files;
    say "prepped";
    my ( %found, %unmatched, %hit, %untranslated, %ignored );

    my @task_list = reverse sort { $a->[0] <=> $b->[0] }    #
      map +( [ length $_, $_, "UTF-16LE" ], [ length $_, $_, "UTF-8" ] ), @tr_keys;
    for my $file (@list) {
        next if handle_file_as_xml $do_blank,   $report_matches, \%found, \%untranslated, \%ignored, $file, %tr;
        next if handle_file_as_asset $do_blank, $report_matches, \%found, \%untranslated, \%ignored, $file, %tr;
        my $content = $file->{filename} ne "Assembly-CSharp.dll"    #
          ? $file->{file}->all
          : parse_csharp \( $file->{file}->all ), $tr_in_enc, $do_blank, $report_matches, \%found, \%untranslated, \%ignored, $file, %tr;
        next if $file->{filename} eq "Assembly-CSharp.dll";         # leaving this in in case we want to reenable s&r for csharp
        search_and_replace( $file, \$content, \%tr, $tr_in_enc, $do_blank, $report_matches, \%hit, \%unmatched, \%found, @task_list );
    }

    my @maybe = map sprintf( "  %-" . ( 30 - length $_ ) . "s %-30s hit x %3s, nomatch x %3s, match x %3s", $_, $tr{$_}{tr}, $hit{$_}, $unmatched{$_}, $hit{$_} - $unmatched{$_} ),
      reverse sort { length $a <=> length $b } sort keys %unmatched;
    my @nowhere = map sprintf( "  %-" . ( 30 - length $_ ) . "s $tr{$_}{tr}", "'$_'" ), grep +( !$found{$_} and !$unmatched{$_} ), @tr_keys;
    saynl " ", "strings not always identified confidently:", @maybe, " ", "strings found nowhere:", @nowhere;
    saynl " ", "strings found during parse, but ignored:", map "  '$_'", sort keys %ignored if $report_ignored;

    my @report;
    for my $jp ( sort keys %found ) {
        my $obj = $found{$jp};
        next if $obj->{no_tr};
        push @report, ( "=" x 110 ) . " [$jp] " . ( "=" x max 0, 300 - length $jp );
        for my $file ( sort keys $obj->%* ) {
            my $f = $obj->{$file};
            push @report, sprintf "|    %-90s - %10s - '%s'", "'$file'", $_, $f->{$_} // "" for sort keys $f->%*;
        }
    }
    io("report.txt")->print( join "\n", filter_nl @report );

    say "\ndone";
    return;
}

sub search_and_replace {
    my ( $file, $content, $tr, $tr_in_enc, $do_blank, $report_matches, $hits, $unmatched, $founds, @task_list ) = @_;

    my $f_enc = $file->{enc};
    my $found;
    my %encs = map +( $_ => 1 ), ( ref $f_enc ? $f_enc->@* : $f_enc );
    my @tasks = grep $encs{ $_->[2] }, @task_list;

    for my $task (@tasks) {
        my ( undef, $jp, $enc ) = $task->@*;
        my %obj = $tr->{$jp}->%*;
        next if $obj{no_tr};
        last if $enc eq "UTF-8" and $file->{filename} ne "Assembly-CSharp.dll" and decode( $enc, $content ) !~ $jp_qr;
        next unless    #
          my @hits = get_hits $content, $jp, $enc;
        for my $hit (@hits) {
            next if $file->{filename} eq "Assembly-CSharp.dll" and $hit >= 4472590 and $hit <= 4633367;
            my $file_hit = "$file->{fileid} $hit";
            next if grep $file_hit eq $_, $obj{skip}->@*;
            $hits->{$jp}++;
            if ( !grep $file_hit eq $_, $obj{ok}->@* and !check_for_null_bracketing $content, $jp, $enc, $hit, $file ) {
                $unmatched->{$jp}++;
                report_near_miss $file_hit, $hit, $enc, $jp, $content;
                next;
            }
            $founds->{$jp}{ $file->{file} }{$hit} = $obj{tr};
            next if !$do_blank and !length $obj{tr};
            report_near_miss $file_hit, $hit, $enc, $jp, $content, "is_a_hit" if $report_matches;
            substr( $content, $hit, length $_ ) = $_ for $tr_in_enc->( \%obj, $enc );
            $found++;
        }
    }
    store_file_as_modded $file, $found, $content;
    return;
}
