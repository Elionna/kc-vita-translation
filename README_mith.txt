=head1 TODO THINGS

- Operations button menu untranslated.
- Options menu untranslated.
- operations and strategy screens have a bunch of untranslated texts
- some textures have weird red background (dock empty message) or
  pink (formations in battle, Diamond, Double Line)
- Strategy is too wide. Choose is too wide.
- extend the dictionaries for utf8+16 translation and give them more options and
  logic
- speed up application of dictionaries to binary files by cleaning out the
  unpack dir and using cygwin/find to prefilter more
- add a flag to the dictionary so strings used multiple times are numbered in
  patch creation so they can be matched against the output of the patch builder
  to find which ingame string is in which data file
- extract japanese strings for the dictionaries from the original files
- create a dictionary for xml translations, so english XMLs can be built from JP
- combine all dictionaries
- maybe set up a way to generate xdelta patches so the distributed translation
  patch can be smaller
- figure out certain strings that are in csharp.dll, but aren't exposed by the
  decompiler, like compass fairy strings: "ばんをまわしてね"
- on the "repair costs" screen the number of days it takes to repair overlaps
  the "days" character

wtf is a monobehavior, is the file format described? found mention that it's a
base class of unity objects and the exact type is clarified by the ids. needs
more research.

=head1 VISUAL TEXT RESTRICTIONS

quest descriptions have space for 4 lines, seem to need newlines as they break
below ui elements otherwise,
fit about this much: "Have 2 ships in your main fleet. Mmmmmmm mmm"

ship greeting texts have space for 4 lines and auto-break,
fit about this much: "mmm mmmmmm mmmmmm mmmmm mmmmmm", 630 px

I have a small script that can calculate the width of a string in a given font.

The font for the bottom pop up menu was too wide, so i replace it during patch
building with one that's more narrow and fits more characters.

=head1 TEXTURES

These are a bit annoying as a dictionary approach won't work well there. Maybe
at least transcribe the texts into a glossary?

In order to avoid degradation by way of DDS compression we use UnityTexTool to
convert the raw unityex *.tex files directly into PNG and back.

=head1 ASSEMBLY-CSHARP.DLL

Contains mostly strings in UCS-2 LE. But however a FEW strings (enums) are also
UTF8. No idea if the latter show up ingame.

There's a bunch of tools to inspect this, and even a bunch that can compile a
dll again, but none of these that i tried were able to recompile the dll into
something that doesn't crash the vita, so i went the way of binary modding the
dll.

As for simply unpacking it, Jetbrains C# Decompiler seems to be the best option
and results in perfectly readable C# code.

Sadly due to the strings being in UTF16 and binary modding not allowing changes
to the byte length, it's necessary to mod the fonts in order to show more
characters per byte than usually possible.

A-OTF-ShinGoPro-Regular.ttf in assets 2, used for quest name texts, ship greetings, ...
A-OTF-UDShinGoPro-Regular.ttf in assets 3, used for the popup button guide at the bottom, ...
(there's more fonts)

online font tester: http://torinak.com/font/lsfont.html

For modding fonts i use FontForge. When using it there might be an error window
in the taskbar but not actually visible, click on that and hit esc once.

=head1 BUILDING THE PATCH

The following stuff doesn't need to be done everytime, it's just preparation.

- put unityex in ../unity_tools
- put the decrypted game in ../kc_original
  - decrypt on console: https://github.com/TheRadziu/NoNpDRM-modding/wiki#obtaining-decrypted-game-assets-from-nonpdrm-rips-through-vitashell-decryption-on-console
  - decrypt on pc: https://github.com/TheRadziu/NoNpDRM-modding/wiki#obtaining-decrypted-game-assets-from-nonpdrm-rips-through-psvpfstools-decryption-on-pc
- # perl unpack_original_files.pl
- note that this deletes many files in kc_original_unpack that aren't needed now
  and slow things down

- edit any of the following files to add new japanese strings or translations
  - binary_translations.pm for translations in utf8 strings in asset files
  - en/Xml/* for bulk texts in raw xml
  - en/Unity_Assets_Files for images
- to add translated images the *.tex files in kc_original_unpack will need to be
  converted to PNG with UnityTexTool. when put in the right directory the patch
  builder will pick them up on its own

- # perl _build_translation_patch.pl

This script then will do all of this:

It attempts to replace strings in unpacked binary files from the units asset
files.

It attempts to replace strings in Assembly-CSharp.dll with their translations.
It might complain, depending on whether the translation dictionary has changed,
that tuples are missing. Add them judiciously to the font_mod_character_pairs
file until it stops complaining. Tuples can probably be very long. The
translation will be padded if more bytes are necessary. However shorter tuples
might be useful for reusing things. (Then again we have 5000 glyphs available.)

Then it sets up the fonts to prepare them for injecting character tuples:

- Directories are created with copies of the fonts that contain 5000+
  prepared glyphs in the first "Private Use Area".
- Then copies of these directories are made and multi-character strings injected
  into the prepared glyphs according to font_mod_character_pairs.
- After that those mod directories are converted into fonts which are placed in
  the appropriate translation candidate sub-directories.

(The script caches font directories in ../fonts to speed things up. If you
suspect something went wrong, deleting either of them will cause them to be
rebuilt.)

Then it injects the fonts and other asset files from the directory
kc_original_unpack_modded into asset files in kc_translation_mod_candidate.

And lastly it copies everything onto the vita mounted at E: and warns about
extraneous files there.

=head1 BUILDING A PATCH UPDATER

Since the patch itself will be pretty big, it's nice for working with others to
have a small patcher to apply incremental changes without downloading the whole
thing.

To do this i chose Download Inno Setup XDELTA Patch Maker 2.5.6.7, which is only
available from Softpedia because the russian author has no own site, but seems
to work fairly ok.

https://www.softpedia.com/get/Programming/Patchers/Inno-Setup-XDELTA-Patch-Maker.shtml

Preparation: Have a previously known version of the translation patch unpacked
in a directory; and have a release candidate built with a version number
assigned.

- run ISXPM
- click "load project" in the bottom right, and load
  ISXPM.Patcher.KancolleKai/ISXPM.Patcher.KancolleKai.ini
- (note that if you save now, it'll go into the projects dir of ISXPM. to get
  config changes into the repo the ini needs to be copied manually)
- paste the path to the previous version in the "old version" field
- paste the path to the release candidate in the "final version" field
- update the "patch name" field to indicate from which ver to which ver it goes
- adjust the "patch output" field to set where the exe is saved
- click "build patch"
