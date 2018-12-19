package binary_translations;
use strictures 2;
use utf8;

1;

sub data {
    return (
        "選択" => {
            tr   => "Select",
            desc => "explains numpad (the skips should be rechecked probably)",
            ok   => 'a-csharp 4562125',
            skip => [
                map "a-csharp $_", qw( 4487986 4495884 4539834 4539862 4539876 4539906 4548242 4548242 4548404 4550010 4550048 4550486 4552806
                  4562095 4562119 4495298 4563095 4563131 4563513 4564079 4566677 4570003 4595855 4595927 4596751 4596779
                  4596809 4596951 4598553 4598719 4598751 4633240 4698799 4698860 4698896 4699107 4699472 )
            ]
        },
        "提督コマンド" => {
            tr   => "Admiral Command",                                                       # quick menu?
            desc => "usually opens quick menu or changes the buttons to an alternate set",
            ok   => [ 'a-csharp 4518854', 'a-csharp 4682604' ]
        },
        "戦略へ" => {
            tr   => "Strategy",
            desc => "leads to fleet strategy management screen",
            ok   => [ 'level22/level22_00034.-4 196', 'sharedassets5/sharedassets5_00617.-4 196', 'a-csharp 4518868' ],
        },
        "決定" => { tr => "Choose", desc => "select the currently chosen option", ok => 'a-csharp 4518802' },
        "戻る" => { tr => "Back",   desc => "back out of the current screen",     ok => 'a-csharp 4518826', skip => [ 'a-csharp 4566833', 'a-csharp 4682617' ] },
        "戻る　　決定"             => { tr => "Back  Choose", },
        "選択　　戻る　　決定" => { tr => "Select  Back  Choose", },

        "作戦難易度を選択してください" => { tr => "Please select strategy difficulty level", },
        "タイトルに戻る"                      => { tr => "Back to Title", desc => "name choosing menu", ok => 'a-csharp 4566823' },
        "提督名入力" => { tr => "Admiral Name Entry", desc => "name choosing menu title, not sure if this one is used", ok => [ 'a-csharp 4566487', 'a-csharp 4699444' ] },

        "  貴官の提督名をお知らせ下さい。(" => { tr => "Please choose your Admiral's name. (Up to ", ok => 'a-csharp 4566537', desc => "TODO: verify the space at the start" },
        "文字まで入力可能です)"                  => { tr => " characters can be entered.)",               ok => 'a-csharp 4566575' },
        "貴官の提督名をお知らせ下さい。\n（１２文字まで入力可能です）" => {
            tr => "Please choose your Admiral's name.\n(Up to 12 characters can be entered.)",
            ok => 'level5/level5_00004.-6 196'
        },
        "ゲーム開始" => { tr => "Game Starters", ok => [ 'a-csharp 4566659', 'a-csharp 4699455' ], desc => "title of game starter set selection screen" },
        "初期艦選択" => {
            tr => "Starter Ship Selection",
            ok => [
                'a-csharp 4566671',
                'a-csharp 4699466',    # unsure about this one
            ],
            desc => "not sure, think title of the screen after the set"
        },
        "チュートリアル" => {
            tr => "Tutorial",
            ok => [
                'a-csharp 4566683',
                'a-csharp 4699477',    # unsure about this one
            ]
        },

        "配備輸送船数" => { tr => "Transports",    desc => "Number of deployed shipping vessels, Cargo ships in area" },
        "予想獲得資源" => { tr => "Expected Gain", desc => "Expected acquisition resources (Daily Resource Generation) Fuel, Ammo, Steel, Bauxite per map hex" },
        "海上護衛艦隊" => { tr => "Escort fleet",  desc => "Maritime Escort Fleet", skip => [ 'a-csharp 4604319', 'a-csharp 4604361' ] },                           #  part of larger strings
        "回航中"          => { tr => "At Sea",        desc => "map screen, tankers at sea, apparently, but also used elsewhere" },
        "未配備"          => { tr => "Undeployed",    desc => "Undeployed - one char too long, really need to add glyph aliasing here too" },

        # 艦隊   状態   艦種   艦船名    Lv    耐久   火力   対空   速カ
        # ship table headers
        "艦隊"    => { tr => "Fleet",     desc => "Fleet Number. Defines which fleet the ship is assigned to. try with adding space at begin for formatting. further adjustements pending results." },
        "状態"    => { tr => "State",     desc => "Status/Condition of current fleet (red is has acted, blue is on expedition, blue is (maybe) 'on deployment', and blue is at sea again)" },
        "艦種"    => { tr => "Class",     desc => "Ship Class - maybe 'Hull'" },
        "艦船名" => { tr => "Ship Name", desc => 'prefer "Ship Name" over "name" - requires font substitution.' },
        # Lv - Obviously no need to translate
        "耐久" => {
            tr   => "Health",
            desc => 'Use Health for clarity as it is already used in other locations. is easier to use "Health" if we can resize it slightly than it would be for changing all the previous uses.'
        },
        "火力" => { tr => "ATK",   desc => "Firepower or ATK, I am still shying from FP or ATK though - Seriosuly this should be easy. The Kanji is literally Fire and Power" },
        "対空" => { tr => "AA",    desc => "Anti-air or Air Defense" },
        "速力" => { tr => "Speed", desc => "Try Speed instead of SPD as I would like to check table formatting." },
        #[1:06 AM] Knofbep: the damage shows up 2nd to last column
        #[1:06 AM] Knofbep: next to the heart lock
        # https://cdn.discordapp.com/attachments/235919493686231051/522193240510693396/2018-12-11-182050.png
        # https://cdn.discordapp.com/attachments/235919493686231051/522203328923435018/2017-05-02-221035.jpg

        # this.Title.text = monthName.Length != 2 ? "新しい月、　　　　となりました！" : "新しい月、　　　となりました！";
        "新しい月、　　　　となりました！" =>
          { tr => "A new month,　　　　, is here!", ok => 'a-csharp 4572711', desc => "the month is rendered as a separate string into the gap" },
        "新しい月、　　　となりました！" => { tr => "A new month,　　　, is here!", ok   => 'a-csharp 4572679', desc => "the month is rendered as a separate string into the gap" },
        "新しい週になりました！"             => { tr => "A new week is here!",             desc => "weekly resource bonus" },

        "ターンを終了しますか？" => { tr => "Do you want to end your turn?", desc => "Quick Turn End by hitting Square - performed on Operations Screen." },
        "ターン終了" => { tr => "End of Turn", desc => "Quick Turn End, hit Square to confirm", skip => [ 'a-csharp 4573257', 'a-csharp 4604171' ] },    # skips part of larger strings
        "[12c112]ターン終了[-]" => { tr => "[12c112]End of Turn[-]", desc => "Quick Turn End, hit Square to confirm" },

        # Enemy Raiding
        # these are embedded in sharedassets5/Atlas_RaderHukidashi.tex
        # 護衛艦損傷！        - Escort ship damage! #
        # 輸送船×1隻喪失！    - Transport ship x 1 lost!
        # 輸送船×13隻喪失！   - Transport ship x 13 lost!, example 2
        "護衛艦" => { tr => "Escort",    desc => "could use the word 'ship' in there, but too long, need font replacement" },
        "輸送船" => { tr => "Transport", desc => "could use the word 'ship' in there, but too long, need font replacement" },

        # Expedition completion screen (these are probably art)
        # 獲得ボーナス        - Acquisition bonus # can't find as text
        # 海域EXP            - Marine area EXP # can't find as text
        # （基本経験値）       -(Basic experience value) # only as part of a longer text in Assembly-CSharp\local\models\battle\BattleResultModel.cs
        "獲得アイテム" => { tr => "Earned items", desc => "followed by icons and numbers which don't need translation" },

        "敵艦隊"             => { desc => "" },

        # https://www.thoughtco.com/can-you-tell-me-the-old-names-of-the-months-2027868
        #this.monthFormat.MonthNames = new string[13]
        #{ "睦月", "如月", "弥生", "卯月", "皐月", "水無月", "文月", "葉月", "長月", "神無月", "霜月", "師走", string.Empty };
        "睦月"    => { tr => "Jan", ok => 'a-csharp 4594475' },
        "如月"    => { tr => "Feb", ok => 'a-csharp 4594481' },
        "弥生"    => { tr => "Mar", ok => 'a-csharp 4594487' },
        "卯月"    => { tr => "Apr", ok => 'a-csharp 4594493' },
        "皐月"    => { tr => "May", ok => 'a-csharp 4594499' },
        "水無月" => { tr => "Jun", ok => 'a-csharp 4594505' },
        "文月"    => { tr => "Jul", ok => 'a-csharp 4594513' },
        "葉月"    => { tr => "Aug", ok => 'a-csharp 4594519' },
        "長月"    => { tr => "Sep", ok => 'a-csharp 4594525' },
        "神無月" => { tr => "Oct", ok => 'a-csharp 4594531' },
        "霜月"    => { tr => "Nov", ok => 'a-csharp 4594539' },
        "師走"    => { tr => "Dec", ok => 'a-csharp 4594545' },

        #this.yearFormat = new Dictionary<string, string>()
        #{ {"0","零"},{"1","壱"},{"2","弐"},{"3","参"},{"4","肆"},{"5","伍"},{"6","陸"},{"7","質"},{"8","捌"},{"9","玖"},{"10","拾"} };
        "零" => { tr => "0",  ok => 'a-csharp 4594551' },
        "壱" => { tr => "1",  ok => 'a-csharp 4594555' },
        "弐" => { tr => "2",  ok => 'a-csharp 4594559' },
        "参" => { tr => "3",  ok => 'a-csharp 4594563' },
        "肆" => { tr => "4",  ok => 'a-csharp 4594567' },
        "伍" => { tr => "5",  ok => 'a-csharp 4594571' },
        "陸" => { tr => "6",  ok => 'a-csharp 4594575' },
        "質" => { tr => "7",  ok => 'a-csharp 4594579' },
        "捌" => { tr => "8",  ok => 'a-csharp 4594583' },
        "玖" => { tr => "9",  ok => 'a-csharp 4594587' },
        "拾" => { tr => "10", ok => 'a-csharp 4594597' },

        "獲得アイテム:{0}" => { tr => "Earned items:{0}", ok => 'a-csharp 4601141', desc => "tbh not sure where exactly this is" },

        # Map screen
        "どこに進む？" => {
            tr   => "Where to now?",
            ok   => [ 'a-csharp 4564133', 'a-csharp 4698584' ],         # i am VERY unsure about these hits, they'll require verification
            desc => q["Where do you want to go?" or " Where to now?"]
        },

        # Battle formation screen
        "陣形を選択してください。" => { tr => "Please select the formation.", ok => [ 'a-csharp 4564073', 'a-csharp 4698793' ] },
        "単縦陣"                            => { tr => "Line Ahead",                   ok => 'a-csharp 4501426' },
        "複縦陣"                            => { tr => "Double Line",                  ok => 'a-csharp 4501434' },
        "輪形陣"                            => { tr => "Diamond",                      ok => 'a-csharp 4501442' },
        "梯形陣"                            => { tr => "Echelon",                      ok => 'a-csharp 4501450' },
        "単横陣" =>
          { tr => "Line\nAbreast", ok => 'a-csharp 4501458', desc => "these are all the accepted translations already. so yes. See if you can do a carriage return after Line ina all of these." },

        "獲得可能資材" => { tr => "Materials Available",      desc => '[OC] that means out of context and i reserve the right to edit later.' },
        "残り輸送船数" => { tr => "Ships Remaining",          desc => '[OC]' },
        "必要輸送船数" => { tr => "Required Number of Ships", desc => '[OC]', skip => 'a-csharp 4603172', },                                       # part of a larger string

        # Compass fairy with magic hat
        # https://cdn.discordapp.com/attachments/235919493686231051/523259704890097675/2018-12-05-071704.png
        "らしんばんをまわしてね！" => {
            tr   => "Spin the compass please!",
            ok   => 'a-csharp 4564621',
            desc => "a compass fairy text. these are in csharp, but the decompiler doesn't see them. maybe these could default to a cute way of speaking?"
        },
        # Compass fairy with bob haircut
        # https://cdn.discordapp.com/attachments/235919493686231051/523259707331313713/2018-12-05-102944.png
        "ここっ"                                     => { tr => "Here!",                           desc => 'but with exertion', ok => 'a-csharp 4564761' },
        "よーし、らしんばんまわすよー！" => { tr => "Alright, I'll spin the compass!", ok   => 'a-csharp 4564557' },
        "えいっ"                                     => {
            tr   => "Ey!",
            ok   => 'a-csharp 4564647',
            desc => ''
              . q[it is a sound uttered/made when someone is exerting force, or throwing something. you would be best ignoring the "っ" for now. It is not a tsu. "つ" is. I am nt reallt set to explain sokuon and glottal stops.]
        },
        "それっ" => {
            tr   => "Take that!",
            ok   => 'a-csharp 4564655',
            desc => q["Take That!" or "That!" - can also use "This". remember implied words. once more with the っ. (i think it could also be "There!")]
        },
        # Compass fairy with chick on head
        # https://cdn.discordapp.com/attachments/235919493686231051/523259713480163328/2018-12-13-232523.png
        "はやくはやくー！" =>
          { tr => "Hurry! Hurry!", desc => q[i may localize it a little better. "C'mon! Hurry up!" alternate, more localized. Kind of like it myself.], ok => 'a-csharp 4564539' },
        "えいえいえーいっ" => {
            tr   => "Here!\x{200B}\x{200B}\x{200B}",
            desc => 'I need to see it in use for flavour. Knofbath translation does not feel right at this time. since the tr was VERY short i added zero width spaces',
            ok   => 'a-csharp 4564885'
        },
        "とまれーっ" => { tr => "And Another One", desc => 'i will come back to this one. more context needed.', ok => 'a-csharp 4565001' },
        # Compass fairy who is sleepy
        # https://cdn.discordapp.com/attachments/235919493686231051/523259716009328658/2018-12-14-010022.png
        "えー？らしんばん、まわすのー？" => {
            tr   => "Uh? You want me to spin the compass?",
            desc => 'or "Spin The Compass Please" - this is extremely localized. for a more literal use, "What? Spin the compass." or maybe, "What? Spin The Compass Already."',
            ok   => 'a-csharp 4564589'
        },
        # https://cdn.discordapp.com/attachments/235919493686231051/523021829737283585/2018-12-14-010344.png
        "……ん" =>
          { tr => "...un!", desc => 'grunt like sound - [Previous is Depriciated - and context matters : "Huh?" -  Literally "....n." - Maybe use "Wha?" [t intentioanlly omitted]]', ok => 'a-csharp 4564867' },
        # https://cdn.discordapp.com/attachments/235919493686231051/523021901270876172/2018-12-14-010056.png
        "……あい" => { tr => "...huhn", desc => 'Literally "...ai" - Or, maybe she is just making a sound. I am going to play with this some.', ok => 'a-csharp 4564875' },

        "艦隊の針路を選択できます。" =>
          { tr => "Plot the fleet's course.", desc => "todo: verify if this and the next string are one unit", ok => [ 'a-csharp 4563083', 'a-csharp 4698848' ] },
        "提督、どちらの針路を選択しますか？" =>
          { tr => "Admiral, pick a course?", desc => 'could be "Admiral, which course do you prefer?"', ok => [ 'a-csharp 4563111', 'a-csharp 4698876' ] },

        "この艦隊で出撃しますか？" => { tr => "Sortie this fleet?", ok => [ 'a-csharp 4567317', 'sharedassets5/sharedassets5_00551.-4 196', 'level4/level4_00309.-15 196' ] },
        "※艦娘保有数が上限に近いため、新しい艦娘と邂逅できない可能性があります。" =>
          { tr => "※If you have too many Ship Girls, there is a possibility of not getting a Ship Girl reward.", desc => 'Edited for consistency.', ok => "a-csharp 4567477" },
        "※艦娘保有数が上限に達しているため、新しい艦娘との邂逅はできません。" =>
          { tr => "※You have reached your limit of Ship Girls and can not accept your reward.", desc => 'red, ships full', ok => 'a-csharp 4567407' },

        # https://cdn.discordapp.com/attachments/235919493686231051/523300790283272234/2018-12-14-194114.png
        # I want to point out, the symbol ※ is used as a Bullet Point Mark.
        # Example: https://i.imgur.com/Z6yQfbF.png

        # https://cdn.discordapp.com/attachments/235919493686231051/523030725616992267/2018-12-14-005011.png
        "補給してよろしいですか？" => { tr => "Resupply with the following?", desc => 'this is more localized.', ok => 'sharedassets5/sharedassets5_01265.-4 196' },
        "必要燃料数"                      => { tr => "Fuel Required",                desc => 'maybe "Needed"?' },
        "必要弾薬数"                      => {
            tr   => "Ammunition Required",
            desc => 'maybe "Needed". If after testing "Ammunition Required" does not fit well, switch to "Ammo Required". I would prefer to not be sloppy if we do not have to be though.'
        },

        "接近"                    => { tr => "Approach" },
        "離脱"                    => { tr => "Escape", desc => 'Withdraw, Escape or Retreat' },
        "航空攻撃"              => { tr => "Aerial Attack", desc => 'Air Strike' },
        "砲撃"                    => { tr => "Shelling", desc => 'Bombard could work as well. ' },
        "対潜攻撃"              => { tr => "ASW\x{200B}", desc => 'Anti-submarine attack or Anti-submarine warfare(ASW)' },
        "突撃（接近+砲撃）" => { tr => "Assault (Approach + Shelling)", desc => 'Charge, increase accuracy while decreasing evasion' },
        "雷撃"                    => { tr => "Torpedo", desc => 'can be opener and closer if ships have right equipment or are high-level subs' },
        "回避"                    => { tr => "Evade", desc => 'no attack, Evasion' },
        "統射" => {
            tr   => "Coordinated Shelling",
            desc => 'Tough one. It literally breaks down into Unified Archery. For now, I am going to go with Coordinated Shelling to see how it fits - Radar-coordinated Shelling'
        },
        #           ** I have updated these to reflect \resources\Textures\info6_set.tex.png ** these are final translations between 接近 and 統射

        "制空権確保" => { tr => "Air Secured",      desc => 'Air Supremacy or Air Superiority Ensured' },
        "航空優勢"    => { tr => "Air Advantage",    desc => 'Air Superiority' },
        "制空権喪失" => { tr => "Air Disadvantage", desc => 'Air Incapability or Air Superiority Lost' },

        "装備数が保有上限に達し開発できません" =>
          { tr => "Equipment storage full, can not develop any more.", desc => 'warning message when constructing ships', ok => 'a-csharp 4501118' },
        "装備の保有上限に達しています" => { tr => "Equipment storage full.", desc => 'warning message when claiming ships', ok => 'a-csharp 4501184' },
        "艦が保有上限に達し建造できません" =>
          { tr => "Ship Girl storage full, can not construct any more.", desc => 'warning message when constructing ships', ok => 'a-csharp 4501084' },

        #"廃棄は □ボタンで行います",                      #"Discarding is done with □ button" 
        #"解体は □ボタンで行います",                      #"Demolition is done with □ button" 
        #"ボタンで戦略コマンドを\n開き、ターンを終了せよ！",  #"Open the Strategy map with the button, and End the Turn!"
        #"ボタンで戦略コマンド\nを開き、艦隊出撃せよ！",     #"Open the Strategy map with the button, and Sortie the Fleet!"
        #"ボタンで\n旗艦提督室へ移動せよ！",               #"Move to the Admiral's Room with the button!

        #"旗艦提督室で\n任務達成を\r確認せよ！\n",         #"Check your Quests in the Admiral's Room!"
        #"略ポイント[-]は、任務達成及び\n海域攻略などで手に入ります。\n\n[0055aa]工" => { tr => q[eaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaea], },
        #"0000][0055aa]戦略ポイント[-]は、任務達成及び\n海域攻略などで 手に入ります。\n\n[0055aa]工廠[-]での輸送船建造や、\r\n[0055aa]アイテム屋さん[-]でのアイテム購入で\n使用します" => { tr => q[eaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaeaaeaeaeaeaeaeaeaea], },

        # "廃棄は □ボタンで行います" =>  the square is a special char. not sure if it belongs in the string or was a separator
        # "解体は □ボタンで行います" =>  the square is a special char. not sure if it belongs in the string or was a separator

        # "{0}(この海域への移動中:{1}) -" some kinda long string - "{0} (Moving to this area: {1}_ -seems self explanatory, there is probably a trailing ) you missed

        # 戦略ポイントが不足しています    "Strategy points are missing"                 probably the shop error when you don't have enough points to buy
        # 該当装備がロックされています    "Equipment is locked"                             self-explanatory again

        "被弾回避率補正" => {  ok => 'a-csharp 4556831' },  #"Evasion adjustment"                literal hit avoidance rate correction
        "攻撃命中率補正" => { },                            #"Attack Hit% adjustment"            literal attack hit ratio correction
        "雷撃命中率補正" => {  ok => 'a-csharp 4556847' },  #"Torpedo Hit% adjustment"           literal lightning shot hit ratio correction, but lightning shot(雷撃) is torpedo
        "海上護衛艦隊の対潜/対空能力:{0}" => { },             #"Escort Fleet ASW/AA skill: {0}      for raiding defense I think
        "海上護衛艦隊(MemId:{0})は{1}のダメージ({2})" => {ok => 'a-csharp 4604361' }, #"Escort Fleet {0} deals {1} damage {2}"           ???
        "海上護衛艦隊名海上護衛艦隊名" => { },                #"Escort Fleet Name Escort Fleet Name"       ???
        "[000000][0055aa]海上護衛艦隊[-]の配備について" => { }, #"Escort Fleet about deployment"           ???

        "廃棄は □ボタンで行います" => {ok => 'a-csharp 4517038'}, #"Discarding is done with □ button"       delete equipment with square
        "解体は □ボタンで行います" => {ok => 'a-csharp 4517066'}, #"Demolition is done with □ button"       delete ships with square
        "ボタンで戦略コマンドを" => {},                      #"Strategy command with button"          ???  figured it out, this string is missing 2 kanji ボタンで戦略コマンドを\n開き、
        "輸送船団や海上護衛艦隊への" => {},                   #"To the Transports and Escort Fleet"      ??? tutorial to show you where to assign transports/escort fleet?
        "建造日数が掛かります" => {},                        # "Construction days it will take"              ??? tutorial about building ships? alternate "Days construction will take"
        "海域の輸送船団に海上護衛艦隊を配備することで、\r\n敵通商破壊部隊から輸送船を護ることが可能です。\n\nまた" => {},
        # "By deploying an escort fleet to a transportation fleet in an area, \r\n it is possible to protect ships from an enemy raiding unit. \ N \ n"
        "備の輸送船数:{0}(この海域への移動中:{1}) - 総数:{2}" => {}, #"Number of Transports:{0} (moving to this area: {1})  - Total number: {2} "
        "輸送船 x {0}" => {},                             #"Transports x {0}" 
        "ロック済艦娘を" => {},                            #"Shipgirl is locked" literal "Locked ship daughter" alternate "Fleet Girl is locked"
        "艦隊司令部情報" => {},                            #"Fleet Headquarters information"
        "艦隊切替" => {},                                 #"Fleet switch" or maybe "Switch fleet" depending on context
        "艦隊数" => {},                                   #"Fleet number"
        "ボタンで戦略コマンド" => {},                       #" "Strategy command with button"
        "[000000][0055aa]輸送船[-]の配備について" => {},    #"About deployment of Transports" alternate "About Transport deployment"
        "工廠[-]での輸送船建造や、" => {},                  #"Factory[-]Building a Transport ship at," alternate "Building a Transport ship at the Factory" color codes missing from string would go around Factory
        "[66ccff]全艦種出撃可能[-]です。"=>{},              #"All ship types can sortie." alternate with color codes "[66ccff]All ship types[-] can sortie."
        #"のみ[66ccff]出撃可能[-]です" => {},              #"Only listed ship types can sortie."  literal "Only sortie is possible."
        #"は[missing color red]出撃不可[-]です" => {},     #"These ship types unable to sortie." literal "Sortie is impossible."
        "ボタンで戦略コマンドを\n開き、ターンを終了せよ！"=>{ok=> 'resources/resources_01107.-10 199'},
        #"Open the strategy command with the button, and end the turn!"
        "迎撃戦は、[0055aa]連合艦隊[-]で出撃可能です。\n[0055aa]連合艦隊[-]は、\r\n" => {},
        #"Interception, sortie is possible with an [0055aa]Allied Fleet[-].\n[0055aa]Allied Fleet[-] is, \r\n"
        "Inspectorで設定して使用" => {},
        "Inspector上でイベントを" => {},
        "Inspector上でボタンに設" => {},
        "Inspector上で使用します" => {},
        "Inspector上で使用するメ" => {},
        "Inspector上で設定して使" => {},
        "Inspector上で選択して使" => {},
        "Inspector上に設定して使" => {},
    );
}
