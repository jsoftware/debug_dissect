NB. Copyright (c) Henry H. Rich, 2012-2017.  All rights reserved.

locales =. 'dissect'&,&.> ('' ; ;: 'obj extendv monad dyad recursionpoint noun verb assign vandnm vandnmdyad fork hook allnouns righttoleft irregularops fitok powerexpansion insertexpansion adverseexpansion displaytwo selectshape each') , 'partition'&,&.> ''; ;: 'selector adverb conjunction'
NB. Clear definitions of old locales and create anew.  This will remove hangover definitions. These locales can be small since they hold mostly verb-names
NB. The 2 1 gives the name-table sizes: 2 1 0 0 0 ...
NB. The dissectionlist thing is to preserve the list over reloads, for debugging.  This is also its initialization
NB. Don't delete a locale that we have switched to, to prevent interaction unpleasantness
NB. 10 10 here is the starting position of the first window
3 : 'dissectionlist_dissect_ =: d [ ((cocreate ([ coerase))"0~   2 1 {.~ #) y [ d =. (,:($0);10 10)&[^:(0=#) ".''dissectionlist_dissect_''' (coname'') -.~ locales

NB. DISSECTLEVEL is updated from time to time whenever there is a change to an external interface, indicating the dissect release level
NB. at the time of the change
DISSECTLEVEL_dissect_ =: 4 0
NB. CONFIGFILELEVEL is the current EC level of the config file
CONFIGFILELEVEL_dissect_ =: 2

NB. set ALLOWNONQTTOOLTIP to enable tooltips for J6 (they are always on in JQT).  In J6 tooltips
NB. take over the timer interrupt
ALLOWNONQTTOOLTIP_dissect_ =: 1

NB. if any of the debugging switches is turned on, printf is required
NOCLEANUP_dissect_ =: 0  NB. set to 1 for debugging to allow postmortem
DEBPARSE_dissect_ =: 0   NB. set for parser printout
DEBTRAVDOWN_dissect_ =: 0   NB. set for travdown printout
DEBNOCATCH_dissect_ =: 0  NB. Set to allow debug to catch internal errors
DEBHLIGHT_dissect_ =: 0   NB. set for highlight printout
DEBSELECT_dissect_ =: 0   NB. set for selection printout incl opselin
DEBHLIGHT2_dissect_ =: 0   NB. set for highlight printout - pixel details
DEBVERB_dissect_ =: 0   NB. set for travdown printout
DEBLAYOUT_dissect_ =: 0   NB. display grid details
DEBROUTE_dissect_ =: 0   NB. display routing details
DEBGRAF_dissect_ =: 0   NB. display all drawn graphics
DEBOBJ_dissect_ =: 0  NB. display drawn-object details
DEBDOL_dissect_ =: 0  NB. display drawing details
DEBINHU_dissect_ =: 0 NB. display inheritu
DEBDOL2_dissect_ =: 0  NB. display drawing locales
DEBDOvn_dissect_ =: 0  NB. display object headers
DEBPICK_dissect_ =: 0  NB. display pick progress
DEBEXEGESIS_dissect_ =: 0  NB. display exegetic creation
DEBMOUSE_dissect_ =: 0   NB. display mouse events
DEBTIME_dissect_ =: 0  NB. Show elapsed times
DEBSCROLL_dissect_ =: 0  NB. Show scroll status
DEBROUTETABLES_dissect_ =: 0  NB. Audit routing tables for consistency
DEBSENTENCELOG_dissect_ =: 0  NB. Write sentence to file
SENTENCELOGFILE_dissect_ =: <'C:/J8/dissectlog.txt'
QP_dissect_ =: qprintf
SM_dissect_ =: smoutput
PR_dissect_ =: printf   NB. So this code will lint with printf undefined
VB_dissect_ =: vbsprintf
edisp_dissect_ =: 3 : '(":errorcode) , ''('' , (errorcodenames{::~2+errorcode) , '')'''

0 : 0
alltests_dissect_''
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
testsandbox_dissect_ 1
(3 ;< 'check';'no') testsandbox_dissect_ 2
)

alltests_dissect_ =: 3 : 0
stime =. 6!:1''
dly =: 10   NB. Time to recover from tests?
config_displayautoexpand2_dissect_ =: 0
displayshowcompmods_dissect_ =: 0
config_displayshowfillcalc_dissect_ =: 0
config_displayshowstealth_dissect_ =: 0
('Step 0:',LF) 1!:2 SENTENCELOGFILE_dissect_
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
'Time after step 0: %4.1f sec' PR_dissect_ stime -~ 6!:1''
6!:3 dly
config_displayautoexpand2_dissect_ =: 1
('Step 1:',LF) 1!:2 SENTENCELOGFILE_dissect_
0!:2 ; <@('/'&e. # LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
'Time after step 1: %4.1f sec' PR_dissect_ stime -~ 6!:1''
6!:3 dly
config_displayautoexpand2_dissect_ =: 0
displayshowcompmods_dissect_ =: 1
('Step 2:',LF) 1!:2 SENTENCELOGFILE_dissect_
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
'Time after step 2: %4.1f sec' PR_dissect_ stime -~ 6!:1''
6!:3 dly
displayshowcompmods_dissect_ =: 0
config_displayshowfillcalc_dissect_ =: 1
('Step 3:',LF) 1!:2 SENTENCELOGFILE_dissect_
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 ; ((#~  +./\ *. +./\.) ('$FILL$' +./@:E. ])@>) <;.2 runtests_dissect_
'Time after step 3: %4.1f sec' PR_dissect_ stime -~ 6!:1''
6!:3 dly
config_displayshowfillcalc_dissect_ =: 0
config_displayshowstealth_dissect_ =: 1
('Step 4:',LF) 1!:2 SENTENCELOGFILE_dissect_
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_resize__y 0''^:(''''-:$) ' ,^:('dissect' +./@:E. ]) [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
'Time after step 4: %4.1f sec' PR_dissect_ stime -~ 6!:1''
config_displayshowstealth_dissect_ =: 0
)

0 : 0
traversebsp =: #@] }. (( (] , {:@[ + ({~ {.)~ <!.0 (1 { [))^:(0 <: {:@])~   ({~ {:) )^:_    ,&0.)^:(*@#@[)

bsptest =: ".;._2 (0 : 0)
0 1 2  NB. 0 coord0 > 1, 2 3
0 0 0
1 2 4  NB. 2 coord1 > 2, 4 5
0 0 _1  NB. 3 leaf
0 4 6  NB. 4 coord0 > 4, 6 7
1 6 8  NB. 5 coord0 > 6, 8 9
0 10 10  NB. 6 coord0>10, 10 11
0 0 _1  NB. 7 leaf
0 5 12  NB. 8 coord1>5, 12 13
0 0 _1  NB. 9 leaf
0 0 _1  NB. 10 leaf
0 0 _1  NB. 11 leaf
0 0 _1  NB. 12 leaf
0 0 _1  NB. 13 leaf

dissect 'bsptest traversebsp 2 2'
when run, right-click traversebsp: does not subdissect
dissect the sentence alone: localizes error to wrong block
)


NB. TODO
NB. If big verb is too big for stack, show ... at end
NB. Should put 'atom' as shape of selection when it is shown filled with ()?
NB. u :. v doesn't allow getting into u with right-click
NB. (,.4 3) toupper;.0 'abracadabra'  should highlight the implied selection from the ;.0?
NB. dissect '25{.(,.~ <@>:@i.@#) ;({."#. <@(0&#`({.@{.(;,)<@}."1)@.(1<#))/. ])/:~~.,/(+,/:~@,)"0/~3^~1+i.100'   slow
NB. Put type of value into highlight line
NB. 
NB. Must add both other penalties when taking a turn? - no
NB. Adj penalties can cause a long route where a spread would help.  But where to localize the spread?
NB. Think more about grayed-out words in the sentence
NB. Have a way to do selections from script, for testing

NB. ?work on Android, with wd 'activity'

NB. (1) 3&+&2 (5 6 7)  shows ^: in the stack.  Change the 1 and see duplicates too - if details enabled
NB.     going to leave the ^:1 in to show what happened.  Change ^: in stack to (^:)?
NB. fit value needs to be evaluated in its locale to get the value right (needed by /.) - if nonsdt, should come in on the right
NB. Enforce a recursion limit to help debug stack error - if original failed w/stack error?
NB. support u . v y
NB. support {::
NB. Add rank-calculus for primitives with known behavior?

NB. display:
NB. Unicode?

NB. dissect - 2d graphical single-sentence debugger

NB. the call is:
NB. [options] dissect [sentence]
NB. where sentence is a string to be executed.  The sentence is parsed and modified so that every verb execution creates
NB. logging information about its input and outputs.  Then the modified sentence is executed (in the same context as the original
NB. dissect verb), and then the results are displayed in 2d form.  If sentence is omitted, the sentence from the last error is used.
NB.
NB. Options are (bitmask)[;(label options)] where
NB.  bit 0 is 1 to use a sandbox for executing the sentence
NB.  bit 1 is 1 to return the locale of the dissect window
NB.  bit 2 is 1 to suppress assignment statements.  They will not be executed and an error will result if an assigned name is referred to later.
NB.  bit 3 is 1 if this call was from the J debugger.  It changes the error messages if the assignments rule is violated.
NB.
NB. Label options are a table of (type);(string) where types are
NB.  'title'  string is (fontchange)TABtitle  fontchange is +-amount to increase over sentence size, title is text
NB.  'link'  string is (fontchange)TABdisplay textTABlink text
NB.  'datasize'  string is vpct hpct   max size of noun, in % of screen (must be converted to numeric)
NB.  'check'  string is 'all'=full comparison, shape'=shape and type only; 'error'=success/failure only; 'no'=no comparison (and don't execute the original sentence)
NB.
NB. If y is boxed, it should be a table ready for use in parse, i. e. nx3 where the first line gives
NB. options;locale;text of sentence
NB. and the remaining lines are the local names defined in the running explicit definition, as described in z458095869 below
NB.
NB. Result is a string containing an error message if a window couldn't be created, otherwise an empty string EXCEPT when the
NB. dissect locale was requested: in that case return it if it was created

dissect_z_ =: [: ([: display_dissect_ <@". :: (''"_)&.>)`nodisplay_dissect_@.(2=3!:0)  [: parse_dissect_ ((0: : [)    (([ ; 18!:5@(''"_) ; ]) , z458095869_dissectnopath_@(''"_))  getsentence_dissect_@])^:(0=L.@])

NB. The locale dissectnopath is used to find local names.  Its path is empty.  The locale contains only one name, z458095869
cocurrent 'dissectnopath'
copath ''
NB. The verb z458095869 returns a table of defined local names.  It is a table of (name;(type from 4!:0);(5!:5 form of name))
z458095869 =: (([ ,. <"0@] ,. (".@[`(rankinv_dissect_@[)`(rankinv_dissect_@[)`(rankinv_dissect_@[))@.]&.>) (4!:0)) @ ((<'z458095869') -.~ 4!:1@i.@4:)

IFJA_z_ =: {. 0 ,~ ". 'IFJA_z_'  NB. gl2 expects it
require 'strings gl2'
cocurrent 'dissect'
coinsert 'jgl2'

NB. The address of the jsoftware wiki
JWIKIURL =: 'http://code.jsoftware.com/wiki/'

defaultfonts =: (<"0 (12 12 14 8 10)) ,.~ (('Darwin';'Linux') i. <UNAME) { ".;._2 (0 : 0)
'"Courier"' ; '"Lucida Console"' ; '"Arial"' ; '"Arial"' ; '"Arial"'    NB. Mac version
'"monospace"' ; '"Lucida Console"' ; '"Arial"' ; '"Arial"' ; '"Arial"'    NB. Linux version
'"Courier New"' ; '"Lucida Console"' ; '"Arial"' ; '"Arial"' ; '"Arial"'    NB. Version for all others
)

debscroll =: 3 : 0
QP '6!:1$0 y >coname$0 scrollingtype__COINSTANCE >scrollinglocale__COINSTANCE '
)

NB. lines beginning config_ are names that are initialized in the instance from the globals here
NB. the others are global, shared among running dissections
CONFIG =: 0 : 0
configfilelevel =: 0
tooltipctrl =: 0
fontchoices =: defaultfonts
tooltipdelayx =: 2 NB. tooltip delay
tooltipdetailx =: (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'    NB. tooltip detail level
displayshowcompmods =: 0   NB. display full modified verb, not just modifier line
displayshowstructmods =: 0   NB. display a line for @ @: & etc
config_maxnoundisplaysizex =: 3 3
config_displayshowstealth =: 0
config_displayautoexpand2 =: 0   NB. Automatically show u/ on 2 items as dyad
config_displayshowfillcalc =: 0  NB. Make a rankstack mark when fill-cell is used
config_displayprecisionx =: 2   NB. default display precision
displayshowtipoftheday =: 1  NB. Display a tip at startup
)

NB. Read & apply config file.  Run in dissect locale.
loadconfig =: 3 : 0
try.
  NB. Define the defaults first, in case config format has changed
  cfile =. CONFIG , 1!:1 <jpath '~config/dissect.ijs'
catch.
  cfile =. CONFIG , 'configfilelevel =: _1' , LF
end.
NB. Remove CR, force LF termination
cfile =. LF ,~^:(~: {:) cfile -. CR,TAB
0!:0 cfile
NB. If the config file was downlevel, here is where we take action
select. configfilelevel
case. _1 do.
  NB. no config file - keep defaults
case. 0 do.
  NB. config file, but created before levels assigned
  tooltipdetailx =: 2 + tooltipdetailx  NB. we added 'none' & 'one line' options
  displayshowcompmods =: {. ". 'displaycompmods'   NB. we renamed this
  displayshowstructmods =: {. ". 'displaystructmods'   NB. we renamed this
  NB. Default the font for the status line
  fontchoices =: fontchoices , (#fontchoices) }. defaultfonts
case. 1 do.
  NB. Downlevel config file
  tooltipdetailx =: (tooltipdetailx > 0) + tooltipdetailx  NB. we added 'one line' option
  NB. Default the font for the status line
  fontchoices =: fontchoices , (#fontchoices) }. defaultfonts
case. do.
  NB. current level - that's OK
end.
0 0$0
)

NB. Write current settings to config file.  Called in the instance that we want to save
saveconfig =: 3 : 0
NB. Save the config file with the current level
configfilelevel =: CONFIGFILELEVEL
NB. Get the variable names we want to save under
cnames =. {.@;:;._2 CONFIG
NB. Get the name to save from - same with config removed, so we get the instance value
inames =. ('config_' (#@[ }. ])^:([ -: #@[ {. ]) ])&.> cnames
NB. create log string
cdata =. ; cnames <@(LF ,~ >@[ , ' =: ' , 5!:5@])"0 inames
try.
  cdata 1!:2 <jpath '~config/dissect.ijs'
catch.
  wdinfo 'Error creating config file';'Unable to write config file'
end.
0 0$0
)

NB. Apply config variables to the current instance.  Called in the instance locale.
NB. If y is not empty, it is the table of input options.  We override the config file with them
applyconfig =: 3 : 0
inames =. a: -.~ (#~   'config_' -: 7&{.)&.>@{.@;:;._2 CONFIG
NB. All names starting config_ become instance names losing the config_
(7 }.&.> inames) =: ".&.> inames
if. #y do.
  if. (#y) > dsx =. ({."1 y) i. <'datasize' do.
    NB. Input options specify the max display sizes, use it
    maxnoundisplaysizex =: (}:MAXNOUNPCTCHOICES) I. 2 ($,) 0 ". (dsx,1) {:: y
  end.
end.
NB.?lintonly tooltipdelayx =: tooltipdetailx =: displayshowcompmods =: displayshowstructmods =: 0
NB.?lintonly maxnoundisplaysizex =: 0 0 [ displayshowstealth =: displayautoexpand2 =: displayshowfillcalc =: displayprecisionx =: =: displayshowtipoftheday =: 0
0 0$0
NB.?lintsaveglobals
)

NB. Apply config setting to form - must wait until form exists.
setformconfig =: 3 : 0
NB. We have to set the form after loading the values
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '1'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '1'
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0 ,~ tooltipdelayx) wdsetvalue '1'
('fmtooltipdetail' , TOOLTIPDETAILCHOICES {::~ <0 ,~ tooltipdetailx) wdsetvalue '1'
maxnoundisplayfrac =: 0.01 * maxnoundisplaysizex { MAXNOUNPCTCHOICES
('fmprec' , ": displayprecision =: DISPLAYPRECCHOICES {~ displayprecisionx) wdsetvalue '1'
calccfms fontchoices
NB. The rest of the form settings are performed each traversal
NB.?lintsaveglobals
)


dissectinstance =: 0$a:

defstring =: 'start of traversal'"_   NB. for debugging only

NB. Handle special inputs: 0=last error, 1=clipboard, others pass through unchanged (numeric 2 will be a quiet return)
getsentence =: (' ' takeafter LF (i:~ }. ]) [: }:^:(LF={:) (13!:12)@(''"_))`(LF (>:@i:~ }. ]) LF , [: }:^:(LF={:) CR -.~ [: wd 'clippaste'"_)`]@.((0;1) i. <)

NB. Maximum line length that we will try to display in a grid cell
MAXSENTENCEWIDTH =: 0.5  NB. max frac of screenwidth that we allow for sentence display

ifdefined =: 0 <: [: 4!:0 <  NB. true if name defined in path
ifinlocale =: < e. (0 1 2 3) 4!:1~ {.   NB. true if name defined in current locale 

NB. ******************* code for function keys ******************
finddissectline =: (3 : 0) :. (smoutput^:(*@# *. 2 = 3!:0))
NB.?lintonly  WinText_jqtide_ =: '3 + 5' [ WinSelect_jqtide_ =: 0 0
NB. y tells what kind of run: 0=line under cursor, 1=last error, 2=clipboard
select. y
case. 0 do.
  NB. fs is a character index; if window contains non-ASCII, convert to unicode
  ft =. 7 u: WinText_jqtide_
  fs =. WinSelect_jqtide_
  NB. If a single value is selected, take the whole line; otherwise the selected region
  if. 1 < # ~. fs do.
    sentence =. 8 u: (-~/\ fs) (];.0~ ,.)~ ft 
  else.
    NB. Select sentence - but find the part between control words
    pref =. 8 u: (LF taketo&.|. ({.fs) {. ft)   NB. The line before the cursor, in UTF-8
    line =. pref , 8 u: LF taketo ({.fs) }. ft  NB. The whole line
    try.
      words =. ;: line  NB. convert to words
    catch.
      smoutput 'cannot dissect: ' , ((<:13!:11'') {:: 9!:8 '') , ' in sentence'
      2 return.   NB. causes quiet return from parse
    end.
    NB. For each word, calc number of nonblanks from begin line to end of word
    wordnb =. +/\ +/@:~:&' '@> words
    prefnb =. +/@:~:&' ' pref   NB. number of nonblanks in prefix
    NB. Get mask of control words
    cwx =. words I.@:e. ,&'.'&.> controlwords
    NB. Get the index of the control-word containing the cursor: the frets are the beginning of each cw,
    NB. so that cursor just before the first char of a cw indicates the previous cw; cursor in cw means following sentence
    currcwx =. (cwx { 0 , wordnb) I. prefnb
    NB. Get #nonblanks before the beginning of the cursor cw.  We have a list of starting nb counts, prepending 0 for start of line
    bgnnb =. currcwx { 0 , cwx { wordnb
    NB. Get #nonblanks to the end of the cursor cw.  0,wordnb is a list of ENDING nb counts; we add one for the end-of-line
    endnb =. currcwx { (cwx { 0 , wordnb) , {: wordnb
    if. bgnnb < endnb do.
      NB. Convert nonblank counts to char counts by indexing the user's nonblanks
      NB. This indexes the beginning char & the end char
      be =. (+/\@:~:&' ' line) i. (>:bgnnb),endnb
      NB. Extract from beginning to before end+1
      sentence =. line (];.0~ ,.) -~/\ 0 1 + be
    else. sentence =. ''  NB. No nonblanks in selected region
    end.
  end.
  NB. If user selected all blanks, give a message and use a sentence of 2 to create quiet return from parse
  if. sentence +./@:~: ' ' do.
    sentence
  else.
    smoutput 'nothing to dissect'
    2  NB. this will cause quiet return from parse
  end.
case. 1 do.
  0  NB. 0 means 'last error'
case. 2 do.
  1  NB. 1 means 'clipboard'
case. do.
  ''  NB. Will give usage message
end.
)

NB. ********************** from here on is devoted to parsing J sentences ***************
NB.
NB. Parsing stuff, copied from trace.ijs
NB. sdt means self-defining term: a number or string rather than a name or a result
(x) =: 2^i.#x =. ;:'noun verb adv conj lpar rpar asgn name mark sdt'
any =: _1
avn =: adv + verb + noun
cavn =: conj + adv + verb + noun
edge =: mark + asgn + lpar
invvalences =: invmonad+invdyad

x =. ,: (edge,       verb,       noun, any      ); 0 1 1 0; '0 Monad'
x =. x, ((edge+avn), verb,       verb, noun     ); 0 0 1 1; '1 Monad'
x =. x, ((edge+avn), noun,       verb, noun     ); 0 1 1 1; '2 Dyad'
x =. x, ((edge+avn), (verb+noun),adv,  any      ); 0 1 1 0; '3 Adverb'
x =. x, ((edge+avn), (verb+noun),conj, verb+noun); 0 1 1 1; '4 Conj'
x =. x, ((edge+avn), (verb+noun),verb, verb     ); 0 1 1 1; '5 Trident'
x =. x, (edge,       cavn,       cavn, any      ); 0 1 1 0; '6 Bident'
x =. x, ((name+noun),asgn,       cavn, any      ); 1 1 1 0; '7 Is'
x =. x, (lpar,       cavn,       rpar, any      ); 1 1 1 0; '8 Paren'

PTpatterns =: >0{"1 x  NB. parse table - patterns
PTsubj =: >1{"1 x  NB. "subject to" masks
PTactions =: 2{"1 x  NB. actions

bwand =: 17 b.    NB. bitwise and
bwor =: 23 b.    NB. bitwise or
bwxor =: 22 b.   NB. bitwise XOR
bwlsl =: 33 b.  NB. logical left shift
enclosing =: ([: > [: {. [) , ] , [: > [: {: [

prespace =: ,~ e.&'.:'@{. $ ' '"_
NB. preface a space to a word beginning with . or :

isname =: ({: e. '.:'"_) < {. e. (a.{~,(i.26)+/65 97)"_
NB. 1 iff a string y from the result of ;: is is a name

NB. y is a value, result is 1 if it looks like a gerund
isar =: 0:`((1:`((2 = #) *. (2 = 3!:0@>@{.))`0:)@.(2 32 i. 3!:0)@>)@.(32=3!:0)"0
isgerund =: [: +./ isar

class =: 3 : 0         NB. the class of the word represented by string y
if. y-:mark do. mark return. end.
if. isname y do. name return. end.
if. 10>i =. (;:'=: =. ( ) m n u v x y')i.<y do.
  i{asgn,asgn,lpar,rpar,6#name return.
end.
(4!:0 <'x' [ ".'x =. ',y){noun,adv,conj,verb
)
NB. *** end of copied stuff

NB. Other utilities

NB. result is 1 if y is empty
isempty =: 0 e. $

NB. Result is 1 if y is numeric type (regardless of empty)
isnumtype =: 1 4 8 16 64 128 1024 4092 8192 16384 e.~ 3!:0

NB. Result is 1 if y is numeric or empty
isnumeric =: isnumtype@[^:(0=]) isempty

NB. Result is 0 if y is nonnumeric, 1 is numeric noninteger, 2 if integer (after conversion)
isinteger =: (+   9&o. -: <.)~^:] isnumeric

 NB. *** end of utilities

NB. possible starting variables, in name;type;value form
startvbls =: 'xymunv' (,@[ ; '' ;~ ])"0 noun,noun,noun,(verb+sideeff),noun,(verb+sideeff)

enparen =: '(' , (' ' #~ '.:' e.~ {.) , ,&')'

NB. y is an AR.  Result is string form.  But if the result is more than 50 chars, we
NB. return empty; if more than 20 chars, we return the first 20
ARtostring =: 3 : 0"0
y =. y 5!:0
y =. 5!:5 <'y'
if. 50 < #y do.
  ' ... '
elseif. 20 < #y do.
  enparen (20{.y),'...'
elseif. do.
  enparen y
end.
)

NB. y is name;<<locale to look in
NB. y may have object names appended
NB. Result is simplename;locale the name was found in; empty if not found
findnameloc =: 3 : 0
'name loc' =. y
NB. If there are object names, resolve them in loc and replace loc with
NB. the result
if. '__' +./@:E. name do.
  NB.?lintonly loc =. <'dissectobj'
  cocurrent loc
  loc =. ('__' takeafter name)~
  name =. '__' taketo name
end.
NB.?lintonly loc =. <'dissectverb'
NB. Follow search path starting in loc, and stop when the name is encountered
ret =. ''
for_l. loc , 18!:2 loc do.
  NB.?lintonly l =. <'dissectverb'
  cocurrent l
  if. (<name) e. ({. name) (4!:1) 0 1 2 3 do. ret =. name ; l break. end.
end.
ret
)

NB. called after error. y is the ARs of the operands that were executed
NB. x is 1 (default 1) to include J error info - use only if there has been an error
postmortem =: 3 : 0
1 postmortem y
:
if. x do.
  s =. LF,((<:13!:11''){::9!:8'')
else. s =. ''
end.
s,LF, ; <@ARtostring y
)

parse =: 3 : (('catch.';'catchd.') stringreplace^:DEBNOCATCH_dissect_ 0 : 0)  NB. called in dissect locale
QP^:DEBTIME'startparse=?6!:1'''' '
NB. dissectinstance should be empty when parse is called.  parse will then allocate the instance and run the parse,
NB. which ends by executing the parsed verb.  display/nodisplay is then called to display the instance.
NB.
NB. If dissectinstance is nonempty here, it means that the parsed verb is attempting a recursion into dissect, which we must
NB. intercept.  We return empty, which will cause nodisplay to be called for the recursion.  nodisplay will
NB. clear dissectinstance, so that display for the original dissect call will find no dissectinstance, which it
NB. interprets as a recursion request, exiting with an appropriate message and everything reset.
if. #dissectinstance do. '' return. end.  NB. Return empty... which will bypass display
dissectinstance =: '' conew 'dissect'   NB. global because must persist over return to user environment
errormessage =: ''
try.
  parsemain__dissectinstance y
catch.
  NB. Unexpected error (not set by failmsg)
  smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput 13!:12''
  'error during parsing'
catcht.
  errormessage
end.
NB.?lintsaveglobals
)

NB. Signal failure of the parse.  y is the error message
NB. We keep it in the main dissect locale for ease, since it is valid only over the parse
failmsg =: 3 : 0
errormessage_dissect_ =: y
throw.
smoutput 'Throw was not caught!'
13!:8 (1)
)

saveJenvirons =: 3 : 0   NB. called in the instance locale.  creates the instance name 'wdtimer'
NB. We save the locale sizes and the sys_timer status.  If this is not QT, we define sys_timer_z_
NB. but only if sys_timer_base_ is not defined; and we delete it when we restore
NB. Make the timer check only when starting the first instance
NB. If a timer was NOT created because of earlier sys_timer, stub out wdtimer in the instance;
NB. otherwise allow it to go through to the definition in dissect locale
if. ALLOWNONQTTOOLTIP *. -. IFQT do.
  if. 0 = #a: -.~ {."1 dissectionlist_dissect_ do.
    if. 0 > 4!:0 <'sys_timer_base_' do.
      sys_timer_z_ =: sys_timer_dissect_  NB.?lintonly =: 0:
    end.
  end.
  NB. If somebody else's timer is present, disable our use of it
  if. 3 = 4!:0 <'sys_timer_z_' do. if. (<'sys_timer_dissect_') -.@-: 5!:1 <'sys_timer_z_'  do. wdtimer =: ] end. end.
end.
Jenvirons =: (9!:38 '')
NB.?lintsaveglobals
)
NB. Restore on any return to immediate mode.
NB. If we are returning after the last destroy, we also restore the timer.
NB. This makes sure we leave the user in his original state always
restoreJenvirons =: 3 : 0   NB. called AFTER removing instance from the list
9!:39 Jenvirons
if. (0 = #a: -.~ {."1 dissectionlist_dissect_) *. (ALLOWNONQTTOOLTIP *. -. IFQT) do.
  if. 3 = 4!:0 <'sys_timer_z_' do. if. (<'sys_timer_dissect_') -: 5!:1 <'sys_timer_z_' do.
    NB. If we created a timer, remove it
    4!:55 <'sys_timer_z_'
  end. end.
end.
0 0$0 
)

CASCADEOFFSET =: 20 20   NB. Amount to offset a new window-level from the last previous window-level
NB. Initialization
create =: 3 : 0
NB. Save the initial environment BEFORE we indicate instance running
saveJenvirons''
slottouse =. ({."1 dissectionlist_dissect_) i. a:
if. slottouse = #dissectionlist_dissect_ do.
  NB. new window position.
  NB. Figure out the initial position for the new window.  It is at a cascade offset from the
  NB. position of the last window.  We have to ask the window its position, since we don't
  NB. get an event for a pmove 
  lastparent =. (<_1 0) { dissectionlist_dissect_
  NB.?lintonly lastparent =. <'dissect'
  NB.?lintmsgsoff
  wd 'psel ' , winhwnd__lastparent
  NB.?lintmsgson
  try. 
    initpos =. CASCADEOFFSET + 1 0 { 0 ". wdqform''
    dissectionlist_dissect_ =: dissectionlist_dissect_ , ($0);initpos
  catch.
    NB. error reading form position for last slot.  It must have failed before the form started.
    NB. close it and reuse its slot and initial position
    destroy__lastparent 1
    slottouse =. <: slottouse
  end.
end.
dissectionlist_dissect_ =: (coname'') (<slottouse,0)}  dissectionlist_dissect_
objtable =: 0$a:   NB. list of parse objects
ticket =: 0   NB. sequential log number
loggingallowed =: 1   NB. allow logging
debuglocs =: 0$a:  NB. List of locales created by expanding tacit names from this window

NB. Variables used to control hovering in this window
hoverinitloc =: $0   NB. Init no hover active
sentencehovertok =: $0  NB. token # we are hovering over, if any
blockhoverloc =: 0$a:   NB. locale we are hovering over, if any

NB. Create the name we use to get to the instance - in this locale it points to itself
COINSTANCE =: coname''

winhwnd =: ''  NB. Init to no window
NB. Use lightweight locales - we use less than 100 entries usually
9!:39 (1) 1} 9!:38 ''
NB. For nodes that do not have a parallel path (i. e. all but forks and &), this locale will
NB. be the predecessor locale, and will not signal an error
errorcode =: 0
NB. Because of debug irregularities, we execute debug out of a different event from
NB. the one that started it.  The presence of startdebuglocale on mbrup indicates that we need to execute it
startdebuginfo =: 0$a:
NB.?lintsaveglobals
)

NB. Add new object to the list of objects
NB. We make the newest object first to solve a subtle problem: certain locales (like assignments) coinsert an
NB. existing locale to resolve undefined names.  If a locale in the path is destroyed, it will make names
NB. like codestroy unresolvable.  So, we order the locales here to be destroyed in the opposite order of creation.
newobj =: 3 : 0
objtable =: y , objtable
)

NB. Utility to create rank,invertible flags for a verbname
NB. y is name of a verb, visible in current context
NB. result is 5!:5 value of name
rankinv =: 5!:5@:<

NB. anything beginning with one of these words and ending with . is a control word
controlwords =: ;: 'assert break continue for goto label if do else elseif end return select case fcase throw try catch catchd catcht while whilst'
NB. for each line, find control words; then recollect sentences between control words; then
NB. append the line number of the line. run all the blocks together.  This deletes empty sentences, too
NB. For multiple blocks on the same line (caused by control words), give them fractional parts to
NB. distinguish them
NB. Verb, returning 1 if a word is a control word
iscw =: ('NB.' -: 3 {. >) +. e.&controlwords@(('_'&taketo)@}:&.>) *. ('.'={:)@>  NB. verb, applied to boxed word.  Any remaining comment must be a lint directive

NB. **** verbs to create nodes.  DO NOT USE CONEW because it doesn't set COCREATOR properly

NB. Create a verb node.  y is (string form of the verb[;display form]);(token number)[;(one-line def)]
NB. if display form is not given, string form is not boxed
NB. x is locale to use for COCREATOR (if omitted, we must be calling from the main instance, just use its name)
NB. Result is result from create which is type;locale;token #
createverb =: 3 : 0
(coname'') createverb y
:
NB. If the primitive is known, use its locale
nobj =. conew (0 { y) (#@]  ('dissectprim' , ":@])`('dissectverb'"_)@.=  i.&1@:((e.>)"0)) dissectprimindex
NB.?lintonly nobj =. <'dissectobj'
COCREATOR__nobj =: x
create__nobj y
)

NB. Create a noun node.  y is (string form of the verb[;display form]);(token number);value
NB. if display form is not given, string form is not boxed
NB. x is locale to use for COCREATOR (if omitted, we must be calling from the main instance, just use its name)
NB. Result is result from create which is type;locale;token #)
createnoun =: 3 : 0
(coname'') createnoun y
:
nobj =. conew 'dissectnoun'
COCREATOR__nobj =: x
create__nobj y
)

NB. Adverb.  u is 1 to assign COCREATOR (used only when called outside the main instance)
NB. Create a modifier node.  y is whatever is needed by the modifier
NB. for normal nodes, (string form of the verb[;display form]);(token number)
NB. if display form is not given, string form is not boxed
NB. x is locale to create.
NB. Result is result from create which is (type;locale;token #)
createmodifier =: 1 : 0
:
nobj =. conew >x
NB.?lintonly nobj =. <'dissectobj' [ COCREATOR =. ''
if. m do. COCREATOR__nobj =: COCREATOR end.
create__nobj y
)

NB. Routine to parse and execute a block
NB. inparms is the environment:
NB.  table of local variables (name;type from 4!:0;ranks if verb)
NB. the first line of the table is special: it's options;locale;sentence to execute
NB. giving the locale in which the verb will execute
NB. Result is the boxed string form of the instrumented sentence, ready to execute;
NB.  or a string containing an error message.  If result is a string, processing ceases
NB. As a side effect, many objects are created indicating the parse structure
NB. In paticular, resultroot is the boxed locale of the sentence result.
NB. If there is an error, resultroot is empty
NB. Options: bit 0 is 'sandbox', in which case we create an explicit definition to run the
NB.   sentence in, and define all the user names in it, before running it
NB. bit 1 is 'return locale', which returns boxed locale (an atom) if there is no error
NB. bit 2 is 'noassign' which neuters assignments (useful in debug)
NB. bit 3 is 'debug', reserved for future use 
parsemain =: 3 : 0   NB. runs in object locale
defnames =: }. y  NB. table of names
'options loc sentence' =. {. y
NB. Convert the options to keyword;value form.
NB. We must do this first because destroy uses dispoptions, so dispoptions must be properly defined
NB. before any failure is detected
NB. If the options is unboxed, box it.
if. 2 > #$ dispoptions =: boxopen options do.
  NB. Options are a list or atom.  See if first one is numeric
  if. 1 4 8 16 e.~ 3!:0 numopt =. {.!.0 > {. dispoptions do.
    NB. Old numeric form.  Convert the number to a table of options.
    transopt =. (2 2 2 2 #: numopt) # (;: 'fromdebugger noassignment returnobject sandbox') ,. <1
    NB. Append the other options if there are any
    if. 1 < #dispoptions do.
      dispoptions =: transopt , ({."1 transopt) subkl _2 ]\^:(2 > #@$@]) 1 {:: dispoptions
    else. dispoptions =: transopt
    end.
  else.
    NB. List of options, but no numeric.  Convert to table
    dispoptions =: _2 ]\ dispoptions
  end.
end.
NB. Audit the options for validity
if. #badopts =. ({."1 dispoptions) -. ;: 'fromdebugger noassignment returnobject sandbox check title link datasize parent' do.
  failmsg 'Invalid options: ' , ;:^:_1 badopts return. 
end.
binopts =. dispoptions #~ ({."1 dispoptions) e. ;: 'fromdebugger noassignment returnobject sandbox'
if. 1 e. emask =. ({:"1 binopts) -.@e. 0;1 do.
  failmsg 'Invalid value for option: ' , ;:^:_1 emask # {."1 binopts return. 
end.
if. (<'all' qopt 'check') -.@e. ;:'all shape error no' do.
  failmsg 'Invalid value for ''check'' option.' return. 
end.

NB. The numeric atom 2 is used by finddissectline to create a quiet return of an empty string
if. 2 -: sentence do. '' return. end.

if. (2 ~: 3!:0 sentence) +. (1 < #$sentence) do.
  failmsg 'The sentence to be dissected must be a string.' return. 
end.
NB. The returnobject flag must be in dissect locale so that display can get to it
returnobject_dissect_ =: qopt 'returnobject'

NB. Break the input into words.  If there is an error, fail.  Discard any comment
NB. Discard anything past the first LF, and remove CR
sentence =. CR -.~ ({.~ i.&LF) sentence
if. DEBSENTENCELOG do. (sentence,LF) 1!:3 SENTENCELOGFILE end.
try. queue =. ;: sentence catch. queue =. 0$a: end.
NB. If the last word is a comment, delete it
if. #queue do.  NB. following fails on no words
NB. Get mask of words to discard: discard leading control words, or anything starting with a control word after a non-control
  dischdtl =. (*./\ ,: [: +./\ 0 , (2) </\ ]) iscw queue
  if. (('.:' -.@e.~ {:) *. 'NB.' -: }:) 4 {. _1 {:: queue do. dischdtl =. 1 (<1 _1)} dischdtl end.
  NB. Get the sentence in the form the user gave it, by deleting the nonblank characters corresponding
  NB. to the discarded words.
  ndiscardshdtl =. dischdtl (#@(-.&' ')@;@#)"1 queue
  NB. Make sure the queue matches the tokens that have been selected for processing
  queue =. ;: usersentence =: ' ' (-@(i.&0@:= |.) }. i.&0@:= }. ]) sentence ((}.~ {.) }.~ -@{:@]) ndiscardshdtl i.~"0 1 (0) ,. (+/\ ,: +/\@|.) ' ' ~: sentence
end.
NB.?lintonly usersentence =: ''
NB. If the sentence is empty, abort
if. 0 = #queue do.
  failmsg 'Usage: dissect ''sentence''',LF,LF,'Try   dissect ''0'' to see example screen' return.
end.

NB. Append an end-of-queue mark to the sentence, and initialize the stack.
NB. The stack is type;value;tokennums where value is the locale of the object producing the result, for verb and noun;
NB. or the string form, for a modifier.  Tokennums are the input token numbers that contribute to the item
queue =. mark ; queue
stack =. 4 2 $ mark;''

NB. In case of parse failure, we remember whether we executed a user modifier.  If we did, our assumption that it produced
NB. a verb may have caused the failure, and we give a suitably couched error message
usermodifierencountered =: 0

NB. Process the sentence through the stack
while. do.
NB. If the stack contains an executable combination, execute it
NB. If part of the execution has unknown value, produce an unknown result, of type 'noun' for verb executions,
NB. and 'verb' for modifier executions
  select.
    NB.?lintonly stack =. (verb,verb,verb,noun,4$mark);"0 1 '';''
    if. (#PTpatterns) > pline =. 1 1 1 1 i.~ * PTpatterns bwand"1 ,>4 1{.stack do.
      exeblock =. (subj =. pline{PTsubj) # 4 {. stack  NB. the executable part
      exetypes =. > subj # , 4 1 {. stack   NB. the corresponding types
    end.
    NB.?lintonly exeblock =. 3 3$<'' [ exetypes =. 0 0 0 [ subj =. 0 1 1 1
    QP^:DEBPARSE'pline exetypes exeblock stack '
    pline
   
  case. 0;1 do.  NB. monad
    NB. If the sentence is going to create a noun result, a monad/dyad must be followed by rpar, mark, or conj.
    NB.  Anything else would fail or produce an adverb.  If we detect anything else, we fail, localizing the error
    NB. to between the y operand and the following token
    if. ((<:sdt) bwand (< 0 ,~ 3+pline) {:: stack) -.@e. mark,rpar,conj do.
      etype =. 'Syntax error: execution of monad not at the end'
      failmsg etype , LF , 'Error snippet: ' , ;:^:_1 (<: /:~ ; (< 2 ;~ 1 2 3 + pline) { stack) { ;: sentence  NB. Decr tok #s for leading mark in queue
      return.
    end.
    NB. Create a monad execution block for the operands, and put that on the stack
    stack =. ((subj i. 1){.stack),('dissectmonad' 0 createmodifier exeblock),((>:subj i: 1)}. stack)

  case. 2 do.  NB. dyad
    NB. Verify followed by rpar/mask, as for monad exe
    if. ((<:sdt) bwand (< 4 0) {:: stack) -.@e. mark,rpar,conj do.
      etype =. 'Syntax error: execution of dyad not at the end'
      failmsg etype , LF , 'Error snippet: ' , ;:^:_1 (<: /:~ ; (< 2 3 4;2) { stack) { ;: sentence  NB. Decr tok #s for leading mark in queue
      return.
    end.
    NB. Create a dyad execution block for the operands, and put that on the stack
    stack =. ((subj i. 1){.stack),('dissectdyad' 0 createmodifier exeblock),((>:subj i: 1)}. stack)

  case. 3;4 do.  NB. adverb/conjunction execution
    stack =. ((subj i. 1){.stack),(execmod exeblock),((>:subj i: 1)}. stack)

  case. 5 do.  NB. Trident N V V or V V V
    NB. Create a trident execution block for the operands, and put that on the stack
    stack =. ((subj i. 1){.stack),('dissectfork' 0 createmodifier exeblock),((>:subj i: 1)}. stack)

  case. 6 do.   NB. bident  A A, C VN, VN C, V V  and errors like N N, C C, etc
    NB.?lintonly exetypes =. 0 0 [ exeblock =. '';'';''
    if. bwand/ verb , exetypes do.  NB. V V
      stack =. ((subj i. 1){.stack),('dissecthook' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
    elseif. (bwand/ adv , exetypes) +. (conj = +/ conj bwand exetypes) do. NB. A A, C VN, NV C
      NB. This becomes an adverb type.  The value is the exeblock, which will be executed later
      stack =. ((subj i. 1){.stack),(adv;exeblock;(< /:~ ; 2 {"1 exeblock)),((>:subj i: 1)}. stack)
    elseif. do.
      etype =. 'Syntax error: invalid sequence ' , ;:^:_1 ('Verb';'Adverb';'Conjunction';'Noun') {~ 1 i.~"1 * exetypes bwand/ (verb,adv,conj)
      failmsg etype , LF , 'Error snippet: ' , ;:^:_1 (<: /:~ ; 2 {"1 exeblock) { ;: sentence  NB. Decr tok #s for leading mark in queue
      return.
    end.

  case. 7 do.  NB. assignment
    NB. See if we can analyze the assignment.  If so, add to the name table.
    NB. If the assignment is not a noun value, ignore it with a warning
    if. 0 = noun bwand 2 { exetypes do.
      failmsg 'Undissectable sentence: non-noun assignment not supported'
      rname =. 0$a:
      return.
    NB. See if it's a simple assignment to a name
    elseif. name = 0 { exetypes do.
      rname =. (<0 1) { exeblock  NB. boxed name
    NB. If the assignment is an AR assignment, ignore it with a warning
    elseif. (sdt+noun) ([ -: bwand) 0 { exetypes do.
      rname =. (<0 1) {:: exeblock  NB. locale of sdt
      NB.?lintonly op_dissectnoun_ =: '' [ rname =. <'dissectnoun'
      if.  2 = 3!:0 lvalue =. ". op__rname do.  NB.?lintonly [ lvalue =. ''
        if. '`' = {. lvalue do.
          failmsg 'Undissectable sentence: AR assignment to ' , lvalue , ' not supported'
          rname =. 0$a:
          return.
        else.
          rname =. ;: :: (a:$~0:) lvalue
        end.
      else.
        failmsg 'Undissectable sentence: invalid assignment'
        return.
      end.
    NB. If the assignment is to a variable name, we can do nothing with it
    elseif. do.
      rname =. 0$a:
    end.
    NB. If the assignment is one we can handle, we will have one or more names.  In that case, create an
    NB. assignment block on the stack
    stack =. ((subj i. 1){.stack),('dissectassign' 0 createmodifier exeblock;qopt 'noassignment'),((>:subj i: 1)}. stack)
    NB. We would like to preserve the value of the unhandleable assignment, but we can't, because
    NB. We need an assignment node to account for the assignment tokens, and we can't get a value for the
    NB. modifier because it might be complex (a train).  If we try to push the assignment tokens into
    NB. the rvalue, it would have to be able to handle them, which we're not ready to do since they would be
    NB. out of order. So, we lose the value of the assignment (kludge)
    NB. Otherwise, we have to ignore it.  We can't produce an assignment block, because we don't know what
    NB. value to put into the executed sentence.  So we just ignore the assignment, leaving
    NB. the rvalue on the stack.  We will leave the assignment tokkens out of the display too, since we
    NB. don't process them
      
    NB. rname has the list of names that we should define.  If this is a global assignment,
    NB. append the locale name to each name that doesn't contain a locative
    if. (<'=:') -: (<1 1) { exeblock do. rname =. (('_',(>loc),'_') ,~ ])^:('__'&(+./@:E.) +: '_' = {:)&.> rname end.

    NB. We can't deal with assignments to object locatives since we track only the part of speech, not the value, at parse time
    if. +./ elocs =. '__'&(+./@:E.)@> rname do.
      NB. We used to give a warning but that's probably doing too much
      NB. smoutput 'Assignment to object locatives not supported: ' , ;:^:_1 elocs # rname
      rname =. (-. elocs) # rname
    end.

    NB. Define the names, as nouns (J nameclass 0).
    defnames =: (rname ,"0 1 ((0+256*qopt 'noassignment');'')) , defnames

  case. 8 do.  NB. ( x ) - but remember the token numbers of the parens
    if. (noun+verb) bwand 1 { exetypes do.
      NB. If the stackop is a verb or noun, it has a locale & we should install the tokens there
      insideop =. (<1 1) {:: stack
      NB.?lintmsgsoff
      tokensource__insideop =: tokensource__insideop , ; (<0 2;2) { stack
      NB.?lintmsgson
    end.
    NB. Also remember the tokens in the exeblock.  If the word is a modifier, they will
    NB. eventually be added into a locale.  In any case, they will be around in case of error
    stack =. (< ; (<0 1 2;2) { stack) (<1 2)} stack

    NB. Remove () from the stack
    stack =. (<<<0 2) { stack

    NB. If the stack did not have an executable combination, bring the next word onto the stack.
  case. do. NB. no executable fragment
    if. 0 = #queue do. pline =. _1 break. end.  NB. This is how we end the sentence, with pline set as a flag
    qend =. > qendb =. {: queue
    queue =. }: queue

    NB. If this is the last word in the queue, it's the mark, keep it
    if. mark = qend do.
      stack =. (mark;'';(#queue)) , stack
      NB. If this is an assignment statement, and the new word is a name, this is where we detect that.
      NB. We stack the bare name as the value
    elseif. (asgn = (<0 0) {:: stack) *. isname qend do.
      stack =. (name;qend;(#queue)) , stack
    NB. If punctuation, keep it
    elseif. qendb e. ;:'() =. =:' do.
      stack =. ((qend;(#queue)) ;~ (lpar,rpar,2#asgn) {~  (;:'() =. =:') i. qendb) , stack
    NB. If self-defining term, create a noun block for it, mark as sdt.  String, number, a. a: _.
    elseif. (qend e. ;:'a. a: _.') +. (':' ~: {: qend) *. ({. qend) e. '''_0123456789' do.
      try.
        stack =. stack ,~ (<sdt+noun) 0} createnoun qend;'';(#queue);<".qend
      catch.
        NB. Any error must be an ill-formed token
        failmsg ((<:13!:11'') {:: 9!:8 '') , ': ' , qend
      end.
    elseif. isname qend do.
      NB. Name.  Resolve the name to find part of speech.
      NB. split the name into (global part),(object locative).  If the name is absolute (ending in _),
      NB. Make that the entire object locative, so that we look it up in case it has been directly assigned earlier
      NB. in the sentence
      if. '__' +./@:E. qend do.
        'glopart objloc' =. (({. ; 2 }. }.)~   '__'&(i:&1@:E.)) qend
      else.
        'glopart objloc' =. '';qend
      end.
      NB. Look up the (object locative)/(simple name) [depending on whether there is an
      NB. object locative] in the local name table, resolving to type;value/rank if found
      if. (<objloc) e. {."1 defnames do.
        'objtype objval' =. 1 2 { (({."1 defnames) i. <objloc) { defnames
        gloc =. objval
      elseif. (<objloc =. objloc , '_' , (>loc) , '_') e. {."1 defnames do.
        NB. not found as a local, but it may have been assigned in this sentence as a global.  If so,
        NB. use that value
        'objtype objval' =. 1 2 { (({."1 defnames) i. <objloc) { defnames
        gloc =. objval
      elseif. do.
        NB. Nothing found in local table - set to resolve the whole thing globally
        gloc =. loc
        glopart =. qend
      end.
      NB. Now we have resolved any local that we are going to use.  If there was one, it is in
      NB. objtype/objval.  But a global search may still be needed: if there was
      NB. an object locative, or if the local search failed.  This search will start in locale gloc.
      NB. This search, if performed, must succeed, and we will convert the result to a type/(rank if verb)
      if. #glopart do.  NB. If was not resolved in table either as (simple, or immed locative)
        NB. First, see if this global name was assigned in this sentence.  If so, use that value
        if. (<objloc =. glopart , '_' , (>gloc) , '_') e. {."1 defnames do.
          NB. Name is in our local table.  Use that
          'objtype objval' =. 1 2 { (({."1 defnames) i. objloc) { defnames
        else.
          savloc =. coname''
          NB.?lintonly savloc =. <'dissect'
          NB.?lintmsgsoff
          cocurrent gloc
          NB.?lintmsgson
          NB. Get the value: the value itelf for a noun; the linear rep for others
          select. objtype =. 4!:0 :: _2: <glopart
          case. 0 do.
            objval =. ". glopart
          case. 1;2;3 do.
            objval =. rankinv_dissect_ f. glopart
          case. do.
            objtype =. _1
          end.
          cocurrent savloc
        end.
      end.
      NB. Now objtype/objval are set.  If the name is a noun or verb, create a locale for it
      NB.?lintonly 'objtype objval' =. 0;0 0 0 0
      select. objtype
      case. 0 do.
        ntypeval =. createnoun qend;qend;(#queue);<objval  NB. Keep name, and save name for display
      case. 1 do.
        NB. adverb: handle the special code (currently only &.>)
        NB. If the value of the user name matches special code, expand it on the stack
        select. objval
        case. '&.>' do.
          NB. Create an adverb containing the &.>, with correct tokens.  We have to put the single
          NB. adverb on the stack, rather than conj+verb, to avoid a parse error (if we had
          NB. N0 V1 N2 on the stack, A N V N would execute the dyad but C V N V N would not and would
          NB. eventually execute verb N0 erroneously)
          ntypeval =. adv ; ((conj;'&.';(#queue)) ,: createverb (,'>');(0$0)) ; $0
        case. '"_' do.
          NB. Create an adverb containing the "_, with correct tokens.  We have to put the single
          NB. adverb on the stack, rather than conj verb, to avoid a parse error (if we had
          NB. N0 V1 N2 on the stack, A N V N would execute the dyad but C V N V N would not and would
          NB. eventually execute verb N0 erroneously
          ntypeval =. adv ; ((conj;(,'"');(#queue)) ,: createnoun (,'_');'';(0$0);_) ; $0
        case. do.
          NB. If the value of the user name matches a supported primitive, replace the name by the supported value
          ntypeval =. adv;(((<objval) +./@:((e.>)"0) dissectprimindex) {:: qend;objval);(#queue)
        end.
      case. 2 do.
        ntypeval =. conj;(((<objval) +./@:((e.>)"0) dissectprimindex) {:: qend;objval);(#queue)
      case. 3 do.
         NB. If the verb has a one-line definition, pass that into the definition for tooltip purposes
        ntypeval =. createverb qend;(#queue);glopart;gloc;objval;loc;(#defnames)
      case. 0+256 do.  NB. Special type: name previously assigned which in ignore assignment mode
        failmsg 'Undissectable sentence: the name ''' , qend , ''' was previously assigned in this sentence, but assignments are ignored',((qopt 'fromdebugger') # ' when dissect is called from the debugger'),'.'
        return.
      case. do.
        failmsg 'Undissectable sentence: undefined name ' , qend
        return.
      end.
      
      NB. Make the stack entry for the new name
      stack =. ntypeval,stack

    elseif. do.
      NB. Must be a primitive.  Get its type and stack it
      try. 
        ". 'exeobj =. ' , qend
        select. 4!:0 <'exeobj'
        case. 0 do.   NB.?lintonly exeobj =. 0
          ntypeval =. (<sdt+noun) 0} createnoun qend;'';(#queue);<exeobj
        case. 1 do.
          ntypeval =. (sdt+adv);qend;(#queue)
        case. 2 do.
          ntypeval =. (sdt+conj);qend;(#queue)
        case. 3 do.
          ntypeval =. (<sdt+verb) 0} createverb qend;(#queue)
        case. do.
          failmsg 'Undissectable sentence: invalid type for primitive'
          return.
        end.
      catch.
        failmsg 'Undissectable sentence: invalid word in sentence: ' , qend return.
      end.
      stack =. ntypeval,stack
    end.
  end.
  if. pline < 9 do. errstartpoint =. _2 {. }. queue end.
end.   NB. End of loop processing stack.  top of stack is a mark
NB. verify that the sentence has a valid finish: 1 noun.
if. 1 1 -.@-: * (noun,mark) bwand >(<1 2;0){stack do.
  failmsg usermodifierencountered {:: 'Undissectable sentence: sentence did not produce a noun result';'Undissectable sentence: sentence does not seem to produce a noun result.  Dissect assumes that user modifiers produce verb results.'
  return.
end.

NB. The locale at the top of the stack is the overall result.  Save that, and return the instrumented sentence
NB. This call will fill in all the verb-to-noun locale references
resultroot =: (<1 1) {:: stack
NB.?lintonly resultroot =: <'dissectmonad' [ scrollinglocale =: <'dissectobj'
QP^:DEBTIME'endparse=?6!:1'''' '
NB. Decide where nilads should have inputs displayed: 2:"0 etc
calcdispniladinputs__resultroot 0
NB. Init the instance variables from the defaults in the dissect locale
NB. This also sets the values in the form.  We pass in the display options so they can be used
applyconfig dispoptions
NB. Init the displayshowstealth in every object.  Must be refigured if we change dispstealth
calcdispstealth__resultroot displayshowstealth # 1 2

NB. Create the string to execute.  If we have to create a sandbox, do so
NB. The raw sentence has the user's tokens, but the the invisible ones removed (for noassign sentences).
vissentence =. ; (<: /:~ ; ((1;1)&{::"1 # 0&{"1) > gettokenlevels__resultroot '')&{&.;: usersentence
execsentences_dissect_ =: vissentence ;^:('no' -.@-: qopt 'check') ,<exestring__resultroot''
if. qopt 'sandbox' do.
  NB. create the sandbox verb in the user's locale
  createsandbox defnames
  (('(' , ] , ';' , (quote >loc) , ') sandbox_dissect_ ' , quote@[)&.> '01' {.~ -@#) execsentences_dissect_
  NB.?lintsaveglobals
else.
  execsentences_dissect_
end.
NB.?lintsaveglobals
)

NB. Tools to read options
NB. x is list of keys, y is keyed list, result is y with x lines removed
subkl =: -.@(e.~ {:"1) # ]

NB. y is name of option, a string
NB. Result is unboxed first matching value in dispoptions, or x if not found - default 0
qopt =: 3 : 0
0 qopt y
:
(({."1 dispoptions) i. <y) {:: ({:"1 dispoptions) , <x
)

NB. y is name of option, a string
NB. Result is list of all matching boxes in dispoptions
qoptb =: 3 : 0
(({."1 dispoptions) = <y) # {:"1 dispoptions
)

NB. y is table of names
NB. We create globals in the dissect locale that describe the names to be
NB. created in the sandbox, and the string DEFSTRING_dissect_ that will
NB. cause them to be defined.  No result.
NB. The sandbox must be executed right after it is created, without an intervening return
NB. to immex, because we use globals to hold the variable-names and -values
createsandbox =: 3 : 0
defnounmask =. (<0) = 1 {"1 y
NOUNNAMES_dissect_ =: defnounmask # 0 {"1 y
NOUNVALUES_dissect_ =: defnounmask # 2 {"1 y
DEFSTRING_dissect_ =: (*#NOUNNAMES_dissect_) # '(NOUNNAMES_dissect_) =. NOUNVALUES_dissect_',LF
DEFSTRING_dissect_ =: DEFSTRING_dissect_ , ; ([ , ' =. ' , LF ,~ ])&.>/"1 (0 2) {"1 (-. defnounmask) # y
''
NB.?lintsaveglobals
)

NB. x (1 to delete globals;the locale to run in), y is the sentence to execute
sandbox =: 4 : 0
NB.?lintonly x =. 0;'dissectobj'
cocurrent 1{x
NB. Create sentence to define/delete names, and call it x.  We could simply do this all on the line that
NB. executes y, but that clutters the debug display with a long sentence 
x =. (quote (0{::x) # 'NOUNNAMES_dissect_ NOUNVALUES_dissect_ DEFSTRING_dissect_') , '(4!:55@[    DEFSTRING_dissect_ 0!:100@[ 4!:55)&;: ''x y'''
NB. Starting value of y (the sentence to be executed) is stacked before x is executed
NB. delete x and y
NB. run DEFSTRING 
NB. delete DEFSTRING etc. (optional)
NB. execute y, and that gives the result
". y [ ". x
)


NB. Here to execute a modifier.  We do that when we encounter a modified verb.
NB. This will be from VN A or VN C VN, but the A in VN A might be a compound adverb.
NB. We detect that if the adverb's value is boxed rather than a string.  In that case we
NB. recur on the parts of the adverb.
NB. y is the exeblock.
NB. Result is a line to put on the stack, coming from the execution of the modifier
execmod =: 3 : 0
exeblock =. y
NB. If the modifier's value is boxed, it is a compound modifier, necessarily bident.  We will classify it as
NB. C VN, VN C, or A A, and execute it as is appropriate.
if. 32 = 3!:0 modblock =. (<1 1) {:: exeblock do.
  select. (conj bwand 0 {::"1 modblock) i. conj
  case. 0 do. NB. C VN
    ntypeval =. execmod ({.exeblock),modblock
  case. 1 do. NB. VN C
    ntypeval =. execmod modblock,{.exeblock
  case. do.  NB. A A
    ntypeval =. execmod (execmod ({.exeblock),:{.modblock) ,: {: modblock
  end.
  NB. We have guaranteed that every modifier line of modblock has been executed, but any tokens in the second
  NB. line of exeblock have not been assigned to any modifier.  These must be parentheses that wrapped the compound
  NB. modifier.  We will assign them to the top-level modifier that we just created.
  NB. We need to assign only the parentheses, i. e. not the tokens for the verb, since the verb will be added
  NB. when the sentence is traversed
  topmod =. 1 {:: ntypeval
  NB.?lintmsgsoff
  tokensource__topmod =: tokensource__topmod , ((<1 2) {:: exeblock) -. 2 {:: ntypeval
  NB.?lintmsgson
else.
  NB. Not sdt.  Look up the locale of the modifier, and execute it.  The executed modifier must correctly guess the
  NB. part of speech that is going to be produced.
  ntypeval =. exeblock (0 createmodifier)~ 'dissectprim' , ": ifound =. ((<1 1) { exeblock) i.&1@:((e.>)"0) dissectprimindex
  NB. If the modifier was not one that we recognize, remember that, in case of parse failure
  usermodifierencountered =: usermodifierencountered >. ifound = #dissectprimindex
NB.?lintonly nobj =. localedefault
end.
NB. In case of parsing error, we collect the tokens contributing to this node
ntypeval =. (< ; 2 {"1 exeblock) 2} ntypeval
ntypeval
)

NB. Values for scrolltype, which tells what the user is doing
'SCROLLTYPENONE SCROLLTYPEIMAGE SCROLLTYPESCROLLBAR SCROLLTYPESIZEDATA SCROLLTYPEWIREHIGH' =: i. 5

NB. Following lines must match the menus!
MAXNOUNPCTCHOICES =: 5 10 20 30 40 50 60 70 80
DISPLAYPRECCHOICES =: 1 2 3 4 5 6 7 8 9

MAXEXPLORERDISPLAYFRAC =: 0.8   NB. Amount of screen to allow for nouns in explorer

NB. The tooltip size will be selected according to detail and expanded according to fontsize
ISISIZEPERTTPOINT =: _2 ]\ 0 0   3 20   25 35   30 60   50 120
MINIMUMISISIZE =: 200 200     NB. minimum size for graphics - low to allow small screen
TOOLTIPMAXPIXELS =: 900  NB. Max width of tooltip, in pixels
TOOLTIPMAXFRAC =: 0.6  NB. Max tooltip width, as frac of isigraph width

TOOLTIPDELAYCHOICES =: ('immed';'250';'500';'1000') ,. ('immediate';'0.25 sec';'0.5 sec';'1 sec') ,. <"0 (1 250 500 1000)
TOOLTIPDETAILCHOICES =: ('0';'1';'2';'3';'4') ,. ('none';'one line';'laconic';'verbose';'tutorial') ,. <"0 (0 1 2 3 4)

preclines =. ; <@('menu fmprec' , ": , ' "' , ": , '";' , LF"_ )"0 DISPLAYPRECCHOICES
sizexlines =. ; <@('menu fmmaxnounsizex' , ": , ' "' , ": , '%";' , LF"_ )"0 MAXNOUNPCTCHOICES
sizeylines =. ; <@('menu fmmaxnounsizey' , ": , ' "' , ": , '%";' , LF"_ )"0 MAXNOUNPCTCHOICES
ttdlines =. ; ('menu fmtooltipdelay' , [ , ' "' , ] , '";' , LF"_ )&.>/"1 (2) {."1 TOOLTIPDELAYCHOICES
ttdetlines =. ; ('menu fmtooltipdetail' , [ , ' "' , ] , '";' , LF"_ )&.>/"1 (2) {."1 TOOLTIPDETAILCHOICES
DISSECT=: ((,&LF&.> 'rem prec;';'rem sizex;';'rem sizey;';'rem ttdlines;';'rem ttdetlines;') ,. preclines;sizexlines;sizeylines;ttdlines;ttdetlines) stringreplace 0 : 0
pc dissect;
menupop "&File";
menu fmclosethis "Close this window and descendants";
menu fmclosefamily "Close this and all related windows";
menu fmcloseallbutthis "Close windows unrelated to this";
menu fmcloseall "Close all windows";
menupopz;
menupop "&Preferences";
menupop "&Fonts";
menu fmfontvalues "Select font for values...";
menu fmfontheadings "Select font for headings...";
menu fmfontimsgs "Select font for messages...";
menu fmfontttips "Select font for tooltips...";
menu fmfontstatus "Select font for status line...";
menupopz;
menupop "Display precision for floats";
rem prec;
menupopz;
menu fmshowstealth "Show ][ 0-9:";
menu fmshowcompmods "Show full compound-names";
menu fmshowstructmods "Show @ @: hook fork etc";
menu fmautoexpand2 "Show u/ on 2 items as dyad";
menu fmshowfillcalc "Show when argument with empty frame is replaced by a cell of fills";
menusep;
menu fmshowtipoftheday "Show a helpful Tip at startup";
menupopz;
menupop "&Sizes";
menupop "Max Block &Width as % of Screen";
rem sizex;
menupopz;
menupop "Max Block &Height as % of Screen";
rem sizey;
menupopz;
menupopz;
menupop "&Tooltips";
menupop "Delay";
rem ttdlines;
menupopz;
menupop "Detail";
rem ttdetlines;
menupopz;
menusep;
menu fmtooltipctrl "Show tooltips only while CTRL pressed";
menupopz;
menupop "&Config";
menu fmsaveconfig "Save current settings";
menu fmapplyconfig "Revert to saved settings";
menupopz;
menupop "&Help";
menu fmhelplearning "Learning Dissect";
menu fmhelpusing "Using Dissect";
rem menusep;
rem menupop "&Labs";
rem menu fmlab1 "Introduction to Dissect";
rem menu fmlab2 "Advanced Dissect";
rem menusep;
rem menupopz;
rem menu fmwikidissect "View Wiki Page" "F1";
rem menu fmwikinuvoc "View NuVoc Page" "Shift+F1";
rem menusep;
rem menu fmshowtip "Show a Tip";
menupopz;
xywh 3 4 20 18;cc fmshowerror button;cn "<<";
xywh 26 4 20 18;cc fmbwd button;cn "<";
xywh 48 4 20 18;cc fmfwd button;cn ">";
xywh 74 4 120 18;cc fmstatline static rightmove;
xywh 3 23 200 200;cc dissectisi isigraph rightmove bottommove;
pas 0 0;
rem form end;
)

DISSECT =: (((,&LF&.> 'rem prec;';'rem sizex;';'rem sizey;';'rem ttdlines;';'rem ttdetlines;') ,. preclines;sizexlines;sizeylines;ttdlines;ttdetlines) stringreplace 0 : 0) [^:IFQT DISSECT
pc dissect;
menupop "&File";
menu fmclosethis "Close this window and descendants";
menu fmclosefamily "Close this and all related windows";
menu fmcloseallbutthis "Close windows unrelated to this";
menu fmcloseall "Close all windows";
menupopz;
menupop "&Preferences";
menupop "&Fonts";
menu fmfontvalues "Select font for values...";
menu fmfontheadings "Select font for headings...";
menu fmfontimsgs "Select font for messages...";
menu fmfontttips "Select font for tooltips...";
menu fmfontstatus "Select font for status line...";
menupopz;
menupop "Display precision for floats";
rem prec;
menupopz;
menu fmshowstealth "Show ][ and 0-9:";
menu fmshowcompmods "Show full compound-names";
menu fmshowstructmods "Show @ @: hook fork etc";
menu fmautoexpand2 "Show u/ on 2 items as dyad";
menu fmshowfillcalc "Show when argument with empty frame is replaced by a cell of fills";
menusep;
menu fmshowtipoftheday "Show a helpful Tip at startup";
menupopz;
menupop "&Sizes";
menupop "Max Block &Width as % of Screen";
rem sizex;
menupopz;
menupop "Max Block &Height as % of Screen";
rem sizey;
menupopz;
menupopz;
menupop "&Tooltips";
menupop "Delay";
rem ttdlines;
menupopz;
menupop "Detail";
rem ttdetlines;
menupopz;
menusep;
menu fmtooltipctrl "Show tooltips only while CTRL pressed";
menupopz;
menupop "&Config";
menu fmsaveconfig "Save current settings";
menu fmapplyconfig "Revert to saved settings";
menupopz;
menupop "&Help";
menu fmhelplearning "Learning Dissect";
menu fmhelpusing "Using Dissect";
menusep;
menupop "&Labs";
menu fmlab1 "Introduction to Dissect";
menu fmlab2 "Advanced Dissect";
menupopz;
menusep;
menu fmwikidissect "View Wiki Page" "F1";
menu fmwikinuvoc "View NuVoc Page" "Shift+F1";
menusep;
menu fmshowtip "Show a Tip";
menupopz;
bin vhh0;
cc fmshowerror button;cn "<<";set fmshowerror wh 40 36;
set fmshowerror tooltip Go back to initial selection;
cc fmbwd button;cn "<";set fmbwd wh 40 36;
set fmbwd tooltip Undo selection;
cc fmfwd button;cn ">";set fmfwd wh 40 36;
set fmfwd tooltip Redo selection;
cc fmspacer static;cn "";set fmspacer wh 4 36;
bin zh1;
minwh 200 36;cc fmstatline static;
bin zz;
minwh 50 140;cc dissectisi isidraw;
bin z;
pas 0 0;
rem form end;
)

NB. wd covers
wdsetitems =: ([: wd 'set ', [ , ' *' , ])`([: wd 'set ', [ , ' items *' , ])@.IFQT
wdsettext =: ([: wd 'set ', [ , ' *' , ])`([: wd 'set ', [ , ' text *' , ])@.IFQT
wdsetvalue =: ([: wd 'set ', [ , ' *' , ])`([: wd 'set ', [ , ' value *' , ])@.IFQT
wdsetselect =: ([: wd 'setselect ', [ , ' ' , ])`([: wd 'set ', [ , ' select ' , ])@.IFQT
wdsetcaption =: ([: wd 'setcaption ', [ , ' *' , ])`([: wd 'set ', [ , ' caption *' , ])@.IFQT
wdsetshow =: ([: wd 'setshow ', [ , ' ' , ])`([: wd 'set ', [ , ' show ' , ])@.IFQT
wdsetfocus =: ([: wd 'setfocus ', ])`([: wd 'set ', ' focus' ,~ ])@.IFQT
wdsetenable =: ([: wd 'setenable ', [ , ' ' , ])`([: wd 'set ', [ , ' enable ' , ])@.IFQT
wdsetxywh =: ([: wd 'setxywhx ', [ , ' ' , ":@])`([: wd 'set ', [ , ' wh ' , [: ": _2 {. ])@.IFQT
wdqform =: ([: wd 'qformx'"_)`([: wd 'qform'"_)@.IFQT
wdqchildxywh =: ([: wd 'qchildxywhx ' , ])`([: wd 'qchildxywh ' , ])@.IFQT
wdsetfont =: ([: wd 'setfont ', [ , ' ' , ])`([: wd 'set ', [ , ' font ' , ])@.IFQT
wdpmove =: ([: wd 'pmovex ' , ])`([: wd 'pmove ' , ])@.IFQT
3 : '(glfontextent_jgl2_ =: glfont_jgl2_)^:0 (0)'^:(0 > 4!:0) <'glfontextent_jgl2_'  NB. defined in 8.03
NB. JVERSION is not defined at startup script on J6, so use a null string for that
wdmbfont =:  ([: wd 'mbfont ' , ]) ` (([: wd 'mb font ' , ])`([: wd 'mb font ' , [ , ' ' , ])@.({. 0 = /: 1 4 7 ,:  _3 {. , 0&".;._2 '.' ,~ 's' -.~ '/' taketo 'Qt IDE:' takeafter ". 'JVERSION')) @. IFQT
JEVERSION =: _3 {. , 0&".;._2 '.' ,~ 's' -.~ LF taketo 'Library:' takeafter ". 'JVERSION'
NB. return 1 if JE version is at least y
JEversionatleast =: 3 : '0 = {. /: y ,: JEVERSION'

NB. timer covers.  On 602 we have a single global, shared by all instances, indicating which locale the
NB. timer is running for.  Kludge, but seemingly OK since only one locale can have focus.  Called
NB. in the locale of the desired return
NB. wdtimer is the name defined in dissect locale, depending on IFQT.  The same name is copied to the instance locale
NB. if tooltips are enabled for non-QT
wdtimer =: 3 : 0
runningtimerloc_dissect_ =: COINSTANCE  NB. sys_timer will emulate a ptimer call
runningtimerloc__COINSTANCE =: coname ''
wd 'timer ' , ": y
NB.?lintsaveglobals
)
wdtimer =: wdtimer f. ` (3 : 0)@.IFQT
runningtimerloc__COINSTANCE =: coname ''
wd 'psel ' , winhwnd__COINSTANCE
QP^:DEBMOUSE'settimer:y=?y >coname'''' winhwnd '
wd__COINSTANCE 'ptimer ' , ": y  NB. Make sure the timer call is in instance locale
wd 'psel ' , winhwnd
)

NB. Set the initial value to use for the form, so we remember it from call to call
NB. Called whenever we think the control might have moved
setsessionyx =: 3 : 0
dissectionlist_dissect_ =: (<1 0 { 0 ". wdqform'') (<(({."1 dissectionlist_dissect_) i. coname''),1)} dissectionlist_dissect_
)
NB. Get the starting value
getsessionyx =: 3 : 0
((({."1 dissectionlist_dissect_) i. coname''),1) {:: dissectionlist_dissect_
)

NB. ************** start of the display section (after the parse is over) ****************

NB. Either nodisplay or display is always called.  The clearing of dissectinstance is a way to prevent recursion.
NB. y is the error message, which we pass through
NB. Always called in dissect locale
nodisplay =: 3 : 0
destroy__dissectinstance ''   NB. This will clear dissectinstance and restore
y
)

display =: 3 : (('catch.';'catchd.') stringreplace^:DEBNOCATCH_dissect_ 0 : 0)   NB. called in dissect locale
QP^:DEBTIME'startdisplay=?6!:1'''' '
if. #dissectinstance do.
  try.
    displaymain__dissectinstance y
    if. returnobject do. ret =. dissectinstance else. ret =. 0 0 $0 end.
    dissectinstance =: 0$a:
    QP^:DEBTIME'enddisplay=?6!:1'''' '
    NB. Normal return: quiet return, unless user asked for the object id
    restoreJenvirons''
    ret
  catch.
    NB. error not coming through failmsg: error
    smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
    smoutput 13!:12''
    destroy__dissectinstance''
    'Error during initial display'
  catcht.
    NB. Error, with emsg set by failmsg
    destroy__dissectinstance''
    errormessage
  end.
else.   NB. user tried recursive execution; dissectinstance is now empty
  'Vivisection is illegal.'
end.
)


NB. lay out the grid.  Should be called in the locale of the major instance
NB. Sets globals   topinfo (top drawing info)
NB. This must be called whenever a display parameter changes, to resize the sentence and drawing.
NB. y is the screensize yx
calcplacement =: 3 : 0
NB.?lintonly satzcfm =: 4 3$a:
NB. Get the size of the sentence and links.  Remember the layout of the sentence
topinfo =: usersentence sizesentence gettokenlevels__resultroot ''
NB. Check the current screensize, and calculate the box sizes in pixels
maxnoundisplaysizes =: <. (maxnoundisplayfrac ,: MAXEXPLORERDISPLAYFRAC) *"1 y

NB. Select our drawing control, so we have something to check sizes with
wd 'psel ',winhwnd
glsel 'dissectisi'
NB. Traverse, producing the final DOL
NB. Add on the display for the final collector
endingloc =. (<0 0) { joinlayoutsl traverse 0
NB.?lintonly endingloc =. <'dissectobj'
NB. calculate grouping hierarchy
NB. place the blocks
'dl dyxhw dwires' =. establishgrouping__endingloc ''
NB. create wiring and revise placement
NB. create the parameters for the router: the brick of start/end+1, and the
NB. list of nets, in the form <source,dest,dest...
NB. where each point is  obj#,face#,fraction
dyxbr =. +/\"2 dyxhw  NB. Convert to start/end+1; adjust to leave top & bottom margin
wirenets =. (<@((<0 0)&{ , {:"2)/.~ {."2) dwires  NB. Convert nets (same source) to net form
NB.  size is ymax,xmax
NB.  gridsize is spacing between lines
NB.  standoff is min distance between a block and a line
NB.  penalties is penalty for a turn (in units of movement)
QP^:DEBTIME'startrouter=?6!:1'''' '
NB. obsolete start   =. 6!:1''
routeresult =. dl ; routegrid dyxbr;<wirenets
NB. obsolete if. 0.4 < dur   =. start -~ 6!:1'' do. 'Routing time: %6.1f' PR dur end.
NB. Now that we have traversed, replace the locale-names with the inheritedtail, so that selection in the sentence moves to the locale
NB. that is actually formatted for display
sentenceinfo =. (<1 1) {:: topinfo
topinfo =: (<sentenceinfo =. (<locs =. ((3 :'findinheritedtail__y :: ((<0)"_)$0')"0 (3) {:: sentenceinfo)) 3} sentenceinfo) (<1 1)} topinfo
NB. Gray out any words of the sentence that do not have a valid display block
ndl =. 3 : 'if. y = <0 do. 1 else. 0 = #DOsize__y end.'"0 locs
topinfo =: (< (< satzcfm (<"1@[ { ])~ ndl ,.~ 0 {:: sentenceinfo) 0} sentenceinfo) (<1 1)} topinfo
routeresult
NB.?lintsaveglobals
)


mismatchmsg =: 0 : 0
Dissect's result from running the sentence did not match the result from running the sentence in the J session.  There are 3 possible reasons:

1. Your sentence is irreproducible, because it uses random numbers (the ? verb) or inconstant external data, or it has side effects.

2. The J interpreter has a bug executing your code that does not show up when executing Dissect's sentence, which is slightly different.

3. Dissect has a bug.

Do you want Dissect to display its result anyway?
)

NB. Useful nugget to suggest at startup
tiplist =: <@(LF ('*' I.@:= ])} ]);._2 (0 : 0)  NB. first line is next to display
to scroll the display, click-and-hold in an empty area, then move the mouse.
click-and-hold on a wire to highlight its entire network.
hover over a word in the sentence at the top of the screen to highlight its block.
click on a word in the sentence at the top of the screen to focus on its block.
you can choose how big the data blocks are with the Sizes menu.
use the Config menu to save the current settings as defaults for future executions of dissect.
when a block has a bottom-right resize handle, drag the handle to resize the block.
if the data won't fit in a block, you can right-click on the data to get a fullscreen window on the data.
to look inside a named verb, select a single result-cell and right-click the name.*You will get a new window (either dissect or debug).
if a block shows no results, you might need to select a single result-cell of a later verb.
when a display block contains the results of more than one verb*(as in +"1"2 which displays results of +"2, +"1, and +),*you may click more than once to select a result-cell of successive verbs.
the arguments contributing to a selection are outlined in the same color as the selection.
the word producing a selection is color-coded in the sentence.
the smallest selected cell is indicated with a heavy outline.
] and [ are not normally shown, but you can change that in the Preferences menu.
the << < > buttons allow you to undo and redo selections.
the shape of a result is shown just above the values.
the shape of the result-cells of a verb is shown in purple in the shape line.
the frame of the result is shown in the shape line before the purple result-cell shape.*The frame is color-coded as selections are made.
an empty result-cell is shown as a gray rectangle.
a value with leading 1 in the shape is shown in italics.*Examples: a one-atom list; an array with shape 1 4
when you make a selection, the path to the selection is shown below the shape.
a parenthesized value at the end of the shape indicates the presence of framing fill.
atoms added by framing fill are crosshatched.
the cell where an error occurred is doubly crosshatched.
the numbers to the side(s) of verb-names are the rank(s) of the cells the verb was applied to.
if the message displayed by a click is too small, click again for a bigger display.
use the Preferences menu to set the font and sizes for display components.
when a block is outlined in red, it means not all the data fit into the display.
if your sentence has an error, dissect automatically selects to the point of error.
if you have clicked in the dissection of a sentence with error, the << button will reset selections to the point of error.
you can set a PF key to dissect a sentence from a script or session log.
you can set the debugger to dissect sentences automatically when you stop on them.
dissect ignores control words, so you can dissect 'if. condition do.' as is. 
hidden computation, such as in u/, u^:n, or u1`u2@v, can be seen by clicking in the result*to create an expansion block that shows the hidden computation.
results of different executions in an expansion block are each boxed so they can be collected into a single value. 
> in the shape or selection line indicates that an additional level of boxing has been added for display purposes.
when a value has more than 2 axes, they alternate horizontal and vertical directions.*Blue lines in the data separate cells of rank 2 and up.
when the cursor is over a value, the status line (above the main display) shows the index list and boxing path to the atom under the cursor.
left-click on a word in the block titles to see the NuVoc page for it.
right-click on the shape of a block (above the data) to copy the data to the clipboard.
when a cell is selected, right-click in the selection line (below the shape) to copy the cell to the clipboard.
buy low; sell high.
) 

NB. y is the results from running the user's original sentence and our instrumented version.
displaymain =: 3 : 0  NB. called in dissectinstance locale
NB. Make sure the results are the same
NB. If the sentence ran correctly for the user, make sure we get the same result.
NB. If there is only one result, we don't compare (comparisonlevel must have suppressed uninstrumented execution)
if. 1 < #y do.
  select. 'all' qopt 'check'
  case. 'all' do.  NB. compare for equality (should it be intolerant?  Naah - there may be shortcuts)
    equal =. -:/ y
  case. 'shape' do.  NB. compare for equality of shape and type
    equal =. (-:&({.@(0&($,)) L:0) *. -:&($ L:0))/ y
  case. do.  NB. must be 'error' - compare for equality of failure
    equal =. =&(0 = #)&>/ y
  end.
  if. -. equal do.
QP^:NOCLEANUP'y 13!:12$0 '
    if. '' -: '' qopt 'check' do.
      NB. If the user didn't specify a check, give him a chance to accept the mismatch
      if. IFQT do.
        continue =. 'yes' -: wd 'mb query mb_yes =mb_no "Dissect result mismatches J session" "',mismatchmsg,'"'
      else.
        continue =. 'YES' -: wd 'mb "Dissect result mismatches J session" "',mismatchmsg,'" mb_yesno'
      end.
      if. continue do.
        NB. If the user wants to continue, modify the display options so that subdissections will inherit the choice
        dispoptions =: dispoptions , 'check';'no'
      else.
        failmsg 'dissect error: dissected sentence has incorrect result'
        return.
      end.
    else.
      failmsg 'dissect error: dissected sentence has incorrect result'
      return.
    end.
  end.
end.

NB. For forced tooltips (clicks on unexpandable data), we enforce a minimum display time so that
NB. the user has a chance to see that there is a tooltip there.  This value is the session-timer value
NB. at which the display can be withdrawn.
hoversessmin =: 0

NB. save the crash indicator
if. crashed =. 0 = #0 {:: y do.
NB. Save the error message for the crash
  errormessagefrominterp =: _6 }.^:(' error' -: {.) (<:13!:11''){::9!:8''
else.
  errormessagefrominterp =: ''
end.

NB.?lintonly COCREATOR =: <'dissectobj'
NB. Create a selector that will admit anything, without infinities
TRAVNOUN =: 0;NORANKHIST;(0$a:);0 , >:ticket

NB. The argument of $0 indicates that we want to set the crash variables
NB. debug wd :: 0: 'psel dissect;pclose'
wd DISSECT
winhwnd =: wd 'qhwndp'
wd 'pn *Dissecting ' , usersentence
'fmstatline' wdsetfont ;:^:_1 ":&.> 4 {:: fontchoices
?~ >: <. {: 6!:0''  NB. deal some random numbers
tiplist =: (({~ ?~@#)@}: , {:) tiplist  NB. create local tiplist, randomly ordered
dissect_fmshowtip_button displayshowtipoftheday  NB. show a starting tip if enabled, and set showtip flag
setformconfig''

wdsetfocus 'dissectisi'

NB. Convert the logged values from high-speed-collecting form to analysis form (one box per result)
coalescealllogs__resultroot 0
NB. Initialize the parent-node indicator in every object, a: (=invalid) at the top
initparentnodes__resultroot <'dissectobj'  NB. root will point to harmless parent
NB. Init the SDT-detail indicator in every object
NB.?lintonly resultissdt_dissectmonad_ =: 0
initnounshowdetail__resultroot resultissdt__resultroot
NB. If we crashed, do an initial traversal to set selection flags to find the error
maxnoundisplaysizes =: 2 2$0  NB. Init to small display for sniff, for speed

NB. Initialize selection history.  This sets enables for the buttons too.  Sniff modifies the selections, so do this early.
initselections''

NB. If the user codes a verb whose result is never used (owing to stealth), and that verb crashes,
NB. we will never see the error (unless we have the wit to show ][).  We look out for that here.
NB. We create a flag that will be cleared if we display an error block.  If that is still set after the
NB. first display, we will turn on displayshowstealth and retry
needtocheckerrordisplayed =: crashed *. -. displayshowstealth
if. crashed do.
  joinlayoutsl traverse 1   NB. Don't forget the final display!
  setdisplayerror__resultroot''
end.
NB. The sniff may have set scrollpoints based on the tiny screen, so reset them all
propscroll__resultroot 0

NB. Save the size of the screen, which we will use to decide max noun size and scrollbar size.  h w
screensize =: 3 2 { 0 ". wd 'qscreen'
NB. Convert fraction-of-screen values to pixels
(,&'PIXELS'&.> nm) =: (<./ screensize) (* ".)&> nm =. 'SCROLLBARWIDTH';'SCROLLBARMINWIDTH';'SCROLLBARENDTHICKNESS'

NB. Set the initial selection of cells:
NB. If the result is an sdt, the user is probably noodling around with a new sentence, so select everything
NB. resultissdt__resultroot
NB. Do the initial traversal, calculate the initial placement.
placeddrawing =: calcplacement screensize
NB. Set the starting scroll position, just below the sentence/link
NB. We have to have a nominal y position so we can size the drawing.  We don't know
NB. the actual x placement, though, until we have sized the drawing and centered the
NB. title/sentence.
NB. scrolltlc is the amount to add to DO positions to get positions on the
NB. display; in other words the pixel position of the top-left corner
NB. of the routing layout area
scrolltlc =: 0 ,~ (2 + +)`>./ |. (0;0) {::"1 topinfo NB. y sizes of brects
NB. Init scrolling vbl in case the window system fails to give a left-click before leftdbl
lastscrollingtype =: scrollingtype =: SCROLLTYPENONE
NB. autosizestate is 1 initially, to get initial position
NB. 0 from then on to resize according to required size
NB. If user resizes, autosizestate goes negative to indicate that e has taken control
NB. We MUST resize before pshow, to get initial size right, because pre-J8.04
NB. releases called _resize immediately on pshow, rather than as a queued event
sizedrawingandform autosizestate =: 1
wd 'pshow'
NB. On J6, we will get an immediate event, but it might be a mouse-move.
NB. Make sure we have enough variables defined to make mouse-move harmless until
NB. the drawing is made
wirehighdisttocheck =: _ 0   NB. Normally defined in drawplacement
wirehighmousepos =: _1000 _1000
NB. On QT, resize event, which will call paint
0 0$0
NB.The initial paint event will draw the screen
NB.?lintsaveglobals
)


NB. y is 0 for normal traversal, or 1 for error traversal.  We initialize and traverse
NB. Called in instance locale
traverse =: 3 : 0
NB. Initialize for the traversal
errorlevelinit''
snifferror =: y
NB. We keep track of which monad/dyad execution is running, so that we can propagate all errors entirely
NB. through the tree for that exec
executingmonaddyad =: 0$a:
NB. Initialize the traversal stats: we use these to decide what options to offer the user
maxactualnounsize =: 0 0  NB. The number of pixels needed to show all nouns in their entirety
stealthopencountered =: 0   NB. Set if there is a stealth op or nilad on the display
NB. Before traversal, initialize DOyx to empty.  If the locale creates a layout, the layout will
NB. have a valid DOsize, but if not, because for some reason (error, stealth, or unexecuted agenda) the block is not
NB. displayed, we will know
setDOyxs__resultroot 0$0
NOLAYOUTS traverse__resultroot TRAVNOUN
NB.?lintsaveglobals
)

EXPANSIONROOMAROUNDISI =: 200 100  NB. Number of pixels to leave at margin, yx
NB. Size the isigraph and the parent, and size the drawing for display
NB. y is resizestate: positive for first call, zero for non-first call
NB. that has not resized, negative if user has taken control with resize.
NB. Globals topinfo, scrolltlc, placeddrawing have been set
NB. Result is the drawing (the result of sizeplacement)
NB. Side effect: the isigraph and parent are resized (up only) as required
NB. When we resize the isigraph, we include expansion room
sizedrawingandform =: 3 : 0
NB. Decide what we will automatically control: position, size
'initxy autosize' =. (y > 0) , (y >: 0)
NB. Get the required size - mostly the isi, but must be wide enough for the sentence/links too
NB. Minimum top height is max of (link height if any, title height + sentence height)
NB. Minimum top width is (max of title/sentence width) + link width (if any) + 2 if there are links
topminsize =. (>.`+/@[ , (2 + +)`>./@])/ |: |. >@:({."1) topinfo
NB. Remove the scrollpoint from the drawing size.  Here in automatic sizing we are handling
NB. only the size needed to draw the picture in normal position.  If the user offsets the display, we
NB. do not automatically change the window - he could have done that himself.  But we do make sure
NB. we leave enough room for the title/sentence display
yxneeded =. topminsize >. scrolltlc -~ 0 {:: shifteddrawing =. scrolltlc sizeplacement placeddrawing
NB. Get the current size of the isi; if insufficient, make it bigger, with expansion added
if. initxy +. autosize *. yxneeded +./@:> 2 3 { cyxhw =. 1 0 3 2 { 0 ". wdqchildxywh 'dissectisi' do.
  minisi =. MINIMUMISISIZE >. <. (0.8 * screensize) <. (tooltipdetailx{ISISIZEPERTTPOINT) * (<3 1) {:: fontchoices  NB. Tooltip never more than much of the screen
  NB. For QT, always size the canvas to the full screen
  cyxhw =. (minisi >. autosize * EXPANSIONROOMAROUNDISI + yxneeded) 2 3} cyxhw
  if. IFQT do. wd 'set dissectisi wh _1 _1'
  else. 'dissectisi' wdsetxywh 1 0 3 2 { cyxhw
  end.
  NB. If the main form has grown now that the isi has grown, resize it too.
  if. initxy do. xywh =. 1 0 3 2 { (getsessionyx'') , +/ 2 2 $ cyxhw
  else. xywh =. (0 ". wdqform'') >. 0 0 , |. +/ 2 2 $ cyxhw
  end.
  wdpmove ": xywh
  if. -. IFQT do. wd 'pas 6 6' end.  NB. JQt ignores pas
end.
NB. Now that we have the size of the isigraph, center the sentence in the screen area; remember brect
NB. Center the sentence, but make sure it doesn't overlap the links if any
NB. Stack the first two sentencesizes on top of each other, centered
titlesncebb =.  (,:~"1   (0,(<0 0)&{) ,. (0 >. (<.@:-:@:-~ |.))@:({:"1)) > (<0 1;0) { topinfo  NB. brects of title/sentence, before centering
centerxadj =. (0 >. 2 + >./ (<(<0 1);1) { >@:({."1) topinfo) >. ({: cyxhw) <.@-:@- >./ (<a:;1;1) { titlesncebb  NB. widths except first two
NB. First rect is sentence, moved to proper place; next (if any) is links
NB. These rects are yx,:hw
topbrect =: brect sb =. (titlesncebb +"2 (2 _2) {. centerxadj) , 0 0 ,:"1 (2) }. >@:({."1) topinfo
locpickrects =: sb (i.#sb)} locpickrects NB.?lintonly =: 3$a:
shifteddrawing
NB.?lintsaveglobals
)


NB. y is 1 for an internal call that needs to refigure the placement
dissect_dissectisi_paint =: 3 : (('catch.';'catchd.') stringreplace^:DEBNOCATCH_dissect_ 0 : 0)
NB. To avoid an error loop, terminate quietly if there is an error
try.
  NB. If we are highlighting a sentence, stop doing so
  sentencehoverend''
  NB. Establish local J environment.  The user's environment was saved when we started
  NB. if we need to refigure the placement because of a change like selection or a display parameter, do so.
  if. 1 = {. y do. placeddrawing =: calcplacement screensize end.
  NB. Draw the revised placement and wiring.  Save the placement to speed scrolling
  QP^:DEBTIME'startdraw=?6!:1'''' '
  drawplacement }. sizedrawingandform autosizestate
  NB. If we didn't put out the error message, show stealth and try again
  if. needtocheckerrordisplayed do.
    NB. set stealth visible and retry
    calcdispstealth__resultroot (displayshowstealth =: 1) # 1 2
    placeddrawing =: calcplacement screensize
    drawplacement }. sizedrawingandform autosizestate
    needtocheckerrordisplayed =: 0   NB. do this only once
  end.
  NB. Once we have made initial display, go into autosizing mode, until user resizes
  autosizestate =: autosizestate <. 0

  NB. If we are highlighting from the sentence, do so (it's possible, if the paint came from
  NB. as external source while we were hovering)
  sentencehoverdraw''

  glpaint''
  
  NB. Set the user-option buttons based on the display results
  NB. If the largest noun on the display is bigger than our smallest display option,
  NB. give the user the option of changing the display size
  actualpctused =. >. 100 * maxactualnounsize % screensize
  NB. Disable all choices that are two notches above the actual max size
  enablesz =. '01' {~ actualpctused >:"0 1 |.!.0 MAXNOUNPCTCHOICES  NB. Prepend 0 so that 9 eg will enable 10
  (0 { enablesz) (wdsetenable~   'fmmaxnounsizey' , ":)"0 MAXNOUNPCTCHOICES
  (1 { enablesz) (wdsetenable~   'fmmaxnounsizex' , ":)"0 MAXNOUNPCTCHOICES
  NB. If current selection not enabled, remove the confusing check
  ('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue (0 { maxnoundisplaysizex) { 0 { enablesz
  ('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue (1 { maxnoundisplaysizex) { 1 { enablesz

  NB. If there are stealth/nilad operands on the display, enable the button and caption it
  NB. according to whether we are displaying them.  Enable the button if
  NB. (we are suppressing stealth and we suppressed something) or
  NB. (we are allowing stealth, which might be a config default)
  'fmshowstealth' wdsetenable ": displayshowstealth +. stealthopencountered

  ('fm'&, wdsetvalue ":@:".@('display'&,))@> ;: 'showcompmods showstructmods autoexpand2 showfillcalc showstealth showtipoftheday' 
  ('fm'&, wdsetvalue ":@:".)@> ;: 'tooltipctrl' 
  
  NB. Restore the user's environment before returing to immediate mode
  restoreJenvirons''
catch.
  smoutput 'error in paint'
  smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput 13!:12''
  NB. destroy the failing locale in case of error.  If we are performing the initial display, this will also reset
  NB. dissectinstance so that we aren't corrupted next time
  destroy''
catcht.
  NB. If we get a failmsg, honor it
  destroy''
  errormessage_dissect_ return.
end.
)


NB. Clean up everything.  y is the return value to use.  If x is 1 (default 0), close regardless of CLEANUP
destroy =: 3 : 0
0 destroy y
:
NB.?lintmsgsoff
NB. dissectinstance is used only during the initial setup; when we return to wait for user interaction,
NB. it must be empty.  Here we clear it for the cases of nodisplay and of error during initial display
dissectinstance_dissect_ =: 0$a:
if. NOCLEANUP *. -. x do. '' return. end.
NB. Destroy all debug windows started here.  This saves the user the trouble of deleting them by hand
for_o. debuglocs do. if. 1 = 18!:0 o do. destroy__o '' end. end.
for_o. ~. objtable do. destroy__o '' end.
NB. Remove this locale from its parent, if it has one
if. a: ~: parentloc =. {. qoptb 'parent' do. debuglocs__parentloc =: debuglocs__parentloc -. coname'' end. 
NB.?lintmsgson
resultroot =: 0$a:
if. #winhwnd do.
  wd 'psel ' , winhwnd
  NB. Remember where the window was, so we can put the next one there
  setsessionyx''
  wd 'pclose'
  winhwnd =: ''  NB. not required
end.
if. (coname'') e. {."1 dissectionlist_dissect_ do.
  dissectionlist_dissect_ =: a: (<((coname'') i.~ {."1 dissectionlist_dissect_),0)} dissectionlist_dissect_
end.
if. ALLOWNONQTTOOLTIP *. -. IFQT do.
  NB. Turn off timer to avoid late interrupt
  runningtimerloc_dissect_ =: 0$a:
  wd 'timer 0'
end.
NB. Restore original session state.  We hope this hasn't been changed while we were running!
restoreJenvirons''
codestroy''
y
)

NB. Toggle the state of stealth display
dissect_fmshowstealth_button =: 3 : 0
'fmshowstealth' wdsetvalue ": displayshowstealth =: -. displayshowstealth
NB. The operand is the list of types that should NOT be displayed
calcdispstealth__resultroot displayshowstealth # 1 2
dissect_dissectisi_paint 1
)
NB. Toggle the state of compmod display
dissect_fmshowcompmods_button =: 3 : 0
'fmshowcompmods' wdsetvalue ": displayshowcompmods_dissect_ =: -. displayshowcompmods
dissect_dissectisi_paint 1
)
NB. Toggle the state of structmod display
dissect_fmshowstructmods_button =: 3 : 0
'fmshowstructmods' wdsetvalue ": displayshowstructmods_dissect_ =: -. displayshowstructmods
dissect_dissectisi_paint 1
)
NB. Toggle the state of structmod display
dissect_fmautoexpand2_button =: 3 : 0
'fmautoexpand2' wdsetvalue ": displayautoexpand2 =: -. displayautoexpand2
dissect_dissectisi_paint 1
)
NB. Toggle the state of fill-cell display
dissect_fmshowfillcalc_button =: 3 : 0
'fmshowfillcalc' wdsetvalue ": displayshowfillcalc =: -. displayshowfillcalc
dissect_dissectisi_paint 1
)
NB. Toggle the state of startup-tip display
dissect_fmshowtipoftheday_button =: 3 : 0
'fmshowtipoftheday' wdsetvalue ": displayshowtipoftheday_dissect_ =: -. displayshowtipoftheday
NB. No need to paint - it's for config only
)
NB. Toggle the state of tooltipctrl display
dissect_fmtooltipctrl_button =: 3 : 0
'fmtooltipctrl' wdsetvalue ": tooltipctrl_dissect_ =: -. tooltipctrl
)

dissect_close =: 1&destroy
dissect_cancel =: dissect_close
dissect_fmclosethis_button =: dissect_close

NB. Close all windows related to this one
dissect_fmclosefamily_button =: 3 : 0
NB. find the parent.  Closing it will close all the children
while. #parentloc =. qoptb 'parent' do.
  NB.?lintonly parentloc =. <'dissect'
  cocurrent {. parentloc
end.
dissect_close''
)

NB. Close windows unrelated to this one
dissect_fmcloseallbutthis_button =: 3 : 0
NB. Find the parent, and exempt it from the global close
while. #parentloc =. qoptb 'parent' do.
  NB.?lintonly parentloc =. <'dissect'
  cocurrent {. parentloc
end.
dissect_fmcloseall_button coname''
)
NB. Close all, except any locales given in y (must be top-level locales)
dissect_fmcloseall_button =: 3 : 0
NB. Find all windows without parents.  Closing them will close all
for_l. y -.~ (#~ 3 : '0 = #qoptb__y ''parent'''"0) (a: -.~ {."1 dissectionlist_dissect_) do.
  dissect_close__l''  NB.?lintonly [ l =. <'dissect'
end.
i. 0 0
)

NB. We use dissect locale for fontsize, because it is shared between instances
dissect_fmfontsize_button =: 4 : 0
NB. Get title to tell the user what he's doing.  Some versions don't display the title
title =. '"Select font to use for ' , x , '"'
if. #fontsel =. title wdmbfont ;:^:_1 ":&.> y { fontchoices do.
  NB. Extract font and size; convert size to numeric; discard adornments
  fontname =. '"' , '"' dropafter '"' takeafter fontsel
  fontsize =. 0 ". > {. ;: '"' takeafter '"' takeafter fontsel
  calccfms fontchoices_dissect_ =: (fontname;fontsize) y} fontchoices
  dissect_dissectisi_paint 1
end.
)
dissect_fmfontvalues_button =: 'values'&(dissect_fmfontsize_button 0:)
dissect_fmfontheadings_button =: 'headings'&(dissect_fmfontsize_button 1:)
dissect_fmfontimsgs_button =: 'messages'&(dissect_fmfontsize_button 2:)
dissect_fmfontttips_button =: 'tooltips'&(dissect_fmfontsize_button 3:)
dissect_fmfontstatus_button =: 3 : 0
'status line' dissect_fmfontsize_button 4
'fmstatline' wdsetfont ;:^:_1 ":&.> 4 {:: fontchoices
)

dissect_fmmaxnounsize_button =: 4 : 0
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '0'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '0'
maxnoundisplaysizex =: y x} maxnoundisplaysizex
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '1'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '1'
maxnoundisplayfrac =: 0.01 * MAXNOUNPCTCHOICES {~ maxnoundisplaysizex
dissect_dissectisi_paint 1
)
(4 : 0"0 i.@#) MAXNOUNPCTCHOICES
". 'dissect_fmmaxnounsizey' , (":x) , '_button =: 0&dissect_fmmaxnounsize_button@(', (":y) ,'"_)'
". 'dissect_fmmaxnounsizex' , (":x) , '_button =: 1&dissect_fmmaxnounsize_button@(', (":y) ,'"_)'
)

dissect_fmtooltipdelay_button =: 3 : 0
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0,~ tooltipdelayx) wdsetvalue  '0'
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0,~ tooltipdelayx_dissect_ =: y) wdsetvalue '1'
)
(4 : 0&> i.@#) 0 {"1 TOOLTIPDELAYCHOICES
". 'dissect_fmtooltipdelay' , x , '_button =: dissect_fmtooltipdelay_button@(', (":y) ,'"_)'
)

dissect_fmtooltipdetail_button =: 3 : 0
('fmtooltipdetail' , TOOLTIPDETAILCHOICES {::~ <0,~ tooltipdetailx) wdsetvalue  '0'
('fmtooltipdetail' , TOOLTIPDETAILCHOICES {::~ <0,~ tooltipdetailx_dissect_ =: y) wdsetvalue  '1'
)
(4 : 0&> i.@#) 0 {"1 TOOLTIPDETAILCHOICES
". 'dissect_fmtooltipdetail' , x , '_button =: dissect_fmtooltipdetail_button@(', (":y) ,'"_)'
)

NB. We do not use dissect locale for displayprecision, because it is not shared between instances
dissect_fmprec_button =: 3 : 0
('fmprec' , ": DISPLAYPRECCHOICES {~ displayprecisionx) wdsetvalue  '0'
('fmprec' , ": displayprecision =: DISPLAYPRECCHOICES {~ displayprecisionx =: y) wdsetvalue  '1'
dissect_dissectisi_paint 1
)
(4 : 0"0 i.@#) DISPLAYPRECCHOICES
". 'dissect_fmprec' , (":x) , '_button =: dissect_fmprec_button@(', (":y) ,'"_)'
)

dissect_fmsaveconfig_button =: 3 : 0
saveconfig''
)

dissect_fmapplyconfig_button =: 3 : 0
loadconfig_dissect_''  NB. get values from config file
applyconfig 0   NB. apply them to this instance, ignoring input options
setformconfig''   NB. put the values into the form
dissect_dissectisi_paint 1
)


dissect_fmhelplearning_button =: helpshow_dissecthelplearning_

dissect_fmhelpusing_button =: helpshow_dissecthelpusing_

dissect_fmlab_button =: 3 : 0
try.
  NB. See if labs are installed
  require 'labs/labs'
  NB. Run our lab - will give message of not found
  NB.?lintmsgsoff
  lab_jlab_ y
  NB.?lintmsgson
catch.
  wdinfo 'Labs Addon Required';'To get the J Labs, use Package Manager and select labs/labs'
end.
0 0$0
)
dissect_fmlab1_button =: 3 : 0
dissect_fmlab_button '~addons/labs/labs/debug/dissect1.ijt'
)
dissect_fmlab2_button =: 3 : 0
dissect_fmlab_button '~addons/labs/labs/debug/dissect2.ijt'
)

dissect_fmwikidissect_button =: 3 : 0
NB.?lintonly browse_j_ =. 3 : 'y'
browse_j_ JWIKIURL,'Vocabulary/Dissect'
0 0$0
)
dissect_f1_fkey =: dissect_fmwikidissect_button

dissect_fmwikinuvoc_button =: 3 : 0
NB.?lintonly browse_j_ =. 3 : 'y'
browse_j_ JWIKIURL,'NuVoc'
0 0$0
)
dissect_f1shift_fkey =: dissect_fmwikinuvoc_button
4!:55^:(-.IFQT) 'dissect_f1_fkey';'dissect_f1shift_fkey'

dissect_jctrl_fkey =: 3 : 0
if. 0 < 4!:0 <'lab_jlab_' do.
  9!:27 'lab_jlab_ 0'   NB. create immex sentence
  9!:29 (1)
end.
0 0 $0
)

dissect_fmshowtip_button =: 3 : 0
if. y -: 0 do.  NB. first display with tip suppressed
  statlinehasnotip =: 1 
  'fmstatline' wdsettext ''
else.
  statlinehasnotip =: 0  NB. Indicate there is a tip to preserve
  'fmstatline' wdsettext 'Tip: ' , _1 {:: tiplist =: 1 |. tiplist
end.
)

dissect_dissectisi_char =: 3 : 0
NB. If user presses ctrl-J, treat it as 'labrun'
NB.?lintonly sysdata =. 20 0 146 { a.
select. a. i. ,sysdata
case. ,10 do.  NB. ctrl-J, now handled in main form
case. 239 160&,&.> 146 + i. 4 do.  NB. arrow: l u r d
  NB. Arrow key: move the selection
  'axis incr' =. (146 -~ a. i. 2 { sysdata) { _2 ]\ 0 _1  1 _1  0 1  1 1   
  if. movelastselection axis,incr do. dissect_dissectisi_paint 1 end.
case. (,20)&+&.> i. 4 do.  NB. arrow: l u r d in J6
  NB. Arrow key: move the selection
  'axis incr' =. (20 -~ a. i. {. sysdata) { _2 ]\ 0 _1  1 _1  0 1  1 1   
  if. movelastselection axis,incr do. dissect_dissectisi_paint 1 end.
end.
0 0$0
)

NORANKHIST =: 0 2$a:
NORANKHISTNOUN =: ''   NB. string means noun

NB. Conjunction for tree traversal down (i. e. root to leaves)
NB. y is the argument to pass to the first node
NB. x is the traversal type: 0 to stop before nouns, 1 to go through conjunction noun operands, 2 to go through everything, 3 to get tokens for the sentence
NB. u is the verb for applying to y at each level going down; its result will be the y for the next level
NB. v produces the result, and joins multiple results.  It is applied dyadically (with x='') at leaf nodes or
NB. (with x=the contents of the non-locale result) if a non-locale is returned from proplocales,
NB.  and monadically at interior nodes. y to the dyad is the result of applying u to y; to the monad it is the array of results from executing on lower locales
NB.
NB.
NB. NOTE that users of this conjunction must interpolate a named verb to call it, so that the current locale will not be modified
NB.
NB.
traversedown =: 2 : 0
assert. 'travdown monadic'
:
NB.?lintonly proplocales =: ]
NB. Run the downward traversal on a node before asking that node for its descendants.  We rely on this to
NB. let a node with initialselection decide which descendants it should forward a selection to
uy =. u y
if. #nloc =. x~ '' do.
  nloc ([:  v   (v (1&{::))`(u (2 : (':';'l ([ cocurrent)~ (u traversedown v)&>/ y [ (cocurrent x) [ l =. coname''''')) v)@.(2 = 3!:0@>@[)"0 _ ) x ,&< uy
else.
  '' v uy
end.
)

NB. The default branch-followers all vector through proplocales with a different y.
NB. We split them into different names so that a locale can selectively override them.
NB. Here are the defaults that vector through proplocales
propselclones =: proplocales@_1:  NB. return locales that need to be cloned when this locale is cloned
propselstopatnoun =: proplocales@0:  NB. return locales feeding into this one, but not including nouns
propselstopatxy =: proplocales@1:  NB. return locales feeding into this one, including m/n but not verb ops x/y
propselall =: proplocales@2:  NB. return all locales feeding into this one
propseltokens =: proplocales@3:  NB. list of tokens for creating sentence display: locales of noun/verbs, plus strings as needed for modifiers


cocurrent 'dissect'

NB. Clear scroll point (at nodes leafward from the starting node).  y is 1 to start, and the assignment is made only if <: 0
propscroll =: 'propselall'&((3 : 'if. y <: 0 do. scrollpoints =: 0 2$0 end. <: y') traversedown 0:)

NB. init parent nodes in all objects.  This must be done in a separate verb because only a named verb resets the locale
NB. Called in locale of the base of the tree
initparentnodes =: 'propselall'&((3 : 'coname 0 # parent =: y') traversedown 0:)

NB. set all tokens in a subtree invisible
NB. Called for the left operand of assignment when assignments are suppressed
suppresstokens =: 'propselall'&((3 : 'tokensvisible =: y') traversedown 0:)

NB. Convert all the logs to each-result-boxed form.  Called once at end of traversal
coalescealllogs =: 'propselall'&((3 : 'coalescelog 0') traversedown 0:)

NB. Clear all selections.  Called in the locale of the base of the tree.
clearselect =: 3 : 'displaysellevel =: 0 [ selections =: 0$a: [ ishighlightnode =: 0'
clearselections =: 'propselall'&(clearselect traversedown 0:)

NB. Set ishighlightnode for as many levels as there are selections (so clicking initialselection highlights expansion)
prophighlightnode =: 'propselall'&((3 : 'if. y >: 0 do. ishighlightnode =: 1 end. <: y') traversedown 0:)

NB. called after sniff to indicate which nodes can have an error display
setdisplayerror =: 'propselall'&((3 : 'errorwasdisplayedhere =: errorcode -.@e. ENOTERROR') traversedown 0:)

NB. init SDT-display flag in all objects.  y is the value to set
NB. Called in locale of the base of the tree
initnounshowdetail =: 'propselall'&((3 : 'y [ nounshowdetail =: y +. -. resultissdt') traversedown 0:)

NB.On the way down, we set dispstealthoperand: y is 1 2 to remove those codes from stealth, making ][ displayable
NB. Called in locale of the base of the tree
calcdispstealth =: 'propselall'&((3 : 'y [ dispstealthoperand =: {. stealthoperand -. y') traversedown 0:)
ctest =: 0: traversedown 0:
NB. Decide whether inputs to niladic verbs should be displayed.
NB. Normally not, but after " (ex: 2:"0) we may want to see them.  Result of each node is passed to its descendants; the input is saved as the status for this node to use
NB. Called in locale of the base of the tree
calcdispniladinputs =: 'propselall'&((3 : 'shownilad dispniladinputs =: y') traversedown 0:)

NB. Return selection level for each token in the input string
NB. Result is table of (token number(s));(selection level, visibility);(display locale)
NB. Called in locale at the base of the tree
gettokenlevels =: 'propseltokens'&((3 : '(displaysellevel,tokensvisible);coname$0') traversedown (3 : ('<;y';':';'<,:x,y')))

NB. Set DOyx and DOsize in each locale.  Used to detect undisplayed locales
setDOyxs =: 'propselall'&((3 : 'DOsize =: DOyx =: y') traversedown 0:)

NB. common routines used by the object locales.  Objects are subclasses of dissectobj

cocurrent 'dissectobj'
coinsert 'dissect'

NB. Return 1 if children should display their nilad inputs.  y indicates whether this node displays
NB. nilad inputs.  This is 1 if we think this node may have less than infinite rank; 0 if infinite (no
NB. need to force nilad display then)
shownilad =: 0:    NB. normally not

EMPTYPRH =: 3 0$a:
NOPHYSREQ =: 0$a:  NB. this matches NOLAYOUTS

NB. Defaults for switches set only in certain paths
rankcalculussupported =: 1

NB. The following names must be redefined when an object is cloned
clonenames_dissect_ =: ;: 'selections scrollpoints scrolltravelers displaysellevel winhwnd errorwasdisplayedhere pointoffailure sellevel selectable stealthoperand ishighlightnode endhighlightnode dispniladinputs nuvocpage'

NB. Object creation.  create the signaling variables used for communicating with the grid.
NB. y is <the tokens that these values came from
NB. Each verb-type object is responsible for creating:
NB. titlestring: the name that will appear in the display at the top of a box
NB. stealthoperand: at create time, this is set to 0 for normal verb, 1=], 2=[, 4=[:, 5=] never displayed, 6=[ never displayed
NB. valence: when the verb gets a valence (i. e. when its monad/dyad exec happens), that valence is saved.
NB. displaysellevel: like #selections, but the value to use for display.  Noun operands of conjunctions are given the same level
NB.  as the verb, for pleasing display
create =: 3 : 0
coinsert COCREATOR
NB. The following names are not modified after the object is cloned:
titlestring =: ''
tokensource =: ~. > 0 { y
tokensvisible =: 1
invertiblymonadic =: 0

NB. The following names are guaranteed modified in the clone after this object is cloned:

NB. The following names are possibly modified after cloning.  Therefore, they must be copied into the clone
NB. when the clone is created, so that a mod to the original doesn't affect the clone.
(clonenames) =: (0$a:);(0 2$0);(2 2 2$0);0;'';1;($0);_;0;0;0;0;0;''

NB.?lintonly valence =: 0$a: [ snifferror =: 1
NB.?lintonly errorlevel =: 1 3$a:
NB.?lintonly defstring =: ":
NB.?lintonly resultissdt =: nounhasdetail =: nounshowdetail =: 0
NB.?lintonly 'displaylevrank fillmask' =: (0 3$a:);($0)
NB.?lintonly dispstealthoperand =: 0
NB.?lintonly dispniladinputs =: 0
NB.?lintonly DOyx =: DOsize =: 0 2$0
NB.?lintsaveglobals
''
)

NB. Object destruction.  This is inherited by all the object locales, if they have no local work to do (which they don't)
destroy =: 3 : 0
NB. if there is a grid attached to this locale, destroy it.
destroyexplorer''
codestroy''
)

NB. for debugging, verify that the execution string has valid syntax
NB. y is the string, and the result
auditstg =: 3 : 0
if. ~:/ '()' +/@:="0 _ y do. if. ~:/ '()' +/@:="0 _&;: y do.  NB. test as chars, then as tokens

  smoutput 'unbalanced parens'
  smoutput 'Error in exestring in locale ' , >coname''
  smoutput 'string: ' , y
  estring__ =: y
end.end.
y
)

NB. y is a short string, usually the name of the modifier that creates a verb.
NB. result is the value to use for titlestring.
NB. x indicates the context: (this is structural modifier)
NB. if y begins or ends with a space, that's a signal that we should always return empty for the modifier
fulltitlestring =: 4 : 0
isstruct =. x
if. y -: '' do. ''
elseif. ' ' e. 0 _1 { y do. ''
elseif. isstruct *. -. displayshowstructmods do. ''
elseif. displayshowcompmods do. MAXSTACKDEFSTRINGLENGTH defstring 0
elseif. do. y
end.
)

NB. fit conjunction is normally not supported, gives error
applyfit =: 1:

NB. Clone.  Nilad.  Create a new locale.  Chain path through the path of the current object.
NB. That way, a cloned noun points to the originating noun, which will be the one that gets
NB. a value after execution, so the value can be reused
NB. Then recur on any verb operands
NB. We know that modifier operands are named uop, vop
NB. Result is new locale
clone =: 3 : 0
NB.?lintonly uop =. vop =. <'dissectobj'
NB. Create new locale with same path, but with the current object in front
cl =. cocreate''
NB.?lintonly cl =. coname''
((, copath) coname'') copath cl
NB. Perform unaliased initialization.  The values (look in create) that may be different between
NB. clones, and that are not guaranteed to be assigned after the clone, must be initialized in the clone.
(,&'__cl'&.> clonenames) =: ".&.> clonenames

NB. Switch to the new locale
cocurrent cl
NB. If there are modifier verb operands, clone them too.  We don't clone noun operands, because
NB. they are not known until they are evaluated, and they haven't been evaluated yet, and will be evaluated only
NB. in the original locale.  Our treatment here means that the noun may be displayed in multiple places, with the
NB. traverseup flags aliased together.  We'll worry about that later (solution might be a special noun-clone verb
NB. that becomes a subclass of the original noun, but with the log removed)
for_l. propselclones '' do.
  loc =. ".@> l
NB.?lintmsgsoff
  (l) =: clone__loc ''
NB.?lintmsgson
end.
NB. Handle any post-clone initialization
postclone''
NB. Return the new locale
cl
)

NB. default null postclone
postclone =: ]

NB. Switch object processor.  y is the name of the new object processor.
NB. Replace the current path with the path to y (including y)
NB. If x is given, it is the split point: locales before the split point
NB. are replaced by the new object; locales at the split point and after are kept as is.
NB. The default split point is 'dissectobj'
changeobjtypeto =: 3 : 0
'dissectobj' changeobjtypeto y
:
afteroldobj =. (<x) (i.~ }. ]) copath coname''
beforenewobj =. (<x) (i.~ {. ]) (, copath) newloc =. boxopen y
(beforenewobj , afteroldobj) copath coname ''
)

NB. Insert override locales into the path
NB. x, if given, is the name to insert the overrides before (default '', which means 'at front of search path')
NB. y, if given, is the name(s) of the override locales (if empty, use current locale)
insertoverride =: 3 : 0
'' insertoverride y
:
iloc =. ,&.> boxopen x
if. 0 = #oloc =. ;:^:(0=L.) y do. oloc =. coname'' end.
(copath~   ~.@(({. , oloc , }.)~   # | i.&iloc)@copath) coname''
)

NB. Init the logging table.  if y is 0, create the dyad logging table that saves an additional value
NB. for each logged item; if y is 1 or 2, create logging table for y [& x] (used by named verbs)
initloggingtable =: 3 : 0
logticket =: 0 $ 0
logvalues =: 0 $ 0
logvaluesarchive =: 0$a:
select. ,y
case. ,0 do. logvaluesd =: 0 $ 0
case. ,1 do. logvaluesy =: 0 $ 0
logvaluesarchivey =: 0$a:
case. ,2 do. logvaluesx =: logvaluesy =: 0 $ 0
logvaluesarchivex =: logvaluesarchivey =: 0$a:
end.
NB.?lintonly logticket =: logvaluesd =: 0$0 [ logvalues =: logvaluesx =: logvaluesy =: 0$a:
NB.?lintonly verbex =: ]
NB.?lintsaveglobals
''
)

NB. return the type of y: 0=numeric 1=character 2=box 3=sparse 4=symbol
nountype =:  0 1 0 0 0 2 0 0 3 3 3 3 3 3 4 1 {~ 1 2 4 8 16 32 64 128 1024 2048 4096 8192 16384 32768 65536 131072 I. 3!:0
NB. add to log.  Always called in the locale of the parse object.  x, if given, is the values log into the secondary area logvaluesd
NB. y is the value to log; it becomes the result
addlog =: 3 : 0
NB.?lintmsgsoff
if. NOCLEANUP_dissect_ do. if. -. ifdefined 'logvaluesarchive' do. logvaluesarchive =: 0$a: end. end.
NB. If we detect a restarted primitive, we suppress logging while the restart is in progress.
if. loggingallowed__COCREATOR do.
  logticket =: logticket , ticket__COCREATOR =: >: ticket__COCREATOR
  if. 0 = #logvalues do.
    logvalues =: 0 $ ,: y
  elseif. y (=&nountype *: (-: }.)&$) logvalues do.
    logvaluesarchive =: logvaluesarchive , <logvalues
    logvalues =: 0 $ ,: y
  end.
  logvalues =: logvalues , y
end.
NB.?lintmsgson
y
:
NB. Dyad: log x to the dyad area
if. loggingallowed__COCREATOR do.
  logvaluesd =: logvaluesd , x
end.
addlog_dissectobj_ f. y  NB. In case addlog has a cover in the calling locale, try THIS monad
)

NB. Bivalent; add y [& x] arguments to their logs.  We know that this is called immediately after the result is logged,
NB. so we don't need to get a ticket; just add the argument.
NB. Result is immaterial
addlogxy =: 3 : 0
NB.?lintmsgsoff
if. NOCLEANUP_dissect_ do. if. -. ifdefined 'logvaluesarchivey' do. logvaluesarchivey =: 0$a: end. end.
NB. If we detect a restarted primitive, we suppress logging while the restart is in progress.
if. loggingallowed__COCREATOR do.
  if. 0 = #logvaluesy do.
    logvaluesy =: 0 $ ,: y
  elseif. y (=&nountype *: (-: }.)&$) logvaluesy do.
    logvaluesarchivey =: logvaluesarchivey , <logvaluesy
    logvaluesy =: 0 $ ,: y
  end.
  logvaluesy =: logvaluesy , y
end.
NB.?lintmsgson
''
:
NB.?lintmsgsoff
if. NOCLEANUP_dissect_ do. if. -. ifdefined 'logvaluesarchivex' do. logvaluesarchivex =: 0$a: end. end.
NB. If we detect a restarted primitive, we suppress logging while the restart is in progress.
if. loggingallowed__COCREATOR do.
  if. 0 = #logvaluesx do.
    logvaluesx =: 0 $ ,: x
  elseif. x (=&nountype *: (-: }.)&$) logvaluesx do.
    logvaluesarchivex =: logvaluesarchivex , <logvaluesx
    logvaluesx =: 0 $ ,: x
  end.
  logvaluesx =: logvaluesx , x
end.
NB.?lintmsgson
addlogxy y
)


NB. Nilad.  Convert the log to one boxed value for each result
coalescelog =: 3 : 0
NB. Make sure we do this only once per node.  We can come through here twice for conjunction noun operands,
NB. and we wipe out logvaluesarchive the first time
for_t. '';'y';'x' do.
  'a v' =. (;: 'logvaluesarchive logvalues') ,&.> t
  if. ifdefined a do.
NB.?lintmsgsoff
    if. #v~ do. (a) =: a~ , <v~ end.
      if. #a~ do.
        (v) =: ; <"_1&.> a~
        4!:55 <a
    else.
      (v) =: 0$a:
NB.?lintmsgson
    end.
  end.
end.
0 0$0
)

NB. create string to use to add log entry.  If y is nonempty, it is the value to pass in as x
NB. The string produces a verb of infinite rank whose value is the same as its y
logstring =: 3 : 0
(')' ,~ ('((' , ')&' ,~ (5!:5 <'y'))&,)^:(*#y) 'addlog_' , (>coname'') , '_ '
)
NB. Same, to log y [and x] - never has an operand
logstringxy =: 3 : 0
'addlogxy_' , (>coname'') , '_ '
)

NB. Create string to add for logging a verb.  This creates a verb, named verbex, in the current locale
verblogstring =: 3 : 0
'verbex_' , (>coname'') , '_ =: '
)

NB. Create string to add for logging a conjunction result.  This creates a name, conjex, in the current locale
conjlogstring =: 3 : 0
'conjex_' , (>coname'') , '_ =: '
)

cocurrent 'dissectobj'
NB. Propagate selection down the tree (root to leaves).  y is the value to propagate.  We propagate
NB. selections to all verb operands; #selections to conjunction noun operands as well
NB. The calls to traversedown must be named verbs!!
installsel =: 3 : 0
NB. Get the value to set
newsel =. selectiontodisplay y
NB. If we are changing the selection coming into this node (not the selection here: the user is looking at that),
NB. clear the scrollpoints so they will be refigured
if. newsel -.@-:&(sellevel&{.) selections do. scrollpoints =: 0 2$0 end.
NB. If we encounter the highlight node, change it so we know we touched it
endhighlightnode =: +: endhighlightnode
selections =: newsel
NB. Return the selection value to be passed down to next level.  But do not propagate the
NB. one-shot value, which is used as an initialselection to control expansion on/off when there is not actual initialselection to pass on
y
)
NB.propsel0 =: 'propselstopatnoun'&((3 : 'y [ selections =: selectiontodisplay y') traversedown 0:)
propsel0 =: 'propselstopatnoun'&(installsel traversedown 0:)
propsel1 =: 'propselstopatxy'&((3 : ('y [ displaysellevel =: y')) traversedown 0:)
propsel =: propsel1@# [ propsel0


NB. ************ fwd/bwd buttons **************
cocurrent 'dissect'

NB. When we make a selection, we add it to a list of selections.  The list is a table of
NB. level;selection;locale;type of change.  type is _1 for selection to higher level, 0 for selection at same level, 1 for selection at lower (must be next) level
NB. The list, and the verbs that manage it, are in the instance locale (which is the same as the
NB. locale of the form).  The entry point for propagating a selection, which is called from any object locale, switches to
NB. the  instance locale.

NB. Initialize
initselections =: 3 : 0   NB. called in instance locale
NB. The selections we have made, and those that we have undone
selectionsqueue =: 0 4$a:
NB. The number of selections currently displayed
selectionct =: 0
NB. The number of selections that get us to the error, if one was found.  When we go back to the beginning,
NB. this is how many selections we will have
selectionctatinitialerror =: 0
NB. Disable buttons until there has been a selection
('fmfwd';'fmbwd';'fmshowerror') wdsetenable&> '0'
0 0$0
NB.?lintsaveglobals
)

cocurrent 'dissectobj'

NB. Add a selection (and propagate it to descendants)
NB. y is list of selections (relative to the sellevel; in other words, values below
NB. sellevel are left undisturbed)
NB. Called from object locales
makeselection =: 3 : 0
NB.?lintonly COCREATOR =. <'dissect'
NB. When we add a selection, it invalidates redo
selectionct__COCREATOR =: #selectionsqueue__COCREATOR =: (selectionct__COCREATOR {. selectionsqueue__COCREATOR) , sellevel ; y ; (coname'') ; * (sellevel+#y) - #selections
NB. If we are still looking for errors, mark this selection as one we will keep when the user says 'way back'
if. 1=snifferror__COCREATOR do. selectionctatinitialerror__COCREATOR =: selectionct__COCREATOR end.
applyselection__COCREATOR ''
0 0$0
)

NB. Move the last selection (in response to arrow key)
NB. y is (axis to move),(amount to move)
NB. We find the locale of the last selection if any, and then try to move the selection
NB. Runs in instance locale
NB. Result is 1 if a change was made
movelastselection =: 3 : 0
NB.?lintonly selections =. ,a: [ selframe =. '' [ sellevel =. 0
'axis dist' =. y
NB. Get selresult index corresponding to current selection
if. '' -: currsel =. selectusingisf sellevel { selections do. 0 return. end.
currsel =. >currsel
if. -. selectable do. 0 return. end.
NB. Get selection index after the move.  We do the calculation in selection space; then
NB. convert to ticket number and then back to selection
newsel =. selframe selindextoisf >@selectiontoticket selframe #. selframe | currsel + dist (_1 - axis <. <:#selframe)} 0 * currsel
NB. Audit new position for validity
if. 0 < auditselection newsel do. 0 return. end.
NB. Convert position to isf form and select it
makeselection newsel
1
)

cocurrent 'dissect'

NB. Look at the undo list and establish the state indicated there.  We process each selection
NB. starting at the beginning.  The last selection will be the node that gets the highlight.
NB. We process each selection in turn.  If the selection went through the endhighlightnode, we
NB. mark the locale of the selection as an inhighlightnode (which will enable highlighting for it)
applyselection =: 3 : 0   NB. runs in instance locale
clearselections__resultroot 0
NB. It turns out that having multiple selections active is confusing, even if they are for different
NB. monad/dyad execs.  So, show selections only for the last selection at each level (and delete any that are out of order)
NB. Selections are either up (added a value to selections), down (took away a value), or sideways (changed a value
NB. without changing the count).  Down happens only when an expansion is being rescinded.  Our challenge is to make sure
NB. we include, in addition to the final operation at each level, a previous up if that up installed an initialselection
NB. into a preselection node, because that state is used as a flag to indicate that the expansion should be created.
NB. We note that sideways operations are always reselections of a previously-selected node.  We will discard everything up to (but not
NB. including, though it doesn't matter) the last down; then we will take the last up; then we will take the last sideways following the last up.  The sideways
NB. must be in the path selected by the up, so that fact that the up is overridden by the sideways causes no trouble.
NB.
NB. After we have picked the actions for each level, we discard any actions for a level n if lower lavels have a later action.
NB. This cannot cause trouble, because it is impossible for a lower level to have an action between an up and a sideways of a leter level.
NB.
NB. We can be sure that the levels are encountered in ascending order.
if. #seltoexamine =. selectionct {. selectionsqueue do.  NB. The selections to show
  levelindexes =. ({."1 </. i.@#) seltoexamine   NB. for each level, the indexes of selections at that level
  leveltypes =. {&(> 3 {"1 seltoexamine)&.> levelindexes  NB. for each level, the types (down, sideways, up)
  NB. Get the index of the last down, up, sideways, in that order.  Discard any that don't exist.  Since we discard
  NB. out-of-order indexes, this will have the effect of discarding an element if its last occurrence is not after the last occurrence
  NB. of the lower-priority element.
  keepsbylevel =. (_1 1 0&e. # i:&_1 1 0)&.> leveltypes  NB. index, per level, of final selections of each type
  keepindexes =. ; keepsbylevel {&.> levelindexes
  seltouse =. ((#~ (= >./\)) keepindexes) { seltoexamine  NB. The selections to apply to create the correct state
  if. #seltouse do.
    NB. Get the locale of the last selection.  This node, and selectors contributing to it, will be enabled for highlighting
    NB.?lintmsgsoff
    lastsel =. _1 2 {:: seltouse
    NB.?lintmsgson
    NB.?lintonly lastsel =. <'dissectobj'
    for_s. 3 {."1 seltouse do.
      endhighlightnode__lastsel =: 1
      'lvl sel loc' =. s
      NB.?lintonly loc =. <'dissectobj'
      propsel__loc (sellevel__loc {. selections__loc) , sel
      NB. If we click on an initialselection, we must activate highlighting in the expansion also.  Calculate the # levels to extend highlighting
      if. endhighlightnode__lastsel ~: 1 do. prophighlightnode__loc (#selections__loc) - sellevel__loc + selectable__loc end.
    end.
    NB. Tidy up for next time, leaving no endhighlightnode set
    endhighlightnode__lastsel =: 0
  end.
end.
NB. Set button enables
wd 'psel ',winhwnd
'fmfwd' wdsetenable ": selectionct < #selectionsqueue
'fmbwd' wdsetenable  ": selectionctatinitialerror < selectionct
'fmshowerror' wdsetenable  ": selectionctatinitialerror < selectionct
0 0$0
)

NB. Move the last selection (in response to arrow key)
NB. y is (axis to move),(amount to move)
NB. We find the locale of the last selection if any, and then try to move the selection
NB. Runs in instance locale
movelastselection =: 3 : 0
if. selectionct > selectionctatinitialerror do.
  loc =. (<(<:selectionct),2) {:: selectionsqueue NB.?lintonly =: 4 4$a:
  NB.?lintonly loc =. <'dissectobj'
  movelastselection__loc y
else. 0  NB. No change if nothing selected yet
end.
)

NB. Undo one selection, moving it to the redo list
dissect_fmbwd_button =: 3 : 0   NB. runs in instance locale
NB. If nothing to undo, do nothing
if. selectionctatinitialerror >: selectionct do. 0 0$0 return. end.
selectionct =: <:selectionct
applyselection''
NB. Refresh the display
dissect_dissectisi_paint 1
)

NB. Move one selection from the redo list to the undo list, and execute it
dissect_fmfwd_button =: 3 : 0   NB. runs in instance locale
if. selectionct = #selectionsqueue do. 0 0$0 return. end.
selectionct =: >:selectionct
applyselection''
NB. Refresh the display
dissect_dissectisi_paint 1
)

NB. Cut back to the initial selection: none or starting error
dissect_fmshowerror_button =: 3 : 0   NB. runs in instance locale
if. selectionctatinitialerror >: selectionct do. 0 0$0 return. end.
selectionct =: selectionctatinitialerror
applyselection''
NB. Refresh the display
dissect_dissectisi_paint 1
)

NB. ***************** traverse down ****************

NB. ****** error-level management ******

cocurrent 'dissect'
NB. errorlevel__COCREATOR is a list of locales that start an execution that may have a quiet error.
NB. There are two cases: u :: v and fill-cells.  In each case we add the current locale to the
NB. error list.
NB.
NB. The locale for u :: v is explicitly removed after u has executed, before v starts.  The locale
NB. for a fill-cell is removed by endtraverse, that is, after every block completes.
errorlevelinit =: 3 : 0   NB. Called in locale of instance
NB. The error-level record is (locale, single-level-boxed);type;(locale where the error is to be flagged, a: if no error found yet)
errorlevel =: 0 3$a:
)

NB. fillmask - this has the shape of the open of frame $ result, and gives status for each atom thereof.  This status is the selection
NB.  level of this node (in the upper bits), and validity information in the lower bits.  The validity is
NB.  0=normal 1=fill 3=first unexecuted cell (presumably error, but that may depend on what happened elsewhere) 2=later unexecd cell
NB.  error is calculated per result cell & propagated to atoms; fill is calculated per atom.  fillmask is valid only for nouns, or if the
NB.  unselected result has a frame with multiple cells, and is undefined otherwise
'FILLMASKNORMAL FILLMASKFILL FILLMASKUNEXECD FILLMASKERROR' =: i. 4
FILLMASKNOCOLLECT =: 4
FILLMASKCHECKER =: 1 bwlsl FILLMASKNOCOLLECT
FILLMASKEXTRAAXES =: 1 bwlsl FILLMASKCHECKER
FILLMASKSELLEVEL =: 1 bwlsl FILLMASKEXTRAAXES

NB. Values for expansionstate
'UNEXPANDABLE EXPANDABLE EXPANDED' =: i. 3

cocurrent 'dissectobj'
ERRORLEVELNONE =: 0   NB. must be 0
ERRORLEVELFILL =: 1
ERRORLEVELADVERSE =: 2

NB. stack a new errorlevel.  y is the type (must be ERRORLEVELFILL or ERRORLEVELADVERSE)
adderrorlevel =: 3 : 0
errorlevel__COCREATOR =: ((coname''),y;a:) , errorlevel__COCREATOR
)

NB. Remove the most recent errorlevel (used in adverse)
remerrorlevel =: 3 : 0
errorlevel__COCREATOR =: }. errorlevel__COCREATOR
)

NB. Remove the error level for the current locale if there is one (it had better be the top!)
NB. Called after every traverse, so it must preserve its input
endtraverse =: 3 : 0
NB. If we are running a recoverable path (and not examining fill-cells), demote early errors back to non-fatal errors to avoid msg
if. (-. displayshowfillcalc) *. ERRORLEVELFILL = {. > {. 1&{"1 errorlevel__COCREATOR do.
  if. errorcode e. EEARLYERROR do. errorcode =: ENOEXECD end.
end.
if. (coname'') = {. 0{"1 errorlevel__COCREATOR do. errorlevel__COCREATOR =: }. errorlevel__COCREATOR end.
assert. 0 = (coname'') e. {. 0{"1 errorlevel__COCREATOR
y
)

NB. Return the most recent error level
geterrorlevel =: 3 : 0
> {.!.(<ERRORLEVELNONE) 1 {"1 errorlevel__COCREATOR
)

NB. Indicate the locale of failure for recoverable errors.  If we have not previously seen an error
NB. at this level, remember where we found the first one.  We will flag errors at that level
setrecovflagpoint =: 3 : 0
if. #errorlevel__COCREATOR do.
  if. a: -: (<0 2) { errorlevel__COCREATOR do.
    errorlevel__COCREATOR =: (coname'') (<0 2)} errorlevel__COCREATOR
  end.
end.
''
)

NB. Nilad.  Return 1 if the current locale signals a recoverable error
isrecovflagpoint =: 3 : 0
(coname'') -: {. 2 {"1 errorlevel__COCREATOR
)

NB. utilities for traversal and selection
NB. These are overridden as needed by individual modifiers.  The versions here work for simple verbs

NB. get the rank to use for this verb.
NB. Result is the rank to use for the verb's valence, or $0 if we don't know
getverbrank =: 3 : 0
select. 4!:0 <'verbex'
case. 3 do.
  NB. Verb - get ranks
  (valence { 0 1 _2) {. verbex b. 0
case. _1;_2 do.
  NB. undefined - probably not excecuted owing to error, or it's a noun - give empty ranks
  $0
case. do.
  NB. Other types happen only if a modifier returns a non-verb
  failmsg 'dissect restriction: an explicit modifier must return a verb'
end.
)

NB. Selection calculation for traversedown
NB. If there are input selectors, we see if this node qualifies the input still
NB. further.  We calculate the level to be used for the next selection (which is incremented from
NB. the current level if this level makes a selection), and the new selection, including the
NB. selected operands and the selected result.  We leave a lot of stuff lying around in globals for
NB. use by creategridobj; what we return is the selector we use at this node (if any) and a flag
NB. indicating whether the result of that selection is a singleton (which means we can collect it)
NB.
NB. If snifferror__COCREATOR is 1, it means we are doing the initial selection to find where the crash was.  We
NB. set this value to 2 after we have encountered the error, so that we catch only the first error.  We catch framing errors
NB. before execution errors; otherwise we follow the J order
NB. When there is a selection, we always select the last thing that happened.
NB. y is a single list with 0-4 items depending on selection status.  When operands are given, it is implied that
NB. the selector is selecting a single application of the verb.
NB.
NB. The global   selections  is maintained by the selection system, and indicates click status.  The value holds the entire selection path from
NB.  the base selector.  If there is a selection at this level (i. e #selection is > sellevel), then sellevel{selections
NB.  if the selector to use here
NB.
NB. Object globals set here are:
NB. sellevel - the selection level of this node.  Incremented when there is a selection.  Gives the number of previous selections
NB. physreqandhighlights - the physical selections corresponding to the selections for each operand.  This is filled in here during traversal.
NB.  Boxed list, with one table per operand.  Each row has
NB. (*#frame)+sellevel entries when well-formed.  Each entry in first row is a box containing a table of index lists (all
NB.  necessarily of the same rank); each entry in second row holds a cell to be highlighted for that operand; third row gives the
NB.  sellevel at the time the entry was added
NB. frame - the frame of the verb.
NB. frames - the individual franes, boxed
NB. selframe - the frame for selection purposes.  Normally the same as frame, but some compounds which have
NB.  infinite rank actually produce results in sections that need to be selected.  Examples are u^:v, u;.n etc
NB. errorcode - An indication of the result of applying the selection to the results of the verb, and comparing it to the
NB.  frame of the operands.  Values > 0 indicate error, 0=OK, _1=unknown because no operands; _2=unknown because no selector.
NB.  The errorcode gives the status of the input values passed to this verb, BEFORE any local selector is applied.
NB.  Values: _2,_1=unknown 0=OK others below.
NB.  UNEXECD=some cells ran, but not all, but no error NOEXECD=no cells ran
NB.  ABORTED=no cells ran, and error was detected EXEC=cells ran, but one failed
NB.  Values EOK and below are terminals; they should not be replaced.  Values above EOK indicate
NB.  incomplete results; the lower a value, the more precise it is, so we will replace higher values
NB.  with a lower during inheritu.
(errorcodenames =: ;:'EFILLERROR ENOUN EOK ENOAGREE EFRAMINGABORT EFRAMINGEXEC EABORTED EEXEC EFRAMING ENOEXECD EUNEXECD ENOOPS ENOSEL EINVALIDOP EINVALIDVERB EINVALIDMODOP ENOAGREEMASK EINVALIDOPMASK EINVALIDVERBMASK EINVALIDMODOPMASK EINADVERSE') =: _2 + i. 21
EEARLYERROR =: ENOAGREE,EINVALIDOP,EINVALIDVERB,EINVALIDMODOP
EEARLYMASK =: ENOAGREEMASK,EINVALIDOPMASK,EINVALIDVERBMASK,EINVALIDMODOPMASK
NB. If there were results to display, we will create a fillmask for them.  The cases follow:
EHASVALIDFILLMASK =: ENOUN,EOK,EEXEC,EFRAMING,EUNEXECD,EFRAMINGEXEC,EEARLYMASK,EFILLERROR
EHASFILLMASK =: EHASVALIDFILLMASK   NB. there are results, and a fillmask
EFAILED =: EEARLYERROR,EABORTED,EEXEC,EFRAMING,ENOEXECD,EUNEXECD,EFRAMINGABORT,EFRAMINGEXEC,EEARLYMASK  NB. incomplete execution
EALLFRAMING =: EFRAMING,EFRAMINGABORT,EFRAMINGEXEC   NB. framing error, with or without others
EGENERR =: EEARLYERROR,EFRAMINGABORT,EFRAMINGEXEC,EABORTED,EEXEC,EFRAMING,EEARLYMASK
EPROPERR =: ENOEXECD,EUNEXECD,EINVALIDVERB
ESHOULDINHERIT =: ENOUN,EOK,EEARLYERROR,EFRAMINGABORT,EFRAMINGEXEC,EABORTED,EEXEC,EFRAMING,ENOEXECD,EUNEXECD,EEARLYMASK  NB. This node adds to the knowledge of u@v
ENOTERROR =: ENOUN,EOK,ENOEXECD,EUNEXECD,ENOOPS,ENOSEL
NB. selresult - the result of applying the selection to the result of the verb.  This is the selection based on the INPUT to
NB.   this verb, not including any local selector, which is used for subnodes (the local selector is used for selector and selopinfo)
NB.   Since the selected result might not collect properly, we leave it
NB.   with each item boxed.  This is the raw selected result, always a boxed list.  It is used only for display, and needs
NB.   to be shaped using the frame before it is opened.
NB. selresultshape - the shape that selresult will have if it collects properly.  Passed on as the input to the next verb
NB. selector - the selector to use for subnodes of the current node, created by applying the local selector to the selector
NB.  given in the input arguments.  If there is an early error (agreement), or if the local selector gives an error,
NB.  the selector is an empty list, which will suppress further analysis.  If the selector contains multiple ranges, we assume that
NB.  we are just waiting for a final collection
NB. selopshape - The shape of the selected operand: the (remaining shape).  These come in from the y input, representing the state before this level, and go out
NB.  with the state after going through the local selection.  The operand shape is inferred from the rank of
NB.   the verb, so is valid even when there is no selector.  List of boxes, one per operand.
NB. selopinfovalid - set if there is no frame, or if there is a local selection.  List, one per operand.  This indicates that a single operand cell
NB.   has been selected, and can therefore be used by a subsequent v verb.
NB. errorlevel - a derivative of *#errorlevel__COCREATOR at the time this is parsed; 0 = nothing, 1 = fill-cell, 2 = adverse.
NB.  When it comes time to display error info, we don't use the result failure type for anything except
NB.  top-level errors
NB. opselin - the operand selections made on the current node.  This is initialized to empty here, and added onto whenever a layout is drawn
NB. vranks - the rank(s) of the operand. 1 or 2 numbers, but empty for a noun or for a verb that didn't execute
NB. rankhistory - 1{y holds the table of previous ranks.  We append vranks to it, to produce the rank stack for this operand.  The format of the rank stack is
NB.  (string form of name to display);(locale (NOT boxed));(rank r)[;(rank l).  The rank is the effective rank rather than actual: the rank of the cells
NB.  that the verb actually applies to.  This depends on the rank of the argument.
NB.  If the string form is instead 0, it means that this rankhistory is a 'heavy' locale marker that should always float down to u operands
NB. resultlevel - indicates boxing of result: '' = none, normal; 0 = result replaces ops in hierarchy (L:); 1 = box is added (&.> or expansion); 2=collection error
NB. nvalidresults - the number of valid selresults when we started this node.  We have to calculate this here, because inheritance of an error cell into a higher result may add a selresult, and we need
NB.  to make sure we don't try to select one beyond THAT cell.

dummytraversedowncalcselect =: 3 : 0   NB. used to set the essential variables needed by travops
if. 3 = #y do.
  'sellevel rankhistory selopinfo' =: y
  selector =: 0$a:
else.
  'sellevel rankhistory selopinfo' =: 3 {. y
  selector =: 3 { y
end.
physreqandhighlights =: {.@> selopinfo
inputselopshapes =: selopshapes =: , }.@> selopinfo
bnsellevel =: <sellevel
rankhistory =: rankhistory , '' ; coname''
''
)

NB. If an operand to a modifier is invalid, so that the verb will not run, we
NB. abort it early by producing an invalid frame.  This will localize the error to the failing modifier
FRAMETOCREATEABORT =: ,0.5

traversedowncalcselect =: 3 : 0
assert. 1 = #$y
assert. (#y) e. 3 4
NB. Initialize the locales where detail is to come from.  We will inherit these locales from u as long as u is valid
'sellevel rankhistory selopinfo' =: 3 {. y
physreqandhighlights =: {.@> selopinfo
inputselopshapes =: selopshapes =: , }.@> selopinfo
errorlevel =: geterrorlevel''
opselin =: 0 2$a:  NB. initialize opselin to empty (=no selection)
vranks =: getverbrank selopshapes
NB. Non-atomic sellevel is a flag indicating that the nominal rank is to be ignored, because the verb runs at infinite rank
NB. (used in ^:_1 because u has a rank, but u^:_1 always has infinite rank; also in @.)
if. #$sellevel do.
  vranks =: _:"0 vranks
  sellevel =: {. sellevel
end.
if. #vranks  do.  NB. forceinfinite overrides the observed verb ranks
  NB. Calculate the effective rank: the rank, but no larger than the actual rank of the argument.
  NB. If there is no argument shape, leave the ranks empty (will turn to space later)
  if. 0 = #inputselopshapes do. effranks =. (<'')"0 vranks
  else. effranks =. <@;"1 vranks (#&'!'&.>@(* + (> |.))@(0 >. -~) ,. ":@<.&.>) #@($^:(0<L.))@> inputselopshapes
  end.
  rankhistory =: rankhistory , (;:^:_1^:(0<L.) titlestring) ; (coname'') , |. effranks
end.
selectable =: 0  NB. Init unselectable unless we set otherwise
expansionstate =: UNEXPANDABLE
selx =. a:  NB. In case we don't set it, we need this to pick up 'all logvalues' for display purposes
NB. If a compound is bypassed for display (for example, u@:v where u fails, holding some data), we may display
NB. u rather than u@:v.  But then, selection will leave out the u@:v locale.  So, each locale keeps the
NB. name of the locale in which propsel should start; this is set to u@:v in this case
NB.
NB. inheritroot will always point to the base of the inheritance tree: the node with the finest detail
inheritroot =: coname ''
NB. We have an inheritance chain, which ends in the stubs in 'dissect' locale
inheritedto =: inheritedfrom =: <'dissect'
NB. initialselection is set for expansion nodes, to indicate where a click will cause an expansion,
NB. and what the initial value should be
initialselection =: 0$a:
QP^:DEBTRAVDOWN 'snifferror__COCREATOR%,loc=?>coname''''%,type=?0{::copath coname''''%defstring 0%>uop%>vop%>cop%vranks%sellevel%4!:55<''fillmask''%selections%$y%y%rankhistory%'
QP^:(DEBHLIGHT*.-.DEBTRAVDOWN) 'snifferror__COCREATOR%,loc=?>coname''''%,type=?0{::copath coname''''%defstring 0%y%sellevel%selections%'
if. 3 = #y do.
NB. No selector: we can't do much
  'selframe frame frames arglevel resultlevel errorcode selresult selector selopinfovalid' =: ($0);($0);a:;($0);($0);ENOSEL;(0$a:);(<0$a:);0 0
elseif.
selector =: 3 { y
QP^:DEBTRAVDOWN '$>selector $selopshapes >selector selopshapes '
(0 = #selopshapes) do.
NB. Just a selector, but no operands.  Must be an active multiple selection, or we just haven't hit operands yet (must be a noun),
NB. or possibly a rank-calculus probe that ran out of rank.  We will not come through here if we are sniffing errors
NB. Select the derived verb results, using the shape of the selector as the frame.
  if. 0 = #vranks do.
NB. It's a noun (either an SDT or the result of a verb exec).  It should have 1 result; if not, the exec failed
NB. Use the selector to cull the operand - this will be used only for recursion.  For normal nouns, we traverse with
NB. a selector that selects everything.  For recursions, the operand values for each level of recursion are logged in the
NB. locale of the noun when the recursion starts.
    QP^:DEBTRAVDOWN '#logvalues '
NB. selector and selop already set, keep them
    if. 1 < #logvalues do.
      NB. Since nouns appearing in u&v (ex: =&(i."0) are executed twice, in that case
      NB. discard all but the first one.
      logvalues =: 1 {. logvalues
    end.
    if. *#".'valence' do.
      NB. Valence expected - it must be a verb (or compound producing a verb)
      NB. If there are NO logvalues, there must have been an error creating the verb - some invalid form like +@2
      NB. There were no verb ranks, but we are treating this as a failing verb; so add a line to rankhistory for it
       rankhistory =: rankhistory , (;:^:_1^:(0<L.) titlestring) ; coname''
      'selopinfovalid selframe frame frames arglevel resultlevel errorcode selresult' =: (0:"0 physreqandhighlights);($0);($0);a:;($0);($0);EINVALIDVERB;<(0$a:)
    else.
      'selframe frame arglevel resultlevel selresult' =: ($0);($0);($0);($0);<logvalues
      errorcode =: (*#logvalues) { ENOEXECD,ENOUN
    end.
  else.
    if. *#>selector do.
      assert. 2 = {: $ > selector [ 'malformed selection'
NB. If non-noun going with no operands, there was some higher-level multiple selection: we can't collect
      selframe =: frame =: 0$0
      selresult =: (; findselection > selector) { logvalues  NB. fails if no values
      errorcode =: (ENOOPS,EUNEXECD,ENOEXECD,ENOEXECD) {~ #. (1,(*/ frame)) > #selresult
    else.
NB. empty selector with no operands.  This is a rank-calculus probe that ran through a verb that it couldn't predict.
NB. Treat it as if there had been no selector.
      'selframe frame errorcode selresult selector' =: ($0);($0);ENOSEL;(0$a:);<(0$a:)
    end.
  end.
  'frames arglevel resultlevel selopinfovalid ' =: a:;($0);($0);(0:"0 physreqandhighlights)
NB. We can pass the selector to u, which will collect; but not to v
elseif. do.
NB.?lintonly selresult =: 0$a:
NB. This is the main line.  We still have operands, which means that we are selecting a single application of
NB. the current verb.  We will inspect the operands to check for early error; then we will get the selected results
NB. and shape them according to the operand shapes.  This produces selresult, frame, and frames. At that time, we will check for aborted execution.
NB. After we have accounted for operands and results, we will validate the local selector, and apply it to
NB. the operands and results to produce selopinfo.
NB.
NB. We know that we MUST be processing a single selection and can therefore collect any result that comes out of this path
NB. If this is a noun, it could be a terminal, in which case what we do doesn't matter, or it could be nvv, in which case we
NB. have to treat it as a verb of infinite rank.  We will detect that by the absence of verbex, and use the one (required) value of the noun
NB. Extract the components of selopinfo
  if. 0 = #vranks do.
    NB. No ranks, must be a noun or invalid verb.  Since nouns appearing in u&v (ex: =&(i."0) are executed twice, so in that case
    NB. discard all but the first one.
    if. 1 < #logvalues do. logvalues =: 1 {. logvalues end.
    NB. If there are NO logvalues, there must have been an error creating the verb - some invalid form like +@2
    if. 0 = #logvalues do.
      NB. There were no verb ranks, but we are treating this as a failing verb; so add a line to rankhistory for it
       rankhistory =: rankhistory , '' ; coname''
      'selopinfovalid selframe frame frames arglevel resultlevel errorcode selresult' =: (0:"0 physreqandhighlights);($0);($0);a:;($0);($0);EINVALIDVERB;<(0$a:)
    else.
NB. selector and selop already set, keep them
      'selopinfovalid selframe frame frames arglevel resultlevel errorcode selresult' =: (1:"0 physreqandhighlights);($0);($0);a:;($0);($0);ENOUN;<(logvalues)
    end.
  elseif.
NB. Verb.  Get the frames of the verb and check for agreement error.
  execdframe =. >./ > execdframes =. (- vranks) }.&.> , $^:(0<L.)&.> selopshapes  NB. frame meaningful only if no framing error
  NB. Audit the frames.  They should agree. and they should match the number of results we got.
  NB. If the frame is invalid, we know that this verb is going to die before it executes; indicate agreement error
  NB. Clear the selector to short-circuit further processing
  -.@-:/ (<.&#&>/ {.&> ]) 2 {. execdframes,<$0 do.  NB. No agreement error on monad
    NB. Here for verb with agreement error.
    NB. Remember the locale of the agreement error.  If we come here multiple times, that's OK; only the
    NB. last one will be the actual failure
    agreeerrorlocale__COCREATOR =: coname''
    'errorcode selector selopinfovalid selresult arglevel resultlevel frames selframe frame' =: ENOAGREE;(0$0);(0:"0 physreqandhighlights);(0$a:);($0);($0);execdframes;execdframe;execdframe
  elseif. a: -: 'selframe frame frames resultlevel arglevel' =: selopshapes calcdispframe execdframes do.
    NB. Partitioning verbs can detect agreement on infinite-rank operands eg.  (1 ]/. 2 3)
    agreeerrorlocale__COCREATOR =: coname''
    'errorcode selector selopinfovalid selresult' =: ENOAGREE;(0$0);(0:"0 physreqandhighlights);<(0$a:)
  elseif.
NB. No agreement error.  Calculate the frames that we will use
NB. Calculate the shape of the selected operand.  We will
NB. return this shape, whether there is a valid selection or not, in case rank calculus allows us to
NB. continue calculating frames without selectors.  This also creates a fill-cell if the frame was empty.
NB. Simulate cell-replication:  prepend any surplus frame from the
NB. other operand, then remove the frame from each operand
    
NB. frame and selresult will be the values we use for DISPLAY at this node.  At this point frame is
NB. what we expect to see - deviation will be because of error
NB. selopshapes will be the shapes passed into the next selection level applied to this input
NB. This is provisional, and invalid for u/ and u^:, as well as selections that use a level.  All these
NB. are valid only if there is a selection, where selopshapes will be refigured later.
NB. The selopshapes calculated here is useful only for rank-calculus purposes
  assert. 1 = #$frame
  assert. 1 = #$selframe
  (-.@-: <.) frame do.
NB. Here for verb with invalid frame - an early error
    'errorcode selector selopinfovalid selresult' =: EINVALIDOP;(,a:);(0:"0 physreqandhighlights);(0$a:)
  elseif.
  NB. No early error.  See if selections are possible.  We make a node selectable if it has a selection frame, even if that frame has
  NB. only one possibility (to provide consistent UI), or if the verb is L: or S:, similarly even if only one result is possible; but that too
  NB. comes through the (pseudo)frame
  NB. BUT: as currently coded, selection only drops down one level at a time; and some of the highlighting code might(?) depends on
  NB. only one drop per selector (on second thought, how could it? L: can drop more).  So we must flag &.> as selectable, to keep the
  NB. selection in sync with the selectable.  Kludge.  Should recode selection to add boxing for unselectable &.> nodes following the selected node
  selectable =: (*#selframe) +. (resultlevel -: 1)
  a: -: selector do.
    SM^:DEBTRAVDOWN'rank-calculus probe'
NB. There were no selectors.  This means that rank calculus was applied somewhere to give us a shape without
NB. a valid selector.  The frame is valid, as just calculated, and the operand shapes too, provided this is a regular verb
    'errorcode selresult selopinfovalid' =: ENOOPS;(0$a:);(rankcalculussupported"0 physreqandhighlights)  NB. leave selector unchanged
    NB. do the rank-calculus processing of the shape, and whether we can accept a selection
    selopshapes =: ($^:(0<L.)&.> selopshapes) (}.~ #)&.> frames
  elseif. do.
    NB. There is a valid selector.  Process the results
NB. Find all the result indexes that represent valid results.  Some modifiers, such as u/ and u^:v, log out
NB. extra information.  Here we look at the list of executions and decide which ones are valid
    selx =. calcdispselx rawselx =. ; findselection > selector
NB. Normal case, verb with no agreement error
NB. Assume the verb completed unless we learn otherwise
NB. If we get to here, the operand being presented will be collected, as in u@:v .  So we can display it.
    errorcode =: EOK
NB. Also, the number of results should match the number of cells in the frame, except
NB. when the frame contains 0, in which case there will be 0 or 1 result.
    QP^:DEBTRAVDOWN '$selopshapes $&.>selopshapes selopshapes $frames frames $frame frame resultlevel arglevel $selx '
    if. 0 e. frame do.
      smoutput^:DEBTRAVDOWN 'execution on fill-cell'
      NB. Execution on a cell of fills.  We should have 0 or 1 result.  If 0, it means that the
      NB. execution on the cell of fills failed, and we will use a scalar numeric as the replacement
      assert. 0 1 e.~ #selx
      selresult =: selx performselection logvalues
      if. 0 = #selx do.
        NB. No result.  must be error processing fill-cell.  Assume result of 'integer'
        selresult =: ,<0
        NB. Since error-in-fill is automatically handled, it can't coexist with any other error, so
        NB. we can signal the case with an errorcode, if we want to display that.
        if. displayshowfillcalc do. errorcode =: EFILLERROR end.
      end.
NB. we will extend fill-cell with frame
NB. Keep selector unchanged, since there was just one cell in the operand and there still will be
      NB. fill-cells run, as it were, in a try block; if they fail, they do not show the error.
      NB. Start the try block here.  It will be removed by the endtraverse for this node
      adderrorlevel ERRORLEVELFILL
      NB. Set the errorlevel for this cell.  We can never fail on the cell that creates the fill, so we use
      NB. errorlevel as a flag to indicate whether the exec failed
      NB. If the user has asked for it, mark the rankhistory indicating the line at which the fillcell was substituted, by appending
      NB. '*' to any nonempty rank
      if. displayshowfillcalc do.
        rankhistory =: (,&'*'^:(*@#)&.> (<_1;<<0 1) { rankhistory) (<_1;<<0 1)} rankhistory
      end.
    else.
NB. Not fill cell.  If there is no error, we should have just the right number of results
NB. TEMP kludge!!  Error is not fatal, if we are in adverse or chasing a fill-cell.  Only too many results is always bad
      assert. ((*/ frame) > #selx) +. (*/ frame) = #selx [ 'travdowncalcselect'
NB. If the frame is valid, but we didn't get enough results, it means something died in this verb;
NB. mark this verb as requiring detail and set the selector (if any) to select the failing item, which
NB. will be the first item we did NOT create.  OR, it could mean that we are executing on a cell of fills, which
NB. might terminate with a error, which would be ignored.
NB. See if all the cells executed
      if. (*/ frame) > #selx do.
        smoutput^:DEBTRAVDOWN 'incomplete execution'
NB. Cells did not execute.  If the selector is the one we found when sniffing errors, the error is in this cell.
NB. Mark this cell as an execution error.  But if the selector is different, there is no permanent error here: either the user
NB. changed the selector to a later cell which didn't execute anything, or we have determined that this cell
NB. was partially-executed because of an error elsewhere, and there's no error here.  In those cases, we mark
NB. the cell as partially-executed, with no error
        if. operationfailed'' do. 
          NB. See if incomplete operation represents failure.  It does for a verb, but not for something like u@v.  In general
          NB. we detect the failure at the same point J would: so 1.5 u/ y would fail on u/

          NB. Always signal failure in the current traversal.  We use this for flagging recoverable errors
          setrecovflagpoint ''
          NB. If we are in the search for a failure, and this might be it (i. e. it's not recoverable), remember where it is
          if. (1 = snifferror__COCREATOR) *. ERRORLEVELNONE = errorlevel do. setfailurepoint selector end.
        end.

        if. (1 = snifferror__COCREATOR) *. ERRORLEVELNONE = errorlevel do.
        NB. Continue narrowing the search for the error
        NB. If there is a frame, select the first non-executing cell.  For us to get here, any selectable higher levels
        NB. must have selected, so we will be adding to a selection chain.
          if. #failingisf =. selframe selindextoisf #selx do. 
            NB. we should have had errors at higher levels, which will have set the previous selectors
            assert. sellevel <: #selections  [ 'error in sniff'
            NB. During sniff, each error is propagated down separately.  We append the new error to the previous.
            NB. We know the previous exists, but there might be more, if we have sniffed error before; so we
            NB. append the calculated selector to the selectors valid for this node
            NB.
            NB. In normal debugging, selectors are added from the root outward.  If a selection is made above
            NB. the root, we don't propagate the selection back to the root, on the theory that if the user
            NB. wanted to select at the root, they could have.
            NB. The error selector must have the correct structure for the current node
            makeselection , failingisf
            QP^:DEBTRAVDOWN 'edisp'''' $selections selections '
          end.
        end.
        NB. Set the errorcode: if we are at the failure point, indicate the appropriate type of error; otherwise
        NB. just call it unexecuted
        reporterror =. (selector -: pointoffailure) +. displayshowfillcalc *. isrecovflagpoint''
        errorcode =: (#. reporterror , cellswereexecuted #selx) { ENOEXECD,EUNEXECD , EABORTED,EEXEC
      end.
      NB. We need to save the selected value.  We use this to calculate the predicted frame after collection.
      selresult =: selx performselection logvalues NB. This is the (unopened, since it might not collect) result from this object's verb, in execution order
    end.
    
NB. If the selection trims down the selection of results, apply that trim to the selector.
NB. If we are sniffing and this verb failed, the final selection would fail
NB. by definition; we will have handled that case above.  We increment sellevel whenever there is a frame, so that even nonselecting
NB. frames show up in the shape display; but empty frames do not have selectors (to make the color sequence predictable).
NB. Thus, we have to skip the selection when the frame is empty
NB. We also ignore a forced selection (ex: u/ when y has 2 items), which shows up as an empty selector.  We leave the selector, so that
NB. we get predictable sellevels, but it is known not to be needed (i. e. it is created only when we have seen that the current
NB. selection is forced, on a previous traversal).  If we get a change of selection this gets reexamined.
    'seltype thissel' =. getselection rawselx  NB. classify the type of selection.  Selections not normally needed (recursion uses them, as does {)
    QP^:DEBTRAVDOWN'seltype thissel sellevel selections '
    select. seltype  NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only, 4=autoselect of node with no frame,
                     NB. 5=removal of forced selection 6=node like { that always selects even if no frame
    case. 2 do.
      NB. Forced selection: if this is the first time we see it, perform the forced selection, propagating it to lower nodes
      if. unforcedselection'' do. makeselection , thissel end.
      NB. If forced selection, don't apply thissel, because it would result in multiple ranges.  Just keep
      NB. the selector we had.
    case. 1 do.
      NB. Here we select for the unforced selection
      NB. Calculate the selection interval corresponding to each selected result.  Put result intervals into selection order, then choose one
      NB. But if there is 0 in the frame (meaning we ran on a cell of fills), selection is perfunctory and we will simply
      NB. keep the old selector
      if. -. 0 e. frame do.
        selector =: <^:(0=L.) (tickettonatural frame $ selector selectticketintervals rawselx) selectusingisf thissel
        assert. <:/"1 > selector
        assert. *./ ({.@> isfensureselection isftorank2 thissel) (>:&#)&> frames
        assert. 2 = {: $ > selector [ 'malformed selection'
      end.
NB. No action for type 3, which is pick-only selection
    end.
NB. The number of operands can change during traversal: monad u/ turns into a dyad, and dyad u\ turns into a monad.
NB. We handle that here by replicating the previous selectors if monad turns to dyad, or keeping the
NB. selectors for y (kludge) if dyad to monad.  In any case, add the physical selections for this node
NB. Result is a list of boxes one per operand, each box containing a 2-row table, where the first row contains
NB. the boxed physical selections and the second row the corresponding highlight
    if. seltype = 0 do.
NB. If there was no selection, we can still create a display for v if this selection produces a single cell,
NB. which we know will be presented to v all at once.  Account for operand replication, by using the common frame
NB. rather than the individual frames.
NB. But if this operation is irregular, such as u/ which changes the number of operands, we can't predict shapes or even valence
NB. based on frame, so we suppress lower analysis
      selopinfovalid =: ($selopshapes) $ 1 >: */ selframe
      NB. We update the selopshapes using the frames, even though we didn't have a valid selection, because rank calculus may
      NB. result in our continuing to use the shape (e. g. to catch an agreement error in a lower level, or at least to display the frames).
      NB. If this execution produces a single result, we need to keep the map structure of the operand valid, since we will continue processing it.
      NB. Otherwise there will be no further selections and it is inadmissible to look inside multiple boxes at once,
      NB. we can lose the map structure of selopshapes, if it has any
      selopshapes =: selopinfovalid calcunselectedshapes selopshapes
    else.
      NB. if highlighting is enabled, even a forced one, use it to calculate highlighting and the operand shapes after selection
      NB. Some of the objects require calcphys to run always (to set globals), so we do that before we see if we will use the highlight
      newp =. calcphysandhighlights thissel
      NB. Whether we keep the highlight or not, honor the change-of-valence it represents, by replicating the old highlight as needed
      physreqandhighlights =: physreqandhighlights ($&.|.~ #) newp
      QP^:DEBHLIGHT'ishighlightnode '
      if. ishighlightnode +. seltype=6 do.
        physreqandhighlights =: physreqandhighlights ,"1&.> ,&(<sellevel)&.> newp
        QP^:DEBHLIGHT'physreqandhighlights '
      end.
NB. recalculate selopshapes now that we have the selection
      selopshapes =: calcselectedshapes thissel
      selopinfovalid =: 1:"0 selopshapes  NB. Selection means input shapes are valid for next v
    end.
    assert. ((0<L.) +. (1=#@$))@> selopshapes
  end.

end.
assert. 1 = #$selresult
NB. Remember the number of results that we started with.  We must never try to select more than 1 beyond this count,
NB. even if we add a selresult cobbled together from lower-level result
nvalidresults =: #selresult
NB. Now selresult (a list) contains the unopened and unframed results.  frame contains the frame.
NB. We will calculate a faux
NB. shape for the result, by looking at the values without opening selresult.  If they have a common
NB. shape, we will show that shape after the frame.  If the cell collected, and there was no error
NB. (meaning they all ran), we will create the opened version of selresult, which is what we will use later.
NB. It is valid only if errorcode=0 and collected=1.
NB. Calculate the fill mask for the current verb, without requiring it to collect properly
'maxcellresultshape fillatom fillrequired' =: checkframing selresult
NB. If the result has a frame, simulate collecting it, to detect framing error.
NB. If selection was impossible, either because the were no operand shapes
NB. or a missing or empty selector, don't try to calculate fillmask for the cells (we might
NB. create a zero fillmask below)
NB. If no cells were executed, no fillmask is meaningful.
NB. If nothing was executed, create a dummy fillmask that we can add results to
if. errorcode e. EHASVALIDFILLMASK do.
  if. 1 < */ frame do.
NB. Calculate the per-item part of fillmask: the selection level (upper bits), plus validity,
NB. which is 0=OK, 2=first missing item, 3=later missing item.  No 'first missing item' unless there is an error here
    NB. If the result is uncollectable, note that in the fillmask too
    collecterror =. (0=#fillatom) *. (0=#resultlevel)  NB. If there is a resultlevel, it always collects
    errorfillcell =. (errorcode e. EEXEC,EUNEXECD,EABORTED,ENOEXECD) # FILLMASKERROR
    NB. If we have an error here, and the selection points to the error, start the fillmask for the error cell at the NEXT
    NB. selection level so that even if nothing lower gets inherited into the cell, it shows its selection level
    NB. kludge this leaves the error cell shaded even when another cell is selected
    if. selectable *. sellevel < #selections do. errorfillcell =. errorfillcell + FILLMASKSELLEVEL * 0 = auditselection sellevel { selections end.
    fillmask =: ((FILLMASKNOCOLLECT * collecterror) + FILLMASKSELLEVEL * sellevel) + tickettonatural frame $!.FILLMASKUNEXECD (FILLMASKNORMAL #~ #selresult) , errorfillcell
NB. Combine the per-item and per-atom parts of the fillmask
NB. The fillmask just created has one atom per selection value.
NB. If we have result cells, calculate a fillmask for each.  The per-item fillmask
NB. will be expanded to match the per-atom info

NB. If this node has a level (including expansion), or isuncollectable, box the fillmask to indicate
NB. that the result should stay boxed.  The boxing of fillmask acts as a flag to indicate that the selresult
NB. should not be unboxed
NB. The node can collect.  Calculate the per-atom part of fillmask, which indicates actual fill.
    if. (0=#resultlevel) do.
      if. -. collecterror do.
        fillmask =: fillmask + > frame $!.(<maxcellresultshape$0) maxcellresultshape&([ {.!.FILLMASKFILL (0) $~ (-#maxcellresultshape) {.!.1 $@])&.> selresult
NB. If the result contains dissimilar types, raise an error.  Treat empty as no type
      else.   NB. If framing error, so indicate
NB. Framing error is always fatal; stop any ongoing sniff
        setrecovflagpoint''   NB. Indicate failure for recoverable errors
        if. (1 = snifferror__COCREATOR) *. ERRORLEVELNONE = errorlevel do. setfailurepoint selector end.
        errorcode =: EFRAMING
        resultlevel =: 2   NB. Signify 'collection error'
      end.
    end.
  else.
NB. The value is displayable, but it has only one item, so we know it's going to collect.   Use the
NB. shape of the (one or none) result, and the current selection level.  Note that there is no display
NB. if there were no cells executed.
NB. If this node boxes its result, don't include the frame of the result
    fillmask =: (FILLMASKSELLEVEL * sellevel) $~ frame , (0=#resultlevel) # $@> '' ($!.a:,) selresult
  end.

  select. resultlevel
  NB. If L:, put the fillmask into the map of the result
  case. 0 do.
      NB. If this is L:, the fillmask (currently a numeric list) must be put into the map of the overall result, where it can receive updates from
      NB. selections as they come in
      NB.?lintonly resultseqmap =: ''
      fillmask =: resultseqmap { L:0 _ fillmask
  case. 1;2 do.
  NB. If the selresult will have an extra level of boxing applied, either because it won't collect or because of &.>, box fillmask
  NB. to match 
    fillmask =: <"0 fillmask
  end.
end.

NB. Calculate the shape of the result of this execution.  This comes from looking at the results, unless this
NB. is an expansion node, in which case selresult contains extraneous information and we need to examine the actual
NB. result of the verb.  In that case the overall verb may have failed, in which case selresultshape is immaterial,
NB. since nothing closer to the root will execute.
selresultshape =: selx calcselresultshape maxcellresultshape;<fillatom

NB. If this level is selectable, increment the selector level to use for this and subsequent levels - whether
NB. we have a selector yet or not.  If this level is selectable, and we didn't qualify it down to a single input,
NB. no selection will be possible at lower levels, no matter what happens later.  So we might as well assume that
NB. we qualified, and we just add one to the next level if selection here was possible.
NB. Set the indicator that this node can take a selector.  It can if
NB. this verb has a selection frame OR a level
bnsellevel =: < sellevel + selectable
QP^:DEBTRAVDOWN 'edisp'''' frame selframe $selresult selresult $selresultshape selresultshape selector selopinfovalid fillmask selections rankhistory selectable '
NB.?lintonly 'selopshapes frame selections sellevel' =: (2$a:);($0);(1$a:);0
NB.?lintsaveglobals
)

NB. Check for collectability, and calculate a fill cell
NB. y is a selresult - possibly empty list of boxes containing results.
NB. result is shape of max opened cell;(atom for fillcell);(fill required), or maxshape;empty;0 if not collectable
NB. An empty y is considered not collectable.
checkframing =: 3 : 0
NB. Extract the shapes of the operands, reversed, as a list
if. 0 e. $y do.
  ($0);($0);0
else.
  bshp =. , |.@$&.> y
  NB. Extend each shape with 1s to the maximum length; then take maximum across all axes; then reverse back to original order
  maxsize =. |. >./ exshapes =. (>./ #@> bshp)&({.!.1)@> bshp
  if. 2 <: +/ classes =. 0 ~: 16b74dd 16b0802 16b8020 16b10000 16b20000  bwand bwor/ ((0 -.@e. $) * 3!:0)@> y do.
    maxsize;($0);0   NB. framing error
  else.
    NB. framable: calculate fill cell, and see if fill needed
    maxsize ; ((0;' ';a:;({.s:'');(u: ' ');0) {::~ classes i. 1) ; -. *./ (-:"1 {.) exshapes
  end.
end.
)

NB. y is a skeletal map (with ignoranda at the leaves)
NB. Result is (#leaves);sequential map: each leaf contains the sequence number of its execution
seqmapfrommap =: 3 : 0
serseqno =: _1
(>: serseqno) ; < 3 : 'serseqno =: >: serseqno' L:0 y 
)

NB. x is a map, y is an index
NB. Result is the path to that sequential leaf, boxed
pathfromindex =: ({   < S:1@:{::)~


NB. Conjunction.  Apply u at the cell indicated by n
applyintree =: 2 : 0
if. #n do. ((u applyintree (}.n)) L:_1 ({.n){y) ({.n)} y else. u y end.
:
if. #n do. (x u applyintree (}.n) L:_ _1 ({.n){y) ({.n)} y else. x u y end.
)

NB. Return the true value of selresult, after collecting.
NB. y is selresult
NB. x is fillmask (which must have the shape of the desired result)
NB. result is framed selresult
NB. if framing failed, the box around each component of selresult is retained; otherwise the
NB. boxing is removed and the results collected
NB. framingok is an atom, or (if this is a selection node) a boxed atom for each box in the selection
NB. If fillmask is boxed, it means that the
NB. boxes correspond to the boxes of selresult, because either
NB. (1) resultlevel is set, and the boxes of selresult are never intended for collection: they
NB. will be either treated as atoms of a boxed array (resultlevel=1) or as leaves in a map (resultlevel=0);
NB. (2) the boxes of selresult are uncollectable.
NB. If y is a scalar, it means that this result has been framed previously.  We simply return the
NB. opened y.  The fillmask will conform to this value
frameselresult =: 4 : 0
NB. If this value has been formatted already (atomic y), return the previous formatting, which matches fillmask
if. ''-:$y do. >y
NB. If we are trying to create a valid selresult out of nothing, just give it the shape of the frame.
NB. But careful!  We may actually end up displaying this value, if the 'empty' selresult is forced out,
NB. as happens when it is the result of a recursionpoint which we display so as to allow selection.  In that
NB. case, the result had better be boxed to match the boxing expected for an expansion.
elseif. 0 = #y do. a:"0 x
elseif. do.
  select. resultlevel
  NB. If the fillmask is boxed, the selresult is an expansion, or has a level, or is uncollectable.  In all
  NB. those cases, don't unbox the selresult, just box it into the shape/map of the fillmask.  But we still have
  NB. to bring it to the correct shape, and we add fill (box with one space, for ease in seeing the crosshatching) in case the execution was short.
  case. 0 do.
    NB. If this is L:, we must assemble the result using the result map (which has the same structure as the
    NB. fillmask)
    NB.?lintonly resultseqmap =: ''
    resultseqmap (>@{) L:0 _ frame $!.(<' ') y
  case. 1;2 do.
    tickettonatural ($x) $!.(<' ') y
  case. do.
    'cs fill fillreqd' =. checkframing y  NB. result cell size, fill atom (empty if unframable)
    NB. This is where we add fill as required
    <"0@(cs&{.)@>`>@.(#fill) tickettonatural ((-#cs) }. $x) $!.(<cs $ {.!.' ' fill) y
  end.
end.
)

NB. x is max sellevel supported
NB. y is a fillmask code
NB. result is new fillmask code, clamped to the sellevel in x
colorlimitsellevel =:  ((<. *&FILLMASKSELLEVEL)~ (-FILLMASKSELLEVEL)&bwand) bwor (<:FILLMASKSELLEVEL) bwand ]

NB. Use the fillmask to give the color for each cell.  Low-order 2 bits are 0=normal 1=fill 2=error 3=unexecd;
NB. bit 2 is set if uncollectable;
NB.  bit 3 is 0 (filled in by checkerboard); higher bits are selection levels
NB. We just add in the checkerboard
NB. x is the upper limit on selection level (before checkerboard added), y is the fillmask to create a checkerboard for
NB. if y is boxed, this must be a selection node, and we recur on the selected node, to put the checkerboard there
NB. We use the shape of the fillmask to detect extra axes: if there is a leading 1, set extra-axis
checkerboardfillmask =: 4 : 0
assert. 0 = L. y
sel =. x colorlimitsellevel y
NB. Checkerboard: works for scalars too.  Create a checkerboard cell of rank no more than 2, then
NB. replicate as needed for higher rank, so that there is a predictable odd/even pattern within each rank-2 cell
sel + (({.   (0,FILLMASKCHECKER) $~ 1&bwor) ({.~ -@(2<.#)) $ sel)"2 sel
)

NB. Result is 1 if this node DOES NOT have a forced selection, which is tagged by a selection containing empty
unforcedselection =: 3 : 0
if. selectable *. sellevel < #selections do.
  a: ~: {. > isfensureselection isftorank2 sellevel { selections
else.
  1
end.
)

NB. Create accumulated frame, from the current node through all its inheritance, to the end.  Nilad.
NB. Result is a table, one per node, each row being sellevel;ISF for the node;locale for the node (no extra box);maxcellresultshape;fillrequired
NB. The ISF contains the frame and any SFOPENs called for by the resultlevel
NB. Forced selections are replaced by empty frame
NB. y is set if the previous node executed a fill-cell.  In that case, we will abort the search, returning the
NB. scalar error result, if the current node had an error.  On the first call, y is 0.
accumframe_dissect_ =: (0 5$a:)"_
accumframe =: 3 : 0
QP^:DEBDOvn'accumframe for ?defstring]0%>coname''''%selframe%unforcedselection''''%sellevel%selections%'
QP^:DEBDOvn'#selopshapes selector errorcode '
NB. Keep taking frames as long as they are valid, i. e. as long as we have selected from them using a
NB. selector that is not a rank-calculus probe.
NB. We have to patch over the first node of monad/dyad, which looks like a noun and has no selops out;
NB. the following verb will be OK
if. y *. errorcode e. EFAILED do. 0 5$a:
elseif. (*#vranks) *. (0 = #selopshapes) +. (selector -: a:) do. 0 5$a:  NB. keep frames as long as there is valid shape
elseif. do.
  resultinsidebox =. (<resultlevel) e. 0;1   NB. If this is L: or each, which always boxes results separately
  (sellevel ; ((<(unforcedselection'') # selframe) , resultinsidebox # SFOPEN) ; (coname'') , ((selframe -&# frame) }. maxcellresultshape) ; fillrequired *. -. resultinsidebox) , accumframe__inheritedfrom 0 e. frame
end.
)

NB. Change the error message displayed on the error block
NB. We call this when we understand an error and want to explain it
NB. We honor the request only during sniff, and only when we are not in a try block
changeerrormessagefrominterp =: 3 : 0
if. (errorlevel = ERRORLEVELNONE) *. (snifferror__COCREATOR = 1) do.
  errormessagefrominterp__COCREATOR =: y
end.
''
:
NB. For the dyad, make the change only if the errorcode (in x) indicates failure (which it should, if we diagnosed the error correctly)
if. x e. EFAILED do. changeerrormessagefrominterp y end.
''
)

NB. Signal early error
NB. Agreement error requires insertion of a node showing the location of the error.  For the nonce,
NB. we will abort traversal at that point.
NB. y is either the dols (x operand to traverse) or a list of (dols ; rightoperand)
NB.   where rightoperand is dol;highlights suitable for joinlayoutsl
NB. result is a suitable return value from traverse, viz (y ,&< locale) [; right]
earlyerror =: 3 : 0
NB. Since we abort the traversal, roll up the failing part and install it as the last name in the rank stack
displaylevrank =: (<MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
if. 2 > #$y do.
  1 0 1&(#!.(<coname'')^:_1) y
else.
  y ,&< coname''
end.
:
NB. The dyad is used for errors detected during traversal, eg u@.v where v creates a non-atom,
NB. or datatype errors. We set the errorcode
errorcode =: x
earlyerror y
)

NB. A sniff has found the error.  Make the global mark indicating that the error has been found,
NB. and save the failing selector in the failing node so that we can recognize when we hit it again
NB. y is the failing selector
setfailurepoint =: 3 : 0
if.snifferror__COCREATOR = 1 do.
  snifferror__COCREATOR =: 2  NB. Indicate that the sniff is over
  pointoffailure =: y  NB. This is where the error was found
end.
)

NB. Start by calling findinheritedtail__loc ''; result is name of locale at the tail of the chain
findinheritedtail_dissect_ =: ]
findinheritedtail =: 3 : 0
(coname'') findinheritedtail ''
:
NB. In the dyad, x is the locale we are in now, y is the locale we were in previously.
NB. When we get to the end, we are in 'dissect' locale, and the result is the preceding locale
inheritedto findinheritedtail__inheritedto x
)

NB. Append locale x (default=current locale) to the end of the chain ending in locale y
NB. It is possible that y points to the middle of a chain, so we have to be careful
NB. to add to the end
NB. The chain starts at u and ends at u"v"w....  Info in the root of the chain is most detailed.
NB. inheritroot is the first locale inherited from (u above)
NB. inheritedfrom is pointer to locale inherited from (u"v points to u)
NB. inheritedto points to the locale above this (u points to u"v)
NB. findinheritedtail finds the largest node (smallest sellevel)
extendinheritchain =: 3 : 0
(coname'') extendinheritchain y
:
NB.?lintonly x =. y =. <'dissectobj'
NB. chain the old chain to the new node.  But add the entire chain, from the end
inheritedfrom__x =: findinheritedtail__y''
NB.?lintonly inheritedfrom__x =: <'dissectobj'
NB. Remember the end of the chain
inheritroot__x =: inheritroot__y
NB. Update all the tail pointers, in case we display from the middle of the chain
inheritedto__inheritedfrom__x =: x
)


NB. Inherit information from u and v
NB. After u has been traversed, we roll its info into the current object for later display.  This ensures that
NB. in a sequence of u@u@u@v, the lowest u gets its values propagated into the display.
NB. y is result of traverse: dol ,&< locale of new dol.
NB. Result is the dol ,&< locale to display: usually this locale, but if this locale had no results and the lower
NB. locale did, the lower locale
NB. 0{x is 1 (default 0) to add an end-of-computation mark to the display stack.  This is a line with empty title and
NB. the current locale
NB. 1{x is 1 (default 1) to copy dispstealthoperand from u.  This is used so that t@(]@v) will treat the output of ]@v, which has
NB. nothing since v has already been outed, as a stealth.  But if u@v is a collector that loops back, we have to make sure
NB. it displays, so we leave the ] in
NB. 2{x is 1 (default 0) if this node is an expansion, and should preserve its data even if there is no data from the higher level
DISPINFO =: ;: 'expansionstate displaylevrank dispstealthoperand'
inheritu =: 3 : 0
0 1 inheritu y
:
'addcompmark copystealth isexpansion' =. x , (#x) }. 0 1 0
loc =. 1 {:: y
NB.?lintonly loc =. <'dissectobj'
SM^:DEBINHU 'inheritu: in ' , (>coname'') , ' ' , defstring 0
QP^:DEBINHU'$floc >loc defstring__loc]0 edisp'''' edisp__loc'''' >selector selresult '
QP^:DEBINHU'$fillmask fillmask fillmask__loc selresult selresult__loc selectable sellevel selections resultlevel resultlevel__loc '
QP^:DEBINHU'displaylevrank__loc copystealth dispstealthoperand '
QP^:DEBDOL2'physreqandhighlights physreqandhighlights__loc '
NB. The display information is always inherited from the last u, which creates it.
NB. The only time we wouldn't inherit is if the error is detected before the last u, example 1.5 u/ y which
NB. would detect it on u/.  We detect that by the error-point codes
if. errorcode -.@e. EABORTED,EEXEC do.
  cnames =. (<:copystealth) }. DISPINFO  NB. drop last name if copystealth is not set
  NB.?lintonly cnames =. DISPINFO
  (cnames) =: ".@(,&'__loc')&.> cnames
  if. 32 ~: 3!:0 displaylevrank do.
    NB. We are inheriting a noun, presumably a nilad.  At this point it becomes a verb.
    NB. Replace levrank with the rankhistory for the verb (preserving the levrank as the name),
    NB. and make the verb the base of the inheritance chain
    NB.?lintonly niladtitle =: ''
    displaylevrank =: (< niladtitle&[^:(0=#) displaylevrank) (<_1 0)} rankhistory
    extendinheritchain loc
    NB. The chain pointers are already set up to start the chain at this locale
    (<coname'') 1} y return.
  end.
  if. addcompmark do. displaylevrank =: displaylevrank , DLRCOMPEND;coname'' end.
end.
NB. If any node contributing to this display is highlightable, enable the highlight.  The flag
NB. may not be at the end (if not enough selections) or at the beginning (it is unselectable).
ishighlightnode =: ishighlightnode +. ishighlightnode__loc
NB. If the new dol is uninheritable (it is a selector node added by u/ or u^:v and its fillmask etc
NB. is incommensurate with the selector for the current node), inherit nothing and display the current locale
replaceresult =. 0
NB. If the u was uncollectable, or if it executed no cells, it can't contribute to the fillmask
NB. and its fillmask is guaranteed undefined.  Likewise, if there was no selector, there will be no fillmask
NB. Append the frame of u to the ranks calculated at this level.  If u lacked selectors, the frame
NB. will be empty, and we will ignore it.  If a parent of u had an empty selector, the actual numbers
NB. in the frame might have been replaced by placeholders of _1, but their number will be right.
NB. If u@v did not fail, but u did, ignore it - it must be a fill-cell
if. errorcode <: EOK do.
  if. errorcode__loc e. EFAILED do.
    assert. 0 e. frame  [ 'u failed but u@v succeeded'
  elseif. errorcode__loc = EFILLERROR do. errorcode =: errorcode__loc
  end.
  
NB. If u has the error source then u@v should have failed short.   If u@v has no results,
NB. inherit the u locale to replace it
elseif. errorcode__loc e. EGENERR do.  NB. all errors but NOEXECD,UNEXECD
  assert. errorcode e. EPROPERR [ 'u@v died but u@v was OK'  NB. if u died, u@v should be sick
  if. errorcode e. EHASFILLMASK do.
    NB. Inherit the fact of failure, but preserve existing data.  If we failed framing or agreement, pass that up the line
    NB. If the failure was an early error, keep the early error but note that we now have a mask
    if. errorcode__loc e. EEARLYERROR do.
      errorcode =: (ENOAGREEMASK,EINVALIDOPMASK,EINVALIDVERBMASK,EINVALIDMODOPMASK) {~ (ENOAGREE,EINVALIDOP,EINVALIDVERB,EINVALIDMODOP) i. errorcode__loc
    else.
      errorcode =: (#.(errorcode__loc e. EALLFRAMING) , errorcode e. EHASVALIDFILLMASK) { EABORTED,EEXEC,EFRAMINGABORT,EFRAMINGEXEC  NB. Inherit the error indic
    end.
  else.
NB. u@:v has no result - replace it with u, provided u has real data
    replaceresult =. errorcode__loc e. EHASFILLMASK
    NB. We also have to inherit the status
    errorcode =: errorcode__loc
  end.
elseif. (errorcode__loc = ENOEXECD) *. (errorcode = EUNEXECD) do.
NB. If u@v had results but u didn't, the explanation must be that v failed (perhaps we should signal a different error code for u).
NB. We will have put out the error on v, so we suppress it on u.  But if this is an expansion node, we will keep the display so we can select results
  if. -. isexpansion do.
    errorcode =: ENOEXECD
  end.
elseif. (errorcode__loc = EUNEXECD) *. (errorcode = ENOEXECD) do.
NB. u executed incompletely, but u@v not at all??  Yes, it must be that there was an error, but we
NB. selected off the error path, so now we don't go through GENERR; or it could be a partition that calculated a few
NB. partitions and then failed, leaving no result from the selector.  Replace u@v with u (actually the
NB. NOEXECD is probably from a monad/dyad exec)
  errorcode =: EUNEXECD
  replaceresult =. 1
end.
NB. (errorcode__loc = EUNEXECD) *. (errorcode = ENOEXECD) can happen if the error path is not selected

NB. If both locales have fillmasks, insert or replace the fillmask from u.  If
NB.  this is an expansion node, also modify the selresult (which may overwrite the
NB.  selresult added from the error)
if. replaceresult do.
  NB. This node has no fillmask, and the lower node has a value.  We will copy the value and
  NB. create a fillmask for it.  We have to do this to get the value inserted into result, if this node
  NB. has a resultlevel.  We format the lower node using its formatting.  The code here is similar to the
  NB. code below where we use the lower node to override the upper value; the difference here
  NB. is that there is no fillmask for the upper node and we have to create one.
  select. resultlevel
  case. 0 do.
    NB.?lintonly resultseqmap =: ''
    NB. If this is a L: node, there must be only a single result, but we need to insert it and its fillmask
    NB. into the result map.  So, we box the fillmask and insert it into the result map at location 0, and
    NB. make the formatted result the sole result of L:
    selresult =: ,< fillmask__loc frameselresult__loc selresult__loc
    fillmask =: (fillmask__loc) [ applyintree (> resultseqmap pathfromindex 0) FILLMASKUNEXECD"0 L:0 resultseqmap
  case. 1;2 do.
    NB. If this is &.> or collection error, box the fillmask.  Leave the result single-boxed: it represents the result of the lower boxing level and will
    NB.  have another level added.
    selresult =: ,< fillmask__loc frameselresult__loc selresult__loc
    fillmask =: tickettonatural frame $!.(<FILLMASKUNEXECD) < fillmask__loc
  case. do.
    NB. In other cases, format the selresult according to ITS settings, and then save it according to the way it will
    NB. be formatted (i. e. by collection using the fillmask)
    NB. In all cases, we have a lower fillmask whose shape matches its result; we replicate this using the
    NB. frame of the upper node.
    fillmask =: frame $!.(<^:(*L.fillmask__loc) FILLMASKUNEXECD) ,: fillmask__loc
    if. #resultlevel__loc do.
      NB. If the lower node has resultlevel, that means that each atom of the fillmask has the structure for the
      NB. corresponding atom of the selresult, so we box those atoms and run them into a list.
      selresult =: , <"0 fillmask__loc frameselresult__loc selresult__loc
    else.
      NB. If the lower node does not have resultshape, its result shape matches its fillmask, and we box that value
      NB. to be the sole value for the final result (it will be filled when this result is collected)
      NB. We box the lower fillmask (in case it has structure) and extend it with UNEXECDs
      selresult =: selresult__loc
    end.
  end.
elseif. errorcode *.&(e.&(EHASFILLMASK)) errorcode__loc do.
NB. If a fillmask was calculated at the lower level, it should be more accurate than the
NB. fillmask for the current level, or at least more detailed.  If this level has a selection, we
NB. know ipso facto that this level had a frame and therefore a fillmask; in that case, insert
NB. the lower fillmask into this level's, after expanding it to match the collected result-cells
NB. (because the selected cell may need fill to fit into the final result)
NB.  If the higher verb failed in the middle of execution, that should be possible only if u
NB. also failed in the middle of execution.  Insert the fillmask as in the no-error case
NB.  If the higher verb had no operands, it must be a monad/dyad execution;
NB. it will perforce have no selection, so just pick up the fillmask from the u
  if. 0 e. frame do.
    NB. If this is a fill-cell, do nothing.  We will have executed the lower verbs on a single cell rather
    NB. than an array, and there is nothing to insert the result ionto
  elseif. selectable *. (sellevel < #selections) do.
    NB. sel1 is the path to the selection.  It may go down multiple levels, but it will
    NB. always end with a dropdown if there is a dropdown.  If the fillmask is boxed, there will
    NB. be a dropdown as long as this node is selectable (it isn't in the case of L: when the arguments
    NB. are initially at level).
    NB. So, we convert the selection to path form, which merely requires removing the dropdowns.
    sel1 =. SFOPEN -.~ > isfensureselection isftorank2 sellevel { selections
NB. If the current fillmask is boxed, it has internal structure, and we
NB. should replace the selected portion with the fillmask and data that was calculated in u
    select. resultlevel
      NB. tmodx gives the index into selresult of the selection.  For L: we get it from
      NB. the result map; for others we calculate from the index
      NB. fillmask, which describes what ought to be, is in natural order.
      NB. selresult, which describes what is, is in ticket order.
    case. 0 do.
      NB.?lintonly resultseqmap =: ''
      NB. L: requires that we box the lower fillmask, whatever it is, and insert it into the tree.
      NB. We also install the lower selresult
      tmodx =. sel1 {:: resultseqmap
      if. 1 >: #$>tmodx do. if. (>tmodx) >: #selresult do. selresult =: (>:tmodx) {. selresult end. end.
      fillmask =: (fillmask__loc) [ applyintree sel1 fillmask
      selresult =: (< fillmask__loc frameselresult__loc selresult__loc) tmodx} selresult
    case. 1;2 do.
      NB. expansion/each, and collection error both require that we install the boxed fillmask.
      NB. We install into the fillmask array
      tmodx =. ($fillmask)&#.&.> selectiontoticket sel1
      NB. We must collect the selresult before we copy it, so that we use the formatting in the locale where it is valid.
      NB. This happens only when we have an unknown number of items, such as in a recursion.  Here we support it in all
      NB. nodes that have a list for a result; this will embrace all selection nodes
      if. 1 >: #$>tmodx do. if. (>tmodx) >: #selresult do. selresult =: (>: > tmodx) {. selresult end. end.
      fillmask =: (< fillmask__loc) sel1} fillmask
      selresult =: (< fillmask__loc frameselresult__loc selresult__loc) tmodx} selresult
    case. do.
      NB. Normal fillmasks, which may or may not be boxed (they will be boxed if they contained some boxed detail such as
      NB. L: or each)
      NB. We install the lower fillmask directly into the upper.  No change is made to selresult.
      NB. We expand the lower fillmask to the size of a cell of the upper - and vice versa.
      NB. If the fillmasks have different boxing status, we box atoms of whichever is unboxed
      NB. The value to use for filling cells in the u fillmask depends on the errorcode for u.  If there
      NB. is no error, it's just normal fill
      select. * fillmask ,&L. fillmask__loc
      case. 1 0 do. fillmask__loc =: <"0 fillmask__loc
      case. 0 1 do. fillmask =: <"0 fillmask
      end.
      sel1 =. {. sel1   NB. only 1 atom allowed; make it an atom
      fillval =. <^:(*L.fillmask__loc) (FILLMASKSELLEVEL * sellevel) + (FILLMASKUNEXECD,(2#FILLMASKERROR),FILLMASKFILL) {~ (EUNEXECD,EEXEC,EFRAMINGEXEC) i. errorcode__loc
      NB. Find the size of a cell of the merged fillmasks
      maxcellresultshape =: ($fillmask__loc) >./@(,:!.1)&.|. fillcellshape =. (#frame) }. $ fillmask
      NB. Bring the old fillmask up to the new merged size, if that is larger.  This can happen only if
      NB. the lower result contains a failure that never made it up to the higher during initial allocation
      if. maxcellresultshape -.@-: fillcellshape do.
        fillmask =: (maxcellresultshape {.!.(<^:(*L.fillmask) FILLMASKFILL + FILLMASKSELLEVEL * sellevel) ,:^:(maxcellresultshape -&# fillcellshape))"(#fillcellshape) fillmask
      end.
      NB. Bring the new fillmask up to the new merged size, if that is larger
      NB. Insert new fillmask into old
      fillmask =: (maxcellresultshape ([ {.!.fillval (({.!.1 $)~ -@#)~ ($,) ]) fillmask__loc) sel1} fillmask
      NB. Install the new value into selresult - needed only if it is an error value and thus an addon at the end
      if. (#selresult) = frame #. >sel1 do.
        NB. Install the new value into selresult - needed only if it is an error value and thus an addon at the end
        selresult =: selresult , (< fillmask__loc frameselresult__loc selresult__loc)
        NB. Also refigure cell information, since there may now be collection error
        'maxcellresultshape fillatom fillrequired' =: checkframing selresult
      end.
    end.
  elseif. do.
    NB.  If this level does not have a selection, the lower
    NB. fillmask must cover the same cells as this level, and this level might have no frame and
    NB. therefore no fillmask; so in that case use the lower fillmask as this level's entire fillmask.
    NB. Here we overwrite the selresult from the previous level, so we'd better not get anything wrong!
    NB. Roll up the results into a single result, and make it the sole result from this level.
    select. resultlevel
    case. 0 do.
      NB.?lintonly resultseqmap =: ''
      NB. If this is a L: node, there must be only a single result, but we need to insert it and its fillmask
      NB. into the result map.  So, we box the fillmask and insert it into the result map at location 0, and
      NB. make the formatted result the sole result of L:
      selresult =: ,< fillmask__loc frameselresult__loc selresult__loc
      fillmask =: (fillmask__loc) [ applyintree (> resultseqmap pathfromindex 0) fillmask
   case. 1;2 do.
      NB. If this is &.> or collection error, box both the fillmask.  Leave the result single-boxed: it represents the result of the lower boxing level and will
      NB.  have another level added.  The incumbent fillmask must have a single value, but we preserve its shape
      selresult =: ,< fillmask__loc frameselresult__loc selresult__loc
      fillmask =: ($fillmask) $ < fillmask__loc
    case. do.
      NB. In other cases, format the selresult according to ITS settings, and copy the fillmask.  Leave the
      NB. selresult as an atomic box to indicate that it has been formatted already, and that fillmask applies to its opened contents
      selresult =: < fillmask__loc frameselresult__loc selresult__loc
      fillmask =: fillmask__loc
    end.
  end.
end.

NB. append the current locale to the inheritance chain - if it has anything to add
extendinheritchain loc

NB. Inherit the operands selection (for highlighting)

QP^:DEBINHU'endingecode=?edisp'''' fillmask $selresult selresult '

(<coname'') 1} y
)

NB. Extract selected cell from selresult.  Nilad
NB. The current node must have a selection.  We return the (boxed) selected cell of the selresult.
NB. This logic follows the logic in inheritu that is used to update the selected cell
extractselectedcell =: 3 : 0
NB. sel1 is the path to the selection.  It may go down multiple levels, but it will
NB. always end with a dropdown if there is a dropdown.  If the fillmask is boxed, there will
NB. be a dropdown as long as this node is selectable (it isn't in the case of L: wiehn the arguments
NB. are initially at level).
NB. So, we convert the selection to path form, which merely requires removing the dropdowns.
sel1 =. SFOPEN -.~ > isfensureselection isftorank2 sellevel { selections
NB. If the current fillmask is boxed, it has internal structure, and we
NB. should replace the selected portion with the fillmask and data that was calculated in u
select. resultlevel
NB. tmodx gives the index into selresult of the selection.  For L: we get it from
NB. the result map; for others we calculate from the index
NB. fillmask, which describes what ought to be, is in natural order.
NB. selresult, which describes what is, is in ticket order.
case. 0 do.
  NB.?lintonly resultseqmap =: ''
  NB. L:
  tmodx =. sel1 {:: resultseqmap
case. do.
  NB. expansion/each, and collection error are treated like regular selections, where we 
  tmodx =. selframe&#.&.> selectiontoticket sel1
end.
NB. Return the selected cell.
if. tmodx <:&# frame do.
  tmodx { :: ((<'?')"_) selresult
else.
NB. selresult is collected using frame, while selection uses
NB. selframe.  If selframe is longer than frame, we must select using frame first, and
NB. then continue with rest of selframe inside the selected result
  ((< (#frame) }. tmodx)&({ :: ('?'"_)))&.> (< (#frame) {. tmodx) { :: ((<'?')"_) selresult
end.
)

NB. called in locale of an operand
NB. null rankcalculus for cases where we can take no action
NB. Nilad.  We know that the current node has no selection but has valid selopshapes.
NB. Result is 2 if it is OK to turn this into a rank-calculus probe, 0 if not.
rankcalculus =: 0:
NB. y is selopinfovalid or a selection thereof (suitable for v-types, where validity comes from the selected operands)
NB. result is selector validity, operand validity: 0 if operand shapes are invalid, 1 if valid, 2 if valid but not selected, and this should turn into a rank-calculus probe
NB. If the operand is invalid, but shapes exist, we consult rankcalculus to see whether a rank-calculus probe can be performed, keeping the
NB.  operand shapes but clearing the selector
vopval =: 3 : 0
1 vopval y
:
if. *./ y do.
  x,1
else.
  if. $selopshapes do.
    x,rankcalculus ''
  else.
    x,0
  end.
end.
)
NB. y is one or two operand locales (suitable for u-types where validity comes from results of v-types)
NB. result is selector validity,operand validity: 0 if operand shapes are invalid, 1 if valid, 2 if valid but not selected, and this should turn into a rank-calculus probe
NB. If the operand is invalid, but shapes exist, we consult rankcalculus to see whether a rank-calculus probe can be performed, keeping the
NB.  operand shapes but clearing the selector
uopval =: 3 : 0
1 uopval y
:
'l1 l2' =. <"0 (2 $ y)  NB. extract locale names, possibly equal
NB.?lintonly l1 =. l2 =. <'dissectobj'
evals =. errorcode__l1 , errorcode__l2
if. evals +.@:> EOK do.
  NB. A v failed: treat the operands as invalid
  x,0
else.
  if. evals *./@:e. EHASVALIDFILLMASK do.
    NB. Both operands have valid values, mark them as valid
    x,1
  else.
    NB. one operand invalid; try turning the request into rank-calculus (questionable decision)
    x,rankcalculus ''
  end.
end.
)

NB. Flag value, in displaylevrank/rankhistory, in the titlestring, which indicates a 'heavy' line.
NB. A heavy line is one that settles to the bottom (u) of u@v.  A heavy line indicates a marker for the end of
NB. the computation started in the locale in the line.  Ex: u@v/. passes a heavy end-of-/. line into u@v,
NB. and that line gravitates to u
DLRCOMPEND =: 0

NB. Omnibus generator of y operands to traverse
NB. This runs in the locale of the current node, whose globals are collected
NB. Result is (bnsellevel);(rankhistory);(operand info)[;selector]
NB. operand info is 1 or 2 boxes, each containing physreq[;(operand shape)]
NB. y is (rankhistory column selector);(preserve physreq history);((selector validity),(operand validity));(operands shape(s))[;(operand indexes to use)]
NB.  rankhistory column selector is the list of columns to take, or a: for all, or '' for none (use NORANKHIST)
NB.  preserve physreq history is 1 to include current physreq in the (normal for a v-type node), 0 to start fresh (as for a u).  Or, a list,
NB.   giving the index of the physreq for each operand.  The index is taken from (physreq,EMPTYPRH) so _1 = EMPTYPRH etc.
NB.    whenever this is atomic 0 there MUST be operand shapes so we will know what valence to use
NB.   selector validity is 1 if the selector is valid.  If 0, selector will not appear in the result
NB.   operand validity is 0 if operand shapes are invalid, 1 if valid, 2 if valid but not selected, and this should turn into a rank-calculus probe
NB.  operand indexes to use is the list of valid operands whose shapes we will transfer to the result.  If omitted or a:, take all operand shapes
NB.  operand shapes are the boxed shapes of the known operands.  Either empty (meaning unknown, because unselected) or one boxed value per operand.
NB. Uses globals from travdowncalc: bnsellevel, rankhistory, selector, physreqandhighlights
NB. If x is given it is a gerund that is used to modify the value of rankhistory that we use, used in combination with TRAVOPSKEEPINALL

NB. Values used for rhsel below:
TRAVOPSSTARTINHEAVY =: 0&(,&<)
TRAVOPSKEEPINALL =: 1&(,&<)
TRAVOPSKEEPINLIGHT =: 2&(,&<)
TRAVOPSSTARTHEAVY =: TRAVOPSSTARTINHEAVY a:
TRAVOPSKEEPALL =: TRAVOPSKEEPINALL a:
TRAVOPSKEEPLIGHT =: TRAVOPSKEEPINLIGHT a:

NB. Values used for pphys below:
TRAVOPSPHYSNEW =: 0
TRAVOPSPHYSKEEP =: 1
TRAVOPSPHYSCHOOSE =: ,   NB. choose the ops, 0=left, _2=right, _1=empty
NB. if a list, it gives indexes to keep
travops =: 3 : 0
]`'' travops y
:
'rhsel pphys val shapes' =. 4 {. y
'sval oval' =. val
opx =. 4 {:: y , <a:  NB. Indexes of operands we are using
NB. Calculate the rankhistory to use
'rhcull rhcols' =. rhsel
NB. If we start heavy or keep light, cull the list down to those elements
select. rhcull
case. 0 do.  NB. start (always keeping heavy)
  rh =. x`:6 (2) {."1 (#~ (<DLRCOMPEND) = 0&{"1) rankhistory
case. 1 do.  NB. keep all
  rh =.  x`:6 rankhistory
case. do.  NB. keep light
  rh =. (#~ (<DLRCOMPEND) ~: 0&{"1)  x`:6 rankhistory
end.
NB. Now select the requested columns
rh =. rhcols {"1 rh
NB. Choose the physreqs to use
if. #$pphys do.
  NB. pphys specified for each operand.  In case this is a change of valence, make sure we preserve the correct number of highlights (keeping y over x),
  NB. and fill the rest with empties
  opinfo =. pphys { physreqandhighlights , <EMPTYPRH
elseif. pphys = TRAVOPSPHYSKEEP do.
  NB. Preserve physreq: use the old values corresponding to operands we are keeping
  opinfo =. opx { physreqandhighlights
elseif. do.
  NB. Starting anew: figure out the number of operands we are using: the number of operand indexes, if given, or
  NB. the number of valid shapes
  assert. 0 ~: ((#shapes)"_^:(0=]) #>opx)
  opinfo =. ((#shapes)"_^:(0=]) #>opx) # <EMPTYPRH
end.
NB. Create operand info: physreq followed by shapes if valid.  If ops not valid, this will take no shapes, without error
if. #ss =. , (((*oval) # opx) { shapes) do.
  opinfo =. <"1 opinfo ,. ss
else.
  opinfo =. <"0 opinfo 
end.
NB. Create selector, depending on validities and rank-calculus
sel =. sval # oval {:: selector;selector;<a:
(bnsellevel , rh ;< opinfo) , sel
)


NB. **************** code for display objects *********************

cocurrent 'dissect'

NB. ****************** place-and-route for wires ***********************

ROUTINGGRIDSIZE =: 5   NB. number of pixels between routing channels

yxtogrid =: %&ROUTINGGRIDSIZE
gridtoyx =: *&ROUTINGGRIDSIZE

WIRESTANDOFF =: 7  NB. min number of pixels between a wire and a block
MINROUTINGCH =: 1   NB. Number of routing channels between boxes, minimum
MINBOXSPACING =:(ROUTINGGRIDSIZE + >:MINROUTINGCH) + WIRESTANDOFF + WIRESTANDOFF  NB. Number of pixels between boxes, minimum
NB. If we know that the low side of a block is always on a grid boundary, the second WIRESTANDOFF above can be rouned down
NB. to a multiple of grids.  This is true for vertical positioning but not for horizontal, as things stand now
RGRIDDIST =: 8  NB. bits 0-3 hold move code
RMOVEEOC =: 5   NB. special start-of-route indicator
RGRIDWINDOW =: 5000*RGRIDDIST   NB. max routing distance including (possibly huge) penalties
RFRONTIERRANGE =: 1*RGRIDDIST  NB. depth of frontier.  At 0, only the shortest candidates are on the frontier.
ROUTINGMARGIN =: 4   NB. min number of gridpoints to leave around border of grid.  Used to calc routing area
NB. We need 1 for roundup of points, 1 that we use to mark the boundary for comp ease, 2 to allow 2 wires in
TGTPROXFORROLLUP =: RGRIDDIST*10  NB. if the route is farther than this much from the target, we engage full-grid analysis

INITSHELFCT =: 10  NB. Number of nearest starting places to keep for initial shelf
INITSHELFFRINGE =: RGRIDDIST*10  NB. Expand the initial shelf to include points within this distance of the top INITSHELFCT points

NB. Place-and-route line drawer for dissect
NB. Given a block placement and a set of interblock connections, we adjust the placement
NB. as needed and create the vector necessary to connect the blocks
NB. Result is the new placement (yx only), and the list of wires as table of startyx,endyx,type (0=line 1=arc)

NB. Convert each gridblock to units of gridsize, covering the gridpoints that are within a standoff
NB. y is gridblocks;nets
NB. Result is gridblocks blocked areas in grid space
initgrids =: 3 : 0
gridblocks =. y
rareasize =. <. yxtogrid (WIRESTANDOFF + ROUTINGGRIDSIZE * >: ROUTINGMARGIN) + >./ {:"2 gridblocks
NB. Create top-left,:bottom-right+1 in grid units for each block.  We block off any point that is less than a WIRESTANDOFF
NB. away from the block.  On the low side, subtract WIRESTANDOFF and round up; on the top, add WIRESTANDOFF and round down
gsctlbr1 =. <. yxtogrid gridblocks +"2 +/"2 (0 2,:1 3) { standoffbyface
NB. Create the row;column vector for each block
gscrowcolvec =. (+ i.)&.>/@(-~/\)"2 gsctlbr1
NB. Initialize the routing zero distance.  This will be reduced for every run that is routed
routingzero =: (2*RGRIDWINDOW) -~ (-RGRIDDIST) bwand _1 (33 b.) _1
NB. Create the blockage grid, a 1 where there is a blockage.  This name does not get
NB. updated during the route, if areas get bocked off, but it serves its purpose of checking for
NB. originakl blocks positioned inside a region
blockedgrid =: rareasize $ 0
for_b. gscrowcolvec do.
  blockedgrid =: 1 (<b)} blockedgrid
end.
NB. Create the penalty table, counting one unit for each move.  Other penalties will be added as routes create them
penaltygrid =: rareasize $ RGRIDDIST
NB. Initialize the penalty for adjacent to a block
gscrowcolvec =. (<:@{. , ] , >:@{:)&.> gscrowcolvec
if. RPENALTYADJBLOCK do.
  for_b. (<"1 ({. , (0 _1&{)&.>@{:)"1 gscrowcolvec) , (<"1 ((0 _1&{)&.>@{. , {:)"1 gscrowcolvec) do.
    penaltygrid =: (RGRIDDIST*>:RPENALTYADJBLOCK) b} penaltygrid   NB. include the one unit for movement
  end.
end.
NB. Install a penalty at the corners, to try to keep routes off the corners without punishing every step along the way.
NB. We can't do just the corners, because then the route can slip inside between the corner and the block.  But if we split
NB. the blockage into two cells neighboring the outside corner, we catch it.
if. RPENALTYOUTCORNER do. penaltygrid =: (RGRIDDIST*>:RPENALTYOUTCORNER) (((,: |.) 1 _2;0 _1) (<@:({&.>) , <@:({&.>))"1/ gscrowcolvec)} penaltygrid end.
NB. Block off the border of the grid to ensure routing doesn't get out of hand
blockedgrid =: 1 (<<0 _1)} blockedgrid
blockedgrid =: 1 (<a:;0 _1)} blockedgrid
routingzero =: routingzero - RGRIDWINDOW  NB. fresh start for first route
routinggrid =: (4,$blockedgrid) ($,) blockedgrid { (routingzero + RGRIDWINDOW) , _1
penaltygrid =: ($routinggrid) ($,) penaltygrid
 NB. Add a huge penalty for a move to the first (in the scan direction) cell of a blocked area.  This is used during
NB. propagation of straight moves to prevent a run from crossing over a blocked area
penaltygrid =: penaltygrid +"2 RGRIDWINDOW * (_2 ]\ _1 0 1 0 0 _1 0 1) (] > |.!.0)"1 _ blockedgrid
gsctlbr1
NB.?lintsaveglobals
)

RPENALTYCROSS =: 4  NB. penalty for wire-crossing
RPENALTYTURN =: 4   NB. penalty to assign to a turn
RPENALTYJOG =: 7   NB. number of blocks of penalty to assign to a jog
RPENALTYADJWIRE =: 1    NB. Penalty for having a wire next to a wire
NB. We want to keep routes from lying next to a block.  Unfortunately, adding a penalty to the cells next to
NB. a block makes routing inefficient, because the route has to fight through the penalty zone, and all the
NB. while it is off the top of the frontier and so never gets to extrapolate.  A better way to get the same result
NB. is to penalize the corners only, so that the route gets a chance to extrapolate.
RPENALTYADJBLOCK =: 0    NB. Penalty for having a wire next to a block
RPENALTYOUTCORNER =: 0   NB. Penalty for points outside corners - the idea is to force runs to go around them

NB. Routing schedule
NB. This gives the (overlap penalty),(neighboring-wire penalty),(crossing penalty),(score crossing penalty) for a sequence of trial routes
NB. We try these routes in order; after each one we add more space if there are crossings or overlaps.
NB. When the route stops improving, we keep the last one
NB. To begin with, we don't penalize overlaps much, so that we see where spacing would be helpful;
NB. on later tries we penalize it heavily
NB. In the early routes we spread out when there is a crossing; but after a few spreads the crossings
NB. are probably unavoidable, so we stop reacting
routeschedule =: (RGRIDDIST * ".);._2 (0 : 0)
0 ,0 ,0, 1
1 ,0 ,0, 1
3 ,RPENALTYADJWIRE ,RPENALTYCROSS, 1
500 ,RPENALTYADJWIRE ,RPENALTYCROSS, 0
)

NB. y defines the objects:  gridblocks;nets
NB.  gridblocks is a list of (topleft y,x,:bottomright+1 y,x)
NB.  nets is a list of boxes, each containing one net, a table, as source,dest...
NB.  each of source/dest is (# of gridblock),(# of face),fraction of edge displacement from center
NB.   face # 0=top, 1=bot, 2=left, 3=right
NB. Result is (block positions tlbr);<(wiring list)
NB. Where wiring list is a list of boxes with the wires for one net (in accidental block order)
routegrid =: 3 : 0
'gridblocks nets' =. y
NB. Calculate the routing area size.  Adjust blocks to leave a minimum top/left margin,
NB. and create the routing area to leave a right/bottom margin
gridblocks =. gridblocks +"1 ([: <. 0 >. ROUTINGMARGIN&-)&.(%&ROUTINGGRIDSIZE) (<./ {."2 gridblocks) - WIRESTANDOFF
NB. We will perform the trial place-and-route, saving all the results so we can choose the one we like best
prevscore =. bestscore =. _  NB.?lintonly [ bestroute =: 2$a:
NB. The routing level selects the penalties to use.
NB. For the early routes, we don't penalize overlaps.  This allows each route to find its natural best
NB. spot.  If that leaves overlaps, we spread the array and try again.
NB. If spreading the array doesn't reduce the number of overlaps, we add a modest penalty for overlaps and see if
NB. that is enough incentive to avoid them, and continue expanding as long as there is improvement.
NB. Finally, if there are still overlaps, we slap on a punitive penalty and do a final route, accepting what results
routelevel =. 0
firsttimeatlevel =. 1
whilst. do.
  'ov neigh cross scorecross' =. routelevel { routeschedule
  gsctlbr1 =. initgrids gridblocks
QP^:DEBROUTE'(<a:;a:;0){drg ' [ drg =. '*ST ' {~ (_1,(routingzero + RMOVEEOC),(routingzero + RGRIDWINDOW)) i. routinggrid
  NB. Route the nets. result is table of boxes, one row per net, holding
  NB.  (list of boxes each holding path of a routed run);(table of other wires) where the path of the routed run is
  NB.  (table of dir,row,col,movetype of occupied cells)
  route =. (ov,neigh,cross) routenets gridblocks;<nets
  if. 0= #route do. score =. 0  NB.?lintonly  [ 'crosspts overlapsns overlapsew' =. 3 0 3$0
  NB. Coalesce routes, but first append the net number to each route
  elseif. #occupied =. ; (,.&.> i.@#) {."1 route do.
    NB. Get yx of places where spread is needed.  These are overlaps and crossings, which we figure out from the occupied cells.
    NB. The crossings of turn/jog over turn/jog are handled by creating synthetic 'occupations' at the bend corners.  We do that after routing
    NB. each net, to create a penalty at each such point; and again here
    NB. Since these synthetic points should not signal crossings but only overlaps (indeed, the points for a corner cross themselves),
    NB. we create a separate list of them and see if any of them show up in the occupied list
    NB. Both ends of a turn/jog, i. e. the turn and the preceding cell, are marked perp to the routing direction.  But only once.
    occupied =. occupied , 2 0 0 0 0 bwxor"1 (~. (, >:) (3 {"1 occupied) I.@:e. 1 2 3 4) { occupied
    NB. Convert occupation table to y,x,nsdir form (nsdir instead of nswedir)
    occupied =. /:~ (nswetons@:(0&{"1) ,. 1 2 4&{"1) occupied
    NB. Look for consecutive identical points in the sorted list
    'crosspts overlapsns overlapsew' =. gridtoyx&.> ((<(<0);1 2)&{ <@#~ (2 -:&0 1 1 0@:=/\ ]) ,  (<(<0);0)&{ (< ,: *.)  2&((-:&}: *. ~:&{:)/\)) occupied
    NB. Score the placement: 1 point for a crossing, a zillion for occupancy>1
    NB. Penalize crossings only on the first routes
    score =. 1 1000000 1000000 +/@:* #@> (scorecross #&.> crosspts);overlapsns;overlapsew
  elseif. do. score =. 0  NB.?lintonly  [ 'crosspts overlapsns overlapsew' =. 3 0 3$0
  end.
  NB. If this is a new routing level, and there were overlaps at the previous level, we must
  NB. expand going to the new level.  If this is the same level, expand only if there is an improvement in overlaps
  adjneeded =. firsttimeatlevel { (score < 1000000 >. bestscore - 500000) , (prevscore >: 1000000)
  if. score < bestscore do.
    bestscore =. score
    bestroute =. ({."2 gridblocks);<route
  end.
  NB. Get routing level to use for the next route
  NB. Advance the routing schedule if we do not adjust the placement - we must be
  NB. just trying the new routing weights.  But also advance if we are running with
  NB. no overlap penalty - the routes will continue to overlap if we don't penalize overlaps
  routelevel =. routelevel + firsttimeatlevel =. (-. adjneeded) +. ov = 0
NB. If the placement is perfect, or we have gone through all the routing schedule, stop looking
  if. (score = 0) +. (routelevel = #routeschedule) do. break. end.
  if. adjneeded do.
    NB. Not perfect, but we want to try to improve it.  Adjust the placement for the next try.
    NB. Convert block positions to form used for vertical analysis
    vblockparms =. 0 analform gridblocks
    NB. Associate each needed spread with the block below it
    NB. This produces y,x,block
    NB. First time through, count each overlap twice, because a blockage probably results from a turn,
    NB. which needs two routing channels
    vspreadblockcol =. ((>:&0@] # ,.) vblockparms&blocknoforpoint) crosspts,(>:ov=0) # overlapsew
    hspreadblockcol =. ((>:&0@] # ,.) (1 analform gridblocks)&blocknoforpoint) crosspts,(>:ov=0) # overlapsns
    NB. Move the start and end+1 of the blocks the requested amount
    gridblocks =. gridblocks +"1"2 1 gridtoyx vblockparms blockshift vspreadblockcol
    NB. Repeat for horizontal.  The spreads were calculated before the vertical moves and associated
    NB.  with blocks then; we use those moves, but calculate dependencies after the vertical move
    NB.  has been performed
    gridblocks =. gridblocks +"1"2 1 gridtoyx (1 analform gridblocks) blockshift hspreadblockcol
  end.
  prevscore =. score
end.
4!:55 ;:'routinggrid penaltygrid blockedgrid'  NB. remove the large globals
'bestgrids bestoccwires' =. bestroute
bestgrids ;< (,  occtowires)&.>~/"1 bestoccwires
NB.?lintsaveglobals
)

NB. 4x5x3x2x2.  Index is (direction),(move type) [,(vector number),(start/end),(y/x)]
NB. The index comes from the cell moved into, and has the direction and type of the move at the moved-to cell.
NB. The result is the table of vectors (y/x) to be drawn for the turn.  Each vector is relative to the
NB. gridpoint moved into
NB. Direction 0: the route entered going north (this wire is in the opposite direction)
turnsn0 =. 3 2 2 $ 0
turnsn1 =. 2 ,:/\ _2 ]\ 0 0  1 0  ,ROUTINGGRIDSIZE, (-ROUTINGGRIDSIZE-1)   ,ROUTINGGRIDSIZE ,(-ROUTINGGRIDSIZE)
turnsn2 =. 2 ,:/\ _2 ]\ 0 0  1 0  ,ROUTINGGRIDSIZE, (ROUTINGGRIDSIZE-1)   ,ROUTINGGRIDSIZE ,ROUTINGGRIDSIZE
turnsn3 =. 2 ,:/\ _2 ]\ 0 0  0 0  ,(ROUTINGGRIDSIZE-0),ROUTINGGRIDSIZE   ,ROUTINGGRIDSIZE ,ROUTINGGRIDSIZE
turnsn4 =. 2 ,:/\ _2 ]\ 0 0  0 0  ,(ROUTINGGRIDSIZE-0),(-ROUTINGGRIDSIZE)   ,ROUTINGGRIDSIZE ,(-ROUTINGGRIDSIZE)
turnsn5 =. 2 ,:/\ _2 ]\ 0 0 2 2 4 4  ,ROUTINGGRIDSIZE,ROUTINGGRIDSIZE
turnsn6 =. 2 ,:/\ _2 ]\ 0 0 2 _2 4 _4  ,ROUTINGGRIDSIZE,(-ROUTINGGRIDSIZE)

turnsn =. turnsn0,turnsn1,turnsn2,turnsn3,turnsn4,turnsn5,:turnsn6
NB. Direction 1: route entered going south
turnss =. -turnsn
NB. Direction 2: route entered going west
turnsw =. |."1 (1 _1) *"1 turnsn
NB. Direction 3: route entered going east
turnse =. - turnsw
turnmoves =: turnsn,turnss,turnsw,:turnse

NB. 4x5x2, indexed by (dir,jogmove), giving dir,move for the combining turnmove
'N S W E' =. 0 1 2 3
uglyturnforjogn =. 2 2 $ W,1 , E,2
uglyturnforjogs =. 2 2 $ E,1 , W,2
uglyturnforjogw =. 2 2 $ S,1 , N,2
uglyturnforjoge =. 2 2 $ N,1 , S,2
uglyturnforjog =: 4 1 #"2 uglyturnforjogn,uglyturnforjogs,uglyturnforjogw,:uglyturnforjoge

NB. y is table of dir,row,col,movetype of occupied cells
NB. Result is table of wires
occtowires =: 3 : 0
if. 0 = #y do. 0 4$0 return. end.
yxvals =. 1 2 {"1 y
movetype =. 3 {"1 y
NB. For move type 0 (straight run), find the first and last+1 in a sequence and put a wire between them
NB. The last movetype is always RMOVEEOC
straightwires =. (($,)~ 4 (%~ , [) */@$) gridtoyx yxvals #~ (~: |.!.0) 0 = movetype
NB. For bends and jogs, fetch the vector list for each and add in the position
NB. First, cover a blemish: a turn and jog ending on the same point look bad.  So if that happens,
NB. delete them and replace them with a new movetype (we reuse ) that gives the combined display.
NB. Since there aren't many jogs, we start with them and see if there are matching turns
turnjogs =. y {~ movetype I.@:e. 1 2 3 4
if. #jogs =. (#~   3 4 e.~ 3&{"1) turnjogs do.
  turns =. (#~   1 2 e.~ 3&{"1) turnjogs
  NB. create the turns that would match the jogs.  For a left-jog, the ugly turn is a left-turn
  NB. whose direction is 90 degrees ccw of the jog; for a right-jog, a right-turn 90 degrees cw of the jog
  uglyturns =. 2 0 1 3 {"1 (1 2 {"1 jogs) ,. (0 3 {"1 jogs) (<"1@[ { ]) uglyturnforjog
  NB. Get the jog (if any) corresponding to each turn
  jogx =. uglyturns i. turns
  NB. Remove the turns that had a matching jog - keep the ones that didn't
  turns =. (jogx = #uglyturns) # turns
  NB. Replace the matched jogs with corner moves.  We keep the same direction as the jog, but use the new turn type
  jogx =. ~. (#~ <&(#jogs)) jogx
  jogs =. (2 + (<jogx;3) { jogs) (<jogx;3)} jogs
  turnjogs =. turns , jogs
end.
turnwires =. (($,)~ 4 (%~ , [) */@$) (gridtoyx@:(1 2&{"1) +"1 turnmoves (<"1@[ { ])~ 0 3&{"1) turnjogs
NB. Ignore RMOVEEOC, which ends each run
straightwires , turnwires
)

NB. Create analysis form for gridblocks:
NB. (gridblocks);(block-start positions in ascending order,:block-end positions in ascending order);(grading vector for starts,grading vector for ends);(1 if horizontal)
NB. y is gridblocks
NB. x is 0 if we are analyzing for vertical spread, 1 for horizontal
analform =: 4 : 0
NB. Get start, end+1 values for "x"
startend =. |: (-. x) {"1 y
grades =. /:"1 startend
y;(grades {"1 startend);(<"1 grades);x
)

NB. x is analysis form for blocks, which for finding the block below a point is
NB. (gridblocks);(block-start positions in ascending order,:block-end positions in ascending order);(grading vector for starts,grading vector for ends);(1 if horizontal)
NB. y is table of yx of points
NB. Result is index of block below each point, or _1 if no block is below it 
blocknoforpoint =: 4 : 0
'gridblocks startend bgrades horiz' =. x
'height intsel' =. (horiz |. 0 1) {"0 1/ y
NB. For each "x" value, get number of intervals that start before and that end before.  The overlapping intervals are those that started
NB. but have not ended.
NB. x I. y finds the number of values in x that are less than y.
NB. To get the number of blocks that started before or equal to intsel, we add 1 to intsel.
NB. To get the number of blocks whose end is less than intsel, we subtract 1 from intsel (since the block holds end+1)
candidates =. -.&.>/"1 bgrades {.&.>"1~ startend I."1 0"_ 1 intsel +/ 1 _1
NB. Choose the candidate with the smallest "y" greater than the point.  _1 if none
height (,&_1@] {~ ((] i. <./@(< # ])) {&((<0,horiz) {"2 gridblocks)))&> candidates
)

NB. Create boxed list of blocks that depend on y.  The first value is always y itself
NB. y is block number, or <block number,exclo,exchi+1
NB.  where exclo,exchi is the range of "x" values that have been handled already and should not be reexamined (perf boost)
NB. x is the analysis form of the selected direction
blockdependencies =: 4 : 0"1 0
'gridblocks startend bgrades horiz' =. x
'blockno exclo exchi' =. 3 {. 10000000 0 ,~ >y
NB. Find blocks that overlap the current block
blockstend1 =. (<blockno;a:;-.horiz) { gridblocks
NB. The overlapping blocks are those that have startpoints less than the end+1 of the current block,
NB. but not including ones whose end+1 is less than the start+1 of the current block
overlaps =. -.&>/ bgrades {.&.>~ startend I."1 0 |. blockstend1 + 1 0
NB. Remove those that are above the current block.  The remainder are dependencies, though
NB. they may have been processed earlier
overlaps =. overlaps #~ (<blockno;0;horiz) <&({&gridblocks) (<overlaps;0;horiz)
NB. Remove those that are entirely within the (input exclusion zone+current block).  They do not overlap
NB. any block that hasn't been overlapped before, so they do not require recursion
olapstend1 =. (<overlaps;a:;-.horiz) { gridblocks
exczone =. (exclo <. {. blockstend1) , (exchi >. {: blockstend1)
recursneeded =. overlaps #~  <:/"1  exczone <:"1 (0 _1)+"1 olapstend1
NB. Recur on the remaining outlying dependencies; collect their results and discard duplicates
~. blockno , overlaps , ; (x blockdependencies <@(,&exczone))&.>^:(*@#) recursneeded
)

NB. x is (number of bumps for the block),(block #),(#s of dependent blocks)
NB. y is (block number),:(# bumps) for previously-processed blocks
NB. We know that this block (1{x) will not be dependent on any subsequent block, so
NB. we can establish its movement value here
NB. result is new value for (block number),:(# bumps)
bumpprop =: 4 : 0
'bump block' =. 2 {. x
NB. Amount to move this block is total of its own moves plus its inherited moves
totalbump =. bump + (({ ,&0)~ i.&block)~/ y
NB. Amount to move other blocks is maximum of their inheritances.  Append a column
NB. with (block #),:total for each block, including this block, and take the maximum for each
NB. block.  The value for this block will necessarily be the one computed here, and will not be changed
NB. again, because of the initial topologocal sort.
(~.@[ ,: >.//.)/ y ,. (}. x) ,: totalbump
)

NB. x is analysis form
NB. y is ("y","x",block) for each added spread
NB. Result is amount to shift each gridblock, a list per gridblock to be added to each row
blockshift =: 4 : 0
'gridblocks startend bgrades horiz' =. x
if. 0 = #y do. (2 ,~ #gridblocks) $ 2-2 return. end.
NB. Calculate number of bumps per block, which is the maximum number of requested bumps in a single column
'blocks bumps' =. ({:"1 (~.@[ ,: >./@(#/.~)/.) (-.horiz)&{"1) y
NB. For each block, create (number of bumps for the block),(block #),(#s of dependent blocks)
bumpblockdepends =. bumps ,&.> x <@blockdependencies blocks
NB. Cascade the required movements through the dependencies.  If block A depends on block B, the dependency
NB. list for A must be greater than that of B.  So, decreasing order of #dependents is a topological sort.
NB. We create a table where the first row is block # and the second row is #bumps inherited.  Initially, this is
NB. empty.  For each block, we add the bumps required at this block to the # inherited for this block, giving the 
NB. precise number of bumps for this block.  We then use the # for the block to >. to find the number of inherited
NB. bumps for blocks depending on this block.
blockmoves =. bumpprop&:>/ ((\: #@>) bumpblockdepends) , <2 0$0
NB. Move the start and end+1 of the blocks the requested amount
(horiz { 2 _2) {."0 ({: blockmoves) ({. blockmoves)} (#gridblocks) # 2-2
)



NB. lookup to convert a face number into fetch indexes to get start,end
NB. Indexes to fetch from ystart,xstart,:yend,xend to give y1y2,:x1x2
facex =: 4 2 2 2 $ 0 0  0 0  0 1  1 1    1 0  1 0  0 1  1 1   0 0  1 0  0 1  0 1   0 0  1 0  1 1  1 1

NB. perform a routing pass
NB. x is penalties: overlap,neighboring run,crossing
NB. y is gridblocks;nets
NB. grids are used as globals, and modified
NB. Result is (list of boxes, each containing occupancy list for one run);table of wires: start,end,type (0=line 1=arc)
routenets =: 4 : 0
'gridblocks nets' =. y
if. #nets do.
NB. Convert the nets from gridblock,face,fraction to y,x,face.  We leave the direction parallel to the face
NB. on a grid boundary; the other is allowed to float
  fnets =. ; nets   NB. Flatten the nets for proc ease; will reconstitute later
  y12x12 =. (facex {~ 1&{"1 fnets) (<"1@:[ { ])"3 2 gridblocks ({~ (0&{"1)) fnets
NB. Find the center point; round to grid; clamp to within interval (the clamp will ensure no movement along perp direction
  centers =. ({."1 >. {:"1 <. [: <.@(0.5&+)&.(%&ROUTINGGRIDSIZE) -:@:(+/"1)) y12x12
NB. The amount to move is the given fraction of the face, rounded toward 0 to a multiple of ROUTINGGRIDSIZE, which we achieve by adding a small
NB. sign-dependent amount which should be good enough for screen resolutions
  adjust =. (2 {"1 fnets) <.@(0.5&+ + 1e_6 * *)&.(%&ROUTINGGRIDSIZE)@:* -~/"1 y12x12
  nets =. (; (1 {.~ #)&.> nets) <;.1 (centers + adjust) ,. 1 {"1 fnets
  
NB. Sort nets to route shorter ones first.  This might reduce crossings?
  nets =. (/:   +/@(>./ - <./)@:(2&{."1)@>) nets
end.
NB. See which nets do not require routing, and draw them directly
if. 1 e. ddrawmsk =. *./@:(0&~:)@({. directdrawok }.)@> nets do.
  routes =. (<0 4$0) ,. ({. ,"1&:(}:"1) }.)&.> ddrawmsk # nets
else. routes =. 0 2$a:
end.
NB. Route the nets that need routing
if. 0 e. ddrawmsk do.
  routes =. routes , x&routenet@> (-. ddrawmsk) # nets
end.
NB. Return
routes
NB.?lintsaveglobals
)

NB. point in polygon, for convex ccw polygon (2D)
NB. x is the polygon
NB. y is the point(s)
NB. result is 1 if point is tolerantly in polygon, including on the edge
pipccw =: (0&(*./@:<:))"1 @: (2&((-/ . *)\)"2) @: (-"1/ (, {.))~

NB. convex hull
NB. y is a table of y,x value (or x, y if you want to think of it that way)
NB. result is y,x points of the convex hull, in CCW order in left-handed coordinates
convexhull =: 3 : 0
pts =. y
NB. Find points with max/in x and y; make a ccw quadrilateral out of them: 4 edges, closed
ccwminmax =. (, {.) pts {~ 0 2 1 3 { , (i.!.0    <./ , >./)"1 |: pts
NB. Calculate outside-the-edge masks for each point, and discard points inside all edges (Eddy-Floyd)
NB. The determinant of (y,x) in ccw order in left-handed system is positive, so we take polygon-point to get + determinant for ccw
anyout =. +./"1 outmask =. ccwminmax 0&(>!.0) @: (2&((-/ . *)\)"2) @: (-"1"_ 1) pts
outpts =. anyout # pts
outmask =. anyout # outmask
assert. *./ 1 = +/"1 outmask  NB. each point can be outside only one edge
NB. Each surviving point will be outside exactly one edge; associate the point with that edge
outpts =. ((=/~ i. 4) -:"1/ outmask) <@# outpts
NB. Find the convex hull corresponding to each edge.  The two endpoints PQ of the edge are known to
NB. be on the hull.  Sort the points outside the edge into the order they will be encountered in a CCW sweep from P.
NB. For each edge AB, we calculate the winding of the triangle ABC (C is the next point).  If ABC is cw, delete point B.
NB. Repeat the procedure until no points are deleted.
NB. Taking, say, the first edge, which goes from ymin to xmin, the points must have a smaller x than the edge point but they could have equal y.
NB. So calculate the slope as dy/dx, sort decending.  For the second edge, the points must have larger y, so
NB. calculate -dx/dy, sort decending.  For third, dy/dx descending; for fourth, -dx/dy descending
sortpts =. outpts \:&.> 1 _1 1 _1 *&.> %/"1&.> 0 _1 0 _1 |."1&.>  outpts -"1&.> <"1 }: ccwminmax
hullpts =. (<"1 }. ccwminmax) ({.@] , }.@] (#~ 0&(<!.0)) 3&((-/ . *)@:(}. -"1 {.)\)@:,~)^:(1<#@])^:_:&.> sortpts
NB. Append each quadrilateral point with the following hull points and run together to form the result
; (<"1 }: ccwminmax) ,&.> hullpts
)

NB. Centroid of polygon
NB. y is table of 1 or more points, each y,x
NB. Result is centroid y,x
polygoncentroid =: 3 : 0
NB. Centroid of a single point is that point; and we can't handle 0 points
if. 2 > #y do. y return. end.
NB. Create wraparound list of points.  This is faster than subtracting away the first point
points =. (, {.) y
NB. We split the polygon into triangles.  Calculate the areas
if. 0.0001 > | area =. +/ areas =. 2 -/ . *\ points do.
  NB. If the total area is 0, we have a degenerate polygon; return the average of the points
  (+/ % #) y
else.
  NB. Calculate the centroid coordinates according to the formula
  (+/ (2 +/\ points) * areas) % 3 * area
end.
)


NB. Angle ranges for allowable lines, startpoint to endpoint, in the screen's upside-down y coordinates
angleranges =: 12 o. _10j_1 _1j_10 1j_10 10j_1 10j1 1j10 _1j10 _10j1
NB. OK range values for each type of first face
facerange =: 1 2 3 3 , 5 6 7 7 , 7 8 0 1 ,: 3 4 5 5
NB. OK range values for each type of second face
revfacerange =: 1 0 3 2 { facerange
NB. x is source, y is table of destinations.  Each is y,x,face
NB. Result is mask, one bit per destination, indicating that it is OK to draw that wire directly:
NB. 0=no direct-draw, 1=direct-draw allowed, 2=direct-draw required (if deltax is +-1 grid)
directdrawok =: 4 : 0
NB. Create the angle of each wire
angles =. angleranges I. 12 o. j.~/"1 yxdiff =. y -"1&:(2&{."1) x
NB. See if the angle is in the OK range for the source face, and if the negative is in the OK range for the target face
NB. But don't direct-draw any vertical or horizontal line longer than 3 grids, since they could overlap a
NB. routed line exactly
angleok =. (-. (0 e."1 yxdiff) *. ((MINBOXSPACING + gridtoyx 1) > +/"1 yxdiff)) *. (angles e. (2{x) { facerange) *. angles e."0 1 (2 {"1 y) { revfacerange
NB. Allow the wire if no vertices in the box containing the wire's corners (adjusted inward a smidgen)
NB. We move the wire away from the face by one gridunit more than the calculation used to move the corners.
NB. Then we use those values as corners, and look to see whether the region is clear.  We allow direct routing if so.
movedpoints =. <. yxtogrid (0 1&{"1 + standoffbyfaceplusone {~ 2&{"1) x , y
NB. Kludge - should create yx for each point and check just those
interiorblocked =.  blockedgrid +./@:,;.0~ ({. (<. ,: >:@:|@:-)"1 }.) movedpoints
angleok > interiorblocked
)

'N S W E' =. _1 0 , 1 0 , 0 _1 ,: 0 1
NB. For each face, the amount to adjust a coordinate to move outward-perpendicular to the face to a gridpoint
NB. at least a WIRESTANDOFF away.  This move will be followed by <.@yxtogrid.  For faces pointing down, we
NB. move down by the standoff and then round up; for faces pointing up, we move up by the standoff and then round down
standoffbyface =: _2 ]\  ((<:ROUTINGGRIDSIZE)-WIRESTANDOFF) ([,0 , ],0 , 0,[ , 0,]) WIRESTANDOFF
standoffbyfaceplusone =: standoffbyface + _2 ]\ (-ROUTINGGRIDSIZE) ([,0 , ],0 , 0,[ , 0,]) ROUTINGGRIDSIZE  NB. One gridpoint more than the routing point

NB. Route one net
NB. x is list of penalty values
NB. y is a net (table of y,x,face)
NB. Globals in use: grids, blockzero
NB. Result is (occupancy table: cells filled by routed wires);(wires to connect the display blocks to the routing grid)
NB. occupancy is y,x,dir,movetype
routenet =: 4 : 0
'penov penneigh pencross' =. x
NB. Create the routing positions of the source and dests.  Since we have placed the handle on a gridpoint
NB. in the parallel-to-face direction, it can function as the true end-of-route in that direction.  The routing position, which must
NB. be a gridpoint, is the closest gridpoint that is a WIRESTANDOFF away from the point.  This will necessarily have
NB. the same parallel coordinate as the true routeend, but the other coordinate may be adjusted.  The point so created will
NB. always be a point that was blocked off in the grids, but we will override that when we route the net, to allow routing to the point
trueroutend =. <. 2 {."1 y
routend =. <. yxtogrid trueroutend + (faces =. 2 {"1 y) { standoffbyface
NB. Attach the direction to each point.  For the destinations, this is the opposite of the face normal: face 0 (top) requires an entry
NB. in direction 1 (south).  For the source, the initial direction is the same as the face normal
routendface =. routend ,.~ faces bwxor (#faces) {.!.1 (0) NB. dir,gridy,gridx (nswe) for each routing point
NB. Order the points for routing.  Shortest run first.  We have tried: closest to centroid of points; closest to cnetroid of the convex hull
startroute =. {. routendface
remroute =. }. routendface
routbydist =. remroute ([ \: +/"1@:|@:(-"1)&:(1 2&{"1)) startroute
NB. The routing points are generally on the display block and therefore start out as blocked (=_1).
NB. To allow routing to them, we need to mark them as unrouted points for purposes of calculating
NB. penaltyrollup.  We further need to mark the initial frontier as zero-length, but we do that
NB. after we have culled the initial frontier.  It is OK to leave non-frontier points unblocked,
NB. because no route can ever go to one, except when it is the target
routinggrid =: (routingzero+RGRIDWINDOW) (<"1 routendface)} routinggrid
if. TGTPROXFORROLLUP < RGRIDDIST * +/ | ({. routbydist) -&:(1 2&{"1) startroute do.
  NB. We roll up the penalties back toward the 'starting' end of the grid (for North, that's the South end)
  NB. But we install a large penalty at the start of every blockage, which will prevent runs from
  NB. extending over the blockage
  NB. The penaltygrid includes the cost of moving to the cell (1 unit), plus a full GRIDWINDOW penalty for the first
  NB. cell (in the scan direction) of a blocked area.  Cells that are blocked later (by routes) are blocked by a hefty penalty
  NB. in the penaltygrid and do not show here as 'blockages'
  blockages =. routinggrid < 0
  penaltyrollup =. (-. blockages) * ('+/\.','+/\','+/\."1',:'+/\"1') 128!:2"1 2 penaltygrid
  NB. Prevent the blockage itself from affecting the subsequent run by installing a big negative rollback
  NB. value, which pushes the blockage value above all filled cells
  penaltyrollback =. penaltyrollup + blockages * -(-RGRIDDIST) bwand _1 (33 b.) _1
else. penaltyrollback =. penaltyrollup =. $0
end.
NB. Do all the routes; create the wires to connect the block I/Os to the routing grid (discard any of 0 length)
routeoccwires =. ((penaltyrollup;penaltyrollback)&routerun@;&:>/ (<"1 routbydist) , < ,: startroute , RMOVEEOC) ; ((gridtoyx routend) (-.@-:"1 # ,.) trueroutend)

NB. After the net is complete, install penalties for subsequent nets.  They shouldn't take effect until the net is finished
NB. If we have no overlap penalty, other penalties hardly matter, so skip them all for a little speed
if. penov do.
  NB. Install penalties near the routes.  Remove route ends
  routepos =. (#~ RMOVEEOC ~: (3&{"1)) allroutes =. 0 {:: routeoccwires
  NB. First, overlaps
  dir =. (0 {"1 routepos) bwxor/ 0 1
  yx =. 1 2 {"1 routepos
  penpos =. dir ,: yx   NB. 2xnx2: 2 dirs ,: y,x
  NB. Also install overlap penalties for the crossing points of bends/jogs
  if. #turnx =. , (,. >:) (3 {"1 allroutes) I.@:e. 1 2 3 4 do.
    penpos =. penpos ,. bjends =. (((<turnx;0) { allroutes) bwxor/ 2 3) ,: ((<turnx;1 2) { allroutes)
  else. bjends =. $0
  end.
  penposb =. <"1 ((,.~ <"1)~ <"0)/ penpos
  penaltygrid =: (penov + penposb { penaltygrid) penposb} penaltygrid
  NB. Adjacencies.  Add and subtract 1 from the crossing direction to find the place to add the penalty
  NB. Include the penalty on the crossing direction of bends/jogs too
  penposb =. <"1 (<"1@[ ,. (+&.>  (_1 1;0) |."0 _~  0&(e."1))~)/ penpos
  penaltygrid =: (penneigh + penposb { penaltygrid) penposb} penaltygrid
  NB. Corners of a bend/jog count as a crossing + 2 neighbors too (in both directions), so the route doesn't avoid a crossing penalty by going through it;
  NB.  and then we throw in a jog penalty too, because routes crossing over a bend are really confusing
  NB. This probably overpenalizes the outer corner, but we really would rather leave that open, and it's easier to
  NB. calculate both corners
  if. #bjends do.
    NB. convert y0,x0 ,: y1,x1 to y0,x1 ,: y1,x0
    penposb =. <"1 a: ;"1 <"0 (_2) ({."1 ,. |.@:({:"1))\ {: bjends
    penaltygrid =: (((RGRIDDIST*RPENALTYJOG)+pencross+2*penneigh) + penposb { penaltygrid) penposb} penaltygrid
  end.
  NB. Crossing.
  penpos =. <"1 (<"1 (2) bwxor dir) ,. <"0 yx
  penaltygrid =: (pencross + penpos { penaltygrid) penpos} penaltygrid
end.

routeoccwires
NB.?lintsaveglobals
)

NB. y is array of nswe (0=n, 1=s, 2=w, 3=e), result is array of ns (0=ns, 1=ew)
nswetons =: _1&(33 b.)

NB. 4x5x4, (nswedir) x (move types) x (dir,row,col,distance/move)
NB. This gives the amount to add to the frontier dir/dist to update it
NB. The amount added to the distance includes the move-direction code to store into the grid
NB. Order of moves is straight, turn left, turn right, jog left, jog right
NB. If we are moving n (0), subtract 1 from y, modify x & direction.  Dist/movecode will be added below
movesn =. 0 0 0 -"1~ _3 ]\ _1 0 0  _1 _1 2  _1 1 3  _1 _1 0  _1 1 0
NB. Moving s (1), add to y
movess =. 0 0 1 -"1~ _3 ]\ 1 0 1  1 1 3  1 _1 2  1 1 1  1 _1 1
NB. Moving w (2), subtract from x
movesw =. 0 0 2 -"1~ _3 ]\ 0 _1 2  1 _1 1  _1 _1 0  1 _1 2 _1 _1 2
NB. Moving e (3), add to x
movese =. 0 0 3 -"1~ _3 ]\ 0 1 3  _1 1 0  1 1 1  _1 1 3  1 1 3
NB. reorder to dir,y,x,move.  The move penalty here does not include the one-unit cost for each move, which is
NB. already factored in to penaltygrid
movesbydir =: 2 0 1 3 {"1 (movesn,movess,movesw,:movese) ,."2 (i.@# + RGRIDDIST&*) 1 2 2 # 0,RPENALTYTURN,RPENALTYJOG

NB. 4x6x3, (nswedir) x (directioncode) x (row,col,dir)
NB. Gives the amount to add to the row,col,dir to get to the previous point in the route
NB. The last line, for the code in the initial frontier, is 0 to end the search
NB. tautology: 
NB. ( 3&{. -: backmovesbydir ((3 {. ]) + ({~ <@(2&{ , (<:RGRIDDIST) bwand {:))) movesbydir&(] + ({~ <)) ) row,col,dir,0
NB. Move type is straight, left turn, right turn, left jog, right jog, halt
NB. Route direction is n (0)
backn =. 0 0 0 -"1~ _3 ]\ 1 0 0  1 _1 3   1 1 2   1 1 0   1 _1 0  0 0 0
backs =. 0 0 1 -"1~ _3 ]\ _1 0 1  _1 1 2   _1 _1 3   _1 _1 1   _1 1 1  0 0 1
backw =. 0 0 2 -"1~ _3 ]\ 0 1 2  1 1 0   _1 1 1   _1 1 2   1 1 2  0 0 2
backe =. 0 0 3 -"1~ _3 ]\ 0 _1 3  _1 _1 1   1 _1 0   1 _1 3  _1 _1 3  0 0 3
NB. reorder to dir,y,x
backmovesbydir =: 2 0 1 {"1 backn,backs,backw,:backe

T =. 1+RPENALTYTURN
J =. 1+RPENALTYJOG
NB. 4x4x9x9, indexed by (source dir, target dir, dy class, dx class).  target-source
NB. The minimum turn penalties for a given position
NB. sourcedir=n, targetdir=n
NB. dy<_3
tpnn_4 =. (_4+2*T),(_4+2*T),(_4+2*T),(_2+J),(0),(_2+J),(_4+2*T),(_4+2*T),(_4+2*T)
NB. dy=_3
tpnn_3 =. tpnn_4
NB. dy=_2
tpnn_2 =. tpnn_4
NB. dy=_1
tpnn_1 =. (_4+J+2*T),(_4+J+2*T),(4*T),(_2+J),(0),(_2+J),(4*T),(_4+J+2*T),(_4+J+2*T)
NB. dy=0
tpnn0 =. (_4+4*T),(4*T),(4*T),(4*T),(0),(4*T),(4*T),(4*T),(_4+4*T)
NB. dy=1
tpnn1 =. (_4+4*T),(4*T),(4*T),(4*T),(4*T),(4*T),(4*T),(4*T),(_4+4*T)
NB. dy=2
tpnn2 =. tpnn1
NB. dy=3
tpnn3 =. tpnn1
NB. dy>3
tpnn4 =. tpnn1
tpnn =. tpnn_4,tpnn_3,tpnn_2,tpnn_1,tpnn0,tpnn1,tpnn2,tpnn3,:tpnn4
NB. sourcedir=n, targetdir=s
NB. dy<_3
tpns_4 =. (_2+2*T),(_2+2*T),(_2+2*T),(_2+J+2*T),(_2+4*T),(_2+J+2*T),(_2+2*T),(_2+2*T),(_2+2*T)
NB. dy=_3
tpns_3 =. tpns_4
NB. dy=_2
tpns_2 =. tpns_4
NB. dy=_1
tpns_1 =. (_2+2*T),(_2+2*T),(_2+2*T),(_2+J+2*T),(4*T),(_2+J+2*T),(_2+2*T),(_2+2*T),(_2+2*T)
NB. dy=0
tpns0 =. (_2+2*T),(_2+2*T),(_2+2*T),(J+2*T),(2+4*T),(J+2*T),(_2+2*T),(_2+2*T),(_2+2*T)
NB. dy=1
tpns1 =. (_2+2*T),(_2+2*T),(_2+2*T),(_2+J+2*T),(4*T),(_2+J+2*T),(_2+2*T),(_2+2*T),(_2+2*T)
NB. dy=2
tpns2 =. (_2+2*T),(_2+2*T),(_2+2*T),(_2+J+2*T),(_2+4*T),(_2+J+2*T),(_2+2*T),(_2+2*T),(_2+2*T)
NB. dy=3
tpns3 =. tpns2
NB. dy>3
tpns4 =. tpns2
tpns =. tpns_4,tpns_3,tpns_2,tpns_1,tpns0,tpns1,tpns2,tpns3,:tpns4
NB. sourcedir=n, targetdir=e
NB. dy<_3
tpne_4 =. (_4+3*T),(_4+3*T),(_4+3*T),(_4+3*T),(_2+J+T),(_2+T),(_2+T),(_2+T),(_2+T)
NB. dy=_3
tpne_3 =. tpne_4
NB. dy=_2
tpne_2 =. ((0<.J-4)+3*T),((0<.J-4)+3*T),((0<.J-4)+3*T),((0<.J-4)+3*T),(_2+J+T),(_2+T),(_2+T),(_2+T),(_2+T)
NB. dy=_1
tpne_1 =. (3*T),(3*T),(3*T),(3*T),(2+3*T),(_2+T),(_2+T),(_2+T),(_2+T)
NB. dy=0
tpne0 =. (3*T),(3*T),(3*T),(3*T),(2+3*T),(2+3*T),(_2+J+T),(_2+J+T),(_2+J+T)
NB. dy=1
tpne1 =. (_2+3*T),(_2+3*T),(_2+3*T),(_2+3*T),(3*T),(3*T),(3*T),(_4+3*T),(_4+3*T)
NB. dy=2
tpne2 =. (_2+3*T),(_2+3*T),(_2+3*T),(_2+3*T),(3*T),(3*T),((0<.J-4)+3*T),(_4+3*T),(_4+3*T)
NB. dy=3
tpne3 =. tpne2
NB. dy>3
tpne4 =. tpne2
tpne =. tpne_4,tpne_3,tpne_2,tpne_1,tpne0,tpne1,tpne2,tpne3,:tpne4
NB. sourcedir=n, targetdir=w.  Switch the ew directions
tpnw =. |."1 tpne
tpn =. tpnn,tpns,tpnw,:tpne
NB. sourcedir=s.  target nswe behaves like snwe for sourcedir=n, but with the ns signs reversed
tps =. 'tpsn tpss tpsw tpse' =. |."2 tpns,tpnn,tpnw,:tpne
NB. sourcedir=w.  target nswe behaves like wens for sourcedir=n.  The ordering of dy,dx must be reversed
tpw =. 0 2 1 |: tpnw,tpne,tpnn,:tpns
NB. sourcedir=e.  target nswe behaves like ewsn for sourcedir=s.  The ordering of dy,dx must be reversed
tpe =. 0 2 1 |: (tpsw),(tpse),tpsn,:tpss
minposspenalty =: tpn,tps,tpw,:tpe
NB. Create penalty, accessed by targetdir,sourcedir,ydist,xdist, ordered by sourceyx
NB. Since minposspenalty is indexed by target-source, we must mirror it in both axes
NB. reorder to dir,y,x
penaltybytarget =: RGRIDDIST * (1 0 2 3) |: |."2 |."1 minposspenalty  NB. Remap from sourcedir,targetdir,ydist,xdist

NB. x is target dir,ypos,xpos
NB. y is table of dir,ypos,xpos,dist/move (ignored)
NB. Result is min distance to each point: the distance that can be achieved if there
NB. are no penalties incurred except for turns.  This is shifted up into the dist
NB. position of dist/move
minroutingdist =: 4 : 0
NB. Get distance, target-source
stdist =. x -"1&:(1 2&{"1) y
NB. Classify distance into <_1,_1,0,1,>1
NB. Lookup turn penalty, add to Manhattan distance.  Turn penalty is looked up as
NB. source dir, target dir, dy class, dx class
RGRIDDIST * (+/"1 | stdist) + ((0{"1 y),.(0{x),.(_4 _3 _2 _1 0 1 2 3 I. stdist)) (<"1@[ { ]) minposspenalty
)

NB. Audit the tables for consistency
3 : 0^:DEBROUTETABLES ''
NB. Make sure that the routing distance for a given position equals the minimum of the
NB. distances for each possible move from the position
NB. The goal is the origin, and we examine a 6x6 region around the origin, for each direction
startpts =. 0 0 -.~ ,/ ,"0/~ 0 1 _1 2 _2 3 _3 4 _4 5 _5 6 _6
for_s. ,/ startpts ,"1/ ,/ ,"0/~ i. 3 do.
  'y x sd td' =. s
  newpos =. (sd,y,x,0) +"1 (_1 _1 _1,-RGRIDDIST) bwand"1 (sd) { movesbydir
  newdist =. (3 {"1 newpos) + (td,0 0) minroutingdist newpos
  if. (olddist =. (td,0 0) minroutingdist (,:sd,y,x,0)) ~: <./ newdist do.
    'Routing table error: yx=(%d,%d), sourcedir=%d, targetdir=%d; premove dist=%d, postmove:' PR y;x;sd;td;olddist
    for_n. newpos,.newdist do.
      'yx=(%d,%d), dir=%d, indist=%d, mindist=%d' PR n
    end.
    break.
  end.
end.
NB. Make sure that each forward-move creates the move-code that leads back to its cell
if. *./ , fwdback =. ( 3&{. -:"1 backmovesbydir ((3 {. ]) + ({~ <@(0&{ , (<:RGRIDDIST) bwand {:)))"_ 1 movesbydir&(] +"1 ({~ 0&{)) )"1 ] 0 0 ,"1~ (i.4),.0 do.
  smoutput 'Routing tables are consistent'
else.
  smoutput 'Error in fwd/back check:'
  smoutput fwdback
end.
''
)

NB. 4x2x4x2 indexed by (target dir,penalties for low/high detours,source dir,low/high side of target)
NB. 
NB. The table as shown here is created as (target dir,low/high side of target,source dir,penalties for low/high detours)
NB.   and transposed below
NB. Consider when the target is North, and we are working on cells that are West of a blockage.
NB. A run going North will have to take 4 turns - 4 + 2 * (the amount of detour-1) if it continues North,
NB.  or 4 turns - 4 + 2 * (the amount of detour-1) if it switches to South (where the North detour is
NB.  limited to the amount above the target, and the South to the amount below the target)
NB. A run going South will have to take 6 turns - 6 + 2 * (detour-1) if it turns North,
NB.  or 2 turns - 2 + 2 * (detour-1) if it continues South.
NB. A run going West will take 5 turns - 2 + 2 * (detour-2) if it goes North,
NB.  or 3 turns  + 2 * (detour-2) if it turns South.
NB. A run going East will take 5 turns - 4 + 2 * (detour-2) if it goes North,
NB.  or 3 turns - 2 + 2 * (detour-2) if it turns South.
detpennl =. _2 ]\ (_6+4*T),(_6+4*T), (_8+6*T),(_4+2*T), (_6+5*T),(_4+3*T), (_8+5*T),(_6+3*T)
NB. The high side switches we
detpennh =. 0 1 3 2 { detpennl
detpenn =. detpennl ,: detpennh
NB. If target is South, interchange ns & lh penalties from the North data
detpens =. (<a:;1 0 2 3;1 0) { detpenn
NB. Now consider targets pointing West, from the low side
NB. A run going North will take 3 turns - 2 + 2 * (detour-1) if it stays North, or 5 turns - 4 + 2 * (detour-1) if it turns South
NB. A run going South will take 5 turns - 4 + 2 * (detour-1) if it goes North, or 3 turns - 4 + 2 * (detour-1) if it stays South
NB. A run going West will take 4 turns + 2 * (detour-2) either way
NB. A run going East will take 4 turns - 2 + 2 * (detour-2)  either way
detpenwl =. _2 ]\ (_4+3*T),(_6+5*T), (_6+5*T),(_6+3*T), (_4+4*T),(_4+4*T), (_6+4*T),(_6+4*T)
NB. From the high side, the ns runs looks the same, but a West run takes 4 turns - 4, an East one 4 turns - 2
detpenwh =. _2 ]\ (_4+3*T),(_6+5*T), (_6+5*T),(_6+3*T), (_6+4*T),(_6+4*T), (_4+4*T),(_4+4*T)
detpenw =. detpenwl ,: detpenwh
NB. If target is East, interchange we & lh penalties from the West data
detpene =. (<a:;1 0 2 3;1 0) { detpenw
NB. Combine to form the horizontal penalties - applied as a result of vertical blockages
NB. Reorder to 4x2x2x4 shape
detourpenaltiesh =: 0 3 2 1 |: RGRIDDIST * detpenn , detpens , detpenw ,: detpene

NB. The vertical penalties are applied as a result of horizontal blockages
NB. They interchange nw & es, in both target & source directions, but keep low/high unchanged
detourpenaltiesv =: (<2 3 0 1;a:;2 3 0 1) { detourpenaltiesh

NB. Route run using astar algorithm
NB. y is target y,x,dir
NB.  frontier is table of ypos,xpos,direction,movecode
NB.  target is a single ypos,xpos,direction
NB. Globals: grids, routingzero
NB.  routingzero is the value to use for initial zero distance.  All results from previous
NB.   routing are guaranteed to be RGRIDWINDOW higher than routingzero, so as not to interfere with
NB.   this route (occupied cells are less than 0)
NB. Result is input frontier, new frontier, each ypos,xpos,direction,movecode
NB. The global 'routinggrid' holds the routing info
routerun =: 4 : 0
'penaltyrollup penaltyrollback' =. x
'target origfrontier' =. y
QP^:DEBROUTE'router input:target=?target '
QP^:DEBROUTE'origfrontier '
frontier =. 3 {."1 origfrontier
NB. Precalculate min distance-to-target for each point in the routing grid
NB. Will be addressed by sourcey,sourcex,sourcedir
selkernel =. (0 { target) { penaltybytarget
NB. Create a grid, with shape like routinggrid, with the kernel centered on the target position.
NB. The first/last rows/columns are duplicated to get the target point in the proper position
'rgh rgw' =. rgshape =. _2 {. $ routinggrid
'tgdir tgy tgx' =. target
minpengrid =. selkernel {~"2 <rgshape {.!._1&.> (tgy,tgx) (i.@] {.~ -@(+ -:@>:))&.> _2 {. $ selkernel
NB. Calculate the detours required around blocks that cover the target.  We only worry about blocks
NB. that overlap the target, because the route tends to get stuck aiming for a position that is blocked off the target
NB. Work on blocks that block east-west
NB. We could include cells block by earlier routes here, if we want to
NB. Starting at the target row, scan up & down: first clear cells following a gap;
NB. then total to leave the number of detours for each cell.  This gives two sections, one above (& including) the
NB. target, one including & below.  Expand each of these to the full grid size by repeating the row for the
NB. target
NB. We take advantage of the fact that we have cleared occupancy in the target cell, in the target direction.
NB. This will guarantee that we do not detect detours in the target cell
blockages =. 0 > tgdir { routinggrid
detour =. (+/\@(*./\.)@((>:tgy)&{.) blockages) (([ ,: ((*"1 >&0)~ {.)) (,"2  }:"2) ([ ,:~ ((*"1 >&0)~ {:))~) +/\.@(*./\)@(tgy&}.) blockages
NB. Propagate that value left-to-right away from the target, to get #detours for west-east runs
NB. This is a brick with two layers: (# detours going North, # going South)
detour =. (>./\."1 tgx {."1 detour) ,"1 (>./\"1 tgx }."1 detour)
NB. Combine them with the turn penalties for each direction and take the lower value, to give
NB. the total # of turn-penalties at each position
NB. (Note: this does not take into account a possible jog-move at the end of the block.  Probably not
NB. material, since such a move would incur neighbor penalties; but if we want to handle it, we would reduce
NB. the penalty for cells that have a detour count of 1, in the direction of the target only)
NB. There are different rules for left-of-target and right-of-target, and for each
NB. source direction.  Fetch the combining rules, one for left and one for right, for each
NB. source direction (which is how the result table will be indexed).
NB. Expand rules to one value for left and one for right.  It doesn't matter what happens
NB. at the exact target x, because the target cell is empty and therefore has no blockages in its column
NB. Each detour will count twice; but if 0, use a low-value to remove all penalty
NB. Note: some of the penalties were calculated in the table as calling for detour-2; for those we should add back
NB.  2 detours if detour is actually 1; but it's not worth doing, and anyway no crime to underestimate the possible distance.
NB.  It tends to self-correct anyway, as such a run would turn immediately, to a position that has the correct value
ewdetourpenalty =. <./ ((<tgdir;a:;a:;rgw {.!.1 tgx # 0) { detourpenaltiesh) +"1"1 2 (2 * RGRIDDIST) * (-   500000 * 0&=) detour 
NB. Repeat for detours for north-south runs.
detour =. (+/\"1@:(*./\."1)@((>:tgx)&{."1) blockages) (([ ,: ((* >&0)~ {."1)) (,"1 }:"1) ([ ,:~ ((* >&0)~ {:"1))~) +/\."1@(*./\"1)@(tgx&(}."1)) blockages
detour =. (>./\."2 tgy {."2 detour) ,"2 (>./\"2 tgy }."2 detour)
nsdetourpenalty =. <./ ((<tgdir;a:;a:;rgh {.!.1 tgy # 0) { detourpenaltiesv) +"1 2 (2 * RGRIDDIST) * (-   500000 * 0&=) detour 

NB. The number of detours is the largest of the kernel and the ns and we detours
minpengrid =. minpengrid >. ewdetourpenalty >. nsdetourpenalty
NB. Add in the Manhattan distance from each gridpoint to the target
minpengrid =. minpengrid +"2 +/&:(RGRIDDIST * |)&((- i.)/)/ target ,.&(1 2&{) $routinggrid

QP^:DEBROUTE'(<a:;a:;0){drg ' [ drg =. '*ST ' {~ (_1,(routingzero + RMOVEEOC),(routingzero + RGRIDWINDOW)) i. routinggrid
NB. Calculate minimum distance-to-target to frontier points.  Get minimum in tgtprox, which shows the distance-to-target
NB. (0-origin, unbiased by routingzero)
tgtprox =. <./ fmindist =. frontier (<"1@[ { ])  minpengrid
NB. Append 0 distance-to-point to each frontier point.
frontier =. frontier ,. routingzero

NB. Bias fmindist by routingzero, where it will stay
fmindist =. routingzero + fmindist

NB. Remove frontier points that are far away from the best
NB. Leave 10 points on the shelf, if there are that many points
NB. Split frontier into active and shelved based on best distance
frontmsk =. fmindist <: cutoffdist =. routingzero + RFRONTIERRANGE + (<: RGRIDDIST) bwor tgtprox
if. INITSHELFCT < #fmindist do.
  shelfmsk =. fmindist <: INITSHELFFRINGE + INITSHELFCT ({ /:~) fmindist
else.
  shelfmsk =. 1   NB. If too few points, keep them all
end.
NB. Initialize board to 0 for frontier
NB.   with special 'move type' to signal end-of-chain for backtracking
NB. All the other endpoints, including the target itself, were marked as unreached when we started the
NB. route, and we cannot have an interest in any that we have previously routed to
routinggrid =: (routingzero + RMOVEEOC) (<"1 (3) {."1 shelfmsk # frontier)} routinggrid
shelfmsk =. shelfmsk > frontmsk  NB. Don't shelve active points
shelffrontier =. shelfmsk # frontier
shelfmindist =. shelfmsk # fmindist
frontier =. frontmsk # frontier
NB. Init indicator of shelf points that have been activated already.  To save movement we
NB. don't delete shelf points as soon as they are activated.  But any point with shelfmindist <:
NB. shelfcutoffdist has been activated and should not be reactivated
shelfcutoffdist =. cutoffdist
shelfdeadct =. 0  NB. Count of points on shelf with mindist <: shelfcutoffdist
NB. Init distance-to-target to 'not found'
bestfinaldist =. _1
NB. Loop till both frontier portions are empty
while. do.
  if. cutoffdist >: routingzero + RGRIDWINDOW do. failmsg 'route exceeds length limit' return. end. 
  NB. Calculate each possible move along the active frontier.  Remove movetype bits first
  frontier =. ((-RGRIDDIST) bwand 3 {"1 frontier) (<a:;3)} frontier
  frontier =. ,/ frontier +"1 (0 {"1 frontier) { movesbydir

  NB. Apply any penalties associated with the move; keep coded distance/move type where move type
  NB.  is in the low bits of the code
  frontier =. ((3 {"1 frontier) + (3 {."1 frontier) (<"1@[ { ]) penaltygrid) (<a:;3)} frontier

  NB. Remove moves that are not improvements.  When we are exploring ahead, we spawn lots of
  NB. moves simultaneously, and many of them are not improvements.  Before we check for duplicates,
  NB. we discard what we can, since duplicate testing is expensive

    NB. If there are duplicates on the active frontier, keep only the shortest distance
    frontier =. (((<0;0 1 2)&{ , <./@:({:"1))/.~   3&{."1) frontier
  if. #frontier =. frontier #~ (3 {"1 frontier) <&((-RGRIDDIST)&bwand) (3 {."1 frontier) (<"1@[ { ]) routinggrid do.

    NB. Fetch current distance to each point in the active frontier, and replace with frontier
    NB. distance if it is shorter.  Remove points from frontier that could not move
    frontbx =. <"1 frontx =. 3 {."1 frontier
    routinggrid =: ({:"1 frontier) frontbx} routinggrid

    NB. Calculate minimum distance-to-target in the active frontier; see if we hit target
    tgtprox =. <./ fmindist =. frontx (<"1@[ { ]) minpengrid
    NB. Add the distance to date to the min distance to target to get the min total distance
    NB. to target for each point.  From now on fmindist is the min final distance for frontier
    fmindist =. fmindist + 3 {"1 frontier
    NB. To reduce # iterations, extend every frontier element by straight moves to the end of the grid
    NB. This is expensive for large grids, so do it only occasionally
    if. (*#penaltyrollup) *. (tgtprox > TGTPROXFORROLLUP) do.
      NB. Limit the straight moves to positions that are best-possible moves.  It's not smart to
      NB. extrapolate a move that can't be a winner, because it will just immediately be shelved.  If we
      NB. wait, we will get a clear winner to extrapolate.  Rather than calculate the best-possible dist,
      NB. we use the value derived from the cutoff before this move.  This means that steps that were
      NB. penalized will not be extrapolated, which is a good thing
      NB. cutoffdist has low 3 bits set to 111 & so will be > any equal frontier value
      if. #topfrontier =. ((cutoffdist - RFRONTIERRANGE) > fmindist) # frontier do.
        NB. For speed, we want to extrapolate as little as possible.  So we figure a start,end for each frontier point,
        NB. stopping shortly after the target point if we are going toward it, or limited to a max distance if away;
        NB. and combine them to get a
        NB. rectangular region for propagation.  That region needs to be as small as possible
        topdirns =. nswetons topdir =. 0 {"1 topfrontier
        topstart =. 1 2 {"1 topfrontier
        NB. The constant is EXTRAPOVERSHOOT
        NB. We avoid }"0 1 which lacks IRS
        NB. Point is OK if (going up) matches (end > start)
        NB. Get coordinate of moving point for each extrap
        floatpos =. topdirns {"0 1 topstart
        NB. up=1 if floating coord is going in the positive direction
        up =. topdir e. 1 3
        NB. abovetgt=1 if floating coord is above the target (taking turn time into account)
        abovetgt =. (floatpos + 1 * up) > floattgt =. topdirns { 1 2 { target
        NB. If an up extrap is above the target, go for 100 units from floatpos; otherwise to target.  Here select the base
        usefloatpos =. up = abovetgt
        floatpos =. usefloatpos} floattgt ,: floatpos
        NB. Select offset, and add to base
        floatpos =. floatpos + (up ,. abovetgt) (<"1@[ { ]) _100 _5,:5 100
        NB. Get coordinates to consider, in y and x
        blocky =. ({."1 topstart) , (-. topdirns) # floatpos
        blockx =. ({:"1 topstart) , (topdirns) # floatpos
        NB. Get the extent (start,end+1) of the coordinates mentioned.  Clamp to within grid limits.
        extblock =. 0 4 ,. -~/\ |: 0 >. (_2 {. $ routinggrid) <. ((<./ , >:@:(>./)) blocky) ,: ((<./ , >:@:(>./)) blockx)
        NB. Back penalties back to beginning-of-line to put all points at equal level; then scan to
        NB. find the smallest overhead; then restore penalties.  The penalty blocks are designed to reset the
        NB. scan when a filled block is encountered.
        NB. We do not replace the movetype in the propagated value.  This may leave a higher-than-needed movetype,
        NB. but that's not a problem.  We do make sure below that if we change value, we force the movetype to 'straight'
        rolledgrid =. (extblock ];.0 penaltyrollup) + ('<./\.','<./\','<./\."1',:'<./\"1')&((128!:2)"1 2) (extgrid =. extblock ];.0 routinggrid) - extblock ];.0 penaltyrollback
        NB. Find the indexes of the cells that have an improvement.  The values in the window may be uninitialized from previous
        NB. iterations & would show 'improvement' to a value out of range.  So we requires that the improvement be to a value
        NB. inside the current gridwindow.
        if. #newfx =. ($ #: I.@,) rolledgrid < extgrid <. routingzero + RGRIDWINDOW do.
          NB. Fetch the distance-to-point for the improved points; convert them to move type 0 (=straight ahead)
          frontier =. frontier , (newfx =. newfx +"1 {.extblock) ,. impval =. (-RGRIDDIST) bwand newfx (<"1@[ { ]) rolledgrid
          NB. Store the new values
          routinggrid =: impval (<"1 newfx)} routinggrid
          NB. Restore tgtprox/fmindist for the new points
          tgtprox =. <./ fmindist =. (3 {."1 frontier) (<"1@[ { ]) minpengrid
          fmindist =. fmindist + 3 {"1 frontier
        end.
      end.
    end.

    NB. If We hit the target, cull any point from shelved frontier that cannot beat the distance that hit
    if. tgtprox = 0 do.
      NB. The only way to have dist-to-target=0 is to hit it.  We just did that.
      NB. Purge shelved points that are now dead, to save carrying them around
      NB. (active points are purged below on every step).
      NB. Use dist in frontier, but discard the move-type part
      NB. We must also purge dead shelf points so that our count of them is accurate
      bestfinaldist =. (-RGRIDDIST) bwand (<target) { routinggrid
      shelfmsk =. (shelfmindist < bestfinaldist) *. (shelfmindist > shelfcutoffdist)
      shelffrontier =. shelfmsk # shelffrontier
      shelfmindist =. shelfmsk # shelfmindist
      NB. Mark that we have outed all points below cutoffdist
      shelfdeadct =. 0
    end.

    NB. Delete any points from active frontier that can't beat the best hit distance
    if. bestfinaldist > 0 do.
      frontactx =. fmindist I.@:< bestfinaldist
      frontier =. frontactx { frontier
      fmindist =. frontactx { fmindist
    end.
  else. fmindist =. 0$0  NB. If frontier empty, can't modify routinggrid
  end.

  NB. Find the new best minimum distance: minimum of active and shelved frontier,
  NB. with leeway to broaden the frontier
  if. 0 = #alldists =. fmindist , shelfcutoffdist (< # ]) shelfmindist do. break. end.
  cutoffdist =. RFRONTIERRANGE + (<: RGRIDDIST) bwor <./ alldists
  if. cutoffdist > shelfcutoffdist do.
    NB. Activate shelved points if there are any to come to life.  But don't activate shelf points
    NB. that have been activated before.  Activate if dist <: cutoffdist, but not if
    NB. dist also <: shelfcutoffdist
    shelfactpts =. shelffrontier #~ </"1 shelfmindist <:/ shelfcutoffdist,cutoffdist
  else.
    NB. If we are still going as fast as we thought we were last step, there can be nothing on the shelf to activate
    shelfactpts =. 0 4$0
    NB. It is possible that the cutoffdist goes down, if our calculation of the best possible distance for the last step
    NB. was too high.  This is an error, because it means we might cutoff the best path; but we allow it sometimes,
    NB. especially a jog around a detour, because we don't want to take the time to get the distance right and we don't
    NB. think the route we are excluding would actually be the winning route.  In that case, we keep cutoffdist from
    NB. going down (which would make our bookkeeping of the shelf wrong); in effect we widen the fringe temporarily
    cutoffdist =. shelfcutoffdist
  end.
  NB. Shelve active points that have fallen off the pace
  frontactmsk =. fmindist <: cutoffdist
  shelffrontier =. shelffrontier , (-. frontactmsk) # frontier
  shelfmindist =. shelfmindist , (-. frontactmsk) # fmindist
  frontier =. (frontactmsk # frontier) , shelfactpts
  NB. Mark that we have outed all points below or equal cutoffdist
  shelfcutoffdist =. cutoffdist
  NB. Keep track of number of shelved points activated.  They are dead weight on the shelf, so we
  NB. garbage-collect them when they exceed 50% of the shelf
  if. (shelfdeadct =. shelfdeadct + #shelfactpts) > 0.5 * #shelffrontier do.
    NB. Find dead points - those with len <: cutoffdist
    livemsk =. shelfmindist > cutoffdist
    shelfmindist =. livemsk # shelfmindist
    shelffrontier =. livemsk # shelffrontier
    shelfdeadct =. 0
  end.
end.
if. bestfinaldist < 0 do. failmsg 'route failed' return. end. 
NB. End loop.  Target has been hit

NB. Backtrack from the target to create the wiring, using the move type to indicate what to emit
NB. The last result, the end-of-route, will be repeated; remove it
backchainpos =. (+    backmovesbydir (<"1@[ { ])~ 0&{ , (<: RGRIDDIST) bwand (<"1@[ { ])&routinggrid)^:a: target
NB. Append the move type to each position
backchain =. backchainpos ,. (<: RGRIDDIST) bwand backchainpos (<"1@[ { ]) routinggrid

NB. Decrement routingzero, which clears all cells to 'unoccupied'
routingzero =: routingzero - RGRIDWINDOW

NB. Append the new frontier to the one we had coming in, and return it
origfrontier,backchain
)

NB. *************** end of router - start of display-object management **************
RGBTOLUMINANCE =: +/@:*"1&0.2989 0.5870 0.1140


SCROLLBARWIDTH =: 0.01  NB. width of scrollbar as fraction of min(screen height,screen width)
SCROLLBARMINWIDTH =: 0.005  NB. Minimum thickness of traveler as fraction of min(screen height,screen width)
SCROLLBARENDTHICKNESS =: SCROLLBARWIDTH  NB. width of scrollbar endcap as fraction of min(screen height,screen width)
SCROLLBARCOLOR =: <192 192 192   NB. color for scrollbar - no pen
SCROLLBARENDCOLOR =: <240 240 240
SCROLLBARTRAVELERCOLOR =: <128 128 128
SCROLLBARCORNERCOLOR =: <64 64 64

NB. FONTNUM - Font for 'data' - numeric data, shape, rank, etc
NB. FONTCHAR - Font for 'text' - verb names, noun names, status messages
NB. FONTIMSG - Font for easy readability - tooltips, error messages
'FONTNUM FONTCHAR FONTIMSG FONTTTIP' =: i. 4

NB. y is string, result is string with characters translated for display.  We do this
NB. to make the J boxing characters visible
dataxlate =: ((1 22 2 25 16 23 3 21 4 5 6 { a.) (16+i. 11)} a.) {~ a. i. ]

NB. for the verb-name cell
VERBCOLOR =: 114 30 30
VERBTEXTCOLOR =: 255 255 255
VERBCOLOREXPANDABLE =: VERBCOLOR
VERBTEXTCOLOREXPANDABLE =: VERBTEXTCOLOR
VERBCOLOREXPANDED =: 160 30 60
VERBTEXTCOLOREXPANDED =: 240 255 240
VERBFONT =: FONTCHAR
VERBFONTSIZE =: 0
VERBMARGIN =: 1
VERBMARGINEXPANDABLE =: 7   NB. Includes half of the pensize
VERBPENEXPANDABLE =: 224 255 255,4,PS_DOT
NB. for noun body, top level
NOUNCOLOR =: 200 200 255
NOUNTEXTCOLOR =: 0 0 0
NOUNFONT =: FONTCHAR
NOUNFONTSIZE =: 0
NOUNMARGIN =: 1
NB. for status messages
STATUSCOLOR =: 255 0 0
STATUSTEXTCOLOR =: 255 255 255
STATUSFONT =: FONTIMSG
STATUSFONTSIZE =: 2
STATUSMARGIN =: 1
NB. for recoverable status messages
ISTATUSCOLOR =: 255 255 255
ISTATUSTEXTCOLOR =: 255 128 128
ISTATUSFONT =: FONTIMSG
ISTATUSFONTSIZE =: 2
ISTATUSMARGIN =: 1
NB. for the user's sentence
SATZCOLOR =: 248 248 248
SATZTEXTCOLOR =: 0 0 0
SATZTEXTDISABCOLOR =: 192 192 192
SATZFONT =: FONTNUM
SATZFONTSIZE =: 0
SATZMARGIN =: 1
NB. for the shape, top level
SHAPEFONT =: FONTNUM
SHAPEFONTSIZE =: 0
SHAPEMARGIN =: 2 2 $ 1 1 2 1
NB. for data, except top level
DATAFONT =: FONTNUM
DATAFONTSIZE =: 0
DATAMARGIN =: 2 2 $ 1 1 2 1
NB. for tooltips
TOOLTIPCOLOR =: 255 255 0
TOOLTIPTEXTCOLOR =: 0 0 0
TOOLTIPMARGIN =: 1
NB. for title
NB. for the user's sentence
TITLCOLOR =: 240 240 240
TITLTEXTCOLOR =: 0 0 0
TITLFONT =: FONTIMSG
TITLFONTSIZE =: 2
TITLMARGIN =: 0
NB. for links
LINKCOLOR =: 240 240 240
LINKTEXTCOLOR =: 0 0 192
LINKFONT =: FONTIMSG
LINKFONTSIZE =: _4
LINKMARGIN =: 0
NB. for assignment
ASSIGNCOLOR =: 96 96 192
ASSIGNCOLORERROR =: 224 0 0
ASSIGNTEXTCOLOR =: 255 255 255
ASSIGNFONT =: FONTCHAR
ASSIGNFONTSIZE =: 0
ASSIGNMARGIN =: 1


NB. The shape colors/textcolors give the main data colors for the selection level
SHAPECOLORS =: ".;._2 (0 : 0)
255 255 255
000 255 000
255 000 255
000 255 255
255 000 000
128 255 000
)
SHAPETEXTCOLORS =: 0 0 0"1 SHAPECOLORS
NB. for the shape display of the (filled) result cell
RESULTSHAPECOLOR =: 100 50 200
RESULTSHAPETEXTCOLOR =: 255 255 255
RESULTSHAPEFONT =: FONTNUM
RESULTSHAPEFONTSIZE =: 0
RESULTSHAPEMARGIN =: 1

NB. Data colors are like the shape colors, but there is a checkerboard effect, so each
NB. odd color has the background dimmed a little.  The first (unselected) data color
NB. is different from the shape color, to keep them separate
DATACOLORS =: (255 255 255 (0}) SHAPECOLORS)

NB. Now spread out the data colors, providing the checkerboard
NB. Make the dim one come first, so an empty cell displays a visible rectangle (which lets us pick it)
NB. This is an nx2x3 table
DATACOLORS =: <. DATACOLORS  *"1/ 1 1 1 ,: 0.75 0.75 0.75

NB. The text colors are selected from the data type: 0=normal, 1=string
DATATEXTCOLORS =: _3 ]\ 0 0 0   0 96 0

NB. The colors for each level of highlighting.  The first highlight contrasts with normal
NB. data; thereafter we rely on the vivid colors to contrast, and we match each highlight with
NB. its shape selector; but we reduce the intensity to a max value to ensure contrast with the
NB. background.
HIGHLIGHTCOLORS =:  <. (*    1 <. 110 % RGBTOLUMINANCE) (0 0 0 (0}) SHAPECOLORS)

NB. Colors to use for empty, including a checkerboard
EMPTYCOLORS =: <. (120 120 120)  *"1/ 1 1 1 ,: 0 0 0

FRINGECOLOR =: _3 <\ 128 128 128    200 200 0    255 128 128   255 255 255   0 0 0    NB. color(RGB) of fringes: in order label,shape,status,data,assignment
FRINGEBORDER =: (<0 0 0 1)"0 FRINGECOLOR   NB. border of fringes: RGB,width

DOBORDERCOLORS =: _3 ]\ 0 0 255 0 0 255  0 0 0  255 0 0   NB. Black border for box, but red if incomplete, blue if empty
RANKCOLOR =: 0 0 255  NB. color of dashed line for high-rank ops
BOXBORDERCOLOR =: 0 0 0  NB. color of lines between boxes


NB. Highlight must specify pen width and style
HIGHLIGHTLINEWIDTH =: 2
HIGHLIGHTBORDERSTYLE =: 0 0 0,HIGHLIGHTLINEWIDTH,PS_DOT  NB. color,width,style of lines for highlight in operands (dashed)
SELECTIONBORDERSTYLE =: 0 0 0,HIGHLIGHTLINEWIDTH,PS_SOLID  NB. color,width of lines for highlight in the clicked result (solid)


WIRECOLOR =: 0 0 0   NB. Color of wires
WIREHIGHCOLOR =: 255 100 255  NB. Highlight color for nets, blocks, & sentence words
SENTENCEHIGHRECTHIGHWIDTH =: 3
SENTENCEHIGHRECTPEN =: WIREHIGHCOLOR , SENTENCEHIGHRECTHIGHWIDTH 

BOXMARGIN =: 2 ($,) 3   NB. Space to leave around boxed results
BOXLINEWIDTH =: 2 ($,) 1  NB. Width of lines making boxes

EMPTYEXTENT =: 15 10   NB. Size to use for displaying empty

MINFONTSIZE =: 7   NB. As small as we can readably display

COLORSFORCLASSSHAPE =: (SHAPETEXTCOLORS ;"1 SHAPECOLORS) , RESULTSHAPETEXTCOLOR;RESULTSHAPECOLOR

NB. Create cfms for later use.  y is table of font;size
NB. called in locale of main instance, leaves names defined there
NB. Operands to drawtext/sizetext
NB. These take colors from the selectors, except for the first one, which tells part of speech, and the last
NB. few, which are status/unexecd/fill
calccfms =: 3 : 0
modfontsize =. ] ({.@] , [ (MINFONTSIZE >. +)&.> {:@]) y {~ [
nouncfm =: < NOUNCOLOR;NOUNTEXTCOLOR;(NOUNFONT modfontsize NOUNFONTSIZE),<NOUNMARGIN
nouncfm =: nouncfm , < (SHAPECOLORS ;"1 SHAPETEXTCOLORS) ,"1 (SHAPEFONT modfontsize SHAPEFONTSIZE),<SHAPEMARGIN
nouncfm =: nouncfm , < STATUSCOLOR;STATUSTEXTCOLOR;(STATUSFONT modfontsize STATUSFONTSIZE),<STATUSMARGIN
nouncfm =: nouncfm , < ISTATUSCOLOR;ISTATUSTEXTCOLOR;(ISTATUSFONT modfontsize ISTATUSFONTSIZE),<ISTATUSMARGIN

verbcfm =: < VERBCOLOR;VERBTEXTCOLOR;(VERBFONT modfontsize VERBFONTSIZE),<VERBMARGIN
verbcfm =: verbcfm , < (SHAPECOLORS ;"1 SHAPETEXTCOLORS) ,"1 (SHAPEFONT modfontsize SHAPEFONTSIZE),<SHAPEMARGIN
verbcfm =: verbcfm , < STATUSCOLOR;STATUSTEXTCOLOR;(STATUSFONT modfontsize STATUSFONTSIZE),<STATUSMARGIN
verbcfm =: verbcfm , < ISTATUSCOLOR;ISTATUSTEXTCOLOR;(ISTATUSFONT modfontsize ISTATUSFONTSIZE),<ISTATUSMARGIN

cfmdata =: ,/ ,/ (DATACOLORS ;"1 (0 0 0)) ,"1"2 1"2 (({. ,. (('';' bold italic') (,~ ":)&.> {:)) DATAFONT modfontsize DATAFONTSIZE) ,. <DATAMARGIN

RESULTSHAPECFM =: RESULTSHAPECOLOR;RESULTSHAPETEXTCOLOR;(RESULTSHAPEFONT modfontsize RESULTSHAPEFONTSIZE),<RESULTSHAPEMARGIN

NB. For the displayed sentence.  This is (n,2)$ indexed by (selection level),(disabled)
satzcfm =: ((SATZCOLOR (0}) SHAPECOLORS) ;"1 (SATZTEXTCOLOR"1 SHAPETEXTCOLORS)) ,"1 (SATZFONT modfontsize SATZFONTSIZE),<SATZMARGIN
satzcfm =: satzcfm ,:"1 ((SATZCOLOR (0}) SHAPECOLORS) ;"1 (SATZTEXTDISABCOLOR"1 SHAPETEXTCOLORS)) ,"1 (SATZFONT modfontsize SATZFONTSIZE),<SATZMARGIN


NB. For titles
titlcfm =: TITLCOLOR;TITLTEXTCOLOR;(TITLFONT modfontsize TITLFONTSIZE),<TITLMARGIN

NB. For links
linkcfm =: LINKCOLOR;LINKTEXTCOLOR;(LINKFONT modfontsize LINKFONTSIZE),<LINKMARGIN

NB. For assignments
assigncfm =: ASSIGNCOLOR;ASSIGNTEXTCOLOR;(ASSIGNFONT modfontsize ASSIGNFONTSIZE),<ASSIGNMARGIN

NB. Calculate for each class in the rank and selection
NB. First line is for lines before the last; next lines are for the last line, depending on expansionstate
NB. font pen margin tcolor bcolor size
fontsforclassrankverb =: (((<VERBFONT,0){y),'';VERBMARGIN;VERBTEXTCOLOR;VERBCOLOR) ,"1 0 <.&.-:@>:&.> ((<VERBFONT,1){::y) * 1 , (>:EXPANDED) # 1.6
fontsforclassrankverb =: (VERBPENEXPANDABLE;VERBMARGINEXPANDABLE;VERBTEXTCOLOREXPANDABLE;VERBCOLOREXPANDABLE) (<(>:EXPANDABLE);1 2 3 4)} fontsforclassrankverb
fontsforclassrankverb =: (VERBTEXTCOLOREXPANDED;VERBCOLOREXPANDED) (<(>:EXPANDED);3 4)} fontsforclassrankverb
fontsforclassrankshape =: (((<SHAPEFONT,0){y),.(<''),.SHAPEMARGIN ;"_ 1 COLORSFORCLASSSHAPE) ,"1 0 ((<SHAPEFONT,1){y)
TOOLTIPFONT =: FONTTTIP { y

NB. x is data type (0=normal 1=string etc.)
NB. y is fillmask codes
NB. result is the values to use for drawtext: the selected background color, with stippling added; the data color; the font etc.
NB. the stippling value is the checkerboard part of the fillmask.  It is appended to the selected background color
textinfofromtypefillmask =: (( ({:"1 ,"1 0~ (> {."1 cfmdata) {~ {."1)@] ;"1 (DATATEXTCOLORS {~ [)) ,"1 (2 }."1 cfmdata) {~ {."1@]) (0,FILLMASKCHECKER)&#:

NB.?lintsaveglobals
''
)

NB. For empty nouns, use a dark rectangle.  There is no text
emptycfm =: (FILLMASKSELLEVEL_dissectobj_%FILLMASKCHECKER_dissectobj_) $ EMPTYCOLORS ;"1 a: , a: , a: , <DATAMARGIN

FORCEDTOOLTIPMINVISTIME =: 0.4   NB. Minimum time a forced tooltip will be displayed

NB. Maximum length for a defstring that we put into the display stack
MAXSTACKDEFSTRINGLENGTH =: 50
NB. Maximum length for a defstring that we put into the final verb name for a compound
MAXFINALDEFSTRINGLENGTH =: 50

NB. Size and position of resize handle, relative to lower-right corner of data
RESIZEHANDLEXYWH =: 2 2 $ _2 _2 4 4
RESIZEHANDLECOLOR =: <0 0 0  NB. color of resize handle
MINRESIZABLE =: 50 80  NB. Don't put resize handles on anything smaller
RESIZERECTCOLOR =: <192 192 192 3  NB. Color of resizing rect, width
RESIZEMAXFRAC =: 0.9 0.9   NB. Maximum size to resize up to

NB. Calculate a font from a class and selection
NB. x is the class of the characters:
NB.  0=verb in rank stack
NB.  1=shape/selection (in rank stack or in shape/sel line)
NB. y is selection[,decoration]
NB. selection is a number that is meaningful to the class
NB. decoration is a choice from below
NB. Result is the font spec for use by sizetext/drawtext:
NB.   (background color[;pen color,width]);text color;text font;text size;yx margin around text (scalar or yx or 2 2 $ tlbr);(split LF-delimited texts into lines (kludge))
NB.   background color may be RGBA, where A is the stipple pattern: 0=none, 1=downleft, 2=downright, 3=both
'FONTCLASSRANKVERB FONTCLASSSHAPE' =: i. 2
FONTDECORATIONSTIPPLE =: 3
FONTDECORATIONITALIC =: 4
FONTDECORATIONBOLD =: 8
FONTDECORATIONSIZE =: 16b10   NB. 0=no adj, 4-bit signed field, each representing 10%
cfmforclass =: 4 : 0"0 1
'font pen margin tcolor bcolor size' =. cfmforclassrankverb`cfmforclassshape@.x {. y
if. 1 < #y do.
  'sizeadj bolditalic stipple' =. 16 4 4 #: 128 + 1 { y
  size =. <.&.-: 1 + size * 0.2 0.1 p. sizeadj
  size =. (": size) , bolditalic {:: '';' italic';' bold';' bold italic'
  if. stipple do. bcolor =. bcolor , stipple end.
  NB. kludge if field is italic, add 1 to right-hand margin.  Normal sizing seems to get it wrong.
  if. 1 bwand bolditalic do. margin =. (2 2 $ 0 0 _2 2) + 2 2 ($,) margin end.
end.
if. #pen do. bcolor =. bcolor;pen end.
bcolor;tcolor;font;size;margin
)

NB. The center column of the rank stack.
NB. Selection is 0 (normal) or 1 (for the verb at the bottom) 
cfmforclassrankverb =: 3 : 0
y { fontsforclassrankverb
)
NB. shape/selection, and shapes in the rank stack
NB. y is sellevel if positive, or negative for special types (_1 = result-cell)
cfmforclassshape =: 3 : 0
y (] {~ (<.    2 -~ #)) fontsforclassrankshape
)

NB. ************** drawing the sentence *****************

NB. Draw the user's top info: the sentence including highlighting,
NB. and any title and links
NB. x is the sentence, in the user's spacing
NB. y is a table of (token number(s));(selection level, visibility);(display locale)
NB. Result is table of (brect for header data);< other stuff needed to draw the sentence.  Ending brects are hw
NB. First row is title (possibly empty), second row is sentence, third (if any) is links
sizesentence =: 4 : 0
usentence =. x
NB. Reselect in case explorers were drawn
wd 'psel ' , winhwnd
glsel 'dissectisi'
NB. If there is a title, size it
if. #titlelines =. qoptb 'title' do.
  titlsizetext =. TAB (0&".@taketo ; takeafter)&> titlelines
  titlcfms =. (#titlsizetext) $ ,: titlcfm
  NB. Add in fontsize adjustment
  titlcfms =. ((0 {"1 titlsizetext) (MINFONTSIZE >. +)&.> 3 {"1 titlcfms) (<a:;3)} titlcfms
  NB. Get the size of the rectangles.
  titlrectsizes =. titlcfms sizetext ,. titltxts =. 1 {"1 titlsizetext
  NB. Get the starting y position for each title line
  titly =. |.!.0 +/\ {."1 titlrectsizes
  NB. Get max title width
  maxtitlewid =. >./ {:"1 titlrectsizes
  NB. Get the starting x position - so as to center the titles
  titlx =. <. -: maxtitlewid - {:"1 titlrectsizes
  titlrects =. (titly,.titlx) ,:"1 titlrectsizes
  titlinfo =. titlcfms;<titltxts
else.
  titlinfo =. (0$a:) ,&< (0$a:)
  titlrects =. 0 2 2$0
end.

NB. If there are links, size them too.  Create a table, empty if no links
if. #linklines =. qoptb 'link' do.
  linksizetextlink =.<;._2@:(,&TAB)@> linklines
  linkcfms =. (#linksizetextlink) $ ,: linkcfm
  NB. Add in fontsize adjustment
  linkcfms =. ((0&".@> 0 {"1 linksizetextlink) (MINFONTSIZE >. +)&.> 3 {"1 linkcfms) (<a:;3)} linkcfms
  NB. Get the size of the rectangles.
  linkrectsizes =. linkcfms sizetext ,. linktxts =. 1 {"1 linksizetextlink
  NB. Get the starting y position for each link line
  linky =. |.!.0 +/\ {."1 linkrectsizes
  NB. Links are left-justified, start at 0
  linkrects =. (linky,.0) ,:"1 linkrectsizes
  linkinfo =.  ,: (>./ +/"2 linkrects);< linkcfms;linktxts;linkrects;< 2 {"1 linksizetextlink
else.
  linkinfo =. 0 2$a:
end.



NB. Create table of token#;(level,visibility);locale.  Decrement token # to account for queue-end added at front.
NB. This also deletes any boxes of y that contain 0 token numbers - these will have been added for
NB. emulation purposes, for example vi@:u to handle &.
toklevloc =. /:~ ; <@(<:@,@>@{. ;"0 1 }.)"1 > y
assert. (-: i.@#) > {."1 toklevloc

tokens =. ;: usentence
NB. Get the length of each token (except the last) in the user's spacing
tokulen =. 2 -~/\ (' ' +/\@:~: usentence) I. (>: |.!.0 +/\ ' '&(+/@:~:)@> tokens)

NB. Looking at pairs of tokens, insert after each the number of blanks needed to match the
NB. user's spacing.  Give this string the proper color: the selection level if both are the same, or
NB. _1 if they differ.  Result is a table, with a pair of rows for each token, the first token;level for token and the next for the following space.
NB. Handle the last token, which is never followed by anything.
NB. Create token;level;visible;locale   for each token
toklevvisloc =. tokens ,. (<"0@(1&{::) , 2&{)"1 toklevloc
NB. Create blanks;level;visible;locale for each token, where level;locale comes from the token with higher sellevel
addedblanks =. tokulen (' ' #~ (- #))&.> }: tokens  NB. blanks added to end of each token
NB. Interleave tokens and blanks; then remove lines for empty (=no blanks) or invisible
utokspacelevvisloc =. (#~  (*@#@(0&{::) *. 2&{::)"1) (({."1 toklevvisloc) ,&.> addedblanks , a:) (<a:;0)} toklevvisloc
NB. Calculate cfms to use.
cfms =. satzcfm {~ <0 ;~ cfmsx =. (_2 + #satzcfm) <. > 1 {"1 utokspacelevvisloc
rectsize =. cfms sizetext ,. txts =. 0 {"1 utokspacelevvisloc
NB. Box them into sections that fit within the allowed part of the screen, one box per line
scrwid =. <. MAXSENTENCEWIDTH * {: screensize
boxhw =. , (((}.~) (, $:)~^:(*@#@[) <@{.~)   1 >. scrwid I.~ (+/\@:({:"1))) rectsize
NB. Get the height of each line
lh =. >./@:({."1)@> boxhw
NB. Install starting yx, and move rects into horizontal position
rects =. ; (0 ,.~ |.!.0 +/\ lh) (] ,:~"1 (+"1    (0) ,. [: |.!.0 +/\@:({:"1)))&.:>"1 0 boxhw
NB. Get the max size for the string, and return the data for drawing
NB. We return the index to the cfm, since we select the color later
rectinfo =. (>./ +/"2 rects);<cfmsx;txts;rects;< 3 {"1 utokspacelevvisloc
(((<. 0 >. >./ +/"2 titlrects);<titlinfo,<titlrects),:rectinfo),linkinfo
)

NB. Draw the sentence.  y is the first two lines of the result of sizesentence (the title/sentence info)
NB. x is the brect for the region
drawsentence =: 4 : 0"2 1
tlc =. x * 1 0
'cfms txts rects' =. 3 {. 1 {:: y
if. #cfms do.
  cfms drawtext txts ,. <"2 tlc +"2 rects
end.
''
)

NB. Draw the links.  y is the second line of the result of sizesentence (the link info)
NB. x is the brect for the region
drawlinks =: 4 : 0"2 1
tlc =. x * 1 0
'cfms txts rects links' =. 1 {:: y
cfms drawtext txts ,. <"2 tlc +"2 rects
)

NB. Get the extent of the region of the current window that is on the visible screen
NB. Whichever window is active is used, and it is assumed that dissectisi has been glsel'd in it
NB. Result is the xywh of the part of dissectisi that is on the physical screen
NB. We use the smaller of the childsize and glqwh to limit the size, since they seem to differ
NB. and it is fatal to read outside the glqwh size
findonscreenyxhw =: 3 : 0
NB. Get child xywh, convert to xyrb and clamp size to graphics size, add form position, convert to yxbr in screen space
NB. The x may be negative eg, if control is offscreen left
isiyxbr =. ((+ (glqwh'')&<.)/\ (2 2) $ 0 ". wdqchildxywh 'dissectisi') +"1&:(|."1) 2 {. ". wdqform ''
NB. Get offset into dissectisi of the top-left visible point
yxvisible =. 0 >. - {. isiyxbr
NB. Get br visible point of dissectisi in screen space, subtract screen tr of visible to get visible size
NB. Remove 100 pixels of height to account for taskbar (major kludge)
onscreensize =.  ((0 0 >. _36 0 + 3 2 { 0 ". wd 'qscreen') <. {: isiyxbr) - (0 >. {. isiyxbr)
NB. Return offset,:size
yxvisible ,: onscreensize
)

NB. Pick for display object, vectoring to the indicated type
NB. x is ('l' or 'r') flag
NB. y is main/explorer;(y,x relative to pickrect);formatted sysdata;index of pickrect
NB. no result.  selections are performed
NB. This version, which is defined in dissect locale, is used for pickrects that do not
NB. have a DO, but are tied to the display instance.  Those are always links.  They are
NB. processed in the locale of the display instance, but come here.
pickDO =: 4 : 0
debscroll^:DEBSCROLL 'pickDO';x;<y 
QP^:DEBPICK 'coname'''' y '
'exp yx sd px' =. y
NB. Only left-click in the top area is honored
if. 'l' = x do.
  select. px
  case. 1 do.
    NB. Click in the sentence
    'cfms txts rects locs' =. (px,1) {:: topinfo
    for_r. rects (_1 findpickhits) yx do.
      'ix pyx' =. r
      disploc =. ix { locs
      NB.?lintonly disploc =. <'dissectobj'
      if. disploc = <0 do.
        ('sentence';ix) drawttipwithemphasis yx ; 'The current selections do not produce a display for this word'
      elseif. #DOyx__disploc do.
        NB. Center the displayed block on the focus point of the screen
        NB. Get the position of center of block in the routing area
        blockcenter =. <. scrolltlc -~ DOyx__disploc (+ -:)&{. DOsize__disploc
        NB. Get the minimum amount above bottom-of-screen to position the focuspoint, to
        NB. allow full display of the selected block.  If the block is large, the bottom
        NB. 25% of the screen may not be big enough
        minfocusrise =. <. -: {. DOsize__disploc
        NB. Set scrollpoint to screen focuspoint - routing centerpoint
        NB. The focuspoint is in the bottom-middle of the onscreen part of the
        NB. screen area.  We have to use the parent/child info rather than
        NB. gl commands because gl doesn't know about screen placement
        focuspoint =. (+   <.@(- minfocusrise >. 0.25 0.5&*))/ findonscreenyxhw''
        scrolltlc =: focuspoint - blockcenter
        NB. redraw the scrolled screen.  No retraversal needed
        dissect_dissectisi_paint 0
      elseif. do.
        ('sentence';ix) drawttipwithemphasis yx ; 'The current selections do not produce a display for this word'
      end.
    end.
  case. 2 do.
    NB. Click in the links
    'cfms txts rects links' =. (px,1) {:: topinfo
    for_r. rects (_1 findpickhits) yx do.
      'ix pyx' =. r
      QP^:DEBPICK 'x ix pyx exp yx sd ix{::picknames '
      if. IFQT do.   NB.?lintonly browse_j_ =. 3 : 'y'
        browse_j_ ix {:: links
      else.
        drawttipwithemphasis yx ; 'Links not supported in J6'
        hoversessmin =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
      end.
    end.
  case. do.
    NB. Not in links or sentence - must be titles or empty area, ignore
  end.
end.
0 0$0
)

NB. y is a brick of rectangles yx,:hw, or an array of boxes containing rects at some level,
NB. or a single rect (which becomes the bbox)
NB. Result is bounding rect, a single yx,:hw
brect =: (<./@:({."2) ,: >./@:({:"2))&.:(+/\"2)^:(2<#@$)@(>@:(<S:0)^:(0<L.))


NB. *********** create DOs
cocurrent 'dissectobj'


NB. x is alignment(s) for a single rectangle
NB. y is hw
NB. result is boxed hw,:alignment (vert,horiz)
addalignmentrect =: <@(,:~)"1

NB. y is a table of boxed rects, or a list representing horizontal stacking
NB. x is (vert alignment mode;horiz alignment mode) - possibly boxed lists for each
NB. result is one box representing those aligned objects
addalignmentgroup =: [: < >^:(1<L.)@{:@[ ,. >^:(1<L.)@{.@[ ,  ,:^:(0>.2-#@$)@]


NB. manifests for alignment/formatting
'ALIGNCENTER ALIGNLEFT ALIGNRIGHT ALIGNSPREAD' =: i. 4
NB. verb used to calculate rectangle alignment
NB. x is start,size,len where start and size refer to the bounding box allocated to the rectangle, and len is the extent of the original rectangle
NB. y is the formatting selection: 0=center 1=ljust 2=rjust 3=swell
NB. Result is start,size of the rectangle coordinate
formatcoordtable =. ,:                 1 0.5 _0.5 ,: 0 0 1   NB. center
formatcoordtable =. formatcoordtable , 1 0 0 ,: 0 0 1   NB. ljust
formatcoordtable =. formatcoordtable , 1 1 _1 ,: 0 0 1   NB. rjust
formatcoordtable =. formatcoordtable , 1 0 0 ,: 0 1 0   NB. swell to fill bbox
formatcoord =: [: <. (+/@:*"1  {&formatcoordtable)"1 0

NB. Lay out rects recursively
NB. Each rect is assigned a bounding box, and the boxes are combined up the tree
NB. Then, rects are aligned down the tree, filling the combined boxes,
NB. according to the flags at each level.
NB. y, a rectobj,  is either an open rect description or a table of boxes, where the first row and column contain
NB.  alignment flags [, space before [, space after]]
NB. for that column/row: 0=center 1=ljust 2=rjust 3=spread
NB. The other cells of y contain rectobjs.
NB. The open rect description is a table containing yx followed by formatting selection: 0=center 1=ljust 2=rjust 3=swell
NB. The result has the same structure as the input, except that the alignment boxes and formatting info are
NB. removed, and all the rectangles are adjusted to be relative to the (0,0) origin of
NB. the entire object.
NB. The call to the monad must always have boxed y
alignrects =: 3 : 0
assert. 32 = 3!:0 y [ 'alignrects'
NB. Calculate tree of bounding boxes.  This has the format (hw) ; subtrees
NB. where yx,;hw is the smallest box that can hold the items, and subtrees is a
NB. table of bounding boxes, one for each box in the object (i. e. not including
NB. the alignment flags), with the bounding box for the boxed object (relative to the (0,0)
NB. of that object.  Rectangles are tables; omitted rectangles must have hw = 0 0
(findbbox y) alignrects y
:
NB. x is the bbox[;subboxes] corresponding to y
'yx hw' =. 0 {:: x
NB. if y is a rectangle, apply the formatting to create it from its bbox
if. 32 ~: 3!:0 y do.
  |: (yx,.hw,.{.y) formatcoord {: y
else.
NB. Boxed y.  Align the bboxes for each subbox, and recur to format them
NB. Get the subboxes, and the bbox for each.
  subbb =. 0 {::"1 subb =. 1 {:: x
NB. Split each bbox into start,:end.  The end value will be the size that we align;
NB. then we will account for the starting position by adding the start position to
NB. the beginning and removing it from the end
  subbbstart =. {."2 subbb
  subbbend =. +/"2 subbb
NB. Apply alignment rules to columns of h and rows of w to spread the bboxes through
NB. the allotted hw
  valign =. {.@> }. {. y
  halign =. {.@> }. {."1 y
  suby =. 1 1 }. y
  adjbb =. (1 0 2 |: valign ({.hw) applyalign |: {."1 subbbend) ,"0 (halign ({:hw) applyalign {:"1 subbbend)
NB. offset the bboxes relative to the containing bbox, and account for start offset of the contained box.  Then process each block
  (((yx,:0) +"2 ((,:"1  -) subbbstart) + adjbb) ;"2 0 {:"1 subb) <@(alignrects >)"1 0 suby
end.
)

NB. bbox is the position of a single rectangle, or the combination of the positions of the
NB. subboxes, if there are subboxes.  Result is bbox yx,:hw ; table of yx,:hw
findbbox =: 3 : 0
if. 32 = 3!:0 y do.
NB. boxed - strip the alignment field, find subboxes, calculate min box
NB. Get the spacing (amount at front, amount at end) for hw; convert to spacing for each box (add
NB. the front space to the start, and the end space to the length); add to each box
  spacing =. (2&{.@:}.@>@}. {."1 y) ,."1/ (2&{.@:}.@>@}. {. y)
NB. get the boxes (recursively if needed), add the spacing
  bonly =. spacing + 0 {::"1 sbbox =. findbbox@> 1 1 }. y  NB. obsolete > {."1
  bbox =. ((<0 0 0){spacing) ,: ( (+/@:(>./"1)@:({."1) , +/@:(>./)@:({:"1)) ) +/"2 bonly
  bbox ;< sbbox
else.
NB. Not boxed - use rect hw
  (0 0 ,: {. y);''
end.
)

NB. x is alignment code: 0=center 1=ljust 2=rjust 3=spread
NB. y is list of bbox extents
NB. m is extent of allocated area
NB. Result is start,extent for each bbox (a table)
applyalign =: (1 : 0)("0 1)
:
select. x
  case. 0 do.  NB. center
    ((|.!.0 + [: <.@-: m - {:) +/\ y) ,. y
  case. 1 do.  NB. ljust
    (|.!.0 +/\ y) ,. y
  case. 2 do.  NB. rjust
    ((|.!.0 + m - {:) +/\ y) ,. y
  case. 3 do.  NB. spread
    (|.!.0 ,. +/\^:_1) <. (*  m % {:) +/\ y
end.
)


NB. y is a dol (i. e. valueformat)
NB. result is hw of the DOL.  This is the maximum of x and y, plus a margin if
NB. the data is boxed (the left and right margins, plus the closing line)
extractdatasize =: {:@>@(1 2&{)"1
addboxmargin =: (BOXLINEWIDTH + +: BOXMARGIN)&(+"1)
extractDOLsize =: addboxmargin@]^:(3 < {:@$@[) extractdatasize  NB. Extracts size of each DOL separately
NB. similar, but x gives the sizes of a block in yx, result is size needed to display the largest
NB. contiguous blocks of that size
extractDOLsizelimited =: ]  addboxmargin@]^:(3 < {:@$@[)  (>./@(+/\   [: +/\^:_1 (0&,))&>  1 2&{)"1

NB. When we go to display an empty shape, we reimagine it with the shape up to the empty discarded
NB. y is the shape of empty, result is the shape to display
truncemptyshape =: 3 : 0
r =. ({.~ i.&0) y
NB. It is possible that this shape will be too big.  If so, we cut if off at a respectable length
r =. ({.~ 1e6 i.&1@:< */\) r
)

NB. Create the display object layout (DOL) for a noun
NB. data to use comes from globals set by traversedown
NB.  selresult - the value of the noun is the unboxed value
NB.  selfillinfo has the same shape as selresult; 0=normal, 1=fill
NB.  frame - a list of boxes, with the frames of all verbs that complete with this value.  The outermost frame comes first
NB.  selections -  what selections were performed.  This is a list of boxes, corresponding to the allframes,
NB.   each containing a selector for that frame.  The list of selectors may be shorter than the list of allframes,
NB.   in which case lower selectors are omitted.
NB. y is information about formatting (font size for the data)
NB. result is the layout, saved in globals in the locale
NB.  valueformat is shape;y endpixels;x endpixels[;array of boxed subnouns, with shape of the boxed noun, each box containing a DOL]
NB.   y endpixels is the ending position of each row in the display.  The number of atoms will be */ (even items of shape, counting from the end)
NB.    a gap of 1 pixel is left between blocks of rank 3, 5, etc.  This is included in the y endpixels values
NB.   x endpixels is similar for x
NB.   subnouns is provided only for boxed selresult.  It gives the valueformat for each box of the represented part of selresult.
NB.
NB.  The actual result of createDOL is the size of the allocated data area, in pixels, hw
createDOL =: 3 : 0
NB. The monad is what is called externally.  It uses the top-level values saved in the locale
NB. Create the data for the selresult.  We have to try to collect it, because the verb may have encountered
NB. an error before we tried to collect it, possibly leaving an uncollectable result (i. e. it would have
NB. had a framing error if it survived long enough to collect).  If collection succeeds, we use the
NB. collected value; if not, we box the atoms, collect that, and use it.  When we come to display
NB. the uncollectable value we will suppress the outer boxing in the display.
NB. If collection succeeds, we fill out an incomplete execution with fills of the appropriate type;
NB. if collection fails, we fill with spaces (OK since the values are boxed immediately, and we don't want
NB. the fills to have text)
(fillmask frameselresult selresult) createDOL y  NB. top result is boxed
NB.?lintsaveglobals
:
NB. The dyad does the work, and calls itself if the value is boxed.  The dyad returns
NB. valueformat, which is (shape of noun);y endpixels;x endpixels[;subDOLs]
fontdesc =. y
origshape =. $value =. x
if. 0 e. $value do.
  subDOLs =. 0$a:  NB. no subnouns unless boxed
  NB. If the noun is empty, create a display value out of the non-empty part of the shape.
  NB. We will display an empty rectangle for each such value
  value =. (truncemptyshape $ value) $ 0
  hw =. ($ value) $ ,: EMPTYEXTENT
elseif. 32 = 3!:0 value do.
  NB. If the noun is boxed, get a DOL for each box; extract the height/width from it
  NB. and add left/right margin and left line; if the CONTENTS
  NB. was also boxed, add the right margin for its closing line
  hw =. (BOXLINEWIDTH + +: BOXMARGIN) +"1 (extractdatasize + BOXLINEWIDTH * 3 < #)@> > subDOLs =. < createDOL&y&.> value
elseif. do.
  NB. If the noun is not boxed, just get the height/width for each atom
  NB. We also come here for the top level, which is boxed because it might not collect
  subDOLs =. 0$a:  NB. no subnouns unless boxed
  glfontextent ; (<(1 = {. $ value);0 1) { fontdesc
  NB. For very large nouns, sizing the text may take a long time.  Taking advantage of the
  NB. fact that we use fixed-pitch font for data, calculate the size once for each
  NB. displayed atom, and reuse that
  if. do.
    NB. fixed-pitch optimization.  Calculate # chars for each cell, then one example of each
    charcts =. #@(":!.displayprecision)"0 value
    charctsnub =. ~.@:, charcts
    lookuptbl =. (>:>./charctsnub) $ ,: 10 10
    lookuptbl =. (|."1 glqextent@(#&' ')"0 charctsnub) charctsnub} lookuptbl
    extentsyx =. charcts { lookuptbl
  else.
    NB. for variable-pitch font, use this
    extentsyx =. |."1 glqextent@dataxlate@(":!.displayprecision)"0 value
  end.
  hw =. (+/ 2 2 ($,) (<0 2) {:: fontdesc) +"1 extentsyx
end.
NB. combine the height/widths for the row & columns to get the size of each row/column
NB. Get the transposition vector: we bring the odd axes (starting from the end) in front of the even
NB. axes to get the display order.
axes =. (i. ((#~ -.) ; #~) [: |. $&1 0)@#@$ value   NB. 1;0 2   or 0 2;1 3
sizes =. (*/@> rcshapes =. axes ({&.:>"0 _ $) value) ($,)"_ _1 (_1 , ;axes) |: hw
NB. Take max across rows to get column extents; across columns to get row extents.
NB. row info (y endpixels) comes first
rcextents =. (>./"1@[ ; >./@])/ sizes
NB. Insert spacing between rank boundaries.  The extent values (nominally ending positions) will be the
NB. starting positions after we prepend a 0.  So we want to add the spacing to the last number at a rank,
NB. which will become the first of the next rank - except for the last, which doesn't have spacing after.
NB. Since the 1-pixel-wide line seems too narrow, add 1 extra space to each nonzero boundary
bdynos =. rcshapes (+ *)@(1&(|.!.0))@:(0&(i.&1@:~:)@|."1)@(#: i.@#)&.> rcextents
rcextents =. +/\&.> bdynos +&.> rcextents
NB. Assemble final result
origshape;rcextents,subDOLs
NB.?lintsaveglobals
)

MAXNAMELEN =: 30  NB. Maximum length of names for assignment
NB. create the display object for a verb/noun: header + result
NB. y is color/font/margin info:
NB.  (info for label);(info for shape);(info for status line);(info for data)
NB.  Each info is:
NB.    color;text color;font;fontsize;margin
NB.  for label and status line, we have just a list
NB.  for shape and data, we have a table which we will index by selector.
NB. formatting info is in globals
NB. we set globals, all of which have one entry for main and one for explorer if allowed
NB.  DOsize - pixel size of object
NB.  DOlabelpos - yx,:hw of the string, within the object
NB.  DOshapepos - yx,:hw of shape, within the object
NB.  DOdatapos - yxhw of data, within the object
NB.  DOcfm - the cfm info given in y
NB. result is 0 if we created the object, 1 if it was omitted
createDOvn =: 3 : 0
NB.?lintonly DOcfm =: 4 # <1 4$a: [ valueformat =: 4$a: [ DOyx =: 0 0 [ DOrankcfm =: 2 0 $ a:
if. #y do. DOcfm =: y end.
'cfmlabel cfmshape cfmstatus cfmimsgs' =. DOcfm
QP^:DEBDOvn 'createDOvn:?> coname''''%defstring 0%stealthoperand%'
NB. We need a graphics object selected so we can check text sizes.  We use the
NB. main form, because the isigraph may not be opened on the explorer yet
wd 'psel ' , winhwnd__COCREATOR
glsel 'dissectisi'

NB. Get the locale at the end of the inheritance chain.  This is usually this locale, but if there
NB. is an error we may be displaying data from a locale other than the end.  Even then, though, we want
NB. to display all rank & selection info from the end-of-chain
inheritedtailforselectinfo =: findinheritedtail''
NB.?lintonly inheritedtailforselectinfo =: <'dissectobj'

NB. Create the top line: name (if any), flanked by rank(s) (if any)
NB. We are creating one box that will describe the top line
NB. Get size of verb/name string, plus margin
QP^:DEBDOvn 'displaylevrank '
if. 2 = 3!:0 displaylevrank do.
NB. string; no ranks
  if. #DOranks =: displaylevrank do.
    stringdesc =. ALIGNCENTER addalignmentrect cfmlabel sizetext <DOranks
    namedesc =. (<ALIGNCENTER) addalignmentgroup stringdesc
  else. namedesc =. 0 addalignmentrect 0 0
  end.
else.
  assert. *#displaylevrank
NB. table; contains string;locale;rank[;rank].
NB. Remove lines with no display symbol
  nonemptylevrank =. (#~   ('';DLRCOMPEND) -.@:e.~ {."1) displaylevrank
  NB. Get the titles (verbs & modifiers)
  verblines =. 0 {"1 nonemptylevrank
  NB. Get the locales for each row
  DOranklocales =: 1 {"1 nonemptylevrank
  NB. Get the rank box(es) for each row.  Convert empty box to a more pleasing single space.  This is a table, with each row in y[,x] order
  ranklines =. ([: {.^:(0=#) ":)&.> 2 }."1 nonemptylevrank
  NB. Create the level,decoration for each rank label: the selection level, followed by a value, for each rank box, set if
  NB. the rank box contains ! indicating that the rank affected the result
  ranklevels =. (3 : '{. sellevel__y'"0 DOranklocales) ,"0 ((FONTDECORATIONSIZE*3)+FONTDECORATIONBOLD+FONTDECORATIONITALIC) * '!' = {.@> ranklines NB. table for each locale, 1 row per rank, y[x] order
  NB. Interleave rank and verb to produce [x,]v,y.  Remove the ! flag from the rank
  DOranks =: (-.&'!'&.> ranklines) (}.@[ , ] , {.@[)"1 0 verblines
  NB. Create the font class for [x,]v,y
  fontclass =. (-{:$DOranks) {.!.FONTCLASSSHAPE FONTCLASSRANKVERB,FONTCLASSSHAPE
  NB. Create the size,decoration for the verb cells: normal size except in the last (then large, and possibly expandable); no decoration
  NB. Interleave size,decor with sellevel,decor to produce a table per row of display, each row a size,decor, [x,]v,y order
  fontseldecor =. ranklevels ((}.@[ , ]) , {.@[)"2 1 ((,: (>:expansionstate),0) {.~ -#ranklevels)
  NB. Calculate the font for each box
  DOrankcfm =: fontclass cfmforclass"0 1"1 2 fontseldecor
  NB. Size the characters for each box
  rankrects =. DOrankcfm sizetext"1 0 DOranks
NB. Make the left rank left-justified, the right rank right justified.  Align each stack
NB. vertically({."1 displaylevrank) ({."1@] ,. [ ,. }."1@])
  namedesc =. (<ALIGNCENTER) addalignmentgroup ,. (<ALIGNCENTER)&addalignmentgroup"1 (ALIGNCENTER,ALIGNLEFT) addalignmentrect rankrects
end.
NB. Account for error string, if any; 0 0 if none
NB. If we are not in a try block, allow display of error only at the place where the error was detected
NB. during sniff.  This handles the case where the user makes a selection after sniff, and then there is
NB. no error detected at the point of error, and the enclosing conjunction shows its error.
if. errorwasdisplayedhere +. errorlevel ~: ERRORLEVELNONE do.
  DOstatusstring =: ((((#ENOTERROR) , 2 3 2 2 1 1)#'';'agreement';'framing';'invalid verb';'invalid operand';'0 for fill result';'recoverable error'),errorlevel { errormessagefrominterp;'error on fill-cell';'recoverable error') {::~ (ENOTERROR,ENOAGREE,ENOAGREEMASK,EFRAMING,EFRAMINGABORT,EFRAMINGEXEC,EINVALIDVERB,EINVALIDVERBMASK,EINVALIDMODOP,EINVALIDMODOPMASK,EFILLERROR,EINADVERSE) i. errorcode
else.
  DOstatusstring =: ''
end.
if. #DOstatusstring do.
  NB. If this failure is in a try path, parenthesize the error
  if. (errorlevel ~: ERRORLEVELNONE) +. errorcode e. EFILLERROR,EINADVERSE do.
    DOstatusstring =: '(' , DOstatusstring , ')'
    DOcfmstatus =: cfmimsgs
  else.
    DOcfmstatus =: cfmstatus
  end.
  statuslast =. 0   NB. Put error line before data
elseif. 0 = 4!:0 <'assignmentlabel' do.
  NB.?lintonly assignmentlabel =. ''
  NB. No error, and this value is the source of an assignment.  If so, make the statusstring into an
  NB. assignment line (if there is an error, the assignment must fail).  Enforce maximum display size
  DOstatusstring =: (_2 {. assignmentlabel) ,~ MAXNAMELEN ('...' ,~ {.)^:(<#) _2 }. assignmentlabel
  DOcfmstatus =: assigncfm
  NB. If there was an error, make the background red
  if. ' ' = {. assignmentlabel do.
    DOcfmstatus =: (<ASSIGNCOLORERROR) 0} DOcfmstatus
    DOstatusstring =: }. DOstatusstring
  end.
  statuslast =. 1  NB. Put assignment line after data
elseif. do.
  statuslast =. 0  NB. immaterial
  DOcfmstatus =: cfmimsgs
end.
if. #DOstatusstring do.
  thw =. DOcfmstatus sizetext <DOstatusstring
else.
  NB. No error string, but there is an assignment string (which will succeed, since there was no error).
  NB. Use the status block to report the assignment - we will move it to below the data
  thw =. 0 0
end.
statusdesc =. ALIGNSPREAD addalignmentrect thw
NB. Create the shape/selector line if we can
NB. The shape is the concatenation of the frames, so that in an expansion node it includes the expansion.
NB. We also append the shape of the max result cell in the last node, to get the total shape of the result
if. #af =. accumframe__inheritedtailforselectinfo 0  do.
  DOshapes =: <@;/./ |: 0 1 {"1 af  NB. boxes, each containing a (level-1 or -2) isf
  NB. Remember the locale of the last verb executed
  lastexecutednode =: (<_1 2) { af
  NB. The accumframe may include nodes that has a frame containing 1s but nonselectable.  We included them in
  NB. the DOshapes above.  For the rest of the calculation we want only the info from the node that did the selecting
  selectinglocaleinfo =. ({:/.~ 0&{"1) af
  NB.?lintonly lastexecutednode =: <'dissectobj'
  NB. If the last verb does not allow a selection (ex: i.@>), remove it from the shapes so that it doesn't show a selection block,
  NB. and also the fill status from the last selection goes through to the result-cell
  DOshapelocales =: 2 {"1 selectinglocaleinfo
  NB. Split the locales into sections ending with a dropdown
  endingfrets =. 1 (_1}) SFOPEN&e.@> DOshapes
  NB. Calculate the filled-cellsizes.  For each level get the shape of the filled cellsize at that level.  Result is
  NB. in boxes by dropdown sections
  filledsizes =. endingfrets <@(({.~ -@#)&.>~ {.);.2 (3) {"1 selectinglocaleinfo  NB. maxcellresultshape
  NB. Calculate filled status.  Fill starts after the first fill in a section and continues till the end.  Boxed by dropdown section
  filledflags =. endingfrets <@(+./\);.2 (4) {::"1 selectinglocaleinfo  NB. fillrequired
  NB. Calculate the filled frame: for each cell, the difference between the result of the previous level and the result of this level.
  NB. Always empty (actually immaterial, since never flagged) for the first level
  filledframes =. (2 (}.~ -@#)&.>/\ a:&,)&.> filledsizes

  NB. Roll up the frames to match the shapes, then append the final result, which is the very last cellshape.
  finalframes =. ;filledframes
  NB. Corresponding cellshapes
  finalsizes =. ;filledsizes
  NB. For fillflags, shift flags down so that the first in each section is 0 (fill at the highest level is attributed to the
  NB. result, not the frame).  But keep the very last fillflag to be the flag for the final result
  finalflags =. ; |.!.0&.> filledflags
  NB. If the last node doesn't select, it contributed to the result but it shouldn't contribute to the frame, not even an empty
  NB. rectangle.  So delete it everywhere it appears
  if. -. selectable__lastexecutednode do.
    DOshapes =: }: DOshapes
    DOshapelocales =: }: DOshapelocales
    finalframes =. }: finalframes
    finalflags =. }: finalflags
    NB. We also have to remove the undisplayed cell from the cell-size list, because the last cell-size
    NB. becomes the result cellsize
    finalsizes =. }: finalsizes
  end.

  NB. append the result info: the last cellshape, and the last flag.  We always have this, so that
  NB. there is one moe shape than shapelocales.

  NB.First we calculate the extra fillinfo value
  finalframes =. finalframes , {: finalsizes
  NB. We just take the flag to be the very last flag of the last section.  It doesn't matter whether
  NB. the last node doesn't select; if it doesn't, it won't fill, and the value in the last node will be
  NB. the same as in the one before.
  finalflags =. finalflags , (_1;_1) {:: filledflags   NB. obsolete {: > {:
  NB. Format the fillinfo: (fillframe) if fill called for
  fillinfo =. finalflags (#   '(' , ')' ,~ ":)&.> finalframes
  NB. Each box in DOshapes represents a frame.  If a frame contains 0, indicate that fact by putting * after the frame
  NB. We don't put * in for the last (result) box if there is one
  NB. Display of * is only performed when the user asks for it
  if. displayshowfillcalc do.
    fillinfo =. (0 ,~ (0 e. [: ; -.&SFOPEN)@> DOshapes) (, #&'*')&.>~ fillinfo
  end.

  DOshapehasfill =: +./ finalflags

  NB. Now append the last shape, which is the result shape of the last node if it is not selected, and the selected
  NB. shape if there is a selection.  Change empty to 'atom' in the result-shape
  if. selectable__lastexecutednode *. sellevel__lastexecutednode < #selections__lastexecutednode do.
    NB. User selected a result.  Display its shape.
    DOshapes =: DOshapes , <, ('atom' [^:(0=*@#@]) $)&.> extractselectedcell__lastexecutednode''
  elseif. (<resultlevel__lastexecutednode) -.@e. 0;1 do.
    NB. If the unselected result filled, don't repeat the shape - it will be in the fill
    DOshapes =: DOshapes , <, < 'atom' [^:(0=*@#@]) (_1 3) {:: af
  elseif. do.
    NB. But if the last node is a nonselecting dropdown (L: or each), we really have no idea of any shape
    NB. beyond the last node - they are incommensurate - so don't confuse things by showing one
    DOshapes =: DOshapes , <, <''
  end.
  NB. Convert each box to displayable, and install fill info.  No fill possible in the first selection
  NB. In each box of DOshapes the boxes up to SFOPEN are selection, the rest are dropdown(s)
  NB. The fill info is (optional * if verb is applied to a cell of fills);(parenthesized shape of cellsize if fill added)
  NB. This is where we convert DOshapes to a table (selection row if any is added later)
  NB. split each box of DOshapes into (<frame) , dropdowns; convert to character
  DOshapes =: ((<@":@;@{. , }.)~ i.&SFOPEN)&.> isftorank2 DOshapes
  DOshapes =: ,: ;&.> fillinfo <@(({.@] , [ , }.@]) >)"0 DOshapes
  if. #;DOshapes do.
    NB. There is a shape.  Format it and add selections
    cellshapedisp =: (<0 _1) {:: DOshapes

    NB. append selections if any
    if. #currselections =. sellevel__inheritedtailforselectinfo }. (sellevel__inheritroot + selectable__inheritroot) ((<. #) {. ]) selections__inheritroot do.
      NB. Convert to rank-2 ISF, then convert each box to displayable.  Add as second row
      DOshapes =: DOshapes , ;&.> ":L:0 isftorank2 currselections
    end.
  elseif. (0 < #selresult) *. (errorcode e. EHASVALIDFILLMASK) *. (0 ~: #fillatom) do.
    NB. The value, which has no shape, is a valid data value, viz an atom.  Display that shape to distinguish it
    NB. from an unselected value.  This is perforce the cell color, so make it the last thing in the list
    DOshapes =: ,: ,<'atom'
    cellshapedisp =: ''
    DOshapelocales =: 0$a:
  elseif. do.
    NB. No display because no selection
    DOshapes =: 0 0 $ <''
    NB.?lintonly cellshapedisp =: ''
  end.
else.
  NB. If there are NO valid selections (it's a block that has to wait for a selection), there is no shape-line
  DOshapes =: 0 0 $ <''
  DOshapelocales =: 0$a:
  lastexecutednode =: 0$a:
  cellshapedisp =: ''
end.
QP^:DEBDOvn'defstring]0 $shapetouse shapetouse errorcode sellevel selectable selections '
QP^:DEBDOvn'defstring__inheritroot]0 sellevel__inheritedtailforselectinfo sellevel__inheritroot selections__inheritroot '
QP^:DEBDOvn'maxcellresultshape__inheritroot '
if. #DOshapes do.
  NB. Get the pixel extent of each string.  Start at the selection level of this object
  DOshapecfm =: FONTCLASSSHAPE cfmforclass"0 (_1) ,~ sellevel__inheritedtailforselectinfo + i. <:{:$DOshapes
  shapeext =. DOshapecfm sizetext"1 0"_ 1 DOshapes
  NB. Create a rect object for the shape/selections
  shapedesc =. (<ALIGNCENTER) addalignmentgroup (<ALIGNSPREAD)&addalignmentgroup@,."1@|: ALIGNSPREAD addalignmentrect shapeext
else.
  shapedesc =. 0 addalignmentrect 0 0
end.
QP^:DEBDOL'defstring]0 >coname'''' shapetouse sellevel DOshapes selections resultlevel fillmask selresult '

NB. Early error is special: it formats the status above the verb, it aborts traversal,
NB. and it suppresses the display of data.  Go ahead and
NB. handle that here.
'DOlabelpos DOshapepos DOstatuspos DOdatapos DOassignpos displayscrollbars' =: <,:0  NB. make sure all names defined
resizable =. 0
if. errorcode e. EEARLYERROR do.
  NB. Create rectangles for status and verb, and stack them, expanding the status line if needed
  NB. No data on early error
  picknames =: 'DOstatuspos DOlabelpos'
  arects =. ,: , alignrects > (ALIGNLEFT;ALIGNCENTER) addalignmentgroup statusdesc ,: namedesc
NB. format the DOL if any
elseif. (errorcode e. EHASVALIDFILLMASK) *. (0 < #selresult) +. ((<'dissectrecursionpoint') e. copath coname'' ) do.
NB. The case of no results can happen here only if we have EUNEXECD which we passed through
NB. because of previous error.  We generally want to display these without any data, to show that they
NB. have none; but recursions are an exception, where we need to display the started-recursions that
NB. haven't completed, so that they can be selected for analysis.
NB. If there are ranks, create rectangles for each and stack vertically.
NB. Join the string and the ranks to create the top line
NB. Get size of shape string, plus margin.
NB. shape of result is shape of fillmask; box according to shapes of selectors
NB. Stack vertically: string/rank,shape,status,data

NB. Find the sizes to display: main, and explorer if allowed.  A table of 2 rows, one for each data type (normal and hidden-axes)
  hwtable =. calcformsize valueformat =: createDOL ((<0 2;2 3 4) { cfmdata)
QP^:DEBDOL'valueformat '
  NB. Keep track of the size of the largest noun encountered
  NB. Also remember the full size of this noun, in case we want to resize it
  maxactualnounsize__COCREATOR =: maxactualnounsize__COCREATOR >. maxdispsize =. extractDOLsize valueformat
  NB. If data doesn't fit in the allocated area, append scrollbars as needed.  We install the
  NB. bars here; the endpoints and traveler are added when the box is drawn
  hwtable =. hwtable +"1 SCROLLBARWIDTHPIXELS * |."1 displayscrollbars =: hwtable <"1 extractDOLsize valueformat
  datadesc =. ALIGNCENTER addalignmentrect hwtable
  NB. If the status block contains an assignment, move it to after the data.  Otherwise it's either empty
  NB. or has an error indicator; leave it before the data
  if. statuslast do.
    picknames =: 'DOlabelpos DOshapepos DOdatapos DOassignpos'
    arects =. ,@:alignrects@:>@:((ALIGNLEFT;ALIGNCENTER)&addalignmentgroup)"_1 (namedesc ,: shapedesc) ,"_ _1 datadesc ,:"_1 _ statusdesc
  else.
    picknames =: 'DOlabelpos DOshapepos DOstatuspos DOdatapos'
    arects =. ,@:alignrects@:>@:((ALIGNLEFT;ALIGNCENTER)&addalignmentgroup)"_1 (namedesc , shapedesc ,: statusdesc) ,"_ _1 datadesc
  end.
  NB. A block is resizable if it is bigger than the minimum threshold, where the minimum width is increased to
  NB. account for the width of the label and shape
  NB. Remember the minimum size in case we resize
  resizable =. +./ maxdispsize > MINRESIZABLE >. 0 , (<1 1) { brect brect@> (<0;0 1) { arects
elseif. do.
  NB. No data.
  picknames =: 'DOlabelpos DOshapepos DOstatuspos'
  arects =. ,: , alignrects > (ALIGNLEFT;ALIGNCENTER) addalignmentgroup namedesc , shapedesc ,: statusdesc
end.

NB. pickrects and picknames contain info for picking.  pickrects is a table, picknames is a list
NB. arects is a TABLE of info, one for each size
pickrects =: brect@> arects
NB. If any pickrects are empty, delete them.  This will also delete them from the eventual display
pickok =. * (<1 0)&{"2 pickrects
pickrects =: pickok #"_1 pickrects
NB.?lintmsgsoff
(picknames =: ({.pickok) # ;: picknames) =: |: arects =. pickok #"_1 arects
NB.?lintmsgson

NB. There should always be SOMETHING to display (shape, label, or data), but during sniff we don't allow
NB. any space for the data, which could leave us with nothing.  Make sure we have a valid DOsize then
DOsize =: 2 2&>.@{:@brect"_1 pickrects

NB. If this data block is resizable, create the resizing handle (in the main display only).  The resizing handle
NB. is outside the main data area, so we don't include it in DOsize
if. resizable do.
  NB. Extract the position(s) of the data blocks
  datarects =. (picknames i. <'DOdatapos') {"1 arects
  NB. append resize handle, as last item for high visual priority
  pickrects =: pickrects ,. (RESIZEHANDLEXYWH + ,:&0)"1 ({:"1 DOsize) 1}"0 1 (+/)@> datarects
  NB. Indicate resize handle present
  picknames =: picknames ,<'DOresizepos'
end.
NB. For picking, refigure the bbox to include the pick handle
DOpicksize =: 2 2&>.@{:@brect"_1 pickrects

NB. If there is a label stack, create an index for it.  The index will be a brect for each row of the label stack,
NB. but offset from the starting point of the pickrect for the rank stack.  If there is a label instead of a rank
NB. stack, we create a null pickrect
if. (#picknames) > labelx =. picknames i. <'DOlabelpos' do.
  if. 2 = 3!:0 displaylevrank do.
    NB. Just a label - no rack stack to pick.  Make one rectangle for the text
    DOlabelpospickrects =: ($arects) $ 1 2 2 $ 0
  else.
    NB. Convert the overall brect to yx,:00, then subtract it from the brect for each row
    DOlabelpospickrects =: (brect"1@> labelx {"1 arects) -"2"_1 (1 0) *"1 2 labelx {"3 pickrects
    assert. (#DOranklocales) = 1 { $DOlabelpospickrects
  end.
end.

NB. Similarly, if there is a shape/selection line, create an index for it
if. (#picknames) > shapex =. picknames i. <'DOshapepos' do.
  NB. Convert the overall brect to yx,:00, then subtract it from the brect for each row
  NB. This converts the shape/selection to a single row of pickrects
  DOshapepospickrects =: (brect"0@{.@> shapex {"1 arects) -"2"_1 (1 0) *"1 2 shapex {"3 pickrects
  assert. (#DOshapelocales) = <: 1 { $DOshapepospickrects
end.

NB. If selections have changed such that this locale cannot raise an explorer, delete any old one that exists
if. 2 > #DOsize do. destroyexplorer '' end.
QP^:DEBDOvn'DOsize pickrects '

NB. The scrollpoint persists over reselection/redraw.  But if the scrollpoint has been reset (by initialization or
NB. a higher selection), make sure scrollpoints makes the selection visible.  We are guaranteed to recreate the DOL
NB. whenever there is a change of selection.
NB. Nonzero initial scrollpoint could happen if the initial selection (for example, from u/ or error) is not 0, or if we have scrolled
NB. and then reset the selection to 0.  This action will also set the number of defined scrollpoints to the
NB. same as the number of allowed views, in case those mismatch
if. (0 = #scrollpoints) *. (<'DOdatapos') e. picknames do.
  if. #shr =. hlightforselection inheritroot do.
NB. calculate highlight rectangle tlbr; compare ending position against size of each datapos object; if either coordinate too high, set scroll to start at selection
    SM^:DEBHLIGHT'setting scrollpoint'
    QP^:DEBHLIGHT'defstring]0 edisp'''' shr sellevel selections valueformat selresult fillmask '
    if. #htlbr =. INVALIDRECT -.~ valueformat hlighttotlbr (<0 1) { shr do.
      scrollpoints =: ({. htlbr) *"1 0 (pickrects {~ < a: ; _1 ;~ picknames i. <'DOdatapos') +./@:<"1 {: htlbr
    end.
  end.
end.
NB. In case a view has been added or deleted, make the number of scrollpoints match the number of views.  Default to 0 if not set above
scrollpoints =: (#DOsize) {. scrollpoints
NB. Force the scrollpoint to 0 in any dimension that doesn't have a scrollbar.  That could happen if we have
NB. a scrolled display and the user enlarges the max datasize; then the scrollbar would be removed with data not
NB. on the screen
scrollpoints =: displayscrollbars * scrollpoints
NB.?lintonly 'DOlabelpos DOshapepos DOstatuspos DOdatapos DOassignpos' =: <2 2 $ 0
NB.?lintonly 'DOranks DOranklevels DOshapes DOshapehasfill' =: ($0);($0);(0$a:);0
NB.?lintonly 'DOlabelpospickrects DOranklocales' =: (1 0 2 2$0);(0$a:)
NB.?lintonly 'DOshapepospickrects DOshapelocales' =: (1 0 2 2$0);(0$a:)
NB.?lintonly lastexecutednode =: <'dissectobj'
NB.?lintonly DOshapecfm =: 2 5$a:
0  NB. object created, say so
NB.?lintsaveglobals
)

SFOPEN =: <,'>'  NB. This element in a CSF or ISF means 'drop down a level'
NB. The highlight system puts highlights into canonical selection form.
NB. standard selection form is a box containing a list of boxes
NB. Each box contains a list of boxes, where each nonempty box specifies
NB. selection and each SFOPEN specifies dropping down a boxing level.

NB. y is a box containing selectors, or SFOPEN
NB. Result is 0 if it contains unboxed selectors,
NB. _1 if it is SFOPEN, 1 if boxed selectors
classsel =: <:@L.`_1:@.(-:&SFOPEN)"0

NB. y is an ISF, a box which contains either an array of axes of a list of boxes with selections.
NB. We convert any level-1 boxes (containing the array of selections) to level 2, by converting each
NB. list to a list of 1 boxed atom.  We are careful to make sure that any boxed ISF is a list at least.
isftorank2 =: ,^:(''-:$)@(,@<"1^:(0=L.))&.>
NB. y is ISF, which convert to level 3 by boxing any level-2 contents.  Result is in one-box-per-axis form
isftolevel3 =: <"0&.>^:(0=classsel)"0&.>@isftorank2
NB. y is a rank-2 ISF.  Make sure it starts with a selection, by prepending a null selection if it starts with SFOPEN
isfensureselection =: (<0$0)&,^:(SFOPEN={.)&.>

NB. selections themselves are in initial selection form, which is like CSF without the outer box,
NB. and also allows the contents of a box to be a (nonempty) list which specifies selection only.
NB. Unboxed contents should be combined together and merged into the first nonempty box of the
NB. next boxed contents.
NB. ISF works because it is impossible to have a selection that drops down a boxing level and then
NB. does a selection without going down a level (in other words, ends with a selection-only).
NB. y is list of ISFs, result is boxed CSF
NB. First bring any rank-1 contents to ranks 2.  Then cut on SFOPEN, combining all boxes found between them.
NB. If there are no boxes between SFOPEN, don't create one (that would change the boxing levels)
isftocsf =: ;@:(<@(3 : 0)"1)
NB. Bring the list of ISFs up to level 3, where each box contains selections from one axis
NB. Collect all the selections at the same boxing level into one list.  This produces a sequence
NB. of boxes containing lists of boxes, one per axis, interspersed with SFOPENs
NB. Remove empties, which select nothing.  They have been needed till now to distinguish highlighting
NB. a scalar (which selects nothing) from no highlight at all.  But here we know we have a highlight
if. #highlightblocks =. a: -.~ ;@(<@(SFOPEN ,~ <@;^:(*@#));._2)&.(,&SFOPEN)@;@:isftolevel3 y do.
  NB. Convert each list of selections to a list of highlights, where a highlight is a boxed 2-row table
  NB. of top-left index,:bottom-right index.  For the last two axes only, collect contiguous indexes
  NB. into a single highlight.  For other axes, make each index a separate highlight
  NB. Before the last drop-down, there must be no multiple selection
  pathx =. (SFOPEN,highlightblocks) i: SFOPEN  NB. number of boxes including last SFOPEN
  path =. pathx {. highlightblocks
  assert. 1 > >./ #@$@> ; path -. SFOPEN  NB. no multipleselects before last dropdown
  NB. Convert the selections in the path, now a list of boxes containing atoms, to a two-row table of start,:end+1
  path =. (,: >:)@;&.>^:(-.@-:&SFOPEN)"0 path
  NB. Turn each box of the path after the dropdown into a box containing boxed tables, which can then be
  NB. catalogued and run together.  Each axis (now an axis containing one or more selections) will turn into a box
  NB. containing boxes, where each box holds a 1x2 table describing the selection.  For the last 2 axes,
  NB. the box holds (start,end+1), one for each interval; for preceding axes, it holds (sel,sel), one for each
  NB. selection.
  if. #lastsel =. pathx }. highlightblocks do.
    NB. There is a selection after the dropdown
    NB. See how many items in each row are collectable into a larger blocks.  Max 2.  Take the neg since we take/drop from the end
    ncollectable =. - 2 <. {: $ > lastsel
    last2 =. ncollectable {."1 > lastsel
    prev =. ncollectable }."1 > lastsel  NB. Now boxing level 1
    prev =. 1 2&$&.>&.> prev  NB. Now boxing level 2
    NB. Take first, last+1 of each sequence of consecutive values, whether ascending or descending
    last2 =. (<@,:@(<./ , >:@(>./));.1~     (-@# {.!.1 (1) < [: | 2&(-/\)))&.> last2
    assert. ((,1) = }:$prev) *. ((,1) = }:$last2)   NB. higher ranks untested
    <"1 path ,"1 0 , |:@;&.> { prev ,"1 last2
  else.
    NB. No selection after dropdown.  Just use the path to the dropdown
    < path
  end.
else.
  NB. No selections at all: must be highlighting a scalar.  Create a selection with no axes
  < ,<2 0$0
end.
NB. Create the cartesian product of the highlight requests
NB. roll up each row into a table, transpose it, make it a single box
NB. Prepend the selection before the last dropdown
NB. Box each request
)

NB. Create highlight rect for the current selection.
NB. y is the locale of the base of the inheritance chain; that's the finest highlight, so that's what we use
NB. We take all the selectors there are, up the length of the frame of this level; but if there aren't enough, we don't highlight.  Discard
NB. selectors for higher levels.  The only way we can get more selectors than frame is during sniff, where the lower
NB. selection is propagated up automatically.
NB.
NB. Result is table of (selection type (0 here));(boxed highlight in CSF (if any)), or empty table if no selection
hlightforselection =: 3 : 0
NB.?lintonly y =. <'dissectobj'
NB. If the coarsest node is not highlightable, make no highlight
if. ishighlightnode do.
  (<0) ,"0 , isftocsf^:(*@#) sellevel }. (sellevel__y+selectable__y) (] }.~ 0 <. (- #)) selections__y
else. 0 2$a:
end.
)

NB. Create highlight rects for the operands that have been selected from this node
NB. y is a table of (selection level);(<list of ISFs)
NB. Overall result is a table of (selection level);(CSF, a single cell).
NB.
NB. Each 1{::y is a sequence of boxes: each box contains an array of boxed ISFs, where each list corresponds to one selection
NB. If the selector contains an array,
NB. each list describes one selected cell (obviously all such cells have the same rank) and the shape with respect to lists
NB. gives the shape of the selected group of cells, which may become important if subsequent selectors select from the group.
NB.
NB. We go through the sequence, appending each new selection to the previous one, leaving a sequence of increasingly long
NB. ISFs.  The interesting part comes when one of the selections has rank >1 (example: u/.).  When this is first
NB. encountered, it creates an array of ISFs.  If this array is subsequently selected from, the leading axes of the selection
NB. select from the array of rects, and any surplus is appended to the selection.
hlightforoperands =: ;@:(4 : 0&.>/"1)`(a:"0)@.(0=#)
NB. Here x is the selection level.
NB. We use axes from each new selection to take from the old selections, and then append
NB. the new selections, with those leading axes removed.  The result may have any shape, but each list is
NB. an hrect
NB. y here is a list of boxes, each containing (an array of) lists of selections.  The selections are isfs, so first
NB. bring them up to rank 2
NB. If there is nothing to highlight, return empty.  We must test explicitly because isftorank2 behaves oddly on empty
if. 0 e. $y do. 0 2$a:
else.
  (<x) ,. , isftocsf > chainISFs&.>/&.|. a: , isftorank2 y
end.
)
NB. x and y are contents of a single box of (an ISF that has been brought to level 2); i. e. x and y have boxing level 1 or 2
NB.  and are to be interpreted as (an array of) lists of selections
NB. Each box in xy contains either a list of axes or a boxed list of alternatives for successive axes.  For simplicity here we demand that
NB. any level-3 operand (one that is a boxed list of alternatives) must be in a box by itself
NB. Result is the joined lists (y,x), flattened so that each axis is in one box
chainISFs =: dyad define
NB. get the first box of selections from x.  If there are others, they will be SFOPEN, which we can't handle here, so one box is enough
NB. Turn the selections into a box for each axis
selx =. <"0^:(0=L.)@>@{."1 x
if. #ysel =. y do.
  ranky =. <: # $ ysel  NB. number of axes of x that can select
  NB. take selections using x, for as many axes as y can handle.  These will select from y.  There may be surplus y shape
  usableselx =. (selrank =. ({: $ selx) <. ranky) {."1 selx
  NB. get the selection from y for each x.  This may select an array of y, if y has surplus frame
  ysel =. usableselx (<"1@[ { ]) ysel
  NB. Now delete the ranks we used from x.
  if. {:$selx =. selrank }."1 selx do.
    NB. Repeat the procedure, now using remaining axes of x to select from the trailing boxes of y that contain multiple values
    NB. Operate on each list of ysel/selx.  Result is new ysel;selx
    'ysel selx' =. (|:~   i.@<:@#@$) selx combineyxsels ysel
  end.
end.
NB. replace the first box of x (if there is any residual x after selection), and join it to the selected y.  If y has surplus rank this will copy the surplus rank to the result
ysel ,"1 selx (,~ <)~"1^:(*@{:@$@[) }."1 x
)

NB. y is a ysel, x is a selx.  Result is list of (new ysel);(new selx)
combineyxsels =: 4 : 0"1
assert. 1 = #$x
assert. 1 = #$y
NB. Since we are perforce down to a single list for ysel, run all the selections into a single level-2 list.
NB. Preserve single boxes, including SFOPENs, intact
yl2 =. , ; <^:(1=L.)"0 y
NB. There may be multiple boxes of y containing boxes with multiple values, BUT: these boxes must
NB. be trailing boxes of y.  See how many there are.  This is the number of axes of x we can index.  this is ranky
NB. We take the LEADING x axes among the eligible ones, so that successive axes go in order
NB. selrank, the number of boxes of x we can process, is limited by the number of trakiling boxes of y
if. selrank =. (#x) <. ranky =. 1 i.&1@:~: |. classsel yl2 do.
  NB. There are selections to make.  They should not include SFOPEN
  NB. We apply to the leading axes among those that have alternatives.
  NB. The result of the selection should have the same boxing level as x: if x is a list of alternatives,
  NB. so should the result be; while if x is a simple selection, so should the result be.  So, we
  NB. open y all the way, then apply x at level 0
  yl2 =. (- ranky) (}. ,  selrank (}. ,~ (selrank {. x) ({L:0 >^:L.)"0 {.) {.) yl2
end.
NB. Return the modified axes of y, and the unprocessed selections of x
(,< yl2) ;< selrank }. x
)

NB. Convert highlight rectangle(s) to rectangles (tl,:br) unboxed (never empty)
NB. x is shape;yendpos;xendpos[;subDOLs], y is boxed CSF
NB. Each box of y is either SFOPEN or a 2-row table where the first row is the index of top-left and the
NB. bottom row is the index of bottom-right
NB. Result is table of top,left,:bottom,right
NB. We drop down through the boxing hierarchy according to the occurrences of SFOPEN
NB. It is possible that this routine will be called with an invalid rectangle: to wit, when, during sniff,
NB. we select a nonexistent output (which we keep for highlighting porposes, since the input exists).  This
NB. case manifests as surplus shape with no corresponding subDOLs.  We return INVALIDRECT then
INVALIDRECT =: 2 2 $ 0 0 _1 _1

hlighttotlbr =: (4 : 0"1 0)`((2 2$0)"0)@.(0=#@])
NB. If the data is empty, we don't have any good way to highlight it, and it will generate an error here
NB. when we ultimately try to select two axes.  So we reject it early
if. 0 e. 0 {:: x do. INVALIDRECT return. end.
NB. We must start with a selection; if it's a drop-down, prepend empty selection
if. SFOPEN -: {. >y do. y =. (<2 0$0)&,&.> y end.
sel =. (0 0,:1 1) (]  ,"1~  -@{:@$@]  |.!.0"1  ({.~ #)) 0 {:: > y  NB. obsolete > {.
remainingcsf =. }. > y
axes =. (i. ((#~ -.) ; #~) [: |. $&1 0)@# shapeused =. 0 0 , 0 {:: x  NB. axes: 1;0 2   or 0 2;1 3
NB. Normally, the selection does not exceed the size of the displayed result, since it came from a click on the display.
NB. But if there is an error, we will select further to sniff out the error, and that may leave us with
NB. undisplayable selections.  So we discard them
sel =. shapeused (<.&({:@$) {."1 ]) sel
NB. To handle <2 axes, we will add 2 leading 0 axes to the highlight selector.
NB. We compensate by adding 2 to all the axis numbers, and inserting a leading axis.
NB. If there are no axes to add to, there are 2 cases: 1 axis, which is ($0);,0: we turn that
NB. into 1;0 2 - and 0 axis, which we turn into 1;1 - repetition is OK, since the repeated axis is the
NB. added axis.  We add leading 0 (actual value immaterial) to axisshapes to match the shape
NB. Get selection for each axis, producing a 2x(1 or 2) table of indexes
tlx =. (<"0 axes) (({. #. }.)@:({"1))"0 _ shapeused , sel   NB. extend localsel with lower 0; pull the y and x values to get indexes of top-left
tlbr =. |: tlx {&>"1 0 (0)&,&.> (1 2) { x  NB. fetch yx of topleft from input positions (and bottom-right, if after last dropdown)
NB. We have corners.  
NB. If the next instruction is to enter the box, do so
NB. If there is more to do after entering the box, recur to get the position of the next-level rectangle
NB. If nothing to do after entering the box, get the full size of the opened operand
NB. and add the tl to produce the result
if. (#remainingcsf) *. (3 < #x) do.
  assert. SFOPEN -: {. remainingcsf  NB. in CSF, selection must be followed by open
  assert. sel =&({:@$) shapeused  NB. selection to end of shape required before open
  openedDOL =. (3;(<0;<<0 1) { sel) {:: x
  if. 1 = #remainingcsf do.
    NB. The selection ended by dropping into the last selection box.  Treat that as a selection of the entire
    NB. opened operand, and create a rectangle from the top-left (implied 0) and bottom-right
    selrect =. 0 0 ,: {:@> 1 2 { openedDOL
  else.
    NB. There is selection left to do in the opened operand.  Go do it
    selrect =. openedDOL hlighttotlbr < }. remainingcsf
  end.
  NB. We have the rectangle for the selection, relative to the start of the box.  Add the box position, and
  NB. include offset to contents
  (({.  tlbr) + BOXLINEWIDTH + BOXMARGIN)&+"1^:(INVALIDRECT -.@-: ]) selrect
elseif. # ; remainingcsf -. SFOPEN do.
  NB. Surplus selector with nothing to select from: return invalid rectangle
  NB. If there are only drop-down and empty selections, that's OK, it's opening an open noun and we ignore the excess
  INVALIDRECT
elseif. do.
  NB. No further selection. 
  tlbr
end.
)



NB. ***** joining display objects *****

cocurrent 'dissect'
NOLAYOUTS =: 0 4$a:   NB. Starting point for nouns: no layouts at all
NB.
NB. This code deals with screen layouts, which are kept in the form
NB. (DOL locales);(yx,:hw for each locale);wires;resulthook
NB. where
NB.  DOL locales is list of DOL locales
NB.  yx,:hw brick of yx for each locale - each position relative to top-left of layout
NB.  wires is brick of nx2x2: locale,<(face# tblr),(fractional position)  source then dest
NB.  resulthook is a table of: (result DOL locale),<(face,position of hook on bottom row)
NB. If a layout is a reference only, it will have empty DOLtable and margins


MAXVERTFLOAT =: 10  NB. Number of gridcells leeway to allow a box to move up to maximize overlap

NB. x is a single DOL, y is the physreqandhighlights table for it
NB. Install the highlight rects for it, provided the operand has not been marked as a stealth path.
NB. The proviso is to prevent highlighting an operand that is nominally referred to in a stealth path but doesn't
NB. actually affect the result
addselecttoDOL =: 4 : 0"1 0
if. -. a: e. x do.  NB. If there are handles, they point to the output
  loc =. {: x  NB. the locale of the DOL
NB.?lintonly loc =. <'dissectobj'
  QP^:DEBSELECT'addselect:?defstring]0 sellevel sellevel__loc >y '
  addselectedoperands__loc >y
end.
''
)

NB. Entry point when dol and locale are joined together.  This is also called from original traversal,
NB. thus needs to be in outer locale.
NB. We create the DOL for the locale named in y, and then add the operand selections originating in that locale
NB. to the places they come from
joinlayoutsl =: 3 : 0
'dol loc right' =. 3 {. y , <0 2$a:
NB.?lintonly loc =. <'dissectobj'
NB. If there are operand selections, apply them to the input locales
QP^:DEBSELECT'dol joinlayouts:physreq=?physreqandhighlights__inheritroot__loc coname$0 defstring]0 >loc defstring__loc]0 defstring__inheritroot__loc]0 '
if. *#physreqandhighlights__inheritroot__loc do.
NB. The highlight requests have been consolidated by inheritu so that they now are a list for
NB. each operand, with one highlight request per sellevel.  Also, physreqandhighlights has been
NB. brought back so that it contains all the selections out to the last highlight request.
NB. For each request, we take all the
NB. physical selections before the highlight, and append the highlight
  dol addselecttoDOL physreqandhighlights__inheritroot__loc
end.
NB. if there are right-sided operands, highlight them too
if. #right do.
NB. Install highlights
  addselecttoDOL&>/"1 right
end.
NB. If this node is a suppressed stealth operand, remember the fact so we can give the user the option of showing it
assert. stealthoperand__loc e. 0 1 2 4 5 6
if. dispstealthoperand__loc e. 1 2 do. stealthopencountered__COCREATOR =: 1 end.
NB. Init that we have not allocated this block.  We need this even for stealth verbs, since we may try to
NB. display them if selected from the sentence display
DOsize__loc =: ''
NB. If stealth verb, there is no display; but because of inheritance and suppressed detail, we might have the stealthoperand flag
NB. set in a locale that is creating a noun; we'd better create that.  We detect nouns, as usual, by absence of handles in
if. (dispstealthoperand__loc e. 1 2 5 6) *. (*#dol) do.
  NB. Stealth operand vanishes, replaced by its selected input
  (, dispstealthoperand__loc { 0 _1 0 0 0 _1 0) { dol
else.
  displayxyinputs__loc =: dol
  if. #right do.
    displayrightinputs__loc =: 0&{::"1 right
  else.
    displayrightinputs__loc =: 0 2$a:
  end.
  1 2 $ loc
  NB.?lintsaveglobals
end.
)

cocurrent 'dissectobj'

NB. Join layouts left-to-right
NB. y is (brick of yxhw);(brick of yxhw)
NB. x is (y offset for right box),(1 (default 1) if OK to float the right box (down) vertically),(minimum movement of r)
NB. Result is like the input, but with the positions adjusted
joinlayoutslr =: 3 : 0
0 1 0 joinlayoutslr y
:
QP^:DEBLAYOUT'Joinlayoutslr:x?x y '
'lyxhw ryxhw' =. y
'rofsty floatok minrmove' =. x
NB. bottom-justify the blocks.  slacks is how much slack is left at the top of each block.  One
NB. of these values is 0; round the other to an even number of grids
slacks =. <.@(0.5&+)&.(%&ROUTINGGRIDSIZE) (- <./) (>./ +/"1 {."1 lyxhw) , (rofsty + >./ +/"1 {."1 ryxhw)
if. </ slacks do. lyxhw =. lyxhw +"2 (2 2) {. +/ slacks else. ryxhw =. ryxhw +"2 (2 2) {. +/ slacks end.
  
NB. Calculate right profile of left block.  lss is the start/stop list, which is a table of
NB. (y,x) values: positive x means a block starts at that y-position and x-value; negative x means
NB. a block ends at that y-position-1 and |x-value.  We extend the blocks by MINBOXSPACING to leave
NB. margin, and we sort descending so a scan from the end encounters blocks in y order
lssy =. {."1 lss =. \:~ ,/ ((MINBOXSPACING + +)/\@:({."1) ,. (,-)@(+/)@:({:"1))"2 lyxhw
NB. group the start/stops in boxes by y, and then roll them up, leaving the active x values.  Then
NB. take the max of each box, to give the rightmost position.  The resulting list is the largest x
NB. active at each point in lssy; 0 for empty rows.  Do removals before insertions, in case a value
NB. is added and deleted simultaneously
lprof =. >./@> ((#~ >:&0)@[ , (-. (,-)@:(#~ <&0))~)&.>/\.&.(,&(<,0)) lssy </. {:"1 lss  NB. endpoint, with one extra 0
NB. Calculate left profile of right block, plus one (reqd so never have position of 0).  This is the
NB. leftmost filled position for each y.  Higher values give more slack; use 1e6 for empty rows
rssy =. {."1 rss =. \:~ ,/ ((MINBOXSPACING + +)/\@:({."1) ,. (,-)@(1 + (<0 1)&{))"2 ryxhw
rprof =. <./@> ((#~ >:&0)@[ , (-. (,-)@:(#~ <&0))~)&.>/\.&.(,&(<,1e6)) rssy </. {:"1 rss
NB. Calculate spacing
NB. We look up each point in the profile in the OTHER profile, and take left-right to get possible
NB. starting position.  Actual start position is max of the differences.
NB.
NB. We do this computation for a range of vertical offsets, and take the one with smallest start position
NB. Since the profiles coalesced identical points, we'd better do the same on our lists of important points
lssyrng =. (ROUTINGGRIDSIZE * i. MAXVERTFLOAT) +/ lssy =. ~. lssy   NB. left vert positions, shifted
rssyrng =. (ROUTINGGRIDSIZE * i. floatok} 1,MAXVERTFLOAT) +/ rssy =. ~. rssy
NB. Each y value gives the valid x until the next higher y value; so we will keep the
NB. y values in DESCENDING order and look up in that table, so that match on an interval means
NB. that the corresponding x is valid.  We extend the table to handle searches that run off the
NB. end (i. e. are lower than all the points being looked up).
llookups =. >./"1 lprof -"1 (rprof,1e6) {~ rssy I. lssyrng  NB. left vert positions looked up in right, subtracted from left horiz pos
rlookups =. >./"1 rprof -~"1 (lprof,0) {~ lssy I. rssyrng
NB. Calculate the moves (right,left) represented by each lookup: left moves then right moves
allmoves =. (0 ,.~ i. # llookups) , (0 ,. i. # rlookups)
NB. Get the distance for each lookup, left then right
NB. To make sure we check all the points of change, we look up the left points in the right, and
NB. vice versa
alllooks =. minrmove >. llookups ((>. {.) , (>. {.)~) rlookups
NB. Penalize each vertical movement by a small amount, and then choose the smallest score
bestx =. (i. <./) alllooks + (0.6 * ROUTINGGRIDSIZE) * +/"1 allmoves
NB. Get the amount to move each side up, corresponding to the winning value
moves =. ROUTINGGRIDSIZE * |. bestx { allmoves
NB. Get the spacing represented by the best move
lookbest =. bestx { alllooks
NB. Move each side down (i. e. up the diagram) to the extent there is slack; move other side up the rest
vertadj =. moves ([ (|.@:- - ]) <.) slacks
NB. Move blocks to establish spacing
lyxhw =. lyxhw +"2 (2 2){. {. vertadj
ryxhw =. ryxhw +"2 (0) ,:~ ({: vertadj) , >.&.(%&ROUTINGGRIDSIZE) lookbest + MINBOXSPACING + 1

lyxhw;ryxhw
)


NB. DOsize is initialized to empty when the node is first joined
NB. displayxyinputs is a table of (display locale);(wiring locale) (no extra boxing)
NB.  If the wiring locale is not the same as the diaplay locale, this is a loopback
NB.  If the display locale is a:, this node and its wires are not diaplayed
NB. displayrightinputs is a table of inputs coming in on the right, similarly
NB. called in an active locale, the root for startes

NB. Go through all the locales, creating the layouts.  This sets DOsize.
NB. Initialize parentlocales to empty.  These will hold the parents
NB.  that will create the final layout grouping
NB. Return the locales created by this node and its descendants, but empty if this locale has already
NB. been laid out.  The final result is the list of all active locales.
findactivelocales =: 3 : 0
if. DOsize -: '' do.
  parentlocales =: 0$a:
  createDO''  NB. sets DOsize
  newlocs =. ,coname''  NB. This may set return value
  for_l. a: -.~ 0 {"1 displayxyinputs , displayrightinputs do.
    NB.?lintonly l =. <'dissectobj'
    newlocs =. newlocs , findactivelocales__l '' NB.This sets return value
  end.
  NB.?lintsaveglobals
else.
  0$a:
end.
)


NB. Establish heights for each locale
NB. Called in the locale of the root
NB. There is an eligible list of (higher locale);(lower locale);(yposition)
NB.   where higher means xy and lower means the verb they are args to
NB. We continually take the lowest eligible height and
NB.   add the higher to the parentlocales of the lower
NB.   call the higher.  It will add its height+margin to the yposition and return
NB.     a table of (higher loc);(lower loc);yposition
NB.   decrement the usecount for each returned higher locale, and put in onto the
NB.     eligible list, at most once, only when the use count goes to 0
NB. Result is (list of locales);(brick of yxhw for each object, not necessarily in the same order as the locales);(wires)
NB.  Each wire is a 2x3 table of yxhw#,face# (tblr),fraction   The face# is the index into the brick of yxhw
establishgrouping =: 3 : 0
getplacelocaleindex__COCREATOR =: (placelocales =. findactivelocales'')&i.
eliglist =. 0 3$0
elignow =. getplacelocaleindex coname ''
heightnow =. 0
prevloc =. 0$a:
NB.?lintonly prevloc =. <'dissectobj'
parents =. 0$0 [ wires =. 0 2 2$0
NB. Accumulate all wires, and all parent references
for_l. placelocales do.
  NB.?lintonly l =. <'dissectobj'
  'p w' =. findusectandwires__l l_index
  parents =. parents , p
  wires =. wires , w
end.
NB. Create usect+1 for each locale, in order
usect =. #/.~ (i. # placelocales) , parents
while. do.
  NB. At top of loop elignow is the next locale to handle, and its height
  NB. is heightnow.
  NB. decrement usect+1 for elignow, and skip processing if the count doesn't
  NB. drop to 1 (which means all instances processed)
  usect =. (nct =. <: elignow { usect) elignow} usect
  if. nct <: 1 do.
    NB. OK to lay out elignow.  Add it to the parentlocales of the calling node;
    NB. put layout parents of elignow onto the eligible list at the correct height
    el =. elignow { placelocales
    NB.?lintonly el =. <'dissectobj'
    if. #prevloc do. parentlocales__prevloc =: parentlocales__prevloc , el end.
    eliglist =. eliglist , (getplacelocaleindex a: -.~ 0 {"1 displayxyinputs__el) ,"0 1 elignow , heightnow + MINBOXSPACING + (<0 0) { DOsize__el
    eliglist =. eliglist , (getplacelocaleindex a: -.~ 0 {"1 displayrightinputs__el) ,"0 1 elignow , heightnow + (<0 0) { DOsize__el  NB. right input flush with top of el
  end.
  if. 0 = #eliglist do. break. end.
  NB. Select the lowest height from the eligible list, and remove the value from the list
  nextx =. (i. <./) 2 {"1 eliglist
  'elignow prevloc heightnow' =. nextx { eliglist
  prevloc =. prevloc { placelocales
  eliglist =. (<<<nextx) { eliglist
end.
assert. *./ usect <: 1
'locales yxhw' =. assemblelayout''
assert. placelocales -:&(/:~) locales
NB. Since the wires use the placelocale order, reorder the blocks into that order
placelocales ; (yxhw /: getplacelocaleindex locales) ; wires
NB.?lintsaveglobals
)


NB. Visit all active locales, returning
NB. (list of all locales referred to, possibly multiple times);(table of wires, nx2x2 where each
NB.  2x2 is (sourcelocale);(source position),:(target locale);(target position)
NB. y is index of current locale
findusectandwires =: 3 : 0"0
NB. Get the wires to inputs of this node.
if. #actinputs =. (#~   [: -. a:&e."1) displayxyinputs do.
  inputpos =. 0 ,. (#actinputs) {:: '';(,0);_0.3 0.3
else.
  inputpos =. 0 2$0
end.
NB. Remove any deleted stealth inputs, create wires for any remaining
NB. If an input is stealth, use the stealth position

NB. repeat for right inputs, but use different input position
if. #actright =. (#~   [: -. a:&e."1) displayrightinputs do.
  inputpos =. inputpos , 3 ,. 0.5 - 0.6 ^ >: i. #actright
end.

NB. Get the output positions, depending on whether the wire is a loopback
NB. which is detected by layout locale different from wiring locale
outputpos =. 1 ,. (~:/"1 allparents =. getplacelocaleindex actinputs , actright) { 0 0.3

wires =. (({:"1 allparents) ,. outputpos) ,:"1 (y ,. inputpos)

NB. Return locales followed by wires
({."1 allparents) ,&< wires
)


NB. Create the final layout
NB. Recur through the parentlocales, joining parents horizontally and then vertically to
NB.  the children
NB. Result is (list of locale-names);(brick of yxhw for those locales)
NB.  the last locale (and brick) always contains the output
NB. called recursively, in a locale to lay out
assemblelayout =: 3 : 0
select. #toplocs =. parentlocales (e. # [)~ ~. a: -.~ xylocs =. 0 {"1 displayxyinputs
case. 1 do.
  yl =. {. toplocs
  NB.?lintonly yl =. <'dissectobj'
  'locs yxhw' =. assemblelayout__yl''
  NB. Position the lower block so that the input that this wire feeds is directly below the
  NB. output.  We have to see which input corresponds to this output.
  outputx =. (+  -:)/ 1 {"1 outyxhw =. {: yxhw
  leftx =. outputx - (1 { {.DOsize) * ((xylocs i. yl) + (<:#xylocs)) { 0.5 0.2 0.8
  topy =. MINBOXSPACING + +/ 0 {"1 outyxhw
case. 2 do.
  'xl yl' =. <"0 toplocs
  NB.?lintonly yl =. xl =. <'dissectobj'
  'locs yxhw outyxhw' =. (assemblelayout__xl'') (,&.>&{. , (,&.> , ,:&{:&.>)/@joinlayoutslr@,&{:) (assemblelayout__yl'')
  NB. Position the lower block midway between the two outputs
  outputx =. +/@, 0.5 0.25 *"1 (1) {"1 outyxhw
  leftx =. outputx - (1 { {.DOsize) * 0.5
  topy =. MINBOXSPACING + >./ +/"1 (0) {"1 outyxhw
case. do.
  locs =. (0$a:) [ yxhw =. 0 2 2 $0
  NB. Just position the block at 0
  leftx =. topy =. 0
end.
NB. Center the block so that the output is on a gridpoint in the horizontal direction
thisyxhw =. (<. >.&.(%&ROUTINGGRIDSIZE)&.(+&(-: {. DOsize)) topy,leftx) ,: {.DOsize
NB. Put this locale last so it will contain the result
yxhw =. yxhw , thisyxhw
locs =. locs , coname''

NB. If there is a right input, append it,adjusted up to the top of the left, and with movement suppressed
if. #rightlocs =. (parentlocales -. toplocs) (e. # [)~ a: -.~ 0 {"1 displayrightinputs do.
  assert. 1 = #rightlocs
  rl =. {. rightlocs
  NB.?lintonly rl =. <'dissectobj'
  'rlocs ryxhw' =. assemblelayout__rl''
  NB. Calculate the minimum right movement of the right operand so as to leave its output to the right
  NB. of the right edge f the overall result.  The join routine adds MINBOXSPACING to the calculated distance,
  NB. so we make sure the output has a grid of space between the output and the input position
  minrmove =. ((ROUTINGGRIDSIZE-MINBOXSPACING) + +/ 1 {"1 thisyxhw) - (-: (<_1 1 1) { ryxhw)
  NB. Connect the right inputs, adjusted up to the top of the left, and with movement suppressed
  NB. Make sure we keep the left side, containing the final output, last
  yxhw =. ; |. ((<. 0.8 * (<0 0) { DOsize),0,minrmove)&joinlayoutslr yxhw ,&< ryxhw
  locs =. rlocs , locs
end.
NB. If the new block was off the left, move everything back to the right
if. 0 > hplace =. (<0 1) { thisyxhw do.
  yxhw =. yxhw -"2 (2 _2) {. <.&.(%&ROUTINGGRIDSIZE) hplace
end.
locs ,&< yxhw
)

NB. ********** draw DOs

cocurrent 'dissect'

NB. x is pen color,width[,style]
NB. y is table of yx,:yx
drawline =: 4 : 0
if. DEBGRAF do.
  'Lines: color=%j, width=%j, style=%j, xywh=%j' PR (3{.x);(3{x);(4}.x); }: ; '((%j,%j)-(%j,%j)),' VB ,"2 |."1 y
end.
glrgb 3 {. x
glpen 2 {.!.PS_SOLID 3 }. x
gllines ,"2 |."1 y
0 0$0
)

NB. x is color,width[,pen] either a list or a table for each line
NB. y is (list of starting y);(list of starting x),:(x start/end positions for y lines);(y start/endpositions for x lines)
drawmesh =: 4 : 0
x drawline"1 2 (, |."1)&>/ ,."0 1&.>/ y
)

STIPWIDTH =: 10
NB. x is interior color;(pen color,width).  If pen is omitted, null is used
NB. if color is empty, use null brush
NB. Color may be RGBA, where A is the stipple pattern: 0=none, 1=downleft, 2=downright, 3=both
NB. y is yx,:hw of rectangles to draw with that color
drawrect =: 4 : 0
if. 0 e. $y do. return. end.
irgb =. 3 {. ic =. > {. x
if. DEBGRAF do.
  'Rectangles: color=%j, pencolor=%j, xywh=%j' PR (2{.x), < }: ; '((%j,%j)-(%j,%j)),' VB ,"2 |."1 y
end.
if. 1 < #x do.
  NB. Pen given, with width. Adjust the box to move the border to be inside the box.  Qt draws the border centered outside the box
  y =. y +"2 (1 _2) * <. -: (1;3) {:: x
  glrgb (1;<<0 1 2) {:: x
  glpen 2 {.!.PS_SOLID (1;<<<0 1 2) {:: x
else.
NB. No color, no pen
  (([: glpen (0,PS_NULL)"_) [ glrgb) irgb
end.
if. #ic do.
  glrgb irgb
  glbrush ''
else. glbrushnull''
end.
y =. <. y + 0.5  NB. force coordinates to integral
glrect 0 0 1 1 +"1^:(-.IFQT) ,"2 |."1 y
NB. If stippling called for, do it
if. 3 < #ic do.
  tlbr =. ,"2 +/\"2 y
  (([: glpen (1,PS_SOLID)"_) [ glrgb) 0 0 0
  if. 1 bwand stiptype =. 3 { ic do.
NB. downleft stippling requested: get the list of segments in the rect
    startsegno =. >.(%&STIPWIDTH) +/"1 (0 1) {"2 tlbr   NB. tl
    endsegno =. <. (%&STIPWIDTH) +/"1 (2 3) {"2 tlbr   NB. br
    segofsts =. startsegno (STIPWIDTH * [ + i.@>:@-~)&.> endsegno  NB. y-intercept of segment
    bl =. segofsts (- ,. ])&.> 1 {"1 tlbr   NB. find intersection with left edge, producing (left,bottom)
    bl =. bl ([ + [: (,. -) 0 <. (- {."1)~)&.> 2 {"1 tlbr   NB. find (negative) amount bottom is below rectangle bottom, and transfer that to left (add y, sub x)
    tr =. segofsts (- ,. ])&.> 3 {"1 tlbr   NB. find intersection with right edge, producing (right,top)
    tr =. tr ([ + [: (,. -) 0 >. (- {."1)~)&.> 0 {"1 tlbr   NB. find (negative) amount top is below rectangle top, and transfer that to right (sub from y, add to x)
NB. stipple in black
    gllines 1 0 3 2 {"1 ; bl ,.&.> tr
  end.
  if. 2 bwand stiptype =. 3 { ic do.
NB. downright stippling requested
    startsegno =. >.(%&STIPWIDTH) -/"1 (0 3) {"2 tlbr   NB. tr
    endsegno =. <. (%&STIPWIDTH) -/"1 (2 1) {"2 tlbr   NB. bl
    segofsts =. startsegno (STIPWIDTH * [ + i.@>:@-~)&.> endsegno  NB. y-intercept of segment
    tl =. segofsts (+ ,. ])&.> 1 {"1 tlbr   NB. find intersection with left edge, producing (left,top)
    tl =. tl ([ +"1 0 (0) >. (- {."1)~)&.> 0 {"1 tlbr   NB. find amount top is above rectangle top, and transfer that to left (add y, add x)
    br =. segofsts (+ ,. ])&.> 3 {"1 tlbr   NB. find intersection with right edge, producing (right,bottom)
    br =. br ([ + [: (,. -) 0 >. (- {."1)~)&.> 0 {"1 tlbr   NB. find (negative) amount bottom is below rectangle bottom, and transfer that to right (sub from y, add to x)
NB. stipple in black
    gllines 1 0 3 2 {"1 ; tl ,.&.> br
  end.
end.
0 0$0
)

NB. x is (background color[;pen color,width]);text color;text font;text size;yx margin around text (scalar or yx or 2 2 $ tlbr);(split LF-delimited texts into lines (kludge))
NB. y is text;yx,:hw of box
NB. Draw the rectangle, then draw the text
NB. Result is an empty list
drawtext =: 4 : 0"1
'vc tc tf ts mg' =. 5 {. x
NB. Draw the rectangles
(boxopen vc) drawrect > 1 {"1 y
if. DEBGRAF do.
  'Text: colors=%j/%j, font=%j%j, xy=(%j,%j), text=%j' PR vc;tc;tf;ts; (<"0 |. (2 ($,) mg) + {. 1 {:: y) , (0 { y)
end.
NB. Select font & color
glrgb tc
gltextcolor''
glfont tf , ": ts
NB. Draw the strings, offset by the margin.
if. 5 < #x do.
  if. #s =. 0 {:: y do.
    stgs =. <;._2 LF ,~^:(~: {:) s
    ystarts =. ((2 ($,) mg) + (1;0) {:: y) +"1 (0) ,.~ +/\ |.!.0 >: {:"1 glqextent@> stgs
    stgs (gltext@>@[   gltextxy@|.)"0 1 ystarts
  end.
else.
  (gltext@[   [: gltextxy@|. (2 ($,) mg) + {.)&>/ y
end.
''
)

NB. same parms as drawtext, except for the text boxsize
NB. Result is the hw of the box needed
sizetext =: 4 : 0"1
'vc tc tf ts mg' =. 5 {. x
if. DEBGRAF do.
  'Sizetext: colors=%j/%j, font=%j%j, text=%j' PR vc;tc;tf;ts; (0 { y)
end.
glfontextent tf , ": ts
if. 5 < #x do.
  NB. String contains LFs.  Treat the LF as newline, get all sizes, make bbox with one pixel between rows
  bbox =. (>./@:({."1) , +/&.:>:@:({:"1)) glqextent;._2 LF ,~^:(~: {:) >{.y
else.
  bbox =. glqextent >{.y
end.
NB. Add margins all around, return in hw format
(+/ 2 2 ($,) mg) + |. bbox
)

cocurrent 'dissectobj'

EXPLORERYX =: 0 0   NB. The place on the explorer isigraph to start the display
NB. Draw the main object, and also the explorer if it is active
NB. If x is given, it selects the view(s) to draw
drawDOvnall =: 3 : 0
a: drawDOvnall y
:
if. #y do. DOyx =: (#DOsize) {. y ,: EXPLORERYX end.
drawDOvn"1 x { (>:*#winhwnd) {. ((#DOsize) {. (winhwnd__COCREATOR;winhwnd),.(0;1),.(1<#DOsize);0) ,. |: <"_1@> DOyx;DOsize;DOlabelpos;DOshapepos;DOstatuspos;DOdatapos;DOassignpos;displayscrollbars;pickrects;scrollpoints
)

NB. y is info for the surface we are drawing (position, size, etc)
NB. side-effect is graphics ops to draw the DO
NB. DOcfm is color/font/margin info:
NB.  (info for label);(info for shape);(info for status line);(info for data)
NB.  Each info is:
NB.    color;text color;font;fontsize;margin
NB.  for label and status line, we have just a list
NB.  for shape and data, we have a table which we will index by selector.
NB. Result is pick window (yx,:hw) for the DO
drawDOvn =: 3 : 0
NB. These local variables cover the global names inside this routine (kludge).  hwindex is the window number we are displaying
'hwnd hwindex explorable DOyx DOsize DOlabelpos DOshapepos DOstatuspos DOdatapos DOassignpos displayscrollbars pickrects scrollpoint' =. y
wd 'psel ' , hwnd
glsel 'dissectisi'
'cfmlabel cfmshape cfmstatus cfmimsgs' =. DOcfm
SM^:DEBDOL 'drawDOvn: ' , > coname''
NB. Save the position of the object, and as a 2x2
actyx2 =. 0 ,:~ DOyx
NB. Set clipping box to the interior of this object
glclipreset''
glclip 0 0 1 1 + , |."1 DOyx ,: DOsize

NB. See which elements are present
pickactx =. I. picknames ~: <'DOresizepos'  NB. indexes of rects to draw.  Don't include resize handle because it doesn't go to full width
'labelpresent shapepresent statuspresent datapresent assignpresent' =. (i. #FRINGECOLOR) e. presentx =. (;: 'DOlabelpos DOshapepos DOstatuspos DOdatapos DOassignpos') i. pickactx { picknames
NB. Draw covering rectangles for each component - filling out to the full width of the box.  Don't include the resize handle if any
if. #presentx do.
  (presentx { (0 { DOcfmstatus) 2 4} FRINGECOLOR) drawrect"0 2 actyx2 +"2  ((<pickactx;a:;0) { pickrects) ,."1 (0) 0} DOsize
end.
NB. Draw the verb/name string, if any
if. labelpresent do.
NB. If DOranks is a string, it contains ranks with no labels.  It is the only thing
NB. in the box; draw it there
  if. 2 = 3!:0 DOranks do.
    cfmlabel drawtext DOranks;actyx2 + (<0 0) {:: DOlabelpos
  else.
NB. DOranks contains [rank],string,rank.  Draw them all, except rects that contain just a space.
NB. They were added to get the centering right when the rank stack contains a mix of monads and dyads, but
NB. should not actually display	
    (,&.:(<"1)DOrankcfm) drawtext`(''"_)@.((<' ') -: {.@])"1 (,DOranks) ,"0 <"2 actyx2 +"2 ]S:0 DOlabelpos
  end.
end.

NB. draw the shape/selection line, if any
if. shapepresent do.
NB. Draw the shapes/selections.  Start at the selection level of this object
NB. get the text,position for the shapes/selections, which are a rank-2 array
  shapeseltext =. DOshapes ,"0 actyx2&+&.> |: (,"3) 0 _1 |: > DOshapepos
NB. draw frame/selections, which are all but the last column
  (}: DOshapecfm) drawtext"2^:(*@#@[) }:"2 shapeseltext
NB. Draw the result-cell shape, the last column of the first row
  ({: DOshapecfm) drawtext (<0 _1) { shapeseltext
end.

NB. draw the status/assignment string, if any
if. statuspresent do.
  DOcfmstatus drawtext DOstatusstring;actyx2 + DOstatuspos
  NB. If we put out an error message, remember that fact so we keep stealth disabled
  if. '(' ~: {.!.'(' DOstatusstring do. needtocheckerrordisplayed__COCREATOR =: 0 end. 
elseif. assignpresent do.
  DOcfmstatus drawtext DOstatusstring;actyx2 + DOassignpos
end.


NB. Draw the data - if the node has data to display.  If not, we never created a DOL, so don't draw
if. datapresent do.
  assert. 0 = 4!:0 <'fillmask'  [ 'drawDOvn'
  
  NB. Calculate the cliprect for the data portion, as tlhw
  cliptlhw =: (DOyx,:0) + DOdatapos   NB. startpos + tlhw rect
NB. Convert cliprect to tlbr form, and calculate the starting (y,x), which is the window position, plus boxing margin if
NB. the data is boxed, but all backed up by the scroll offset
  boxyx =. BOXMARGIN +^:(3<#valueformat) ({. cliptlbr =. +/\ cliptlhw) - scrollpoint
NB. Reduce the cliprect to the data window (including scrollbars, which are drawn last).
  glclipreset''
  glclip 0 0 1 1 + , |."1 -~/\ cliptlbr
NB. We must always extend the data to match the frame, so that we show the full operand in case there were
NB. unexecuted cells.  If the fill atom is nonnull, it means that the result is collectable, and we collect it.  If
NB. not, we have to show the boxed atoms.
  dispvalue =. fillmask frameselresult selresult
NB. position the start point so that the selected scroll data starts in the window.
NB. y here is tlbr of the clip window;screen startpoint of the data
NB. If the data is boxed, insert the box margin
  (valueformat;dispvalue;<fillmask) drawDOL cliptlbr ; boxyx

NB. After the data is drawn, draw a highlighting rectangle for the item selection(s), if any.
NB. We take all the selectors there are, up the length of the frame of this level; but if there aren't enough, we don't highlight.  Discard
NB. selectors for higher levels.  The only way we can get more selectors than frame is during sniff, where the lower
NB. selection is propagated up automatically
  QP^:DEBHLIGHT'drawDO:defstring=?defstring]0 opselin '
  hlights =. (SELECTIONBORDERSTYLE ;< ~. hlightforselection inheritroot) ,: HIGHLIGHTBORDERSTYLE ;< ~. hlightforoperands opselin
  QP^:DEBHLIGHT'hlights '
NB. Draw accumulated highlight rects
NB. Convert from style;(level;rect) to style;level;rect
  hlights =. ; <@({. ,. >@{:)"1 hlights
  if. # hlights do.
    QP^:DEBHLIGHT2'drawhighlights:defstring=?defstring]0 hlights valueformat  valueformat(hlighttotlbr)2&{"1]hlights '
    mesh =. ((boxyx + 2 2 $ 1 _1 1 _1) +"2 |:"2) valueformat  hlighttotlbr 2&{"1 hlights
    if. +./ meshvalid =. INVALIDRECT -.@:-:"2 mesh do. NB. create top,bottom,:left,right, adjust for rectangle origin
NB. Expand the cliprect to allow for the width of the highlight, which is centered on the edge of the rectangle
NB. and therefore projects outside
      glclipreset''
      glclip (((>. -: HIGHLIGHTLINEWIDTH) * _1 _1 2 2) + 0 0 1 1) + , |."1  -~/\ cliptlbr
      
NB. Expand to size of axisshapes, split into y and x axes
NB. Create delta-y and delta-x
NB. create indexes of ymin ymax ,: xmin xmax
NB. Convert to pixel numbers
NB. Adjust for rectangle origin
NB. Create (ystart,yend);(xstart,xend),:(xstart,xend);(ystart,yend)
NB. Draw mesh
NB. Install the highlight color for the rects into the border
      hlightstyles =. (HIGHLIGHTCOLORS {~ 1&{"1 hlights) (<a:;0 1 2)} > 0&{"1 hlights
      hlightstyles (drawmesh    [: (,: |.) ;/)"1 2&(meshvalid&#) mesh
NB. y is (list of starting y);(list of starting x),:(x start/end positions for y lines);(y start/endpositions for x lines)
NB. Restore cliprect to just the data area
      glclipreset''
      glclip 0 0 1 1 + , |."1 -~/\ cliptlbr
    end.
  end.
  SM^:DEBHLIGHT'finished drawing hlights'
  NB. If there are scrollbars, draw them
  if. +./ displayscrollbars do.
    't l b r' =. , DOyx +"1 +/\ DOdatapos  NB. t l b r of region
    'sh sw' =. (2 * SCROLLBARENDTHICKNESSPIXELS) -~ 'h w' =. ({: DOdatapos) - SCROLLBARWIDTHPIXELS * |. displayscrollbars  NB. actual data h/w
    
    datahw =. extractDOLsize valueformat
    QP^:DEBDOL 'displayscrollbars DOyx DOdatapos datahw t l b r h w scrollpoint sw sh '
    scrolltravv =. scrolltravh =. 0 0
    NB. draw horizontal scroll
    if. 1 { displayscrollbars do.
      NB. Draw the scrollbar itself
      SCROLLBARCOLOR drawrect (vpos =. -/\. b , SCROLLBARWIDTHPIXELS) ,. (l , w)
      NB. Draw the endcaps
      SCROLLBARENDCOLOR drawrect vpos ,."1 (l , SCROLLBARENDTHICKNESSPIXELS) ,: (-/\. (l+w) , SCROLLBARENDTHICKNESSPIXELS)
      NB. Calculate left & right scroll positions of the travelers, as pixel positions in the scrollbar
      scrolltravh =. <. sw * 0 >. 1 <. (+/\ (1 { scrollpoint) , w) % (1 { datahw)
      NB. Make sure the traveler has a minimum width so user can find it.
      NB. Distribute the added width toward the center of the region: negative for left, positive for right
      leeway =. (% -~/) (0,sw) - scrolltravh
      NB. Add needed thickness, and adjust right to account for the leading endcap
      scrolltravh =. scrolltravh + SCROLLBARENDTHICKNESSPIXELS + <. leeway * 0 >. SCROLLBARMINWIDTHPIXELS - (-~/ scrolltravh)
      SCROLLBARTRAVELERCOLOR drawrect vpos ,. -~/\ l + scrolltravh
    end.
    NB. vertical
    if. 0 { displayscrollbars do.
      SCROLLBARCOLOR drawrect (hpos =. -/\. r , SCROLLBARWIDTHPIXELS) ,.~ (t , h)
      SCROLLBARENDCOLOR drawrect hpos ,.~"1 (t , SCROLLBARENDTHICKNESSPIXELS) ,: (-/\. (t+h) , SCROLLBARENDTHICKNESSPIXELS)
      scrolltravv =. <. sh * 0 >. 1 <. (+/\ (0 { scrollpoint) , h) % (0 { datahw)
      leeway =. (% -~/) (0,sh) - scrolltravv
      scrolltravv =. scrolltravv + SCROLLBARENDTHICKNESSPIXELS + <. leeway * 0 >. SCROLLBARMINWIDTHPIXELS - (-~/ scrolltravv)
      SCROLLBARTRAVELERCOLOR drawrect hpos ,.~ -~/\ t + scrolltravv
    end.
    NB. If both scrollbars are drawn, the lower-right area, which was filled by data,
    NB. still has data, which is distracting.  Black it out
    if. 1 1 -: displayscrollbars do.
      SCROLLBARCORNERCOLOR drawrect -/\. (b,r) ,: SCROLLBARWIDTHPIXELS
    end.
    scrolltravelers =: (scrolltravv ,: scrolltravh) hwindex} scrolltravelers
    QP^:DEBDOL 'scrolltravelers '
  end.
  
  NB. Save some status info for tooltips
  'DOdataisboxed DOdatashape' =: ((32 = 3!:0) ; $) dispvalue
else.
  'DOdataisboxed DOdatashape' =: 0 0
end.


NB. *** reset clip rect ***
glclipreset''

NB. Draw rectangles (border only) for each component - filling out to the full width of the box
if. #presentx do.
  ((<'') ,. presentx { FRINGEBORDER) drawrect"1 2 actyx2 +"2  ((<pickactx;a:;0) { pickrects) ,."1 (0) 0} DOsize
end.

NB. Draw a hollow rectangle for the object, just to get the border line.  Color is used to show (no data,NA,all drawn,explorable)
('';((#.datapresent,explorable) { DOBORDERCOLORS),1) drawrect DOyx ,: DOsize

NB. If there is a resize handle, draw it as a solid rect - only in the main display
if. #resizeh =. (<<<pickactx) { pickrects do.
  if. 1 ~: hwindex do.
    RESIZEHANDLECOLOR drawrect actyx2 + {. resizeh
  end.
end.

NB. If we drew to the explorer, we must paint it
if. 1 = hwindex do. glpaint'' end.

NB.?lintsaveglobals
)

NB. x is text-color info, a la cfmdata
NB. y is fillmask codes
NB. result is the value to use for drawtext, with stippling added to the rect color
rectcolorfromfillmask =: (<:FILLMASKNOCOLLECT)&bwand@] ,~&.> ({~      [: < 0 ;~ (- <. 2 ^. FILLMASKCHECKER)&bwlsl)

NB. y is fillmask code
NB. result is 1 if the fillmask is data or plain fill; 0 if error or unexecd
fillmaskisvaliddata =: FILLMASKUNEXECD ~: FILLMASKUNEXECD&bwand

NB. x is (mask of y-onscreen);(mask of (x-onscreen)
NB. y is array of values/masks/fill etc
NB. Result is the visible values
scissortoscreen =: (1&{::@[ #"1 _1 (0)&{::@[ # ])

NB. Draw the graphics for a noun's DOL
NB. x is DOL;values;selection   selection is replicated if needed
NB. y is (tl;:br of drawable region);yx of topleft corner or data, including scroll offset
NB. We execute the gl2 operations to draw the DOL
NB.
NB. For selection nodes, selection and boxmesh are boxed.  In this case we suppress the action of
NB. selection (which fills in rectangles) and force the action of boxmesh (which draws rectangle boundaries).
NB. We then pass the contents of the boxes to recursion, which will open them and use them
drawDOL =: 4 : 0"1
'vf data sel' =. x
'shapeused ysizes xsizes' =. 3 {. vf
'cliptlbr dataorigin' =. y
SM^:DEBDOL 'drawDOL: ' , > coname''
QP^:DEBDOL'vf data sel xsizes ysizes '
NB. If the data is empty, draw nothing (but signal validity).  The size
NB. of the empty was accounted for when the block was created
  
NB.  If there are subDOLs, adjust the rects to leave a box margin
boxyx =. dataorigin
if. DEBOBJ do.
  'DOL: xy=(%j,%j) xsizes=%j ysizes=%j cliptlbr=%j' PR (<"0 |. y),xsizes;ysizes;cliptlbr
end.
if. emptydata =. 0 e. $ usedd =. data do.
  NB. The noun is empty.  Display it as empties, with shape up to the first 0 in the nounshape
  sel =. 0
  shapeused =. truncemptyshape shapeused
  usedd =. (truncemptyshape $ usedd) $ 0
end.
NB. Get the y and x endpoint lists, prepend a zero to give the start of the first cell,  and then
NB. adjust for the starting position of the object
yxpositions =. boxyx (+ 0&,)&.> 1 2 { vf
NB. The shape of the array of rectangles.  We will shape the user's data and the fillmask into this shape
flatshape =. <: #@> yxpositions   NB. <: to remove the leading 0
NB. Convert the array, of whatever rank, to a table
NB. axes is 2 boxes, giving the axis numbers that are assigned to vertical and horizontal.  We assign
NB. axes alternately, starting from the right, with the last axis always going to x
axes =. (i. ((#~ -.) ; #~) [: |. $&1 0)@#@$ usedd  NB. 1;0 2   or 0 2;1 3
NB. axisshapes is the lengths of each axis assigned to y/x.  sizes is the total size of y/x
sizes =. */@> axisshapes =. axes ({&.:>"0 _ $) usedd
NB. reshape the data into a table, regardless of original rank
usedd =. sizes ($,) (;axes) |: usedd
NB. Calculate the mask of rows/columns that fit on the screen
NB. Get start/end+1 of box; a rect is OK if its left nbr end+1 is below the window end+1, AND
NB. its end+1 is above the window start.
NB. This version has shape that matches the rectangles to be displayed
onscreenmsk =. yxpositions ((}:@[ < {:@]) *. (}.@[ > {.@]))&.> <"1 |: cliptlbr
NB. To get rectangle extents, you need the onscreenmsk extended to include the end of the rightmost offscreen
NB. rect, which gives the left end of the leftmost oncreen rect.   This is used to select from an endpoint
NB. vector that has been extended by adding a leftmost 0, and has shape that matches the boundaries to be displayed
onscreenmskext =. (+. 1&(|.!.0))@(0&,)&.> onscreenmsk
NB. Get the set of onscreen points.  If there aren't any, skip the drawing (to avoid errors)
if. -. a: e. onscreenbdys =. onscreenmskext #&.> yxpositions do.
  NB. Create the rectangles for each atom.  This will be mxnx2x2.
  rects =. ,."1/&(}: ,. 2&(-~/\))&>/ onscreenbdys
  NB. Cut the data down to the displayable part
  usedd =. onscreenmsk scissortoscreen usedd
  NB. Extract and reshape the selection information, too.  sel should either be an atom or have
  NB. one atom per data cell.  The data may be truncated, though, so we bring sel up to the
  NB. rank of the shapeused, and then truncate it to shapeused size (using sel as a fill, in case sel
  NB. was an atom).  Then shape to 2D, and trim to the displayable part
  NB. But if this is a selection node, suppress rectangles, force lines, leave rectangles for next level
  if. sel -:&$ usel =. ''"_`>@.(2>L.) sel do.
    sel =. onscreenmsk scissortoscreen sizes ($,) (;axes) |: shapeused {.!.({.,sel) ((-$shapeused) {.!.1 $sel) ($,) sel
  NB. Before filling the cells the first time, initialize the rectangles to the colors given by the fillmask.  This
  NB. is to give the right color to cells that are not drawn at all (empty contents) or whose contents do not fill
  NB. the cell, because of other larger values.
    (cfmdata rectcolorfromfillmask (<:#cfmdata) colorlimitsellevel >sel) drawrect"0 2 rects
  else.
    NB. Selector node
    NB. usedd has been converted to a table - do the same for sel
    sel =. onscreenmsk scissortoscreen sizes ($,) (;axes) |:  sel
  end.

  NB. If there are subDOLs, process each of them.  The operand was boxed.
  if. 3 < #vf do.
    sdol =. onscreenmsk scissortoscreen flatshape ($,) (;axes) |: shapeused {. 3 {:: vf
    NB.  Adjust each inner box position
    (sdol ,"0 1 usedd ,"0 (_1-FILLMASKNOCOLLECT)&bwand^:(0=L.)&.> sel)   drawDOL   cliptlbr ;"2 1 (BOXLINEWIDTH + BOXMARGIN) +"1 {."2 rects
    NB. Draw mesh for the rectangles - dotted if the boxing is because of collection error
    collecterr =. +./@:, 0:`(0~:FILLMASKNOCOLLECT&bwand)@.(0=L.)@> sel
    (BOXBORDERCOLOR,1,collecterr # PS_DOT) drawmesh (,:   [: |. 0 _1&{&.>) onscreenbdys
  else.
    NB. Not boxed data; draw each cell.  If the cell is error/unexecd, delete the text, since the cell
    NB. doesn't really have a value.  We leave its space as a reminder of how big it might have been
    NB. Install checkboard, so it shows up at all levels
     sel =. (FILLMASKEXTRAAXES * 1 = {. $ data) + (<:#cfmdata) checkerboardfillmask sel
     if. emptydata do.
       (emptycfm rectcolorfromfillmask (<:#emptycfm) colorlimitsellevel sel) drawrect"0 2 rects
     else.
       ((2 131072 e.~ 3!:0 usedd) textinfofromtypefillmask sel) drawtext"1 ((fillmaskisvaliddata sel) dataxlate@(# ":!.displayprecision)&.> usedd) (,<)"0 2 rects
     end.
  end.
  
  NB. Draw borders at any boundary (except the first) where a rank rolls over.  The width of the line
  NB. is the number of ranks that rolled over simultaneously.  We see which rectangles start
  NB. on a new boundary, and use the start position to get the line
  NB. Get number of boundaries for each row/col: 0=not a bdy, 1=rank-2 bdy, etc
  NB. Add 1 pixel of width to nonzero boundaries
   bdynos =. axisshapes (+ *)@}.@:(0&(i.&1@:~:)@|."1)@(#: i.)&.> flatshape
  NB. Create table of startpoint,width for each line.  Discard first point of bdynos (always a big
  NB. value for the first cell) and the last point of sizes (gives position of the last cell).  We
  NB. are left with internal boundaries.  Back up the position by the width of the boundary, and discard
  NB. zero boundaries
  if. +/ #@> startwidth =. bdynos (*@[ # ,.~)&.> (<<<0 _1)&{&.> yxpositions do.
    (RANKCOLOR ,"1 PS_SOLID ,.~ {:"1 ; startwidth) drawmesh ({."1&.> startwidth) ,: |.   0 _1&{&.> yxpositions
  end.
end.
0
)

cocurrent 'dissect'

NB. ****************** drawing the placement **********************
NB. called in the locale of the form

NB. Size the placed layout, and convert wires to lines;arcs
NB. y is locales;table of yx (tlc of each block);wires as boxed list, each containing table of y x y x for wires
NB. x is the scroll amount (starting position in screen space of top-left corner of the drawing)
NB. result is size reqd yx;locales;yx;(lines as n 2 2 yx start,:end);(arcs as n 2 2 yx center,:corner)
NB. side effects: pick info created
NB. globals used: topinfo (used to find the link pickrects)
sizeplacement =: 4 : 0
tlc =. x
'dos dotl wires' =. 3 {. y
NB. Apply scroll offset
dotl =. dotl +"1 tlc
wires =. +"1&(tlc,tlc)&.> wires 
NB. Initialize pick information.  For speed, there are two arrays: locpickrects, which is a brick of
NB. yxhw for each object, and picklocs, which is a list of locales, one per pickrect.
NB. Each displayed block is associated with a unique locale.  Links are associated with the form locale.
picklocs =: ((#topinfo) # coname'') , dos
NB. Put in placeholders for the sentence brects.  These will be filled in when we
NB. size the drawing
locpickrects =: (((#topinfo),2 2)$0) , dotl ,:"1 ({."2) 3 : 'DOpicksize__y'"0 dos
NB. Get the max size drawn, and set the control to just big enough to hold it
maxsize =. >./ (+/"2 locpickrects) , 2 2 $ >./ ; wires
maxsize;dos;dotl;<wires
NB.?lintsaveglobals
)

NB. Draw the placed layout
NB. y is locales;yx;(lines as n,4 yx start,end)
drawplacement =: 3 : 0
glclear''
'dos yx wires' =. y
if. DEBOBJ do. QP 'DOL?y ' end.
NB. draw the objects
for_d. dos do.  NB.?lintonly d =. <'dissectobj'
  drawDO__d d_index{yx
end.
NB. draw wires.  reset to select graphics window
wd 'psel ',winhwnd
glsel 'dissectisi'
glclipreset''
glrgb WIRECOLOR
glpen 1,PS_SOLID
if. #wires do.
  gllines 1 0 3 2 {"1 ; wires
end.
NB. Set up information for wire highlighting
wirehighwires =: wires
NB. For each net, the mouse-movement required before the net is revisited
wirehighdisttocross =: ($wires) # 0
NB. (disttocheck;odosincecheck)
NB. disttocheck is the minimum in wirehighdisttocross; we don't need to evaluate anything until going this far
NB. odosincecheck is the negative of the total distance we have traveled since last check
wirehighdisttocheck =: _1 0
NB. The last mouse position evaluated
wirehighmousepos =: _1000 _1000

NB. Show the sentence, with the user's spacing, highlighting according to selection level
locpickrects drawsentence&(2&{.) topinfo
NB. Show the links if any
if. #linkinfo =. 2 }. topinfo do. 
  ((2 + i. #linkinfo) { locpickrects) drawlinks linkinfo
end.
NB.?lintsaveglobals
)

NB. ****************** exegesis for tooltips ****************************
cocurrent 'dissectobj'
NB. Explain the current line of the rank stack.  Called after frame has been formatted; y is the result from exegesisframe
NB. Called in the locale of the node that produced the rank-stack entry.  This depends on each node; here we have the default do-nothing case
NB. Result is a table of retcode;text from previous formatting
exegesisrankstack =: (0 2$a:)"_

NB. Produce an explanation of the block containing this rank stack.  All participants get their chance to contribute,
NB. and the results are put at the top of the tooltip.
NB. x is 1 if this locale appears more than once in this rank stack
NB. y is the titlestring in the line for this locale
NB. Result is a table of retcode;text
exegesisrankoverall =: (0 2$a:)"_

NB. Nilad.  Result is the string to use as the lead for describing the result of the executed verb
exegesisverbdesc =: 3 : 0
,: EXEGESISDATASOURCE ; 'The data shown is the result of the verb:',LF,(defstring 0),CR
)

NB. Get the locale of the verb containing the collection point
NB. y is the locale of the child of this node
NB. result is the locale whose verb will collect the results
NB. For everything except @ & etc., the verb itself runs at infinite rank & collects
findcollectionpoint =: ]
NB. If we are in the v of u@v etc, go up to its parent to indicate that we collect after u
NB. u@:v will never go through here, since it never has frame
findcollectionpoint_dissectextendv =: 3 : 0
NB.?lintonly vop =. parent =: <'dissectobj'
if. vop -: y do. findcollectionpoint__parent coname''
else. y
end.
NB.?lintsaveglobals
)


NB. y is a string, result is y prefixed with a or an
exegesisindefinite =: (,~   'a' , 'n ' }.~ 'aeiou8' -.@e.~ {.)

NB. x is Boolean, y is a string, result is string with s possibly added
exegesisplural =: ('s',~])^:(*@[)

NB. y is a list, like 2 3 4, result is '2x3x4'
exegesisshapewithx =: }.@;@:(('x' , ":)&.>)

NB. y is shape/frame, result is '2x3x... array of' or the like
exegesisshapex =: 3 : 0
select. #y
case. 0 do.
  ''
case. 1 do.
  'list of ' , ":y
case. do.
  (exegesisshapewithx y) , ' array of'
end.
)

NB. y is (shape of an operand),(frame),(1 if this cell contains others which should be called items),(1 if this cell is a subcell of others & should be called a subarray)
NB. result is string describing the cell; singular if frame describes < 2 cells
NB. If x is given, it overrides the singular/plural specification
exegesisfmtcell =: 3 : 0
(1 < */ 1 {:: y) exegesisfmtcell y
:
'shape frame hasitems isitem' =. y
cellwd =. hasitems{::'atom';'item'
subwd =. isitem{::' array';' subarray'
NB. Convert from operand shape to cell shape
shape =. (#frame) }. shape
x exegesisplural (4 <. #shape) {:: (cellwd;((":shape) , '-',cellwd,' list')) , (exegesisshapewithx shape)&,&.> ' table';' brick';subwd
)

NB. y is (shape of a cell),(frame), result is string describing the frame; suitable for being follwed by a cell format, thus
NB. ending with 'of' or a number.  Ends with a space.  Always an indefinite article is appended, except when this starts with a number
exegesisfmtframe =: 3 : 0
'shape frame' =. y
(4 <. #frame) {:: ((exegesisindefinite 'single ');((":frame) , ' ')) , exegesisindefinite@((exegesisshapewithx frame)&,)&.> ' table of ';' brick of ';' subarray of '
)

NB. These are the morphemes we use, in the order they should appear in the final result.
NB. Some may be extended with selection levels when they are created.
NB. The number is the detail level at which the value is displayed
exegesismorphemes =. 3&{.@;:;._2 (0 : 0)
EXEGESISTUTORIAL tutorial Tutorial
EXEGESISRANKOVERALLNODISP laconic Description
EXEGESISRANKOVERALLCOMPEND laconic Description
EXEGESISRANKOVERALLNOOPS laconic Description
EXEGESISRANKOVERALLEXPLAIN laconic Description
EXEGESISRANKSTACKEXPLAIN laconic Description
EXEGESISRANKSTACKPOWERSTART laconic Description
EXEGESISRANKSTACKPARTITIONSTART laconic Description
EXEGESISONELINEDESC laconic Verb
EXEGESISVERBDESC laconic Verb
EXEGESISVERBRANK verbose Verb
EXEGESISVERBRUNDEBUG verbose Verb
EXEGESISFRAMEFILLSTART verbose Frame
EXEGESISFRAMENUGATORY verbose Frame
EXEGESISFRAMENONNOUN verbose Frame
EXEGESISFRAMENOSHAPE verbose Frame
EXEGESISFRAMEVALID verbose Frame
EXEGESISFRAMESURROGATE verbose Frame
EXEGESISFRAMENOFRAME verbose Frame
EXEGESISSHAPEHEADER tutorial Description
EXEGESISSHAPESELECTINGVERB laconic
EXEGESISSHAPEFRAME laconic
EXEGESISSHAPERESULT laconic
EXEGESISDATAHEADER tutorial Description
EXEGESISDATAVERBTUTORIAL tutorial
EXEGESISDATASOURCE verbose
EXEGESISDATASHAPE verbose
EXEGESISDATAPATH laconic
EXEGESISDATAARRANGEMENT verbose
EXEGESISDATAEXPLORABLE verbose
EXEGESISDATACLIPINFO verbose Clipboard
)
({."1 exegesismorphemes) =: i. # exegesismorphemes
exegesislevels =: (1 {"1 TOOLTIPDETAILCHOICES) i. 1 {"1 exegesismorphemes
exegesislabels =: 2 {"1 exegesismorphemes

NB. Instructions for pruning
NB.
NB. Type: set of tags;set of excluded tags.  Excluded tags are deleted if they appear after a tag in the set
NB. the filter is applied BEFORE sorting into grammatical order
tagsexcludebefore =: _2 ]\ (EXEGESISRANKOVERALLNODISP) ; (EXEGESISRANKOVERALLCOMPEND,EXEGESISRANKOVERALLNOOPS) ; (EXEGESISRANKOVERALLNOOPS) ; (EXEGESISRANKOVERALLNOOPS)
NB. Type: set of tags;set of excluded tags.  Excluded tags are deleted if they appear after a tag in the set
NB. the filter is applied AFTER sorting into grammatical order
tagsexcludeafter =: _2 ]\ (EXEGESISFRAMEFILLSTART,EXEGESISFRAMENUGATORY,EXEGESISFRAMENOSHAPE,EXEGESISFRAMENONNOUN,EXEGESISFRAMEVALID,EXEGESISFRAMESURROGATE) ; (EXEGESISFRAMENOFRAME)
tagsexcludeafter =: tagsexcludeafter , _2 ]\ (EXEGESISFRAMENOSHAPE) ; (EXEGESISFRAMENOSHAPE)

NB. Instructions for formatting
NB. Type: tag set A;tag set B    if an A is followed by a B, add a LF to the A
taginsertLF =: _2 ]\ (EXEGESISRANKOVERALLCOMPEND);(EXEGESISRANKOVERALLCOMPEND,EXEGESISRANKOVERALLNOOPS)

NB. y is string
NB. we look for %strt%...%end%; after finding it, we remove any matching strings from the remainder of the file,
NB. and recur on the remnant
remstrtend =: 3 : 0
if. 0 = #suff =. '%strt%' dropto y do. y return. end.
pref =. '%strt%' taketo y
stg =. '%end%' dropafter suff
rest =. remstrtend ('%end%' takeafter suff) rplc stg;''
pref,(_5 }. 6 }. stg),rest
)

NB. y is a table of morphemes; turn them into a displayable string
exegesisgrammar =: 3 : 0
tt =. y
NB. Cull the morphemes that are below the user's culling level
tt =. (tooltipdetailx >: exegesislevels {~ {.@(0&{::)"1 tt) # tt
NB. Cull excluded tags
tt =. tt #~ -. +./ ({.@(0&{::)"1 tt) ([: (*. |.!.0)~/ (e. >)"_ 0)"1 tagsexcludebefore
NB. Order them in grammatical order
tt =. tt /: > 0 {"1 tt
NB. Cull excluded tags
tt =. tt #~ -. +./ ({.@(0&{::)"1 tt) ([: (*. |.!.0)~/ (e. >)"_ 0)"1 tagsexcludeafter
NB. Insert LF as required
tt =. ({."1 tt) ,. ({:"1 tt) (, #&LF)&.>   +./ ({.@(0&{::)"1 tt) ([: (*.   1 |.!.0 ])/ (e. >)"_ 0)"1 taginsertLF
tags =.  {.@(0&{::)"1 tt
NB. Insert fences before each new nonnull topic
fencewords =. (] ((~:@] *. a: ~: [) #&.> ]) ((LF,'---') , ('---------------------',LF) ,~ ])&.>) tags { exegesislabels
NB. Run the result together, and delete all but the last LF, and any leading LF, and allow no more than 3 consecutive LF
runtext =. (#~   [: -. (LF,LF,LF)&E.) (}.~    LF i.&0@:= ]) ({.~ 2 + LF i:&0@:= ]) ; fencewords ,. 1 {"1 tt
NB. Remove duplicated strt,end pairs
runtext =. remstrtend runtext
NB. Delete the first %al1%, replace others by 'also'
runtext =. '%al1%' (taketo , takeafter) runtext
if. #sx =. '%al1%' ss runtext do. runtext =. 'also ' (,"0 sx +/ i. #'also ')} runtext end.
runtext
)

NB. Explain the frame of the verb.
NB. Called in the locale in which the operands and string form are defined
NB. y is (origlocale;opno) where origlocale is the locale of the original click, which might be
NB.  a smaller verb than where the operands were eventually found, and opno tells what happened
NB.  along the search for operands: 0,1 mean we went down one side of & (so the original click was
NB.  in a monad, but that monad fed into one side of a larger dyad; we will describe the dyad
NB.  and note which side the click was in); 2 means & was never encountered and the valence never changed
NB. Result is table of (retcode;LF-delimited string, empty if there is no frame)
NB.  retcode means: 0=non-verb, 1=no shapes, 2=no frame, 3=frame exists
exegesisframe =: 3 : 0
'labelloc opno datapresent' =. y
if. #vranks do.
  res =. ,: EXEGESISVERBRANK ; 'The rank of the verb is ',(":vranks),'.',LF
else.
  res =. 0 2$a:
end.
NB. If the source of the operands is not the original click, we'd better start with a line
NB. describing that fact
if. labelloc = coname'' do.
  vstring =. 'verb'   NB. refer to the verb this way
else.
  t =. 'The arguments to this verb vary from cell to cell.'
  if. #inputselopshapes do.
    t =. t , '  This verb is part of ',(opno {:: 'the x argument path of ';'the y argument path of ';''),'the compound verb:',LF,(defstring 0),CR,'and you can select a result from this verb below.'
  else.
    t =. t , '  Results will be displayed here when a single result cell has been selected.'
  end.
  vstring =. 'compound verb'
  res =. res , EXEGESISFRAMENOSHAPE;t,LF
end.

if. 0 = #vranks do.
  res =. ,: EXEGESISFRAMENONNOUN;''   NB. not a verb, don't try to explain anything
elseif. (0 = #inputselopshapes) +. (0 = #>selector) do.
  NB. We didn't select all the way to the end (either we ran out of selections of shapes or we switched to rank-calculus probes)
  NB. We can't describe the frame of the final verb - but there may be displayed results
  NB. Don't talk about uncertainties in frame if the verb has infinite rank
  if. _ +./@:~: vranks do.
    res =. res , EXEGESISFRAMENOSHAPE;(datapresent # 'This block shows multiple result-cells.  '),'You must select a single result to see the frame of this verb.',LF
  else.
    if. datapresent do.
      res =. res , EXEGESISFRAMENOSHAPE;'This verb has infinite rank, but multiple result-cells are shown here.  Select a single result-cell to see more detail.',LF
    else.
      res =. res , EXEGESISFRAMENOSHAPE;'This verb has infinite rank, but you must select a single result-cell to see more detail.',LF
    end.
  end.
elseif. _ *./@:= vranks do.
  NB. If verb has infinite rank, just say so here.  We don't use the frame because u/ etc has pseudoframe and we want to 
  NB. report NOFRAME for them
  res =. res , EXEGESISFRAMENOFRAME;'This verb has infinite rank and always applies to its entire argument',((2=#inputselopshapes)#'s'),'.',LF    NB. If only 1 cell, can't analyze
elseif.
shapes =. $^:(0<L.)&.> inputselopshapes
0 = #frame do.
  NB. The verb applied to its entire operand
  if. 2=#inputselopshapes do.
    ftext =. 'Each argument is a single cell, so there is a single result-cell.',LF    NB. If only 1 cell, can't analyze
    ftext =. ftext , 'x is ',(exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames),'.',LF
    ftext =. ftext , 'y is ',(exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(1&{) frames),'.',LF
   else.
    ftext =. 'The argument is a single cell, so there is a single result-cell.',LF    NB. If only 1 cell, can't analyze
    ftext =. ftext , 'The argument is ',(exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames),'.',LF
  end.
  res =. res , EXEGESISFRAMENOFRAME;ftext
elseif. do.
  NB. There is a frame.  Describe it
  NB. Set up some language about collection.  If the result is collected after this verb, say the result 'is assembled'.
  NB. But if the collection is remote, say 'will be assembled'
  if. (coname'') = cp =. findcollectionpoint__parent coname'' do.
    NB. collecting in the same place where we define the string: just use the description of that verb
    cvstring =. 'verb.',LF
    ctense =. 'is';'are'
  else.
    NB. collecting in a remote place.  indicate where
    NB.?lintonly cp =. <'dissectobj'
    cvstring =. 'containing compound verb:',LF,(defstring__cp 0),CR
    ctense =. 2#<'will be'
  end.
  select. */ frame
  case. 0 do.
    if. 1 = #vranks do.
      ftext =. 'The argument is empty, so ', (exegesisindefinite exegesisfmtcell (0;0) ,~ (0{::shapes);frame), ' of ' ,((0<#0{::shapes) exegesisplural 'fill') , ' is supplied to the ',vstring,'.',LF
    else.
      if. 1 1 -: emptyop =. 0&e.@> frames do.
        ftext =. 'The arguments are empty, so cells of fill (on the left, ' , (exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames) , '; on the right, ' , (exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(1&{) frames) , ') are supplied to the ',vstring,'.',LF
      else.
        'ename nonename' =. 'xy' \: emptyop
        ftext =. ename , ' is empty, so it is replaced by a cell of fill (' , (exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames) , '); ',LF
        if. # (emptyop i. 0) {:: frames do.
          ftext =. ftext , nonename, ' is broken into ' , (exegesisfmtframe shapes ,&((emptyop i. 0)&{) frames) , (exegesisfmtcell (0;0) ,~ shapes ,&((emptyop i. 0)&{) frames),' which are supplied to the ',vstring,'.',LF
        else.
          ftext =. ftext , nonename , ' is a single cell, ', (exegesisindefinite exegesisfmtcell (0;0) ,~ (0{::shapes);frame) , '.  These arguments are supplied to the ',vstring,'.',LF
        end.
      end.
    end.
    ftext =. ftext , 'The frame, ' , (":frame) , ', ' , (0{::ctense) , ' prepended to the result of the ',cvstring
  case. 1 do.
    if. 1 = #vranks do.
      ftext =. 'There is only one cell, ', (exegesisindefinite exegesisfmtcell (0;0) ,~ (0{::shapes);frame) , ' which is supplied to the ',vstring,'.',LF
    else.
      ftext =. 'Each argument has only one cell (on the left, ' , (exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames) , '; on the right, ' , (exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(1&{) frames) , ') which are supplied to the ',vstring,'.',LF
    end.
    ftext =. ftext , 'The frame, ' , (":frame) , ', ' , (0{::ctense) , ' prepended to the result of the ',cvstring
  case. do.
    if. 1 = #vranks do.
      ftext =. 'The frame is ',(":frame),'.',LF
      ftext =. ftext , 'The argument is broken into ' , (exegesisfmtframe (0{::shapes);frame) , (exegesisfmtcell (0;1) ,~ (0{::shapes);frame) , ', which will be supplied one by one to the ',vstring,'.',LF
    else.
      select. *@#@> frames
      case. 1 0 do.
        ftext =. 'The frame of x is ',(": 0 {:: frames),'.',LF
      case. 0 1 do.
        ftext =. 'The frame of y is ',(": 1 {:: frames),'.',LF
      case. do.
        ftext =. 'The frame of x is ',(": 0 {:: frames),' and the frame of y is ',(": 1 {:: frames),'.',LF
      end.
      if. # 0 {:: frames do.
        ftext =. ftext , 'x is broken into ' , (exegesisfmtframe shapes ,&(0&{) frames) , (exegesisfmtcell (0;1) ,~ shapes ,&(0&{) frames),'.',LF
      else.
        ftext =. ftext , 'x is a single cell (',(exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(0&{) frames),') that will be replicated for use with each cell of y.',LF
      end.
      if. # 1 {:: frames do.
        ftext =. ftext , 'y is broken into ' , (exegesisfmtframe shapes ,&(1&{) frames) , (exegesisfmtcell (0;1) ,~ shapes ,&(1&{) frames),'.',LF
      else.
        ftext =. ftext , 'y is a single cell (',(exegesisindefinite exegesisfmtcell (0;0) ,~ shapes ,&(1&{) frames),') that will be replicated for use with each cell of x.',LF
      end.
      if. (i.&0@:=/ > frames) < <./ #@> frames do.
        ftext =. ftext , 'This is an agreement error.',LF
      elseif.
      surplusframe =. 0 -.~ bwxor/ > frames  NB. extend shorter frame with 0, XOR, remove common frame (we know no 0 in any frame)
      (0~:#surplusframe) *. 0 -.@e. #@> frames do.
        NB. cells of the short operand must be replicated, and not just a single cell.
        'short long' =. (>&#&>/ frames) |. 'x';'y'
        ftext =. ftext , 'Each ',(0 exegesisfmtcell (0;1) ,~ (0{::shapes);frame),' of ' , short , ' is replicated into ' , (exegesisfmtframe '';surplusframe) ,'identical cells, and then corresponding cells of x and y are supplied one by one to the ',vstring,'.',LF
      elseif. do.
        ftext =. ftext , 'Corresponding pairs of cells are supplied one by one to the ',vstring,'.',LF
      end.
    end.
    ftext =. ftext , 'The results ',(1{::ctense),' assembled into ' , (exegesisindefinite exegesisshapex frame) , ' result-cells after completion of the ',cvstring
  end.
  res =. res , EXEGESISFRAMEVALID ; ftext
end.
res
)

NB. y is rank box(es) [x,]y
NB. If the rank contains *, we give a comment about the start of the fill-cell computation
exegesisranks =: 3 : 0
if. '*' e. ; y do. EXEGESISFRAMEFILLSTART ; 'The execution of this verb on a cell of fills begins here.  It completes in the block containing the shape marked with *.',LF
else. 0 2$a:
end.
)

NB. ********** rankoverall exegesis lines ************************

NB. y is appearstwice;startlabel if any
NB. Result is type;string for end-of-computation that has operands
exegisisrankoverallcompend =: 3 : 0
'appearstwice tit vname' =. 3 {. y , <'verb'
select. appearstwice , errorcode e. EHASFILLMASK
case. 1 1 do.
  EXEGESISRANKOVERALLCOMPEND;'This block %al1%calculates and displays the ',vname,':',LF,(defstring 0),CR
case. 0 1 do.
  EXEGESISRANKOVERALLCOMPEND;'This block %al1%displays the result of the ',vname,':',LF,(defstring 0),CR,'%strt%and shows the last verb in the computation',tit,'.',LF,'%end%'
case. 1 0 do.
  EXEGESISRANKOVERALLNOOPS;'This block will %al1%highlight the result of the ',vname,':',LF,(defstring 0),CR,'after a selection has been made.',LF
case. do.
  EXEGESISRANKOVERALLNOOPS;'This block will %al1%highlight the result of the ',vname,':',LF,(defstring 0),CR,'after a selection has been made.',LF
end.
)
NB. y is appearstwice;startlabel if any
NB. Result is type;string for end-of-computation that has no display
exegisisrankoverallnodisp =: 3 : 0
'appearstwice tit vname' =. 3 {. y , <'verb'
if. appearstwice do.
  t =. 'This block will %al1%calculate and display the ',vname,':',LF,(defstring 0),CR,'after sufficient selections have been made.',LF
else.
  t =. 'This block will %al1%display the result of the ',vname,':',LF,(defstring 0),CR,'and show the last verb in the computation',tit,'.  Sufficient selections must be made for this result to appear.',LF
end.
EXEGESISRANKOVERALLNODISP;t
)


NB. ******************* class-dependent portion of display and pick support **********************
cocurrent 'dissectobj'

NB. Calculate the size in pixels to allocate on the main form
NB. y is valueformat
NB. result is table of allowed sizes.  If exploring is allowed, the second row is the size for exploring
calcformsize =: 3 : 0
sz =. extractDOLsize y
}:^:(sz -: {.) maxnoundisplaysizes <."1 sz
)

cocurrent 'dissectdisplaytwo'
coinsert 'dissectobj'  NB. Needed for lint
NB. This locale is used for nodes that fall back to displaying just two results if the whole result won't fit.
NB. ex: u/, u^:

NB. Calculate the size in pixels to allocate on the main form, and the number of allowed views
NB. y is valueformat
NB. result is table of allowed sizes.  If exploring is allowed, the second row is the size for exploring
calcformsize =: 3 : 0
NB. If the entire display fits on the main form, return that single display.
NB. Otherwise, size the main form to display the largest infix of 2 results; or, if that won't fit, the
NB. largest single result.  If that won't fit, just use the max size
sz =. extractDOLsize y  NB. Get the full noun size
if. sz -: {. dsizes =. maxnoundisplaysizes <."1 sz do.
NB. The first display is big enough - discard the second one
  }: dsizes
else.
NB. Get full size, size of 2 items, and of 1.  For each dimension, use the first size that doesn't exceed max for the main form
NB. That's for the main view - we can explore the entire value
  dsizes 0}~ ({. maxnoundisplaysizes) (,~ {~ i.&1@:>:)"0 1 |: sz , (2 2,:1 1) extractDOLsizelimited y
end.
)

cocurrent 'dissectobj'

NB. Create DO
createDO =: 3 : 'createDOvn (32 = 3!:0 displaylevrank) {"1 nouncfm,.verbcfm'

NB. Draw DO.  y is yx of DO
drawDO =: 3 : 'drawDOvnall y'

NB. ********************************** pick DOs
NB. Pick for display object, vectoring to the indicated type
NB. x is ('l' or 'r') flag
NB. y is main/explorer;(y,x relative to pickrect);pickflags (numeric atom of left,right,ctrl,shift)
NB. no result.  selections are performed
pickDO =: 4 : 0  NB. Called in locale of the node that drew the DO
QP^:DEBPICK 'coname'''' y '
'exp yx sd px' =. y
for_r. (exp{pickrects) (_1 findpickhits) yx do.
  'ix pyx' =. r
  QP^:DEBPICK 'x ix pyx exp yx sd px ix{::picknames '
  if. 3 = 4!:0 <name =. 'pick' , x , ix {:: picknames do. (sd;exp) name~ pyx end.
end.
0 0$0
)

NB. hover/statline is like pick, but there is no mouse button and no control keys
NB. We return the string to use for the tooltip
hoverstatDO =: 4 : 0  NB. Called in locale of the node that drew the DO
'exp yx' =. y
if. #r =. (exp{pickrects) (_1 findpickhits) yx do.
  'ix pyx' =. {. r
  if. 3 = 4!:0 <name =. x , ix {:: picknames do.
    exp name~ pyx
  else. ''
  end.
else. ''
end.
)
hoverDO =: 'hover'&hoverstatDO
statlineDO =: 'statline'&hoverstatDO

NB. Statline is like hover.  We return the statline string

NB. For all these hover verbs, x is (view number), y is the yx position of the click relative to start of pickrect

NB. y is the child of the locale that this verb is being executed in
NB. Return the nearest ancestor (including this node itself) that
NB. has operands, and which operand(s) this node represents.
NB. Result is locale;operand code:
NB. 2=@ (or monad &), 0=& (x operand), 1=&(y operand)
NB.
NB. This is the default case.  Just return y;empty which means to
NB. go back to the previous level
findparentwithshapes =: ;&2
NB. This definition is overridden for & and @

hoverDOlabelposchartutorial =: (#~  LF&= <: (LF,LF)&E.) 0 : 0
This is the name field for a named noun.  In verb blocks this will be the rank stack.

)
hoverDOlabelposranktutorial =: (#~  LF&= <: (LF,LF)&E.) 0 : 0
This is the rank stack.  At the bottom of the stack is the verb whose result is displayed in the data portion of the block.  
Other lines in the rank stack are modifiers that affect the execution of the verb by changing the cells it operates on.


The numbers at the right (and also the left, for dyads) of each line indicate the rank of the cells operated on by the verb/modifier shown in that line.  
Each number is the lesser of the rank of the argument and the corresponding verb-rank.  
If this value is shown in normal font, the entire argument was a single cell of the verb and has the indicated rank.  
If this value is shown in bold italics, the argument was broken up into cells of the indicated rank, and the verb was executed on each cell separately.


A modifier is assigned a new selection level if it produces more than one result-cell.  By repeatedly clicking in the data area you can select from successive levels.


Each selection level has a color.  
This color is used for all information for the level:

(1) the rank here,

(2) the color of the frame of the modifier in the shape line, 

(3) the background color of a selected result-cell, 

(4) the outline color used to show the argument cells that contributed to the selected result.


For example, in the sentence

(i.3) +"1"2 i. 3 4 3

the rank stack for + will have 3 lines, one for each " modifier and one for the verb + .  
The top line (white), with rank 2, relates to the overall verb +"1"2 .  The middle line (green), with rank 1, relates to the verb +"1.  
The bottom line (magenta), with rank 0, relates to the verb + .  Selection is possible at each level.


Hovering over a line of the rank stack will provide more information.  
Hovering over the verb line will describe the overall operation of the block; hovering over a modifier line will describe that modifier.

)


NB. Hovering in the label 
hoverDOlabelpos =: 4 : 0
exp =. x
if. #r =. (exp{DOlabelpospickrects) (_1 findpickhits) y do.
  NB. See if this block has data displayed
  datapresent =. picknames e.~ <'DOdatapos'
  'ix pyx' =. {. r   NB. index of pickrect found
  select. tooltipdetailx
  case. 0 do. text =. ''
  case. 1 do. text =. LF ,~ x statlineDOlabelpos y
  case. do.
    tt =. 0 2 $a:
    if. 2 ~: 3!:0 displaylevrank do.
      tt =. ,: EXEGESISTUTORIAL ; hoverDOlabelposranktutorial
      NB. If there is a rank stack, process it.  First the frame, then the individual item
      labelloc =. ix { DOranklocales
      NB.?lintonly labelloc =. <'dissectobj'
      NB. Get the maximum scope for the selected line.  If there is no selection (i. e. no shapes) at
      NB. the locale selected (ex: u@v, we look at v), go up (from v to u@v, which has no explicit line)
      NB. to see if there are shapes there.  If the parent is & or @ (which are known to leave no line
      NB. in the rank stack) and we are the v, we use the parent.  Result is locale;operand code:
      NB. 2=@ (or monad &), 0=& (x operand), 1=&(y operand)
      if. #inputselopshapes do.
        'frameloc opno' =. labelloc;2
      else.
        'frameloc opno' =. findparentwithshapes__parent labelloc
      end.
      NB. Get the explanation of frame
      tt =. tt , exegesisframe__frameloc labelloc;opno;datapresent
      NB. Append the analysis of the rank
      NB. Append any explanation unique to this line.  The y argument is the boxed rank text, [x,]y
      tt =. tt , exegesisranks (<ix;<<_2) { DOranks
      NB. (1 if this locale appears more than once);(1 if this is the last locale in the stack);(1 if this block displayed data)
      NB. Computational flags display only when both are 0, leaving the display for the overall node
      tt =. tt , exegesisrankstack__labelloc (1 < labelloc +/@:= 1 {"1 displaylevrank),(ix = <:#DOranklocales),datapresent
      NB. Finally, any explanation for the node in general.  Expansions and Finals are explained here.  Every line contributes.
      NB. y is (1 if this block displayed data);(1 if this block is the last one with nonempty title);(title string)
      NB. x is (1 if this locale appears twice in the stack)
      NB. We process bottom-up to leave explanations in reverse order, at the top of the tooltip
      QP^:DEBEXEGESIS'tt displaylevrank '
      if. DEBEXEGESIS do. for_l. 1{"1 displaylevrank do. QP 'defstring__l]0 ' end. end.
      isfirsttitled =. [: (*. (= +/\)) (DLRCOMPEND;a:) -.@e.~ {."1
      if. ix = <:#DOranklocales do. tt =. tt ,~ ; (}: (4 : '(1 < y +/@:= 1 {"1 displaylevrank) <@exegesisrankoverall__y x') {:)"1 (<datapresent) ,. (;~"1 0   isfirsttitled) |. 2 {."1 displaylevrank end.
    else.
      tt =. ,: EXEGESISTUTORIAL ; hoverDOlabelposchartutorial
      NB. If the display was text, we will pass that text into the overall for the node
      tt =. tt , 0 exegesisrankoverall datapresent;1;displaylevrank
    end.
    text =. exegesisgrammar tt
  end.
else.
  text =. ''
end.
reflowtoscreensize text
)
statlineDOlabelpos =: 4 : 0
exp =. x
if. #r =. (exp{DOlabelpospickrects) (_1 findpickhits) y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. 2 ~: 3!:0 displaylevrank do.
    labelloc =. ix { DOranklocales
    NB.?lintonly labelloc =. <'dissectobj'
    text =. defstring__labelloc 0
    ". :: 0: 'tempnm =. ' , text
    if. 3 = 4!:0 <'tempnm' do. text =. text , '  verb rank: ' , ": vranks__labelloc end.
  else.
    text =. displaylevrank
  end.
else.
  text =. ''
end.
text
)

NB. Picking in the label.  Left-click gets NuVoc
picklDOlabelpos =: 4 : 0
'sd exp' =. x
msgtext =. ''
if. #r =. (exp{DOlabelpospickrects) (_1 findpickhits) y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. 2 ~: 3!:0 displaylevrank do.
    NB. If there is a rank stack, process it.
    labelloc =. ix { DOranklocales
    NB.?lintonly labelloc =. <'dissectobj'
    if. #nuvocpage__labelloc do.
      NB.?lintonly browse_j_ =. 3 : 'y'
      browse_j_ JWIKIURL,'Vocabulary/' , nuvocpage__labelloc
    else.
      msgtext =. 'No NuVoc available'
    end.
  else.
    msgtext =. 'No NuVoc available'
  end.
  if. #msgtext do.
    NB. If we are in an explorer, stay here; but if on the main form, we have to switch to that locale
    formloc =. exp { COCREATOR,coname''
    NB.?lintonly formloc =. <'dissect'
    ('label';ix) drawttipwithemphasis__formloc (pyx + (((exp,0 0);0 0;0) {:: DOlabelpos) + exp { DOyx + (<ix,0) {"_1 DOlabelpospickrects) ; msgtext
    hoversessmin__COINSTANCE =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
  end.
end.
0 0$0
)

NB. Picking in the label.  Right-click lauches debug/dissect, but only if the verb
NB. is named
pickrDOlabelpos =: 4 : 0
'sd exp' =. x
msgtext =. ''
if. #r =. (exp{DOlabelpospickrects) (_1 findpickhits) y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. 2 ~: 3!:0 displaylevrank do.
    NB. If there is a rank stack, process it.
    labelloc =. ix { DOranklocales
    NB.?lintonly labelloc =. <'dissectobj'
    msgtext =. pickrlaunchdebug__labelloc ''
  end.
  if. #msgtext do.
    NB. If we are in an explorer, stay here; but if on the main form, we have to switch to that locale
    formloc =. exp { COCREATOR,coname''
    NB.?lintonly formloc =. <'dissect'
    ('rlabel';ix) drawttipwithemphasis__formloc (pyx + (((exp,0 0);0 0;0) {:: DOlabelpos) + exp { DOyx + (<ix,0) {"_1 DOlabelpospickrects) ; msgtext
    hoversessmin__COINSTANCE =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
  end.
end.
0 0$0
)

NB. Nilad.  Launch debug/dissect for right-click.  Result is tooltip text to
NB. display (no tooltip if empty)
pickrlaunchdebug =: 3 : 0
'Only named verbs can be debugged.'
)

getselectedcell =: 3 : 0
NB. sel1 is the path to the selection.  It may go down multiple levels, but it will
NB. always end with a dropdown if there is a dropdown.  If the fillmask is boxed, there will
NB. be a dropdown as long as this node is selectable (it isn't in the case of L: when the arguments
NB. are initially at level).
NB. So, we convert the selection to path form, which merely requires removing the dropdowns.
sel1 =. SFOPEN -.~ > isfensureselection isftorank2 sellevel { selections
NB. If the current fillmask is boxed, it has internal structure, and we
NB. should replace the selected portion with the fillmask and data that was calculated in u
select. resultlevel
  NB. tmodx gives the index into selresult of the selection.  For L: we get it from
  NB. the result map; for others we calculate from the index
  NB. fillmask, which describes what ought to be, is in natural order.
  NB. selresult, which describes what is, is in ticket order.
case. 0 do.
  NB.?lintonly resultseqmap =: ''
  NB. L: requires that we box the lower fillmask, whatever it is, and insert it into the tree.
  NB. We also install the lower selresult
  tmodx =. sel1 {:: resultseqmap
case. 1;2 do.
  NB. expansion/each, and collection error both require that we install the boxed fillmask.
  NB. We install into the fillmask array
  tmodx =. ($fillmask)&#.&.> selectiontoticket sel1
case. do.
  NB. Normal fillmasks, which may or may not be boxed (they will be boxed if they contained some boxed detail such as
  tmodx =. frame #. 0 {:: sel1   NB. only 1 atom allowed; make it an atom   obsolete > {.
end.
tmodx {:: selresult
)

pickrDOshapepos =: 4 : 0
'sd exp' =. x
if. 0 = #DOshapes do.
elseif. #r =. (exp{DOshapepospickrects) (_1 findpickhits) y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  NB. Get the picked row: 0 for shape, 1 for selection.  The selection is possible only if there is
  NB. a selection line, i. e. #DOshapes=2
  row =. ({.pyx) > ((<exp,_1 1 0) { DOshapepospickrects) % #DOshapes
  if. (row = 0) +. (ix >: #DOshapelocales) do.
    t =. fillmask frameselresult selresult
    msg =. 'result copied to clipboard'
  else.
    l =. ix { DOshapelocales   NB. the locale of the selection
    NB.?lintonly l =. <'dissectverb'
    NB. Put the selected data on the clipboard
    t =. getselectedcell__l ''
    msg =. 'selected cell copied to clipboard'
  end.
  try.
    wd 'clipcopy *' , 5!:5 <'t' [ t
  catch.
    msg =. 'error writing to clipboard'
  end.
  NB. If we are in an explorer, stay here; but if on the main form, we have to switch to that locale
  formloc =. exp { COCREATOR,coname''
  NB.?lintonly formloc =. <'dissect'
  ('shape';ix) drawttipwithemphasis__formloc (pyx + (((exp,0 0);0 0;0) {:: DOshapepos) + exp { DOyx + (<ix,0) {"_1 DOshapepospickrects) ; msg
  hoversessmin__COINSTANCE =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
end.
0 0$0
)

hoverDOshapepostutorial =: (#~  LF&= <: (LF,LF)&E.) 0 : 0
This is the shape line.  
It gives the result-shape.  
If you make a selection in the data area, a second line will appear to show which cell was selected.


The result-shape is the concatenation of the frame and the shape of the result cells.  
If you ignore the coloring and simply read the numbers, that will tell you the shape of the data block.  


The frame, if any, is the part of the result-shape before the dark-blue result-cell shape.  
As you make selections in the data area, you will see the coloring of the frame change to indicate which portions of the frame were used at each selection level.


The shape of the result cells (the last component of the result-shape) is shown in white against a dark-blue background.  
If the result-cells have varying shapes, the filled shape, enclosed in parentheses, is appended to the result-shape to indicate that the result contains fills, which are shown by crosshatching in the data area.


If the block is part of a compound using @, &, or &. there is no displayable result until a single result-cell has been selected.  Until that time the shape/selection lines are omitted.

A result that is a single atom (with empty shape) is indicated by the word 'atom' in the result-cell shape.


If the block includes modifiers that look inside the boxing structure, namely each, &.>, L:n, or S:n, entry into a level of boxing is indicated with '>' at the appropriate point.

)
hoverDOshapeposseltutorial =: (#~  LF&= <: (LF,LF)&E.) 0 : 0
These are the shape/selection lines.  
The first line gives the result-shape; the second line gives the path of the selection.


The result-shape is the concatenation of the frame and the shape of the result cells.  
If you ignore the coloring and simply read the numbers, that will tell you the shape of the data block.


Each selection level has a color.  
This color is used for all information for the level: 

(1) the rank in the rank stack, 

(2) the background color of the frame and selection here, 

(3) the background color of a selected result-cell, 

(4) the outline color used to show the argument cells that contributed to the selected result.


The frame, if any, is the part of the result-shape before the dark-blue result-cell shape.
As you make selections in the data area, you will see the coloring of the frame change to indicate which portions of the frame were used at each selection level.


The shape of the result cells (the last component of the result-shape) is shown in white against a dark-blue background.
It indicates the shape of the results of the last selection level.
If the result-cells have varying shapes, the filled shape, enclosed in parentheses, is appended to the result-shape to indicate that the result contains fills (which are shown by crosshatching in the data area).


The second line indicates the selections you have made.
Each selection selects a single result-cell; the path to that cell is shown beneath the frame that it was selected from.


If the block is part of a compound using @, &, or &. there is no displayable result until a single result-cell has been selected.  Until that time the shape/selection lines are omitted.

A result that is a single atom (with empty shape) is indicated by the word 'atom' in the result-cell shape.


If the block includes modifiers that look inside the boxing structure, namely each, &.>, L:n, or S:n, entry into a level of boxing is indicated with '>' at the appropriate point.

)

NB. default verbs for other hovering
hoverDOshapepos =: 4 : 0
select. tooltipdetailx
case. 0 do. text =. ''
case. 1 do. text =. LF ,~ x statlineDOshapepos y
case. do.
  tt =. ,: EXEGESISTUTORIAL ; (1 < #DOshapes) {:: hoverDOshapepostutorial;hoverDOshapeposseltutorial
  tt =. tt , EXEGESISSHAPEHEADER ; ''
  exp =. x
  if. 2 = 3!:0 displaylevrank do.
    tt =. tt , EXEGESISSHAPERESULT ; 'The shape of this noun.',LF
  elseif. #r =. (exp{DOshapepospickrects) (_1 findpickhits) y do.
    if. #DOshapelocales do.
      'ix pyx' =. {. r   NB. index of pickrect found
      if. ix < #DOshapelocales do.
        l =. ix { DOshapelocales   NB. the locale of the selection
        NB.?lintonly l =. <'dissectverb'
        ttt =. 'This is the frame of the verb:',LF,(defstring__l 0),CR
        if. (<resultlevel__l) e. 0;1 do.
          if. 1 < #DOshapes do.
             ttt =. ttt , 'This verb operates inside the boxed structure.  The top line shows the number of times the verb was executed inside the boxed structure.  The second line gives the path to the selected result.',LF
          else.
             ttt =. ttt , 'This verb operates inside the boxed structure.  The number is how many times the verb was executed inside the boxed structure.',LF
          end.
        else.
          if. 1 < #DOshapes do.
            ttt =. ttt , 'The top line is the frame of the verb; the bottom line is the index of the selected result',LF
          end.
        end.
        if. '*' e. (0,ix) {:: DOshapes do.
          ttt =. ttt , 'The frame contains 0, so the verb is executed on a cell of fills.  The place where computation of the fill-cell starts is marked with * after the rank.',LF
        end.
        tt =. tt , EXEGESISSHAPESELECTINGVERB ; ttt
      else.
        ttt =. 'The shape of a single result-cell of the verb:',LF,(defstring__lastexecutednode 0),CR
        select. {: cellshapedisp
        case. ')' do.
           ttt =. ttt , LF,'Some cells are padded with fill.',LF
        case. '?' do.
           ttt =. ttt , LF,'The shape of a result-cell is unknown.',LF
        end.
        tt =. tt , EXEGESISSHAPERESULT ; ttt
      end.
    else.
      tt =. tt , EXEGESISSHAPERESULT ; 'The shape of the result of this verb.',LF
    end.
  end.
  text =. exegesisgrammar tt
end.
reflowtoscreensize text
)
statlineDOshapepos =: 4 : 0
exp =. x
if. 2 = 3!:0 displaylevrank do.
  tt =. 'shape of noun'
elseif. #r =. (exp{DOshapepospickrects) (_1 findpickhits) y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. ix < #DOshapelocales do.
    l =. ix { DOshapelocales   NB. the locale of the selection
    NB.?lintonly l =. <'dissectverb'
    tt =. 'frame of ' , defstring__l 0
  else.
    tt =. ((*#DOshapelocales) {:: 'shape of result of '; 'shape of result-cell of ') ,(defstring__lastexecutednode 0) , (')' = {: cellshapedisp) # ' (fill added)'
  end.
elseif. do. tt =. ''
end.
tt
)

erroragree =: 3 : 0
NB.?lintonly agreeerrorlocale =. <'dissectverb'
text =. 'This dyadic verb has x and y operands that cannot be matched up cell-for-cell:',CR
text =. text , (defstring__agreeerrorlocale 0),LF
text =. text , 'The verb has left rank of ' , (": lr =. 0{vranks__agreeerrorlocale), '; the left-argument shape is ' , (": ls =. $^:(0<L.)@> {. inputselopshapes__agreeerrorlocale),LF
text =. text , 'The verb has right rank of ' , (": rr =. 1{vranks__agreeerrorlocale), '; the right-argument shape is ' , (": rs =. $^:(0<L.)@> {: inputselopshapes__agreeerrorlocale),LF
text =. text , 'The left frame is ' , (": lf =. (-lr) }. ls),LF
text =. text , 'The right frame is ' , (": rf =. (-rr) }. rs),LF
if. lf =&# rf do.
  text =. text , 'The frames should match, but they don''t.'
else.
  text =. text , 'The frames must be identical, or one must be a prefix of the other.'
end.
text , '  This error is reported as a ''length error'' in the J session. The J session stops when it detects the first error; here we indicate all blocks with arguments that do not agree.',LF
)

NB. If the error description is a single word, it is the name of a verb that will analyze the error
errorlookup =: (LF&taketo ; LF&takeafter);._1 (0 : 0)
?agreement
erroragree
?framing
The verb completed correctly on each cell, but the result-cells are of different types and cannot be assembled into a single result.

This error is reported as 'domain error' in the J session.

The result-cells that could not be assembled are shown below, with each result inside its own dashed box, so that you can see where the incompatibility arises.
?no neutral
When (u/ y) is executed with empty y, the result is a neutral (aka identity element) for u.  But this u has no neutral.
?invalid operand
An operand to a modifier is invalid.  The derived verb was not executed.

This is reported as a 'domain error' in the J session.
?invalid verb
This combination was rejected before it was even executed on its arguments.

This is reported as a 'domain error' in the J session.
?error
This execution resulted in an error, but overall execution continued.

If this verb is being executed on a cell of fills, the error result is treated as if it were 0.

If this verb is executed in the combination u :: v, execution continues with the v side.
?incompatible selectors
In x m} y, the selectors select regions of y that have different shapes.  The selectors are the atoms of x in x { y or the atoms of m in x m} y.

This is reported as a 'domain error' in the J session.
?non-atomic v
In m@.v, the execution of v must result in a numeric atom.

This is reported as a 'domain error' in the J session.
?selection too long
In x u;.3 y, x specifies more axes than y has.

This is reported as a 'length error' in the J session.
?selector invalid
In x m} y, a complementary selector is not a single atomic box.  The selectors are the atoms of >m in x m} y.

This is reported as an 'index error' in the J session.
?selector rank
In x { y or x m} y, the selectors have rank > 1.  The selectors are >x in x { y or >m in x m} y.

This is reported as a 'rank error' in the J session
?selector level
In x { y or x m} y, a selector has boxing level higher than 3.  The selectors are the atoms of x in x { y or the atoms of m in x m} y.

This is reported as a 'domain error' in the J session.
?selector too long
In x { y or x m} y, a selector specifies more axes than y has.  The selectors are the atoms of x in x { y or the atoms of m in x m} y.

This is reported as a 'length error' in the J session.
?xm agreement
In x m} y, $x must be a suffix of $m{y.

This is reported as a 'domain error' in the J session.
?attention interrupt
You interrupted execution with JBreak.
?break
You interrupted execution with JBreak.
?domain
The arguments to this verb are invalid.
?file name
You are operating on a nonexistent device or file.
?file number
There is no file open with that number. 
?index
You are accessing outside the bounds of your array.
?interface
You have an ill-formed filename, or are making an illegal request.
?length
Your argument has a length that this verb can't handle.
?locale
You have tried to reuse a numeric locale number.
?limit
An argument exceeds an internal limit in J.
?NaN
The computation produced a non-numeric (NaN) value, represented in J by _. .
?nonce
The operation you requested is not supported yet.
?out of memory
A value was so large that the computer ran out of memory.  This is signaled when J needs more memory to store a value and the operating system refuses to supply it.
?rank
Your argument has a rank that this verb can't handle.
?security violation
You have requested heightened security, and this verb would be insecure.
?stack
You have exceeded J's generous recursion limit, probably because of an infinite recursion.
?syntax
This verb either executes an illegal sentence or attempts to return a result that is not a noun.

The usual cause is an undefined name in the definition of the verb.
?time limit
Execution took too long.  An execution time-limit has been set by (9!:33).
?value
Execution of this verb attempted to use a name that has not been assigned.
?recoverable error
This verb failed, but execution continues with the verb given by the :: conjunction.
?0 for fill result
Execution on the cell of fills for this block failed.  Execution continues here as if the execution had produced 0.
?error on fill-cell
This block is part of an execution on a cell of fills started in another block.  This block failed.  Execution continues in the block that had the fill-cell, as if the execution on the fill-cell had produced 0.
)

hoverDOstatuspos =: 4 : 0
origemsg =. (<<<0 _1)&{^:('('={.) DOstatusstring
if. ' ' -.@e. vnm =. LF taketo msgtext =. ((1 {"1 errorlookup) , <' ') {::~ (0 {"1 errorlookup) i. <origemsg do.
  msgtext =. vnm~ ''
end.
reflowtoscreensize msgtext
)

hoverDOassignpos =: 4 : 0
NB. Get the target names.  If there are names that were totally left out of the display, they show up as a trailing '+'
NB. (which will be a word by itself).  A name that was cut off shows as '...'
names =. ;: _2 }. assignmentlabel  NB.?lintonly =. 'a b c'
NB. See which ones are locatives
locatives =. '_' (('__' (+./@:E.) ]) +. (={:) *. 1 < +/@:=)&> names
NB. See if there were too many names to fit
omittednames =. '.' e. _2 }. DOstatusstring
NB. get type of assignment
public =. ':' = {: DOstatusstring
if. ' ' = {. assignmentlabel do.
  NB. The assignment failed.  Try to figure out why
  if. (1 < #names) *. (#names) ~: {.!.(#names) DOdatashape do.
    NB. length error
    text =. 'The number of names being assigned does not match the number of items in the value.'
  elseif. public do.
    NB. public assignment.  Might be to privately-assigned name
    if. 1 = #names do.
      NB. simple assignment
      if. {. locatives do.
        text =. 'The assignment failed.  Perhaps the locale is invalid.'
      else.
        text =. 'The assignment failed.  It is illegal to make a public assignment to a name that has been privately assigned in the same verb.'
      end.
    else.
      if. *./ locatives do.
        text =. 'The assignment failed.  Perhaps one of the locales is invalid.'
      elseif. +./ locatives do.
        text =. 'The assignment failed.  One of the locales may be invalid, or you may have attempted to make a public assignment to a name that has been privately assigned in the same verb.'
      elseif. do.
        text =. 'The assignment failed.  It is illegal to make a public assignment to a name that has been privately assigned in the same verb.'
      end.
    end.
  elseif. do.
    if. +./ locatives do.
      text =. 'The assignment failed.  Check the validity of the locales.'
    else.
      text =. 'The assignment failed.'
    end.
  end.
else.
  if. 1 < #names do.
    if. '' -: DOdatashape do.
      text =. 'The value in this block is assigned to each of the names shown.'
    else.
      text =. 'The value in this block is assigned to the names shown, one item to each name.'
    end.
    if. DOdataisboxed do.
      text =. text , '  One level of boxing is removed.'
    end.
    if. omittednames do.
      text =. text , '  There are more names assigned than would fit into the display.'
    end.
  else.
    if. omittednames do.
      text =. 'The value in this block is assigned.  The name is too long to fit into the display.'
    else.
      text =. 'The value in this block is assigned to the name shown.'
    end.
  end.
  if. public do.
    if. 1 < #names do.
      text =. text , '  The names are assigned publicly and can be used outside of the verb they are assigned in.'
    else.
      text =. text , '  The name is assigned publicly and can be used outside of the verb it is assigned in.'
    end.
  else.
    select. 0 1 e. locatives
    case. 1 0 do.   NB. all simple names
      if. 1 < #names do.
        text =. text , '  The names are assigned privately and can be used only in the verb they are assigned in.'
      else.
        text =. text , '  The name is assigned privately and can be used only in the verb it is assigned in.'
      end.
    case. 0 1 do.   NB. All locatives
      if. 1 < #names do.
        text =. text , '  Because the names are locatives, they is assigned publicly in their locales and can be used outside of the verb they are assigned in.'
      else.
        text =. text , '  Because the name is a locative, it is assigned publicly in its locale and can be used outside of the verb it is assigned in.'
      end.
    case. do.   NB. Must be a mixture
      text =. text , '  The locatives are assigned publicly in their locales; the simple names are assigned privately and can be used only in the verb they are assigned in.'
    end.
  end.
end.
reflowtoscreensize text,LF
)

hoverDOdatapostutorial =: (#~  LF&= <: (LF,LF)&E.) 0 : 0
This is the data area, where the result of the verb is displayed.  
If the result contains multiple result-cells, left-clicking in the data area will select a single result-cell for further inspection.


Each selection level has a color, used for all information for the level: 

(1) the rank in the rank stack, 

(2) the background color in the shape/selection lines, 

(3) the background color of a selected result-cell here, 

(4) the outline color used to show the argument cells that contributed to the selected result.


White is the color of the topmost selection level.  Until a selection has been made, the background of the result is white.  
When you select a result-cell, that cell becomes active in the second selection level, which is green: 

 * the background of the selected cell changes color

 * the shape/selection lines show the frame of the selected level and the path to the selected cell

 * the arguments to the computation of the result are outlined with the color of the level (the y argument is a solid outline, while x is dashed) 


The most recent selection is always indicated with a heavy black outline.  
A selection in the final executing verb does not change the background color of the selected cell, but it does highlight the arguments to its computation, 
using the color of the next selection level.


To keep the display compact, the screen space allocated to the data area is limited.  
You may change the default maximum size in the Sizes menu.  
You may also resize an individual data block by dragging the resizing handles at the lower-right of the data block.  
Whenever the entire result does not fit in the data area, scrollbars are provided; in addition you can right-click in the data area to create a fullscreen explorer window that will show the result.

)

NB. y is table of path;shape
NB. Result is character-string description
NB. We use 'atom' for an atom; '<atom>' for a boxed atom; ';' for dropdown; 'empty(shape)' for an empty 
isftodisplayablepath =: 3 : 0
NB. format of empty: discard the axes inside the empty bit (they are represented by _1), and append 'empty' to show the rest
boxindexes =. (":&.>@{.) ` ((,&' out of '^:(*@#)@":@(-.&_1)@[ , 'empty(shape=' , ')' ,~ ":@])&.>/) ` ((<'<atom>')"_)  @.(([: #. ''&-: , 0&e.)@:(1&{::))"1 y  NB. nonempty, empty, atom
NB. Remove <> from last box, and put ';' after all previous
; (,&'; '&.>@}: , -.&'<>'&.>@{:) boxindexes
)

jdatatypes =: <;._2 (0 : 0)
numeric, Boolean
character, byte
numeric, integer
numeric, float
numeric, complex
boxed
numeric, extended integer
numeric, rational


sparse, numeric, Boolean
sparse, character, byte
sparse, numeric, integer
sparse, numeric, float
sparse, numeric, complex
sparse, boxed
symbol
character, Unicode
)

NB. Name signifies 'compend found','multiple modifier lines'
hoverDOdataposverbtutorialsimple =: 0 : 0
To see details about the verb that produced this result, hover over the name of the verb in the rank stack above the shape line.

)
hoverDOdataposverbtutorialfinal =: 0 : 0
This block shows the overall result of executing a modifier.  Hover over the name of the verb in the rank stack for instructions on how to explore the execution of the modifier.

)

hoverDOdataposverbtutorialcompound =: 0 : 0
This is the result of a modified verb and is thus simultaneously the result of all the compounds that end with the execution of the verb.  Hover over the rank stack to see the verb and the compounds.  The verb is the last line of the rank stack.

)

hoverDOdatapos =: 4 : 0
exp =. x
hoveryx =. y
NB. Ignore hover in the scrollbar
NB. See which scrollbar, if any, the click is in
dhw =. (<exp,1) { DOdatapos
if. 0 = +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTHPIXELS * |. exp { displayscrollbars do.
  select. tooltipdetailx
  case. 0 do. text =. ''
  case. 1 do. text =. LF ,~ x statlineDOdatapos y
  case. do.
    disp =. ,: EXEGESISTUTORIAL ; hoverDOdatapostutorial
    disp =. disp , EXEGESISDATAHEADER ; ''
    NB. Start accumulating the display
    if. 2 = 3!:0 DOranks do.
      NB. This is either a noun or a monad/dyad that suppresses detail; verbs have rank stacks
      t =. 'This is a ',((*#DOranks) # 'named '),'noun.'
      if. nounhasdetail *. -. nounshowdetail do.
        t =. ' The value shown is the result of a computation that does not depend on any names.  To see the details of this computation, click anywhere in the value.'
      end.
      disp =. disp , EXEGESISDATASOURCE ; t,LF
    else.
      NB. Not a noun.
      if. 1 +./@:< #@;:@> (<DLRCOMPEND) -.~ texts =. a: -.~ 0 {"1 displaylevrank do.
        t =. hoverDOdataposverbtutorialfinal
      elseif. 1 < #texts do.
        t =. hoverDOdataposverbtutorialcompound
      elseif. do.
        t =. hoverDOdataposverbtutorialsimple
      end.
      NB. Insert tutorial information for the data area, depending on the complexity of the rank stack
      disp =. disp , EXEGESISDATAVERBTUTORIAL ; t
      disp =. disp , exegesisverbdesc 0
    end.
    if. sellevel <: #selections do.
      NB. Display the shape of the result
      rshape =. 0{::valueformat
      disp =. disp , EXEGESISDATASHAPE ; 'This is ',(exegesisindefinite exegesisfmtcell (0;0) ,~ rshape;''),'.',LF,LF

      NB. y is y,x within the display rectangle.  Convert that to offset within the display of the entire noun, by adding
      NB. the offset of the top-left corner of the displayed box, and subtracting the display position of the normal
      NB. top-left, which position is 0 for unboxed, but at a boxmargin for boxed values
      selx =. valueformat yxtopathshape BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + hoveryx
      NB. Convert the isf to a path.
      endempty =. _1 = {: > {: path =. {."1 selx
      t =. 'You are hovering over the atom ' , (endempty{::'with path=';'with path beginning ') , (isftodisplayablepath selx),'.'
      NB. Calculate the type and value of the selected atom
      if. endempty do.
        NB. The path ends on an empty.  Discard the last selection, which will leave us pointing to the empty array
        path =. }: path
        t =. t , '  Because the value is empty, only a prefix of the path can be determined.'
      end.
      NB. We can't get an accurate type for an empty path.  If the result is coming from
      NB. a monad/dyad execution, the empty value may have changed type (don't know why).
      if. #path do.
        selatom =. path {:: fillmask frameselresult selresult
        t =. t , LF,'Type=(',((2 ^. 3!:0 selatom) {:: jdatatypes),')'
        if. -. endempty do.
          t =. t , ', Value=',(":!.10 selatom)
        end.
      end.
      disp =. disp , EXEGESISDATAPATH ; t,LF,LF
 
      NB. If this has rank higher than 2, explain the display
      if. 2 < #rshape do.
        if. 2 | #rshape do.
          NB. Odd number of axes
          t =. 'This array is displayed as a list of ', (":{.rshape) , ' ' , (; }: , ,.&(<' of ') _2 <@(' tables' ,~ ":@{. , 'x' , ":@{:)\ }.rshape) , '.'
        else.
          t =. 'This array is displayed as a ', ((":@{. , 'x' , ":@{:) 2{.rshape) , ' table of ' , (; }: , ,.&(<' of ') _2 <@(' tables' ,~ ":@{. , 'x' , ":@{:)\ 2 }.rshape) , '.'
        end.
        disp =. disp , EXEGESISDATAARRANGEMENT ; t,LF,'Boundaries above rank 2 are indicated by blue lines, with wider lines used for higher boundaries.',LF,LF
      end.
    end.
    NB. If the window is explorable, but the user hasn't created an explorer window, tell him about that option
    if. 1 < #DOsize do.
      if. 0=#winhwnd do.
        disp =. disp , EXEGESISDATAEXPLORABLE ; 'This value is larger than the largest allowed on the main display. You can (1) right-click-and-drag the handle in the lower right corner to resize the window; (2) change the maximum size of all blocks on the Sizes menu; (3) right-click the data to open a separate window for exploring this value.',LF
      elseif. exp=0 do.
        disp =. disp , EXEGESISDATAEXPLORABLE ; 'Right-click the data to bring up the explorer window.',LF
      elseif. do.
        disp =. disp , EXEGESISDATAEXPLORABLE ; 'Right-click the data to destroy the explorer window.',LF
      end.
    end.
    NB. Describe the options for putting the data onto the clipboard.
    select. #DOshapes
    case. 1 do.
      disp =. disp , EXEGESISDATACLIPINFO ; 'Right-click in the shape line to copy this result to the clipboard.',LF
    case. 2 do.
      disp =. disp , EXEGESISDATACLIPINFO ; 'Right-click in the shape line to copy this result to the clipboard; right-click in the selection line to put the cell it selects ',(DOshapehasfill # '(before fill) '),'onto the clipboard.',LF
    case. do.
    end.
    text =. exegesisgrammar disp
  end.
else.
  NB. Hover in the scrollbars, ignore
  text =. ''
end.
reflowtoscreensize text
)
statlineDOdatapos =: 4 : 0
exp =. x
hoveryx =. y
dhw =. (<exp,1) { DOdatapos
if. 0 = +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTHPIXELS * |. exp { displayscrollbars do.
  if. 2 = 3!:0 DOranks do. text =. 'noun'&[^:(0=#) DOranks
  else. text =. defstring 0
  end.
  if. sellevel <: #selections do.
    selx =. valueformat yxtopathshape BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + hoveryx
    text =. text , '    path=' , isftodisplayablepath selx
    NB. Do not include the type/value because this requires instantiating the value and that
    NB. might take a noticeable amount of time
  end.
else. text =. ''
end.
text
)

NB. tooltip messages
'PICKTOOLTIPMSGOK PICKTOOLTIPMSGNOFRAME PICKTOOLTIPMSGNOSELYET PICKTOOLTIPMSGPREVERR PICKTOOLTIPMSGEMPTY PICKTOOLTIPMSGNOMORESEL PICKTOOLTIPMSGNOORIDE PICKTOOLTIPMSGFILLED' =: i. # PICKTOOLTIPMSGS =: <;._2 (0 : 0)

unselectable - no frame
you must make a higher-level selection before you can select this result
cell was not executed - previous error
frame contains 0 - there are no items to select
no further selection possible

cell was not executed - added by fill
)

NB. Custom selection, used in picking.  If this returns 1, it means that the pick has been handled in the locale
selectionoverride =: PICKTOOLTIPMSGNOORIDE"_
postselectionoverride =: PICKTOOLTIPMSGNOORIDE"_

NB. For all these verbs, x is (button flags,view number), y is the yx position of the click relative to start of pickrect

picklDOdatapos =: 4 : 0
QP^:DEBPICK 'valueformat scrollpoints y '
NB. Click in the data region.
'sd exp' =. x
NB. If the click is in the scrollbar, handle scrolling
NB. See which scrollbar, if any, the click is in
dhw =. (<exp,1) { DOdatapos
select. +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTHPIXELS * |. exp { displayscrollbars
case. 1 do.
NB. sclick is the mask indicating which axis was selected
NB. select the information for the selected axis, for analysis
  'trav clickpos end bindlist spt' =. (sclickx =. sclick i. 1)&{&.> (exp{scrolltravelers);y;shw;(1 2{valueformat);(exp{scrollpoints)
  QP^:DEBPICK 'trav clickpos end bindlist spt '
NB. Classify the click as +-creep, +-page, or click in traveler
  assert. (-: /:~)SCROLLBARENDTHICKNESSPIXELS,trav,end-SCROLLBARENDTHICKNESSPIXELS
  select. clickpos I.~ scrollbarsections =. SCROLLBARENDTHICKNESSPIXELS,trav,end-SCROLLBARENDTHICKNESSPIXELS
  NB. If creep, move to the next index, or one screenful, whichever is smaller
    case. 0 do. NB. creep back
      newspt =. (spt (I.~ { 0 , ]) >bindlist) >. spt - end   NB. prev item, but no more than 1 screenful
    case. 4 do. NB. creep forward
      newspt =. ((>:spt) (I.~ { ]) >bindlist) <. spt + end   NB. next item, but no more than 1 screenful
  NB. If scroll, move to the start of last cell that is displayed on the screen, or one screenful if that would not progress
    case. 1 do.   NB. scroll back
      newspt =. ((spt-end) ([ [^:(spt <: ]) I.~ { ]) 0 , >bindlist)
    case. 3 do.  NB. scroll forward
      newspt =. ((spt+end) ([ [^:(spt >: ]) I.~ { 0 , ]) >bindlist)
  NB. If click in traveler, start waiting for mmove events
    case. 2 do.
      newspt =. spt   NB. keep spt unchanged
  NB. Remember what we need for handling the mouse movement:
  NB. which locale is scrolling (for main view); which axis is scrolling; starting clickpos (on the entire isigraph control) for that axis;
  NB. start/trav/end limits for that axis (on the entire isigraph control)
      dwo =. sclickx { exp { DOyx + 0 {"2 DOdatapos  NB. Data Window Offset in selected window
      'scrollingtype__COINSTANCE scrollinglocale__COINSTANCE scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit' =: SCROLLTYPESCROLLBAR;(coname'');sclickx;spt;(clickpos+dwo);(dwo+0 _1 { scrollbarsections);(end -~ {:>bindlist)
  NB.?lintonly scrollinglocale__COINSTANCE =: <'dissectobj'
  NB.?lintsaveglobals
  NB.?lintonly case. do. newspt =. 0
  end.
NB. Clamp the scrollpoint to keep the window entirely within the noun
  newspt =. 0 >. (end -~ {:>bindlist) <. newspt
  
NB. If the scrollpoint changed, remember the new value and call for a redraw of the modified window
  if. newspt ~: spt do.
    scrollpoints =: newspt (<exp,sclickx)} scrollpoints
    exp drawDOvnall ''
    if. 0 = exp do. glpaint'' end.
  end.

case. 0 do.
NB. Not scrollbar.  Find the indexes of the clicked cell
NB. Find the y,x position of the click and go process it
  if. PICKTOOLTIPMSGOK = selres =. exp processdataclick y do.
NB. If the selection changed, redraw the screen.  If the selection was from the explorer, change the scroll in the main view
NB. to show the selected cell at top-left
    if. exp = 1 do.
      scrollpoints =: (flatyxtopixel 1 pixeltoflatyx y) 0} scrollpoints
    end.
    dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
  else.
NB. User tried to select, but we couldn't do it.  Give him a tooltip.
    NB. If we are in an explorer, stay here; but if on the main form, we have to switch to that locale
    formloc =. exp { COCREATOR,coname''
    NB.?lintonly formloc =. <'dissect'
    ('data';coname'') drawttipwithemphasis__formloc (y + exp { DOyx + 0 {"2 DOdatapos) ; selres { PICKTOOLTIPMSGS
    hoversessmin__COINSTANCE =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
  end.
end.
)

NB. right click - if explorable, create the explorer, or raise it if it already exists
pickrDOdatapos =: 4 : 0
if. 1 < #DOsize do.
  if. 0 = #winhwnd do. createexplorer''
  else. wd 'psel ' , winhwnd , ';pshow;setfocus dissectisi'
  end.
end.
)

NB. Click on a resize handle
NB. save a screenshot, and enough info for mouse-move to do the resizing
picklDOresizepos =: 4 : 0
'sd exp' =. x
NB. Ignore resize in explorer.  The handle is not displayed, but the pick window is there
if. exp do. '' return. end.
NB. Read the pixels in the image
pickpixels__COINSTANCE =: (, glqpixels) 0 0 , |. windowsize =. (|. glqwh'')
'scrollingtype__COINSTANCE scrollinglocale__COINSTANCE pickscrollcurryx__COINSTANCE' =: SCROLLTYPESIZEDATA;(coname'');(1 0 { sd)
NB. There's a minimum size; but also don't try to resize width to smaller than the label.
NB. Clear starting x position to get true width
minsize =. MINRESIZABLE >. 0 , (<1 1) { topsize =. brect 0 (<a:;0 1)} (brect DOlabelpos) ,: (brect DOshapepos)
NB. The maximum size is the size of the data, but also limited by screen size.  Remove top header from allowed size
maxsize =. (extractDOLsize valueformat) <. <. RESIZEMAXFRAC * windowsize - ((<1 0) { topsize),0
NB. pickscrollinfo is table of yx: tl of data,min,max,startsize,startcursor
pickscrollinfo__COINSTANCE =: ((exp { DOyx) + (<exp,0) {  DOdatapos) , minsize , maxsize , ((<exp;,1) { DOdatapos) , pickscrollcurryx
''
)

cocurrent 'dissect'

NB. Utility to look up a yx offset in a set of pickrects
NB. x is pick rectangles, YX,;HW
NB. y is y,x
NB. m is number of hits to return, first m if positive, last m if megative
NB. result is table of (index to hit rect);relative y,x in rect    empty if no hits
findpickhits =: 1 : 0
:
if. #index =. (+/\"2 x) I.@:(*./"1)@:(>/"2)@:(<:"1) y do.
  NB. Because the resize handle overlaps the data, we return only the last hit if any
  index =. m {. index
  index ;"0 1 y -"1 (<index;0) { x
else. 0 2$a:
end.
)

NB. ****** tooltips *****

NB. J602 emulation of ptimer.  On QT, or if tooltips disabled on J6, we will define as dissect_timer and immediately overwrite
NB. On J6 with tooltip, this redefines the timer handler
((IFQT +. -. ALLOWNONQTTOOLTIP) {:: 'sys_timer' ; 'dissect_timer') =: 3 : 0
NB.?lintonly runningtimerloc_dissect_ =. <'dissect'
if. #l =. runningtimerloc_dissect_ do.
  try.
    dissect_timer__l ''
  catch.
    smoutput 'error in timer'
    smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
    smoutput 13!:12''
    wd 'timer 0'  NB. stop error loop
  end.
else.
  wd 'timer 0'
end.
)

NB. When the timer expires, perform the hover action.  Runs in instance locale
dissect_timer =: 3 : 0
NB.?lintonly wdtimer =: wd
QP^:DEBMOUSE'timer:hwnd=?winhwnd wd''qhwndp'' >runningtimerloc >coname'''' winhwnd__runningtimerloc '
wd 'psel ' , winhwnd__runningtimerloc 
hoverdo__runningtimerloc''
wdtimer 0  NB. Must do this last, as it modifies runningtimer
0 0$0
)

NB. We have internal actions hoverstart, hoverend, hoverdo.  hoverstart is called when the mouse
NB. moves; we record where it was and set a timer, abandoning the old timer if the mouse has moved.
NB. hoverend is called whenever anything happens to abort the hover (click, focuslost, etc).  hoverdo
NB. is called when the timer expires: we then see where the cursor is and call the owner to get a tooltip.

NB. y is the mouse position yx.  Start/continue a hover timer, clearing an old one if the mouse has moved
MAXHOVERMOVEMENT =: 6 8   NB. Allow this much movement from start-of-hover position, depending on button status
'HOVEROFFSETY HOVEROFFSETX' =: _8 5  NB. amount to offset tooltip from the hover
CURSORYSIZE =: 20  NB. Our estimate of cursor size
CURSORXSIZE =: 10

NB. This is called in whichever locale and psel of whatever form is active - the main or an explorer.
NB. The timer will run in the main form
NB. x is ((window type 0=main 1=exp),(1 to write to the statline (which doesn't exist on explorers)));(sysdata at tme of click)
hoverstart =: 4 : 0
'opts sd' =. x
'isexp writestat' =. opts
NB. Is a button down?  We will use that to calculate the jiggle tolerance
buttonisdown =. 1 e. 4 5 { sd
if. #hoverinitloc do.  NB. We are hovering.  Does this continue the same hover?
  if. (buttonisdown { MAXHOVERMOVEMENT) < >./ | y - hoverinitloc do. hoverend'' end.
end.
NB. Allow starting a hover only if no button is down (don't hover during dragging of window)
if. (-. buttonisdown) *. 0 = #hoverinitloc do.   NB. If no hover running (and perhaps we just cleared it), start one
NB.?lintonly wdtimer =: wd
QP^:DEBMOUSE'hoverstart:hwnd=?winhwnd wd''qhwndp'' '
  NB. Is it OK to create a tooltip?  Yes if tooltips are enabled, except when control required and not pressed
  if. (0 ~: tooltipdetailx) *. -. tooltipctrl *. 0 = 6 { sd do.
    wdtimer (tooltipdelayx,2) {:: TOOLTIPDELAYCHOICES  NB. start the hover timer
  end.
  hoverinitloc =: y
  hoverisexp =: isexp
  NB. The same action that would start the hover timer will set the status line immediately - even if hover disabled
  if. writestat do. statlinedo y end.
  NB.?lintsaveglobals
end.
)

NB. Nilad.  Ask the owner for a tooltip and display it if there is one
NB. This runs in the hovering locale, either the main form or an explorer
hoverdo =: 3 : 0
if. #hoverinitloc do.   NB. should always be there, but we might get a late timer event
NB.?lintonly hoverinitloc__COINSTANCE =: 0 0
  NB. Get the locale of the object.  If we are on an explorer, it's just that; if on the main, we have to
  NB. look for the pickrect
  if. hoverisexp do.
    pickloc =. coname''
    yx =. hoverinitloc
  else.
    if. #pr =. locpickrects (_1 findpickhits) hoverinitloc do.
      'l yx' =. {. pr
      pickloc =. l { picklocs
    else.
      pickloc =. ''
NB.?lintonly yx =. 0 0
    end.
  end.
NB.?lintonly pickloc =. <'dissectobj'
  glsel 'dissectisi'  NB. reselect graphics window
  if. #pickloc do.
QP^:DEBMOUSE'hoverdo:hwnd=?winhwnd wd''qhwndp'' hoverisexp pickloc '
    if. 3 = 4!:0 <'hoverDO__pickloc' do.
      hstring =. hoverDO__pickloc hoverisexp;yx
      if. #hstring do. drawtooltip hoverinitloc;hstring end.
    end.
  end.
end.
)

NB. y is mouse position.  Write to the status line, which is a lot like a hover
NB. This runs in the main form only, since statline doesn't exist on explorer
statlinedo =: 3 : 0
stattext =. ''
for_r. pr =. locpickrects (_1 findpickhits) y do.
  'l yx' =. r
  pickloc =. l { picklocs
NB.?lintonly pickloc =. <'dissectobj'
  if. 3 = 4!:0 <'statlineDO__pickloc' do.
    stattext =. statlineDO__pickloc hoverisexp;yx
    statlinehasnotip =: 1   NB. First time we hit something, remove the tip forever
  end.
end.
if. statlinehasnotip do. 'fmstatline' wdsettext stattext end.
0 0$0
)


NB. y is a list of (next positions assuming a start at this position)
NB. Result is the chain of start positions, beginning at 0
startschain =: # (i.~{.]) [: }. (,#) {~^:a: 0:
NB. x is (maximum width in pixels),(width of a space)
NB. y is a list of pixel-widths for each word
NB. Result is a list of starting positions for lines (first one is always 0)
reflowwords =: 4 : 0
'maxwidth spacewidth' =. x
pixelstarts =. +/\ spacewidth + y  NB. starting position of each word, if all words run together
NB. Prepend starting position for first word; then find, for each startposition, the maximum endposition
NB. and then do binary search to find the word at which each startposition would have to break.  Then chase
NB. the chain starting at 0 to find all the startpositions (except the first, 0, which we add)
0 , startschain pixelstarts I. (maxwidth+1+spacewidth) + |.!._1 pixelstarts
)
NB. x is max width of text, y is text
NB. LF indicates mandatory newline, CR is a newline with no break allowed after preceding newline
NB. Result is text, with LF between lines
reflowtooltip =: 4 : 0
NB. Find the width of a space
glfontextent ;:^:_1 ":&.> TOOLTIPFONT
spacewidth =. {. glqextent ' '
NB. Split the string into lines
bl =. (<;.2~   e.&(CR,LF)) y
NB. Split each line into words according to its type
bw =. ((<;._2~  e.&(' ',LF))`(<@}:)@.(CR={:))&.> bl
NB. Calculate size of each word in pixels
sizeword =. {."1@:(glqextent@>)&.> bw
NB. Reflow each string, getting the count of words/line for each output line
intervals =. (x,spacewidth)&reflowwords&.> sizeword
NB. Combine the blocks, with spaces between, and LF after each group
; intervals ([: ; 1:`[`(0 #~ #@])} <@(LF ,~ ;:^:_1);.1 ])&.> bw
)

NB. Get the max width from the screen info
reflowtoscreensize =: 3 : 0
(TOOLTIPMAXPIXELS <. <. TOOLTIPMAXFRAC * 0 { glqwh '') reflowtooltip y
)

NB. y is position;text for a tooltip
NB. x (optional) is anything
NB. drawtooltip, but increase the fontsize if the message and x-value are the same as from the previous click
drawwithemphasishistory =: 0;'';''   NB. click number;x-value;message
drawttipwithemphasis =: 3 : 0
'' drawttipwithemphasis y
:
if. drawwithemphasishistory -: (seqclickno-1);x;1{y do.
  ((, 2&*&.>)/ TOOLTIPFONT) drawtooltip y
else.
  drawtooltip y
end.
drawwithemphasishistory =: seqclickno;x;1{y
)

NB. y is cursor position;string
NB. Draw a tooltip there after saving the pixels
NB. For the dyad, x is the font;size to use
drawtooltip =: 3 : 0
TOOLTIPFONT drawtooltip y
:
'cpos string' =. y
NB. There is a tooltip.  Display it.
glsel 'dissectisi'
NB. Remember where the tooltip is to be drawn.  From now on we check for movement from this spot
hoverinitloc =: cpos
NB. Copy the pixels we are about to overwrite
'ctlx ctly' =. glqwh ''
'hovery hoverx' =. cpos
'ttiph ttipw' =. (TOOLTIPCOLOR;TOOLTIPTEXTCOLOR;x,TOOLTIPMARGIN;'') sizetext <string  NB. kludge
NB. Find the portion of the window that is currently visible.  We want the tooltip there
'onscreent onscreenl onscreenb onscreenr' =. , +/\ findonscreenyxhw''
NB. Position the tooltip to be on screen.  We try to put the bottom-left corner at the hover offset, above the cursor
NB. Get desired top position; if it's off the top of the screen, switch to the right of the hover
ttipx =. hoverx
if. onscreent > ttipy =. HOVEROFFSETY + hovery - ttiph do.
  NB. default too high: try to position to the right of the cursor
  ttipy =. hovery
  ttipx =. ttipx + HOVEROFFSETX >. CURSORXSIZE
end.
NB. If that's too far right, we back the x onto the screen (and then right if offscreen), and drop the y to below the cursor
NB. Get desired left position, but if that goes offscreen right, move left; then if offscreen left, move right
if. onscreenr < ttipx + ttipw do.
  ttipx =. 0 >. ctlx - ttipw  NB. back x onto the screen
  NB. If we were trying the right of the cursor position, it dodn't work, drop below cursor
  if. ttipy = hovery do. ttipy =. hovery + CURSORYSIZE end.
else.
  NB. The tooltip fits to the right of the cursor (i. e. ttipx is ok).
  NB. If it runs off the bottom of the onscreen area, move it up till it fits
  if. onscreenb < ttipy + ttiph do.
    ttipy =. onscreent >. onscreenb - ttiph
  end.
end.

NB. That's the topleft of the tooltip.  Now calculate the rectangle that we will use to save the pixels
NB. We have to save an extra pixel all the way around (seeming glrect error), and we have to make sure
NB. that the rectangle is all onscreen, else QT will crash
ttpyx =. 0 >. _1 + ttipy,ttipx
ttphw =. (2 + ttiph,ttipw) <. (ctly,ctlx) - ttpyx
tooltippixels__COINSTANCE =: (, glqpixels) 1 0 3 2 { ttpyx,ttphw
tooltiplocale__COINSTANCE =: coname''  NB. Remember the locale that the tooltip was drawn in
(TOOLTIPCOLOR;TOOLTIPTEXTCOLOR;x,TOOLTIPMARGIN;'') drawtext string;2 2 $ ttipy,ttipx,ttiph,ttipw  NB. kludge
glpaint''
NB.?lintsaveglobals
)

NB. Nilad.  Turn off the hover timer.  If a tooltip is active, restore the pixels it covered
hoverend =: 3 : 0
hoverinitloc =: $0
NB.?lintonly wdtimer =: wd
wdtimer 0
if. 0 = 4!:0 <'tooltippixels__COINSTANCE' do.
  NB. if the tooltip has a minimum lifetime, delay until that lifetime has been exceeded (kludge).  Should be short
  if. 0.01 < reqddelay =. hoversessmin - 6!:1'' do. 6!:3 reqddelay end.
  NB. select the screen the pixels were drawn in
  wd 'psel ' , winhwnd__tooltiplocale__COINSTANCE
  glsel 'dissectisi'
  glpixels tooltippixels__COINSTANCE
  glpaint''
  4!:55 <'tooltippixels__COINSTANCE'
end.
)



NB. **************************** mouse events in the graphics window ********************
NB. in the locale of the main form

NB. pick flags
'PICKLB PICKRB PICKCTRL PICKSHIFT' =: |. 1 bwlsl~ i. 4

NB. The sequential click number increments for every mouse-click.  We use it to detect when
NB. the user clicks twice in the same place - we emphasize the message then
seqclickno =: 1

NB. mouse button, both left and right.  Return number of picks performed.
NB. x is l or r, y is formatted sysdata
dissect_dissectisi_mbdown =: 4 : 0
hoverend''
sentencehovercheck 1 0 { y
seqclickno =: >: seqclickno
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
for_r. pr =. locpickrects (1 findpickhits) 1 0 { y do.  NB. Return FIRST hit so sentence has priority over data blocks
  'l yx' =. r
  pickloc =. l { picklocs
NB.?lintonly pickloc =. <'dissectobj'
  if. 3 = 4!:0 <'pickDO__pickloc' do.
    x pickDO__pickloc 0;yx;y;l
  end.
end.
#pr
NB.?lintsaveglobals
)

dissect_dissectisi_mbldown =: 3 : 0  NB. Always in form locale
debscroll^:DEBSCROLL 'dmbldown';<y 
NB. In case a button sequence was interrupted, clear button state
dissect_dissectisi_mblreset 1
NB. If the user left-clicked outside a pickrect, that is the start of a highlight or scroll operation.
NB. Remember the clicked position, and the pixels in the screen (we use the presence of
NB. the screen buffer as an indicator of scroll-in-progress, and delete it when we're done,
NB. since it's big)
NB.?lintonly pickscrollinfo =: 5 2 $ 0
if. 0 = 'l' dissect_dissectisi_mbdown sd =. 0 ". sysdata do.
  NB. We will start some kind of scroll/highlight.  Save the pixels
  winsize =. |. glqwh ''  NB. y,x of control.  Mustn't read outside!
NB. Read the pixels in the sentence, and from the end of the sentence area to the bottom of the screen
  picksentencepixels =: (, glqpixels) 1 0 3 2 { , picksentencerect =. ({. ,: winsize <. {:)&.(+/\) topbrect
  scrollblock =. -~/\ (0 (1}) {: picksentencerect) ,: winsize
  pickpixels =: (, glqpixels) , |."1 scrollblock

  NB. See which operation it is: wire highlighting or scroll
  if. #nettohighlight =. wirehighcheck 1 0 { sd do.
    scrollingtype =: SCROLLTYPEWIREHIGH
    NB. Draw the selected net in highlight color
    glclipreset''
    glrgb WIREHIGHCOLOR
    glpen 3,PS_SOLID
    gllines 1 0 3 2 {"1 nettohighlight
    glpaint''
  else.
    scrollingtype =: SCROLLTYPEIMAGE
    pickscrollcurryx =: pickscrollstartyx =: 1 0 { sd
    NB.?lintsaveglobals
  end.
end.
''
)


NB. init left-click status to 'idle'
NB. Before a left click, or after a mouse-up or double-click, we clear the scrolling state
NB. to protect against lost events.
NB. y indicates what we are clearing:
NB. 0=clear mouse-down  info, leave mouse-up for double-click
NB. 1=clear everything
dissect_dissectisi_mblreset =: 3 : 0   NB. always in form locale
select. scrollingtype
case. SCROLLTYPEIMAGE do.
  4!:55 ;: 'pickpixels picksentencepixels'  NB. release memory
case. SCROLLTYPESCROLLBAR do.
  4!:55 ;: 'scrollinglocale scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit'  NB. indicate end-of-scrollbar
case. SCROLLTYPESIZEDATA do.
  4!:55 ;: 'pickpixels picksentencepixels pickscrollcurryx'  NB. release memory
case. SCROLLTYPEWIREHIGH do.
  NB. Undo the highlighting, then release highlight pixels
  NB. End of wire highlighting.  Restore the original pixels
  glpixels pickpixels
  glpaint''
  4!:55 ;: 'pickpixels picksentencepixels'  NB. release memory
case. do.
end.
NB. After clearing saved info, revert to idle state
scrollingtype =: SCROLLTYPENONE
NB. if not mouse-up, clear the mouse-up info
if. y do. lastscrollingtype =: SCROLLTYPENONE end.
''
)

NB. y is mouse position.  Result is net to highlight (table of x y x y x wires), or empty if
NB. nothing to highlight
wirehighcheck =: 3 : 0
NB. See how far the mouse has moved
mdist =. +/ | wirehighmousepos - y
NB. Remember new mouse position for next time
wirehighmousepos =: y
NB. See if we need to refigure anything
if. 0 >: wirehighdisttocheck =: wirehighdisttocheck - mdist do.
  NB. See how far each block is away from recalculation
  NB. 1{mdist has the negative of the accumulated movement since last check.
  NB. See which nets need checking
  if. #chknetsx =. 0 I.@:>: wirehighdisttocross =: wirehighdisttocross + {: wirehighdisttocheck do.
    NB. Recalculate any blocks that might be 0 or below
    wirehighdisttocross =: (newdists =. chknetsx y&wirehighcheckdist@>@{ wirehighwires) chknetsx} wirehighdisttocross
    NB. Take the net, if any, with the smallest distance
    if. 0 >: newdists {~ minx =. (i. <./) newdists do.
      highnet =. wirehighwires {::~ chknetsx {~ minx
    else.
      highnet =. ''
    end.
  else. highnet =. ''
  end.
  NB. Refigure the distance till next check
  wirehighdisttocheck =: 2 {. <./ wirehighdisttocross
  NB. Return the net to draw, if any
  highnet
else.
''
end.
)

WIREHIGHPROX =: 3   NB. Highlight within this distance from a wire
NB. x is mouse position
NB. y is table of wires
NB. Result is the distance from the wire to the mouse.  If positive,
NB. we can delay that much movement before reevaluating
NB. If <: 0, indicates proximity, the lower the closer
wirehighcheckdist =: 4 : 0
NB. Get dist of each endpoint from the cursor; get min dist in each direction
disttowirebbox =. +/"1 (<./"2@:| * =/"2@:*) transwires =. ((($,)~ 2 2 ,~ #) y) -"1 x
NB. Get min dist of all wires; if > min, result is dist to min
if. WIREHIGHPROX < minbbox =. <./ disttowirebbox do. minbbox - WIREHIGHPROX return. end.
NB. Some wire is near the cursor.  Calculate the perpendicular distance to cursor
proxwires =. (proxwirex =. WIREHIGHPROX I.@:>: disttowirebbox) { transwires
proxdist =. <. -: (-/ . * proxwires) % +/"1&.:*: -/"2 proxwires
NB. Min dist is the result
disttowirebbox =. proxdist proxwirex} disttowirebbox
(<./ disttowirebbox) - WIREHIGHPROX
)

dissect_dissectisi_mbrdown =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
NB. In case mbrup omitted, clear mouse state
startdebuginfo =: 0$a:
'r' dissect_dissectisi_mbdown 0 ". sysdata
''
)

NB. mouse movement.  If we are scrolling, drag the pixels along
NB. If we are dragging a scrollbar, vector to the object locale to handle that
dissect_dissectisi_mmove =: 3 : 0   NB. always in form locale
debscroll^:DEBSCROLL 'dmmove';<y 
QP^:DEBMOUSE'mainmmove:hwnd=?winhwnd wd''qhwndp'' sysdata '
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
sd =. 0 ". sysdata
select. scrollingtype
case. SCROLLTYPEIMAGE do.
  pickscrollcurryx =: 1 0 { sd
  glclear''
  scrollblock =. -~/\ (0 (1}) {: topbrect) ,: 3 2 { sd
  pickpixels =: (|. ({. scrollblock) + pickscrollcurryx - pickscrollstartyx) 0 1} pickpixels
  glpixels pickpixels
  glpixels picksentencepixels
  glpaint''
case. SCROLLTYPESCROLLBAR do.
NB. Perform the scroll, on the main window, but in the locale of the data
  0 scrollmmove__scrollinglocale 1 0 { sd
case. SCROLLTYPESIZEDATA do.
  NB. redraw the original screen to erase any previous box
  glpixels pickpixels
  NB. Draw the resizing rect, from the start of data to the audited cursor position
  NB. pickscrollinfo is table of yx: tl of data,min,max,startsize,startcursor
  ('';RESIZERECTCOLOR) drawrect ,:`>.`<.`+`(-~)/ pickscrollinfo , pickscrollcurryx =: 1 0 { sd
  glpaint''
case. SCROLLTYPEWIREHIGH do.
  NB. If we move during wire highlighting, keep updating the odometers (to spread load), but
  NB. take no other action
  wirehighcheck 1 0 { sd
case. do.
  NB. mmove not for scrolling.
  NB. Update the wire-scrolling odometer
  wirehighcheck 1 0 { sd
  NB. If mouse is in the sentence, perform the sentence-hover action to highlight the
  NB. hovered-over block
  if. -. sentencehovercheck 1 0 { sd do.
    NB. Hover not in sentence. Set radius to use depending on whether a button is down
    NB. If button down, use larger radius but don't allow a new tooltip to start
    (0 1 ; sd) hoverstart 1 0 { sd
  end.
end.
i. 0 0
)

NB. y is mouse position
NB. We check to see if the mouse is over the sentence or a block.  If so, start highlighting it
NB. If not, stop highlighting it.  If the selected token changes, handle that correctly
NB. Result is 1 if we are in the sentence
sentencehovercheck =: 3 : 0
sentencetok =. $0
blockhloc =. 0$a:
if. #pr =. locpickrects (1 findpickhits) y do.  NB. Return FIRST hit so sentence has priority over data blocks
  'l yx' =. {. pr  NB. Get index and offset position
  if. 1 = l do.
    NB. In the sentence rectangle.  Find the matched token
    'cfms txts rects locs' =. 1 1 {:: topinfo
    if. #r =. rects (_1 findpickhits) yx do.
      'ix pyx' =. {.r
      if. (<0) ~: tokl =. ix { locs do.  NB.?lintonly tokl =. <'dissectobj'
        NB. The locale exists, but it may not have been initialized for display.  In that
        NB. case, don't highlight it in the sentence
        if. #DOyx__tokl do. sentencetok =. ix end.
      end.
    end.
  elseif. l >: #topinfo do.
    NB. Over a block.  Save the locale of the displayed entity
    blockhloc =. l { picklocs
  end.
end.
NB. If there is no change to the highlight, keep it as is
if. (sentencetok -.@-: sentencehovertok) +. (blockhoverloc -.@-: blockhloc) do.
  NB. There is a change to the sentence highlight.  Remove the old one if any
  sentencehoverend''
  NB. Clear any active hover, so that it doesn't mess up our highlighting
  hoverend''
  NB. Draw the new highlight, if any
  sentencehovertok =: sentencetok
  blockhoverloc =: blockhloc
  sentencehoverdraw''
end.
#sentencehovertok
)

NB. sentencehovertok tells what token, if any, we are hovering over.  If we are, save the screen and draw
NB. the highlight rectangles
sentencehoverdraw =: 3 : 0
if. sentencehovertok +.&# blockhoverloc do.
  NB. Save the pixels from the screen, in two blocks: the sentence, and the rest of the screen
  winsize =. |. glqwh ''  NB. y,x of control.  Mustn't read outside!
  picksentencerect =. ((0 >. {.) ,: winsize <. {:)&.(+/\) (_1 2 * <. -: >: SENTENCEHIGHRECTHIGHWIDTH) + topbrect
  sentencehoverpixelss =: (, glqpixels) 1 0 3 2 { , picksentencerect
  scrollblock =. -~/\ (0 (1}) {: picksentencerect) ,: winsize
  sentencehoverpixelsb =: (, glqpixels) , |."1 scrollblock
  NB. Get the locale to be displayed.  We know there must be one.
  NB. For sentence hover, highlight the blocks too
  if. 0 = #slocale =. blockhoverloc do.
    slocale =. <(1 1;3;sentencehovertok) {:: topinfo
    NB.?lintonly slocale =. <'dissectobj'
    ('';SENTENCEHIGHRECTPEN) drawrect DOyx__slocale ,:&{. DOsize__slocale
  end.
  NB. Get all the sentence rectangles that display in the same locale as the one being hovered over.
  NB. This is a good way to show how multiple verbs can end in the same block
  srect =. > (slocale = (1 1;3) {:: topinfo) # (1 1;2) {:: topinfo
  NB. Draw the highlighting rectangles
  ('';SENTENCEHIGHRECTPEN) drawrect srect +"2 (1 { locpickrects) * 1 0
  glpaint''
NB.?lintsaveglobals
end.
''
)

NB. If we are hovering over the sentence, restore the pixels and release them
NB. DO NOT CLEAR sentence-hover status because we may be about to repaint and immediately highlight,
NB. which we must be able to do with no mouse movement; but DO clear the block-hover locale, because
NB. we may have redrawn the screen & the hovered-over block may have moved
sentencehoverend =: 3 : 0
if. sentencehovertok +.&# blockhoverloc do.
  NB. Restore the unhighlighted sentence; restore the object only if we were
  NB. hovering over the sentence.  If we were hovering over the object, we may have scrolled,
  NB. and the original pixels are obsolete
  if. #sentencehovertok do.
    glpixels sentencehoverpixelsb
  end.
  glpixels sentencehoverpixelss
  glpaint''
  4!:55 <'sentencehoverpixelsb sentencehoverpixelss'  NB. Remove to save space
  blockhoverloc =: 0$a:
end.
''
)

NB. mouse release.  If we are scrolling, set the new offset and redraw
NB. If we are dragging a scrollbar, vector to the object locale to finish that
dissect_dissectisi_mblup =: 3 : 0   NB. always in form locale
debscroll^:DEBSCROLL 'dmblup';<y 
NB. On mouse-up, save the scrolling type for use if there is a doubleclick
lastscrollingtype =: scrollingtype
hoverend''
select. scrollingtype
case. SCROLLTYPEIMAGE do.
NB. Use the last-drawn position as the new position.  If it hasn't changed from the original, don't bother to redraw
  if. pickscrollcurryx -.@-: pickscrollstartyx do.
    scrolltlc =: <. scrolltlc + pickscrollcurryx - pickscrollstartyx
    dissect_dissectisi_paint 0  NB. no need to recalc placement
  end.
case. SCROLLTYPEWIREHIGH do.
case. SCROLLTYPESCROLLBAR do.
case. SCROLLTYPESIZEDATA do.
  NB. Save current info in case of doubleclick
  lastscrollingtype =: lastscrollingtype , (ifinlocale__scrollinglocale 'maxnoundisplaysizes') , (2 { pickscrollinfo)
  NB. pickscrollinfo is table of yx: tl of data,min,max,startsize,startcursor
  NB. extract the hw of the last pick.  Set as the size to use in the selected locale.  If the
  NB. the cursor didn't move, do nothing (to speed up response in case of double-click)
  if. pickscrollcurryx -.@-: {: pickscrollinfo do.
    maxnoundisplaysizes__scrollinglocale =: (]`>.`<.`+`(-~)/ pickscrollinfo , pickscrollcurryx) 0} maxnoundisplaysizes
    dissect_dissectisi_paint 1  NB. recalc placement with new sizes
  else.
    NB. redraw the original screen to erase any previous box, in case there is one
    glpixels pickpixels
  end.
end.
NB. Reset to 'button-up' state, but leave lastscrollingtype for possible double-click
dissect_dissectisi_mblreset 0
NB.?lintsaveglobals
)

dissect_dissectisi_mbrup =: 3 : 0
hoverend''
NB. If there is a sandbox, execute it
if. #startdebuginfo do.
  NB. Debug info is (<locale of original verb);(arg tbl);(execution locale)
  sdloc =. {. startdebuginfo  NB.?lintonly ] <'dissectverb'
  startdebug__sdloc }. startdebuginfo
  startdebuginfo =: 0$a:
end.
)

dissect_dissectisi_focuslost =: 3 : 0
hoverend''
sentencehoverend''
)

dissect_dissectisi_mbldbl =: 3 : 0
hoverend''
sentencehovercheck 1 0 { 0 ". sysdata
select. {. lastscrollingtype
case. SCROLLTYPEIMAGE do.
  NB. Put sizing back under automatic control
  autosizestate =: 0
  NB. Draw, without changing the placement
  dissect_dissectisi_paint 0
case. SCROLLTYPESIZEDATA do.
  NB. In this case lastscrollingtype contains info about the click
  NB. If maxnoundisplaysizes was previously defined, clear it, to revert to original size
  if. 1 { lastscrollingtype do.
    4!:55 <'maxnoundisplaysizes__scrollinglocale'
  else.
  NB. If not, set to max possible size
    maxnoundisplaysizes__scrollinglocale =: (2 3 { lastscrollingtype) 0} maxnoundisplaysizes
  end.
  NB. Redraw with the new sizes
  dissect_dissectisi_paint 1
end.
NB. Reset to button-up state
dissect_dissectisi_mblreset 1
)


NB. Resize happens at the beginning for QT to kick off display
dissect_dissectisi_resize =: 3 : 0
NB. Make autosizestate negative to indicate user in control
NB. On initial event, take resize from 1 to 0 to avoid repeating initial sizing
autosizestate =: <: autosizestate
NB. We have already calculated the drawing; it doesn't change.  So tell paint not to recalculate it
dissect_dissectisi_paint 0
)

cocurrent 'dissectobj'

NB. Handle mouse movement for scrolling.  Called in the locale whose data is being displayed.
NB. x is the window number to draw, y is the yx of the click.  Globals were set at the start of the scroll
NB. to allow us to figure out the new scrollpoint.
scrollmmove =: 4 : 0
exp =. x  NB. 1 for explorer
NB. select the coordinate for the scrolling axis
c =. scrollingaxis { y
dsize =. ((1+scrollingaxis);_1) {:: valueformat
NB. move the original scrollpoint by the fraction of the data size corresponding to the fraction of mouse movement to scrollbar size
newspt =. <. scrollingorigscrollpt + dsize * (c-scrollingorigclick) % -~/ scrollinglimits
NB. Clamp the scrollpoint to keep the window entirely within the noun
newspt =. 0 >. scrollptlimit <. newspt
NB. If the scrollpoint changed, remember the new value and call for a redraw of the modified window
if. newspt ~: (<exp,scrollingaxis) { scrollpoints do.
  scrollpoints =: newspt (<exp,scrollingaxis)} scrollpoints
  exp drawDOvnall ''
  if. 0 = exp do. glpaint'' end.
end.
i. 0 0
)


NB. x is view number, y is yx (in the window)
NB. Result is (x,y) in the flattened display within which the yx falls
NB. We discard the last value to cause all off-display values to map to the last value
pixeltoflatyx =: 4 : 0
(>: (x{scrollpoints) + y) (I.~ }:)&> 1 2 { valueformat
)

NB. y is (y,x) in the flattened display, or a table of (y,x)
NB. Result is the starting pixel positions (yx) of the cell(s)
flatyxtopixel =: 3 : 0
y ({ 0&,)&>"1 (1 2 { valueformat)
)

NB. x is DOL descriptor, y is flatyx, result is table of (selection);(shape at level)
yxtopathshape =: 4 : 0
NB. If the noun is empty, we make it nonempty for display by discarding all elements of the shape starting with
NB.  the first 0.  We do this so as always to leave something pickable.
if. empty =. 0 e. s =. origs =. 0 {:: x do. s =. truncemptyshape s end.  NB. shape of the noun, both original and as displayed
flatrc =. (>: y) (I.~ }:)&> 1 2 { x  NB. Look up to find containing row/col
NB. Split the shapeused into vert;horiz, ending on horiz.  OK to add high-order 0s to
NB. ensure that there is some infix of length 2.
NB. Convert row/col to indexes.  Interleave the indexes for row/col to get cell indexes.  Remove 0
NB. if it was added
NB. Append _1 to end to indicate empty.
NB. Box the indexlist
indexlist =. (empty # _1) ,~ (-#s) {. , |: (|. |: _2&(]\)&.|. 0 0 , s) #: flatrc
NB. If there is no lower boxing level, indexlist is the result
if. 3 < #x do.
  NB. Boxed noun.  Recur to look up the next level.  Offset the yx to within the subbox
  NB. Start by selecting at this level and dropping down, followed by the later levels
  NB. Offset within the inner box by the margin plus the leading linewidth
  (indexlist ; s) , ((3;indexlist) {:: x) yxtopathshape y - (BOXLINEWIDTH + BOXMARGIN) + flatrc ({ 0&,)&> 1 2 { x
else.
  NB. Return the indexlist as a boxed CSF (rank-3)
  ,: indexlist ; origs
end.
)

NB. Older interface:
NB. x is DOL descriptor, y is flatyx, result is CSF for path within the noun.  No shortcuts are used in the result
yxtopath =: _2 <\ [: }:@, SFOPEN (<a:;1)} yxtopathshape


NB. Nilad.  Returns the next locale in the inheritance chain for the current node.
NB. This will usually be the next in chain, but some nodes (such as u^: when the user is selecting item 0)
NB. don't allow selection; they return an empty to stop the selection search
getnextpickloc_dissect_ =: 3 : 'inheritedfrom' 
recursiveselection_dissect_ =: (PICKTOOLTIPMSGNOFRAME,PICKTOOLTIPMSGNOMORESEL) {~ *@{.@[   NB. x is sellevel at end: 0 if it is 0, _1 otherwise
NB. y is flattened CSF for the current cell (with higher-level selections removed), i. e. a list of {selection[,{SFOPEN..}]}...
NB. The current locale is a node that is displayed in the current box.  We see if the
NB. selection applies at this node; if so, we propagate it to all descendants.  If not,
NB. we go to the next locale in the inheritance chain and give it a chance.  We have to
NB. make sure rthat a selection in u in u@:v is not propagated to v; so it must be seen
NB. as inapplicable to u@:v.
NB. Result is 1 if we made a change and a redraw is needed, 0 if there was no frame at all
NB. (i. e. sellevel = 0 at the end of the chain), _1 if there were selections but no more allowed,
NB. _2 if selection not allowed because of empty in frame, _3 if unexecuted cell
NB. x is (sellevel of this node);(rank of cell of previous verb)
recursiveselection =: 3 : 0
0 _1 recursiveselection y
:
NB. First, try custom selection, and if it made a change, return fast since it finished the pick
if. PICKTOOLTIPMSGOK = selectionoverride'' do. PICKTOOLTIPMSGOK return. end.
'inslevel prevcellrank' =. x
QP^:DEBPICK'selecting in ?defstring]0%coname''''%x%y%initialselection%sellevel%selections%selframe%frame%resultlevel%'
selectionfound =. 0    NB. scalar = no selection
usinginitialselection =. ''
NB. If y is empty (possible if we are running to end), make it an empty list
if. 0 = #y do. y =. ,a: end.
if. (<resultlevel) e. 0;1 do.
  assert. (SFOPEN I.@:= y) -: >: +: i. <. -: #y  NB. every other move should be a dropdown
  if. 1 -: resultlevel do.
    NB. This node selects by level.  Take the appropriate set of boxes from y
    NB. We don't have to worry about forced selections or initial selections through this path
    NB. Negative level means we want to count down from the top: &.> has level _1
    localf =. 2 {. y
  else.
    NB.  This is L:.  Consult the result map to see how much of the selection applies at this node.
    NB. We will always take an even number of boxes, ending with a drop-down.
    NB.?lintonly resultseqmap =: ''
    localf =. y {.~ +: (y -. SFOPEN) >:@]^:(0 < [: L. resultseqmap {::~ {.~)^:_ (1)
    assert. localf <:&# y
  end.
  NB. See if this is a new selection
  if. sellevel >: #selections do. selectionfound =. ,<localf   NB. should never be >
  elseif. localf -.@-: sellevel {:: selections do. selectionfound =. ,<localf
  end.
  residualy =. (#localf) }. y
  NB. The result cells of this verb are boxes, but we will be dropping down into one, so we really don't know what the cell looks like
  thiscellrank =. _1
else.
  NB. This node doesn't set the level; use the frame
  NB. Ignore the frame contributed by a forced selection, since that doesn't show up in the display
  NB. and is therefore not in y.  No initial selection is allowed
  thisverbframelen =. (unforcedselection'') * #selframe
  NB. If there is no previous level, or the last thing was a dropdown which acts like a fresh start,
  NB. use the entire result shape of this verb as the previous cell result-shape.  The result shape is
  NB. the frame concatenated with the max-size result cell.
  if. prevcellrank < 0 do. prevcellrank =. frame +&# maxcellresultshape end.
  NB. prevcellrank tells how big a filled cell of the previous level is.  That will be filled at this level
  NB. by 3 things: (A: leading axes added by fill at the previous level),(B: frame at this level),(C: cell result at this level).
  NB. We need to pull the axes from y that correspond to B to be this level's selection, and to discard
  NB. A and B from the axes passed on to the next selection level.
  NB. If selframe is larger than frame (happens for u^:n), it means that the selection actually reaches inside the cell.
  NB. So we have to make those axes available for selection
  filledframe =. prevcellrank - ((#maxcellresultshape) - selframe -&# frame)
  assert. filledframe >: thisverbframelen
  localf =. (- thisverbframelen) {. filledframe {. frame1 =. 0 {:: y
  NB. We append the initial selection (if any) of the FIRST click that activates the expansion node.  If
  NB. this node has no selframe, the first click is recognized by non-existence of later selection.
  NB. If this node has selframe, the first click is one that DOES NOT set selectionfound, provided
  NB. no later selection exists (this means the first click selected at this node, and we will let this next
  NB. click send on the initialselection). We have to
  NB. do it this way because the initialselection may be in a v-type rather than a u-type, and the v-type
  NB. is not in the inheritance chain.
  NB. If this node has an initialselection and also a selection, that means that it has triggered an expansion
  NB. already.  In that case, a click on it REMOVES the expansion.  We detect this when the selection here
  NB. has values beyond this selection level
  if. 0 = #selframe do.
  NB. If this node has no frame, it has nothing to add to the conversation; but the initialselection might
      if. sellevel >: #selections do.
        if. #initialselection do. selectionfound =. usinginitialselection =. ,initialselection end.
      else. selectionfound =. 0$a:
      end.
  else.
    NB. This node has frame.  Look at the leading elements of y.  If they don't match the current
    NB. selection (or if there is no current selection), we've seen enough: the selection starts
    NB. at this level.
    NB. But: if the frame contains 0, no selection can be valid (it will perforce contain an index error)
    NB. and we must abort with a status code indicating the fact
    if. (0 e. selframe) +. (_1 e. localf) do. PICKTOOLTIPMSGEMPTY return. end.
    if. 0 = #localf do.
    NB. Forced selection.  It was propagated when first detected, so we just ignore the node and
    NB. keep looking
    elseif. sellevel >: #selections do. selectionfound =. ,<localf   NB. should never be >
    elseif. localf -.@-: sellevel {:: selections do. selectionfound =. ,<localf
    elseif. (sellevel = <:#selections) *. (*#initialselection) do. selectionfound =. ({: selections) , usinginitialselection =. initialselection
    elseif. sellevel < <:#selections do. selectionfound =. 0$a:   NB. remove expansion if reclick on expanded selection
    end.
  end.
  NB. Get the list of remaining frame - but remove it if empty, if we diminished it (if we didn't diminish it, leave it
  NB. where it might be used for the next selection, which would have to be a drop-down
  residualy =. (a: -.~^:(*filledframe) < filledframe }. frame1) , }. y
  NB. Get the rank of a cell of this verb, for use in next level
  thiscellrank =. (errorcode <: EOK) { _1,(#maxcellresultshape)
end.
NB. If we found a selection, propagate it to the end and declare a change.  If not,
NB. try again at the next spot in the inheritance chain
if. #$selectionfound do.
  QP^:DEBPICK'selectionfound%initialselection%propagating:?(sellevel {. selections) , selectionfound%localf%frame1%thisverbframelen%prevcellrank%filledframe%thiscellrank%maxcellresultshape%'
  NB. Check to see if the new selection is OK.  It is, if it refers to an executed cell or to the FIRST unexecuted cell.
  NB. We create a selection window for each of those.  This approach requires that the block we are looking at have been
  NB. traversed up to the point where we could select from it.  If we are pushing an initialselection, this will not
  NB. be the case, because the new block might not even have been traversed at all lacking the selection.  So, we don't
  NB. audit initialselections, and assume they're valid since we generate them internally.
  if. #usinginitialselection do. selok =. _1 else. selok =. auditselection ,selectionfound end.  NB. _1=normal cell, 0=error cell, 1 = invalid
  if. selok <: 0 do.
    NB. Propagate the new selection
    makeselection , selectionfound
  end.
  selok { PICKTOOLTIPMSGOK,PICKTOOLTIPMSGPREVERR,PICKTOOLTIPMSGFILLED,PICKTOOLTIPMSGOK
else.
  NB. Before we leave this block to look at the next, give the block a chance to perform an action
  NB. Returns nonzero if it handled the pick
  if. PICKTOOLTIPMSGOK = postselectionoverride'' do. PICKTOOLTIPMSGOK return. end.
  SM^:DEBPICK'recursion'
  NB. get locale to use next; if empty, use our closer locale
  if. 0 = #recurloc =. getnextpickloc'' do. recurloc =. <'dissect' end.
  QP^:DEBPICK'localf%frame1%thisverbframelen%prevcellrank%filledframe%thiscellrank%maxcellresultshape%residualy%'
  NB.?lintonly recurloc =. <'dissect'
  ((sellevel + selectable),thiscellrank) recursiveselection__recurloc residualy
end.
)

NB. x is view number, y is y,x position
NB. We process a click on that cell.  We convert the y,x to a cell address, change the selectors for the click,
NB. and redraw the screen.  Result is 1 if the selection was changed, and the screen therefore needs to be redrawn
processdataclick =: 4 : 0
NB. y is y,x within the display rectangle.  Convert that to offset within the display of the entire noun, by adding
NB. the offset of the top-left corner of the displayed box, and subtracting the display position of the normal
NB. top-left, which position is 0 for unboxed, but at a boxmargin for boxed values
selx =. ; valueformat yxtopath BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + y
QP^:DEBPICK 'y selx '
QP^:DEBPICK 'sellevel #selections selections '
if. sellevel <: #selections do.
  NB. Process the cells mapped to this block, to see which one gets the selection.  Start the search in
  NB. the highest containing locale for the block, even if we displayed the value from a different one (because of error)
  tail =. findinheritedtail''
  NB.?lintonly tail =. <'dissectobj'
  recursiveselection__tail selx
else.
  NB. If sellevel exceeds the number of selections, there's nothing really here - it must be an empty
  NB. operand that displays a boundary.  Don't pick then
  PICKTOOLTIPMSGNOSELYET
end.
)

NB. **************** end display objects ************************

NB. ***************** explorer control for objects

EXPLORER =: 0 : 0
pc explorer;
xywh 0 0 ?;cc dissectisi isigraph;
pas 0 0;pcenter;
rem form end;
)
EXPLORER =: 0 : 0 [^:IFQT EXPLORER
pc explorer;
minwh ?;cc dissectisi isidraw;
pas 0 0;pcenter;
rem form end;
)

NB. Called in the locale of the object
createexplorer =: 3 : 0
NB. Start the window definition, so we can use existence of 'explorer' to indicate destination
NB. Create the isigraph and finish creating the form
wd '?' (taketo , (": |. -:^:(-.IFQT) {: DOsize) , takeafter) EXPLORER
wd 'pn *Exploring ' , ({.~ 100 <. #) defstring 0
wd 'pshow'
winhwnd =: wd 'qhwndp'
NB. Each explorer has its own hover position
hoverinitloc =: $0
NB. Create name to use to get to the instance locale
COINSTANCE =: COINSTANCE
NB. Draw the object on the explorer form
1 drawDOvnall ''
)

NB. Use destroy to remove the explorer window without writing to the main form, as for example
NB. when the old explorer is invalid
destroyexplorer =: 3 : 0
if. #winhwnd do.
  wd 'psel ', winhwnd
  wd 'pclose'
  winhwnd =: ''
end.
)

NB. Use close to return control back to the main form
explorer_close =: 3 : 0
destroyexplorer''  NB. Remove the form
)
explorer_cancel =: explorer_close

NB. ** explorer mouse events **

NB. The only event is a click in the one defined region.  We are already in the object locale
explorer_dissectisi_mbldown =: 3 : 0
debscroll^:DEBSCROLL 'embldown';<y 
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
hoverend''
seqclickno__COINSTANCE =: >: seqclickno__COINSTANCE
yx =. EXPLORERYX -~ 1 0 { sd =. 0 ". sysdata
if. *./ yx < {:DOsize do. 'l' pickDO 1;yx;sd;0 end.
)


NB. mouse movement.
NB. If we are dragging a scrollbar, vector to the object locale to handle that
explorer_dissectisi_mmove =: 3 : 0
debscroll^:DEBSCROLL 'emmove';<y 
QP^:DEBMOUSE'expmmove:hwnd=?winhwnd wd''qhwndp'' sysdata '
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
sd =. 0 ". sysdata
if. scrollingtype = SCROLLTYPESCROLLBAR do.
NB. Perform the scroll, on this explorer window.  If the scroll was started in a different locale,
NB. abort it.
  if. scrollinglocale__COINSTANCE -: coname'' do.
    1 scrollmmove 1 0 { sd
  else.
    explorer_dissectisi_mblup''
  end.
elseif. do.
  NB. mmove not for scrolling.  Set radius to use depending on whether a button is down
  NB. If button down, use larger radius but don't allow a new tooltip to start
  (1 0 ; sd) hoverstart 1 0 { sd
end.
i. 0 0
)

NB. mouse release.  If we are scrolling, set the new offset and redraw
NB. If we are dragging a scrollbar, vector to the object locale to finish that
explorer_dissectisi_mblup =: 3 : 0
debscroll^:DEBSCROLL 'emblup';<y 
if. scrollingtype = SCROLLTYPESCROLLBAR do.
  4!:55 ;: 'scrollinglocale__COINSTANCE scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit'  NB. indicate end-of-scrollbar
end.
scrollingtype__COINSTANCE =: SCROLLTYPENONE
)

explorer_dissectisi_focuslost =: 3 : 0
hoverend''
)


NB. right-click in explorer - delete the explorer window
explorer_dissectisi_mbrdown =: explorer_close

NB. Pass char events as if pressed in main form
explorer_dissectisi_char =: 3 : 0
sysdata__COINSTANCE =: sysdata  NB.?lintonly =. 'abc'
dissect_dissectisi_char__COINSTANCE''
)

NB. ********************** end of explorer ***************

NB. ***************** utilities used by the object locales *****************

NB. y is a selector: any shape, but each selector has shape ,2; so $=?,2
NB. Result is array of boxes with one box for each selector, containing the
NB. indices of the results for each selector
NB. The selector is (interval]
findselection =: 3 : 0
<@([ + i.@-~)/"1 logticket I. >: y
)


NB. join display strings
NB. x and y are strings to create the display form of a sentence
NB. Result is the two strings joined together
NB. We add parentheses around x if the last word of x and the first word of y are both numeric
NB.   (x because with right-to-left use turning x y z into x (y z) is bad)
NB. Add space before y if the two adjacent characters are both alphameric
jd =: 4 : 0
if. '.:' e.~ {. y do. y =. ' ' , y end.
if. 1 1 -: (({:x) , ({.y)) e. '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' do. y =. ' ' , y end.
if. ({: ;: x) *.&('0123456789_' e.~ {.@>) ({. ;: y) do. x =. '(' ([`(' '&(i.&0@:=)@])`(' '&,@]))} x , ')' end.  NB. Replace last space with (
x , y
)

cocurrent 'dissectobj'

NB. **** pick support *****
NB. y is the selection, a list
NB. result is the selection to store in the node.  This will refer to the selected item but it might
NB. be negative to suggest negative indexing
selectiontodisplay =: ]

NB. *** traversal support ***

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
NB. Any needed side effects are taken care of here
NB. Called even when selector is empty, if rank-calculus probe
calcdispframe =: 4 : 0
((2#a:) ,~ (2 # >./&.:>) , <) y
)

NB. y is all the indexes that were selected by  the selector
NB. Result is the selectors to display (a list), in order.  The atom count should match the frame returned by calcdispframe
calcdispselx =: ]

NB. Nilad.  Called when we detect that there are insufficient results from an execution.  Normally we let this
NB. pass, finding the error only at the actual verb execution
operationfailed =: 0:

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
selindextoisf =: 4 : 0
if. selectable do. < x #: y else. '' end.
)

NB. y is the new selection (boxed, and possibly with an initialselection following)
NB. Result is signum of (sel - error spot): 1 if invalid, 0 if on the error, _1 if no error, 2 if impossible (selection >: frame - must be fill)
auditselection =: 3 : 0
if. +./ selframe <: actsel =. >@{.^:(0<L.) > selectiontoticket {. y do. 2
else. * (selframe #. actsel) - nvalidresults
end.
)

NB. y is #selx; result is 1 if it indicates that cells were executed.  The difference between no execs and some is significant
cellswereexecuted =: *

NB. y is anything - intervals, results, fillmasks - that has been expanded into an array using the shape of the frame.
NB. y is in ticket order, i. e. execution order
NB. Result is the array reordered to natural order, i. e. selection order (some primitives process out of order; we reorder to match selection)
tickettonatural =: ]

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
getselection =: 3 : 0
if. selectable *. (sellevel < #selections) do. 1 ;< sellevel { selections
else. 0 0
end.
)

NB. x is limits of previous selection interval (boxed)
NB. y is the indexes in logticket that matched the previous selection
NB. Results is the intervals corresponding to each selection.  This can be either unboxed or boxed:
NB. if there is only one selector, we just return one interval per result (plus one at the end) and it
NB. isn't boxed.  If there are multiple selectors, we return one or more interval for each result-plus-1,
NB. and we box each of them
selectticketintervals =: 4 : 0
sel =. >x
ytick =. y { logticket
assert. 2 -: {: $ sel
if. 1 = # $ sel do.
  NB. simple case: a single interval
  NB. The last interval will be unnecessary unless there was an execution error: in that case it will hold the error
  2 ]\ sel enclosing ytick
else.
  assert. 2 = # $ sel
  NB. multiple intervals (owing to recursion).  x and y are two sets of logticket values.  x is a set of start-1,end pairs,
  NB. and y is a set of endpoints where each describes an interval back to the previous endpoint from y (and there is
  NB. an implied infinity at the end in case execution was aborted).  There should not be any y values in the gaps
  NB. between the x-pairs.
  NB.
  NB. We process these by throwing them together after turning them into lists of (ticket,type) where type is
  NB. 0=xstart-1, 1=yend, 2=ystart-1, 3=xend (a yend equal to the last xend is appended to handle the error case mentioned above)
  NB. Sort in ascending order, and then process intervals ending on yends.  Each such interval should start with
  NB. a ystart, and the next thing should NOT be an xstart.  The interval should end with yend (it will, by definition) and
  NB. the previous thing should NOT be xend.  Each nonoverlapping pair of tickets values will then be a selector.
  newintvls =. (<;.2~ (1 = {:"1)) /:~ (,/ sel ,."1 (0 3)) , (ytick ,. 2) , (1 ,.~ ytick , {:,sel)
  assert. (-:   0 1 $~ #) 1 bwand {:"1 ; newintvls
  _2&(]\)@:({."1)&.> newintvls
end.
)

NB. y is boxed selection (number only, no SFOPEN) in natural order; result is boxed selection in execution order
selectiontoticket =: ]

NB. y is selection, in ISF form
NB. x is the selectors, shaped into a form suitable for indexing
NB. for the dyad, we select from x.  For the monad, we just return the index we would use to select from x
NB. The monad result is a singly-boxed operand for {
selectusingisf =: 3 : 0
NB. In this version, only a single selection is supported, possibly followed by dropping down a level, which we ignore here
NB. because it's used for display only (the selected results were collected only after dropping down)
{. > isfensureselection isftorank2 y
:
(selectusingisf y) {^:(*@#@[) x
)

NB. x is selopinfovalid, y is selopshapes.  The current level has no selection.
NB. Result is our prediction of what the shape of the selected operands 
calcunselectedshapes =: 4 : 0
if. x do.
  NB. There is only one item, so assume we are selecting it and calculate its shape, including dropdown if any
  levelct =. (#frames) {. arglevel   NB. number of levels to drop down
  framel =. levelct (_1:^:(0<[) #)&> frames   NB. length of frame, but _1 if we are dropping down a level - keep map intact then
  NB. If the frame is empty, don't disturb the map.  If we are passing the operand through structural modifiers,
  NB. we want the whole map available when we get down to executing verbs
  framel (}. $^:(0<L.))^:(0<[)&.>     levelct (>@{.@]^:(* 0<L.) )&.> y
else.
  NB. Multiple items.  All we can do is discard the frame from the shape
  ($^:(0<L.)&.> y) (}.~ #)&.> frames
end.
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. This version calculates the shape resulting from a single isf that may include
NB. dropping down a level
NB. Calculate the selections for each operand, which is the requested selection,
NB. but with only as many leading axes as are present in the frame.
NB. If there is a drop-down, preserve it
opsel =. (> isfensureselection isftorank2 y) (}.@[ ,~ ({.~ #)&.>&{.)"1 0 frames
NB. Apply the selection to the selopshapes: if selopshapes is open, just discard leading
NB. axes; otherwise do the selection and open if there is a drop-down
selopshapes ((}.~ #)&.> {.) ` (] >@]^:(1<#@[) <@(({ >)~ {.))  @. (1<L.@[)"0 1 opsel
)

NB. x is selx, the indexes of the selected result tickets
NB. y is logvalues
NB. Result is selresult
performselection =: {

NB. x is the selected indices that matched the selector
NB. y is (max size of a selresult as calculated by checkframe);< fill atom for the variable
NB. Result is the shape we expect this result to have, for use in later traversal
calcselresultshape =: 4 : 0
'maxsize fillatom' =. y
select. resultlevel
case. 1 do.
  (frame $ $L:0 , x { logvalues)   NB. Don't unbox
case. 0 do.
  NB.?lintonly resultseqmap =: ''
  resultseqmap (>@{) L: 0 _  $L:0 frame $!.a: , x { logvalues
case. do.
  select. < fillatom
  case. <a: do.
    NB. y is boxed: return an array with the shape of collected y, with each box containing the recursive shape of contents
    (frame $ $L:0 > , x { logvalues)
  case. <'' do.
    NB. x did not collect; return empty shape, empty size
    ($0)
  case. do.
    NB. unboxed y; return the shape
    frame,maxsize
  end.
end.
)


NB. y is the current selection (a: if forced)
NB. The current execution has a frame and a selector.  Create information about the input cells that contribute to the
NB. selected output.  This will be a table, one row per operand, with each row containing selection info as described below.
NB. For the initial creation here, there will always be a single box in each row; subsequent selections will append to the row(s).
NB. In this default case, we are handling normal verbs, and the contributing input is simply the selector, truncated to the length
NB. of the appropriate frame.
NB. In general, each selector contains an atom/array of index lists of selected cells.  If the selector contains an array,
NB. each list describes one selected cell (obviously all such cells have the same rank) and the shape with respect to lists
NB. gives the shape of the selected group of cells, which may become important if subsequent selectors select from the group.
NB.
NB. For the normal verb, the selector and the highlight are identical.
calcphysandhighlights =: 3 : 0
assert. valence = #frames
NB. default selection: that part of the selection in the frame, shaped into a table
NB. If this selection includes a level, give that many drop-downs at the end of it
NB. Extend the selection to rank 2, then make sure it starts with a selection (possibly null), then
NB. extract that single selection.  Then take leading elements of that, for as long as the frame is
bsel =. ({.@> isfensureselection isftorank2 y) <@({.~ #)&.> frames
if. #arglevel do.
  bsel =. bsel ,&.> (0 >. arglevel) <@#"0 SFOPEN
end.
<@(2 1&$)"0 bsel
)

NB. y is 3-row table of (physical selection after sellevel);(highlights);(sellevel at time this selection was added)
NB. We create one highlight entry for each highlight, by prepending all the earlier physical selections from v-nodes, and restarting when we have a u-node.
NB. Each highlight has the level at which it was added; we discard levels below the current one (since selections before that have been applied
NB. to the operands already).
NB. If, after this trimming, there is an empty box in a selection, delete the selection as invalid - it must be
NB. a u value that was filled with empties and inherited by a higher node in which the early parts are invalid (this happens only in expansion nodes)
NB. We give the highlight the level it applies to, which is one more than than the selection level it is selected from: for example,
NB. a selection at level 0 will make the selected portion of the result display in tghe color of level 1, so we make the corresponding
NB. highlight display in level 1 also.  The highlight box for the selection (most importantly, when it goes past the last selection level)
NB. displays in black, which doesn't match its operands; we might want to change this.
NB. Resulting opselin is a table of highlight color ,&< list of boxes, each containing an array of boxed ISFs
addselectedoperands =: 3 : 0
QP^:DEBSELECT'addselected:defstring=?defstring]0 >coname'''' y opselin '
NB. Create sequence of boxes: H0; P0 H1; P0 P1 H2; etc.  Prepend highlight color, which is 1 more than the sellevel at which the selection was added
allh =. (>:&.>@{: ,. ((_1}&.>~ <)"0~ <\)~/@}:) (#"1~   sellevel <: >@{:) y
opselin =: opselin , (#~  a: ~: {:"1) allh  NB. If nothing left after discarding sellevel, add nothing
QP^:DEBSELECT'opselin allh '
)

NB. ***** Gerund management *****
NB. Nilad.  The locale called must be a noun locale.  The result is the list of verb locales that make up
NB. the gerund in the locale.  If the locale is not a gerund, the result is empty.
querygerund =: (0$a:)"_

cocurrent 'dissectfitok'

NB. x is fit tokens, y is string to use
applyfit =: 4 : 0
NB.?lintonly titlestring =: tokensource =: ''
titlestring =: titlestring , y
fitstring =: y
tokensource =: tokensource , x
0
)


cocurrent 'dissectrighttoleft'
NB. y is anything - intervals, results, fillmasks - that has been expanded into an array using the shape of the frame.
NB. y is in ticket order, i. e. execution order
NB. Result is the array reordered to natural order, i. e. selection order (some primitives process out of order; we reorder to match selection)
tickettonatural =: |.

NB. y is boxed selection in natural order; result is boxed selection in execution order
NB. For travdowncalcselect we get the selection number without the SFOPEN; for auditselection we get
NB. the full selectipon including the SFOPEN; so we discard anything past the first
NB. atom.  For this node we know we have a list of cells
selectiontoticket =: 3 : 0
NB.?lintonly 'selopshapes frame selections sellevel' =: (2$a:);($0);(1$a:);0
< ({.frame) | _1 - > {. >y
)

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
NB. Since only expansion nodes come through here, append SFOPEN to complete the selection
selindextoisf =: 4 : 0
NB.?lintonly SFOPEN =. SFOPEN_dissectobj_ [ selectable =: 0
if. selectable do. < SFOPEN ;~ x #: _1 - y else. '' end. NB. count back from the end
)


cocurrent 'dissectallnouns'

NB. Inherit pickDO from default object
NB. For all these verbs, x is formatted sysdata, y is the yx position of the click relative to start of pickrect

NB. Since labeled nouns are never SDTs, ignore a click on the name
NB. Nouns are either primitives or SDTs.  Clicking on the shape will expand an SDT if possible.  Once expanded,
NB. the display becomes a verb, ad cannot be collapsed.
picklDOshapepos =: 4 : 0
NB.?lintonly 'nounhasdetail nounshowdetail COINSTANCE' =: 0;0;< <'dissectobj'
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COINSTANCE 1  NB. display the updated selection
end.
0 0$0
)

picklDOdatapos =: 4 : 0
NB. If the display of a noun's detail is suppressed, and it has detail, any click on it will turn on the detail
NB.?lintonly 'nounhasdetail nounshowdetail COINSTANCE' =: 0;0;< <'dissectobj'
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COINSTANCE 1  NB. display the updated selection
else.
NB. If the noun is displaying detail, treat a click in the data area same as for a verb result
  x picklDOdatapos_dissectobj_ f. y
end.
)



NB. Here are the object locales for creating the parse table
NB. Each object is responsible for responding to the entry points:
NB. create - create the locale and return a proper stack line (type;locale)
NB. execute - set the locales of the noun operands of verbs
NB. defstring - return executable string without instrumentation
NB.  the operand gives the context of use: 0=verb or right op of verb, 1=left op of verb, 2=left op of modifier, 3=right op of modifier
NB. exestring - return executable string, after instrumentation has been added.  Set locale info
NB.  y is (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 if inverse also needed)

NB. In addition, verbs have the entry point rank which returns the rank of the verb

NB. The object is responsible for storing its value, its rank, and pointers to subobjects

NB. In the layout of the graphics, we assume that each block produces only one result cell,
NB. on a row by itself,
NB. which will be overlapped with the appropriate input position of the next block

cocurrent 'dissectirregularops'
NB. If this verb has no selector, we flag the selops as invalid, both as to shape (which is not fatal)
NB. and valence (which is)
rankcalculussupported =: 0

calcunselectedshapes =: 4 : 0
NB. Even if there is only one input cell, we can't traverse the next level (example: u/ y which
NB. runs u as a dyad)
selopinfovalid =: 0:"0 y
y
)

cocurrent 'dissectselectshape'
NB. Used for ops, like u/ and u^:, whose result shape depends on the selection

NB. x is the selected indices that matched the selector
NB. y is max size of a selresult as calculated by checkframe
NB. Result is the shape we expect this result to have, for use in later traversal
calcselresultshape =: 4 : 0
NB.?lintonly logvalues =: 0$a:
if. 0 = $x do. ($0)   NB. error, immaterial
else.
  (> $L:0 ({:x) { logvalues)
end.
)


NB. Terminal nouns - names or self-defining terms
cocurrent 'dissectnoun'
coinsert 'dissectallnouns dissectobj'
NB. Monad.  y is string form of the noun;name if it is a name;tokens it came from;value
create =: 3 : 0
NB. Not clonable
create_dissectobj_ f. 2 { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand, and the name if any.  We look into op from other locales to get the value of sdts
NB. The value is given, for original noun tokens.  It is undefined in results
'op varname nounvalue' =: 0 1 3 { y
NB. If the name is empty, this must be an SDT
resultissdt =: 0 = #varname
noun;(coname'');tokensource
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
NB. Apply parentheses if left verb operand or conjunction operand operand - but only if more than 1 word
enparen^:((y>0) *. 1 < #@;: op ) op
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 0
exestring =: 3 : 0
NB. init for logging
initloggingtable ''
auditstg '(' , (logstring '') , op , ')'
NB.?lintonly 'logvalues logticket' =: (1$a:);$0
NB.?lintsaveglobals
)

proplocales =: 3 : 0
(y = 3) # <tokensource
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: endtraverse@:(4 : 0)
NB. Normally a noun will have no layouts BUT in the case of m"_ when display is turned on,
NB. m will be given inputs
traversedowncalcselect y  NB. To set globals, including selresult
'displaylevrank nounhasdetail' =: varname;0
x ,&< coname''  NB. Return the empty DOLs
)

cocurrent 'dissectverb'
coinsert 'dissectfitok dissectobj'
NB. y is (string form of the verb);tokens it came from[verb name;verb locale, '' if local;definition of named verb;execution locale;number of names defined at time of parse]
NB. If the string form is boxed, it contains (string form);(title for display purposes)
create =: 3 : 0
create_dissectobj_ f. 1 { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand, as the display and executable form (we may modify the display form later)
'execform titlestring' =: ,&.> boxopen 0 {:: y
stealthoperand =: 1 2 4 5 6 0 {~ ((;:'][[:'),']]';'[[') i. <titlestring  NB. '[[' and ']]' are always-invisible forms
NB. A stealth operand counts as invertibly monadic
invertiblymonadic =: 3 bwand stealthoperand
titlestring =: stealthoperand {:: titlestring; ;: '][?[:]['
fitstring =: ''
NB. Every verb counts as an sdt for modifier processing.
resultissdt =: 1
NB. If this verb has a one-line description, save it
onelinedesc =: ''
NB. verbstate: 0=unnamed 1=named tacit 2=named explicit, short 3=named explicit, long
if. verbstate =: 2 < #y do.
  NB. named verb: remember the definition, and also the number of names that were defined when it
  NB. was encountered
  'verbglopart verbloc verbtext verbexeloc ndefnames' =: 2 }. y
  NB. Remove blank lines from the definition
  verbtext =: (LF,LF) (-.@:E. # ]) verbtext
  NB. The 3 in the next 2 lines is the number of lines we display, max)
  verbstate =: 1 + (3 < LF +/@:= verbtext) + +./ (LF;'1234' ,&.> <' :') +./@:E.&> <verbtext  NB. 1, but 2 if LF or explicit
  onelinedesc =: ,&(LF,'...')^:(verbstate=3) }:^:(LF={:) ((+/\ LF = verbtext) i. 3) {. verbtext
NB.?lintonly else. ndefnames =: verbstate =: 0 [ verbtext =: verbloc =: verbexeloc =: verbglopart =: ''
end.
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Save the number of operands for this invocation
NB. Return value is the locale name (possibly changed)
NB. y is, for each operand, whether the operand is from SDTs
setvalence =: 3 : 0
valence =: #y
resultissdt =: *./y
NB. If this is a primitive verb, get its NuVoc designation
if. IFQT *. (#primexplains) > tx =. (0{"1 primexplains) i. <execform do.
  nuvocpage =: (#. (*#fitstring),(valence = 2)) {:: 4 $ <;._1 '`' , ((<tx,3) {:: primexplains)
end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
NB. Apply parentheses if right conjunction operand - but only if more than 1 word
enparen^:((y>2) *. 1 < #@;: ) execform,fitstring
)

NB. return string form of operands, including instrumentation
NB. y tells what instrumentation is needed:
NB.  (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 for inverse also)

exestring =: 3 : 0
NB. init for logging
initloggingtable (verbstate>0) # valence
if. verbstate>0 do.
  auditstg '((' , (logstring '') , '@(' , (verblogstring '') , execform , ') [ ', (logstringxy '') , ')"',execform,')'
else.
  NB. Instrument the forward verb - bivalent
  auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (execform,fitstring) , '))'
end.
NB.?lintonly 'logvalues logticket' =: (1$a:);$0
NB.?lintsaveglobals
)

proplocales =: 3 : 0
(y = 3) # < tokensource
)

NB. index is (excess frame in off operand),(low 3 bits of dispstealth)
NB. Result is new dispstealth: flag=0 if no off frame OR dispstealth is not 1 or 2
dispsttbl =: 2 7 $ 0 1 2 3 4 5 6  0 9 10 3 4 5 6

NB. Set globals, then initialize display for the verb
traverse =: endtraverse@:(4 : 0)
assert. 1 2 e.~ #x
traversedowncalcselect y  NB. Just to set error globals
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
displaylevrank =: rankhistory
NB. Set the dispstealth status for this node, if dyadic.  If a dyadic ][ has excess frame
NB. on the suppressed side, we must mark it as non-stealth, because we need the node to be
NB. displayed to show the action of the excess frame.  We use bit 3 of dispstealth to indicate
NB. force-displayed stealth.
if. valence = 2 do.
  exsframe =. 3 2 '!!'&(+./@:E.)@;@:({"1)"0 _ rankhistory
  dispstealthoperand =: ((7 bwand dispstealthoperand) <@({ , [) 0 , exsframe , 0 0 0 0) { dispsttbl
end.
x ,&< coname'' NB. no v, so no change to the DOLs
)

NB. Find explanation for verb - overriden in other locales
lookupexplanation =: 3 : 0
if. (#primexplains) > tx =. (0{"1 primexplains) i. <execform do.
  LF ,~ ((*@# - 2 * '!.0'&-:) fitstring) {:: 3 $ <;._1 '`' , ((<tx,valence) {:: primexplains)  NB. 0=norm 1=fit _1=exact
else. ''
end.
)

exegesisrankstack =: 3 : 0
if. execform -.@:-: titlestring do.
  NB. If this is a final node (execform not the same as titlestring), explain the expansion
  NB. We will know that the node has expanded if its initialselection is present
  if. (selectable+sellevel) < #selections do.  NB. expansion selected
    r =. ,: (EXEGESISRANKOVERALLEXPLAIN,selectable+sellevel) ; 'This block %al1%displays the final result of the verb:',LF,(> {: <^:(0=L.) titlestring),CR,'The block feeding into this one shows the details of the computation. Select this result again to hide the details.',LF
  else.
    r =. ,: (EXEGESISRANKOVERALLEXPLAIN,selectable+sellevel) ; 'This block %al1%displays the final result of the verb:',LF,(> {: <^:(0=L.) titlestring),CR,'To see the details of the computation, select the result to see a block containing the intermediate results.',LF
  end.
elseif.  '^:_1' -: _4 {. execform do.
  NB. If this was an inverse added by &.&.:, explain that
  r =. ,: EXEGESISRANKSTACKEXPLAIN ; 'This inverse was added to complete an operation started by &. or &.:',LF
elseif. do. r =. 0 2$a:
end.
if. #onelinedesc do.
  r =. r , EXEGESISONELINEDESC ; 'The definition of this verb ' , ((verbstate = 3) {:: 'is:';'starts with:'),LF,onelinedesc,CR
end.
if. verbstate > 0 do.
  if. (selectable+sellevel) > #selections do.
    if. verbstate = 1 do.
      r =. r , EXEGESISVERBRUNDEBUG ; 'After you have selected a single result-cell, you may right-click the verb to dissect execution on that cell.',LF
    else.
      r =. r , EXEGESISVERBRUNDEBUG ; 'After you have selected a single result-cell, you may right-click the verb to examine execution on that cell using the debugger.',LF
    end.
  else.
    if. verbstate = 1 do.
      r =. r , EXEGESISVERBRUNDEBUG ; 'You may right-click the verb to dissect execution on the selected cell.',LF
    else.
      r =. r , EXEGESISVERBRUNDEBUG ; 'You may right-click the verb to examine execution on the selected cell using the debugger.',LF
    end.
  end.
end.
if. #t =. lookupexplanation'' do.
  if. 0: #nuvocpage do. t =. t , 'Left-click to see the NuVoc page for this primitive.',LF end.
  r =. r , EXEGESISVERBDESC ; t
end.
r
)

NB. Quick descriptions of all primitives
primexplains =: _4 ]\ <;._2 (0 : 0)
=
=y indicates, for each item in the nub of y, whether it matches each item of y
x=y is 1 if the atoms x and y are tolerantly equal`x=!.f y is 1 if the atoms x and y are equal to with tolerance f`x=!.0 y is 1 if the atoms x and y are exactly equal
eq`#dyadic
<
<y boxes y
x<y is 1 if the atom x is tolerantly less than the atom y`x<!.f y is 1 if the atom x is less than y with tolerance f`x<!.0 y is 1 if the atom x is intolerantly less than y
lt`lt#dyadic
<.
<.y is the largest integer not tolerantly exceeding y`<.!.f y is the largest integer not exceeding y by the tolerance f`<.!.0 y is the largest integer not exceeding y (using exact comparison)
x<.y is the smaller of the atoms x and y
ltdot`ltdot#dyadic
<:
<:y is y-1
x<:y is 1 if the atom x is tolerantly less than or equal to the atom y`x<:!.f y is 1 if the atom x is less than or equal to the atom y with tolerance f`x<:!.0 y is 1 if the atom x is less than or equal to the atom y using exact comparison
ltco`ltco#dyadic
>
>y unboxes each atom of y
x>y is 1 if the atom x is tolerantly greater than the atom y`x>!.f y is 1 if the atom x is greater than the atom y with tolerance f`x>!.0 y is 1 if the atom x is greater than the atom y using exact comparison
gt`gt#dyadic
>.
>.y is the smallest integer not tolerantly less than y`>.!.f y is the smallest integer not less than y by the tolerance f`>.!.0 y is the smallest integer not less than y (using exact comparison)
x>.y is the larger of the atoms x and y
gtdot`gtdot#dyadic
>:
>:y is y+1
x>:y is 1 if the atom x is tolerantly greater than or equal to the atom y`x>:!.f y is 1 if the atom x is greater than or equal to the atom y with tolerance f`x>:!.0 y is 1 if the atom x is greater than or equal to the atom y using exact comparison
gtco`gtco#dyadic
_:
_:y is infinity, regardless of y
x_:y is infinity, regardless of x and y
underco`underco#dyadic
+
+y is the complex conjugate of y
x+y is x plus y
plus`plus#dyadic
+.
+.y is a 2-atom list of the real and imaginary parts of the atom y
x+.y is x OR y if x and y are Boolean; generally, the greatest common divisor of x and y
plusdot`plusdot#dyadic
+:
+:y is 2*y
x+:y is the negation of x OR y
plusco`plusco#dyadic
*
*y is signum(y): _1 if y<0, 0 if y tolerantly=0, 1 if y>0`*!.f y is signum(y): _1 if y<0, 0 if y is within f of 0, 1 if y>0`*!.0 y is signum(y): _1 if y<0, 0 if y is exactly 0, 1 if y>0
x*y is x times y
star`star#dyadic
*.
*.y is a 2-atom list of the length and angle of the atom y, in the complex plane
x*.y is x AND y if x and y are Boolean; generally, the least common multiple of x and y
stardot`stardot#dyadic
*:
*:y is y^2
x*:y is the negation of x AND y
starco`starco#dyadic
-
-y is the negative of y
x-y is x minus y
minus`minus#dyadic
-.
-.y is 1-y
x-.y is x, with any items removed that tolerantly match cells of y`x-.!.f y is x, with any items removed that match cells of y with tolerance f`x-.!.0 y is x, with any items removed that exactly match cells of y
minusdot`minusdot#dyadic
-:
-:y is y%2
x-:y is 1 if the arrays x and y match, in shape and values (tolerantly)`x-:!.f y is 1 if the arrays x and y match, in shape and values (with tolerance f)`x-:!.0 y is 1 if the arrays x and y match exactly, in shape and values
minusco`minusco#dyadic
%
%y is 1%y
x%y is x divided by y
percent`percent#dyadic
%.
%.y is the matrix inverse of y (pseudoinverse if y is not square)
x%.y is ((%. y) +/ . * x)
percentdot`percentdot#dyadic
%:
%:y is the square root of y
x%:y is the xth root of y
percentco`percentco#dyadic
^
^y is e^y
x^y is x raised to the power y`x^!.f y is like x^y but f is added to x before each multiplication
hat`hat#dyadic`hat`hat#stope
^.
^.y is ln(y)
x^.y is the logarithm of y, using base x
hatdot`hatdot#dyadic
$
$y is the shape of y
x$y is an array made by using items of y, with the frame given by x`x$!.f y is an array made by using items of y, with the frame given by x; after items of y are exhauseted, use the fill atom f
dollar`dollar#dyadic
$.
$.y creates a sparse matrix from y
x$.y performs a sparse-matrix operation
dollardot`dollardot#dyadic
$:
$:y performs recursion
x$:y performs recursion
dollarco`collarco#dyadic
~.
~.y is the tolerantly unique items of y, in their original order`~.!.f y is the unique items of y (to tolerance f), in their original order`~.!.0 y is the unique items of y (using exact comparison), in their original order

tildedot`tildedot#dyadic
~:
~:y is a Boolean for each item of y, 1 if no previous item tolerantly matches it`~:!.f y is a Boolean for each item of y, 1 if no previous item matches it to tolerance f
x~:y is 1 if the atoms x and y are not tolerantly equal`x~:!.f y is 1 if the atoms x and y are not equal to tolerance f`x~:!.0 y is 1 if the atoms x and y are not exactly equal
tildeco`tildeco#dyadic
|
|y is the magnitude of y
x|y is y(mod x), the remainder after dividing y by x.  0 if the quotient is tolerantly equal to an integer.`x|!.f y is y(mod x), the remainder after dividing y by x.  0 if the quotient is equal to an integer to tolerance f.
bar`#dyadic
|.
|.y is y, in reversed item order`|.!.f y is y, shifted right 1 position, with position 0 filled by f
x|.y is y, with the items rotated left x positions`x|.!.f y is y, with the items shifted left x positions and vacated positions filled by f
bardot`bardot#dyadic`bardot#monadicfit`bardot#dyadicfit
|:
|:y is the transpose of y, y with the axes running in reversed order
x|:y is y with the axes x moved to the end
barco`barco#dyadic
,
,y is a list of all the atoms of y
x,y joins x and y into a single array, with the items of x followed by the items of y.  An atom is replicated to the shape of a result item; other short items are padded with fill.`x,!.f y joins x and y into a single array, with the items of x followed by the items of y.  An atom is replicated to the shape of a result item; other short items are padded with the fill atom f.
comma`comma#dyadic
,.
,.y is a table where each row is a list of the atoms from one item of y
x,.y is x,"_1 y, and joins corresponding items of x and y.  An atom is replicated to the shape of a result item; other short items are padded with fill.`x,.!.f y is x,"_1 y, and joins corresponding items of x and y.  An atom is replicated to the shape of a result item; other short items are padded with the fill atom f.
commadot`commadot#dyadic
,:
,:y is y with a leading axis of length 1 added
x,:y is an array with two items, the first coming from x and the second from y.  An atom is replicated to the shape of a result item; other short items are padded with fill.`x,:!.f y is an array with two items, the first coming from x and the second from y.  An atom is replicated to the shape of a result item; other short items are padded with the fill atom f.
commaco`commaco#dyadic
#
#y is the number of items of y
x#y is an array in which each atom of x tells how many times the corresponding item of y appears.  Complex values in x generate fill.`x#!.f y is an array in which each atom of x tells how many times the corresponding item of y appears.  Complex values in x insert copies of the fill atom f.
number`number#dyadic
#.
#.y is 2 #. y
x#.y converts the list y to a single number, using x as the place values of the representation
numberdot`numberdot#dyadic
#:
#:y is the binary representation of y
x#:y converts the number y to a list, using x as the place values of the representation.  Comparison to integer values is tolerant.`x#:!.f y converts the number y to a list, using x as the place values of the representation.  Comparison to integer values uses tolerance f.
numberco`numberco#dyadic
!
!.y is factorial(y)
x!.y if the number of combinations of y things taken x at a time
bang`#dyadic
/:
/:y is the permutation that would put y into ascending order
x/:y is x sorted into ascending order using the corresponding values of y as keys
slashco`slashco#dyadic
\:
\:y is the permutation that would put y into descending order
x\:y is x sorted into descending order using the corresponding values of y as keys
slashco`slashco#dyadic
[
[y is y
x[y is x
squarelf`squarelf#dyadic
]
]y is y
x]y is y
squarert`squarert#dyadic
{
{y is the Cartesian product of the contents of boxes of y
x{y is a selection from y, using x to control the selection
curlyf`curlylf#dyadic
{.
{.y is the first item of y
x{.y takes the first x items of y (last items if x is negative)`x{.!.f y takes the first x items of y (last items if x is negative).  The fill atom f is used for nonexistent positions.
curlylfdot`curlylfdot#dyadic
{:
{:y is the last item of y

curlylfco`curlylfco#dyadic
}.
}.y is all the items of y except the first
x}.y drops the first x items of y (last items if x is negative)
curlyrtdot`curlyrtdot#dyadic
}:
}:y is all the items of y except the last

curlyrtco`curlyrtco#dyadic
".
".y executes the sentence y, giving its result (if a noun)
x".y converts the string y to numeric, using x as default in case of invalid values
quotedot`quotedot#dyadic
":
":y converts y to string form using default conversions`":!.f y converts y to string form using default conversions, but accurate to f significant digits.
x":y converts y to string form using conversions specified by x
quoteco`quoteco#dyadic
?
?y is a random number (between 0 and 1 if y=0, a nonnegative integer less than y otherwise)
x?y is x distinct nonnegative random integers less than y
query`query#dyadic
?.
?.y is like ?y but uses a fixed starting value
x?.y is like x?y but uses a fixed starting value
querydot`querydot#dyadic
A.
A.y is the permutation number of the permutation y
x A.y reorders the items of y using the permtation whose number is x
acapdot`acapdot#dyadic
C.
C.y converts the permutation y between direct and cycle form
x C.y  reorders the items of y using the permtation x
ccapdot`ccapdot#dyadic`ccapdot#permparity`ccapdot#dyadic
e.
e.y gives, for each opened atom of y, a list indicating which items of (;y) are tolerantly in it`e.!.f y gives, for each opened atom of y, a list indicating which items of (;y) are in it to tolerance f`e.!.0 y gives, for each opened atom of y, a list indicating which items of (;y) are in it (using exact comparison)
x e.y is 1 for each cell of x that is tolerantly an item of y, 0 for cells of x not in y`x e.!.f y is 1 for each cell of x that is an item of y to tolerance f, 0 for cells of x not in y`x e.!.0 y is 1 for each cell of x that is an item of y using exact comparison, 0 for cells of x not in y
edot`edot#dyadic
E.

x E.y a Boolean array, whose shape is the same as that of y, with 1s at the starting corners of subarrays that tolerantly match x`x E.!.f y a Boolean array, whose shape is the same as that of y, with 1s at the starting corners of subarrays that match x to tolerance f`x E.!.0 y a Boolean array, whose shape is the same as that of y, with 1s at the starting corners of subarrays that match x exactly
ecapdot
i.
i.y is an array of consecutive natural numbers of shape y
x i.y for each cell of y (whose rank is the rank of an item of x), the index of the first tolerantly matching item of x, or #x if there is no match`x i.!.f y for each cell of y (whose rank is the rank of an item of x), the index of the first matching item of x to tolerance f, or #x if there is no match`x i.!.0 y for each cell of y (whose rank is the rank of an item of x), the index of the first exactly matching item of x, or #x if there is no match
idot`idot#dyadic
i:
i:y equally-spaced numbers between -y and +y
x i:y for each cell of y (whose rank is the rank of an item of x), the index of the last tolerantly matching item of x, or #x if there is no match`x i:!.f y for each cell of y (whose rank is the rank of an item of x), the index of the last matching item of x to tolerance f, or #x if there is no match`x i:!.0 y for each cell of y (whose rank is the rank of an item of x), the index of the last exactly matching item of x, or #x if there is no match
ico`ico#dyadic
I.
I.y for Boolean y, a list of the indexes of 1s
x I.y the index within y before which x could be inserted keeping the list in order
icapdot`icapdot#dyadic
j.
j.y is 0j1*y
xj.y is x + 0j1 y
jdot`jcapdot#dyadic
L.
L.y is the boxing level of y, 0 if unboxed

lcapdot`lcapdot#dyadic
o.
o.y is 1p1*y
x o.y is a trigonometric function of y depending on x
odot`odot#dyadic
p.
p.y converts polynomial y between coefficient and product-of-roots form
x p.y evaluates the polynomial x at value y`x p.!.f y evaluates the polynomial x at value y, but adding f to y before each power
pdot`pdot#dyadic
p..
p..y is the first derivative of polynomial y
x p..y is the integral of polynomial y, with x the integration constant
pdotdot`pdotdot#dyadic
p:
p:y is the yth prime number (2 is the 0th)
x p:y is a prime-related function of y depending on x
pco`pco#dyadic
q:
q:y is the prime factorization of y, in ascending order
x q:y is the exponents of the prime factorization of y
qco`qco#dyadic
r.
r.y is a complex number on the unit circle, with angle y
x r.y is the complex number with length x and angle y
rdot`rdot#dyadic
s:
s:y creates a symbol to stand for the string y
x s:y is a symbol-related function of y depending on x
sco`sco#dyadic
u:
u:y is the Unicode character corresponding to y
x u:y is a Unicode-conversion function of y depending on x
uco`uco#dyadic
x:
x:y converts y to extended precision`x:!.f y converts y to extended precision, with precision f used to determine fractional approximation
x x:y is a precision-conversion function of y depending on x
xco`xco#dyadic
;
;y is the items of the contents of y, assembled in order
x;y boxes x (and y, if y is unboxed), and joins the boxes into a list.  An atom is replicated to the shape of a result item; other short items are padded with fill.`x;!.f y boxes x (and y, if y is unboxed), and joins the boxes into a list.  An atom is replicated to the shape of a result item; other short items are padded with the fill atom f.
semi`semi#dyadic
;:
;:y is a boxed list containing the words in the string y
x;:y executes the sequential machine x on the data y
semico`semico#dyadic
{::
{::y creates the map of y: each leaf is replaced by its path
x{::y fetches from y using the path x
curlylfcoco`curlylfcoco#dyadic
)


NB. *** traversal support ***
operationfailed =: 1:   NB. An error found during verb execution is a stopper

NB. x is selx, y is logvalues.  Return selection.
NB. Save selx here in case we need it for launching debug
performselection =: 4 : 0
selectedindex =: x
x { y
NB.?lintsaveglobals
)


pickrlaunchdebug =: 3 : 0
if. verbstate=0 do. 'Only named verbs can be debugged.' return. end.
if. (selectable + sellevel) > #selections do. 'You must select a single result-cell for analysis' return. end.
NB. Get the argument(s) for the selected cell, and the text of the sentence (monad or dyad)
NB. If there is no selected index, the only explanation is that the named verb failed
NB. and we are pointing to the failure.  On that assumption, use the last inputs for y[x].
if. 0 = #selectedindex do. selectedindex =. <:#logvaluesy
elseif. selectable do. selectedindex =. (sellevel { selections) { selectedindex
end.
NB. Create the variables in scope for the execution
argvbls =. ,: ((,'y') ; 0) , selectedindex { logvaluesy
if. 2 = valence do.
  argvbls =. argvbls , ((,'x') ; 0) , selectedindex { logvaluesx
end.
NB. Make sure the new x and y override any previous definitions in the locale; more generally
NB. that any reassigned names keep the most recent value.  Use only definitions in place when the
NB. verb was encountered during execution.
argvbls =. (#~   ~:@:({."1)) argvbls , (-ndefnames) {. defnames
if. verbstate = 1 do.
  NB. Tacit verb: dissect it
  NB. Create the sentence to use, with the verb definition expanded
  txt =. ('x ' #~ 2 = valence) , '(' , verbtext , ') y'

  NB. Figure out the locale to run the sentence in.  It is the same as the locale the containing
  NB. ran in, unless this name is a locative
  if. '_' = {: execform do.
    NB. Name was an immed locative.  Use the locale of the locative
    subloc =. '_' (>:@i:~ }. ]) }: execform
  elseif. #verbglopart do.
    NB. Must be an unresolved simplename or object locative.  Extract the starting locale of the search
    if. #oloc =. '__' takeafter verbglopart do.
      NB. object locative.  Resolve it, starting in the search locale
      subloc =. ". oloc , '__verbloc'
    else.
      NB. Unresolved simplename.  Use the search locale
      subloc =. verbloc
    end.
      NB. (which is verbloc or the value of the first name), and also any additional references
  elseif. do.
    NB. Name was resolved locally, as simplename.  Use default locale
    subloc =. verbloc
  end.
  NB. Create the options to use for the sandbox.  Keep the options given by the user, except for title and link;
  NB. clear fromdebugger and noassignment; set returnobject, sandbox, parent.
  useopts =. (;: 'fromdebugger noassignment returnobject sandbox title link parent') subkl dispoptions
  useopts =. useopts , (;: 'returnobject sandbox parent') ,. 1;1;COCREATOR
  NB. Run the sentence in a sandbox; save the locale, if there was no error.
  NB. If there was an error, give a message.
  if. '' -: $ debugloc =. dissect (useopts ; subloc ; txt) , argvbls do.
    debuglocs__COCREATOR =: debuglocs__COCREATOR , debugloc
  else.
    wdinfo 'Error running dissect';debugloc
  end.
else.
  NB. Explicit verb: debug it - if the new debugger is installed
  if. 0 ~: /: 1 4 5 ,:  _3 {. , 0&".;._2 '.' ,~ 's' -.~ '/' taketo 'Qt IDE:' takeafter JVERSION do.
    'You must update your JQt to support debugging explicit definitions' return.
  end.
  NB. Load the debugger if it's not loaded
  NB. Clear the debugger.  It must be initialized, so initialize it if needed
  NB. Turn debug on, wiping out any active session
  NB.?lintonly jdb_close_jdebug_ =: jdb_open_jdebug_ =: jdebug_splitheader_jdebug_ =: jdb_stoponall_jdebug_ =: nl_z_
  if. 13!:17'' do.
    dbg_z_ 0
  end.
  NB. We have to copy this code from dbg_z_ so that we can pass in the 'forcereopen' flag to
  NB. jdb_open.  This covers the fact that 13!:13 returns old data, but is used as a flag
  if. _1 = 4!:0 <'jdb_open_jdebug_' do.
    load '~addons/ide/qt/debugs.ijs'
  end.
  jdb_open_jdebug_ 1
  13!:0 [ 1

  NB. We have to find the locale in which the name is defined, regardless of where it
  NB. is referenced from, so that we can set stops in it.
  NB. This means that we have to pick up where name resolution stopped.  There
  NB. we had glopart and gloc, and we looked up the name glopart starting in gloc.
  NB. If the name was a local definition, glopart was empty.
  if. #verbglopart do.
    NB. glopart is a simplename, or name_loc_, or name__loc[__loc].  In the last case,
    NB. The first object locative has been stripped off already and is in gloc.
    NB. We want to look up the name starting in gloc.
    NB. First: if glopart ends with '_', split it into name and loc, and replace glopart/gloc
    NB. Note this assignment covers the global names, no prob
    if. '_' = {: verbglopart do. 'verbglopart verbloc' =. ('_' i:~ }: verbglopart) ({. ,&< <@((<<<0 _1)&{)@}.) verbglopart end.
    NB. Given the name & locale, see where the name is defined
    if. #nameloc =. findnameloc :: (''"_) verbglopart ,&< verbloc do.
      stopname =. {. jdebug_splitheader_jdebug_ nameloc
    else.
      NB. should not occur
      'No global definition found for name ',verbglopart,'.' return.
    end.
  elseif. '_' = {: execform do.
    NB. Name was resolved locally, as an immed locative.  Extract name & locale, & split the header
    stopname =. {. jdebug_splitheader_jdebug_ (({.~ ; (}.~ >:)) i:&'_') }: execform
  elseif. do.
    NB. Name was resolved locally, as simplename: set stops in its name, but don't split the header
    NB. (we can't since there is no locale).
    stopname =. execform
  end.
  NB. Set stops in every line of the verb we are about to execute
  NB. First we split the definition if it is a multiline definition,
  NB. into header/body.  Then we install stops on all lines of the body.
  NB. The stop code works on names, insensitive to locale
  1 jdb_stoponall_jdebug_ stopname
  NB. Because of debug peculiarities we need to have an immex come between setting the stops
  NB. and executing the debugged sentence.  So we store away our information and wait for the
  NB. mbrup event, where we start the debugger
  startdebuginfo__COCREATOR =: (coname''),argvbls;verbloc
end.
NB. Normal completion: empty string
''
)

NB. y is (arg tbl);(execution locale, single-boxed)
NB. We create the sandbox & execute it in immex
startdebug =: 3 : 0
'argtbl locale' =. y
createsandbox argtbl
9!:27 ('(1;',quote locale) , ') sandbox_dissect_ ' , quote('x '#~2=valence),execform,' y'
9!:29 (1)
)


cocurrent 'dissectmonad'
coinsert 'dissectallnouns dissectobj'
NB. Monad.  y is the locales of the verb and the noun
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ yop =: <'dissectnoun'
resultissdt =: resultissdt__uop
executingvalence__COCREATOR =: 1
NB. clear the flag that we use to see whether there is recursion inside this monad/dyad exec
recursionencountered__COCREATOR =: 0
NB. Tell the verb its valence; the result is the operands that are needed for display.  Here, in this non-verb,
NB. we save the operands needed by the first verb.  The rule is, we will pass to a verb ONLY the operands that
NB. it says it can use.  For comp. ease we may compute an operand but then immediately discard it.
uop =: setvalence__uop ,resultissdt__yop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
if. recursionhere =: recursionencountered__COCREATOR do.
  NB. If there is a recursion inside this execution, insert a recursion point
  uop =: 'dissectrecursionpoint' 1 createmodifier uop,yop
end.
NB.?lintonly uop =: <'dissectrecursionpoint'
NB. Collect input token #s in case of error
noun;(coname'');(; 2 {"1 y)
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y>0) (defstring__uop 0) jd ' ' , (defstring__yop 0)
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , (exestring__uop '') , ' (' , (exestring__yop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(<^:(0=L.)@".@>^:(0 <: y) (1 , y=3) # ;: 'uop tokensource'),(yop #~ y > 1)
)

NB. Traversal up and down the tree.
NB. x is the DOL(s) for the input operands
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is  DOL ,&< locale  where DOL is the accumulated display (leading up to the current result),
NB.   and locale is the current result, to be displayed eventually
traverse =: endtraverse@:(4 : 0)
hasrecursiveexpansion =: 0  NB. no expansion, unless we do it later
nounhasdetail =: 0  NB. No detail, unless we suppress history later
traversedowncalcselect y  NB. To set globals only - there are no inputs here
ylayo =. x traverse__yop y
NB. If a noun operand failed, pull the plug and display only that result
if. errorcode__yop > EOK do. ylayo return. end.  NB. If the noun failed, this node must have failed too
NB. put this locale on the stack as the outermost monad/dyad execution
executingmonaddyad__COCREATOR =: executingmonaddyad__COCREATOR ,~ coname''
if. hasrecursiveexpansion =: 1 = #ures =. (joinlayoutsl`<@.recursionhere ylayo) traverse__uop travops TRAVOPSSTARTHEAVY;TRAVOPSPHYSNEW;(uopval yop);<<selresultshape__yop do.
  NB. If we don't have a locale-name to inherit from, it means that uop was an expansion node
  NB. and it took over the display of u.  We must display the result here separately.
  displaylevrank =: ,: 'Result after all recursions';(coname'')
  ures =. ures ,< coname''
else.
  ures =. 0 1 inheritu ures  NB. OK to inherit stealth for monad
end.
NB. Remove the entry from the stack
executingmonaddyad__COCREATOR =: }. executingmonaddyad__COCREATOR
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displaylevrank nounhasdetail physreqandhighlights__inheritroot' =: NORANKHISTNOUN;1;<NOPHYSREQ
  NOLAYOUTS ,&< coname''
else.
  ures
end.
NB.?lintsaveglobals
)

NB. Add on a description of the rank line if applicable.  It is, if this is a Final node
exegesisrankoverall =: 4 : 0
appearstwice =. x
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
'datapresent endflag linetext' =. y
if. recursionhere do.
  if. linetext -: DLRCOMPEND do.
    NB. Result has not expanded
    t =. 'This is %al1%the final result of a recursive verb. To see the results of all recursions, click on the result of a recursion (in a block labeled $:).'
    if. tutor do.
      t =. t , ' This will create a new block, called an expansion block, feeding into this one.'
      t =. t , ' The expansion block will show the results from all levels of recursion, and you can select one to show the details of its computation.'
    end.
  else.
    NB. Result has expanded
    t =. 'This is %al1%the final result of a recursive verb. The expansion block feeding into this shows the results from each recursion.'
    if. tutor do.
      t =. t , ' To remove the expansion block, click in the result of this block.'
    end.
  end.
  res =. ,: (EXEGESISRANKOVERALLCOMPEND) ; t,LF
else.
  res =. 0 2$a:
end.
res
)

NB. Called when we have passed through this block without performing a selection.  If we have
NB. a recursive expansion block, this is how we remove it
postselectionoverride =: 3 : 0
if. hasrecursiveexpansion do.
  makeselection 0$a:
  PICKTOOLTIPMSGOK
else. PICKTOOLTIPMSGNOORIDE
end.
)

cocurrent 'dissectdyad'
coinsert 'dissectallnouns dissectobj'
NB. Dyad.
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'xop uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ xop =: yop =: <'dissectnoun'
resultissdt =: resultissdt__uop
NB.?lintonly uop =: <'dissectverb' [ yop =: xop =: coname''
executingvalence__COCREATOR =: 2
NB. clear the flag that we use to see whether there is recursion inside this monad/dyad exec
recursionencountered__COCREATOR =: 0
uop =: setvalence__uop resultissdt__xop,resultissdt__yop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
if. recursionhere =: recursionencountered__COCREATOR do.
  NB. If there is a recursion inside this execution, insert a recursion point
  uop =: 'dissectrecursionpoint' 1 createmodifier uop,yop,xop
end.
NB. Clear the flag that is set if the x argument starts execution
dyadxstarted =: 0
NB.?lintonly uop =: <'dissectrecursionpoint'
NB. Collect input token in case of error
noun;(coname'');(; 2 {"1 y)
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y>0) (defstring__xop 1) jd ' ' , (defstring__uop 0) jd ' ' , (defstring__yop 0)
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '(' , (exestring__xop '') , '[ dyadxstarted_',(>coname''),'_ =: 1) ' , (exestring__uop '') , ' (' , (exestring__yop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(xop #~ y > 1),(<^:(0=L.)@".@>^:(0 <: y) (1 , y=3) # ;: 'uop tokensource'),(yop #~ y > 1)
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
hasrecursiveexpansion =: 0  NB. no expansion, unless we do it later
nounhasdetail =: 0  NB. No detail, unless we suppress history later
traversedowncalcselect y  NB. To set globals only - there are no inputs here
ylayo =. x traverse__yop y
if. errorcode__yop > EOK do. ylayo return. end.  NB. If an argument failed, this node will have failed also
xlayo =. x traverse__xop y
NB. Same for x - but don't detect failure on x unless it actually started: u might have an error
NB. in an interior noun which would abort u before x ever got a chance to run.
NB.  BUT: it is vital that we NOT try to run u if there was an error in y.  In that case, u will never
NB. have executed at all, not even to the point of having ranks; and trying to display it will fail.
if. dyadxstarted *. (errorcode__xop > EOK) do. xlayo return. end.
NB. put this locale on the stack as the outermost monad/dyad execution
executingmonaddyad__COCREATOR =: executingmonaddyad__COCREATOR ,~ coname''
if. hasrecursiveexpansion =: 1 = #ures =. (xlayo ,&(joinlayoutsl`<@.recursionhere) ylayo) traverse__uop travops TRAVOPSSTARTHEAVY;TRAVOPSPHYSNEW;(uopval xop,yop);<selresultshape__xop ,&< selresultshape__yop do.
  NB. If we don't have a locale-name to inherit from, it means that uop was an expansion node
  NB. and it took over the display of u.  We must display the result here separately.
  displaylevrank =: ,: 'Result after all recursions';(coname'')
  ures =. ures ,< coname''
else.
  ures =. 0 1 inheritu ures  NB. Don't inherit stealth - we want to show a result
end.
NB. Remove the entry from the stack
executingmonaddyad__COCREATOR =: }. executingmonaddyad__COCREATOR
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displaylevrank nounhasdetail physreqandhighlights__inheritroot' =: NORANKHISTNOUN;1;<NOPHYSREQ
  NOLAYOUTS ,&< coname''
else.
  ures
end.
NB.?lintsaveglobals
)

exegesisrankoverall =: exegesisrankoverall_dissectmonad_ f.

postselectionoverride =: postselectionoverride_dissectmonad_ f.

cocurrent 'dissectrecursionpoint'
coinsert 'dissectobj'

NB. We have already created the locale for this object.  We just add what is needed to support recursion
create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
uop =: {. y   NB. The verb locale
NB.?lintonly uop =: <'dissectverb'
yxop =: }. y  NB. The operand locale(s)
NB. Since this is called after setvalence, we insert valence-related stuff here
valence =: <: #y
resultissdt =: resultissdt__uop
coname''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2)
)

NB. return string form of operands, including instrumentation within u but not within inverse of u
exestring =: 3 : 0
initloggingtable 0
NB. Use dyad logging to log out the type of logentry.  0/1 are operand x/y on entry; _1 is result on exit
NB. If u has an unused operand, we should put @] or @[ after the inverse, because Roger can't handle x u@[^:_1 y but he can handle x u@[^:_1@[ y
xlogstring =. valence {:: ''  ;  '[:'  ;  '(',(logstring 0),')@['
auditstg '(' , (verblogstring '') , '(' , (logstring _1) , '@:((',xlogstring,')' , '(' ,  (exestring__uop '') ,') (',(logstring 1),')@] )))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) ;: 'uop'
)

NB. Traversal up and down the tree.
traverse =: endtraverse@:(4 : 0)
NB. sellevel may not be 0 here, if the recursion is in a nilad or NVV that runs at a level
traversedowncalcselect y
NB. Inside travdowncalcselect, we implied a selection of 0 even if there was no selection.  This was necessary
NB. to get the selector calculated even when there is no selection.

NB. We always traverse u.  If no recursion has been selected, we hide the expansion: we reset the selection level
NB. back to 0, and simply return the result from u.  If there is a selection, we show the expansion, by inheriting u
NB. into it and creating a layout for it, and we return the layout for the expansion with no locale, signifying that
NB. the monad/dyad should display its result without inheriting.
oprshape =. 0$a:
if. sellevel = #selections do.
  bnsellevel =: 0{y  NB. reset selection level if the expansion is dormant
  for_l. yxop do.
    NB.?lintonly l =. <'dissectverb'
    oprshape =. oprshape ,~ < selresultshape__l
  end.
else.
  NB. We need to reach into the noun operand(s) and change their selresult to show the values used for this recursion level.
  NB. We also have to recalculate the layout
  for_l. yxop do.
    NB.?lintonly l =. <'dissectverb' [ levelstartx =: 0
    'displaylevrank__l selresult__l nounhasdetail__l' =: (((2=#yxop) {:: 'I';' i' ,~ l_index { 'yx') , 'nput to recursion ',": (0;0) {:: selections);((levelstartx+l_index){logvalues);1
    oprshape =. oprshape ,~ < $ L:0 > {. selresult__l
  end.
end.
NB. We suppressed the joinlayouts in the monad/dyad; do it now, now that we have the correct labels and values for the display
ures =. (; joinlayoutsl&.> x) traverse__uop travops TRAVOPSSTARTHEAVY;TRAVOPSPHYSNEW;(uopval |.yxop);< oprshape
NB. return value may have been set at this point
if. sellevel < #selections do.
  udol =. 0 1 1 inheritu ures  NB. This is an expansion
  displaylevrank =: displaylevrank , 'Recursions';coname''
  < joinlayoutsl udol
end.  NB. instantiate expansion if it is live
)

NB. Add on a description of the rank line if applicable.  It is, if this is a Final node
exegesisrankstack =: 3 : 0
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
t =. 'The contents of each box shows the result of one recursion, in the order they were started. Select one to see the inputs and computation that produced it.',LF,LF,'This rank stack also contains the last verb in the computation.'
if. tutor do.
  t =. t , LF,LF,'The display is a snapshot of the calculation of the selected recursion.  The inputs to the computation and the results of recursive calls are the ones that contribute to the selected result.'
end.
,: EXEGESISRANKSTACKEXPLAIN ; t,LF
)

NB. Traversal support

NB. y is anything, result is, for each item of y, the number of preceding equal items of y.
itemserial =:  i.~  (] - {) /:@/:

calcdispframe =: 4 : 0
NB.?lintonly xop =: <'dissectpartitionselector'
NB. The pseudoframe (# of recursions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.  Each recursion is marked with a 1 in logvaluesd
ny =. , 1 +/@:= logvaluesd
NB. Calculate permutations to connect inputs with outputs.  We have to number the recursions
NB. by starting order, because if there is an error there may be no ending order.  But the results
NB. are saved in ending order.  So we will have to reorder for display, and for clicking
NB. Get the index number of each start and end
startlocs =: 1 I.@:= logvaluesd
NB. In case of error, pad missing end values to point to where we will add an 'infinity'
endlocs =: (#startlocs) {.!.(#logvaluesd) _1 I.@:= logvaluesd
NB. Get the recursion level of each entry, the running sum of each action
rlevel =. +/\ logvaluesd
NB. Represent each start (and end) as recursion level,sequential number at that level
startls =. (,. itemserial) <: (1 = logvaluesd) # rlevel
endls =. (,.  itemserial) (_1 = logvaluesd) # rlevel
NB. Create the list that gives the ending index corresponding to each starting index.  If there
NB. are errors, the index is off the end of endls (but not startls).  In this case we must ensure
NB. that the LAST unmatched start corresponds to the FIRST missing index, because the first missing
NB. index will be diagnosed as the point of error.  We add the item serial number to the index to
NB. make them unique
endforstart =: (+ itemserial)&.|. endls i. startls
ny =. ,#startlocs
ny ; ny ; (valence # <ny) ; 1 ; a:
NB.?lintsaveglobals
)

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
selindextoisf =: 4 : 0
NB. We need to select the starting index of the recursion that would have been next to finish - that is
NB. where the error is
NB. We add a dropdown, since this is a selection node
if. selectable do. < (< ,endforstart i. y) , SFOPEN else. '' end.
)

NB. We say that a cell was executed if it started, regardless of whether it finished.
cellswereexecuted =: 3 : '*#startlocs'

NB. y is the new selection (boxed, and possibly with an initialselection following)
NB. Result is signum of (sel - error spot): 1 if invalid, 0 if on the error, _1 if no error
auditselection =: _1:  NB. We can never select out of bounds, since only started recursions show up in the display


NB. y is a selector: any shape, but each selector has shape ,2; so $=?,2
NB. Result is array of boxes with one box for each selector, containing the
NB. indices of the results for each selector
findselection =: 3 : 0
NB. In this node the selector selects everything.  We just return all the indexes
NB. where valuesd is _1 (those are the results) 
, < _1 I.@:= logvaluesd
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
getselection =: 3 : 0
NB. We always return type 1 (selected) so that we go through selectusingisf to calculate the actual selector,
NB.  since even recursion level 0 requires a selector.  We default to the last (outermost) value
if. (sellevel < #selections) do. 1 ;< sellevel { selections
else. 1 0
end.
)

NB. y is selection, in ISF form
NB. x is the selectors, shaped into a form suitable for indexing
selectusingisf =: 3 : 0
{.>y   NB. We know y is a single selection, but always double-boxed, since it has a dropdown
:
NB. We ignore y, the selectors calculated by selectticketintervals - they just include all the results, since the incoming selector
NB. was known to select everything.
NB. Instead, we develop the selector that picks out everything at the selected recursion level.  We find the
NB. indexes into valuesd/ticket where it starts and end+1s.
NB. Save the index of the startpoint - we will use this to fetch the operands
levelstartx =: ({.>y) { startlocs
selthisrecur =. ,. -~/\ levelstartx , >: endlocs {~ endforstart {~ {.>y
NB. Then, every time recursion level goes to 1 starts an included interval that runs through the next log entry (which must exist, either going up
NB. to a deeper level or down to finish this level, unless there was an error).
NB. But remember, there are 0 entries for dyad x-values, so we have to ignore tham so they don't start intervals
validthislevel =. 0 ~: valuesdthislevel =. selthisrecur ];.0 logvaluesd
runlevel =. validthislevel # +/\ valuesdthislevel
NB. In case there is an error and the last start never finished, append a high-value
((,. >:) 1 I.@:= runlevel) { (validthislevel #  selthisrecur ];.0 logticket) , ticket__COCREATOR
)

NB. y is anything - intervals, results, fillmasks - that has been expanded into an array using the shape of the frame.
NB. y is in ticket order, i. e. execution order
NB. Result is the array reordered to natural order, i. e. selection order (some primitives process out of order; we reorder to match selection)
tickettonatural =: 3 : 0
NB. The values in y are in order of completion.  Selection order is order of start.
endforstart { y
)


calcphysandhighlights =: 3 : 0
NB. Never highlight from this node
2 0$a:
)

NB. The 'selection' in this node has nothing to do with the operands; it selects a recursion.
NB. And, the selected operands out of this node are not used.  So, stub this out
calcselectedshapes =: 3 : 0
a:"0 selopshapes
)

NB. y is boxed selection (number only, no SFOPEN) in natural order; result is boxed selection in execution order
selectiontoticket =: 3 : 0
NB. y is in selection order (i. e. start position).  We return the corresponding end position
{&endforstart&.> y
)

NB. y is a ticket number for a result from a recursion here
NB. We select the recursion that caused the return
NB. Called when we get a pick in a recursion result
selectrecursion =: 3 : 0
NB. Find the last ticket below the input value; this is our return from that recursion.
NB. Since the recursions are ordered on completion, we simply count the number of ends
NB. before that one
NB. propagate that selection, after adding a dropdown.  We know we are at selection level 0
makeselection ,< SFOPEN ,~ <, endforstart i. <: (endlocs { logticket) I. y
)

NB. **** assignment ****
cocurrent 'dissectassign'
coinsert 'dissectobj'

NB. Assignment does nothing and has no display (for now).  We just have to keep things going for sentence display

NB. Assignment.  y is the stack block of the assignment fragment
create =: 3 : 0
'y noassign' =. y
NB. not clonable
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands.  uop is the locale of a noun, or the string for a name
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =:<'dissectnoun' [ vop =: <'dissectverb'
NB. Remember if uop is a name
uopisname =: name = (<0 0) {:: y
utoken =: (<0 2) { y
NB. Since we don't participate in traversal, fix it so that references to this locale are picked up
NB. by the object of assignment.  We will take resultissdt from the assigner.  We can do this only if
NB. the value is a noun
if. noun bwand (<2 0) {:: y do.
  coinsert vop
end.
NB. If assignment is suppressed, turn off visibility in the left side
if. noassign do.
  tokensvisible =: 0
  if. -. uopisname do. suppresstokens__uop 0 end.
end.
NB.?lintonly uop =: vop =: <'dissectverb'
NB. Return the part of speech of the assigned value
((<2 0){y),(coname'');tokensource
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
if. uopisname do.
  enparen^:(y>0) uop jd cop jd (defstring__vop 0)
else.
  enparen^:(y>0) (enparen defstring__uop 0) jd cop jd (defstring__vop 0)
end.
)

NB. This will only be called if the rvalue is a verb; in that case, pass the call on
setvalence =: 3 : 0
vop =: setvalence__vop y
coname''
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
if. uopisname do.
  auditstg '(' , (logstring'') , (tokensvisible # uop , ' ' , cop) , (exestring__vop '') , ')'
else.
  NB. Make sure we execute u, even if we don't see it, so we see what assignments we are missing
  if. tokensvisible do.
    auditstg '(' , (logstring'') , ('(' , (exestring__uop '') , ' )' , cop) , (exestring__vop '') , ')'
  else.
  auditstg '(' , (logstring'') , ('(' , (exestring__uop '') , ' ) ] ') , (exestring__vop '') , ')'
  end.
end.
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. If uop is a name, we just return its token number; if a value, we return the locale of the noun
if. uopisname do. (y>2 2 1) # utoken,tokensource;vop
else. (y>1 2 1) # uop,tokensource;vop
end.
)

NB. Traversal up and down the tree.
traverse =: endtraverse@:(4 : 0)
res =. x traverse__vop y
NB. We have to transfer the result code from v to this node, so that an error in v will stop monad/dyad
NB. traversal.
if. (errorcode =: errorcode__vop) <: EOK do.
  NB. Put the names assigned into the 'assignmentlabel' variable in the source object (v).
  NB. If there is an error, this might not be the block being displayed, but in that case
  NB. we wouldn't be showing the assignment anyway, so no big deal.  Limit the length.
  if. uopisname do. assignmentlabel__vop =: cop ,~ uop
  else.
    udol =. NOLAYOUTS traverse__uop TRAVNOUN
    NB. If noun operand failed, pull the plug and display only that result
    if. errorcode__vop > EOK do. udol return. end.  NB. If the noun failed, this node must have failed too
    assignmentlabel__vop =: cop ,~ ;:^:_1^:(0<L.) fillmask__uop frameselresult__uop selresult__uop
  end.
  NB. See if the assignment failed.  (it might fail on a length error, or domain error if a name had been
  NB. assigned locally already).  If the assignment failed, raise an error code here.  Leave the error code
  NB. is the source block cleared, so that the error is picked up in the assignment block.
  if. 0 = #logvalues do.
    errorcode =: ENOEXECD   NB. Indicate this didn't run
    assignmentlabel__vop =: ' ' , assignmentlabel__vop  NB. Add leading space to indicate error
  end.
end.
res
)




NB. ********** handling of modifiers **************
NB. Each group of modifiers has a locale to create objects of its type

cocurrent 'dissect'
dissectprimindex =: 0$a:  NB. list of (<list of boxed modifier words)

NB. x is object class(es) to include, default 'dissectobj'.  These classes are put in order
NB. at the top of the search path
NB. The class is erased first, in case there are definitions we need to lose
NB. y is string containing the modifiers that will be handled in this
NB. locale.  Result is the locale name.  Side effect: index extended
NB. MAJOR SIDE EFFECT: locale is changed
primlocale =: ''&$: : (4 : 0)
NB.?lintmsgsoff
cocurrent@(0&cocreate)@([ coerase) newloc =. <'dissectprim' , ": <: # dissectprimindex_dissect_ =: dissectprimindex_dissect_ , <;: y
NB.?lintmsgson
coinsert x , ' dissectobj'
18!:4 newloc   NB. No named verb from here to the end!
newloc
)

cocurrent 'dissectobj'
NB. Append x to the current locale name.  Used when a modifier is so dissimilar
NB. between monad & dyad as to require different locales.  Executed inline during load
NB. y is list of attribute class to include after the common class
NB. The path for the valence is set to the bivalent locale, followed by locales given in y, followed by the
NB.  path for the bivalent locale
NB. Major side effect: locale is changed
startvalence =: 4 : 0
NB.?lintmsgsoff
cocurrent@(0&cocreate)@([ coerase) newloc =. ,&x&.> baseloc =. <'dissectprim' , ": <: # dissectprimindex_dissect_
NB.?lintmsgson
((/: =&(<,'z')) ~. (,    (;:y) , 18!:2) baseloc) 18!:2 newloc
newloc   NB. Return locale name, which we will switch to
)
startmonad =: ([ 18!:4)@('monad'&startvalence)
startdyad =: ([ 18!:4)@('dyad'&startvalence)

NB. Nilad.  Executed at setvalence time.  Inserts the monad/dyad locale and its path at the top of the search path
NB. of the current object, depending on the valence.
NB. Path becomes 'moddyad';'mod';dyad attribute locales;obj etc.
separatevalences =: 3 : 0
NB. The current object may be a clone, if it comes from dyad &; so we search to find the type locale,
NB. and preserve everything before it as a prefix 
oldpreflen =. i.&0@:e.&'0123456789' {.@> oldpath =. 18!:2 loc =. coname''
newtype =. ((valence=2) { 'monad';'dyad') ,~&.> oldpreflen { oldpath
((/: =&(<,'z')) ~. (oldpreflen {. oldpath) , newtype , (18!:2 newtype) , (>: oldpreflen) }. oldpath) 18!:2 loc
''
)

NB. **** @ @: ****
NB. Save the name of the locale that handles @@: - we use it in &&: and also in fork
localeat_dissect_ =: 'dissectextendv' primlocale '@@:'

NB. *** special arguments ***
NB. cop starts with a space for hidden internal operations.  titlestring is always null, and we don't add an end-of-computation line
NB. If cop is '[:', this is a capped fork and we display it that way
NB. We may come here through & (which may be through &.), but only for monad, so in that case we leave the & unchanged, holding monad &[:];
NB.  but we remove the . if any for exestring (no need to bother for defstring, since that is handled by dual)
create =: 3 : 0
NB. For u@n, replace n with n"_ which is its equivalent.  The conjunction-name is a signal to " to suppress the "_ for display
if. nilad =: ((,'@') -: (<1 1){::y) *. * noun bwand (<2 0){::y do.
  rank =. COCREATOR createnoun ('_');'';($0);_
  y =. y 2}~ localerank 1 createmodifier (2{y) ,  (conj;'@';($0)) ,: rank
end.
if. 0 = bwand/ verb , > (<0 2;0) { y do.
  if. ':' e. (<1 1){::y do.
    failmsg 'domain error: operands to ',((<1 1){::y),' must be verbs'
  else.
    failmsg 'domain error: left operand to ',((<1 1){::y),' must be a verb'
  end.
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
if. capped =: '[:' -: cop do. cop =: '@:' end.  NB. Saved capped status, replace for exestring
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
invertiblymonadic =: invertiblymonadic__vop
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
uop =: setvalence__uop resultissdt__vop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB. Figure the NuVoc page for this primitive.  cop can be [: @ @: & &: &. &.:
if. IFQT do.
  if. capped do. nuvocpage =: 'fork#cappedfork'
  elseif. cop -: ,'&' do. nuvocpage =: 'ampv'
  elseif. do.
    nuvocpage =: ('at';'amp';'') {::~ '@&' i. {. cop
    if. '.' e. cop do. nuvocpage =: nuvocpage , 'dot' end.
    if. ':' e. cop do. nuvocpage =: nuvocpage , 'co' end.
  end.
end.
NB. Return the locale of this compound
coname''
NB.?lintsaveglobals
)

NB. 1 if this verb may not have infinite rank, or if this node displays already
shownilad =: 3 : 0
NB. For @, treat it like @>, which is like "0, always displaying (unless it's @n)
NB. for @:, pass y on through
y +. -. (nilad +. ':' e. cop)
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
if. capped do.
  enparen^:(y~:0) '[: ' jd (defstring__uop 3) jd (defstring__vop 0)
else.
  enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
end.
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ' , (cop-.'.') , ' (' , (exestring__vop '') , ')))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
r =. (<^:(0=L.)@".@>^:(0 <: y) ;: 'uop vop')
if. y = 3 do.
  r =. (<tokensource) , r
  if. 0 ~: 4!:0 <'vvv' do.  NB. NOT capped fork, use u@v form
    r =. 1 0 2 { r
  end.
end.
r
)

NB. y is the child of the locale that this verb is being executed in
NB. (except first time, where it is the node itself)
NB. Return the nearest ancestor (including this node itself) that
NB. has operands, and which operand(s) this node represents.
NB. Result is locale;operand code:
NB. 2=@ (or monad &), 0=& (x operand), 1=&(y operand)
NB.
NB. If y was vop, check that parent; otherwise stop the chain
findparentwithshapes =: 3 : 0
NB. If somewhere along the line we encounter &, the original request becomes one side
NB. of a dyad.  We have to remember that, so pass on the return from the call
NB. Only @ and & should go through here - exclude @: &: [:
NB. If this node has shapes, stop, this is it
if. (vop = y)  *. (':' -.@e. cop) do.
  NB. The child was in the v side of u@v.  That means the parent is eligible as
  NB. a source of the operands.
  if. 0 ~: #inputselopshapes do. (coname '');2  NB. parent has operands: use them
  else. findparentwithshapes__parent coname''  NB. otherwise, search ITS parent
  end.
else. y;2   NB. Previous locale was the end, and no & found
end.
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 1 fulltitlestring cop
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Run v.  The result is dol;code where code is either a locale or an initialselection
x =. joinlayoutsl x traverse__vop travops TRAVOPSKEEPLIGHT;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
NB. execute u
NB. If v is a stealth operand, rankhistory and highlighting that would have been applied on v will have disappeared,
NB. so in that case we transfer them to u
NB. We pass only the side of the physsels that is applicable: that's all if it's a monad, or one side if it's a dyad
NB. stealth type 5, ]], is used by dyad u&n.  It causes problems because it turns the u leg of u&n into a monad, but then subsumes its display into
NB. the hidden power which is a dyad.  This causes loss of detail is the u really does need to be a dyad, for example if it contains another u&n.  Our best
NB. solution is not to treat it as stealth
select. dispstealthoperand__vop
case. 1;2;6 do.
  stealthcode =. 3 bwand dispstealthoperand__vop
  NB. We figure out which column is to be deleted (if any).  If that column has important rank (indicated by '!'), don't delete the column - this
  NB. means that the unseen data has an important effect on the result rank.
  if. valence = 1 do.
    rankhistcode =. TRAVOPSKEEPALL
  else.
    coltolose =. stealthcode { 0 3 2  NB. x ] [   to lose column x x y
    NB. We keep the unselected column if it has important data, or if it has a rank and the other column doesn't
    if. ('!' e. ; coltolose {"1 rankhistory) +. +./ >/"1 *@#@> (coltolose,5-coltolose) {"1 rankhistory do.
      rankhistcode =. TRAVOPSKEEPALL
    else.
      rankhistcode =. TRAVOPSKEEPINALL 0 1 2 3 -. coltolose
    end.
  end.
  rankphyscode =. rankhistcode;(valence=2) { TRAVOPSPHYSKEEP;(TRAVOPSPHYSCHOOSE stealthcode { _1 _2 0 _1  )   NB. 0=left _2=right
case. 5 do.
  NB. For the internal case of u&n, keep the whole rankstack.  Ignore highlights, since they go through the power selector anyway
  rankphyscode =. TRAVOPSKEEPALL;TRAVOPSPHYSNEW
case. do.
  rankphyscode =. TRAVOPSSTARTHEAVY;TRAVOPSPHYSNEW
end.
NB. Space in cop is used for hidden nodes, so no end-of-comp record then
(' ' ~: {. cop) inheritu x traverse__uop travops rankphyscode,(uopval vop);<<selresultshape__vop
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. lastinblock do.   NB. If we are the last block, save description for the end (should not occur, unless stealth)
  res =. 0 2$a:
else.
  t =. 'This is the first part of the verb:',LF,(defstring 0),CR
  select. appearstwice, datapresent
  case. 0 0 do.
    t =. t , 'This block will contain a single intermediate result for the verb after sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
  case. 0 1 do.
    t =. t , 'This block contains an intermediate result for the verb.  The overall result is shown in the final block of the verb.',LF
  case. 1 0 do.
    t =. t , 'When a single result-cell is selected, this block will contain the result.',LF
  case. do.
    t =. t , 'This block contains the result.',LF
  end.
   res =. ,: EXEGESISRANKSTACKEXPLAIN;t
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit
  end.
  res
else.
  0 2$a:
end.
)


NB. **** & &: ****
NB. Save the name of the locale that handles &&: - we use it in &. and &.:
localecompose_dissect_ =: 'dissectextendv' primlocale '&&:'

NB. When we find out the valence, we change this to be like @@: if monad, and proceed here only for the dyad

NB. *** special arguments ***
NB. cop contains '.' if this node comes from &.[:].  We use that fact to fiddle with locales.  Otherwise we remove the '.' when we refer to cop.
create =: 3 : 0
NB. If this is u&n or m&v, change the object type to bond, and switch over to that create routine
if. (,'&') -: (<1 1){::y do.
  if. noun bwand bwor/ > (<0 2;0) { y do.
    changeobjtypeto 'dissectvandnm'
    create y return.
  end.
elseif. 0 = bwand/ verb , > (<0 2;0) { y do.
  failmsg 'domain error: operands to ',((<1 1){::y),' must be verbs'
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB. If this verb is in a gerund, it will not know how to proplocales if it has no valence.
NB. So we initialize valence to 0 to detect that case.
valence =: 0
NB. We use localeat for display, so we have to set the global it uses: an indication of capped fork
capped =: 0
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Look at the conjunction used, and use that to find the rank of this object (the derived verb)
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
NB. The monad is just like @@:, so we just point to that locale.  This also applies if u
NB. behaves like a monad
if. 1 = #y do.
  changeobjtypeto localeat
  nilad =: 0   NB. used by localeat
  NB. kludge if this locale is dual, replace the dual in the path to preserve display hooks
  if. '.' e. cop do. insertoverride localedual end.
  NB. leave the . in the conjunction for titlestring purposes - it will be removed before use
NB. Now that we have changed conjunctions, we need to go figure the valences based on the new conjunction
  setvalence y  NB. This will be in the new locale
else.
NB. For dyad, we need to clone v.
  vop1 =: clone__vop ''
  vop0 =: vop
NB.?lintonly vop0 =: vop1 =: <'dissectverb'
  valence =: #y
  vop0 =: setvalence__vop0 {. y
  vop1 =: setvalence__vop1 {: y
NB.?lintonly vop0 =: vop1 =: <'dissectverb'
  uop =: setvalence__uop resultissdt__vop0 , resultissdt__vop1
NB.?lintonly uop =: <'dissectverb'
  resultissdt =: resultissdt__uop
end.
NB.?lintonly vop0 =: vop1 =: <'dissectverb'
coname''
NB. We always get both operands for v, since we have cloned vop0/vop1 (it's not worth saving the operand).
NB.?lintsaveglobals
)

NB. 1 if this verb may not have infinite rank, or if this node displays already
shownilad =: 3 : 0
NB. For &, treat it like &>, which is like "0, always displaying
NB. for &:, pass y on through
y +. -. ':' e. cop
)

NB. return string form of operands, not including instrumentation
defstring =: defstring__localeat f.

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
rankstg =. ((cop -. '.') -: ,'&') # '"', (')' ,~ '(]&' , defstring__vop0 3)
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , '(' , (exestring__vop0 '') , '@[ ' , (exestring__uop '') , (exestring__vop1 '') , '@] ' , ')' , rankstg , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
if. valence = 0 do. proplocales__localeat f. y  NB. Handles gerunds where we don't know our valence
else.
  NB. For highlighting the sentence, we need only one clone.  Use the first
  if. y = 3 do.
    uop,(<tokensource),vop0
  else.
    (<^:(0=L.)@".@>^:(0 <: y) ;: 'uop vop0 vop1')
  end.
end.
)

NB. y is the child of the locale that this verb is being executed in
NB. (except first time, where it is the node itself)
NB. Return the nearest ancestor (including this node itself) that
NB. has operands, and which operand(s) this node represents.
NB. Result is locale;operand code:
NB. 2=@ (or monad &), 0=& (x operand), 1=&(y operand)
NB.
NB. If y was vop, check that parent; otherwise stop the chain
findparentwithshapes =: 3 : 0
NB. If somewhere along the line we encounter &, the original request becomes one side
NB. of a dyad.  We have to remember that, so pass on the return from the call
NB. Only @ and & should go through here - exclude @: &: [:
NB. If this node has shapes, stop, this is it
if. (2 > opno =. y i.~ vop0,vop1)  *. (':' -.@e. cop) do.
  NB. The child was in the v side of u&v.  That means the parent is eligible as
  NB. a source of the operands.
  if. 0 ~: #inputselopshapes do. (coname '');opno  NB. parent has operands: use them
  else. (<opno) 1} findparentwithshapes__parent coname''  NB. otherwise, search ITS parent, but remember which way we went
  end.
else. y;2   NB. Previous locale was the end, and no & found
end.
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 1 fulltitlestring cop
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
dol =. joinlayoutsl (0 1 # x) traverse__vop1 travops (TRAVOPSKEEPINLIGHT 0 1 2);TRAVOPSPHYSKEEP;(vopval 1 { selopinfovalid);selopshapes;1
dol =. dol ,~ joinlayoutsl (1 0 # x) traverse__vop0 travops (TRAVOPSKEEPINLIGHT 0 1 2);TRAVOPSPHYSKEEP;(vopval 0 { selopinfovalid);selopshapes;0
1 inheritu dol traverse__uop travops TRAVOPSSTARTHEAVY;TRAVOPSPHYSNEW;(uopval vop0,vop1);<selresultshape__vop0 ,&< selresultshape__vop1
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. lastinblock do.   NB. If we are the last block, save description for the end (should not occur, unless stealth)
  res =. 0 2$a:
else.
  t =. 'This is the first part of the verb:',LF,(defstring 0),CR
  select. appearstwice, datapresent
  case. 0 0 do.
    t =. t , 'This block will contain a single intermediate result for the verb after sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
  case. 0 1 do.
    t =. t , 'This block contains an intermediate result for the verb.  The overall result is shown in the final block of the verb.',LF
  case. 1 0 do.
    t =. t , 'When a single result-cell is selected, this block will contain the result.',LF
  case. do.
    t =. t , 'This block contains the result.',LF
  end.
   res =. ,: EXEGESISRANKSTACKEXPLAIN;t
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring 
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit
  end.
  res
else.
  0 2$a:
end.
)


NB. **** u&n m&v ****
cocurrent 'dissectvandnm'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Don't try to remember locale yet - we might clone
verboperandx =: * verb bwand (<2 0) {:: y   NB. Index of the verb operand
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. if dyad, switch to dyad routine
setvalence =: 3 : 0
if. 2 = #y do.
NB. Dyad case.  Emulate as u&n@]^:["_ 0 _
NB. We will return the locale of the overall verb
  rbkt =. 1 {:: COCREATOR createverb (']]');($0)  NB. ']', never displayed
  uatr =. 1 {:: localeat 1 createmodifier _3 [\ verb;(coname'');($0);  conj;'@:';($0);  verb;rbkt;$0  NB. u&m@]
  NB.?lintonly uatr =. localeat
  lbkt =. 1 {:: COCREATOR createverb (,'[');($0)  NB. '['
  NB. The conjunction that we send to localepower is ' ^:' for m&v and '^: ' for u&n; in other words the space
  NB. indicates the position of the noun operand.  The space also tells fulltitlestring not to display ^:
  uatrpwrl =. 1 {:: localepower 1 createmodifier _3 [\ verb;uatr;($0);  conj;(verboperandx {:: '^: ';' ^:');($0);  verb;lbkt;$0  NB. u&m@]^:[  don't display ^:
  NB.?lintonly uatrpwrl =. localepower
  rank =. 1 {:: COCREATOR createnoun ('_ 0 _');'';($0);_ 0 _  NB. 0 1 0
  final =. 1 {:: localerank 1 createmodifier _3 [\ verb;uatrpwrl;($0);  conj;'';($0);  noun;rank;$0  NB. u&m@]^:["_ 0 _  don't display "
  NB.?lintonly final =. localerank
  setvalence__final y
  '' insertoverride__uatrpwrl 'dissectvandnmdyad'
NB. Save the locale to use for display of this node in ^:
  disploc__uatrpwrl =: coname''
  '' insertoverride__uatr 'dissectvandnmdyad'
  disploc__uatr =: coname''
  '' insertoverride__final 'dissectvandnmdyad'
  disploc__final =: coname''
  if. IFQT do. nuvocpage__final =: 'ampm#dyadic' end.
  final
else.
  valence =: #y
  'vl nl' =. <"0 verboperandx |. uop,vop
NB.?lintonly vl =. nl =. coname''
NB.?lintmsgsoff
  (verboperandx { 'uop';'vop') =: setvalence__vl verboperandx |. y , resultissdt__nl
NB.?lintmsgson
  resultissdt =: resultissdt__vl
  if. IFQT do. nuvocpage =: 'ampm' end.
  coname''
end.
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd '&' jd (defstring__vop 3)
:
if. verboperandx do.
  rstg =. defstring__vop 3
  lstg =. (x - 1 + #rstg) ([ '...'"_^:(< #) defstring__uop) 2
else.
  lstg =. defstring__uop 2
  rstg =. (x - 1 + #lstg) ([ '...'"_^:(< #) defstring__vop) 2
end.
enparen^:(y=3) lstg jd '&' jd rstg
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. Safe to remember locale now - cloning is over
'verbop nounop' =: verboperandx |. <"0 uop,vop
NB.?lintonly verbop =: nounop =: coname''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , '&' , (exestring__vop '') , '))'
NB.?lintsaveglobals
)

NB. Return the locales for propsel
proplocales =: 3 : 0
if. y = 3 do.
  uop,(<tokensource),vop
else.
  (<^:(0=L.)@".@>^:(0 <: y) ((y>0) +. verboperandx = 0 1) # ;: 'uop vop')
end.
)

NB. Traversal up and down the tree.w
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 0 fulltitlestring cop    NB. Use the & as the name in the rank stack
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
dol =. verboperandx |. x , joinlayoutsl NOLAYOUTS traverse__nounop TRAVNOUN
NB. As we are moving from monad to dyad, add the appropriate column to the existing rankhistory to put previous monad ops on the correct side
NB. The rank value just added is now in the column the verb will be in, so its rank must be replicated on both sides.
NB. Get the actual rank of the noun operand.  We'll put this into the last line.  The rankstack will then hold
NB. label,locale,y,empty/mn.  If the verb is m&v, this will be correct; if it's u&n, we need to switch it.
NB. verboperandx, the index of the verb operand, is 0 for u&n
nounrank =. #@$@>@{. selresult__nounop 
rankhistory =: 0 1 3 2 {"1^:(-.verboperandx) (<'!',":nounrank) (<_1 _1)} 4 {."1 rankhistory
inheritu dol traverse__verbop travops TRAVOPSKEEPALL;(TRAVOPSPHYSCHOOSE verboperandx |. 0 _1);(vopval selopinfovalid);<verboperandx |. selopshapes,<selresultshape__nounop
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN ; 'The constant value is brought in as the ',(verboperandx { 'yx'),' argument.',LF
)

cocurrent 'dissectvandnmdyad'
coinsert 'dissectvandnm'

NB. This locale contains the overrides needed to display dyad & properly
NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
NB.?lintmsgsoff
enparen^:(y=3) (defstring__uop__disploc 2) jd '&' jd (defstring__vop__disploc 3)
NB.?lintmsgson
)

NB. Return the locales for propsel type 3
propseltokens =: 3 : 0
NB.?lintmsgsoff
uop__disploc,(<tokensource__disploc),vop__disploc
NB.?lintmsgson
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN ; 'The x argument tells how many times the verb:',LF,(defstring 0),CR,'is executed.  The x argument is displayed as coming into the right edge of the block.',LF
)

NB. This is called by ^: when the expansion is not used.  We need to remove the x operand,
NB. which doesn't actually feed into the verb

cocurrent 'dissectobj'


NB. **** &. &.: ****
localedual_dissect_ =: (>localecompose_dissect_) primlocale '&.&.:'
NB. we emulate this with v^"_1@:u&[:]v.  The only thing we do here is defstring, so we can recover the original display form of the verb; and exegesis

create =: 3 : 0
if. 0 = bwand/ verb , > (<0 2;0) { y do.
  failmsg 'domain error: operands to ',((<1 1){::y),' must be verbs'
end.
NB. Handle the special form we recognize: &.>
if. ('&.' -: (<1 1) {:: y) *. (verb -: (<2 0) {:: y) do.
  vloc =. (<2 1) {:: y
  NB.?lintonly vloc =. <'dissectverb'
  NB. We check the execform, which may have been translated from a name.  Only verbs have execforms
  if. 0 = 4!:0 <'execform__vloc' do. if. (,'>') -: execform__vloc do.
    changeobjtypeto 'dissecteach'
    create ({.y) ,: adv;''; ; (<1 2;2) { y return.
  end. end.
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
'uop0 cop0 vop0' =: 'uop cop vop' =: 1 {"1 y  NB. Save under private names so we can return defstring
NB.?lintonly uop0 =: vop0 =: vop =: <'dissectverb' [ cop0 =: '&.:'
NB. Create an object to handle v^:_1@:u
NB. First, the verb v^:_1
iop =. 1 {:: COCREATOR createverb ((defstring__vop 2),'^:_1');($0)
NB. Now create an object for vi@:u
NB. Remove the . from &.&.: and create vi@:u
NB. Use leading space in the conjunction to create a node that never creates a titlestring
uop =: 1 {:: localeat 1 createmodifier (_3 [\ verb;iop;($0);conj;' @:';($0)) , 0 { y
NB.?lintonly uop =: <'dissectverb'

NB. Now change this locale to &&: and create i&v
NB. Replace the uop with the uop we just created
NB. We leave the &. locale in the path, so that we can override
NB. We leave the . in cop, as a flag to &&: that it will check and remove at setvalence
create__localecompose f. (uop;cop) (<0 1;1)} y
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
NB. We keep the original form
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop0 2) jd cop0 jd (defstring__vop0 3)
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. lastinblock do.   NB. If we are the last block, save description for the end (should not occur, unless stealth)
  res =. 0 2$a:
else.
  t =. 'This is the first part of the verb:',LF,(defstring 0),CR
  select. appearstwice, datapresent
  case. 0 0 do.
    t =. t , 'This block will contain a single intermediate result for the verb after sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
  case. 0 1 do.
    t =. t , 'This block contains an intermediate result for the verb.  The overall result is shown in the final block of the verb.',LF
  case. 1 0 do.
    t =. t , 'When a single result-cell is selected, this block will contain the result.',LF
  case. do.
    t =. t , 'This block contains the result.',LF
  end.
   res =. ,: EXEGESISRANKSTACKEXPLAIN;t
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring,', '
  tit =. tit , 'and contains the computation of the inverse.  Inverses cannot be probed and are displayed in a single block.'
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit
  end.
  res
else.
  0 2$a:
end.
)

NB. **** u"n u"v m"n m"v****
NB. We also come through here for u&n m&v, with the primitive changed to '&'; & for @n and :: n, with the primitive changed
localerank_dissect_ =: primlocale '"'

create =: 3 : 0
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB. Handle m"nv as a general verb, except when it's m"_ which we handle as a nilad
if. noun bwand (<0 0) {:: y do.
  if. *./ (, *@#) _ = ". ". 'op__vop' do.
    nilad =: 1  NB. Mark nilads as allowing display
  else.
    changeobjtypeto localedefault
    create y
    return.
  end.
else.
  nilad =: 0
end.
create_dissectobj_ f. (<1 2) {  y
NB. In case this is a nilad, create the title to use.  If the tokenlist of v is empty, we must be on a named "_
if. cop -.@-: ,'"' do.
  niladtitle =: '(',cop,'n)'  NB. cop is not '"' - must be @n or ::n
elseif. a: -: (<2 2) { y do.
  niladtitle =: ; (<:tokensource) ({ /: [) ;: usersentence__COCREATOR   NB. decr tokensource to account for MARK added
elseif. do.
  niladtitle =: '(m"_)'
end.

NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the type of v
vtype =: (<2 0) {:: y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
if. -. nilad do.
  uop =: setvalence__uop y
  NB.?lintonly uop =: <'dissectverb'
end.
resultissdt =: resultissdt__uop
if. IFQT do. nuvocpage =: 'quote' , nilad # 'm' end.
coname''
NB.?lintsaveglobals
)

NB. 1 if this verb may not have infinite rank, or if this node displays already
shownilad =: 3 : 0
y +. -. nilad    NB. assuming " will be followed by not _
)

NB. return string form of operands, not including instrumentation
NB. if nilad, empty cop means that the original noun was converted to noun"_; we display just the original
defstring =: 3 : 0@]
if. nilad *. cop -.@-: ,'"' do.
  enparen^:(y=3) (defstring__uop 2)
else.
  enparen^:(y=3) (defstring__uop 2) jd '"' jd (defstring__vop 3)
end.
)

NB. return string form of operands, including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
exestring =: 3 : 0
initloggingtable ''
if. nilad do.
  auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , '"',(exestring__vop ''),'))'
else.
  auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , '"' , '(' , (exestring__vop '') , '))'
end.
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (((y ~: 0) +. (-. nilad)) , (y=3) , y>0) # ;: 'uop tokensource vop'
)

NB. Nilad.  We know that the current node has no selection but has valid selopshapes.
NB. Result is 2 if it is OK to turn this into a rank-calculus probe, 0 if not.
rankcalculus =: 2:
NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. The result is the DOL, up through the result of u
NB. We do not create a cell; we just traverse u.  There is no visible indication of the rank operator, except in the
NB. frames
traverse =: endtraverse@:(4 : 0)
titlestring =: 0 fulltitlestring cop   NB. Since this may show up in the rank stack, have a title string
NB. for u"n, resolve n internally.  It will not display, but we need a result for getverbrank
if. vtype bwand noun do. NOLAYOUTS traverse__vop TRAVNOUN end.
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
if. nilad do.
  stealthopencountered__COCREATOR =: 1  NB. Note that we saw a nilad
  NB. Traverse the noun, but keep sellevel in order so highlights are calculated correctly
  NB. Remove the input if we should not display them
  0 inheritu (x #~ dispniladinputs +. displayshowstealth) traverse__uop bnsellevel 0} TRAVNOUN
  NB. This returned a noun result, which may have a character value for the name in displaylevrank.  So we inherit it
  NB. into m"_, which in inheritu converts it to a verb for subsequent inheritance.  This turns the noun into a niladic verb.
else.
  NB. Insert end-of-computation unless this node is hidden (as part of u&n or u&.v)
  NB. Don't inherit stealth: (]"0) should display
  (0 ,~ cop -: ,'"') inheritu x traverse__uop travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
end.
)

NB. overrides for calcselect

NB. get the rank to use for this verb.
NB. y is selopshapes
NB. Result is the rank to use for the verb's valence, or $0 if we don't know
getverbrank =: 3 : 0
rank =. $0
NB. We use the actual executed rank unless this verb has negative rank and there are operands.
NB. in that case we calculate the rank to use after referring to the actual operand rank
if. vtype bwand noun do.
  if. _1 e. * nrank =. 3 $&.|. fillmask__vop frameselresult__vop selresult__vop do.
    if. #y do.
      rank =. 0 >. (#@> y) (] + (* <&0)) (valence { 0 1 _2) {. nrank
    end.
  end.
end.
if. 0 = #rank do. rank =. getverbrank_dissectobj_ f. y end.
rank
)

exegesisrankstack =: 3 : 0
NB. exit fast with no result if this node is hidden
if. cop -.@-: ,'"' do. 0 2$a: return. end.
'appearstwice lastinblock datapresent' =. y
NB. This will have been described by its frame, but if there is a noframe message, replace
NB. it with something more tailored
if. '' -: frame do.
  res =. ,: EXEGESISFRAMENUGATORY;'This has no effect because the argument level',(valence{::'';' is';'s are'),' already small enough.',LF
else.
  if. lastinblock do.   NB. If we are the last block, save description for the end
    res =. 0 2$a:
  else.
    t =. 'The verb is:',LF,(defstring 0),CR
    select. appearstwice, datapresent
    case. 0 0 do.
      t =. t , 'This block will contain a single intermediate result for the verb after sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
    case. 0 1 do.
      t =. t , 'This block contains an intermediate result for the verb.  The overall result is shown in the final block of the verb.',LF
    case. 1 0 do.
      t =. t , 'When a single result-cell is selected, this block will contain the result.',LF
    case. do.
      t =. t , 'This block contains the result.',LF
    end.

    res =. ,: EXEGESISRANKSTACKEXPLAIN;t
  end.
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit
  end.
  res
else.
  0 2$a:
end.
)

NB. **** uL:n uS:n ****

primlocale 'L:S:'

create =: 3 : 0
if. 0 = verb bwand (<0 0) {:: y do.
  failmsg 'domain error: left operand to ',((<1 1){::y),' must be a verb'
end.
if. 0 = noun bwand (<2 0) {:: y do.
  failmsg 'domain error: right operand to ',((<1 1){::y),' must be a noun'
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
if. IFQT do. nuvocpage =: (tolower {. cop) , 'co' end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , cop , '(' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3) , y>0) # ;: 'uop tokensource vop'
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. The result is the DOL, up through the result of u
NB. We do not create a cell; we just traverse u.  There is no visible indication of the rank operator, except in the
NB. frames
traverse =: endtraverse@:(4 : 0)
NB. Resolve n internally.  It will not display, but we need a result for getverbrank
NOLAYOUTS traverse__vop TRAVNOUN
titlestring =: 0 fulltitlestring cop , ": ulevel =: ((#x) {:: '';(,0);1 2) { 3 $&.|. fillmask__vop frameselresult__vop selresult__vop
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
1 inheritu x traverse__uop travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
NB.?lintsaveglobals
)


NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
NB. Any needed side effects are taken care of here
NB. We create resultseqmap, the sequential map of the result, which is used for selection and highlighting
NB. The frame is the number of items in the map
calcdispframe =: 4 : 0
oplevels =: L.@> x
effulevels =: 0 >. (oplevels * ulevel < 0) + ulevel   NB. Actual level of operand
if. levelunused =: *./ effulevels >: oplevels do.
  NB. The operands start out at
  NB. executable level: bypass level processing, at treat this as a verb with no frame
  ($0) ; ($0) ; (valence # <$0) ; ($0) ; ($0)
else.
  if. 1 = valence do. map =. 0: L: ulevel 0 {:: x else. map =. 0: L: ulevel&>/ x end.
  'frame resultseqmap' =: seqmapfrommap map
  NB. The pseudoframe (# results) is in the stored data, so go ahead and make that the frame
  NB. arglevel here is the level at which each operand will be executed.  Individual frames are
  NB. empty so we don't expect results.
  NB. S: does not have a resultlevel
  (,frame) ; (,frame) ; (valence # <$0) ; ((cop -: 'L:') {:: '';0) ; effulevels
  NB.?lintsaveglobals
end.
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
NB. This will have been described by its frame, but if there is a noframe message, replace
NB. it with something more tailored
if. levelunused do.
  res =. ,: EXEGESISFRAMENUGATORY;'This has no effect because the argument level',(valence{::'';' is';'s are'),' already small enough.',LF
else.
  if. lastinblock do.   NB. If we are the last block, save description for the end
    res =. 0 2$a:
  else.
    t =. 'The verb:',LF,(defstring__uop 0),CR
    if. *./ 0 = effulevels do.
      if. 1 = valence do.
        t =. t , 'is applied to the contents of each innermost box.'
      else.
        t =. t , 'is applied between the contents of innermost boxes of the arguments.'
      end.
    else.
      if. 1 = valence do.
        t =. t , 'is applied to contents that have boxing level ',(":effulevels),'.'
      else.
        t =. t , 'is applied between contents of x with boxing level ',(":{.effulevels),' and contents of y with boxing level',(":{:effulevels),'. Use highlighting to see the correspondence.'
      end.
    end.

    if. cop -: 'S:' do.
      t =. t , '  The results from each application are assembled into a list of result-cells.'
    end.

    select. appearstwice , datapresent
    case. 0 0 do.
      t =. t , '  This block will contain a single intermediate result for the verb when sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
    case. 0 1 do.
      t =. t , '  This block contains an intermediate result for the verb.  The overall result is shown in the final block of the verb.',LF
    case. 1 0 do.
      t =. t , '  When a single result-cell is selected, this block will contain the result.',LF
    case. 1 1 do.
      t =. t , '  This block contains the result.',LF
    end.

    res =. ,: EXEGESISRANKSTACKEXPLAIN;t
  end.
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit
  end.
  res
else.
  0 2$a:
end.
)


NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
selindextoisf =: 4 : 0
if. -. selectable do. ''
elseif. levelunused +. cop -: 'S:' do. x selindextoisf_dissectobj_ f. y
elseif. do. < 1j1 #!.SFOPEN > resultseqmap pathfromindex y
end.
)

NB. y is the new selection (boxed, and possibly with an initialselection following)
NB. Result is signum of (sel - error spot): 1 if invalid, 0 if on the error, _1 if no error
auditselection =: 3 : 0
if. levelunused +. cop -: 'S:' do. auditselection_dissectobj_ f. y
else.
* ((SFOPEN -.~ ,>y) {:: resultseqmap) - #selresult  NB. kludge - , used because selection is a table - but should it be??
end.
)

NB. y is the selection, x is the list of result indexes.  We return the selected index.
NB. We convert the selection into a path (by removing dropdowns); then pull the sequence number
NB. from the sequential map to be the index
selectusingisf =: 3 : 0
if. levelunused +. cop -: 'S:' do. selectusingisf_dissectobj_ f. y
else. < (SFOPEN -.~ >y) {:: resultseqmap
end.
:
if. levelunused +. cop -: 'S:' do. x selectusingisf_dissectobj_ f. y
else. (selectusingisf y) { x
end.
)

NB. x is selopinfovalid,y is selopshapes.  The current level has no selection.
NB. Result is our prediction of what the shape of the selected operands 
calcunselectedshapes =: 4 : 0
if. levelunused do. x calcunselectedshapes_dissectobj_ f. y
elseif. x do.
  NB. Only one item in the frame; must be only one path through the operands
  arglevel 4 : '{. ]S:x y'&.> y
elseif. do.
  NB. If there are multiple possible selection, we have no idea what they might be
  a:"0 y
end.
)

NB. Adverb.  m is the path of the result.  x is the desired level, y is the tree to be searched
NB. Result is path to the desired level.
NB. If y is at the desired level, return empty; otherwise get the path to the next-lower
NB. level indicated by {. m (but use only leading axes that exist in y), select that box, open it, and recur
getpathbylevel =: 1 : 0
:
if. x >: L. y do. 0$a:
else.
  assert. 0 < $m
  pathtonext =. ((#$y) {.&.> {.m)
  pathtonext , x (}.m) getpathbylevel pathtonext {:: y
end.
)
NB. y is current selection
NB. Result is path to the result, which for L: is the path with SFOPEN removed, for S: is the path for that sequential cell
resultpathforselection =: 3 : 0
if. cop -: 'L:' do.
  NB. L: - the selection is the path, but 
  assert. SFOPEN e. > y
  SFOPEN -.~ >y
else.
  NB. S: - convert the selection to a result path
  > resultseqmap pathfromindex {.>y
end.
)
NB. y is the current selection in isf form (must always contain dropdown unless levelunused)
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
if. levelunused do. calcselectedshapes_dissectobj_ f. y
else.
  NB. we must stop with the ranks/levels existing for each operand
  arglevel (((resultpathforselection y) getpathbylevel) {:: ])&.> selopshapes
end.
)

NB.
NB. For the normal verb, the selector and the highlight are identical.
calcphysandhighlights =: 3 : 0
if. levelunused do. valence # (< 2 0$a:)
else.
  arglevel (2 1 $ [: ,@< 1j1 #!.SFOPEN ((resultpathforselection y) getpathbylevel))&.> selopshapes
end.
)

NB. **** m/ u/ ****
primlocale '/'

create =: 3 : 0
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
NB. All we need to pass in is the locale of u and the titlestring
'uop cop' =: 1 {"1 y
utype =: 0 0 {:: y
NB.?lintonly uop =: localeat [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a dyad
setvalence =: 3 : 0
valence =: #y
if. valence = 2 do.
  NB. m/ dyad is an error as a general verb
  if. noun bwand utype do.
    failmsg 'domain error: in x u/ y, u must be a verb'
  end.
else.
  NB. m/ monad should be a gerund.  If we don't recognize the gerund, handle it as generic
  if. noun bwand utype do.
    if. 0 = #gops =: querygerund__uop'' do.
      changeobjtypeto localedefault
      setvalence y return.
    end.
  else. gops =: ''
  end.
  uop =: 'dissectinsertexpansion' 1 createmodifier uop;cop;<gops
end.
NB.?lintonly uop =: localeat
uop =: setvalence__uop y
NB.?lintonly uop =: localeat
resultissdt =: resultissdt__uop
separatevalences''
if. IFQT do. nuvocpage =: 'slash' , (valence=2) # '#dyadic' end.
coname''
NB.?lintsaveglobals
)

NB. Return the locales for propsel.
proplocales =: 3 : 0
(<^:(0=L.)@".@>^:(0 <: y) (1 , y=3) # ;: 'uop tokensource')
)

NB. return string form of operands, not including instrumentation.
NB. The expansion has the full form of the verb, so use it
NB. This version is the one used if we need defstring before setvalence, as for example when we
NB. take this verb into an unknown compound
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. The monadic valence:
startmonad ''

NB. return string form of operands, not including instrumentation.
NB. The expansion has the full form of the verb, so use it
defstring =: 3 : 0@]
defstring__uop y
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , '))'
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Get # items in operand
if. (#inputselopshapes) *. (*#>selector) do.
  nitems =. {. $^:(0<L.) 0 {:: inputselopshapes
else.
  nitems =. 0
end.
NB. Selection is forced if there are 2 items and we allow forced selection
forcedsel =: displayautoexpand2 *. nitems = 2
NB. Expansion is called for if there is a forced selection OR if the user has clicked on our result, which we detect by
NB. seeing our initialselection in the selections
shouldexpand =: (nitems > 0) *. forcedsel +. sellevel < #selections
NB. Allow forced-select only if there is something to see
if. (-. forcedsel) *. 1 < nitems do.
  initialselection =: <(<,0),SFOPEN
  expansionstate =: EXPANDABLE
end.
NB. Run the expansion
resdol =. x traverse__uop forcedsel;shouldexpand;< travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
if. forcedsel do.
  NB. forced selection: leave the result from u/
elseif. shouldexpand do.
  NB. expansion node created, display it and display this result as a final
  resdol =. (joinlayoutsl resdol) ,&< coname''
  displaylevrank =: (<'Final ' , MAXFINALDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  physreqandhighlights__inheritroot =: NOPHYSREQ
  expansionstate =: EXPANDED
elseif. do.
  NB. No expansion node created, display this result as a simple result
  resdol =. x ,&< coname''
  displaylevrank =: (<MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
end.

resdol
NB.?lintsaveglobals
)

NB. u/ is a failure point when applied to 0 results (no neutral)
operationfailed =: 3 : 0
if. 0 = {. $^:(0<L.) 0 {:: inputselopshapes do.
  NB. We did try executing the verb (i. e. there is a selector) but there are no inputs and no outputs;
  NB. it must be a domain error (no neutral).  That is a failure point, since u is never executed
  changeerrormessagefrominterp 'no neutral'
  1
else.
  NB. If there are items, u will be executed, so defer the failure until then
  0
end.
)

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
selindextoisf =: 4 : 0
<(<,0),SFOPEN
)

NB. We treat this as an overall since it does only one thing
exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
if. datapresent do.
  t =. 'This block %al1%displays the final result of the verb:',LF,(defstring 0),CR
  if. tutor do.
    t =. t , 'This verb applies a verb repeatedly between items of y.',LF
  end.
  if. forcedsel do.
    t =. t , 'Because there are only two items, the verb is displayed as if it were a dyad between the two items',LF
  elseif. shouldexpand do.
    t =. t , 'The block feeding into this one is an expansion block showing all the intermediate results.  '
    if. tutor do.
      t =. t , 'There the results are lined up with the first computed result on the right.  You can click on any intermediate result to see how it was computed.  '
    end.
    t =. t , 'To remove the expansion block, click in the result of this block.',LF
  elseif. do.
    if. tutor do.
      t =. t , 'To see the computation of the intermediate results, click anywhere in this result.  '
      t =. t , 'That will cause a new block, called an expansion block, to be created feeding into this one.  '
      t =. t , 'The expansion block will show all the intermediate results, and you can look at them and see how they were computed.',LF
    else.
      t =. t , 'Click on the result to see an expansion block showing the intermediate results of the calculation.',LF
    end.
  end.
  res =. EXEGESISRANKOVERALLCOMPEND;t
else.
  res =. exegisisrankoverallnodisp 1;''
end.
res
)

NB. The dyadic valence:
startdyad ''

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. Simulate dyad u/ by creating a rank
auditstg '((' , (logstring '') , '@:(' , (verblogstring '') , (exestring__uop '') , ')"(_ (_1)} ' , (defstring__uop 2), ' b. 0))"_)'
)

NB. get the rank to use for this verb.
NB. We treat u/ like u"(lu,_)
getverbrank =: 3 : 0
_ (1}) getverbrank_dissectobj_ f. y
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 0 fulltitlestring cop   NB. This is the rank-stack version of /
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN;'x u/ y applies u between each cell of x and the entirety of y. The full verb is',LF,(defstring 0),CR
)

NB. ************ expansion node for monad / ***************

cocurrent 'dissectinsertexpansion'
coinsert 'dissectrighttoleft dissectirregularops dissectselectshape dissectdisplaytwo dissectobj'

NB. y is locale of u;string form of cop;locales of gerunds if any
create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
'uop cop gops' =: y
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
NB.?lintsaveglobals
coname''
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a dyad
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop 2$y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
if. IFQT do. nuvocpage =: (*#gops) {:: 'slash' ; 'CyclicGerund' end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) ;: 'uop'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. #gops do.
  NB. ( vlog (log@:u0)`(log@:u1)(log@:u2).../`(ubare/)@.(0=#)
  gopx =. }. ; (3 : '< ''`('' , (logstring$0) , ''@:('' , (exestring__y$0) , ''))'' '"0) gops
  auditstg '(' , (verblogstring '') , gopx , '/`(' , (defstring__uop 2), '/)@.(0=#))'
else.
  NB. Use unadorned verb on empty operand to get neutral.  Save every result of verb execution.  Overall result saved in u/
  NB. ( vlog (log@:u)/`(ubare/)@.(0=#)
  auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')/`(' , (defstring__uop 2), '/)@.(0=#))'
end.
)
NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
'forcedsel shouldexpand y' =. y
NB. Create display type:
titlestring =: 0 fulltitlestring cop
NB. Fix up a problem with disabling u/ on 2.  We leave this node showing a forced selection.  In that case, we
NB. rescind the forced selection, replacing it with the initialselection
if. -. forcedsel +. unforcedselection'' do. makeselection ,<(<,0),SFOPEN end.
traversedowncalcselect y
NB. Figure out which verb to traverse, if there is a cyclic gerund
if. 0 = #gops do. uuop =. uop
elseif. selectable *. sellevel < #selections do.
  NB. Normal case for gerunds: convert selection to partition #, then find the gerund that applied
  uuop =. gops {~ (#gops) | frame #. > {. sellevel {:: selections  NB. must be singleton that used the first gerund
elseif. do. uuop =. {. gops
end.
NB.?lintonly uuop =. <'dissectverb'

NB. The dyadic operands for u are two copies of the monadic
x =. 2 # x   NB. Create reference for dyad u
if. forcedsel do.
  NB. This is the code for the '(u on 2)' display.  It has been removed because users didn't like it.
  NB. It also had a bug: in dissect '+/ 8 9'  it led to error selecting in the result, because this locale's frame
  NB. (1) got included in the frame of the result, which should be an atom.  I think this is because we put this
  NB. node's chain in the display.
  NB. If we reinstate this block, it should be enabled by a preference switch, and we should simply
  NB. replace this block by the display of dyad u
  formatcode =: 0   NB. forced selection
NB. We have a selection of a u/ of exactly 2 items.  Convert to dyad
NB. Remove the line we added for u/ .  selopinfo has been adjusted
NB. Replace the rankhistory line (which contains the title of the entire u/) with an indication that / was elided
NB. Don't inherit u into u/, because u/ (the collector) has a frame, while u is a single result.  But DO extend u's
NB. locale chain to u/, so that the highlights calculated in u/ are displayed
  NB. u is always executed as a dyad.  If this node selected through its forced selection, physreq will have been expanded
  NB. to dyad shape.  But if not, we take the precaution here of forcing it to be a dyad so that its highlights can carry on.
  x traverse__uuop travops TRAVOPSKEEPALL;(TRAVOPSPHYSCHOOSE 0 _2);(vopval selopinfovalid);<selopshapes
  NB. Return the result of u, which will be passed through as the result of u/

elseif. shouldexpand do.
NB. We have a selector, and at least 2 possible selections.  Display the selector, and traverse u
  formatcode =: 1   NB. selector is displayed

  NB. Generate the y input: from the input y on the last selection; otherwise a loopback
  if. ({.frame) (| ~: <:@[) {. ('';0) {:: isfensureselection isftorank2 sellevel { selections do.
    NB. If this is not the last cell, which takes both inputs from y, change the last one to a loopback by pointing it to this node
    x =. (coname'') (<_1 1)} x
  end.

NB. Replace the last line of rankhistory with simple '/'
NB. Traverse u to display it and its descendants, and create a display node for pre-u.  Then u will be inherited into the display
NB. of the expansion created here, which is part of the result.
  NB. Append a marker indicating that this node completes the expansion calculation; don't copy dispstealth
  1 0 1 inheritu x traverse__uuop ((<cop)&((<_1 0)}))`'' travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
NB. The collector will display the result from the above
elseif. do.
NB. If there is no selector yet, the u/ node vanishes, with the display being provided entirely by the collector.
NB. In this case we (1) want no new display node here; (2) need to add an additional selection level, a negative
NB. value to be used for the initial selection if there is a click on the collector.  We do both functions by placing the numeric
NB. selection value in place of the locale name to display
NB. Remove the adornment of the titlestring in the ] node - if any
  formatcode =: 2  NB. waiting to engage selector
  ''
end.
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. formatcode = 0 do.
  NB. This is the special '/ on 2 items' box.
  ,: EXEGESISRANKSTACKEXPLAIN;'The monadic verb ',LF,(defstring 0),CR,'is applied to an array with 2 items. It is displayed as a dyad, with both x and y arguments coming from the input to the monad.',LF,LF,'This block is a starting point for the dyad.',LF
else.
  select. appearstwice,lastinblock
  case. 1 0 do.  NB. All computation in this block
     ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%shows the intermediate results of the verb:',LF,(defstring 0),CR
  case. 0 0 do.  NB. Computation ends in another block
     ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%starts the computation of an intermediate result of the verb:',LF,(defstring 0),CR
  case. do.
     0 2$a:  NB. leave it for the overall text
  end.
end.
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  if. appearstwice do.  NB. start and end in same stack
    t =. 'This expansion block %al1%calculates and displays all the intermediate results in the execution of the verb:',LF,(defstring 0),CR
  else.
    t =. 'This expansion block %al1%displays all the intermediate results in the execution of the verb:',LF,(defstring 0),CR,'and shows the last verb in the computation. The calculation of the result started in the block(s) marked with ',titlestring,' .',LF
  end.
  t =. t , 'The results are displayed as a list of boxes, where the contents of a box contains one intermediate result. '
  t =. t , 'The order of results matches the order of items of y, which is the reverse of the executed order. In other words, the first result in the list is the final result of the verb. '
  t =. t , 'Select any result to see how it was calculated. Selection of a result will open the selected box (indicated by the ''>'' in the selection line) and allow you to continue selections inside the box. '
  if. tutor do.
    t =. t , LF,LF,'To remove this expansion block, click in the result of the Final block that it feeds into.  '
  end.
  ,: (EXEGESISRANKOVERALLEXPLAIN,selectable+sellevel);t,LF,LF
else.
  0 2$a:
end.
)

NB. Nilad.  Result is the string to use as the lead for describing the result of the executed verb
exegesisverbdesc =: 3 : 0
EXEGESISDATASOURCE ; 'These are the intermediate results of the computation of the verb:',LF,(defstring 0),CR,LF,'The results are displayed as a list of boxes with each intermediate result in its own box.',LF
)

NB. *** traversal support ***
NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);(selopshapes for next level - cells of this verb);resultlevel;arglevel
calcdispframe =: 4 : 0
NB. frame is 1 less than the number of items - if there is more than 1 item.  empty otherwise
NB. Result boxing level will be 1 if the frame is longer than 1
((1 < nframe + -. displayautoexpand2) { ($0);1) _2} x calcdispframe_dissectobj_ f. , < (#~ >&0) nframe =. <: '' ($,) {.@($^:(0<L.))@> x
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only, 5=removal of forced selection
getselection =: 3 : 0
if. selectable *. (sellevel <: #selections) *. (displayautoexpand2 *. frame -: ,1) do.
  2 ,&< a:   NB. forced selection if 2 items
elseif. selectable *. (sellevel < #selections) do.
  1 ;<  sellevel { selections
elseif. do. 0 0
end.
)


NB. y is the current selection (a: if forced)
NB. The left operand selection, which always comes from the monad operand, is always the
NB. nonnegative selection index.  The right operand comes either from the monad (if we are selecting the
NB. last execution) or the self-reference otherwise, and either way has an index one higher.
NB. There is one selection but we
NB. create a selector with 2 rows, to match the fact that we have added a reference.
NB. We must also repair selopshapes and opselout to match the modification to selection
calcphysandhighlights =: 3 : 0
NB. For the left operand, selopshapes has the shape of an item of the selop and this is correct.
NB. For the right operand, if the last result is selected, its selopshapes is the same as
NB. the left operand, with the opselout one larger.  But for the other selections,
NB. selopshapes must be taken from the previous result.
NB. If this is the forced selection, apply the default
NB. The physical selection corresponding to the user's selection.  This is used only to create the highlights; the display in the expansion,
NB. and the installation of highlights into the expansion, are done independently (using the logical selection)
NB. As it turns out, the logical and physical are the same for u/.  We also have to
NB. modify selopshapes to account for the selection
rootsel =. '' ($,) frame | {. (('';0) {:: isfensureselection isftorank2 y) , _1
NB. For operands that are loopbacks (only the second operand, and only if we are not at the last selection)
NB. add on a drop-down so we highlight the operand, not the containing box
(0 , rootsel ~: <:{.frame) <@(2 1 $ <@(] , SFOPEN #~ [))"0 <@,"0 rootsel + 0 1
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. There is always a drop-down after the selection, so we don't bother checking
if. (<: {. frame) = rootsel =. '' ($,) frame | {. (('';0) {:: isftorank2 y) , _1  do.  NB. last selection: both from the monad input
  NB. Both selections come from the input.  Use the shape of an item if not boxed, or else take the actual map of the last 2 items
  (;~@:}.) ` ([: <"_1 (_2&{.)) @. (0<L.) > {. selopshapes
else.
  NB. One selection comes from the input, one from the result.  Get the shape/map of the selection, and append to the shape/map of the result
  (<  }. ` (rootsel&{) @. (0<L.) > {. selopshapes) , (selectiontoticket < >: rootsel) $L:0@{ selresult  NB. Get actual shape of right operand (selresult has not been double-boxed yet)
end.
)


NB. **** ^: ****
localepower_dissect_ =: primlocale '^:'

create =: 3 : 0
NB. Save the original v, whether gerund, noun, or verb
vvop =: (<2 1) {:: y
NB.?lintonly vvop =: <'dissectverb'
if. 0 = verb bwand (<0 0) {:: y do.
  failmsg 'domain error: left operand to ',((<1 1){::y),' must be a verb'
end.

create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the display form of c, the locale of v, and the gerund locales if any
NB. If the display form of the conjunction is not '^:', we are acting as a surrogate for another function like dyad u&m
'cop vop' =: (<1 2;1) { y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Save gerund locales if any
if. #gops =: querygerund__vop '' do.
  NB. If there are gerund locales, the next-to-last will be the one that survives for use as v
  vop =: _2 { gops  NB.?lintonly =: 3$a:
  visnoun =: 0   NB. Gerund is always u^:v
else.
  visnoun =: * noun bwand (<2 0) {:: y  NB. Remember whether it's u^:n or u^:v
end.
NB. We implement u^:v with two locales, because there are two levels of
NB. selection: first the selection from v, if that result is not an atom; then
NB. the expansion node and selection of powers.  We create a new
NB. node that looks like a modifier, i. e. u*^:v where * is the expansion.
NB. The display of the result of u^:v (as an inheritable u) comes from
NB. this locale; the expansion (as a v) comes from *.
NB. All we need to pass in is the locale of u and the titlestring (which may be different from '^:'
uop =: 'dissectpowerexpansion' 1 createmodifier ((<0 1;1) {:: y),<visnoun
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
if. visnoun do.
  resultissdt =: resultissdt__uop
else.
  vvop =: setvalence__vvop y
  NB.?lintonly vvop =: <'dissectverb'
  NB. In case the operand changed, refigure the gerunds
  if. #gops =: querygerund__vvop '' do.
  NB. If there are gerund locales, the next-to-last will be the one that survives for use as v
    vop =: _2 { gops  NB.?lintonly =: 3$a:
  end.
  NB.?lintonly vop =: <'dissectverb'
  resultissdt =: resultissdt__uop *. resultissdt__vvop
end.
if. IFQT do. nuvocpage =: 'hatco' , (-.visnoun) # '#monadicv' end.
NB. Return the dispoperands from v
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd (defstring__vvop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
initlogstring =.  '^:(1:`(',(logstring__uop 0),'@]))'  NB. string to insert to log input to u
if. #gops do.
  gopx =. (3 : '< exestring__y '''''"0) gops
  gopx =. (('(' , ')' ,~ ((logstring__uop 0),'@:') , ])&.> _1 { gopx) _1} gopx
  auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ^: (' , (}: ; (,&'`'&.>) gopx) , ')))'
else.
  auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ^: (' , (exestring__vvop '') , '))' ,initlogstring,')'
end.
)

NB. Return the locales for propsel.
proplocales =: 3 : 0
NB. For stopatxy (which propagates a normal selection), we mustn't propagate the initialselection to anything but the expansion (it wouldn't
NB. hurt to propagate it to v, but fatal to go to gerunds v0 or v2).  We know that we have already installed the new selection in this node,
NB. so we don't propagate to vvop if the number of selections here is greater than sellevel
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), ((y~:0)+.(sellevel >: #selections))) # ;: 'uop tokensource vvop'
)

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
NB. Create a y argument for the pre-u verbs: v and any gerund xy.  Replace the rankstack in y with a rankstack containing only the light lines
dummytraversedowncalcselect y  NB. Set the main global names only
xyy =. (< (#~ (<DLRCOMPEND) ~: 0&{"1) rankhistory) 1} y

NB. Handle gerund operands if any, and figure the drawing connections to use for u & v
NB. For gerunds, to get the error-detection right we have to use the same order used by the interpreter:
NB. v, y, [x]
if. visnoun do.
  vdol =. NOLAYOUTS traverse__vop TRAVNOUN
NB. If noun operand failed, pull the plug and display only that result
  if. errorcode__vop > EOK do. vdol return. end.  NB. If the noun failed, this node must have failed too
  NB. Noun v: no references required, just traverse u on x and y
  ux =. x
else.
  NB. verb/gerund v.
  if. 0 = #refs =. |. _2 |. gops do. refs =. uop,vop end.  NB. refs =. [xop] yop vop or uop vop
  NB. The v op is always last in our list, but executed first.
  vdol =. x traverse__vop xyy
  if. #gops do.
    NB. There are other gerund(s).  Realize them; their results become the input to u.  We treat the xy verbs as occurring
    NB. BEFORE the u^:v.  Any selection performed in u^:v applies to the results of the xy verbs, not the original xy.  So,
    NB. we run the xy verbs BEFORE calcselect here.  That means that travops cannot be used.  But we have to do the same kind of
    NB. work as in fork: the light rank-lines and phys selections must be passed from the input y to the xy verbs, and
    NB. the heavy rank-lines must be kept for u.  Moreover, if an xy verb is stealth, we need to pass rank lines that it
    NB. did not show on to u.
    NB. We end up by replacing y with a new y that shows the result of the xy verbs and the selection operations.

    NB. Execute the [x]y verbs and realize their display

    NB. traverse the xy ops and save their result as ux, to be passed into uop
    xylocs =. }: refs  NB. The locales [x] y
    NB. Traverse y before x just like Roger.  Order is v1 v2 v0
    ld =. (x ,&< xyy)&(4 : '((<selresultshape__y);dispstealthoperand__y);~(joinlayoutsl traverse__y&>/ x)')"0&.|. xylocs  NB. use ;~ to run traverse before inspecting results
    if. valence > #xylocs do.
      NB. If xop omitted for dyad, default it to [ by taking the x from the calculated input/ref, the shape from the first
      NB. shape input, and stealth of 2 (=[)
      ld =. ld ,~ (1 {. x) ; (, {. inputselopshapes) ; 2
    end.
    'ux srs stealth' =. <@;"1 |: ld
    NB. Create the y to use for u, with the heavy lines unless stealth caused an xy verb to be omitted.
    if. 0 = +/ stealthcode =. 3 bwand |. stealth do.  NB. Convert to y x order to match rankhistory
      NB. Not stealth: keep heavies
      rankstackcode =. TRAVOPSSTARTHEAVY
    elseif. valence = 1 do.
      NB. monad with stealth; just keep all the original lines
      rankstackcode =. TRAVOPSKEEPALL
    elseif. do.
      rankstackcode =. TRAVOPSKEEPALL
      NB. dyad with one or both stealth.  Create new rankhistory, where each column is chosen to be the
      NB. value selected by that stealthop, or empty if not stealth.  Delete lines that end up with no rank
      NB. This is copied from fork, except that we never generate highlights
      NB. In stealthcode 1 = ] = y, 2 = [ = x, 0 = neither
      rankhistory =: (#~    0 1 1 -.@-:"1 ('';(,0);(,0)) ="1  $&.>@:((0 2 3)&{"1)) (2 {."1 rankhistory) ,. stealthcode {"1 a: ,. 2 $!.a:"1 (2) }."1 rankhistory  NB. $!.a: needed because rankhistory may be empty (we haven't traversed)
    end.
    NB. If the input y had no operands, we leave it that way.  Everything else doesn't matter.  We might have added
    NB. a default operand here and we shouldn't try to traverse.  If we change y, recalculate the initial assignments
    if. #inputselopshapes do. dummytraversedowncalcselect y =. travops rankstackcode;TRAVOPSPHYSNEW;(uopval xylocs);< srs end.
  else. ux =. x  NB. no gerunds, apply original operands to u
  end.
end.
NB. Now ux has the traverse x inputs for u (the [x]y inputs to u)

NB. Create the layout for v
vlayo =. joinlayoutsl vdol

NB. keep track of how this node is formatted, for exegesis
formatcode =: 0   NB. early error
NB. We need vval for calculating the selframe of u; but it may not be valid, in case vop failed.
NB. We have fixed calcdispframe so that it doesn't look at vval if vop failed, so we just need to
NB. get vval defined when it is valid
if. errorcode__vop e. EHASVALIDFILLMASK do. vval =: fillmask__vop frameselresult__vop selresult__vop
else. vval =: 0   NB. Make sure it's defined
end.

NB. If n produced a gerund at execution time, in other words if it constructed an AR, we're beat, because we don't have
NB. the moxie to analyze that at execution time.
if. isgerund vval do. failmsg 'u^:n produced a gerund n without using ` - dissect doesn''t support that' end.

NB. Perform selections for u - needed for display whether v ran or not
traversedowncalcselect y
NB. If v invalid, detect domain error
if. errorcode__vop e. EFAILED do.
SM'fail'
  errorcode =: EINVALIDVERB
elseif. errorcode__vop -.@e. ENOOPS,ENOSEL do.
  if. (0 < L. vval) *. ((1 < L. vval) +. -. ('';,0) e.~ $&.> vval) do. errorcode =: EINVALIDVERB
  elseif. 2 ~: isinteger > vval do. errorcode =: EINVALIDMODOP
  end.
end.
if. errorcode e. EEARLYERROR do. earlyerror ux ;< vlayo ,&<"1 0 < (2 1 $ <0 0$0) , <0 return. end.

NB. In case we are formatting this node (the usual case), save the input DOLs to it
resdol =. ux ,&< coname''
NB. If v didn't run, there is really nothing we can do about u; just display it.  If v failed because it didn't select, there
NB. is hope for a later traversal
NB.?lintonly vval =: 0
if. errorcode__vop > EOK do.
  if. cop -.@-: '^:' do.   NB. m&v or u&n
    NB. ^: is executing as a dyad, but m&v is executing as a monad; so we have to cull the results and
    NB. highlights accordingly.  cop is ' ^:' for m&v, '^: ' for u&n, i. e. the space stands for the noun
    resdol =. (_1 {. ux) ,&< coname''
    physreqandhighlights__inheritroot =: _1 {. physreqandhighlights__inheritroot
  end.
  displaylevrank =: (<MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
elseif. do.
NB. v ran. Get the actual result of v.  We know v collected successfully

NB. Inspecting ALL the results from v, if every selection is either <0 or <_1, we will have no need for u^:
  if. logvalues__vop *./@:e. 0;_1 do.
NB. One-line request: create request, with appropriate labeling, depending on the possible values of v
NB. But if this node is not displaying as '^:', it must be coming from dyad u&m; format the overall verb and
NB. remove the x operand
    if. cop -: '^:' do.
      formatcode =: 2   NB. ^:0 or ^:_1
      labelstg =. (MAXFINALDEFSTRINGLENGTH defstring__uop 2) , (#. 0 _1 e. ; logvalues__vop) {:: '()';'_1';'0';'(0 or _1)'
    else.
      formatcode =: 3   NB. dyad &
      labelstg =. MAXFINALDEFSTRINGLENGTH defstring 0
      resdol =. (_1 {. ux) ,&< coname''
      physreqandhighlights__inheritroot =: _1 {. physreqandhighlights__inheritroot
    end.
    displaylevrank =: (<labelstg) (<_1 0)} rankhistory
  else.
NB. Otherwise, we will run u^:.  It will possibly create a display for u, or possibly an expansion node.
    
NB. Classify the v results and choose the type of display.  The result of this classification is passed to
NB. u^: (and also used here and in our tooltip)
    
NB. If the selector is invalid, and the totality of v has at least one positive value, traverse u to get a
NB. skeletal display, and make that the (v-type) result, with no expansion node.
    
NB. If the selector is invalid and v has no positive values, there is no need to traverse u.  Open twice in case of boxed v
    traverseu =. +./ , 0&(+./@:<)@(_:^:(0=#))@>@> logvalues__vop

NB. If the v value for the current selection does not require an expansion (<_1, <0, or <1), we will traverse to get
NB. a v-type display of u.  If the current selection is <0 or <_1, u^: will invalidate the selector to get a skeletal display of u.
    'skeletalu noexpansion' =. 2 3 > (<@,"0 (_1 0 1)) i. < ~. , vval
    
NB. If all the v results are <1, we never need a selector and can simply expand the result as a u-type
    vis1 =. logvalues__vop *./@:= <1
    
NB. Figure out what v value has been selected by the current selection.  If selection has not been performed,
NB. the expansion will not expand, so there must be a unique v value.  Pass that into the traversal: we will
NB. display only the powers whose sign matches the selection, and we will display only up to the selection.
    if. ($0) -: $vval do.
      if. 0 = selectedpower =. (- *)@({.!._)@>^:(1 = L.) vval do. traverseu =. 0 end.
    elseif. sellevel < #selections do.
      sel1 =. {. > isfensureselection isftorank2 sellevel { selections  NB. first level of selection, boxed
      if. 0 = selectedpower =. (- *)@>^:(1 = L.) sel1 { vval do. traverseu =. 0 end.
    elseif. do. selectedpower =. 0
    end.
NB. Create the initial selection to use when this result is clicked.  Since the initialselection is for an expansion node, append SFOPEN to it.
NB. we select according to which type (forward or inverse) will be displayed in the expansion.
NB. Create the initialselection only if we are ready to use it, i. e. if we have selected down to a single value to expand.
NB. We must not create an initialselection unless we are prepared to back it up with an expansion node - otherwise the
NB. initialselection will pass on to a later block, creating chaos
    if. (-. noexpansion) *. (*./selopinfovalid)  *. (selectable <: sellevel < #selections) do.
      initialselection =: <(, 0:^:(=&_) |selectedpower);SFOPEN  NB. Make a list to match what's produced during selection
      expansionstate =: EXPANDABLE
    end.

NB. Run the expansion node for u^: (as a v-type node); pass in the analysis of v.  The result is either
NB.  nothing: no expansion, no u.  display will be just u@:v
NB.  u: u expanded, but there is no expansion node.  If vis1, we treat this result as the u-type result; otherwise
NB.   we treat it as a v-type result and realize it it, then connect it to u^:v (but if the selector is 0, we
NB.   connect it to the y input instead
NB. expansion: realize it as a v-type result

    NB. The operands for the expansion depend on the type of power.  For non-gerund, u simply takes the x and y inputs in full.
    NB. For gerund, x and y have already been realized, including light tags, so we should keep only heavy tags.  But if x and y were stealth, we
    NB. want to pass the corresponding rankstack through.  This is all similar to fork

    'expdol code' =. ux traverse__uop traverseu;skeletalu;noexpansion;vis1;selectedpower;visnoun;< travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
    formatcode =: 4+code   NB. 4-7: expansions
    select. code
  NB. Return type: 0=vis1, 1=noexpansion, 2=skeletalu(unwired), 3=full u or expansion
      case. 0 do.
  NB. vis1: u^: produced a u-type result, which becomes our result
        resdol =. expdol
      case. 1 do.
  NB. If the expansion node did not expand, create the display result right here in this node (as a u-type).
  NB. If u doesn't use one of the operands, there will be a confusing extra wire connecting that operand to u
  NB. (if u were expanded, the wire would be removed).  We remove the wire here in that case.  It is most
  NB. important for u&m dyad, where we added the @] unbeknownst to the user
        if. cop -.@-: '^:' do.   NB. m&v or u&n
          NB. ^: is executing as a dyad, but m&v is executing as a monad; so we have to cull the results and
          NB. highlights accordingly.  cop is ' ^:' for m&v, '^: ' for u&n, i. e. the space stands for the noun
          resdol =. (_1 {. ux) ,&< coname''
          physreqandhighlights__inheritroot =: _1 {. physreqandhighlights__inheritroot
        end.
        displaylevrank =: (<(MAXFINALDEFSTRINGLENGTH defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
      case. 2 do.
  NB. skeletalu: u is there for show only; the actual value comes from y.  Create a reference to y and add it as the first input.
        resdol =. joinlayoutsl expdol
        resdol =. resdol , _1 { ux
  NB. Clear the wiring locale from u, indicating 'no wire'
        resdol =. a: (<_1 1)} resdol
        displaylevrank =: (<'Final ' , (MAXFINALDEFSTRINGLENGTH defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
  NB. Whenever we instantiate an expansion node, we alter the number of inputs to the u^:v node.  This invalidates the highlights from
  NB. u^:v.  But there shouldn't be any highlights anyway!  They come from the expansion.  So we just turn them off here
        physreqandhighlights__inheritroot =: NOPHYSREQ
        resdol =. resdol ,&< coname''
        expansionstate =: EXPANDED
      case. 3 do.
  NB. Expansion node executed with u attached, or just u by itself
        resdol =. joinlayoutsl expdol
  NB. expansion node or u where the current selector is <1, use it as a v-type
        displaylevrank =: (<'Final ' , (MAXFINALDEFSTRINGLENGTH defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
  NB. Whenever we instantiate an expansion node, we alter the number of inputs to the u^:v node.  This invalidates the highlights from
  NB. u^:v.  But there shouldn't be any highlights anyway!  They come from the expansion.  So we just turn them off here
        physreqandhighlights__inheritroot =: NOPHYSREQ
        resdol =. resdol ,&< coname''
        expansionstate =: EXPANDED
    end.
    
  end.
end.
NB. resdol will be our u-type result.
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand operand).  This box contains
NB. (nx2 dol table);highlight

NB. Calculate a highlight for the v operand - using its selection if any
NB. If not an array v, no selection
if. 0 = #selframe do.
  vselect =. <EMPTYPRH
else.
NB. Array v.  Take what we selected here, or empty if no selection here
  vselect =. {.!.(<0 0$0) sellevel }. selections
NB. Turn it into a highlight record
  vselect =. < (2 1 $ vselect) , <0
end.
(2 {. resdol) , (<vlayo ,&<"1 0 vselect)
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
if. datapresent do.
  t =. 'The final result of the verb:',LF,(defstring 0),CR
  if. tutor do.
    t =. 'This is the final result of applying a verb multiple times.  '
  end.
  if. #$vval do.
    astg =. LF,'The v operand of ^: produced ' , (exegesisindefinite exegesisfmtcell (0;0) ,~ ($vval);''),'.',LF
    astg =. astg , 'The verb is applied to the argument',((valence=2)#'s'),' for each atom of that array, and the result-cells are assembled into the final result.',LF
  else.
    astg =. ''
  end.

  if. #initialselection do.
    NB. Explanatory string if v produces an array
    select. formatcode
    case. 0;3;4 do.  NB. Early error; ;dyad m&v/u&n, does not go into rank stack; traverseu: vis1, this node not displayed
    case. 1 do.  NB. Gerund
      t =. t , LF , 'dissect doesn''t analyze the gerund form of ^:, sorry.',LF
    case. 2 do.  NB. ^:0 or ^:_1
      t =. t , LF,'Because ' , ((*#$vval){::'v';'each atom of v') , 'is ' , ((#. 0 _1 e. ,vval) {:: '';'_1';'0';'0 or _1') , ', details of calculating the result are not shown.',LF
      t =. t , astg
    case. 5 do.  NB. traverseu: noexpansion
      if. #$vval do. 
        astg =. astg , 'You have selected a result-cell.  Click the result-cell again to see details of its computation.  '
      else.
        astg =. astg , 'Click in the result to see details of its computation.  '
      end.
      if. tutor do.
        astg =. astg , 'This will create a new block, called an expansion block, feeding into this one.  There you can examine the calculation of the individual powers.'
      end.
      t =. t , astg , LF
    case. 6 do.  NB. traverseu: skeletalu (unselected)
      t =. t , astg , LF,'The selected power does not calculate u at all.',LF
    case.  do.   NB. traverseu, expansion or u created
      t =. t , astg , LF,'The expansion block feeding into this one shows the powers that were calculated. To remove the expansion block, click in the result of this block.',LF
    end.
    type =. EXEGESISRANKOVERALLCOMPEND
  else.
    t =. t , astg , LF,'Multiple powers were calculated.  Before you can explore the computation of this result, you must click on one result-cell.'
    type =. EXEGESISRANKOVERALLNOOPS
  end.
else.
  'type t' =. exegisisrankoverallnodisp 1;titlestring
end.
,: type;t
)

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb (always empties here, since rank is _)
NB. result is (selframe);(frame);(frame of value to display);(selopshapes for next level - cells of this verb);resultlevel;arglevel
NB. Called even when selector is empty, if rank-calculus probe
calcdispframe =: 4 : 0
NB. If this is a rank-calculus probe, vval is invalid and we can't look at it.  Its value doesn't matter anyway
if. a: -: selector do. sf =. '' else. sf =. $vval end.  NB. calc selframe
sf;($0);y;a:,a:
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
NB. Here we never return the actual selection, if any, because it's valid only for highlighting - there aren't really items
NB. of logvalues to back it up.  If the selection has been made, we return a forced selection (a:) which will cause the selector
NB. to be unchanged but mark it valid; if not, we return no selection
getselection =: 3 : 0
NB. If we need a selection and we have one, forced, otherwise pick-only
a: ,&<~ (selectable *. (sellevel < #selections)) { 0 3
)

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
NB. This turns ticket order to selection order
selindextoisf =: 4 : 0
if. selectable do.
  NB. Calculating the failing index for ^: is a chore.  We have to figure out what failed - the forward
  NB. or the inverse - and then get an index to whichever failed.  That will set up the selector to find the
  NB. failure in the expansion.  To decide what failed, we have to nose around in the expansion data
  NB. First, we have to find the matching indexes in the expansion
  selx =. ; findselection__uop > selector
  NB. Calculate the expected frame/inversect: the number of results expected (including the 0 'result') and
  NB. the number of negative results expected.  _ means 'don't know'.  If there are both positive.  This is needed
  NB. so we can detect an error during sniff
  fi =. (>./ , [: - <./) 0 , flatvval =. , (- *)@>^:(1 = L.) vval
  NB. Count the number of total and inverse executions, and convert to 'forward' and inverse, where forward includes the original value
  fix =. -/\. (# , _1&(+/@:=)) selx { logvaluesd__uop
  NB. Since we filtered out restarts, the forward/inverse counts are correct EXCEPT when there is a mix of negative and zero
  NB. vvals: then we should have NO forward execs, but we show one.  Remove its count
  fix =. fix <. _ ,~ >: 0 >. >./ vval
  NB. Decide which direction failed - if any.  Infinities can only come up when we are going in one direction
  edir =. fi i.&1@:> fix
  NB. If neither direction failed, the error must have happened during framing.  Ignore that for the nonce
  NB. Find the member of vval that is the largest in the direction of error.  That will be the one we select
  < x #: (i. >./) (edir { 1 _1 1) * flatvval
else. ''
end.
)

NB. y is the new selection (boxed, and possibly with an initialselection following)
NB. Result is _1 if selection is OK, 0 if point of error, 1=invalid
NB. There is no way for this selection to fail, since there is only one result, and if we have anything we have everything
auditselection =: _1:

NB. y is the current selection (a: if forced)
NB. We never generate a highlight.  This level deals only with selections.
calcphysandhighlights =: 3 : 0
valence # < 2&{. EMPTYPRH
)




NB. **** expansion node for ^: ****
cocurrent 'dissectpowerexpansion'
coinsert 'dissectdisplaytwo dissectselectshape dissectobj'

NB. y is locale of u;titlestring to display in rank stack;visnoun flag
create =: 3 : 0
create_dissectobj_ f. a:   NB. no string, no tokens
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the locale of u
'uop cop visnoun' =: y
NB.?lintonly uop =: <'dissectverb'
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
if. IFQT do. nuvocpage =: 'hatco' , (-.visnoun) # '#monadicv' end.
coname''
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB. Return the dispoperands from v
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) , cop
)

NB. return string form of operands, including instrumentation within u but not within inverse of u
exestring =: 3 : 0
initloggingtable 0
NB. If u has an unused operand, we should put @] or @[ after the inverse, because Roger can't handle x u@[^:_1 y but he can handle x u@[^:_1@[ y
monadstring =. invertiblymonadic__uop {:: '';'@]';'@['
NB. We log 5 types: 0 for the original input, 1 for the forward input , _1 for the inverse input, 2 for forward output, _2 for inverse output, and we sort them out in the local addlog
NB. Type 0 was logged back in the original ^: string
if. 1 = valence do.
  auditstg '(' , (verblogstring '') , '(' , (logstring 2) , '@:' , (exestring__uop '') ,'@:' , (logstring 1) , ') :. ( ' , (logstring _2), '@:(' , (logstring__uop'') , ')@:(' , (defstring__uop 2) , '^:_1',monadstring,'@:(',(logstring _1),')) ) )'
else.
  auditstg '(' , (verblogstring '') , '(' , (logstring 2) , '@:(' , (exestring__uop '') ,' ' , (logstring 1) , ')) :. ( ' , (logstring _2), '@:((' , (logstring__uop'') , ')@:(' , (defstring__uop 2) , '^:_1',monadstring,' (',(logstring _1),'))) ) )'
end.
)

NB. ********** addlog for ^: expansion ************
NB. We have to filter out repeats and restarts.  When a forward or reverse input is received, it is checked against
NB. the original input to see if it is a restart.  If so, we back over the first result.  We allow at most 1 restart per direction.
NB. After the restart-if-any, the input should match the previous output, which will be the last value saved.
addlog =: 4 : 0
NB. The first call must always be the original input.  We save it & initialize state
NB.?lintonly originput =: state =: 0
select. x
case. 0 do.   NB. initial y
  originput =: y
  state =: 0
  0 addlog_dissectobj_ f. y
case. 1 do.   NB. input to forward verb
  logok =. 1
  if. (y -: originput) do.
    select. state
    case. 0 do.
      NB. Initial forward exec.  Indicate that it has happened.
      state =: 1
    case. 1 do.
      NB. A second forward exec whose input it the same as the first.  We can't be sure, but this is probably a restart.
      NB. Don't log it.
      state =: 2   NB. we are now past the restart zone
      NB. Stop logging, for here and all descendants, until the restart is complete
      logok =. 0
    case. do.  NB. Must be state 2.  From here on all input must match the previous output.  They may all be the same, but
      NB. we log them all
      assert. y -: {: logvalues [ 'unexpected restart during ^:'
    end.
  else.
    assert. state ~: 0 [ 'unexpected first ^:'
    assert. y -: {: logvalues [ 'chain break during ^:'
    state =: 2   NB. restart should only come as first exec.
  end.
  loggingallowed__COCREATOR =: logok , loggingallowed__COCREATOR
case. _1 do.   NB. input to inverse
  logok =. 1
  if. y -: originput do.
    select. state
    case. 0;1;2 do.
      NB. Initial inverse exec.  Indicate that it happened
      state =: _1
    case. _1 do.
      NB. Repeat inverse.  Probably a restart - ignore it
      state =: _2
      NB. Stop logging, for here and all descendants, until the restart is complete
      logok =. 0    
    case. do.
      NB. State _2.  input must match previous output
      assert. y -: {: logvalues [ 'unexpected restart during ^: inverse'
    end.
  else.
    assert. state -.@e. 0 1 2 [ 'unexpected first ^: inverse'
    assert. y -: {: logvalues [ 'chain break during ^: inverse'
    state =: _2   NB. restart should only come as first exec.
  end.
  loggingallowed__COCREATOR =: logok , loggingallowed__COCREATOR
case. do.
  NB. Result of execution, either forward or inverse
  (*x) addlog_dissectobj_ f. y
  NB. Unstack the logging-allowed flag
  loggingallowed__COCREATOR =: }. loggingallowed__COCREATOR
end.
y
)

NB. Return the locales for propsel.
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) ;: 'uop'
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. where it is combined with the selector for this level to produce the selector for v and u.
NB. We call travdowncalcselect to get the selection for this level; then we traverse v (using the
NB. selector found here), and display v; then we traverse u using the selector found here.
NB. When we display v, its data will display only if it collects at this level
NB. We do not display u: we pass its display information back so that it can eventually
NB. be displayed if it ever reaches a collector.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
'traverseu skeletalu noexpansion vis1 selectedpower visnoun travy' =: y   NB. Unpack the added operands, info about v
assert. (6 0$0) -: $@".@> ;: 'traverseu skeletalu noexpansion vis1 selectedpower visnoun'
titlestring =: 0 fulltitlestring cop
traversedowncalcselect travy
if. errorcode e. EEARLYERROR do. (earlyerror x);0 return. end.

NB. If the selector is invalid, and the totality of v has a positive value, traverse u to get a
NB. skeletal display, and make that the (u-type) result, with no expansion node.

NB. If the selector is invalid and v has no positive values, there is no need to traverse u.  Return
NB. empty


NB. If the v value for thie current selection does not require an expansion (<_1, <0, or <1), we will make the display here or
NB. in the caller, with no expansion.  If this node is <1, traverse u and return that as the (u-type) display.
NB. If the selection is <0 or <_1, invalidate the selector so that we get a skeletal display of u

NB. Otherwise, expansion will be needed.  If no selection has been made, ask for one by returning a
NB. node with no locale.  Traverse u to get a skeletal display, which becomes the (v-type) result.

NB. If a selection has been made, create the display for the expansion; traverse u and inherit it into the expansion node.
NB. The expansion node becomes the (v-type) result

NB. Return type: 0=vis1, 1=noexpansion, 2=skeletalu(unwired), 3=displayable u
if. noexpansion do.
NB. no expansion node ever (all the v results are <_1 <0 or <1)
NB. Traverse u if called for; pass selection info unless disabled by skeletalu
  if. traverseu do.
NB. Some positive values, so we may see u sometimes.  Traverse it.  If skeletalu, disable detail
    udol =. x traverse__uop ((<'^:1')&((<_1 0)})^:vis1)`'' travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;((-.skeletalu) vopval selopinfovalid);<selopshapes
NB. If vis1, this result becomes the u result of u@:v
NB. Otherwise, suppress wiring u if skeletalu
    udol ; (#. vis1,skeletalu) { 3,2,0,0
  else.
NB. No positive values in v, so no way ever to run u
    0 1   NB. no u at all - say so
  end.
else.
NB. This node will be an expansion node, if it exists.  If no selection has been made, it doesn't.
  if. (0 < {.frame) *. (sellevel < #selections) do.
    sel1 =. {. > isfensureselection isftorank2 sellevel { selections  NB. first level of selection, boxed
    if. 1 < | > sel1 do.
      NB. If the selection is 2 or higher, or _2 or lower, we need a selfreference.  If 2 or higher, the selfreference
      NB. will go to u; if _2 or lower, it will go to u^: .  But we will need to make sure that y is still
      NB. displayed, just with the wire removed
      x =. (coname'') (<_1 1)} x
    end.
NB. If this v contains a nonpositive value, that means the y value might get through without going through u.
NB. In that case, we need a wire from y to the selector.  If also the selector is 1, both the expansion and u will
NB. need to connect to y, so we will need a reference.  We will connect the reference to u, because the connection
NB. to the expansion is secure, but the connect to u might be deleted by a stealthop.
    NB. If the user has selected power 0, don't show u.  This is because if u is complex there is no direct path from y
    NB. to the expansion, and it is puzzling to see the result coming as if from nowhere.  We would like to put x
    NB. out of the picture as well, but we're afraid it might contain references
    if. 0 = > sel1 do.
      traverseu =: 0  NB. wire straight to input
    end.
NB. Turning this into the arguments to the expansion and u:
    if. traverseu do.
NB. The arguments to u are:
NB.   x, if any
NB.   selfref (if 1 < |sel), or y
NB. Run u and inherit it into this node
      NB. If the selected power is negative, u will contain an inverse (or identity).  Since Roger always runs the inverse at
      NB. infinite rank, pass in a flag to u indicating that fact (the flag is that sellevel is a list).
      ures =. 0 1 1 inheritu x traverse__uop (, applyintree 0)^:(selectedpower<0) travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;((-.skeletalu) vopval selopinfovalid);<selopshapes
    else.
NB. No u.  Create the expansion, with input coming from y or self
      displaylevrank =: (<(MAXFINALDEFSTRINGLENGTH defstring 0) ,( visnoun {'un') , (selectedpower<0) # ' (inv)') (<_1 0)} rankhistory
      ures =. x ,&< coname''
    end.
    NB. Append the end-of-expansion marker to the display stack
    displaylevrank =: displaylevrank , DLRCOMPEND;coname''
    ures ; 3   NB. Display expansion
  else.  NB. no expansion.
    0 1
  end.
end.
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
select. appearstwice,lastinblock
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%shows the intermediate results of the verb:',LF,(defstring 0),CR
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%starts the computation of an intermediate result of the verb:',LF,(defstring 0),CR
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)
exegesisrankoverall =: 4 : 0
appearstwice =. x
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
  NB. Display overall explanation only on the end-of-computation marker
  if. appearstwice do.  NB. start and end in same block
    t =. 'This expansion block %al1%selects from the powers of',LF,(defstring 0),CR,'and displays the selected result. '
  else.
    t =. 'This expansion block %al1%selects from the powers of',LF,(defstring 0),CR,'%strt%and shows the last verb in the computation',tit,'.',LF,'%end%'
  end.
  NB. Get the selection that has been inited or clicked
  sel1 =. > {. > isfensureselection isftorank2 sellevel { selections
  if. sel1 = 0 do.
    t =. t , 'In this case, the power selected is 0, which is the original y unchanged: there is no computation to view.',LF
  else.
    t =. t , LF,'The boxes in the display shows the results of succeeding applications of the verb. The first box shows power 0 (the original y argument), '
    t =. t , 'the second shows power ',(":*selectedpower),', and so on.  '
    if. selectedpower < 0 do.
      t =. t , 'Negative powers call for application of the inverse, which cannot be probed internally.  '
    end.
    t =. t , 'Select any result to see how it was calculated.  Selection of a result will open the selected box (indicated by the ''>'' in the selection line) and allow you to continue selections inside the box. '
    t =. t , 'The currently selected power is ',(": sel1),'.  ',LF
  end.
  if. tutor do.
    t =. t , LF,'To remove this expansion block, click in the result of the Final block it feeds into.  ',LF
  end.
  ,: EXEGESISRANKOVERALLCOMPEND;t
else.
  0 2$a:
end.
)


NB. Nilad.  Result is the string to use as the lead for describing the result of the executed verb
exegesisverbdesc =: 3 : 0
EXEGESISDATASOURCE ; 'These are the intermediate results of the computation of the verb:',LF,(defstring 0),CR,LF,'The results are displayed as a list of boxes with each successive power in its own box.',LF
)


NB. **** pick support *****
NB. y is the selection, a list of boxes each containing an index list
NB. result is the selection to store in the node.  This will refer to the selected item but it might
NB. be negative to suggest negative indexing
NB. Get the next node in the inheritance chain.  This will be u, EXCEPT when the user has selected item 0,
NB. which doesn't actually execute u.
getnextpickloc =: 3 : 0
if. sellevel <: #selections do.
  if. 0 = (sellevel;0;0) {:: selections do. '' return.  NB. Item 0 selected, further selection not alowed
  end.
end.
inheritedfrom
)

NB. *** traversal support ***

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);(selopshapes for next level - cells of this verb)
NB. selopshapes calculated here are provisional, since the real shape depends on selector
NB. selector may be empty, if this is a rank-calculus probe
calcdispframe =: 4 : 0
NB. The frame is in general unpredictable, since there may be _ involved.
NB. So, we look at the actual results to see how many times the forward and inverse were executed.
NB. This can be tricky, since sometimes the interpreter starts with a throwaway execution of u (this
NB. seems to happen whenever the v result is an array containing a negative value - but NOT when v is a boxed negative number
NB. We can say that if the results contain an inverse, the first execution of u, if any, is throwaway.

NB. Find the indexes corresponding to the input selector, and extract the corresponding fwd/inverse types
NB. The number of valid values will become the frame.
NB. If we know from v what should be produced, use that.  Otherwise figure it out by looking at the result
if. noexpansion do.
  frm =. ''
elseif. selectedpower -.@e. _ __ do.
  frm =. , >: | selectedpower  NB. Add 1 to include 0
elseif. a: ~: selector do.
NB. Frame unknown, use whatever we actually did, of the correct sign
  selx =. calcdispselx ; findselection > selector
  frm =. , #selx
elseif. do.
NB. rank-calculus probe, return empty frame since we can't do rank-calculus
  frm =. $0
end.
NB. Level 1 (output only) if we have an expansion
((*#frm) { ($0);1) _2} x calcdispframe_dissectobj_ f. (-valence) {. <frm
)

NB. y is the indexes that matched; result is the indexes to use, in execution order.
NB. If the expansion node is to be omitted, the only use of this node is no provide
NB. an operand for u; that will be the unmodified y; so make that the only input
calcdispselx =: 3 : 0
NB. Restarts were filtered during addlog
keepmask =: (1 1 _1 {~ *selectedpower) = y { logvaluesd
keepmask =: 1 (0)} keepmask
1 {.^:noexpansion (>:|selectedpower) ((<. #) {. ]) keepmask # y
NB.?lintsaveglobals
)

NB. y is #selx; result is 1 if it indicates that cells were executed.  The difference between no execs and some is significant
NB. Here the first value comes from the input, & is not an execution
cellswereexecuted =: >&1


NB. x is limits of current selection interval
NB. y is the indexes in logticket that matched the selection
NB. Results is the intervals corresponding to each selection.  If we did a throwaway in calcdispselx we have to match it here
selectticketintervals =: 4 : 0
(keepmask,1) # x selectticketintervals_dissectobj_ f. y
)




NB. y is the current selection
NB. The right operand selection is one less than the selection (for forward) or one more (for backward)
NB. indicating a loopback
calcphysandhighlights =: 3 : 0
NB. Get the result shape, which depends on what is selected
ysource =. ,@(- *) ysel =. ('';0) {:: isfensureselection isftorank2 y
NB. Get the highlight to use for y, both its selection and its highlight string.  We append SFOPEN to the selection and highlight if it is a loobpack
if. 0 >: ysel do. skeletalu =: 1 end.
if. 0 = ysel do.
NB. The 0 'result' is specious, not really coming from an execution, so we have to suppress running
NB. u trying to refine it.  We keep the display of u, but with no values
  yphlight =. 2 1$a:
elseif. 0 = ysource do.
NB. If the source is the original y, highlight the entire y operand in its source position
  yphlight =. 2 1$a:  NB. this means 'highlight everything'.  0 0$0 = highlight nothing
elseif. do.
  yphlight =. 2 1 $ < ysource ; SFOPEN  
end.
NB. The left operand selection (if any) is always empty: the entire x
<@,."1@|: (-valence) {."1 yphlight
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. This selection always includes a drop-down
ysource =. ,@(- *)&.> ({. > isfensureselection isftorank2 y)
($L:0 ysource { selresult) _1} selopshapes  NB. Get actual shape of right operand (selresult has not been double-boxed yet)
)

NB. **** um`vn ****
primlocale '`'

create =: 3 : 0
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB. save the type/locale of u and v
'utype vtype' =: (<0 2;0) { y
NB.?lintonly uop =: vop =: coname'' [ cop =: '' [ conjex =: ''
resultissdt =: resultissdt__uop *. resultissdt__vop
valence =: 0  NB. If this gerund is recognized as a verb, we give it a valence
noun;(coname'');tokensource
NB.?lintsaveglobals
)

setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: coname''
uop =: setvalence__uop y
NB.?lintonly uop =: coname''
resultissdt =: resultissdt__uop *. resultissdt__vop
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. If this is a recognized gerund, don't bother with conjunction logging, and log out the results of individual verbs
if. 0 = valence do.
  auditstg '(' , (logstring '') , (defstring__uop 2) , ' ' , cop , (defstring__vop 3) , ')'
else.
  auditstg '(' , (exestring__uop 2) , ' ' , cop , (exestring__vop 3) , ')'
end.
)

proplocales =: 3 : 0
((y=3) +. (0~:valence)) # <^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: endtraverse@:(4 : 0)
assert. 0 = #x [ 'Noun must have no layouts'
traversedowncalcselect y  NB. To set globals, including selresult
'displaylevrank nounhasdetail' =: NORANKHISTNOUN;0
x ,&< coname''  NB. Return the empty DOLs
)

NB. Nilad.  The locale called must be a noun locale.  The result is the list of verb locales that make up
NB. the gerund in the locale.  If the locale is not a gerund, the result is empty.
querygerund =: 3 : 0
NB. If the operand locale is a verb use it; otherwise see if a noun is a gerund.  Join results together, keeping them only if all are verbs
(querygerund__uop^:(* utype bwand noun) uop) (*@<.&# # ,) (querygerund__vop^:(* vtype bwand noun) vop)
)


NB. **** u~ m~ ****
primlocale '~'

create =: 3 : 0
if. noun bwand (<0 0) {:: y do.
NB. Treat m~ using default modifier
  changeobjtypeto localedefault
  create y return.
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop 2$y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

NB. 1 if this verb may not have infinite rank, or if this node displays already
shownilad =: ]

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) , cop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ' , cop , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu (|. 2 $ x) traverse__uop travops (TRAVOPSKEEPINALL 0 1 _1 2);(TRAVOPSPHYSCHOOSE _2 0);(vopval selopinfovalid);< _1 0 {^:(*@#@]) selopshapes
)

exegesisrankstack =: 3 : 0
NB. We delete the frame lines
if. valence = 1 do.
  ,: EXEGESISRANKSTACKEXPLAIN;'The argument is replicated and used as both the x and y arguments',LF
else.
  ,: EXEGESISRANKSTACKEXPLAIN;'The x and y arguments are interchanged',LF
end.
)


NB. **** m@.vn ****

localeatdot =. 'dissectextendv' primlocale '@.'

create =: 3 : 0
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectobj'
NB. Switch to general verb if v is a noun, or if u is not a noun, or if u is not understood as a gerund
if. (* noun bwand (<2 0) {:: y) +. 0 = #ulocales =: querygerund__uop '' do.
  if. 0 = noun bwand (<0 0) {:: y do.
    failmsg 'domain error: verb@.v'
  end.
  changeobjtypeto localedefault
  create y
  return.
end.
NB. It is m@.v with m a gerund.  ulocales is the list of locales in the gerund
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop *. resultissdt__vop
if. IFQT do. nuvocpage =: 'atdot' end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' @. (' , (exestring__vop '') , ')))'
)

NB. Return the locales for propsel.
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 0 fulltitlestring cop  NB. make this show up on rank stack
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
vlayo =. joinlayoutsl x traverse__vop seloperands =. travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
NB. Create the layout for v

NB. If v didn't run, there is really nothing we can do about u; just display the final result.  If v failed because it didn't select, there
NB. is hope for a later traversal.  If there is no selection, we don't know which u to display, so just display then final result then too
NB.?lintonly vval =: 0
if. (errorcode__vop > EOK) +. -. *./ selopinfovalid do.
  vselect =. <EMPTYPRH   NB. Indic no v highlight
  rankhistory =: (<MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  expansionstate =: (#. selectable , sellevel < #selections) { UNEXPANDABLE, UNEXPANDABLE, EXPANDABLE, EXPANDED
elseif. do.
  NB. v ran, and there is only one choice for the selection.  We will be able to display u
  NB. Get the actual result of v.  We know v collected successfully
  NB. See which u-verb was selected.  This uses user data & so must be checked
  try.
    selectedop =. ulocales {~ fillmask__vop frameselresult__vop selresult__vop
  catch.
    changeerrormessagefrominterp 'invalid v value'
    EINVALIDOP earlyerror x ;< vlayo ,&<"1 0 < (2 1 $ <0 0$0) , <0 return.
  end.
  NB. The selection must be a scalar.
  if. ($0) -.@-: $selectedop do.
    changeerrormessagefrominterp 'non-atomic v'
    EINVALIDOP earlyerror x ;< vlayo ,&<"1 0 < (2 1 $ <0 0$0) , <0 return.
  end.
  NB.?lintonly selectedop =. <'dissectverb'
  NB. Insert an end-of-computation marker for the expansion
  seloperands =. ,&(DLRCOMPEND ; (coname'')) applyintree 1 seloperands
  NB. Traverse the selected operand and allocate a layout for it.  This result of u will become the input to the display
  NB. of this node, replacing the original x
  x =. joinlayoutsl x traverse__selectedop seloperands
  NB. Since we have replaced the inputs to this node, the highlighting may have the wrong valence.  But there should
  NB. be no highlights from this node anyway, so suppress them
  physreqandhighlights__inheritroot =: NOPHYSREQ
  vselect =. {.!.(<0 0$0) sellevel }. selections  NB. use the selection for v highlight
  NB. Turn it into a highlight record
  vselect =. < (2 1 $ vselect) , <0
  NB. Since we have used the rankhistory in the detail node, don't repeat it on the summary
  rankhistory =: (<'Final ' , MAXFINALDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  expansionstate =: EXPANDED
end.

NB. Label the display.  Note that x may have changed number of operands, but we have the right one here
displaylevrank =: rankhistory
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand operand).  This box contains
NB. a table of dol;highlights
x ; (coname '') ; < vlayo ,&<"1 0 vselect
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
select. appearstwice,lastinblock
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%shows the calculation of the selected gerund.',LF
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%starts the calculation of the selected gerund.',LF
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
'datapresent endflag linetext' =. y
tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
if. -. datapresent do.
  NB. No data.  Must not be the expansion
  res =. ,: exegisisrankoverallnodisp appearstwice;tit
elseif. linetext -: DLRCOMPEND do.
  NB. This is the node for u.  There is data
  'type t' =. exegisisrankoverallcompend appearstwice;tit
  t =. t , 'The gerund displayed here was selected by the Final block that this block feeds into.',LF
  res =. ,: type;t
elseif. -. endflag do.
  res =. 0 2$a:
elseif. linetext -: titlestring do.
  NB. Start of computation
  res =. ,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%starts the calculation of the selected gerund.',LF
elseif. do.
  NB. The final node, with data
  t =. 'This block %al1%displays the result of executing',LF,(defstring 0),CR,'The selection of executed verb comes in from the right.'
  if. *./ selopinfovalid do.
    if. (errorcode__vop <: EOK) do.
      t =. t,LF,'The calculation for the selected result is shown ending in the expansion block feeding into this one.',LF,'Computation starts in the block(s) labeled ',titlestring,' in the rank stack .'
    end.
  else.
    t =. t,LF,'Select a result-cell to see how it was calculated.'
    if. tutor do.
      t =. t , ' That will create a new block, called an expansion block, feeding into this one.  The expansion block will show the computation.'
    end.
  end.
  res =. ,: EXEGESISRANKOVERALLCOMPEND;t,LF
end.
res
)

NB. Called when we have passed through this block without performing a selection.  If this block is already selected,
NB. that means we have an expansion, and we should remove it.
NB. Also used by partitioning code
postselectionoverride =: 3 : 0
if. selectable *. sellevel < #selections do.
  makeselection 0$a:
  PICKTOOLTIPMSGOK
else. PICKTOOLTIPMSGNOORIDE
end.
)



NB. **** partitions ****
NB. A partition such as u/. is represented internally as u/.S where S is an adverb locale included simply
NB. for the purpose of selecting from the result, in case the arguments have a rank that cause the partition to
NB. be executed more than once.  In the executed sentence, u saves the result of each execution of u, and /.
NB. saves all the u-results for a given partition.  S holds the results for all the partitions.
NB.
NB. S inherits the display of /. into its result.  /. creates a display for u, and also acts as a verb in
NB. displaying its own result.  An x operand of /. is brought in to the left side of the display for /., and an n
NB. operand (which must be constant) is subsumed into the verb line, which is created in /. and shows the whole
NB. partition verb, prefixed by 'Final ' if there is a selection (which would cause u to expand).
NB.
NB. The action of S is simple and does not depend on any characteristic of /. except its rank, which it find by looking
NB. inside /. .  S merely selects, if selection is possible.  It passes its argument x to /.

cocurrent 'dissectpartitionselector'
coinsert 'dissectobj'

NB. y is the locale of the partitioning modifier
create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
uop =: y
newobj__COCREATOR coname''
NB. result is the locale of the created object
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

NB. Pass fit request down the line
applyfit =: 4 : 'x applyfit__uop y'

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
defstring__uop y
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. Nilad, called after cloning this locale
postclone =: 3 : 'xop__uop =: coname'''''  NB. Reach into partition to point back to selector

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a monad
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (verblogstring '') , (logstring '') , '@(' , (exestring__uop '') , '))'
)

traverse =: endtraverse@:(4 : 0)
NB. Check for invalid operands to ;., which would fail before execution of the verb.
NB.?lintonly cop__uop =: ';.' [ vop__uop =: <'dissectverb' [ gops__uop =: 0$a:
if. cop__uop -: ';.' do.
  partitionn =. >{.logvalues__vop__uop
  if. '' -.@-: $partitionn do. failmsg 'domain error: in u;.n, n must be an atom'
  elseif. partitionn -.@e. i: 3 do. failmsg 'domain error: in u;.n, n must be one of _3 _2 _1 0 1 2 3'
  elseif. (partitionn e. _3 3) *. (0 ~: #gops__uop) do. failmsg 'domain error: gerund u not supported for u;.3 and u;._3'
  end.
end.
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. If the partition is dyadic, it will need the VALUE of x.  We will extract that
NB. now.  We need the value of x after applying any selection given here.
if. 1 < #x do.
  NB. Dyadic valence
  xop =. (<0 1) { x  NB. locale of x operand
  NB.?lintonly xop =. <'dissectobj'
  if. errorcode__xop > EOK do.
    partitionx =: ''
  else.
    partitionx =: fillmask__xop frameselresult__xop selresult__xop
    NB. Apply the accumulated selectors to the x input.  These have been specified for the result but may not have been applied to x.
    NB. Apply, but stop when x has been selected as far as it can go, i. e. to the rank of the verb it applies to
    for_s. sellevel__xop }. ((sellevel + selectable) <. #selections) {. selections do.
     if. (#@$ partitionx) <: 0{vranks do. break. end.
     partitionx =: s { partitionx
    end.
NB. obsolete     NB. If there is a selection at this level, apply it to find the correct x value
NB. obsolete     if. selectable *. sellevel < #selections do.
NB. obsolete       partitionx =: (sellevel{selections) { partitionx
NB. obsolete     end.
  end.
  NB.?lintsaveglobals
end.
0 1 1 inheritu x traverse__uop travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
)


NB. ********** the partitioning modifiers themselves *****************

NB. These routines are shared by all partition processing
cocurrent 'dissectpartition'
coinsert 'dissectobj'  NB. for lint

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a monad
setvalence =: 3 : 0
valence =: #y
NB.?lintonly uop =: <'dissectverb'
uop =: setvalence__uop , *./y
NB.?lintonly cop =: '' [ uop =: <'dissectpartition'
NB.?lintonly exegesispartitiondesc =: ]
resultissdt =: resultissdt__uop
separatevalences''
if. IFQT do.
  NB.?lintonly gops =: ''
  if. #gops do.
    nuvocpage =: 'CyclicGerund'
  else.
    nuvocpage =: (('slash';'bslash';'semi';'') {::~ '/\;' i. {. cop) , (('.' e. cop) # 'dot') , (valence=2) # '#dyadic' end.
  end.
coname''
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Since u is always a monad, we will calculate the shape for y only
NB. For all partitions, we can use the highlights as an indication of which items are selected
NB. Extract the array of boxes, each contaiing an isf, as <sel[,dropdowns]
yhlight =. (_1;1 _1) {:: physreqandhighlights
NB. Get the selections, as an array whose 1-cells are selectors
yhlight =. ('';0) {::"1 0 yhlight
NB. If y is boxed, select its boxes; if not, replace its leading shape with the shape of the array of 1-cells.
NB. The 'leading shape' is given by the length of each individual highlight
selopshapes _1}~ < yhlight (}:@$@[ , (}.~ {:@$)~)`(<"1@[ { ])@.(1<L.@]) _1 {:: selopshapes
)


NB. adverb partitions \ \. /.
NB. Note: this locale is inherited by conjunction partitions to get exegesis & postselection
cocurrent 'dissectpartitionadverb'
coinsert 'dissectpartition'  NB. for lint

create =: 3 : 0
NB. We come through here for both adverb and conjunction partitions, keeping both operands
NB. in case we need to switch to default verb
if. noun bwand (<0 0) {:: y do.
  NB. Noun u: should be a gerund.  See if we understand it
  uop =: 0 1 {:: y
  NB.?lintonly uop =: <'dissectverb'
  if. 0 = #gops =: querygerund__uop '' do.
    NB. Unknown noun.  Treat as default verb
    changeobjtypeto localedefault
    create y
    return.
  end.
else. gops =: ''  NB. u is verb; there are no gerunds
end.
NB. gops has the gerunds if there are any
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
'uop cop' =: (<0 1;1) { y
fitstring =: ''   NB. Init to no fit specified
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
NB. Create a selector, which will be the first entry point for this sequence
verb;(xop =: 'dissectpartitionselector' 1 createmodifier coname'');tokensource
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop,fitstring
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. #gops do.
  NB. ( vlog (log u0)`(log u1)`... /. )
  gopx =. }. ; (3 : '< ''`('' , (logstring$0) , ''@:('' , (exestring__y$0) , ''))'' '"0) gops
  auditstg '(' , (verblogstring '') , gopx , cop , fitstring , ')'
else.
  NB. ( vlog log@:(u) /. )
  auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , cop , fitstring , ')'
end.
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
yop =: (<_1 1) { x  NB. locale of wired y operand, needed for u;.1
titlestring =: 0 fulltitlestring cop
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
if. (*./ selopinfovalid) *. (*#>selector) do.
  NB. Create a display for u (as a v-type).  It may or may not have detail, depending on whether anything was selected here
  NB. u is always a monad, so we pass in only the last argument
  NB. To assist tooltipping, we insert a heavy rank line for this locale at the end of the rankstack.
  NB. This will not show, but it will mark the node that performs the computation of the selected node (if there is one)
  rankhistory =: rankhistory , DLRCOMPEND ; (coname'')
  NB. Figure out which verb to traverse, if there is a cyclic gerund
  if. 0 = #gops do. uuop =. uop
  elseif. selectable *. sellevel < #selections do.
    NB. Normal case for gerunds: convert selection to partition #, then find the gerund that applied
    uuop =. gops {~ (#gops) | frame #. sellevel {:: selections  NB. must be singleton that used the first gerund
  elseif. do. uuop =. {. gops
  end.
  NB.?lintonly uuop =. <'dissectverb'
  udol =. joinlayoutsl (_1 {. x) traverse__uuop travops TRAVOPSKEEPALL;(TRAVOPSPHYSCHOOSE _2);(vopval selopinfovalid);selopshapes;_1
  NB. Now that we have displayed u, which has the incoming rank stack plus the line for this partition,
  NB. remove the COMPEND added here, but keep the rest to be displayed on the Final (the /. added above will be replaced below)
  rankhistory =: }: rankhistory
  x =. ({.udol) _1} x
  if. selectable *. sellevel < #selections do.
    NB. If this node is marked as Final, it repeats the rank stack.
    rankhistory =: (< 'Final ' , MAXFINALDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  else.
    NB. If this node is not final (which could happen only if there is
    NB. a single selection that we are showing for courtesy), remove the light lines
    rankhistory =: ((#~  (<DLRCOMPEND) = 0&{"1) rankhistory) ,  (<MAXFINALDEFSTRINGLENGTH defstring 0) 0} {: rankhistory
  end.
  expansionstate =: EXPANDED
else.
  NB. We don't display u, either because there is not a single result-cell selected from the selection node, or the user
  NB. has not clicked on a result-cell here
  NB. If the next click will select a result here, indicate the node is expandable
  if. sellevel = #selections do. expansionstate =: EXPANDABLE end.
  NB. If we do not display u, we have to keep the rank stack to display here, because that's the only chance we'll get
  rankhistory =: (< MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
end.
NB. Create a display for this node, as if it were a u-type verb.  This display will be inherited into the selector.
NB. We initialize the rank stack, and it is that that will give the label for this display.
NB. The /. node always displays the entire partitioning verb, with 'Final' prepended when the /. is selectable
NB. and there has been a selection.
NB. We remove the /. start-of-computation line - it is used only when the calculation is detailed
displaylevrank =: rankhistory
NB. The highlights for x (if any) are preserved, but the ones for y are reset, since there is no selection from u/. into u
physreqandhighlights =: (<EMPTYPRH) _1} physreqandhighlights
NB. The result of u becomes the last argument to the display of this node, along with the original x operand if
NB. there was one.
x ,&< coname''
)


exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
select. appearstwice,lastinblock
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKPARTITIONSTART;'This block %al1%calculates the selected partition of the verb:',LF,(defstring 0),CR
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKPARTITIONSTART;'This block %al1%starts the calculation of the selected partition of the verb:',LF,(defstring 0),CR
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
tutor =. tooltipdetailx = (1 {"1 TOOLTIPDETAILCHOICES) i. <'tutorial'
'datapresent endflag linetext' =. y
NB. This block is pointed to 3 times: u/. in the selector, 0 in the expansion, and /. at the start of computation
if. DLRCOMPEND -: linetext do.
  NB. This block has the expansion result, and possibly the whole expansion.  There will always be data, unless there is an error
  tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
  if. appearstwice do.
    t =. 'This block %al1%calculates the selected partition of',LF,(defstring 0),CR,'This partition ',(exegesispartitiondesc''),'.',LF
  else.
    t =. 'This block %al1%shows the result of the selected partition of',LF,(defstring 0),CR,'including the last verb of the computation',tit,'.  This partition ',(exegesispartitiondesc''),'.',LF
  end.
  res =. ,: EXEGESISRANKOVERALLCOMPEND;t
elseif. endflag do.
  NB. u/. 
  if. datapresent do.
    t =. LF,'This block %al1%shows the result of the verb:',LF,(defstring 0),CR,'which ',(exegesispartitiondesc''),'.',LF
    if. #selframe do.
      t =. t , LF,'The results of execution on the partitions are assembled into ',(exegesisindefinite exegesisfmtcell (1;0) ,~ selframe;''),' of result-cells.',LF
    end.
    if. (0 ~: #inputselopshapes) *. (0 ~: #selector) do.
      if. selectable *. sellevel < #selections do.
        t =. t , LF ,'The computation of the selected partition starts in the block(s) labeled ',cop,' and ends in the expansion block feeding into this one.',LF
      else.
        t =. t , LF,'To see the calculation of a single partition, select its result.'
        if. tutor do.
          t =. t , ' This will create a block, called an expansion block, feeding into this one.  The expansion block will show the calculation.'
        end.
        t =. t , LF
      end.
    end.
    res =. ,: (EXEGESISRANKOVERALLEXPLAIN,selectable+sellevel);t
  else.
    res =. ,: exegisisrankoverallnodisp appearstwice;''
  end.
elseif. do.
  NB. must be /. to start a computation.  That displays only as rankstack, never as overall
  res =. 0 2$a:
end.
res
)

NB. When a single result of /. is selected, it creates an expansion.  There can be
NB. no further selection in this block.  If the same result is reselected, we remove the expansion
NB. Called when we have passed through this block without performing a selection.  If this block is already selected,
NB. that means we have an expansion, and we should remove it
postselectionoverride =: postselectionoverride__localeatdot f.

NB. conjunction partitions ;.
cocurrent 'dissectpartitionconjunction'
coinsert 'dissectpartitionadverb'  NB. to pick up exegesis & postselectoverride

create =: 3 : 0
NB. handle the operands specific to the conjunction, and then transfer to the adverb code
if. 0 = noun bwand (<2 0) {:: y do.
  failmsg 'domain error: right operand to ',((<1 1){::y),' must be a noun'
end.
vop =: 2 1 {:: y
r =. create_dissectpartitionadverb_ f. y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: '' [ gops =: 0$a:
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
r
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd cop jd defstring__vop 3
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3) , 1) # ;: 'uop tokensource vop'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. #gops do.
  NB. ( vlog (log u0)`(log u1)`... ;. (v) )
  gopx =. }. ; (3 : '< ''`('' , (logstring$0) , ''@:('' , (exestring__y$0) , ''))'' '"0) gops
  auditstg '(' , (verblogstring '') , gopx , cop , '(' , (exestring__vop '') , '))'
else.
  NB. ( vlog log@:(u) ;. (v) )
  auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , cop , '(' , (exestring__vop '') , '))'
end.
)

NB. For traversal, we extract the string value of n and append that to the conjunction name;
NB. then we complete the traversal using the adverb form
traverse =: endtraverse@:(4 : 0)
NB. Traverse n.  It's a noun, and so must produce a single result
NOLAYOUTS traverse__vop TRAVNOUN
titlestring =: 0 fulltitlestring cop,":partitionn =: >{.logvalues__vop
x traverse_dissectpartitionadverb_ f. y
)

NB. ************** The individual partitioning modifiers *****************
cocurrent 'dissectobj'

NB. *** \ ***

'dissectpartitionadverb dissectpartition' primlocale '\'

NB. The monadic valence u\ y:
localebslashmonad =. startmonad ''

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
itemctiny =: '' ($!.1,) ($^:(0<L.))@> {: x  NB. needed for \.
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame
ny =. 1 ($!.1,) ($^:(0<L.))@> {: x
ny ; ny ; (,<ny) ; a: , a:
)

NB. y is the current selection (a: if forced)
NB. For the normal verb, the selector and the highlight are identical.
calcphysandhighlights =: 3 : 0
NB. There is only one argument, and we create selections for from selection to end
, < 2 1&$ < <@,@<@< i. >: > y
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Keep the rank of selopshapes (but if atom, change to singleton list), but use the size from the selection
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. selopshapes do.
  (>: {. > y) (, }.)&.> selopshapes
else.
  (>: {. > y) {.&.> selopshapes
end.
)
calcunselectedshapes =: calcunselectedshapes_dissectirregularops_ f.

exegesispartitiondesc =: 3 : 0
'operates on prefixes of y of increasing length' 
)

localebslashdyad =. startdyad ''
NB. The dyad x u\ y:

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
NB.?lintonly xop =: <'dissectpartitionselector'
itemctiny =: '' ($!.1,) ($^:(0<L.))@> {: x
NB. The number of partitions is:
NB. if x is nonnegative, (the number of items of y + 1) - x, but never negative
NB. if x is negative, (the number of items of y) % |x, rounded up
if. (3!:0 partitionx__xop) -.@e. 1 4 8 16 do. ny =. FRAMETOCREATEABORT
elseif. (-.@-: <.) partitionx__xop do. ny =. FRAMETOCREATEABORT
elseif. partitionx__xop >: 0 do. ny =. , 0 >. (>: itemctiny) - partitionx__xop
elseif. do. ny =. , >. itemctiny % -partitionx__xop
end.
ny ; ny ; (($0);ny) ; a: , a:
NB.?lintsaveglobals
)

NB. y is the current selection (a: if forced)
calcphysandhighlights =: 3 : 0
NB. The starting point is given by the current selection, if x is nonnegative; or selection * length if x is negative.
NB. The length is |x, but we have to limit the range to the length of the input
NB. There is never a highlight from x; use an empty
(< 2 0$a:) , < 2 1&$ < <@,@<@< itemctiny (> # ]) (({.>y) [`(*-)@.(0>]) partitionx__xop) + i. |partitionx__xop
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Keep the rank of selopshapes, but use the partition size: x if positive (even if > itemct)
NB. If x is negative, |x unless that would run off the end
if. partitionx__xop >: 0 do. ps =. partitionx__xop
else. ps =. (|partitionx__xop) <. itemctiny - ({.>y) * -partitionx__xop
end.
NB. The shape of x is immaterial since u is always invoked as a monad
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. {: selopshapes do.
  if. JEversionatleast 8 4 15 do.
  NB. Result is a list of (ps) items of y
  a: , < ps , }. _1 {:: selopshapes
  else.   NB. obsolete, eventually
    if. 0 = #yshape =. _1 {:: selopshapes do.
     NB. If x is > 1 for atomic y, the fill-cell is an atom instead of a 1-item list
     a: , < 1 #~ partitionx__xop <: 1
    else.
     a: , < ps 0} yshape
    end.
  end.
else.
  NB. Boxed operand; must get the actual shape of the selected part
  if. (({.>y) + ps) >: # yshape =. _1 {:: selopshapes do.
    NB. If there is not enough real data for an item, create a cell of fills
    if. JEversionatleast 8 4 15 do.
    NB. Result is a list of (ps) items of y, filled with empties (shape of contents = ,0)
    a: , < (ps , }. yshape) $ <,0
    else.   NB. obsolete, eventually
     if. 0 = #$yshape do.
      NB. If x is > 1 for atomic y, the fill-cell is an atom instead of a 1-item list
      a: , < (1 #~ partitionx__xop <: 1) $ <,0
     else.
      a: , < (ps 0} $yshape) $ <,0
     end.
    end.
  else.
    NB. Normal case, select from the actual operand
    a: , < (({.>y) + i. ps)&{ yshape
  end.
end.

)
calcunselectedshapes =: 4 : 0
NB. For negative x, shape is unpredictable; otherwise it's like a selection from 0
if. partitionx__xop < 0 do. x calcunselectedshapes_dissectirregularops_ f. y
else. calcselectedshapes 0
end.
)


exegesispartitiondesc =: 3 : 0
if. #inputselopshapes do.
  if. partitionx__xop < 0 do.
    t =. 'operates on nonoverlapping infixes of length ',(":|partitionx__xop)
    if. shardlen =. (|partitionx__xop) | {. $^:(0<L.) 1 {:: inputselopshapes do.
      t =. t , ' and a final shard of length ',(":shardlen)
    end.
    t
  else.
    'operates on overlapping infixes of length ',(":partitionx__xop)
  end.
else.
  'operates on infixes of y of a given length' 
end.
)


NB. *** \. ***

'dissectpartitionadverb dissectpartition' primlocale '\.'


NB. The monadic valence u\. y:
startmonad >localebslashmonad


NB. calcdispframe comes from \

NB. y is the current selection (a: if forced)
NB. For the normal verb, the selector and the highlight are identical.
calcphysandhighlights =: 3 : 0
NB.?lintonly itemctiny =: 0
NB. There is only one argument, and we create selections for from selection to end
, < 2 1&$ < <@,@<@< (> y) }. i. itemctiny
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Keep the rank of selopshapes, but use the size from the selection
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. selopshapes do.
  (itemctiny - {. > y) (, }.)&.> selopshapes
else.
  ({. > y) }.&.> selopshapes
end.
)

exegesispartitiondesc =: 3 : 0
'operates on suffixes of y of decreasing length' 
)

startdyad >localebslashdyad
NB. The dyad x u\. y:

NB. calcdispframe comes from \

NB. y is the current selection (a: if forced)
calcphysandhighlights =: 3 : 0
NB.?lintonly itemctiny =: 0
NB.?lintonly xop =: <'dissectpartitionselector'
NB. The starting point is given by the current selection, if x is nonnegative; or selection * length if x is negative.
NB. The length is |x, but we have to limit the range to the length of the input
NB. There is never a highlight from x; use an empty
(< 2 0$a:) , < 2 1&$ < <@,@<@< (i. itemctiny) -. itemctiny (> # ]) (({.>y) [`(*-)@.(0>]) partitionx__xop) + i. |partitionx__xop
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Keep the rank of selopshapes, but use the partition size: x if positive (even if > itemct)
NB. If x is negative, |x unless that would run off the end
if. partitionx__xop >: 0 do. ps =. itemctiny <. partitionx__xop
else. ps =. (|partitionx__xop) <. itemctiny - ({.>y) * -partitionx__xop
end.
NB. The shape of x is immaterial since u is always invoked as a monad
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. {: selopshapes do.
  a: , (itemctiny - ps) (, }.)&.> {: selopshapes
else.
  a: , (<<<({.>y) + i. ps)&{&.> {: selopshapes
end.
)
calcunselectedshapes =: 4 : 0
NB. For negative x, shape is unpredictable; otherwise it's like a selection from 0
if. partitionx__xop < 0 do. x calcunselectedshapes_dissectirregularops_ f. y
else. calcselectedshapes 0
end.
)

exegesispartitiondesc =: 3 : 0
if. #inputselopshapes do.
  if. partitionx__xop < 0 do.
    t =. 'operates on nonoverlapping outfixes of length ',(":|partitionx__xop)
    if. shardlen =. (|partitionx__xop) | {. $^:(0<L.) 1 {:: inputselopshapes do.
      t =. t , ' and a final shard of length ',(":shardlen)
    end.
    t
  else.
    'operates on overlapping outfixes of length ',(":partitionx__xop)
  end.
else.
  'operates on outfixes of y of a given length' 
end.
)

NB. *** /. ***

'dissectfitok dissectirregularops dissectpartitionadverb dissectpartition' primlocale '/.'


NB. The monadic valence u/. y:
startmonad ''


NB. *** traversal support ***
NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
yitemshape =: (($,)~   2 <. #) ($^:(0<L.))@> {: x
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.
NB. We extend the itemshape with 1s to 2 atoms; then the frame is the sum - 1; unless the shape contains
NB. 0: then the frame is 0
ny =. , (0&(-.@e.) * [: <: +/) _2 {.!.1 yitemshape
ny ; ny ; (,<ny) ; a: , a:
NB.?lintsaveglobals
)

NB. y is the current selection (a: if forced)
NB. For the normal verb, the selector and the highlight are identical.
calcphysandhighlights =: 3 : 0
NB. the selections are the (ysize) diagonal elements starting at (0,sel) and going down and to the left.
NB. But we must discard leading items that are not in the object: that is ((sel+1)-xsize), if positive;
NB. and we must discard trailing items not in the object: (ysize-(sel+1)) if positive
'ysize xsize' =. _2 {.!.1 yitemshape
diagonaleles =: (-#yitemshape) {."1 (0,sel =. > y) +"1 (i. ysize) */ 1 _1
diagonaleles =: ,. <"1 (- 0 >. ysize - >:sel) }. (0 >. sel - <:xsize) }. diagonaleles
, < 2 1&$ < < ,"1 diagonaleles
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. We have calculated the selected elements in diagonaleles; use that to get the shape
if. 1 = L. selopshapes do.
  (#diagonaleles) (, 2&}.)&.> selopshapes
else.
  NB. Each diagonal element is a row by itself, so we have to flatten them to select just the selected elements as an array
  (,diagonaleles)&{&.> selopshapes
end.
)

exegesispartitiondesc =: 3 : 0
'operates on diagonals of y' 
)

startdyad ''
NB. The dyad x u/. y:

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
NB.?lintonly xop =: <'dissectpartitionselector'
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.  The number of partitions is
NB. the number of unique elements of x
NB. Detect agreement error if the number of items of x doesn't match that of y
if. ~:&({.!.1@($^:(0<L.)))&>/ x do. a: return. end.
if. #fitstring do.
  ny =. , # ~.!.(". 2 }. fitstring) partitionx__xop
else.
  ny =. , # ~. partitionx__xop
end.
ny ; ny ; (($0);ny) ; a: , a:
NB.?lintsaveglobals
)

NB. y is the current selection (a: if forced)
calcphysandhighlights =: 3 : 0
NB. Find the indexes that match the selected item of the nub of x
NB. We highlight the items of x and y
2 # < 2 1&$ < <@,@<@< selitems =: ({.>y) (({ ~.) I.@:= ]) i.~ partitionx__xop
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. The shape of x is immaterial since u is always invoked as a monad
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. {: selopshapes do.
  a: , (#selitems) (, }.)&.> {: selopshapes  NB. Works on atoms too
else.
  a: , selitems&{&.> {: selopshapes
end.
)

exegesispartitiondesc =: 3 : 0
((*@# - 2 * '!.0'&-:) fitstring) {:: 'operates on partitions of y corresponding to tolerantly identical items of x';('operates on partitions of y corresponding to items of x that are identical within the given tolerance ',2}.fitstring);'operates on partitions of y corresponding to exactly identical items of x'
)



NB. *** ;. ***

dissectlocalesemidot_dissect_ =: 'dissectirregularops dissectpartitionconjunction dissectpartition' primlocale ';.'

NB. We handle monads by creating a synthetic x and then handling as dyads.  So both valences
NB. come through here


NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
NB.?lintonly xop =: <'dissectpartitionselector'
NB.?lintonly partitionn =: 0
shapeofy =: ($^:(0<L.))@> {: x
NB.?lintonly canonx =: usedyshape =: $0
select. partitionn
case. 0 do.
  if. (3!:0 partitionx__xop) -.@e. 1 4 8 16 do. ny =. FRAMETOCREATEABORT
  elseif. (-.@-: <.) partitionx__xop do. ny =. FRAMETOCREATEABORT
  elseif. do.
    canonx =: 0&,:^:(2>#@$) partitionx__xop
    usedyshape =: ({:@$ canonx) {. shapeofy
    ny =. $0
  end.
case. 1;_1;2;_2 do.
  NB. default in case of error
  NB. canonize x: convert to list of boxes; then within each box, convert atom to full list, or empty to list of one partition
  boxp =. <^:(0=L.) partitionx__xop
  NB. empty boxes do not participate in the partitioning: we take them in full.  We need to remember their
  NB. position for highlighting
  partitionedaxes =: ,boxp ~: a:
  canonx =: shapeofy ((<.&# {. [) ((#^:(''-:$@])) [^:(0=#@])~ ({. 1:))&.> (<.&# {. ])) boxp
  usedyshape =: ({:@$ canonx) {. shapeofy
  if. 1 < #$canonx do.
    NB. x has invalid rank
    ny =. FRAMETOCREATEABORT  NB. Error if too many partitions
  elseif. shapeofy <&# boxp do.
    NB. More partitions than rank of y
    ny =. FRAMETOCREATEABORT  NB. Error if too many partitions
  elseif. 1 +./@:~: #@$@> canonx do.
    NB. Invalid rank in x
    ny =. FRAMETOCREATEABORT
  elseif. -. (;canonx) *./@:e. 0 1 do.
    NB. If there is an invalid value in the operand, create an invalid frame to abort the operation
    ny =. FRAMETOCREATEABORT
  elseif. (#@> canonx) -.@-: usedyshape do.
    NB. partition shape doesn't match y
    ny =. FRAMETOCREATEABORT
  elseif. do.
    NB. Get shape of result partitions.  Nonpartitioned axes don't show up in the frame.
    ny =. +/@(0&~:)@> partitionedaxes # canonx
  end.
case. do.   NB. 3 or _3
  if. (3!:0 partitionx__xop) -.@e. 1 4 8 16 do. ny =. FRAMETOCREATEABORT
  elseif. (-.@-: <.) partitionx__xop do. ny =. FRAMETOCREATEABORT
  elseif. do.
    NB. convert list to table; replace values bigger than axis by signed length of axis
    canonx =: 1 ,:^:(2>#@$@]) partitionx__xop
    NB. Trailing axes of ;.3 are included in the partitioning, so extend them by assuming a 'take everything'.
    NB. We can't just omit them from canonx and leave them in the frame, because then the selection would be
    NB. longer than canonx
    NB. If canonx is longer than the shapeofy, it's an error that will be caught during execution.
    NB. Here we just limit canonx to the length of shapeofy, to avoid internal error
    canonx =: canonx ,. (0 >. (#shapeofy) - ({:$canonx)) #"0 (0 _)
    if.  (#shapeofy) < {:@$ canonx do.
      canonx =: (#shapeofy) {."1 canonx
      changeerrormessagefrominterp 'selection too long'
    end.
    usedyshape =: ({:@$ canonx) {. shapeofy   NB. Now always same as shapeofy
    canonx =: ({. canonx) ,: (usedyshape<|)`(,:  usedyshape * *)} {: canonx
    NB. Calculate the number of start positions: ceiling of (length of axis/movement vector) for 3,
    NB. or ceiling of (length-size/movement vector) for _3; but 1 if movement vector is 0
    ny =. (0 ~: 0{canonx)} 1 ,: >. (|0{canonx) %~ (| 1 { canonx) -~^:(partitionn=_3) usedyshape
    ny =. (#shapeofy) {.!.1 ny
  end.
end.
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.
NB. If this is a dyad, add an empty frame for x
ny ; ny ; (($0);^:(1<#y) <ny) ; a: , a:
NB.?lintsaveglobals
)

NB. y is raw selections, result is selections to use
NB. There is a quirk in u;.3, probably in the special code, such that the computation may be restarted
NB. depending on the result shape, and the changing shapes of the individual results
NB.
NB. We could keep this from happening by boxing the individual partition results and then unboxing at
NB. the end; but this would be slow on large operands so we don't do that & hope we make proper adjustments here.
findselection =: 3 : 0
r =. findselection_dissectobj_ f. y
if. partitionn e. 3 _3 do.
  NB. We can't figure out what really happened, if there was an error.  If there is no error we will
  NB. recover by keeping the expected number of elements (the most recent ones).  This is a kludge, and an
  NB. even bigger one is that we have to tighten the selector to avoid having results in repeated lower
  NB. verbs match.  The value to use is the ticket of the last rejected execution, if any
  or =. ;r
  if. (#or) > maxvalid =. 1 >. */frame do.
    r =. < (-maxvalid) {. or
    NB.?lintonly maxvalid =. ''   NB. avoids index error in next line
    selector =: (logticket {~ - >: maxvalid)&(0})&.> selector
  elseif. (2 = #".'shapeofy') *. (1 < #or) do.  NB. If the verb never executed, shapeofy is not defined; fail this test then
    NB. We know that when y is a table, the first cell is run twice (first time to get the result-shape) EXCEPT
    NB. when the result is boxed or the input is too narrow
    NB. So here, when it looks like there was an error, discard the first result, and change selector accordingly
    if. 32 ~: 3!:0 > ({. or) { logvalues do.   NB. discard first only if not boxed
    NB. The optimization is invoked only if there is enough space for two movements plus: a cell (for ;._3)
    NB.  or 1 (for ;.3); and if there is no reversal in any axis
    if. (*./ 0 < 1 { canonx) *. (1 { shapeofy) >: +/ 2 1 * 1 (1})^:(3 = partitionn) 1 {"1 canonx do.
      r =. < }. or
      selector =: (logticket {~ - #or)&(0})&.> selector
    end. end.
  end.
end.
r
)

NB. For ;.0, we generate an automatic selection (type 4, which bypasses the selection and does just the highlighting)
NB. Others follow the usual selection route using the frame
getselection =: 3 : 0
select. partitionn
case. 0 do.
  4 0
case. do.
  getselection_dissectobj_ f. y
end.
)

NB. y is the current selection (a: if forced)
calcphysandhighlights =: 3 : 0
NB. This comes up with a sequence of boxes, one per axis processed, where each box holds the
NB. selected indexes
select. partitionn
case. 0 do.
  hlit =: (usedyshape | ([ - (({"0 1 (0 ,. <:@:|))~ 0&>)~)/ canonx) +&.> (i.&.> {: canonx)
case. 1;_1;2;_2 do.
  NB. For each partitioned axis, we calculate the index list in each partition; then we select
  NB. along each axis to create a list of boxes, one per axis, with the highlight.
  NB. nonpartitioned axes are taken in full and produce a single box containing all indexes;
  NB. we insert zeros into y to highlight those axes in full
  NB.?lintonly partitionedaxes =: ,1
  hlit =: (partitionedaxes #^:_1 >y) {&> (<;.partitionn i.@#)&.> canonx
case. 3;_3 do.
  hlit =: usedyshape (> # ])&.> ((>y) * ({. canonx)) +&.> (i.&.> {: canonx)
case. do.
  NB.?lintonly hlit =: $0
end.
NB. There is never a highlight in x; use an empty
(-valence) {. (< 2 0$a:) , < 2 1&$ < ,@<@:(<"0) ,hlit
NB.?lintsaveglobals
)

NB. y is the current selection in isf form
NB. result is new value to use for selopshapes
calcselectedshapes =: 3 : 0
NB. Keep the rank of selopshapes, but apply the highlight selection to the leading axes
NB. The shape of x is immaterial since u is always invoked as a monad
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. {: selopshapes do.
(-valence) {.   a: , ((#&> hlit) (i.#hlit)} ,)&.> {: selopshapes
else.
(-valence) {.   a: , (<hlit)&{&.> {: selopshapes
end.
)




NB. The monadic valence u;. y:
startmonad ''


NB. *** traversal support ***
NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
NB.?lintonly partitionn =: 0
NB.?lintonly yop =: <'dissectobj'
shapeofy =. ($^:(0<L.))@> {: x
NB. Synthesize the x value that describes this monad, and then transfer to the dyadic code
select. partitionn
case. 0 do.
  partitionx__xop =: (0 _1 */shapeofy) 
case. 1;_1;2;_2 do.
  if. errorcode__yop > EOK do.
    partitionx__xop =: ,0
  else.
    yval =. ,^:(''-:$) fillmask__yop frameselresult__yop selresult__yop
    fret =. (- partitionn e. 2 _2) { yval
    partitionx__xop =: fret -:"_ _1 yval
  end.
case. 3;_3 do.
  partitionx__xop =: ($shapeofy)$<./shapeofy
end.
x calcdispframe__dissectlocalesemidot f. y
)

exegesispartitiondesc =: 3 : 0
select. partitionn
case. 0 do.
'applies its verb to y with all axes reversed'
case. 1 do.
'operates on intervals of y that start at frets matching the first item of y'
case. _1 do.
'operates on intervals of y that start at frets matching the first item of y but do not include the fret'
case. 2 do.
'operates on intervals of y that end at frets matching the last item of y'
case. _2 do.
'operates on intervals of y that end at frets matching the last item of y but do not include the fret'
case. 3;_3 do.
'operates on maximal cubes of y'
end.
)

NB. the rest handled in the common locale

startdyad ''
NB. The dyad x u;.n y:

exegesispartitiondesc =: 3 : 0
select. partitionn
case. 0 do.
'applies its verb to the selected subarray of y'
case. 1 do.
'operates on intervals of y that start at frets indicated by 1s in x'
case. _1 do.
'operates on intervals of y that start at frets indicated by 1s in x but do not include the fret'
case. 2 do.
'operates on intervals of y that end at frets indicated by 1s in x'
case. _2 do.
'operates on intervals of y that end at frets indicated by 1s in x but do not include the fret'
case. 3;_3 do.
'operates on subarrays of y specified by x'
end.
)

NB. the rest handled in the common locale

NB. **** :: ****

primlocale '::'

create =: 3 : 0
NB. For u@n, replace n with n"_ which is its equivalent.  The conjunction-name is a signal to " to suppress the "_ for display
if. nilad =: * noun bwand (<2 0){::y do.
  rank =. COCREATOR createnoun ('_');'';($0);_
  y =. y 2}~ localerank 1 createmodifier (2{y) ,  (conj;'::';($0)) ,: rank
end.
if. 0 = bwand/ verb , > (<0 2;0) { y do.
  failmsg 'domain error: left operand to :: must be a verb'
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the display form of the conjunction and the locale of v.
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
blockismarked =: 0
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
resultissdt =: resultissdt__uop *. resultissdt__vop
if. IFQT do. nuvocpage =: 'coco' end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd '::' jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable 0
auditstg '(' , (verblogstring '') , '(' ,  (logstring 0) , ,'@: ' , (exestring__uop '') , ' :: (' , (logstring 1) , '@: ' , (exestring__vop '') , ')))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. y is rawselx
NB. result is selx to use, which just y.  But we hook this to select from logvaluesd
calcdispselx =: 3 : 0
selvaluesd =: y { logvaluesd
y
NB.?lintsaveglobals
)

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
if. blockismarkable =: blockismarked do.
  NB. This node is marked for expansion.  u and v must both have run.
  NB. Traverse them both and realize them, and make them the input to this block
  expansionstate =: EXPANDED
  dol =. joinlayoutsl x traverse__vop (<'::')&((<_1 0)})`'' travops (TRAVOPSKEEPALL);TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
  NB. Indicate error level before running u
  adderrorlevel ERRORLEVELADVERSE
  ures =. x traverse__uop travops (TRAVOPSKEEPALL);TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
  NB. However u failed, replace it with the 'failure in u :: v' code
  errorcode__uop =: EINADVERSE
  dol =. dol ,~ joinlayoutsl ures
  remerrorlevel''
  NB. Since we have replaced the inputs to this node, the highlighting may have the wrong valence.  But there should
  NB. be no highlights from this node anyway, so suppress them
  physreqandhighlights__inheritroot =: NOPHYSREQ
  NB. Create this block - with an invisible wire to u
  displaylevrank =: (<'Final ' , MAXFINALDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  dol ,&< coname''
elseif. (0 = *./ selopinfovalid) +. (0 = $>selector) do.
  NB. Multiple results were selected.  We don't know what to traverse.  Realize this node, labeled as
  NB. u ::v
  displaylevrank =: (<MAXSTACKDEFSTRINGLENGTH defstring 0) (<_1 0)} rankhistory
  x ,&< coname''  NB. no need to inherit, since not markable
elseif. do.
  NB. A single result was selected; traverse and inherit it
  NB. Remember whether u failed.  If it did, this node is markable.
  if. blockismarkable =: {. selvaluesd do.
    NB. u failed, display v, but indicate that u is waiting in the wings
    expansionstate =: EXPANDABLE
    1 inheritu x traverse__vop (<'::')&((<_1 0)})`'' travops (TRAVOPSKEEPALL);TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
  else.
    NB. u succeeded.
    inheritu x traverse__uop travops (TRAVOPSKEEPALL);TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
  end.
end.
NB.?lintsaveglobals
)


exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. (appearstwice,lastinblock) -: 0 0 do.  NB. not last, not twice in stack
  t =. 'This block %al1%starts the computation of the error path of the verb:',LF,(defstring 0),CR
,: EXEGESISRANKSTACKEXPLAIN;t
else.
  0 0$a:
end.
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
tit =. (*#titlestring) # ', which started in the block(s) marked with ',titlestring
if. -. datapresent do.
  NB. No data.  Must not be the expansion
  ,: exegisisrankoverallnodisp appearstwice;tit
elseif. endflag do.
  select. linetext
  case. 0 do.
    if. appearstwice do.
      t =. 'This block %al1%calculates the error path for the verb:',LF,(defstring 0),CR
    else.
      t =. 'This block %al1%displays the result of the error path for the verb:',LF,(defstring 0),CR
    end.
    ,: EXEGESISRANKOVERALLCOMPEND;t
  case. '' do.
    NB. If there are no errors, we don't explain anything
    0 0$a:
  case. do.
    if. blockismarkable do.
      t =. 'This block %al1%displays the final result of the verb:',LF,(defstring 0),CR,'The computation of the normal and error paths are shown in the blocks feeding into this block.',LF
    else.
      t =. 'This block %al1%will display the final result of the verb:',LF,(defstring 0),CR
    end.
    ,: (EXEGESISRANKOVERALLEXPLAIN,selectable+sellevel);t
  end.
elseif. do. 0 2$a:
end.
)


NB. If this block is markable, toggle the mark; otherwise nothing
selectionoverride =: 3 : 0
if. blockismarkable do.
  blockismarked =: -. blockismarked
  PICKTOOLTIPMSGOK
else.
  PICKTOOLTIPMSGNOORIDE
end.
)

clearselect =: 3 : 0
blockismarked =: 0
clearselect_dissect_ f. y
)

NB. *********** } ***********
primlocale '}'

create =: 3 : 0
NB. Switch to general verb if u is a verb
if. 0 = noun bwand (<0 0) {:: y do.
  changeobjtypeto localedefault
  create y
  return.
end.

create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the display form of c, the locale of u, and the gerund locales if any
'uop cop' =: 1 {"1 y
NB. Save the original v, whether gerund, noun, or verb
uuop =: uop
NB.?lintonly uuop =: <'dissectverb'
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
NB. Save gerund locales if any
if. #gops =: querygerund__uuop '' do.
  NB. If there are gerund locales, the next-to-last will be the one that survives for use as u
  uop =: _2 { gops  NB.?lintonly =: 3$a:
end.
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
if. #gops do.
  uuop =: setvalence__uuop y
  NB.?lintonly uuop =: <'dissectverb'
  gops =: querygerund__uuop ''
  uop =: _2 { gops  NB.?lintonly =: 3$a:
  NB.?lintonly uop =: <'dissectverb'
end.
resultissdt =: resultissdt__uop
separatevalences''
if. IFQT do. nuvocpage =: 'curlyrt' , (valence=2) # '#dyadic' end.
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uuop 2) jd cop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uuop '') , cop , '))'
)

NB. Return the locales for propsel.
proplocales =: 3 : 0
NB. We mustn't propagate a selection to a gerund, bacuase the gerund has the same sellevel as m}
NB. and the selection may be invalid for it.  We keep the sellevel of the gerunds the same as
NB. m} for display purposes, so we pass any selection that is at a lower level.
<^:(0=L.)@".@>^:(0 <: y) (((y~:0)+.(sellevel >: #selections)) , (y=3)) # ;: 'uuop tokensource'
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
NB. Here we never return the actual selection, if any, because it's valid only for highlighting - there aren't really items
NB. of logvalues to back it up.  If the selection has been made, we return a forced selection (a:) which will cause the selector
NB. to be unchanged but mark it valid; if not, we return no selection
getselection =: 3 : 0
NB. If we need a selection and we have one, forced, otherwise pick-only
a: ,&<~ (selectable *. (sellevel < #selections)) { 0 3
)

NB. m} is treated as a verb for reporting purposes
operationfailed =: 1:   NB. An error found during verb execution is a stopper

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
NB. This turns ticket order to selection order
selindextoisf =: 4 : 0
''
)

NB. y is the new selection (boxed, and possibly with an initialselection following)
NB. Result is _1 if selection is OK, 0 if point of error, 1=invalid
NB. There is no way for this selection to fail, since there is only one result, and if we have anything we have everything
auditselection =: _1:

startmonad''

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
dummytraversedowncalcselect y  NB. Set the main global names only

NB. Handle gerund operands if any, and figure the drawing connections to use for u & v
select. #gops
case. 0 do.
  NB. Noun m: no references required, operands come from x
  udol =. NOLAYOUTS traverse__uop TRAVNOUN
  NB. If noun operand failed, pull the plug and display only that result
  if. errorcode__uop > EOK do. udol return. end.  NB. If the noun failed, this node must have failed too
case. 2;3 do.
  NB. gerund m.

  NB. Create a y argument for the pre-u verbs: gerund xy.  Replace the rankstack in y with a rankstack containing only the light lines
  xyy =. (< (#~ (<DLRCOMPEND) ~: 0&{"1) rankhistory) 1} y
  NB. Traverse the u verb to create the m value
  udol =. x traverse__uop xyy

  NB. Realize the other gerunds; their results become the input to u.  We treat the xy verbs as occurring
  NB. BEFORE the u^:v.  Any selection performed here applies to the results of the xy verbs, not the original xy.  So,
  NB. we run the xy verbs BEFORE calcselect here.  That means that travops cannot be used.  But we have to do the same kind of
  NB. work as in fork: the light rank-lines and phys selections must be passed from the input y to the xy verbs, and
  NB. the heavy rank-lines must be kept for u.  Moreover, if an xy verb is stealth, we need to pass rank lines that it
  NB. did not show on to u.
  NB. We end up by replacing y with a new y that shows the result of the xy verbs and the selection operations, and x with the results of the xy verbs

  NB. Execute the [x]y verbs and realize their display

  NB. traverse the xy ops and save their results as x (replacing the input x), to be passed into m} below
  xylocs =. _1 {. gops  NB. The locale y
  ld =. (x ,&< xyy)&(4 : '((<selresultshape__y);dispstealthoperand__y);~(joinlayoutsl traverse__y&>/ x)')"0 xylocs  NB. use ;~ to run traverse before inspecting results
  NB. Replace the x used for m} with the gerund result
  'x srs stealth' =. <@;"1 |: ld
  NB. Create the y to use for m}, with the heavy lines unless stealth caused an xy verb to be omitted.
  if. 0 = +/ stealthcode =. 3 bwand |. stealth do.  NB. Convert to y x order to match rankhistory
    NB. Not stealth: keep heavies
    rankstackcode =. TRAVOPSSTARTHEAVY
  else.
    NB. monad with stealth; just keep all the original lines
    rankstackcode =. TRAVOPSKEEPALL
  end.
  NB. If the input y had no operands, we leave it that way.  Everything else doesn't matter.
  NB. Otherwise, we recalculate y, and recalculate the inputselopshapes etc that we use below
  if. #inputselopshapes do. dummytraversedowncalcselect y =. travops rankstackcode;TRAVOPSPHYSKEEP;(uopval xylocs);< srs end.
case. do.
  traversedowncalcselect y
  EINVALIDVERB earlyerror x return.
end.

NB. We have traversed u to get the m value.  x and y contain the inputs to m}

NB. Create the layout for m
ulayo =. joinlayoutsl udol
titlestring =: 0 fulltitlestring 'm',cop

if. errorcode__uop e. EHASVALIDFILLMASK do. mval =: fillmask__uop frameselresult__uop selresult__uop
else. mval =: 0   NB. Make sure it's defined
end.
NB. If m produced a gerund at execution time, in other words if it constructed an AR, we're beat, because we don't have
NB. the moxie to analyze that at display time.
if. isgerund mval do. failmsg 'm} produced a gerund m without using ` - dissect doesn''t support that' end.

mselect =. <EMPTYPRH   NB. start out with no highlight on m
errorloc =: ''  NB. This will hold the index of the detected error if any
yindex =: 0  NB. Indicate no index list for the x operand

NB. If m didn't run, there is really nothing we can do about m}; just display the final result.
if. (errorcode__uop > EOK) do.
  NB. If m did not run, remove validity from m}.  Traverse it for display.
  y =. 3 {. y  
elseif. #inputselopshapes do.
  sy =. $^:(0<L.) 0 {:: inputselopshapes  NB. shape of y
  if. ($mval) -.@-: }. sy do.
    changeerrormessagefrominterp 'ym agreement'
    errorloc =: 0
  elseif. +./@, emsk =. -. mval e. ((] , -~) i.) {. sy do.
    NB. selector out of bounds.  leave as index error
    errorloc =: ($mval) #: 1 i.~ emsk
  end. 
  NB. We have run the gauntlet, and have set errorloc if there is an error

  NB. If there was a selection, calculate the highlight for the selection part
  if. selectable *. sellevel < #selections do.
    NB. There is a selection.  Find the atom of m that contained it; highlight that atom
    if. -. 0 e. $mval do.  NB. If mval empty, fill-cell processing is active, and there is nothing to select anyway
      if. errorloc -: 0$a: do.
        NB. The operands were valid.  Highlight the selected m
        mselect =. < (2 1 $ < ,sellevel { selections) , <0
        yindex =: , ({.sy) | (sellevel { selections) { mval
      end.
    end.
  elseif. {. $ errorloc do.
    NB. No selection, but there was an error detected on the inputs.  Highlight the error
    mselect =. < (2 1 $ <errorloc) , <0
  end.
end.

NB. Perform selections for m} - needed for display whether m ran or not
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x ;< ulayo ,&<"1 0 < (2 1 $ <0 0$0) , <0 return. end.

NB. Label the display.  Note that x may have changed number of operands, but we have the right one here
displaylevrank =: rankhistory
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand operand).  This box contains
NB. a table of dol;highlights
x ; (coname '') ; (<ulayo ,&<"1 0 mselect)
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. datapresent do.
  t =. 'The result of the verb:',LF,(defstring 0),CR

  if. *./ selopinfovalid do.
    t =. t , 'the result is a composite item of y, with each atom of m telling which item of y the corresponding atom of the result should come from',LF
    type =. EXEGESISRANKOVERALLCOMPEND
  else.
    t =. t , 'Select a result-cell to see where it came from.'
    type =. EXEGESISRANKOVERALLNOOPS
  end.
else.
  'type t' =. exegisisrankoverallnodisp 1;titlestring
end.
,: type;t
)

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb (always empties here, since rank is _)
NB. result is (selframe);(frame);(frame of value to display);(selopshapes for next level - cells of this verb);resultlevel;arglevel
NB. Called even when selector is empty, if rank-calculus probe
calcdispframe =: 4 : 0
NB. If this is a rank-calculus probe, vval is invalid and we can't look at it.  Its value doesn't matter anyway
if. a: -: selector do. sf =. '' else. sf =. }. $^:(0<L.) 0 {:: selopshapes end.  NB. calc selframe: shape of an item of y
sf;($0);y;a:,a:
)


NB. y is the current selection (a: if forced)
NB. Calculate a highlight x or y, as appropriate
calcphysandhighlights =: 3 : 0
NB. If the current selection was found when we did the search for mselect, use the
NB. index to x that we calculated there, and highlight that value in x.  Otherwise, highlight
NB. the same position in y.  Only one of these positions will be highlighted.
if. #$yindex do.
  NB. Highlight into y
  ,< 2 1 $ < , yindex&,&.> sellevel { selections
else.
  ,< 2 {. EMPTYPRH
end.

)

startdyad''

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
NB. Create a y argument for the pre-u verbs: gerund xy.  Replace the rankstack in y with a rankstack containing only the light lines
dummytraversedowncalcselect y  NB. Set the main global names only
xyy =. (< (#~ (<DLRCOMPEND) ~: 0&{"1) rankhistory) 1} y

NB. Handle gerund operands if any, and figure the drawing connections to use for u & v
select. #gops
case. 0 do.
  NB. Noun m: no references required, operands come from x
  udol =. NOLAYOUTS traverse__uop TRAVNOUN
  NB. If noun operand failed, pull the plug and display only that result
  if. errorcode__uop > EOK do. udol return. end.  NB. If the noun failed, this node must have failed too
case. 3 do.
  NB. gerund m.
  NB. Traverse the u verb to create the m value
  udol =. x traverse__uop xyy

  NB. Realize the other gerunds; their results become the input to m.  We treat the xy verbs as occurring
  NB. BEFORE the u^:v.  Any selection performed here applies to the results of the xy verbs, not the original xy.  So,
  NB. we run the xy verbs BEFORE calcselect here.  That means that travops cannot be used.  But we have to do the same kind of
  NB. work as in fork: the light rank-lines and phys selections must be passed from the input y to the xy verbs, and
  NB. the heavy rank-lines must be kept for u.  Moreover, if an xy verb is stealth, we need to pass rank lines that it
  NB. did not show on to u.
  NB. We end up by replacing y with a new y that shows the result of the xy verbs and the selection operations, and x with the results of the xy verbs

  NB. Execute the xy verbs and realize their display

  NB. traverse the xy ops and save their results as x (replacing the input x), to be passed into m} below
  NB. order of gerunds is v1 v2 v0
  xylocs =. 0 2 { gops  NB. The locales x y
  ld =. (x ,&< xyy)&(4 : '((<selresultshape__y);dispstealthoperand__y);~(joinlayoutsl traverse__y&>/ x)')"0&.|. xylocs  NB. use ;~ to run traverse before inspecting results
  NB. Replace the x used for m} with the gerund results
  'x srs stealth' =. <@;"1 |: ld
  NB. Create the y to use for m}, with the heavy lines unless stealth caused an xy verb to be omitted.
  if. 0 = +/ stealthcode =. 3 bwand |. stealth do.  NB. Convert to y x order to match rankhistory
    NB. Not stealth: keep heavies
    rankstackcode =. TRAVOPSSTARTHEAVY
  else.
    rankstackcode =. TRAVOPSKEEPALL
    NB. dyad with one or both stealth.  Create new rankhistory, where each column is chosen to be the
    NB. value selected by that stealthop, or empty if not stealth.  Delete lines that end up with no rank
    NB. This is copied from fork, except that we never generate highlights
    NB. In stealthcode 1 = ] = y, 2 = [ = x, 0 = neither
    rankhistory =: (#~    0 1 1 -.@-:"1 ('';(,0);(,0)) ="1  $&.>@:((0 2 3)&{"1)) (2 {."1 rankhistory) ,. stealthcode {"1 a: ,. 2 $!.a:"1 (2) }."1 rankhistory  NB. $!.a: needed because rankhistory may be empty (we haven't traversed)
  end.
  NB. If the input y had no operands, we leave it that way.  Everything else doesn't matter.
  NB. Otherwise, we recalculate y, and recalculate the inputselopshapes etc that we use below
  if. #inputselopshapes do. dummytraversedowncalcselect y =. travops rankstackcode;TRAVOPSPHYSKEEP;(uopval xylocs);< srs end.
case. do.
  traversedowncalcselect y
  EINVALIDVERB earlyerror x return.
end.

NB. We have traversed u to get the m value.  x and y contain the inputs to m}

NB. Create the layout for m
ulayo =. joinlayoutsl udol
titlestring =: 0 fulltitlestring 'm',cop

if. errorcode__uop e. EHASVALIDFILLMASK do. mval =: fillmask__uop frameselresult__uop selresult__uop
else. mval =: 0   NB. Make sure it's defined
end.
NB. If m produced a gerund at execution time, in other words if it constructed an AR, we're beat, because we don't have
NB. the moxie to analyze that at display time.
if. isgerund mval do. failmsg 'm} produced a gerund m without using ` - dissect doesn''t support that' end.

mselect =. <EMPTYPRH   NB. start out with no highlight on m
errorloc =: ''  NB. This will hold the index of the detected error if any
xindex =: 0  NB. Indicate no index list for the x operand

NB. If m didn't run, there is really nothing we can do about m}; just display the final result.
if. (errorcode__uop > EOK) do.
  NB. If m did not run, remove validity from m}.  Traverse it for display.
  y =. 3 {. y
elseif. #inputselopshapes do.
  'sx sy' =. $^:(0<L.)&.> inputselopshapes  NB. shapes of x and y
  NB. Create canonical form of mval, which is a table of index lists for each atom of
  NB. mval.  The shape of the table gives the shape of the selection from y; there must be no fill
  NB. across the atoms.  And, the shape of the x must be a suffix of the selected shape
  if. 0 e. $ mval do. cmval =. 0 $ i. #sy   NB. processed as cell of fills
  else.
    cmval =. mval
    if. 0 = L. cmval do. cmval =. <"0 cmval end.  NB. bring up to boxing level 1
    if. +./@, emsk =. 1 < #@$@> cmval do.
      NB. The first boxing level should contain rank < 2.  If not, change the error message
      changeerrormessagefrominterp 'selector rank'
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif. +./@, emsk =. 2 < L.@> cmval do.
      NB. Selectors should not have level > 3
      changeerrormessagefrominterp 'selector level'
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif. +./@, emsk =. +./@;@> ((1=L.) *. '' -.@-: $)&.>&.> cmval do.
      NB. Complementary selectors must be atomic
      changeerrormessagefrominterp 'selector invalid'
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif. +./@, emsk =. (#sy) < #@> cmval do. 
      NB. Selectors should have no more axes than the rank of y
      changeerrormessagefrominterp 'selector too long'
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif.
    cmval =. <"0^:(0=L.)&.> cmval  NB. Bring up to boxing level 2
    cmval =. (#sy) {.!.(<<'')&.> cmval  NB. Bring up to full length with complementary 
    +./@, emsk =. ([: +./ (((] , -~) i.)&.> sy)&((*@#@-.~ ,@:>)&>))@> cmval do.
      NB. Index not in the valid range.  Leave as index error
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif.
    NB. Valid selector, except for fill.  Make all indexes positive
    cmval =. sy&((|L:0)"0)&.> cmval
    NB. Convert the complementary selectors to the list of selected indexes, in order
    cmval =. sy&((i.@[ -. >@])^:(1=L.@])&.>)&.> cmval
    NB. Now we have a level-2 set of selectors.  Roll them up into a level-1 set of arrays of complete index lists
    NB. append a: to guarantee that there will be at least 2 boxes, so we will not have a neutral which might have any shape.
    NB. This way we know we are going to have complete index lists
    cmval =. > ,"0 1/&.>/@(,&a:)&.> cmval  NB. >>cmval can't be empty
    NB. Get the shape of each fully-filled-out selector; extend the shape with high-order 1s (trickily); verify all shapes
    NB. equal (otherwise there is fill
    +./@, emsk =. (~: ''&($,)) cmvalshapes =. (<"1) |."1  >: <:@|.@> }:@$&.> cmval do.
      NB. Selectors should have no more axes than the rank of y
      changeerrormessagefrominterp 'incompatible selectors'
      NB. We can't finger any one selector, but we find the first one that mismatches the head shape
      errorloc =: ($cmval) #: 1 i.~ emsk
    elseif. (($cmvalshapes) , > '' ($,) cmvalshapes) (] -.@-: ({.!._1~   -@#)) sx do.
      NB. $x must be a suffix of $m{y
      changeerrormessagefrominterp 'xm agreement'
      NB. We can't finger any one selector, so we use a scalar value of errorloc to indicate the error
      errorloc =: 0  NB. scalar 0 - xm agreement
    end.
    NB. We have run the gsuntlet, and have set errorloc if there is an error
  end. 

  NB. If there was a selection, calculate the highlight for the selection part
  if. selectable *. sellevel < #selections do.
    NB. There is a selection.  Find the atom of m that contained it; highlight that atom
    if. -. 0 e. $mval do.  NB. If mval empty, fill-cell processing is active, and there is nothing to select anyway
      if. errorloc -: 0$a: do.
        NB. The operands were valid, so we know we have converted cmval to level-1 table of full index lists
        NB. Find the last box of cmval that contains the selection
        if. 1 e. cmmask =. , ((sellevel { selections) (e. ,) <"1)@> cmval do.
          NB. The selection appeared in m.  Find the last box of m and create a highlight for it.
          NB. Turn it into a highlight record
          mselect =. < (2 1 $ < ,<($cmval) #: cmmask i: 1) , <0
          NB. Get the index in x that corresponds (we will use this for highlighting the cell)
          NB. count all the rows before the one that matched, and use that to generate the index list into x
          tailx =. (, <"1 (cmmask i: 1) {:: , cmval) i: sellevel { selections
          xindex =: sx #: tailx + +/ (cmmask i: 1) {. , */@}:@$@> cmval
        end.
      end.
    end.
  elseif. {. $ errorloc do.
    NB. No selection, but there was an error detected on the inputs.  Highlight the error
    mselect =. < (2 1 $ <errorloc) , <0
  end.
end.

NB. Perform selections for m} - needed for display whether m ran or not
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x ;< ulayo ,&<"1 0 < (2 1 $ <0 0$0) , <0 return. end.

NB. Label the display.  Note that x may have changed number of operands, but we have the right one here
displaylevrank =: rankhistory
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand operand).  This box contains
NB. a table of dol;highlights
x ; (coname '') ; (<ulayo ,&<"1 0 mselect)
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. datapresent do.
  t =. 'The result of the verb:',LF,(defstring 0),CR

  if. *./ selopinfovalid do.
    t =. t , 'the result is y with selected atoms replaced by atoms of x',LF
    type =. EXEGESISRANKOVERALLCOMPEND
  else.
    t =. t , 'Select a result-cell to see where it came from.'
    type =. EXEGESISRANKOVERALLNOOPS
  end.
else.
  'type t' =. exegisisrankoverallnodisp 1;titlestring
end.
,: type;t
)

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb (always empties here, since rank is _)
NB. result is (selframe);(frame);(frame of value to display);(selopshapes for next level - cells of this verb);resultlevel;arglevel
NB. Called even when selector is empty, if rank-calculus probe
calcdispframe =: 4 : 0
NB. If this is a rank-calculus probe, vval is invalid and we can't look at it.  Its value doesn't matter anyway
if. a: -: selector do. sf =. '' else. sf =. $^:(0<L.) 1 {:: selopshapes end.  NB. calc selframe
sf;($0);y;a:,a:
)


NB. y is the current selection (a: if forced)
NB. Calculate a highlight x or y, as appropriate
calcphysandhighlights =: 3 : 0
NB. If the current selection was found when we did the search for mselect, use the
NB. index to x that we calculated there, and highlight that value in x.  Otherwise, highlight
NB. the same position in y.  Only one of these positions will be highlighted.
if. #$xindex do.
  NB. Highlight into x
  (2 1 $ < ,<xindex);< 2 {. EMPTYPRH
else.
  NB. Highlight into y
  (2 {. EMPTYPRH) ;< 2 1 $ < sellevel { selections
end.
)

NB. **** !: ****

localeforeign_dissect_ =: primlocale '!:'

create =: 3 : 0
NB. m and n must be nouns
if. 0 = bwand/ noun , > (<0 2;0) { y do.
  failmsg 'domain error: operands of m!:n must be nouns'
end.

'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =. vop =. <'dissectverb' [ cop =. ''
defstg =. (defstring__uop 2) jd  cop jd (defstring__vop 3)
if. knownmn =: bwand/ sdt , > (<0 2;0) { y do.
  NB. If m and n are sdts, execute the conjunction to determine the part of speech it produces
  try.
    ". 'exeobj =. ' , defstg =. ; uvval =: 3 : 'if. 32 = 3!:0 y do. defstring__y 2 else. y end.'&.> (1) {"1 y
  catch.
    failmsg 'domain error: invalid operand to !:'
    return.
  end.
  tokennums =. ; 2 {"1 y
  restype =: 4!:0 <'exeobj'
else. restype =: 3   NB. assume verb if unknown
  NB.?lintonly tokennums =. 0 [ uvval =: 1;2;3
end.
select. restype
case. 0 do.  NB.?lintonly exeobj =. 0
  createnoun (5!:5 <'exeobj');'';tokennums;<exeobj
case. 1 do.
  adv;(enparen defstg);tokennums
case. 2 do.
  conj;(enparen defstg);tokennums
case. 3 do.
  NB. We will treat this as a generic verb, except for the overrides we have in this locale
  changeobjtypeto 'dissectverb'
  insertoverride localeforeign
  NB. Pass the token number of the modifier in as the verb token number.  That will go into tokensource
  create_dissectverb_ f. defstg;(<1 2){y
case. do.
  failmsg 'Invalid type while applying modifier'
  return.
end.
)

NB. All we do with the valence is override the nuvocpage
setvalence =: 3 : 0
setvalence_dissectverb_ f. y
if. IFQT do.
  if. knownmn do. nuvocpage =: 'Foreigns#m',": 0 ". 0 {:: uvval
  else. nuvocpage =: 'bangco'
  end.
end.
coname''
)

NB. Get text of explanation, if any
lookupexplanation =: 3 : 0
if. knownmn do.
'm!:n creates a verb to perform a special function.  The verb created here is documented under m=',(": 0{::uvval),', n=',(": 2{::uvval),'.',LF
else.
'The m!:n conjunction produces a great variety of features based on the (m) and (n) operands.',LF
end.
)

proplocales =: 3 : 0
(y=3) # uop,tokensource;vop
)

NB. **** b. ****

localebasic_dissect_ =: primlocale 'b.'

create =: 3 : 0
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
defstg =. (defstring__uop 2) jd  cop
if. knownm =: (sdt+noun) = (<0 0) {:: y do.
elseif. bwand/ verb , (<0 0) {:: y do.
  knownm =: 2
end.

NB. We will treat this as a generic verb, except for the overrides we have in this locale
changeobjtypeto 'dissectverb'
insertoverride localebasic
NB. Pass the token number of the modifier in as the verb token number.  That will go into tokensource
create_dissectverb_ f. defstg;(<1 2){y
)

NB. All we do with the valence is override the nuvocpage
setvalence =: 3 : 0
setvalence_dissectverb_ f. y
if. IFQT do. nuvocpage =: knownm {:: 'bdot';'bdot';'bdotu' end.
coname''
)

NB. Get text of explanation, if any
lookupexplanation =: 3 : 0
select. knownm
case. 0 do.
  '(m b.) produces a verb to perform Boolean operations, either on Boolean nouns or on each bit of integers.  The function depends on (m).',LF
case. 1 do.
  uval =. 0 ". defstring__uop 0
  b =. (":uval),' b.'
  if. valence = 1 do.
    b =. enparen b , ' y'
    op =. enparen (16 | uval) {:: '0';'0';'0';'0';'y';'y';'y';'y';'NOT y';'NOT y';'NOT y';'NOT y';'1';'1';'1';'1'
  else.
    b =. enparen 'x ', b , ' y'
    op =. enparen (16 | uval) {:: '0';'x AND y';'x > y';'x [ignoring y]';'x < y';'y [ignoring x]';'x ~: y [XOR]';'x OR y';'x NOR y';'x = y [XNOR]';'NOT y';'x >: y';'NOT x';'x <: y';'x NAND y';'1'
  end.
  i =. ": >: 2 ^. _1&(33 b.)&.<: 0   NB. wordsize in bits
  select. uval
  case. <"0 (_16) , i: 15 do.
    b , ' calculates the function ' , op , ' on Boolean arguments.',LF
  case. <"0 (16) + i. 16 do.
    b , ' calculates the function ' , op , ' bit by bit on integer arguments.  The result depends on the size of an integer [yours are ',i,' bits].',LF
  case. 32 do.
    if. valence=1 do.
      b , ' produces (y).',LF
    else.
      b , ' bitwise-rotates (y) leftwards by (x) positions.  The result depends on the size of an integer [yours are ',i,' bits].',LF
    end.
  case. 33 do.
    if. valence=1 do.
      b , ' produces (y).',LF
    else.
      b , ' performs unsigned shift of (y) leftwards by (x) positions.  Vacated positions are filled with 0 bits.  The result depends on the size of an integer [yours are ',i,' bits].',LF
    end.
  case. 34 do.
    if. valence=1 do.
      b , ' produces (y).',LF
    else.
      b , ' performs signed shift of (y) leftwards by (x) positions.  Vacated positions are filled with 0 bits for left shift, sign bits for right shift.  The result depends on the size of an integer [yours are ',i,' bits].',LF
    end.
  end.
case. do.
  'This is a verb that gives information about the underlying verb:',CR,(defstring__uop 0),LF,'depending on (y).  (y)-value _1 gives the inverse, 0 gives the rank, 1 the neutral function.',LF
end.
)

proplocales =: 3 : 0
(y=3) # uop,<tokensource
)

NB. **** : ****

primlocale ':'

create =: 3 : 0
if. bwxor/ verb bwand > (<0 2;0) { y do.
  failmsg 'domain error: operands of u :v must be verb-verb or noun-noun'
end.
'uop vop' =: (<0 2;1) { y
NB.?lintonly uop =: vop =: <'dissectnoun'
if. noun bwand (<0 0) {:: y do.
  NB. m : n - we must be able to resolve m at parse time.  Create an object of the appropriate type
  if. -. (ifinlocale__uop 'nounvalue') do.
    failmsg 'dissect restriction: in m :n, m must be a constant or a name'
  end.
  if. ifinlocale__vop 'nounvalue' do.
    NB. If we can resolve n at parse time, audit it
    select. 3!:0 nounvalue__vop 
    case. 1;4;8;16 do.
      if. 0 ~: #$nounvalue__vop do.
        failmsg 'domain error: in m :n, numeric n must be an atom'
      elseif. 0 ~: nounvalue__vop do.
        failmsg 'domain error: in m :n, numeric n must be 0'
      elseif. do.
        failmsg 'dissect restriction: in m :n, numeric n is not supported'
      end.
      return.
    case. 2;131072 do.
      if. 2 <: #$nounvalue__vop do.
        failmsg 'rank error: in m :n, literal n must be a table or list'
        return.
      end.
    case. 32 do.
      if. 1 <: #$nounvalue__vop do.
        failmsg 'rank error: in m :n, boxed n must be an atom or a list'
        return.
      elseif. -. (#@$@> nounvalue__vop) *./@:e. 0 1 do.
        failmsg 'rank error: in m :n, boxed n must have contents with rank < 2'
        return.
      elseif. 0 e. ((2 131072 e.~ 3!:0) +. (0 e. $))@> nounvalue__vop do.
        failmsg 'domain error: in m :n, boxed n must contain strings'
        return.
      end.
    case. do.
      failmsg 'domain error: in m :n, n must be boxed, literal, or fixed-length numeric'
      return.
     end.
  end.
  select. nounvalue__uop
  case. 3;4;13 do.
    NB. 3 : n etc.  These define verbs.  This is just as if the : were a generic modifier
    changeobjtypeto localedefault
    create y
  case. 0 do.
    NB. 0 : n - define a noun - silly - just switch to n
    noun;vop;tokensource__vop=:tokensource__uop,((<1 2){::y),tokensource__vop
  case. 1;2 do.
    NB. 1,2 : n - define an adverb/conj, which is then a generic modifier producing a verb
    NB. This is a kludge, because the m : n is not instrumented in the executed sentence.  We would need
    NB. a locale to execute that, and then another to execute the derived modifier; and all that for no analytical
    NB. benefit.  So we just lump together the entire n as part of m : n and make that a modifier of the correct
    NB. type.  This produces a problem: if n is the result of a verb execution, there is no way to find its tokens,
    NB. because the execution has been swallowed up in the m : n.  We could try to ferret them out, but here we
    NB. employ another kludge, noting that since : is a conjunction its right operand must be either a single
    NB. word or in parentheses, and we can get either from tokensource; if there are gaps we fill them in
    (nounvalue__uop{0,adv,conj);('(' , (defstring__uop 0) , ' : ' , (defstring__vop 2) , ')');(tokensource__uop,((<1 2){::y),([ + i.@>:@(-~))/ (<./ , >./) tokensource__vop)
  case. do.
    failmsg 'domain error: invalid m in m :n'
  end.
  return.
else.
  NB.?lintonly uop =: vop =: <'dissectverb'
  NB. verb : verb.  We will wait till we know the valence and then
  NB. treat as if only that valence were given
  NB. Register this object so we can clean up at end
  newobj__COCREATOR coname''
  create_dissectobj_ f. (<1 2) { y
  NB. Set resultissdt for modifier processing
  resultissdt =: resultissdt__uop *. resultissdt__vop
  verb;(coname'');tokensource
end.
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
actop =: valence { '';uop,vop
NB.?lintonly actop =: <'dissectverb'
resultissdt =: resultissdt__actop
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd ':' jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
exestring__actop y
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)


NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 'x traverse__actop y'

NB. **** !. ****

primlocale '!.'

NB. Fit has very light footprint: it passes the fit-noun to u, which is responsible
NB. for all display and inclusion into the sentence.  This is necessary since fit must be applied
NB. directly to the verb it is set for, inside all instrumentation.  We should show the calculation of the
NB. fit and bring it in as a right-side input, but we don't.
create =: 3 : 0
if. noun bwand 0 0 {:: y do.
  failmsg 'domain error: left operand of !. must be a verb'
  return.
end.
if. verb bwand 2 0 {:: y do.
  failmsg 'domain error: right operand of !. must be a noun'
  return.
end.
'uop vop' =: (<0 2;1) { y
NB.?lintonly uop =: vop =: <'dissectverb'
if. ((1 2 {:: y) , tokensource__vop) applyfit__uop (1 1 {:: y) , defstring__vop 3 do.
  failmsg 'domain error: !. not supported'
  return.
end.
codestroy''
verb;uop;tokensource__uop
)


NB. ******* verbs with explicit support ***********

NB. *********** $: *************

'dissectverb' primlocale '$:'

NB. y is (string form of the verb);tokens it came from
create =: 3 : 0
r =. create_dissectverb_ f. y
NB. Changes for $:
resultissdt =: 0   NB. $: is NOT an sdt
r
NB.?lintsaveglobals
)

setvalence =: 3 : 0
if. executingvalence__COCREATOR ~: #y do.
  failmsg 'dissect restriction: recursion must have the same valence as the original execution'
end.
NB. Indicate that this verb-phrase contains a recursion
recursionencountered__COCREATOR =: 1
setvalence_dissectverb_ f. y
)

NB. We save a couple of things and then traverse as a normal verb
traverse =: endtraverse@:(4 : 0)
recursionpoint =: {. executingmonaddyad__COCREATOR
NB.?lintonly recursionpoint =: <'dissectmonad'
x traverse_dissectverb_ f. y
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN;'This block %al1%shows the result of a recursion. Select the result to see results of all recursion levels and examine any of them.',LF
)

getselection =: 3 : 0
NB. Save the ticket for this result.  If we get a selection at this node, we will send this value back to the
NB. recursion point to make the selection
NB. y is the list of valid results.  If it's empty, there is no ticket to save (but there will be no display
NB. either, and the ticket is used only by selection, so there's no harm)
if. #y do. currentticket =: ({. y) { logticket end.
NB.?lintonly currentticket =: 0
getselection_dissectverb_ f. y
NB.?lintsaveglobals
)

NB. Any selection in the recursion result is passed to the recursion point.
NB. We have saved the locale of the recursion point, and 
selectionoverride =: 3 : 0
selectrecursion__uop__recursionpoint currentticket
PICKTOOLTIPMSGOK  NB. Indicate that we have overridden, abort selection
)

NB. *********** 0-9: *************

'dissectverb' primlocale '0:1:2:3:4:5:6:7:8:9:_1:_2:_3:_4:_5:_6:_7:_8:_9:_:__:'

NB. Set globals, then initialize display for the verb
traverse =: endtraverse@:(4 : 0)
assert. 1 2 e.~ #x
traversedowncalcselect y  NB. Just to set error globals
stealthopencountered__COCREATOR =: 1  NB. Note that we saw a nilad
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
if. -. dispniladinputs +. displayshowstealth do.
  NB. we are not displaying the inputs.  Remove inputs to x
  x =. a:"0 x
end.
displaylevrank =: rankhistory
x ,&< coname'' NB. no v, so no change to the DOLs
)

NB. **** { ****

'dissectverb' primlocale '{'

setvalence =: 3 : 0
NB. We only handle the dyad
if. 1 = #y do. changeobjtypeto 'dissectverb' end.
setvalence_dissectverb_ f. y
)

exestring =: 3 : 0
NB. init for logging.  We log the x operand as d
initloggingtable 0
NB. Instrument the forward verb - using dyadic logstring
auditstg '(' ,  , '(<@[ ' , (logstring'') , (verblogstring '') , '{)"0 _)'
NB.?lintonly 'logvalues logticket' =: (1$a:);$0
NB.?lintsaveglobals
)

NB. y is rawselx
NB. result is selx to use, which just y.  But we hook this to select from logvaluesd
calcdispselx =: 3 : 0
selvaluesd =: y { logvaluesd
y
NB.?lintsaveglobals
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
getselection =: 3 : 0
NB. We ALWAYS show the highlights for this verb, even if there is no selection
if. selectable *. (sellevel < #selections) do. 1 ;< sellevel { selections
elseif. 0 e. $^:(0<L.) 0 {:: selopshapes do. 0 0  NB. If empty x, don't highlight
elseif. selectable do. 0 0  NB. If selectable but not selected, don't presume to select
elseif. #selvaluesd do. 6 0   NB. otherwise, must be singleton: autoselect it to show the selection, since user can't click it.  But only if we executed
elseif. do. 0 0
end.
)

NB. The only thing special here is the highlighting.  
calcphysandhighlights =: 3 : 0
assert. valence = #frames
NB. Get shape of y operand
'xshape yshape' =. $^:(0<L.)&.> selopshapes
sval =. >y
selector =. (xshape #. sval) {:: selvaluesd
bsel =. ({.@> isfensureselection isftorank2 y) <@({.~ #)&.> frames
NB. Create ISF for the left operand - just the chosen atom.  But if there is no frame, we highlight wrong if we try to highlight the atom,
NB.  so suppress the highlight then
if. 0 = #>{.frames do. lsel =. (< 2 0$a:) else. lsel =. <<,sval end.
rsel =. (< 2 0$a:)
NB. Create ISF for the right operand - the selected values for each axis
NB. We have to audit for validity, and perform no selection if invalid
if. 0 = L. selector do. selector =. <"0 selector end.  NB. bring up to boxing level 1
if. *./ 2 > #@$@> selector do.  NB. Level-1 selectors must be atom or list
  if. 3 >: L. selector do.  NB. No box with more than 3 levels
    if. *./ ((0=L.) +. '' -: $)@> > selector do.  NB. complementary selectors are atomic
      if. (#>selector) <: #yshape do.   NB. No extra axes
        selector =. <"0^:(0=L.) > selector  NB. Remove outer boxing; bring up to level 1
        selector =. (#yshape) {.!.(<<'') selector  NB. add complementary empty axes to end of shape
        if. *./ (((] , -~) i.)&.> yshape) (*./@:e.~ ,@:>)&> selector do.  NB. all indexes valid
          selector =. yshape |L:0"0 selector  NB. switch all indexes to positive
          selector =. yshape ((i.@[ -. >@])^:(1=L.@]))&.> selector  NB. Convert complementary indexes
          NB. Create the selector: a single selector, containing (two levels down) a list of boxes, one per axis,
          NB. with each box containing an atomic box of alternatives
          rsel =. < < <@,&.> selector
        end.
      end.
    end.
  end.
end.
<@(2 1&$)"0 lsel,rsel
)



NB. **** default ****
localedefault_dissect_ =: primlocale ''
NB. Remove the last element in the search, to make this the 'search failed' locale
dissectprimindex_dissect_ =: }: dissectprimindex_dissect_

NB. This is the default object to handle unknown modifiers

NB. Unknown modifiers create verbs (we hope).  We will create something that looks like a verb -
NB. it will be the display form of the modified input operands.  We will then pretend to be a verb.
NB. y is the exeblock for the modifier, either 2 or 3 boxes
create =: 3 : 0
NB. Get the locales
ucvlocs =: 1 {"1 y
if. 2 = #y do.
  'uop cop' =. ucvlocs
NB.?lintonly uop =. <'dissectverb' [ cop =. ''
  stg =. (defstring__uop 2) jd cop
else.
  'uop cop vop' =. ucvlocs
NB.?lintonly uop =. vop =. <'dissectverb' [ cop =. ''
  stg =. (defstring__uop 2) jd  cop jd (defstring__vop 3)
end.
NB. We will treat this as a generic verb, except for the overrides we have in this locale
changeobjtypeto 'dissectverb'
insertoverride localedefault
NB. Pass the token number of the modifier in as the verb token number.  That will go into tokensource
create_dissectverb_ f. stg;(<1 2){y
)

proplocales =: 3 : 0
(y=3) # (<tokensource) 1} >&.> ucvlocs
)


NB. ********* invisible modifiers ************

NB. **** fork ****
cocurrent 'dissectfork'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: cop =: <'dissectverb'
NB. Remember whether this is an nvv-type fork
if. vvv =: * verb bwand (<0 0) {:: y do.
NB. If it's vvv, see if starts with [: .  If so, go process it as u@:v
  if. 4 = stealthoperand__uop do.  NB. [:
    changeobjtypeto localeat
    nilad =: 0   NB. used by localeat
NB. Move the token values too
NB. Use the conjunction-name as a way of flagging this as [: for display
    create ((1 { y) ,: conj;'[:';(<0 2){y) 0 1} y
    return.
  end.
end.
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop *. resultissdt__cop
NB. Collect token #s in case of error
verb;(coname'');(; 2 {"1 y)
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
if. vvv do.
  uop =: setvalence__uop y
  NB.?lintonly uop =: <'dissectverb'
end.
cop =: setvalence__cop resultissdt__uop , resultissdt__vop
NB.?lintonly cop =: <'dissectverb'
resultissdt =: resultissdt__cop
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y~:0) (defstring__uop 1) jd ' ' , (defstring__cop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. vvv do. uops =. 0,valence__uop,0 else. uops =. 0 1 0 end.
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop uops) , ' ' , (exestring__cop'') , ' ' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. Include u if we want non-nouns (i. e. clone), or if it's a verb
<^:(0=L.)@".@>^:(0 <: y) (((y~:0)+.vvv),1 1,y=3) # ;: 'uop cop vop tokensource'
)

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 1 fulltitlestring 'fork'
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
if. vvv do.
  dolv =. joinlayoutsl x traverse__vop ((<displayshowstructmods#'fork/')&((<_1 0)})`'') travops TRAVOPSKEEPLIGHT;TRAVOPSPHYSKEEP;(vopval selopinfovalid);< selopshapes
  dolu =. joinlayoutsl x traverse__uop ((<displayshowstructmods#'\fork')&((<_1 0)})`'') travops TRAVOPSKEEPLIGHT;TRAVOPSPHYSKEEP;(vopval selopinfovalid);< selopshapes
else.
NB. nvv.  Traverse u as a noun; reset highlighting level for it, since it starts a new path
  dolv =. joinlayoutsl x traverse__vop ((<displayshowstructmods#'fork/')&((<_1 0)})`'') travops TRAVOPSKEEPLIGHT;TRAVOPSPHYSKEEP;(vopval selopinfovalid);< selopshapes
  dolu =. joinlayoutsl NOLAYOUTS traverse__uop TRAVNOUN
end.
NB. If u or v are stealth, we need to preserve the original rank-stack info associated with the inputs, and route that info to
NB. the correct side.  We always need to preserve the heavy inputs
NB. We will create a new rankstack to pass in if either operand is stealth.  This will have x and y arguments, each of which is
NB. chosen from (x,y,empty) as called for by the stealthness of u or v.  If neither operand is stealth, we will just do
NB. STARTHEAVY on the original rankhistory - but we do it by hand here, so we can add the '\fork/' label if called for
if. 0 0 -: stealthcode =. 3 bwand dispstealthoperand__vop , dispstealthoperand__uop do.
  NB. This is STARTHEAVY, calculated by hand
  rankhistory =: (2) {."1 (#~ (<DLRCOMPEND) = 0&{"1) rankhistory
else.
  NB. Create a: y y or a: y x, then select using 0 ] [ to give the following selections:
  NB. nonstealth=a:  monad stealth=y   dyad ]=y   dyad [=x  (for dyads, each argument is selected separately)
  NB. Then discard rows that have nonatom in the title (so not heavy) and empty in both columns
  NB. In stealthcode 1 = ] = y, 2 = [ = x, 0 = neither
  rankhistory =: (#~    0 1 1 -.@-:"1 ('';(,0);(,0)) ="1  $&.>@:((0 2 3)&{"1)) }: (2 {."1 rankhistory) ,. stealthcode {"1 a: ,. 2 $"1 (2) }."1 rankhistory
end.
NB. If we are showing structural tags, add one for this middle tine of fork
if. displayshowstructmods do. rankhistory =: ('\fork/';coname'') , rankhistory end.

NB. We keep the highlights from the input, to both the left and right ops.  This will not be needed EXCEPT when u or v is ][.  Other times,
NB. the highlights go from u to v, and they are at the same sellevel (since u has infinite rank), so nothing that happened below u's sellevel matters.
NB. But if uv is ][, then highlights from c (or below) might reach through this node and highlight a higher node - and those nodes may have
NB. lower sellevels, so we need to make sure the lower selections are as they would have been for the omitted ][.  The way to do this is
NB. to start the physreqs with the values that they would have had to start uv.
NB. But for nvv, remove the highlights from u, since it starts from sellevel 0
1 inheritu (dolu,dolv) traverse__cop travops TRAVOPSKEEPALL;(TRAVOPSPHYSCHOOSE (vvv { _1 0), _2);(uopval uop,vop);< selresultshape__uop ,&< selresultshape__vop
)

exegesisrankstack =: 3 : 0
'appearstwice lastinblock datapresent' =. y
if. lastinblock do.   NB. If we are the last block, save description for the end (should not occur, unless stealth)
  res =. 0 2$a:
else.
  t =. 'This is the part of the fork:',LF,(defstring 0),CR
  select. appearstwice, datapresent
  case. 0 0 do.
    t =. t , 'This block will contain a single intermediate result for the verb after sufficient selections have been made.  The overall result will be shown in the final block of the verb.',LF
  case. 0 1 do.
    t =. t , 'This block contains an intermediate result for the fork.  The overall result is shown in the final block of the fork.',LF
  case. 1 0 do.
    t =. t , 'When a single result-cell is selected, this block will contain the result.',LF
  case. do.
    t =. t , 'This block contains the result.',LF
  end.
   res =. ,: EXEGESISRANKSTACKEXPLAIN;t
end.
res
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked in the rank stack as a ',titlestring
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit;'fork'
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit;'fork'
  end.
  res
else.
  0 2$a:
end.
)

NB. **** hook ****
cocurrent 'dissecthook'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb'
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
NB. Collect token #s in case of error
verb;(coname'');(; 2 {"1 y)
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop {: y
uop =: setvalence__uop 2$y
NB.?lintonly uop =: vop =: <'dissectverb'
NB. dispoperands is set from u
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen (defstring__uop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: endtraverse@:(4 : 0)
titlestring =: 1 fulltitlestring 'hook/'
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
dol =. joinlayoutsl (_1 {. x) traverse__vop travops (TRAVOPSKEEPINLIGHT 0 1 2);TRAVOPSPHYSKEEP;(vopval _1 { selopinfovalid);selopshapes;_1
NB. Calculate the rankhistory to use for u.  The point is to take all the modifiers that apply to the left operand of (u v)
NB. as the rankhistory of u
NB. If (u v) is a monad, we need to take each line, which is title;loc;y and turn it into title;loc;y;y
NB. if (u v) is a dyad, we need to leave each line as title;loc;y;x except that if rankhistory has monad info, make that the x info
if. 3 = {:@$ rankhistory do. rankhistory =: 1 1 2 #"1 rankhistory
NB. If rankhistory has dyad info, look at lines that contain only monad info (i. e. x=a:) and move y to x
else. rankhistory =: (0 1 2 2&{^:(a:-:{:))"1 rankhistory
end.
NB. Then, unless v is a stealthop, wipe out the y column
if. dispstealthoperand__vop -.@e. 1 2 5 6 do. rankhistory =: a: 2}"1 rankhistory end.
NB. If we are displaying the label, put the left-hand label on u
if. displayshowstructmods do. rankhistory =: (< 'hook/' ,~ valence { ' /\') (<_1 0)} rankhistory end.
NB. Use selop0 for x, and selresult for y - but only if selop0 exists, and no travdownuops error
NB. replace the rank with the left rank of the hook, alone.  Preserve previous highlighting from u to the left operand
NB. We keep the highlights from the input, to both the left and right ops.  This will not be needed EXCEPT when v is ][.  Other times,
NB. the highlights go from u to v, and they are at the same sellevel (since u has infinite rank), so nothing that happened below u's sellevel matters.
NB. But if v is ][, then highlights from u (or below) might reach through this node and highlight a higher node - and those nodes may have
NB. lower sellevels, so we need to make sure the lower selections are as they would have been for the omitted ][.  The way to do this is
NB. to start the physreqs with the values that they would have had to start v.
NB. Tell inheritu to add an end-of-computation mark to the display stack
1 inheritu (dol ,~ 0 { x) traverse__uop travops TRAVOPSKEEPALL;(TRAVOPSPHYSCHOOSE 0 _2);((vopval 0 { selopinfovalid) >. (uopval vop));<({. selopshapes),< selresultshape__vop
)

exegesisrankoverall =: 4 : 0
appearstwice =. x
'datapresent endflag linetext' =. y
if. DLRCOMPEND -: linetext do.
  NB. This is the 'end-of-computation' node.  Put out the description
  tit =. (*#titlestring) # ', which started in the block(s) marked in the rak stack as a ',titlestring
  if. datapresent do.
    res =. exegisisrankoverallcompend appearstwice;tit;'hook'
  else.
    res =. exegisisrankoverallnodisp appearstwice;tit;'hook'
  end.
  res
else.
  0 2$a:
end.
)


NB. ***** modifier sequences *****
NB. These are sequences which we will treat and display as a unit
NB. They are detected during parsing and thereafter treated like any other modifier

cocurrent 'dissect'
dissectprimseqindex =: 0 2$a:  NB. list of (locale name;(<list of boxed modifier words)

NB. x is object class(es) to include, default 'dissectobj'.  These classes are put in order
NB. at the top of the search path
NB. The class is erased first, in case there are definitions we need to lose
NB. y is (name of locale);string containing the modifiers that will be handled in this
NB. locale.  Result is the locale name.  Side effect: index extended
NB. MAJOR SIDE EFFECT: locale is changed
modseqlocale =: ''&$: : (4 : 0)
'locname seq' =. y
NB.?lintmsgsoff
dissectprimseqindex_dissect_ =: dissectprimseqindex_dissect_ , locname;seq
cocurrent@(0&cocreate)@([ coerase) newloc =. <locname
NB.?lintmsgson
coinsert x , ' dissectobj'
18!:4 newloc   NB. No named verb from here to the end!
i. 0 0
)

cocurrent 'dissectobj'
modseqlocale 'dissecteach';'&.>'

create =: 3 : 0
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
if. IFQT do. nuvocpage =: 'ampdot#each' end.
coname''
NB.?lintsaveglobals
)

NB. 1 if this verb may not have infinite rank, or if this node displays already
shownilad =: 0:    NB. like "0

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0@]
enparen^:(y=3) (defstring__uop 2) jd '&.>'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')&.>)'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. The result is the DOL, up through the result of u
NB. We do not create a cell; we just traverse u.  There is no visible indication of the rank operator, except in the
NB. frames
traverse =: endtraverse@:(4 : 0)
NB. Set the title string to the actual characters the user used, sorted into order
titlestring =: 0 fulltitlestring ; (<:tokensource) ({ /: [) ;: usersentence__COCREATOR   NB. decr tokensource to account for MARK added
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;TRAVOPSPHYSKEEP;(vopval selopinfovalid);<selopshapes
)

exegesisrankstack =: 3 : 0
select. * L.@> inputselopshapes
case. 0 do.
  t =. 'Each result from execution on an atom is enclosed in a box.',LF
case. 1 do.
  t =. 'Execution on each atom takes place inside its box.',LF
case. 0 0 do.
  t =. 'Each result from execution on atoms is enclosed in a box.',LF
case. 0 1 do.
  t =. 'Each atom of y is removed from its box for the execution, and the result of each execution is enclosed in a box.',LF
case. 1 0 do.
  t =. 'Each atom of x is removed from its box for the execution, and the result of each execution is enclosed in a box.',LF
case. do.
  t =. 'Execution on each atom-pair takes place inside its box.',LF
end.
,: EXEGESISRANKSTACKEXPLAIN;t
)


NB. overrides for calcselect
calcdispframe =: 4 : 0
(1;valence # 1) _2 _1} x calcdispframe_dissectobj_ f. y
)

NB. x is the frame of the full expected result
NB. y is scalar index of a selresult (if we failed, this will be the #valid results)
NB. result is selection for the index: boxed index list of the failing location, in natural order, or empty if no selection
selindextoisf =: 4 : 0
if. selectable do. ;&SFOPEN&.> x selindextoisf_dissectobj_ f. y else. '' end.
)

NB. ******************************** help windows ********************************************
cocurrent 'dissecthelp'
coinsert 'dissect'

ignoredhtml =. (<;._1 ' <strong> </strong>') ,. <''
LFhtml =. (<;._1 ' </ul> </li>') ,. <LF
LFLFhtml =.  (<;._1 ' <h1> <h2> <h3> </h1> </h2> </h3> <p> <ul>') ,. <LF,LF
specialhtml =. (<;._1 ' <li> &gt; &lt; &amp;') ,. ' * ';'>';'<';'&'
htmlrplcs =: ignoredhtml , LFhtml , LFLFhtml , specialhtml
NB. y is HTML, result is a simulation in plain text
htmltoplain =: 3 : 0
NB. Convert whitespace to spaces, remove multiples
ws =. deb y rplc ((TAB,LF,CR) ,. ' ')
NB. Make changes
deb ws rplc htmlrplcs
)

HELP =: 0 : 0
pc help;
xywh 1 1 winsize;cc helptext editm es_autovscroll;
pas 0 0;
)
HELP =: 0 : 0 [^:IFQT HELP
pc help;
minwh winsize;cc helptext edith;set helptext edit 0;set helptext wh _1 _1;
pas 0 0;
)

NB. Called in locale of the help form desired
helpshow =: 3 : 0
NB.?lintonly hwndp =: helpsize =: helptitle =: helptext =: ''
if. #hwndp do.
  wd 'psel ' , hwndp
  wdsetfocus 'helptext'
else.
  NB. Limit starting wh to 80% of screen
  wd HELP rplc 'winsize';": helpsize <. <. 0.8 * 2 3 { ". wd 'qscreen'

  hwndp =: wd 'qhwndp'
  wd 'pn *' , helptitle
  'helptext' wdsettext helptext
  wd 'pshow'
end.
NB.?lintsaveglobals
)

helpclose =: 3 : 0
  if. #hwndp do. wd 'psel ' , hwndp end.
  wd 'pclose'
  hwndp =: ''
)

help_cancel =: help_close =: helpclose

cocurrent 'dissecthelplearning'
coinsert 'dissecthelp'
hwndp =: ''
helptitle =: 'Learning Dissect'
helpsize =: IFQT {:: 200 200;300 400
helptext =: htmltoplain^:(-.IFQT) 0 : 0
<strong>Dissect</strong> displays each verb-execution in a <strong>block</strong> so that you can see its result.  The flow of execution goes generally from top to bottom, with the final result at the bottom of the display.
<p>
Use tooltips to learn Dissect.  Set Tooltips|Detail to 'tutorial', and Tooltips|Delay to 'immediate'.  Move the mouse around your display.
Learn about the components of each block, which are:
<ul>
<li>
Name (for nouns only) - the name of the noun
</li><li>
Rank Stack - the verb (at the bottom of the stack) preceded by modifiers, such as ", that affect its execution.  The rank of each modifier is shown.
</li><li>
Shape line - the shape of the result, split into color-coded parts.  The shape of rhe result-cells is shown against a dark blue background.
The rest of the result shape is the frame of the execution, colored to show which part of the frame was selected by each modifier in the rank stack.
</li><li>
Selection line - if you have selected a portion of a result for analysis, its path is shown beneath the shape.
</li><li>
Error - If execution failed, the block responsible for the error will be flagged.
</li><li>
Result - the result itself
</li>
</ul>
)

cocurrent 'dissecthelpusing'
coinsert 'dissecthelp'
hwndp =: ''
helptitle =: 'Using Dissect'
helpsize =: IFQT {:: 400 600;600 800
helptext =: htmltoplain^:(-.IFQT) 0 : 0
<h1>Flow of execution</h1>
Inputs come in at the top of a block, the output comes out of the bottom.  Execution flows generally from top to bottom.  For blocks such as u^:v and m@.v that have a control input, the control input comes from the right.
<p>
For verbal information about the execution of a sentence, hover over it.
<h2>Display of modifiers</h2>
Each block represents the execution of a verb.  The verb may be a primitive or a named verb, or it may be a <strong>compound verb</strong>, such as u"n, u@v or u/ .
<p>
The last line in the rank stack is a verb that executes to produce a result.  Modifiers that affect the way the verb is applied to its inputs, such as "n or L:n, are shown in a line in the rank stack.
By looking at the ranks in the rank stack, and the color-coding of the shape line, you can see how the input to the verb is broken into cells.
<h2>Invisible modifiers</h2>
Sentence elements that simply control the flow of computation, such as &, @, &., ~, ], [, hook, and fork, are not shown explicitly.  Their effect is taken into account in the way the blocks are connected.
<p>
If a modifier such as "n is applied to a hook or fork, or to u&v, it is shown in the rank stack of each verb it applies to.  For example, in (* % #)"1 the "1 will be shown on the * and # blocks.
<h1>Probing Execution</h1>
To see more about how a result was computed, click on it. This will produce <strong>highlighting</strong>, <strong>selection</strong>, or <strong>expansion</strong>, if there is more to be seen.
<h2>Highlighting</h2>
Clicking on a result that has more than one result-cell will <strong>highlight</strong> that cell, and the arguments that contributed to its calculation.
The result-cell itself is highlighted with a solid black border; the arguments have dashed colored borders.  The color of the argument border matches the selection level of the result-cell.
<h2>Selection</h2>
When a result is created by more than one verb, for example +:@&gt;"1, where the result of +: is also the result of the larger verb +:@&gt; as well as the overall verb +:@&gt;"1, clicking on a result-cell <strong>selects</strong> that cell, in all the verbs that contribute to the result.
The selection chooses a cell of the largest containing verb; clicking again within the selected cell chooses a cell of the next-largest verb, and so on.
Each selected cell is color-coded with a background color that indicates its selection level.  This color matches the highlighting color for its arguments, and also the colors used in the shape and selection lines.
Hovering over the rank stack will show the verbs that selections are made from.
<h3>Hidden Results</h3>
Compound verbs using @, &, and &. operate on cells individually and may produce intermediate results that cannot be assembled into a single result.
For example, in #@&gt; 2;'a' the individual results of > are incompatible.  In such compounds, the verbs before the final result show no values until a single argument-cell has been selected.
<h3>Selection inside boxes</h3>
If the block includes modifiers that look inside the boxing structure, namely each, &.>, L:n, or S:n, selection includes opening any boxes that enclose the actual argument.  Entry into a level of boxing is indicated with '>' at the appropriate point in the selection line.
<h2>Expansion</h2>
Many modifiers, such as u;.1 y, u/ y, or u^:v y, produce many intermediate results on their way to a final result.
Clicking on a result of such a modifier will create a new block, called an <strong>expansion block</strong>.
The overall result of the modifier will be tagged as 'Final', and the expansion block will connect to its input.
Expansion blocks come in two types.
<h3>Expansion for selected results</h3>
<strong>Partitioning modifiers</strong> (u/. u\ u\. u;.) apply the verb u repeatedly, and assemble the individual result into a final overall result.  Computation is <strong>repeated</strong> but not <strong>hidden</strong>.
For these modifiers, the expansion block shows the single application of u that led to the selected result.  You can probe the expansion block to understand this result.

Other modifiers without hidden results, such as m@.v and u :: v, produce single expansion blocks similarly.
<h3>Expansion for intermediate results</h3>
The modifiers u/ and u^:v, as well as recursion $:, perform repeated calculations but show only the result of the last one.  For example, u/ 1 2 3 computes (2 u 3) and then (1 u (2 u 3)).

For these verbs, the expansion block shows all the intermediate results on the way to the final result.  Select one of these intermediate results to see how it was computed, and click further to probe its execution.

The intermediate results are shows as the contents of separate boxes.  When you select a result, the box is opened to allow further probing.  The selection of a intermediate result counts as the first level of selection for the block, and includes a '>' in the shape/selection lines to represent the opening of the selection.
<h2>Sentences containing errors</h2>
When a sentence fails, dissect will automatically select each cell in the path to the error, so that the initial display will show the failing computation.
<h2>Undo and redo</h2>
The buttons at the top of the dissect form allow you to undo and redo selections, or start over in the initial state.
<h1>Display of data</h1>
All arrays are displayed in 2-dimensional form.
<h2>Results with high rank</h2>
A 3-dimensional result is displayed as a list of 2-dimensional arrays.  A 4-dimensional result is displayed as a 2-dimensional array of 2-dimensional arrays, and so on.
Blue lines indicate boundaries between such cells, with the width of the line indicating the rank of the cells it separates.
<h2>Large results</h2>
You can select, under the Sizes menu, the maximum size of the display for a result.  If the result is larger than this maximum, it will be displayed with scrollbars.
If you right-click on a result with scrollbars, a fullscreen window, called an <strong>explorer window</strong>, will pop up providing a larger view.
Selections can be made in either the explorer window or the main dissect form.
Right-clicking in the explorer window will close it.
<h2>Constant nouns</h2>
Results that do not depend on a variable are called constants and are displayed without showing all the detail that created them.  For example, in the sentence
<p>
z + 2 2 $ _1 1 1 0
<p>
the right argument to + will be a single block containing the 2x2 array.  To see the computation that produced the constant, click on the constant's value.
<h2>Fill cells, error cells, and unexecuted cells</h2>
Crosshatching indicates atoms that were added when results of unequal sizes were filled.  Double crosshatching indicates the cell on which an error was detected.
Reverse crosshatching indicates cells that were not executed owing to earlier error.
)

cocurrent 'dissect'
NB. Read the config file at initial load.  Global config variables persist to end of session
loadconfig''



NB. 0!:1 ; <@(LF ,~ '(i. 0 0) [ dissectinstanceforregression_dissect_ 4 : ''destroy__x 0 [ dissect_dissectisi_paint__x 0''^:(0=#@]) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
NB. wd@('psel dissect;pclose'"_)"0 i. 100
runtests_dissect_ =: 0 : 0
('Undissectable sentence: invalid word in sentence: NB' , '..') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect 'select. 2 + 3 NB' , '..'
2 dissect '2+''a'''
2 dissect 'select. 2 + 3 do.'
2 dissect 'select. 2 + 3 do. NB' , '..'
2 dissect '2 + 3 select. NB' , '.. do. NB' , '..'
2 dissect '2 + 3 NB' , '. 4:::'
2 dissect '2,''a'''
2 dissect '2 3+''a'''
2 dissect '1 2 + ''ab'''
2 dissect '1 2 +@+ ''ab'''
2 dissect '1 2 +&+ ''ab'''
2 dissect '1 2 +&+~ ''ab'''
2 dissect '''ab'' +&+ 1 2'
2 dissect '1 2 +@(]"0) ''ab'''
2 dissect '1 2 +@(0:"0) ''ab'''
2 dissect '0 1 2 + 1 2'
2 dissect '+@+ ''a'''
2 dissect '+@{. ''a'''
2 dissect '0 +&+ ''a'''
2 dissect '0 +&+ ''ab'''
2 dissect '0 +&+&> ''a'';''b'''
2 dissect '0 +&:+ ''a'''
2 dissect '''a''+&+ 0'
2 dissect '''ab''+&+ 0'
2 dissect '''a''+&:+ 0'
2 dissect '+&{. ''a'''
2 dissect '+&:+ ''a'''
2 dissect '+&2 (3 4)'
2 dissect '3&* (3 4)'
2 dissect '+&''a'' (3 4)'
2 dissect '(+&2)@:(2&*) 4 6'
2 dissect '3 4 +"1 i. 3 2'
2 dissect '(i. 3 2) +"1 (3 4)'
2 dissect '(i. 3 2) +"1 i. 3 2'
2 dissect '(i. 3 2) +"1 i. 3 1'
2 dissect '(i. 3 2) +"1 i. 1 1'
2 dissect '2 3 +@]&> 5 6'
2 dissect '2 3 +&:+: 4 5 6'   NB. must show agreement error
2 dissect '(i. 3 2) +@]"1 i. 1 1'
2 dissect '(i. 3 2) +@["1 i. 1 1'
2 dissect 'i.@(0&{) ''a'''
2 dissect 'i."0 (1 2)'
2 dissect '+~ i. 2 3'
2 dissect '3 4 +~ i. 2 3'
2 dissect '3 4 +~ i. 3 2'
2 dissect '3 4 +@]~ i. 3 2'
2 dissect '3 4 +@[~ i. 3 2'
2 dissect '3 4 +~ i. 2 3'
2 dissect '3 4 (+ - *) 0 1'
2 dissect '0 1 2 (+ - *) 0 1'
2 dissect '0 1 2 (+ - 0:) 0 1'
2 dissect '0 1 2 (0: - *) 0 1'
2 dissect '0 1 2 (1:"0 - 0:"0) 0 1'
2 dissect '0 1 2 (+ - ]) 0 1'
2 dissect '0 1 2 ([ - -) 0 1'
2 dissect '0 1 2 ([ - ]) 0 1'
2 dissect '0 1 2 (- + * % -)"0 (3 4 5)'
2 dissect '0 1 (+ 0:) ''ab'''
2 dissect '0 1 (+ {.) ''ab'''
2 dissect '0 1 (+ ]) 1 2 3'
2 dissect '(0 1 2 + 0 1"_) 5'   NB. must show agreement error
2 dissect '0 1 2 + '''''
2 dissect '0 1 2 + '' '''
2 dissect '0 (+ - *) '''''
2 dissect '0 (1 2 3 - *) '''''
2 dissect '0 (1 2 3 - *)"0 '''''
2 dissect '2 (+:@+:@+:@+ + ]) 3'  NB. test estheight
2 dissect '2 (+:@+:@+:@+ + +) 3'  NB. test estheight
2 dissect '2 (+ + +:@+:@+:@+) 3'  NB. test estheight
2 dissect '2 ([ + +:@+:@+:@+) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+    + +:@+) + (+:@+:@+:@+:@+ + +   )) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+:@+ + +   ) + (+:@+:@+:@+    + +:@+)) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+:@+ + +:@+) + (+:@+:@+:@+    + +   )) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+    + +   ) + (+:@+:@+:@+:@+ + +:@+)) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+    +~ +:@+) + (+:@+:@+:@+:@+ +~ +   )) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+:@+ +~ +   ) + (+:@+:@+:@+    +~ +:@+)) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+:@+ +~ +:@+) + (+:@+:@+:@+    +~ +   )) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@+    +~ +   ) + (+:@+:@+:@+:@+ +~ +:@+)) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@]    +~ +   ) + (+:@+:@+:@+:@] +~ +:@+)) 3'  NB. test estheight
2 dissect '2 ((+:@+:@+:@]    +~ +:@+   ) + (+:@+:@+:@+:@] +~ +)) 3'  NB. test estheight
2 dissect '2 ([ +:) 3'
2 dissect '2 (] +:) 3'
2 dissect '2 (] ]) 3'
2 dissect '2 ([ ]) 3'
2 dissect '([ +:) 3'
2 dissect '(] +:) 3'
2 dissect '(] ]) 3'
2 dissect '([ ]) 3'
2 dissect '(#@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
2 dissect 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';0' [ z =. 2
2 dissect 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';''q''' [  z =. 2
2 dissect '(1&+@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
2 dissect '(1&+@>)"1 ] 2 2 $ 0;''abc'';''b'';''cd'''
2 dissect '(i.@# ((}.>) ,. ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
2 dissect '(i.@# ((}.>) ,&< ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
2 dissect '(i.@# ((}.>) , ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
2 dissect '0 1 2 3 {~ 2'
2 dissect '(i. 2 3) {~ 2'
2 dissect '(i. 3 2) {~ 2'
2 dissect '('' O'' {~ (] !~ [: i. >:) >/ [: i. [: >./ ] !~ [: i. >:) 8'
2 dissect '1 2 +"_1 0 (1 2)'
2 dissect '1 2 ,"_1 i. 2 3'
2 dissect 'y =. 2 + 5'
2 dissect 'zzz + 5 [ zzz =. 6'
2 dissect '''a b c'' =. i. 3'
'Undissectable sentence: AR assignment to `a b c not supported' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '''`a b c'' =. +`-`%'
'Undissectable sentence: AR assignment to `a b c not supported' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '''`a b c'' =. +&+`-`%'
2 dissect 'r + s [ (''r s t'') =. 0 1 2 [ a =. ''r'';''s'';''t'''
2 dissect '-&.> i. 3'
2 dissect '-&.:> i. 3'
2 dissect '+/ 1 2 3 4 5'
2 dissect '+/ $0'
2 dissect '+/ 1'
2 dissect '+/ 1 2'
2 dissect '(* -)/@> z' [ z =. <@i."0 (3 4 5 6)
2 dissect '+/@> z' [ z =. <@i."0 (3 4 5 6)
2 dissect '+/"1 z' [ z =. i. 4 3
2 dissect '+/"1 z' [ z =. i. 4 1
2 dissect '+/"1 z' [ z =. i. 4 0
2 dissect 'i."0@[/ z' [ z =. 4 3 $ 2 3 4
2 dissect '2([: +/ */) 4'
2 dissect '2 3([: +/ */) 4 5 6'
2 dissect '2 3 4([: +/ */) 4 5 6'
2 dissect '+&.>/ ''a'';0;1;2 '
2 dissect '+&.>/ z ' [ z =. 1;0
2 dissect '+&.>/ z ' [ z =. 1;0;2
2 dissect '+&.>/ z ' [ z =. 'a';0
2 dissect '+&.>/ z ' [ z =. 'a';0;1
2 dissect '+&.>/ z ' [ z =. 2;'a';0;1
2 dissect '+&.>/ z ' [ z =. 1;3;2;'a';0;1
2 dissect '+&.>/ z ' [ z =. $0
2 dissect '+&.>/ z ' [ z =. 'a'
2 dissect 'i./ $0'
2 dissect 'i.&.>/ $0'
2 dissect 'i./"1 i. 4 0'
2 dissect 'i.&.>/"1 i. 4 0'
2 dissect 'i./"1 i. 4 1'
2 dissect 'i.&.>/"1 i. 4 1'
'1 2 3 + y' 4 : '2 dissect x' 4
2 dissect '  (,1) ((}."1~ <:@#@$) ,~"1 ] {~ ({:@$@[ <. <:@#@$@]) <@{."1 [) ,.0 '
2 dissect '2 ([: |: ([ = [: +/ [: ([: |: ] #: [: i. */) 2 $~ ]) #"1 [: ([: |: ] #: [: i. */) 2 $~ ])4'
2 dissect '$@i."1 ]3 + i. 5 2'   NB. select to test vertical resize
2 dissect 'z + > ''a'';1' [ z =. 1
2 dissect 'i.@> z' [ z =. 1 1;(3,.4);6   NB. fills and selections
2 dissect 'i.@> z' [ z =. 1 1;(3,:4);6   NB. fills and selections
2 dissect 'i.@>@> z' [ z =. (1 1;(3,.4);6);<(2;4 2;6,:2)
2 dissect 'i. z' [ z =. 3 1$1 _1 2
2 dissect 'i. z' [ z =. 3 1$1 0.5 2
2 dissect 'i.@> z' [ z =. <@,"0 (1 0.5 2)
2 dissect 'i.@:> z' [ z =. <@,"0 (1 0.5 2)
2 dissect 'i."0 z' [ z =. 0 1 0.5
2 dissect 'i."0 z' [ z =. 0 1 2
2 dissect '+:"0.5 _1 z' [ z =. 0 1 0.5
2 dissect 'a ([ + (+/ % #)@]) z' [ z =. 3 9 6 */ 1 5 9 2 [ a =. 6 5 3
2 dissect '(i. 3) +"1"2 i. 3 4 3'
2 dissect 'i.@> z' [ z =. 1 1;(3 4);'a'
2 dissect 'i.@> z' [ z =. 1 1;(3,:4);'a'
2 dissect '(#@>)"1 ] 5 2 $ ''abc'';''b'';''cd'';''e'';''fg'';0'
2 dissect '(#@>)"1 ] 5 2 $ ''abc'';''b'';''cd'';0;''e'';''fg'''
2 dissect '(#@>)"1 ] 5 2 $ ''abc'';0;''b'';''cd'';''e'';''fg'''
2 dissect '(#@>)"1 |: 5 2 $ ''abc'';''b'';''cd'';''e'';''fg'';0'
2 dissect '(#@>)"1 |: 5 2 $ ''abc'';''b'';''cd'';0;''e'';''fg'''
2 dissect '(#@>)"1 |: 5 2 $ ''abc'';0;''b'';''cd'';''e'';''fg'''
2 dissect '(>:@>)"1 ] 5 2 $ ''abc'';''b'';''cd'';''e'';''fg'';0'
2 dissect '(>:@>)"1 ] 5 2 $ ''abc'';''b'';''cd'';0;''e'';''fg'''
2 dissect '(>:@>)"1 ] 5 2 $ 0;1;2;''cd'';''e'';''fg'''
2 dissect '(>:@>)"1 |: 5 2 $ ''abc'';''b'';''cd'';0;1;2'
2 dissect '(>:@>)"1 |: 5 2 $ ''abc'';''b'';''cd'';0;''e'';''fg'''
2 dissect '(>:@>)"1 |: 5 2 $ ''abc'';0;''b'';''cd'';''e'';''fg'''
2 dissect '(+/ % #)&.:*: i. 3 3 3'
2 dissect '+/ i. 2 4'
2 dissect '+/ i. 3 4'
2 dissect '+/"1 i. 3 4'
2 dissect '+/"1 i. 3 2'
2 dissect '+/@,/"1 i. 3 2'
('Syntax error: invalid sequence Noun Noun',LF,'Error snippet: 3 ''a''') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '3 ''a'''
('Usage: dissect ''sentence''',LF,LF,'Try   dissect ''0'' to see example screen') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect ''
'The sentence to be dissected must be a string.' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect 5 6 7
2 dissect '3 4 5&*"1 i. 5 3'
2 dissect '+/"2 i. 3 4 8'  NB. Failed during selection
2 dissect '+:^:0 (1)'
2 dissect '2 +^:0 (1)'
2 dissect '+:^:1 (1)'
2 dissect '2 +^:1 (1)'
2 dissect '+:^:_1 (1)'
2 dissect '2 +^:_1 (1)'
2 dissect '+:^:2 (1)'
2 dissect '2 +^:2 (1)'
2 dissect '+:^:_2 (1)'
2 dissect '2 +^:_2 (1)'
2 dissect '+:^:0 2 (1)'
2 dissect '2 +^:0 2 (1)'
2 dissect '+:^:_2 2 (1)'
2 dissect '2 +^:_2 2 (1)'
2 dissect '*:^:_ (0.5)'
2 dissect '*:^:__ (0.5)'
2 dissect '*:^:(<_) (0.5)'
2 dissect '*:^:(<__) (0.5)'
2 dissect '+:^:0"0 (1 2 3)'
2 dissect '2 +^:0"0 (1 2 3)'
2 dissect '+:^:1"0 (1 2 3)'
2 dissect '2 +^:1"0 (1 2 3)'
2 dissect '+:^:_1"0 (1 2 3)'
2 dissect '2 +^:_1"0 (1 2 3)'
2 dissect '+:^:2"0 (1 2 3)'
2 dissect '2 +^:2"0 (1 2 3)'
2 dissect '+:^:_2 2"0 (1 2 3)'
2 dissect '2 +^:_2 2"0 (1 2 3)'
2 dissect '+:^:* 0'
2 dissect '+:^:* 1'
2 dissect '+:^:* _'
2 dissect '2 +^:* 0'
2 dissect '2 +^:* 1'
2 dissect '2 +^:(*@]) 0'
2 dissect '2 +^:(*@]) 1'
2 dissect '2 +^:(*@]) _'
2 dissect '0 2 +:@]^:[ 8'
2 dissect '+:^:]"0 (0 1 2)'
2 dissect '2 +^:]"0 (0 1 2)'
2 dissect '3&*^:(100&>)^:_"0 (1 2 3)'
2 dissect '3 *^:(100 > ])^:_"0 (1 2 3)'
2 dissect '<^:]"0 z' [ z =. 1 2 0
2 dissect '+:^:]"0 (0 0.5 1)'  NB. here
2 dissect '(i. 2 3) +:@]^:[ (5)'
2 dissect '(i. 2 3) +:@]^:(+:@[)"0 (5)'
2 dissect '(i. 2 3) +:@]^:(+:@[)"1 (5)'
2 dissect '>:^:0 a:'
2 dissect '>:^:1 a:'
2 dissect '>:L:3 f.@<^:0 1 2 3 4 (5)'
2 dissect '>:L:_3 f.@<^:0 1 2 3 4 (5)'
2 dissect '>:L:_3 f.@<^:1 2 3 4 (5)'
2 dissect '>:L:_3 f.@<^:1 2 3 4 5 (5)'
2 dissect '-:@{:@i.^:8 (12)'
2 dissect 'i."0"1 z' [ z =. 2 2 $ 1 1 1 0.5
2 dissect 'i."0"1 z' [ z =. 2 2 $ 1 1 0.5 1
2 dissect 'i."0"1 z' [ z =. 2 2 $ 1 0.5 1 1
2 dissect 'i."0"1 z' [ z =. 2 2 $ 0.5 1 1 1
2 dissect '+&>/ z' [ z =. 1;2;'a';4;5;6
2 dissect '(] ,~ ([ - ] +/ .* %.)&.|:)&(,:^:(1 = #@$))/&.|: z' [ z =. 3 3 ?@$ 100
2 dissect '1 2 +&+:&(1 = ]) 4 5'
2 dissect '_2 2 +:@]^:[ 8'
2 dissect '(1) 2&+ 5 6 7'
2 dissect '(0) 2&+ 5 6 7'
2 dissect '(_1) 2&+ 5 6 7'
2 dissect '(1 2 3) 2&+ 5 6 7'
2 dissect '(1 2 3) 2&[ 5 6 7'
2 dissect '(<5) +&.> <4'
2 dissect '5 +&.> <4'
2 dissect '(<5) +&.> 4'
2 dissect '5 +&.> 4'
2 dissect '(<2) +&.> <3 4 5'
2 dissect '(100;200) +&.> <"0 i. 2 3'
2 dissect '+/&.> 0 1 2;3 4 5 6'
2 dissect '>:&.>@:i.&.> 3 + i. 4' 
2 dissect '(1&+@>)"1 z' [ z =. 2 2 $ 1 2;3;4;0  NB. interesting selections
2 dissect '(1&+@>)"1 z' [ z =. 2 2 $ 1 2;3;4;'a'  NB. frame highlighting in error path
2 dissect '(3 3 $ 0 1 2 3 4 5 6 7 7.5) ;&:(i."0) 0 1'
2 dissect '(<<"0 i. 6) ,.&.> <"1 <"0 ''abcdef'''
2 dissect '(<<"0 i. 6) ,"0&.> <"1 <"0 ''abcdef'''
2 dissect '(< <"0 i. 3 2) #&.>&.> < ''four five six'' ,.&;: ''one two three'''
2 dissect '(<2) +&.>&.>&.>&.> <^:4 (8)'
2 dissect '(<2) +&.>&.>&.>&.> <^:4 (3 8)'
2 dissect '(<2) +&.> <8'
2 dissect '(<i. 3 2 3) +"2&.> <"1 i. 2 2 3'
2 dissect '>:&.> 1;2;3;''a'''
2 dissect '>:&.> ''a'';1;2;3'
2 dissect '>:&.> 1;''a'';2;3'
2 dissect'((* -> *) -> * -> *.) i:9'  NB. display tester
2 dissect '+:\ i. 4'
2 dissect '+:\\ i. 3 4'
2 dissect '3 +:\ i. 5'
2 dissect '3 +:\ i. 4 5'
2 dissect '4 +:\ i. 4 5'
2 dissect '_3 +:\ i. 5'
2 dissect '5 +:\ i. 4 5'
2 dissect '0 +:\ i. 4 5'
2 dissect '_3 +:\ i. 6'
2 dissect '2 3 +:\ i. 7'
2 dissect '+:\. i. 4'
2 dissect '+:\\. i. 3 4'
2 dissect '+:\.\. i. 3 4'
2 dissect '3 +:\. i. 5'
2 dissect '3 +:\. i. 4 5'
2 dissect '4 +:\. i. 4 5'
2 dissect '_3 +:\. i. 5'
2 dissect '5 +:\. i. 4 5'
2 dissect '0 +:\. i. 4 5'
2 dissect '_3 +:\. i. 6'
2 dissect '2 3 +:\. i. 7'
2 dissect '_3 (_2&(+\.))\. i. 7'
2 dissect '+:/. i. 3 3'
2 dissect '1 1 +//.@(*/) 1 2 1' 
2 dissect '+:/. i. 4'
2 dissect '+:/. i. 0'
2 dissect '1 1 2 3 2 1 4 3 1 2 3 <@,/. ;:''The quick brown fox jumped over the lazy dog and slept'''
2 dissect 'i.\ 0 1 2 3.5'
2 dissect 'i.&.> ]each 0 1 2 3.5'
2 dissect '#&.>\ ;: ''The quick brown fox'''
2 dissect '#&.>/. ;: ''The quick brown fox'''
2 dissect '#&.>\. ;: ''The quick brown fox'''
2 dissect '#&.>\. ''abcd'''
2 dissect '+:;.0 i. 3 3'
2 dissect '(1 2,:2 3) +:;.0 i. 4 5'
2 dissect '(1 3,:2 _2) +:;.0 i. 4 5'
2 dissect '(1 _3,:2 2) +:;.0 i. 4 5'
2 dissect '(1 _3,:2 _2) +:;.0 i. 4 5'
2 dissect '(1,:2) +:;.0 i. 4 5'
2 dissect '<;.1 ''every little thing'''
2 dissect '<;.1 ;: ''a man a plan a canal panama'''
2 dissect '(+/ % #);.1 (3 1 2 3 4 5 3 4 5 6 7 5 4)'
2 dissect '<;._1 ''every little thing'''
2 dissect '<;._1 ;: ''a man a plan a canal panama'''
2 dissect '(+/ % #);._1 (3 1 2 3 4 5 3 4 5 6 7 5 4)'
2 dissect '<;.2 ''every little thing'''
2 dissect '<;.2 ;: ''a man a plan a canal panama'''
2 dissect '(+/ % #);.2 (3 1 2 3 4 5 3 4 5 6 7 5 4)'
2 dissect '<;._2 ''every little thing'''
2 dissect '<;._2 ;: ''a man a plan a canal panama'''
2 dissect '(+/ % #);._2 (3 1 2 3 4 5 3 4 5 6 7 5 4)'
2 dissect '1 0 0 1 0 <;.1 ''abcde'''
2 dissect '1 0 0 1 0 <;.2 ''abcde'''
2 dissect '(1 0 1 0;1 0 1 1 0) +:;.1 i. 4 5'
2 dissect '(1 0 1 0;1 0 1 1 0) +:"1;.1 i. 4 5'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 7'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 7'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 8'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 8'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 9'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 9'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 10'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 10'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 11'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 11'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 12'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 12'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 13'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 13'
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 14'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 14'
2 dissect '(3 4 ,: _2 3) <;.3 i. 10 10'
2 dissect '(3 4 ,: _2 3) <;._3 i. 10 10'
2 dissect '(3 6 ,: 2 3) <;.3 i. 10 10'
2 dissect '(3 6 ,: 2 3) <;._3 i. 10 10'
2 dissect '(3 6 ,: _2 3) <;.3 i. 10 10'
2 dissect '(3 6 ,: _2 3) <;._3 i. 10 10'
2 dissect '(0 4 ,: 2 3) <;.3 i. 10 10'
2 dissect '(0 4 ,: 2 3) <;._3 i. 10 10'
2 dissect '(0 0 ,: 2 3) <;.3 i. 10 10'
2 dissect '(0 0 ,: 2 3) <;._3 i. 10 10'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 7'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 7'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 8'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 8'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 9'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 9'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 10'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 10'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 11'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 11'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 12'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 12'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 13'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 13'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 14'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 14'
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 15'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 15'
2 dissect '(3 4 ,: _2 3) +:;.3 i. 10 10'
2 dissect '(3 4 ,: _2 3) +:;._3 i. 10 10'
2 dissect '(3 ,: 2) <;.3 i. 10 10'
2 dissect '(3 ,: 2) <;._3 i. 10 10'
2 dissect '(0 ,: 2) <;.3 i. 10 10'
2 dissect '(0 ,: 2) <;._3 i. 10 10'
2 dissect '+: L:0 <1 2'
2 dissect '+: L:0 (1 2; 3 4) ; 5 6'
2 dissect '+: L:0 (1 2);''a'''
2 dissect '|.L:0 <^:1 2 3 (0 1 2)'
2 dissect '|.L:1 <^:1 2 3 (0 1 2)'
2 dissect '+: L:0 (1 2);''a'';5 6'
2 dissect '(100;200 300) ,L:0 (0 1);< 2 3 4 ; 1 ;<<5 6 ; 7 8'
2 dissect '(100;200 300) ,L:0 (0 1);< 2 3 4 ; 1 ;<<''a'' ; 7 8'
2 dissect '(<''a'') ,L:1(<0 1);<(<2 3 4);(<1);<<5 6;7 8'
2 dissect '(<''a'') ,L:1(<0 1);<(<2 3 4);(1);<<5 6;7 8'
2 dissect '+: S:0 <1 2'
2 dissect '+: S:0 (1 2; 3 4) ; 5 6'
2 dissect '+: S:0 (1 2);''a'''
2 dissect '|.S:0 <^:1 2 3 (0 1 2)'
2 dissect '|.S:1 a' [ a =. <^:1 2 3 (0 1 2)
2 dissect '+: S:0 (1 2);''a'';5 6'
2 dissect '(100;200 300) ,S:0 (0 1);< 2 3 4 ; 1 ;<<5 6 ; 7 8'
2 dissect '(100;200 300) ,S:0 (0 1);< 2 3 4 ; 1 ;<<''a'' ; 7 8'
2 dissect '(<''a'') ,S:1(<0 1);<(<2 3 4);(<1);<<5 6;7 8'
2 dissect 'a ,S:1 b' [ a =. <'a' [ b =. (<0 1);<(<2 3 4);(1);<<5 6;7 8
2 dissect '>:`<:@.(]"0) 0 1'
2 dissect '+/`0:@.(4<#) i. 2'
2 dissect '+/`0:@.(4<#) i. 3'
2 dissect '+/`0:@.(4<#) i. 8'
2 dissect '(*:@>)`(+:@>)`(-:@>)@.({.@>) 0 1 2;1 3 4;_1 11 12 13'
2 dissect '(*:@>)`(+:@>)`(-:@>)@.({.@>) 0 1 2;''abc'';_1 11 12 13'
2 dissect '2 5 6 (*:>)`(+:>)`(-:>)@.({.@>@]"0) 0 1 2;1 3 4;_1 11 12 13'
2 dissect '2 5 6 (*>)`(+>)`(->)@.({.@>@]"0) 0 1 2;1 3 4;_1 11 12 13'
2 dissect '(* $:@:<:)^:(1&<) 7'
2 dissect '(* <:)^:(1&<) 7'
2 dissect '>:`($:@>)@.(0<L.) 1 2 3;<4 5;6 7'
'domain error: verb@.v' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '>:`($:@.>)@.(0<L.) 1 2 3;<4 5;6 7'
2 dissect '>:`($:&.>)@.(0<L.) 1 2 3;<4 5;6 7'
'dissect restriction: recursion must have the same valence as the original execution' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '>:`(0&$:&.>)@.(0<L.) 1 2 3;<4 5;6 7'
2 dissect '>:`($:&.>)@.(0<L.) 1 2 3;<4 5;<<''a'';6 7'
2 dissect '>L:1 (1);2;3;<<''a'';5'
2 dissect '>L:1 ''a'';<<''a'';5'
2 dissect '>L:1 <<''a'';5'
2 dissect '>:&.> ''a'';5'
2 dissect '>:&.> 5;''a'''
2 dissect '+:&.> 6 12'
2 dissect '+:&.> 6'
2 dissect '>:L:0 <"0 i. 3 4'
2 dissect '>:L:0"0 <"0 i. 4'
2 dissect '>:L:0"0 (1;2;3;''a'')'
2 dissect '>:L:0"0 (1;''a'';3;4)'
2 dissect '>:L:0"0 (''a'';1;3;4)'
2 dissect '>:&.>L:1"0 (1;(<<2);(<<3);(<<''a''))'
2 dissect '>:&.>L:1"0 (1;(<<2);(<<''a''));(<<3)'
2 dissect '>:&.>L:1"0 ((<<''a''));1;(<<2);(<<3)'
2 dissect '(($:@(<#[) , (=#[) , $:@(>#[)) ({~ ?@#)) ^: (1<#) a' [ a =. 20 ? 50
2 dissect '4 1 2 3 +//.@(*/) _1 4 0 2 6'
2 dissect '5 ($: <:)^:(1<]) 6'
2 dissect '+: powconj 4 [ 6' [ ](powconj=:^:)0 (1)
2 dissect '(>: 2 */&i. 3) + (+:@>: i. 2 3)'
2 dissect 'i."0 (2 3) $ 0.5 1 1 1 2 3'
2 dissect 'i."0 (2 3) $ 1 0.5 1 1 2 3'
2 dissect 'i."0 (2 3) $ 1 1 1 2 3 0.5'
2 dissect 'i.&.> (2 3) $ 0.5 1 1 1 2 3'
2 dissect 'i.&.> (2 3) $ 1 0.5 1 1 2 3'
2 dissect 'i.&.> (2 3) $ 1 1 1 2 3 0.5'
'domain error: operands to &: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2&:+ 5'
'domain error: operands to &: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+&:5 (2)'
'domain error: left operand to @ must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2@+ 5'
'domain error: left operand to :: must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2 :: + 5'
'domain error: operands to @: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2@:+ 5'
'domain error: operands to @: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+@:2 (5)'
'domain error: operands to &. must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+&.5 (2)'
'domain error: operands to &. must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2&.+ 5'
'domain error: operands to &.: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+&.:5 (2)'
'domain error: operands to &.: must be verbs' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2&.:+ 5'
'domain error: left operand to ^: must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2^:+: 6'
'domain error: left operand to ^: must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2^:_1 (3)'
'domain error: right operand to ;. must be a noun' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+;.- 2 3'
2 dissect '+:@:+:@+:@+: +: 4 +&+: 5 6 7'
2 dissect '+:^:2"0 i. 5'
2 dissect 'i.^:] ] _2 _1 0 1'
2 dissect '+:^:] ] _2 _1 0 1'
2 dissect '3 </. ''a'''
2 dissect '(<1 23 4) (+&.> 2&(>./\)&.>)~ (<2 3)'
2 dissect '<@i./."2 (0.5) (<1 1 1)} i. 2 3 4'
dissect 2 3 $ 3;(<'base');'qqq+3'  ; 'qqq';0;<,'6'
dissect 2 3 $ 3;(<'base');'qqq+3'  ; 'qqq';0;<6
('Undissectable sentence: non-noun assignment not supported') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect 'a =. /'
('Undissectable sentence: non-noun assignment not supported') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect 'c =. ;.'
2 dissect 't =. 2 2 $ 5'
('Undissectable sentence: non-noun assignment not supported') (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+(a =. /) 3 4 5'
2 dissect '0$a:'
2 dissect '5 (6 $~ 5 + ]) '''''
2 dissect '5 (6 $~ 5 + ])"0 '''''
2 dissect '5 (6 $~ 5 + ])"0 (7)'
2 dissect '5 (6 $~ 5 [ ]) '''''
2 dissect '(+ 2&(>./\))&.>/ pyramid' [ pyramid =. 3;9 2;5 6 12;1 7 8 9;3 1 7 5 3
2 dissect '(+ 2&(>./\))&.>/ pyramid' [ pyramid =. 3;9 2;5 6 12 5;1 7 8 9;3 1 7 5 3
2 dissect '{. (+ 0: ,~ 2: >./\ ])/ m' [ m =. > 3;9 2;5 6 12;1 7 8 9;3 1 7 5 3
2 dissect '{. (+ 0: ,~ 2: crash9_dissect_/\ ])/ m' [ m =. > 3;9 2;5 6 12;1 7 8 9;3 1 7 5 3
2 dissect '(2: crash9_dissect_/\ ])/ 5 + i. 2 3'
2 dissect '(2: crash9_dissect_/\)/ 5 + i. 2 3'
2 dissect '(2: +/\ ])/ 5 + i. 2 3'
2 dissect '(] crash9_dissect_)&.>/ z' [ z =. 1;2 3;4 5 9
2 dissect '2 1 4 +//.@:(*/) 3 _2 3 1'
'dissect error: dissected sentence has incorrect result' (0 0 $ 13!:8@1:^:(-.@-:)) (2;<'check';'all') dissect 't =. t + 2' [ t =. 0
2 dissect '1 2 3 (}.@:+)^:3 i. 3'
2 dissect '(,1)&+/. i. 2 3'
2 dissect 'crash9_dissect_/. i. 3 4'
2 dissect '>L:1 (1);2;3;(<<''a'';5);(<<5 6 7)'  NB. use to check selection of error in L:
2 dissect '(3 6 ,: 2 3) <@:crash9_dissect_;.3 <: i. 10 10'
2 dissect '(3 6 ,: 2 3) <@:crash9_dissect_;.3 >: i. 10 10'
2 dissect '5 ($: <:@crash9_dissect_)^:(1<]) 11'
2 dissect '>:@crash9_dissect_^:1 2 3 i. 2 4'
2 dissect '>^:1 2 3 i. 2 4'
2 dissect 'crash9_dissect_/ 1 2 9 3 4'
2 dissect 'crash9_dissect_"0/ i. 4 2 2'
2 dissect 'crash9_dissect_"0/ z' [ z =. i. 6 2 3
2 dissect 'crash9_dissect_"0/ z' [ z =. i. 6 1 3
2 dissect 'i.@> z' [ z =. 1;2;3
2 dissect 'i.@> z' [ z =. 1;2;'a'
2 dissect 'i.@> z' [ z =. 'a';2;3
2 dissect 'i.@> z' [ z =. 1;'a';3
2 dissect 'i.@> z' [ z =. 1;2;3 4
2 dissect 'i.@>@> z' [ z =. 2 3;(2;3);4
2 dissect 'i.@>@> z' [ z =. 2 3;(2;3);2 2 $2 5 2 3
2 dissect 'i.@>@> z' [ z =. 2 3;(2;3);<<"1]2 2 $2 5 2 3
2 dissect 'crash9_dissect_@i.@>@> z' [ z =. 2 3;(2;3);<<"1]2 2 $2 5 2 3
2 dissect 'crash9_dissect_ :: >:"0 (3 6 9 12)'
2 dissect 'crash9_dissect_@+/ 4 3 3 2 1'
2 dissect 'crash9_dissect_@+/ 0 , 1 , 2 , 3 ,: i. 2 2'
2 dissect 1 [ wd 'clipcopy *' , '3 + 5'
2 dissect ". :: 0: '2 3 + 4 5 6'
2 dissect '<^:(0 > '''' $ ])"0 (1 _1 2)'
'Undissectable sentence: the name ''t'' was previously assigned in this sentence, but assignments are ignored.' (0 0 $ 13!:8@1:^:(-.@-:)) 6 dissect 't + 2 * t =. 4'
'Undissectable sentence: the name ''t'' was previously assigned in this sentence, but assignments are ignored when dissect is called from the debugger.' (0 0 $ 13!:8@1:^:(-.@-:)) 14 dissect 't + 2 * t =. 4'
a (] [ 3 (0 0 $ 13!:8@1:^:(-.@-:)) [) ] ] 6 dissect 'a =: 5' [ 'a b' =. 3 4
a (] [ 3 (0 0 $ 13!:8@1:^:(-.@-:)) [) ] ] 6 dissect '(''a'') =: 5' [ 'a b' =. 3 4
(a,b) (] [ 3 4 (0 0 $ 13!:8@1:^:(-.@-:)) [) ] ] 6 dissect '''a b'' =: 5' [ 'a b' =. 3 4
2 dissect '+:@*:L:0 (1;2;<<3)'
2 dissect '*:@+:@(+:"0)@+: i. 3'
2 dissect '(*:@(+:"0))@+: i. 3'
2 dissect '>:@>:&.>*: i. 3'
2 dissect '*:@:(* +:)@+:^:2 i. 5'
2 dissect '(0 >. <:)^:a: 5'
2 dissect '1 (0 >. -~)^:a: 5'
2 dissect '(-:`(>:@(3&*))`1: @. (1&= + 2&|))^:a: 9'
2 dissect '>:^:-: i. 3'
2 dissect '>:^:crash9_dissect_ 9'
2 dissect '<.@(0.5&+)&.(10&*) 3.14159'
2 dissect '1:`2: @. (2<$) i.10'
2 dissect '${.^: (1 = $)  }:^:(a: = {:) 3 ; i.0'
2 dissect '(i.0)"_/ i. 5'
2 dissect '(i.0)"_/"1 i. 5 2'
2 dissect 'crash9_dissect_@i.@>@> z' [ z =. 2 3;(2;3);<<"1]2 2 $2 5 2 3
2 dissect '$:@:}.^:(2<#) i. 15' 
2 dissect '<"1 z' [ z =. 3 2 3 $ 'a'   NB. click in final result for error
2 dissect ',&.:(<"1)@((] ,"0 1"_1 (1&($:\.)))^:(1<#)) ''abc'''  NB.Click in 'Final' node for error
2 dissect '(0&$);.1 ''abcadabcda'''
2 dissect '(0 2 $ ,:);.1 ''abcadabcda'''
2 dissect '(0 2 $ crash9_dissect_"0);.1 (0 1 0 2 3 0 4 9 0 5 6)'
2 dissect '0 (1 2 3 , ])"0 $0'   NB. $FILL$
2 dissect '0 ([: 1 2 3"0 $)"0 $0'
2 dissect '0 (+ - ]) '''''
2 dissect '0 (1 2 3 - *)"0 $0'
2 dissect '0 (1 2 3 - *)"0 (0)'
2 dissect '0 +@* '''''
2 dissect '0 (+@* - *) '''''
2 dissect '0 (+@* *) '''''
2 dissect '0 (+ *) '''''
2 dissect '([: crash9_dissect_"0 (2) # ,&9)"0 $0'
2 dissect '(3 , ([: crash9_dissect_"0 (2) # ,&9)"0)"1 i. 2 0'
2 dissect '(3 , ([: crash9_dissect_"0 (2) # ,&9)"0)"1 i. 2 0 1'
2 dissect 'crash9_dissect_ 9 , 1 (5 , +)"0 '''''
2 dissect '(i. 2) ([: crash9_dissect_"0 (2) # 9 ,~ ])"0 i. 2 0 1'
2 dissect '(i. 2) ([: crash9_dissect_"0 (2) # 9 ,~ ])"0 :: + i. 2 0 1'
2 dissect '([: crash9_dissect_ 9 , (5 , +)"0) :: (9 $~ ])  '''''  NB. error in fill, then error in u ::
2 dissect 'crash9_dissect_ ([: crash9_dissect_ 9 , (5 , +)"0) :: (9 $~ ])  '''''  NB. error in fill, then error in u ::, then later error
2 dissect '5 (6 + '' '' + 4 , +)"0 i. 2 0'   NB. error during fill-cell calc
2 dissect '5 (6 + 5 + 4 , +)"0 i. 2 0'   NB. No error, result has shape 2
2 dissect '(,5) + 5 (5 + 6 5 4 3 2 1 + 4 , +)"0 i. 2 0'
2 dissect '5 ('' '' + 5 + 4 , +)"0 i. 2 0'   NB. error at end of fillcell calc   $FILL$
2 dissect '2x&*&1&1 (2)'
2 dissect '(i. 2) +:@]"2 i. 2'  NB. inheritance of one-sided rank stack
2 dissect '(i. 2  2 2) +:@]"2 i. 2'
2 dissect '(i. 2  2 2) +:@["2 i. 2'
2 dissect '(i. 2 2 4) +:@["2 i. 2'   NB. Selecting last block fails
2 dissect 'a crash9_dissect_@["2 b' [ a =. 3 + i. 2 2 4 [ b =. i. 2
2 dissect '2x&*&1&1 (3)'
2 dissect '(2&<.#) ''abcde'''
2 dissect '10 ([:, t (+/ . *)^:(<@[` (])) ])1 2 3 4 5' [ t =. i. 5 5
2 dissect '$:@:}.@:(2&|.)^:(2<#) i. 15' 
2 dissect '+:^:(2:`(1+])) 3 4'
2 dissect '+:^:([`(1+])) 3 4'
2 dissect '+:^:(+/`]) 3 4'
2 dissect '+:^:(+/`+:) 3 4'
2 dissect '5 +^:((+/@])`(+:@[)`({.@])) 3 4'
2 dissect '5 +^:({.@]`[`]) 3 4'
2 dissect '5 +^:(]`[`]) 3 4'
2 dissect '5 +^:((+:@[)`({.@])) 3 4'
2 dissect '5 +^:([`]) 3 4'
2 dissect '5 +^:([`]) 3 4'
2 dissect '-^:(1:`(]*:)) 5'
2 dissect '100 -^:(1:`(]*:)) 5'
2 dissect '5 +^:({.@]`[`])"0 (3 4)'
2 dissect '5 6 +^:({.@]`[`])"0 (3 4)'
2 dissect '(+ ])"0 i. 5'
2 dissect '4&(+ ])"1 i. 2'
2 dissect '+ ((&.>)/)(>@:) 6'
2 dissect '+ ((&.>)/)(>@:) 5 6'
2 dissect '(3x (&*) &1) 0 1 2'
2 dissect '((3x&*) (&1)) 0 1 2'
2 dissect '(3x(&*) (&1)) 0 1 2'
2 dissect 'i.@>@> z' [ z =. (<2 3);(,:2;3);<<"1]3 2 $2 5 2 3 2 4  NB. good testcase for selections and display of fill shapes
2 dissect 'i."0"1(2 2 $ 1 4 1 8)'
2 dissect '+: each 1;2;1'
'dissect error: dissected sentence has incorrect result' (0 0 $ 13!:8@1:^:(-.@-:)) (2;<'check';'all') dissect '?~ 100'
(2 ;< 'check';'shape') dissect '?~ 100'
'dissect error: dissected sentence has incorrect result' (0 0 $ 13!:8@1:^:(-.@-:)) (2 ;< 'check';'shape') dissect '(100 ?@$ 3) { 5;''a'';<a:'
(2 ;< 'check';'error') dissect '(100 ?@$ 3) { 5;''a'';<a:'
'dissect error: dissected sentence has incorrect result' (0 0 $ 13!:8@1:^:(-.@-:)) (2 ;< 'check';'error') dissect 'crash9_dissect_ ctup =: ctup + 1' [ ctup =: 7
ctup = 9
(2 ;< 'check';'no') dissect 'crash9_dissect_ ctup =: ctup + 1' [ ctup =: 7
ctup = 8
'Invalid value for ''check'' option.' (0 0 $ 13!:8@1:^:(-.@-:)) (2 ;< 'check';'xxx') dissect '?~ 100'
2 dissect '4 (1)} i. 5'   NB. no error
2 dissect '4 (1)} i. 5 6'  NB. no error
2 dissect '4 (<,:0 1)} i. 5 6'
2 dissect '5 (<<<<1)} i. 5 6'
2 dissect '5 (<<,<1 2)} i. 5 6'
2 dissect '5 (<0 1 2)} i. 5 6'
2 dissect '5 (6)} i. 5 6'
2 dissect '5 (<6)} i. 5 6'
2 dissect '5 (<<6)} i. 5 6'
2 dissect '5 (<,<6)} i. 5 6'
2 dissect '5 (,<,<6)} i. 5 6'
2 dissect '5 (<4 6)} i. 5 6'
2 dissect '5 (<5 4)} i. 5 6'
2 dissect '5 (<4)} i. 5 6'
2 dissect '5 (<4;5)} i. 5 6'  NB. no error
2 dissect '5 (<4;6)} i. 5 6'
2 dissect '5 (<5;5)} i. 5 6'
2 dissect '5 (<2 5;5)} i. 5 6'
2 dissect '5 (<(<2 5);5)} i. 5 6'
2 dissect '5 (<(<2 4);5)} i. 5 6'  NB. no error
2 dissect '5 (<(<2 4);<<5)} i. 5 6'  NB. no error
2 dissect '(i. 4) (<(<2 4);<<5)} i. 5 6'
2 dissect '(i. 5) (<(<2 4);<<5)} i. 5 6'  NB. no error
2 dissect '(i. 3 5) (<(<2 4);<<5)} i. 5 6'  NB. no error
2 dissect '(i. 2 5) (<(<2 4);<<5)} i. 5 6'
2 dissect '5 (<(<2 4);<<6)} i. 5 6'
2 dissect '5 (<(<2 4);<<_6)} i. 5 6'  NB. no error
2 dissect '5 (<(<2 4);<<_7)} i. 5 6'
2 dissect '5 (<(<2 4);<<2 _7)} i. 5 6'
2 dissect '5 (1;1 2)} i. 5 6'
2 dissect '100 200 (2 3;1 2)} i. 5 6'  NB. no error
2 dissect '5 ((<2 3);<<<1 2 4)} i. 5 6'   NB. no error
2 dissect '5 ((<2 3);<<<0 1 3 4)} i. 5 6'
2 dissect '5 (<<2 3,:1 2)} i. 5 6'  NB. no error
2 dissect '5 (<(2 3,:1 2);3)} i. 5 6'  NB. no error
2 dissect '(100*i.4) (<(2 3,:1 2);3)} i. 5 6'
2 dissect '(100*i.2 2) (<(2 3,:1 2);3)} i. 5 6'  NB. no error
2 dissect '5 (<a:;(2 3,:1 2))} i. 5 6'  NB. no error
2 dissect '(100 * i. 2 2) (<a:;(2 3,:1 2))} i. 5 6'  NB. no error
2 dissect '(100 * i. 5 2 2) (<a:;(2 3,:1 2))} i. 5 6'  NB. no error
2 dissect '5 6 (1)} i. 5 6'
2 dissect '5 6 (1 2)} i. 5 6'
2 dissect '5 6 (<a:;1 2)} i. 5 6'  NB. no error
2 dissect '5 6 (<1;1 2)} i. 5 6'  NB. no error
2 dissect '5 6 (<(,1);1 2)} i. 5 6'  NB. no error
2 dissect '5 6 (<3 1;1 2)} i. 5 6'  NB. no error
2 dissect '5 (0 1)} i. 5 6'  NB. no error
2 dissect '5 6 (0 1)} i. 5 6'
2 dissect '5 6 (0 1)} i. 5'   NB. no error
2 dissect '(2 2;4 4) (1:`[`])} i. 3 4 5'
2 dissect '(2 2;1 1) (1:`[`])} i. 3 4 5'  NB. no error
2 dissect '1 (2 2;1 1)} i. 3 4 5'  NB. no error
2 dissect '(2 2;1 1) ((i.@{:@$@])`[`(}:@]))} i. 5 4 5'  NB. no error
2 dissect '(2 2;1 1) ((i.@{:@$@])`[`(*:@]))} i. 5 4 5'  NB. no error
2 dissect '5 +`0:} i. 6'
2 dissect '0} i. 4'  NB. no error
2 dissect '0 2 4 _3} i. 5 4'   NB. no error
2 dissect '0} i. 5 4'
2 dissect '1 0.5} i. 5 2'
2 dissect '0 2} i. 5'
2 dissect '0 2 3 2} i. 3 4'
2 dissect '0 2 _4 2} i. 3 4'
2 dissect '0 2 _3 2} i. 3 4'   NB. no error
2 dissect '{.`]} i. 3 3'
2 dissect '{.`*:} i. 3 3'
2 dissect '(2 2;1 1) ((i.@$@])`[`*:)} i. 5 4 5'
2 dissect '(2 2;1 1) ((i.@$@])`[`(*:~@]))} i. 5 4 5'
2 dissect '5 [: 6'
2 dissect '5 +:@]^:(+:@+`(2:@])`([:)) i. 5 6' 
2 dissect '5 +:@]^:(+:@+`(2:@])`(*:)) i. 5 6'
2 dissect '5 (*: ] +) 6'
2 dissect '2: i. 5'
2 dissect '2:"0 i. 5'
2 dissect '(2:"0 * +:) i. 5'
2 dissect '(2: * +:)"0 i. 5'
2 dissect '1 2 3 4 5 (2: * >.)"0 i. 5'
2 dissect '2"_ i. 5'
2 dissect '2"_"0 i. 5'
2 dissect '(2"_"0 * +:) i. 5'
2 dissect '(2"_ * +:)"0 i. 5'
2 dissect '(5 * 7)"_ i. 5'
2 dissect '(5 * 7)"_"0 i. 5'
2 dissect '((5 * 7)"_"0 * +:) i. 5'
2 dissect '((5 * 7)"_ * +:)"0 i. 5'
2 dissect '2 vb i. 5' [ *(vb =. "_) 4
2 dissect 'z"_"0 i. 5' [ z =. 6
2 dissect 'z"_ i. 5' [ z =. 6
2 dissect '(i. - +:@{.) 10 5'
2 dissect '+:^:z 5' [ z =. 1
2 dissect '0:^:z 5' [ z =. 1
2 dissect '0:^:z 5' [ z =. 0
2 dissect '0:"0^:z 5' [ z =. 1
2 dissect '0:"0^:z 555555' [ z =. 1
2 dissect '+:`*:/. i. 3 3'
2 dissect '*:`crash9_dissect_/. i. 4 4'
2 dissect '+:`*:\. i. 4'
2 dissect '2 +:`*:\. i. 4'
2 dissect '+:`*:`%:/. i. 4'
2 dissect '2 +:`*:`%:\. i. 4'
2 dissect '(<@:+:)`(<@:*:);.1 (1 3 1 4 5 1 6 7)'
2 dissect '1 0 0 0 1 0 0 0 (<@:+:)`(<@:*:);.1 (1 3 1 4 5 1 6 7)'
'domain error: in u;.n, n must be an atom' (0 0 $ 13!:8@1:^:(-.@-:)) 6 dissect '<;.(,0) i. 3 3'
'domain error: in u;.n, n must be one of _3 _2 _1 0 1 2 3' (0 0 $ 13!:8@1:^:(-.@-:)) 6 dissect '<;.(0.5) i. 3 3'
'domain error: gerund u not supported for u;.3 and u;._3' (0 0 $ 13!:8@1:^:(-.@-:)) 6 dissect '+:`*:`%:;._3 i. 10 10'
'domain error: in x u/ y, u must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 6 dissect '3 4 +:`*:/ i. 6'
2 dissect '+:`*:/ i. 5'
2 dissect '+`*/ i. 5'
2 dissect '+`(+ *)/ i. 4 5'
2 dissect '+`*/ i. 0'
2 dissect '+`*/ i. 0 4'
2 dissect '*`crash9_dissect_/ i. 10'
2 dissect 'crash9_dissect_`*/ i. 10'
2 dissect '*`+/ i. 10'
(2 ;< 'title';'0',TAB,'Demo') dissect '3 + 5 + 7 + 9'
(2 ;< 'title';'0',TAB,'Demonstration Title') dissect '3 + 5'
(2 ;< ('title';'0',TAB,'Demonstration Title'),:('title';'-4',TAB,'subtitle')) dissect '3 + 5'
(2 ;< 'link';'0',TAB,'Demo',TAB,'http://code.jsoftware.com/wiki/NuVoc') dissect '3 + 5 + 7 + 9'
(2 ;< 'link';'2',TAB,'Demo link with extremely long name',TAB,'http://code.jsoftware.com/wiki/NuVoc') dissect '3 + 5 + 7 + 9'
(2 ;< ('link';'0',TAB,'Demo',TAB,'http://code.jsoftware.com/wiki/NuVoc'),:('link';'0',TAB,'Demo link with extremely long name',TAB,'http://code.jsoftware.com/wiki/NuVoc')) dissect '3 + 5 + 7 + 9'
2 dissect '1 0 0 0 _4 0 0 1 <;._2 ''abcdefgh'''
2 dissect 'if. 3 + 5'
2 dissect '3 + 5 end. 0'
2 dissect '3 + 5 end. 0 end.'
6 dissect 'if. r =. +/ 1 2 3 do. a =. 5 end.'
6 dissect 'if. 0 = #r =. +/ 1 2 3 do. a =. 5 end.'
2 dissect '+&.> 1 2 3 ; 4 5 6 7'
2 dissect '__&".@>&.> z' [ z =. ('1';'23';'5') ; ('')  ;< ('345';'67')
2 dissect '''abc'' ;._2 ] 1 2 3'
2 dissect '''abc'' <;._2 ] 1 2 3'
2 dissect '''a'' ]\ i. 5'
2 dissect '''a'' ]\. i. 5'
2 dissect '1 0 0 0 <;._2 ] 1 2 3'
2 dissect '(1 0 0;0) <;._2 ] 1 2 3'
2 dissect '''ab'' <;.0 i. 3 4'
2 dissect '''ab'' <;.3 i. 3 4'
(2 ;< 'datasize';'10 20') dissect 'i. 5 100'
(2 ;< 'datasize';'10') dissect 'i. 100 100'
2 dissect '+:L:0 z' [ z =.  <<<4 5
2 dissect '1!:1 <''xxx'''
2 dissect '0 1 2 -: _2 (3!:4) 2 (3!:4) i. 3'
2 dissect '6 (34 b.) 34 b. _5 (33 b.) 33 b. 4 (32 b.) 32 b. 512 (27 b.) 26 b. #. 1 0 0 0 (6 b.) _4 b. 1 0 1 1'
2 dissect '3 : ''+: y'' i. 2 3'
2 dissect '+: : [: 5'
2 dissect '4 [: : * 5'
2 dissect '1 2 + : * i. 3 3'
'domain error: left operand of !. must be a verb' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '2!.3'
'domain error: right operand of !. must be a noun' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '>.!.>. 5'
'domain error: !. not supported' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '+:@+:!.0 (5)'
2 dissect '3 {.!.4 '''''
2 dissect '1 2 1 1 3 </.!.0 i. 5'
2 dissect '1.00000000000001 2 1 1 3 </.!.0 i. 5'
2 dissect '(($0);1 0 1 1 0) +:;.1 i. 4 5'
2 dissect '(($0);1 0 1 1 0) crash9_dissect_@>:;.1 i. 4 5'
2 dissect '(($0);1 0 1 1 0) crash9_dissect_@:(2&+);.1 i. 4 5'
2 dissect '(z) =: i. 3' [ z =. 'a b c'
2 dissect 'n_loc_ =. i. 3'
2 dissect '''n_loc_ n'' =. i. 3'
2 dissect 'nn =. i. 3'
2 dissect 'n =: i. 3'
2 dissect '(z) =: 3' [ z =. ;:'a b__ c'
2 dissect '(;:^:_1 z) =. <"0 i. 3' [ z =. ;:'a b__ c'
2 dissect '(z) =. i. 3' [ z =. 'a b c'
2 dissect '(z) =. a' [ z =. 'p q r' [ a =. i. 3
2 dissect 'q =. 2 + (z) =: i. 3' [ z =. ;:'a b c' [ a =. '5'
2 dissect 'q =. 2 + (z) =. i. 3' [ z =. ;:'a b c' [ a =. '5'
2 dissect 'sesquipedalianapparatchikiindeterminacy =: 5'
2 dissect '''sesquipedalianapparatchikiindeterminacy extra'' =: 5'
2 dissect '2 + d =. '' '' + 5'
2 dissect '2 + ''d e'' =. ,5'
3 : '2 dissect ''z =: 5'' [ z =. 3' ''
3 : '2 dissect ''''''z zz'''' =: 5'' [ z =. 3' ''
3 : '2 dissect ''z__ =: 5'' [ z =. 3' ''  NB. ok
3 : '2 dissect ''''''z z__'''' =: 5'' [ z =. 3' ''
2 dissect 'name__loc =: 5' [ loc =: 'abc'
2 dissect '''a name__loc'' =: 5' [ loc =: 'abc'
2 dissect '0 2 4 { i. 7 8'
2 dissect '(0;1;2) { i. 7 8'
2 dissect '(0 3;1 4;2 2) { i. 7 8'
2 dissect '(0 3;1 4;<3 2;2 4) { i. 7 8'
2 dissect '1 0 { i. 2 3 4 5'
2 dissect '(3;<a:;2) { i. 4 5'
2 dissect '(<a:;2) { i. 4 5'
2 dissect 'a: { i. 4 5'
2 dissect '($0) { i. 4 5'
2 dissect '2 {"1 i. 4 5'
2 dissect '3 2 {"1 i. 4 5'
2 dissect '(''[]'' -&(+/\)/@:(=/) ])''[[]][]'''
2 dissect '1 2 =&(+/) 1 2 3 4 5 6'
2 dissect '3 + 5 N','B. comment'
2 dissect 'a taketo&.> c' [ a =. ;:'a b c' [ c =. 'cba';'def';2
2 dissect '''a''&taketo&.> c' [ c =. 'cba';'def';2
2 dissect '(,''a'')&taketo&.> c' [ c =. 'cba';'def';2
2 dissect '''abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ''&i. ''bc'''
2 dissect '3 * 1 : ('':'';''x u y'') 5'
2 dissect '*: 1 : ''u y'' 5'
2 dissect '+ (1 : ('':''; ''(((#~LF-.@e.])5!:5<''''u'''');,.y),.({.;}.)":x,y u/x'')~) 1 2 3'
2 dissect 'testtacit_dissect_ 5 6 7'
2 dissect 'toupper ''abc'''
2 dissect 'toupper_base_ ''abc'''
2 dissect 'testmonad_dissect_ 4 5'
2 dissect '2 testdyad_dissect_ 4 5'
2 dissect 'testbivalent_dissect_ 4 5'
2 dissect '6 testbivalent_dissect_ 4 5'
2 dissect 'testmonadh_dissect_ 4 5'
2 dissect '2 testdyadh_dissect_ 4 5'
2 dissect 'testbivalenth_dissect_ 4 5'
2 dissect '6 testbivalenth_dissect_ 4 5'
2 dissect 'testmonadh__z 4 5' [ z =. <'dissect'
2 dissect 'a ((i. 2 10) ]~ [:);.3 b' [ b =. i. 3 1 [ a =. 2 2$1 1 3 3
2 dissect '((i. 5 3) crash9_dissect_@+ -)"1 i. 5 5'
2 dissect 'i./ 0$0'
2 dissect '(i./ # [:) 0$0'
2 dissect '5 ]"]\ 3'
2 dissect '5 >\ 3'
2 dissect '5 >\ a:'
2 dissect '5 ]"]\ a:'
2 dissect '2 ]"]\ 3'
2 dissect '2 >\ 3'
2 dissect '2 >\ a:'
2 dissect '2 ]"]\ a:'
2 dissect '1 ]"]\ 3'
2 dissect '1 >\ 3'
2 dissect '1 >\ a:'
2 dissect '1 ]"]\ a:'
'dissect restriction: in m :n, numeric n is not supported' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : 0'
'domain error: in m :n, numeric n must be an atom' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : 0 1'
'domain error: in m :n, numeric n must be 0' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : 5'
'domain error: in m :n, n must be boxed, literal, or fixed-length numeric' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : 2r2'
'rank error: in m :n, literal n must be a table or list' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : a' [ a =. 2 2 2 $ 'a'
'dissect restriction: in m :n, m must be a constant or a name' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '(0-0) : 0'
'rank error: in m :n, boxed n must be an atom or a list' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : a' [ a =. 2 2 $ <'a'
'rank error: in m :n, boxed n must have contents with rank < 2' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : a' [ a =. <2 2 2 $ 'a'
'domain error: in m :n, boxed n must contain strings' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '0 : a' [ a =. <5
'ill-formed number: 1xcv' (0 0 $ 13!:8@1:^:(-.@-:)) 2 dissect '1xcv'
(2 ;< 'check';'no') dissect '(+ - (1 : ''`u'') `:6)1j1'
'dissect restriction: an explicit modifier must return a verb' (0 0 $ 13!:8@1:^:(-.@-:)) (2 ;< 'check';'no') dissect '(+ (+ 1 : ''~'')) 4'
2 dissect '(1 1 1,:1 1 1) <;.3 i. 3 3'
2 dissect '''a'' 3;.1 i. 5'
2 dissect '5@.] 0'
2 dissect '5`6@.] 0'
2 dissect '];.0"1 i. 2 5'
2 dissect '(2 2$3) ((<1 1)&{);._3&.(2 0 1&|:) i. 7 10 3'
2 dissect '".@''a'' 0' [ a =. 'abcde'
2 dissect '5 ".@''a'' 0' [ a =. 'abcde'
2 dissect '''abc'' + :: (i. 6) 1 2 3'
2 dissect '+: :: (i. 6) ''abc'''
2 dissect '</. s: ;: ''zero one two three four five'''
2 dissect '1 1+&(1 1&([/.)) 1 1'
2 dissect '2 4 crash9_dissect_@(3&*)@(]\) i. 5'
2 dissect '3 ([#~ (#$ 1{.~-))~ i.8')

testtacit =: testtacit2"0
testtacit2 =: *: + -:

testmonad =: 3 : 0
i. #y
)

testdyad =: 4 : 0
x + i. y
)

testbivalent =: 3 : 0
*: i. #y
:
x * *: i. #y
)

testmonadh =: 3 : 0"0
i. #y
)
testmonadh =: 3 : 0"0
y =. >: y
i. #y
)

testdyadh =: 4 : 0"0
x + i. y
)

testbivalenth =: 3 : 0
*: i. #y
:
x * *: i. #y
)

testsandbox_dissect_ =: 3 : 0
1 testsandbox y
:
vn =. 1 2 3
vn_dissect_ =: 'abc'
va =. &.>
vc =. &
vv =. 3 : ('y =. y + 1';'y =. y + 2')&.>
sentence =. 'vv y vc + va vn'
arg =. ,: x;(coname'');sentence
arg =. arg , 'vn';0;vn
arg =. arg , (,'y');0;y
arg =. arg , 'va';1;5!:5 <'va'
arg =. arg , 'vc';2;5!:5 <'vc'
arg =. arg , 'vv';3;5!:5 <'vv'
dissect arg
)

crash9 =: ([ [ 13!:8^:]@(9 e. ,))"0

3 : 0 ''
tv_a_ =: +
mv_a_ =: 3 : ('y + 5')
dv_a_ =: 4 : ('2 * x';'x + y')"0
bv_a_ =: 3 : ('2 * y';':';'2 * x';'x * y')
cocurrent 'b'
('a';'z') copath <'b'
l =: <,'a'
k =: <,'d'
cocurrent 'c'
l =: <,'a'
k =: <,'b'
tv_d_ =: +:
''
)
0 : 0
findnameloc_dissect_ 'tv',&< <,'a'
findnameloc_dissect_ 'tv',&< <,'b'
findnameloc_dissect_ 'tv__l',&< <,'b'
findnameloc_dissect_ 'tv__l',&< <,'c'
findnameloc_dissect_ 'tv__k__k',&< <,'c'
findnameloc_dissect_ 'tv__k__k',&< <,'b'
)



0 : 0  NB. Testcases that fail
)

0 : 0
alltests''
0!:2 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_paint__y 0''^:(''''-:$) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_dissect_
testsandbox_dissect_ 1
)

Note  'Overview of dissect'

Operation follows this sequence:

0. Initialization of environment
1. Parsing; creation of sentences to execute and locales for analysis
2. Execute the sentence
3. Analyze the results
4. Display the picture

After the display, user interaction proceeds with
5. Selection
6. Expansion nodes
7. Highlighting
8. Hovering

0. Initialization
Each invocation of dissect runs in its own locale, which is an instance of 'dissect'.  Private names
in the user's namespace are all collected, along with their values and the sentence to be executed,
and passed into 'parsemain' for analysis.  Multiple active dissection do not interfere with each
other except for a few globals that are deemed user-specific rather than dissection-specific, font size being
an example.

1. Parsing
Parsing follows the J parsing rules.  Each word is given a locale, and given the locales of its arguments.
All such locales have COCREATOR set to the dissect instance for the sentence.  When a monad or dyad execution
in encountered, setvalence is called to select the valence for each primitive.

After parsing, the sentence is represented in the parse tree made up of the locales and their interconnections.
The entry point (defstring) creates the string form of the executed sentence (possibly omitting some assignments)
and the entry point (exestring) creates a form that includes code to save every result as created by each verb.

2. Execution
The uninstrumented and instrumented versions are both executed to make sure the results agree.
For the most part, results rather than inputs are saved, though in a few cases the inputs must also be saved to
reconstruct the operation.  The actual executed verb is saved too, so we can get its ranks.  The results are
saved in the name 'logvalues' in each primitive instance.  Each result is given a sequential
result number (called the logticket) among all the results of the sentence; the sequence of result numbers suffices to establish
the order of execution.

3. Analysis
The stored results are analyzed by a traversal through the parse tree.  This single traversal combines
top-down and bottom-up aspects.  The input to the traversal of each node is the locales of the displayed
arguments, and the selection information coming from downstream results; the result of the traversal is the
locale in which the result of the node will be displayed.

a. travdowncalcselect
The first step of traversal is to figure out which result-cell(s) have been selected for display, and which logticket
numbers correspond to inputs to those cells.  This involves looking at the user selections, the frames of the
arguments, and the ranks of the verbs.  The special details of each primitive affect this computation, but the
overall framework is in the verb (travdowncalcselect) which calls a number of sub-verbs that can be replaced in
each primitive locale as needed.  travdowncalcselect sets a couple of dozen globals that are used for calculating the
display and in specifying highlighting and selection for the arguments when they are traversed.

b. traverse components
After travdowncalcselect has figured out the results and inputs to the computation at a node, the nodes that
contribute to it are traversed.  For example, consider u@v.  u@v has its own node, at which selections are allowed.
After u@v has called travdowncalcselect, it traverses v, and then traverses u, passing in the result of v.  Each
of these traversals can apply its own selections.

c. joinlayouts
The result of the traversal is a layout, which is just the locale number of a displayable result.  Creating a layout involves
saving globals used for display (such as the rank and caption information), and saving the wiring information about how the
nodes should be connected.  The node creates its layout when it has sufficient information.

d. inheritu
Not every primitive gets a display node.  For example, hooks/forks have no display node, and in u@v there are display nodes
for u and v, but not for the composite verb; similarly for u"n.  Nevertheless the primitive IS an important entity, because it
has a rank and can be a point of selection.  Thus, the display block of u is shared between u and u@v: the first click
for selection is sent to u@v, and the next to u.  Any number of nodes can share a display block.

The verb (inheritu) is used to combine the display of u into that of u@v.  It is called by any primitive that needs
display-block sharing.

e. error autoselection
If a sentence has an error, there is an extra first traversal (without display) to find out where the error is.  If at any
point a primitive produces too few results, an error is assumed, and a selection is performed as if the user had selected
the error cell.

4. Display
At the end of traversal the final result will have a layout assigned.  At that point the display can be created.  This is
done in two passes.  First, the locales in the display are traversed, and the information to be displayed is calculated and
sized.  This establishes the screen dimensions of each node.  The nodes are then laid out on the screen so that the flow
of information goes top to bottom.

After the screen has been laid out, the wiring is inserted.  This uses a grid-based router.
The procedure takes two passes.  In each, the networks, each comprising a source
and multiple destinations, are routed one by one.  In the first pass, the routing penalizes invalid routes (where two lines
lie on top of each other in the same direction), but does not forbid them.  If the layout requires an invalid route, the
layout is expanded by adding space at the trouble point; these trial routes are repeated until a valid placement is
found.  The result of the last placement becomes the final route.

5. Selection
Each locale, representing one primitive, has a "selection level" (sellevel) and a series of "selections" (selections).  The
locale is "selectable" if it has more than one result-cell.  Left-clicking in a selectable result causes the selected cell to
be calculated, and an additional box added to the selections in its locale and all other locales that depend on the selected
locale.

Since a single display block may contain the results of multiple verbs (for example, u@v y displays the result of v in the
same block as the result of u@v), a click in a shared result must be sent to the proper verb.  The click is taken by the
verb with the lowest selection level that has not been selected yet with the same value.  Thus a click
in the result of u@v will choose a result of u@v.  A subsequent click in that same result-cell of u@v will select a result-cell
of v; but a click in a different result-cell of u@v will change the selection of u@v.

After a selection has been made, the display is retraversed and redisplayed, since it may change radicaly.

6. Expansion nodes
Complex modifiers that execute operands multiple times require additional display nodes to show the results from the different
executions of the operands.  These verbs are initially displayed giving only the overall result of the modifier; but if the
user clicks in that result, they create additional display blocks to show the intermediate calculations.  The intermediate
blocks end in an "expansion block" which allows selection among the results of the operand; blocks feeding into the
expansion block detail the innards of the operand.  When an expansion block is displayed, the overal result of the verb is
marked with the prefix "Final " to indicate the presence elsewhere of the expansion.

7. Highighting
Selection changes the color of result-cells as they are selected.  The argument-cells that
contributed to a selected result are "highlighted" with a color-coded outline box that shows which cells were used.
Highlighting is intricate, because it must pass through sequential modifiers to find the noun argument that contributed
to a result.  During the traversal, each node is given references to the locales that contain the display of the arguments.
The selection information for each argument is accumulated in the display locales.  Each modifier adds to the
selection information, giving the path to the cells selected by the modifier.  To calculate the displayed highlight,
the selections are processed in turn, starting with the map of the original noun, and creating a series of sets of
selected cells.  The cells that are selected at each selection level are then highlighted with the appropriate color.

8. Hovering
Hovering is the main method of informational interaction with the user.  Hovering over a cell results in a timer event which
figures out what cell is hovered over.  Then a complete analysis of the cell is calculated, by looking at overall information
about the display block and also primitive-specific information.  This is all collected into a set of sentences, each of which is
classified with a semantic code.  The sentences are then culled according to the user's desired tutorial level, ordered
according to a grammar that orders the semantic codes, processed to resolve internal references such as "also" and number, and
then reflowed to fit the screen size.  The result is displayed as a tooltip.
)
