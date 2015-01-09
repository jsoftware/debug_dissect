NB. Copyright (c) Henry H. Rich, 2012-2014.  All rights reserved.

locales =. 'dissect'&,&.> ('' ; ;: 'obj extendv monad dyad recursionpoint noun verb assign vandnm vandnmdyad fork hook allnouns righttoleft irregularops powerexpansion displaytwo selectshape each') , 'partition'&,&.> ''; ;: 'selector nadverb conjunction'
NB. Clear definitions of old locales and create anew.  This will remove hangover definitions. These locales can be small since they hold mostly verb-names
NB. The 2 1 gives the name-table sizes: 2 1 0 0 0 ...
NB. The dissectionlist thing is to preserve the list over reloads, for debugging.
NB. Don't delete a locale that we have switched to, to prevent interaction unpleasantness
3 : 'dissectionlist_dissect_ =: d [ ((cocreate ([ coerase))"0~   2 1 {.~ #) y [ d =. ".''dissectionlist_dissect_''' (coname'') -.~ locales

NB. DISSECTLEVEL is updated from time to time whenever there is a change to an external interface, indicating the dissect release level
NB. at the time of the change
DISSECTLEVEL_dissect_ =: 3 7

NB. set ALLOWNONQTTOOLTIP to enable tooltips for J6 (they are always on in JQT).  In J6 tooltips
NB. take over the timer interrupt
ALLOWNONQTTOOLTIP_dissect_ =: 1

NB. obsolete NB. set SINGLESELECTION to clear previous selections when a new selection branch is opened
NB. obsolete SINGLESELECTION_dissect_ =: 1
NB. obsolete 
NOCLEANUP_dissect_ =: 0  NB. set to 1 for debugging to allow postmortem
DEBPARSE_dissect_ =: 0   NB. set for parser printout
DEBTRAVDOWN_dissect_ =: 0   NB. set for travdown printout
DEBHLIGHT_dissect_ =: 0   NB. set for highlight printout
DEBHLIGHT2_dissect_ =: 0   NB. set for highlight printout - pixel details
DEBVERB_dissect_ =: 0   NB. set for travdown printout
DEBLAYOUT_dissect_ =: 0   NB. display grid details
DEBROUTE_dissect_ =: 0   NB. display routing details
DEBGRAF_dissect_ =: 0   NB. display all drawn graphics
DEBOBJ_dissect_ =: 0  NB. display drawn-object details
DEBDOL_dissect_ =: 0  NB. display drawing locales
DEBINHU_dissect_ =: 0  NB. display inheritu
DEBDOL2_dissect_ =: 0  NB. display drawing locales
DEBDOvn_dissect_ =: 0  NB. display object headers
DEBPICK_dissect_ =: 0  NB. display pick progress
DEBTIME_dissect_ =: 0  NB. Show elapsed times
QP_dissect_ =: qprintf
SM_dissect_ =: smoutput
edisp_dissect_ =: 3 : '(":errorcode) , ''('' , (errorcodenames{::~1+errorcode) , '')'''
0 : 0
NB. obsolete 0!:1 ; <@(LF ,~ 'dissectinstanceforregression_dissect_ 4 : ''(i. 0 0) [ destroy__x 0 [ dissect_dissectisi_paint__x 0''^:(0=#@]) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_base_
0!:1 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_paint__y 0''^:(''''-:$) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_base_
testsandbox_base_ 1
)
NB. TODO:
NB. Launch Jwiki from hotlinks in tooltips
NB. hovering over data: allow clicking in low-right of scrollbars to change individual size
NB. Support u :: v
NB. Test display of fill-cells incl errors
NB.  Do better job of showng where error in fill-cell exec occurred
NB.  Distinguish between the two previous on 'error'
NB. explain different types of error
NB. Worry about getting the shape right if the rank stack contains a non-calculus entry (like L:)
NB. Enforce a recursion limit to help debug stack error - if original failed w/stack error?
NB. clicking on vbname (if tacit) should launch sandbox for that name
NB. Highlight net on a click/hover of a wire
NB. Hovering over selected cell to detail computation there?
NB. can simplify combineyxsels
NB. dissect '1 2 1 </."2 i. 2 3 4'   " shows on /. - should be on final as well?
NB. change rank stack in partitions (test /."0), don't dup /.
NB. Re-select of selected cell of @. should remove expansion
NB. pseudoframes show up in frame explanation.  Look at rank?  Messes up L: too
NB. if a recursion produces no result, flag that fact
NB. fix pas in 803
NB. dissect '>:L:0"0 (1;''a'';3;4)'   doesn't crosshatch unexecd cells.  Seems that it should: fillmask is right for it   rectcolorfromfillmask neexds to insert stippling
NB. dissect 'a ,S:1 b' [ a =. <'a' [ b =. (<0 1);<(<2 3 4);(1);<<5 6;7 8   the error cell is empty, so no crosshatching is seen.  Should it be taller?
NB. dissect '(* $:@:<:)^:(1&<) 7'    select result 1 - no detail displayed inside ^:
NB.  this is because there are multiple possible results, so we skeletalu.  But should the wiring bypass the skeletalu?
NB. should we show leading singleton axes?  Should we shoe datatype, for chars at least?
NB. dissect '(($0);1 0 1 1 0) +:;.1 i. 4 5'  fails on selection
NB. support axis permutations for display, for u;.
NB. if there is an error framing the forward and reverse, we don't catch it and don't select it
NB. faster addlog
NB. Audit selection for in-bounds
NB. Display failing node of u/ as error cell; don't set to all fill

NB. Need different text color for digits/text, and for nouns with leading 1s in the shape

NB. put a fence around route to save time?  Take hull of points, then a Manhattan standoff distance
NB. routing: if anything routed, the whole net must be
NB. handle clicking on verb-name part to select tree
NB. create pickrects for displayed sentence, and handle clicks there
NB. plan: save preferences; debug globals
NB. Add space between the label/shape/status blocks - add to bbox layout in alignrects
NB. green lines between ranks-3s don't show up if there's fill
NB. test errorlevel, including for fill cells.
NB. A way to display error encountered during fill cell?

NB. should we allow selection if final result is early error? (what shape then?)

NB. worry about whether gerund needs to traverse.  Shape display of gerund is wrong, because it's calculated incorrectly.  Should use noun methods for result of `

NB. dissect - 2d graphical single-sentence debugger

NB. the call is:
NB. [options] dissect [sentence]
NB. where sentence is a string to be executed.  The sentence is parsed and modified so that every verb execution creates
NB. looging information about its input and outputs.  Then the modified sentence is executed (in the same context as the original
NB. dissect verb), and then the results are displayed in 2d form.  If sentence is omitted, the sentence from the last error is used.
NB.
NB. Options are (bitmask) where
NB.  bit 0 is 1 to use a sandbox for executing the sentence
NB.  bit 1 is 1 to return the locale of the dissect window
NB.
NB. If y is boxed, it should be a table ready for use in parse, i. e. nx3 where the first line gives
NB. parameters;locale;text of sentence
NB. and the remaining lines are the local names defined in the running explicit definition, as described in z458095869 below
NB.
NB. Result is a string containing an error message if a window couldn't be created, otherwise an empty string EXCEPT when the
NB. dissect locale was requested: in that case return it if it was created

NB. obsolete dissect_z_ =: [: ([: display_dissect_ <@". :: (''"_)&.>)`nodisplay_dissect_@.(2=3!:0)  [: parse_dissect_ (0: : [    (([ ; 18!:5@(''"_) ; ]) , z458095869_dissectnopath_@(''"_))   ])@getsentence_dissect_`]@.(0<L.@])
dissect_z_ =: [: ([: display_dissect_ <@". :: (''"_)&.>)`nodisplay_dissect_@.(2=3!:0)  [: parse_dissect_ ((0: : [)    (([ ; 18!:5@(''"_) ; ]) , z458095869_dissectnopath_@(''"_))  getsentence_dissect_@])^:(0=L.@])

NB. The locale dissectnopath is used to find local names.  Its path is empty.  The locale contains only one name, z458095869
cocurrent 'dissectnopath'
copath ''
NB. The verb z458095869 returns a table of defined local names.  It is a table of (name;(type from 4!:0);(numeric ranks, invertible if verb/value if noun, '' if other))
z458095869 =: (([ ,. <"0@] ,. (".@[`(rankinv_dissect_@[)`(rankinv_dissect_@[)`(rankinv_dissect_@[))@.]&.>) (4!:0)) @ ((<'z458095869') -.~ 4!:1@i.@4:)

require 'strings gl2'
require '~addons/format/printf/printf.ijs'
cocurrent 'dissect'
coinsert 'jgl2'

dissectinstance =: 0$a:

defstring =: 'start of traversal'"_   NB. for debugging only

getsentence =: (' ' takeafter LF (i:~ }. ]) [: }:^:(LF={:) (13!:12))^:(0=#)

NB. Maximum line length that we will try to display in a grid cell

MAXSENTENCEWIDTH =: 0.5  NB. max frac of screenwidth that we allow for sentence display

ifdefined =: 0 <: [: 4!:0 <

NB. ******************* code for function keys ******************
finddissectline =: 3 : 0
NB.?lintonly  WinText_jqtide_ =: WinSelect_jqtide_ =: 0 0
NB. y tells what kind of run: 0=line under cursor, 1=last error, 2=clipboard
select. y
case. 0 do.
  ft =. WinText_jqtide_
  fs =. WinSelect_jqtide_
  NB. If a single value is selected, take the whole line; otherwise the selected region
  if. 1 < # ~. fs do.
    (-~/\ fs) (];.0~ ,.)~ ft 
  else.
    (LF taketo&.|. ({.fs) {. ft) , LF taketo ({.fs) }. ft
  end.
case. 1 do.
  ''  NB. empty line means 'last error'
case. 2 do.
  {.^:(0=#) wd 'clippaste'   NB. fail 'no sentence' if nothing on clipboard
case. do.
  ' '  NB. Will fail with 'no sentence'
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
PTactions =:  2{"1 x  NB. actions

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
isgerund =: 0:`(2 32 e.~ 3!:0@>)@.(32=3!:0)"0

class =: 3 : 0         NB. the class of the word represented by string y
if. y-:mark do. mark return. end.
if. isname y do. name return. end.
if. 10>i =. (;:'=: =. ( ) m n u v x y')i.<y do.
  i{asgn,asgn,lpar,rpar,6#name return.
end.
(4!:0 <'x' [ ".'x =. ',y){noun,adv,conj,verb
)
NB. *** end of copied stuff

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

parse =: 3 : 0  NB. called in dissect locale
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
  if. 0 = #errormessage do.
NB. If the error was unexpected, display it
    smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
    smoutput 13!:12''
    errormessage =: 'error during parsing'
  end.
  errormessage
end.
NB.?lintsaveglobals
)

NB. Signal failure of the parse.  y is the error message
NB. We keep it in the main dissect locale for ease, since it is valid only over the parse
failparse =: 3 : 0
errormessage_dissect_ =: y
13!:8 (1)
)

saveJenvirons =: 3 : 0   NB. called in the instance locale.  creates the instance name 'wdtimer'
NB. We save the locale sizes and the sys_timer status.  If this is not QT, we define sys_timer_z_
NB. but only if sys_timer_base_ is not defined; and we delete it when we restore
NB. Make the timer check only when starting the first instance
NB. If a timer was NOT created because of earlier sys_timer, stub out wdtimer in the instance;
NB. otherwise allow it to go through to the definition in dissect locale
if. ALLOWNONQTTOOLTIP *. -. IFQT do.
  if. 0 = #dissectionlist_dissect_ do.
    if.  0 > 4!:0 <'sys_timer_base_' do.
      sys_timer_z_ =: sys_timer_dissect_
    end.
  end.
  NB. If somebody else's timer is present, disable our use of it
  if. (<'sys_timer_dissect_') -.@-: 5!:1 <'sys_timer_z_'  do. wdtimer =: ] end.
end.
Jenvirons =: (9!:38 '')
NB.?lintsaveglobals
)
restoreJenvirons =: 3 : 0   NB. called AFTER removing instance from the list
9!:39 Jenvirons
if. (0 = #dissectionlist_dissect_) *. (ALLOWNONQTTOOLTIP *. -. IFQT) do.
  if. (<'sys_timer_dissect_') -: 5!:1 <'sys_timer_z_' do.
    NB. If we created a timer, remove it
    4!:55 <'sys_timer_z_'
  end.
end.
0 0$0 
)

NB. obsolete 3 : 'if. 0 > 4!:0 <''dissectionlist'' do. dissectionlist =: 0$a: end.' ''  NB. List of open dissect instances.  The last one is the most recent
NB. Initialization
create =: 3 : 0
NB. Save the initial environment BEFORE we indicate instance running
saveJenvirons''
dissectionlist_dissect_ =: dissectionlist_dissect_ , coname''
objtable =: 0$a:   NB. list of parse objects
ticket =: 0   NB. sequential log number
winhwnd =: ''  NB. Init to no window
NB. Use lightweight locales - we use less than 100 entries usually
9!:39 (1) 1} 9!:38 ''
NB. For nodes that do not have a parallel path (i. e. all but forks and &), this locale will
NB. be the predecessor locale, and will not signal an error
errorcode =: 0
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

NB. Create a verb node.  y is (string form of the verb[;display form]);(token number)
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

NB. Create a noun node.  y is (string form of the verb[;display form]);(token number)
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
NB. Result is result from create which is type;locale;token #)
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
NB. Result is the string form of the instrumented sentence, ready to execute
NB. As a side effect, many objects are created indicating the parse structure
NB. In paticular, resultroot is the boxed locale of the sentence result.
NB. If there is an error, resultroot is empty
NB. The only option is 'sandbox', in which case we create an explicit definition to run the
NB. sentence in, and define all the user names in it, before running it
parsemain =: 3 : 0   NB. runs in object locale
defnames =. }. y  NB. table of names
'options loc sentence' =. {. y
sandbox =.  * 1 bwand {.!.0 options
returnobject_dissect_ =: * 2 bwand {.!.0 options

NB. Break the input into words.  If there is an error, fail.  Discard any comment
NB. Discard anything past the first LF, and remove CR
sentence =. CR -.~ ({.~ i.&LF) sentence
try. queue =. ;: sentence catch. queue =. 0$a: end.
if. #queue do.  NB. following fails on no words
NB. Get mask of words to discard: discard leading control words, or anything starting with a control word after a non-control
  dischdtl =. (*./\ ,: [: +./\ 0 , (2) </\ ]) iscw queue
NB. Get the sentence in the form the user gave it, by deleting the nonblank characters corresponding
NB. to the discarded words.
  ndiscardshdtl =. dischdtl (#@(-.&' ')@;@#)"1 queue
  usersentence =: ' ' (-@(i.&0@:= |.) }. i.&0@:= }. ]) sentence ((}.~ {.) }.~ -@{:@]) ndiscardshdtl i.~"0 1 (0) ,. (+/\ ,: +/\@|.) ' ' ~: sentence
NB. keep the nondiscards in the tokenized version
  queue =. (+:/ dischdtl) # queue
end.
NB.?lintonly usersentence =: ''

NB. If the sentence is empty, abort
if. 0 = #queue do. failparse 'No sentence' return. end.

NB. Append an end-of-queue mark to the sentence, and initialize the stack.
NB. The stack is type;value;tokennums where value is the locale of the object producing the result, for verb and noun;
NB. or the string form, for a modifier.  Tokennums are the input token numbers that contribute to the item
queue =. mark ; queue
stack =. 4 2 $ mark;''

NB. We keep track of $: verbs encountered, and insert an expansion node if there are any.  The expansion
NB. is before the monad/dyad execution containing the $: .  We clear the flag indicating recursion to begin with, and after any monad/dyad.
recursionencountered =: 0

NB. In case of parse failure, we remember whether we executed a user modifier.  If we did, our assumption that it produced
NB. a verb may have caused the failure, and we give a suitably couched error message
usermodifierencountered =: 0

NB. Process the sentence through the stack
while. do.
NB. If the stack contains an executable combination, execute it
NB. If part of the execution has unknown value, produce an unknown result, of type 'noun' for verb executions,
NB. and 'verb' for modifier executions
  select.
  NB.?lintonly stack =. (verb,verb,verb,noun);"0<''
      if. (#PTpatterns) > pline =. 1 1 1 1 i.~ * PTpatterns bwand"1 ,>4 1{.stack do.
        exeblock =. (subj =. pline{PTsubj) # 4 {. stack  NB. the executable part
        exetypes =. > subj # , 4 1 {. stack   NB. the corresponding types
      end.
  NB.?lintonly exeblock =. 3 3$<'' [ exetypes =. 0 0 0 [ subj =. 0 1 1 1
      QP^:DEBPARSE'pline exetypes exeblock stack '
      pline
      
    case. 0;1 do.  NB. monad
  NB. Create a monad execution block for the operands, and put that on the stack
      stack =. ((subj i. 1){.stack),('dissectmonad' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
      
    case. 2 do.  NB. dyad
  NB. Create a dyad execution block for the operands, and put that on the stack
      stack =. ((subj i. 1){.stack),('dissectdyad' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
      
    case. 3;4 do.  NB. adverb/conjunction execution
      stack =. ((subj i. 1){.stack),(execmod exeblock),((>:subj i: 1)}. stack)
      
    case. 5 do.  NB. Trident N V V or V V V
  NB. Create a trident execution block for the operands, and put that on the stack
      stack =. ((subj i. 1){.stack),('dissectfork' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
      
    case. 6 do.   NB. bident  A A, C VN, VN C, V V
  NB.?lintonly exetypes =. 0 0 [ exeblock =. '';'';''
      if. bwand/ verb , exetypes do.  NB. V V
        stack =. ((subj i. 1){.stack),('dissecthook' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
      elseif. (bwand/ adv , exetypes) +. (conj = +/ conj bwand exetypes) do. NB. A A, C VN, NV C
  NB. This becomes an adverb type.  The value is the exeblock, which will be executed later
        stack =. ((subj i. 1){.stack),(adv;exeblock; ; 2 {"1 exeblock),((>:subj i: 1)}. stack)
      elseif. do.
        failparse 'Invalid sequence: ' , ;:^:_1 ('Verb';'Adverb';'Conjunction';'Noun') {~ 1 i.~"1 * exetypes bwand/ (verb,adv,conj)
        return.
      end.
    case. 7 do.  NB. assignment
  NB. See if we can analyze the assignment.  If so, add to the name table.
  NB. If the assignment is not a noun value, ignore it with a warning
      if. 0 = noun bwand 2 { exetypes do.
        failparse 'non-noun assignment not supported'
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
            failparse 'AR assignment to ' , lvalue , ' not supported'
            rname =. 0$a:
            return.
          else.
            rname =. ;: :: (a:$~0:) lvalue
          end.
        else.
          failparse 'Invalid assignment'
          return.
        end.
  NB. If the assignment is to a variable name, we can do nothing with it
      elseif. do.
        rname =. 0$a:
      end.
      NB. If the assignment is one we can handle, we will have one or more names.  In that case, create an
      NB. assignment block on the stack
      stack =. ((subj i. 1){.stack),('dissectassign' 0 createmodifier exeblock),((>:subj i: 1)}. stack)
        NB. We would like to preserve the value of the unhandleable assignment, but we can't, because
        NB. We need an assignment node to account for the assignment tokens, and we can't get a value for the
        NB. modifier because it might be complex (a train).  If we try to push the assignment tokens into
        NB. the rvalue, it would have to be able to handle them, which we're not ready to do since they would be
        NB. out of order. So, we lose the value of the assignment (kludge)
        NB. Otherwise, we have to ignore it.  We can't produce an assignment block, because we don't know what
        NB. value to put into the executed sentence.  So we just ignore the assignment, leaving
        NB. the rvalue on the stack.  We will leave the assignment tokkens out of the display too, since we
        NB. don't process them
NB. obsolete         stack =. ((subj i. 1){.stack),((< ; 2 {"1 exeblock) (<0 2)} (subj i: 1)}. stack)
NB. obsolete         stack =. ((subj i. 1){.stack),((subj i: 1)}. stack)
      
  NB. rname has the list of names that we should define.  If this is a global assignment,
  NB. append the locale name to each name that doesn't contain a locative
      if. (<'=:') -: (<1 1) { exeblock do. rname =. (('_',(>loc),'_') ,~ ])^:('__'&(+./@:E.) +: '_' = {:)&.> rname end.
      
  NB. We can't deal with assignments to object locatives since we track only the part of speech, not the value, at parse time
      if. +./ elocs =. '__'&(+./@:E.)@> rname do.
        smoutput 'Assignment to object locatives not supported: ' , ;:^:_1 elocs # rname
        rname =. (-. elocs) # rname
      end.
      
  NB. Define the names, as nouns (J nameclass 0).
      defnames =. (rname ,"0 1 (0;'')) , defnames
      
    case. 8 do.  NB. ( x )
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
  NB. If self-defining term, create a noun block for it, mark as sdt
      elseif. (qend e. ;:'a. a: _.') +. (-. '.:' e.~ {: qend) *. ({. qend) e. '''_0123456789' do.
        stack =. stack ,~ (<sdt+noun) 0} createnoun qend;'';(#queue)
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
  NB. Look up the object locative in the local name table, resolving to type;value/rank if found
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
  NB. objtype/objval.  But a global search may be needed: if there was
  NB. an object locative, or if the local search failed.  This search will start in locale gloc.
  NB. This search, if performed, must succeed, and we will convert the result to a type/(rank if verb)
        if. #glopart do.
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
            if. 0 <: objtype =. 4!:0 :: _2: <glopart do.
              objval =. rankinv_dissect_ f. glopart
            else.
              objtype =. _1
            end.
            cocurrent savloc
          end.
        end.
  NB. Now objtype/objval are set.  If the name is a noun or verb, create a locale for it
  NB.?lintonly 'objtype objval' =. 0;0 0 0 0
      select. objtype
        case. 0 do.
          ntypeval =. createnoun qend;qend;(#queue)  NB. Keep name, and save name for display
        case. 1 do.
          NB. adverb: handle the special code (currently only &.>)
          NB. If the value of the user name matches special code, expand it on the stack
          if. objval -: '&.>' do.
            ntypeval =. (conj;'&.';(#queue)) ,: createverb (,'>');(0$0)
          else.
            NB. If the value of the user name matches a supported primitive, replace the name by the supported value
            ntypeval =. adv;(((<objval) +./@:((e.>)"0) dissectprimindex) {:: qend;objval);(#queue)
          end.
        case. 2 do.
          ntypeval =. conj;(((<objval) +./@:((e.>)"0) dissectprimindex) {:: qend;objval);(#queue)
        case. 3 do.
           NB. If the verb has a one-line definition, pass that into the definition for tooltip purposes
          ntypeval =. createverb (qend;(#queue)) , (-. LF e. objval)#<objval
        case. do.
          failparse 'undefined name: ' , qend
          return.
      end.
      
NB. Make the stack entry for the new name
      stack =. ntypeval,stack
      
    elseif. do.
NB. Must be a primitive.  Get its type and stack it
      ". 'exeobj =. ' , qend
      select. 4!:0 <'exeobj'
        case. 0 do.
          ntypeval =. createnoun qend;'';(#queue)
        case. 1 do.
          ntypeval =. adv;qend;(#queue)
        case. 2 do.
          ntypeval =. conj;qend;(#queue)
        case. 3 do.
          ntypeval =. createverb qend;(#queue)
        case. do.
          failparse 'invalid type for primitive'
          return.
      end.
      stack =. ntypeval,stack
      
    end.
  end.
  if. pline < 9 do. errstartpoint =. _2 {. }. queue end.
end.   NB. End of loop processing stack.  top of stack is a mark
NB. verify that the sentence has a valid finish: 1 noun.
if. 1 1 -.@-: * (noun,mark) bwand >(<1 2;0){stack do.
  failparse usermodifierencountered {:: 'Sentence did not produce a noun result';'Sentence does not seem to produce a noun result.  Dissect assumes that user modifiers produce verb results.'
  return.
end.

NB. The locale at the top of the stack is the overall result.  Save that, and return the instrumented sentence
NB. This call will fill in all the verb-to-noun locale references
resultroot =: (<1 1) {:: stack
NB.?lintonly resultroot =: <'dissectmonad' [ scrollinglocale =: <'dissectobj'
QP^:DEBTIME'endparse=?6!:1'''' '
NB. Init the estheights in every object
calcallestheights__resultroot $0
NB. Create the string to execute.  If we have to create a sandbox, do so
execsentences_dissect_ =: sentence;exestring__resultroot''
if. sandbox do.
  NB. create the sandbox verb in the user's locale
  NB.?lintmsgsoff
  (sandname_dissect_ =: 'sandbox4768539054_',(>loc),'_') =: sandboxtemplate f.
  NB.?lintmsgson
  defnounmask =. (<0) = 1 {"1 defnames
  NOUNNAMES_dissect_ =: defnounmask # 0 {"1 defnames
  NOUNVALUES_dissect_ =: defnounmask # 2 {"1 defnames
  DEFSTRING_dissect_ =: (*#NOUNNAMES_dissect_) # '(NOUNNAMES_dissect_) =. NOUNVALUES_dissect_',LF
  DEFSTRING_dissect_ =: DEFSTRING_dissect_ , ; ([ , ' =. ' , LF ,~ ])&.>/"1 (0 2) {"1 (-. defnounmask) # defnames
NB. obsolete   ARNAMES_dissect_ =: (-. defnounmask) # 0 {"1 defnames
NB. obsolete   ARVALUES_dissect_ =: (-. defnounmask) # 2 {"1 defnames
NB. obsolete   if. 0 < #ARNAMES_dissect_ do.
NB. obsolete     if. 1 = #ARNAMES_dissect_ do.
NB. obsolete       ARNAMES_dissect_ =: 2 # ARNAMES_dissect_
NB. obsolete       ARVALUES_dissect_ =: 2 # ARVALUES_dissect_
NB. obsolete     end.
NB. obsolete     ARNAMES_dissect_ =: '`' , ;:^:_1 ARNAMES_dissect_
NB. obsolete     ARVALUES_dissect_ =: 3 : ('0!:100 ''y =. '' , y';'5!:1 <''y''')&.> ARVALUES_dissect_
NB. obsolete   end.
  '01' ([ , ' ' , sandname_dissect_ , ' ' , ''''&,@(,&'''')@(#~ >:@(=&''''))@])&.> execsentences_dissect_
NB. obsolete   (sandname_dissect_ , ' 0');(sandname_dissect_ , ' 1')
  NB.?lintsaveglobals
else.
  execsentences_dissect_
end.
NB.?lintsaveglobals
)

sandboxtemplate =: 4 : 0
if. x do. 4!:55 <sandname_dissect_ end.
v4768539054_dissect_ =. y
4!:55 ;: 'x y'
NB. obsolete if. #NOUNNAMES_dissect_ do. (NOUNNAMES_dissect_) =. (NOUNVALUES_dissect_) end.
NB. obsolete if. #ARNAMES_dissect_ do. (ARNAMES_dissect_) =. (ARVALUES_dissect_) end.
0!:100 DEFSTRING_dissect_
". v4768539054_dissect_
)

NB. Here to execute a modifier.  We do that when we encounter a modified verb.
NB. This will be from VN A or VN C VN, but the A in VN A might be a compound adverb.
NB. We detect that if the adverb'value is boxed rather than a string.  In that case we
NB. recur on the parts of the adverb.
NB. y is the exeblock.
NB. Result is a line to put on the stack, coming from the execution of the modifier
execmod =: 3 : 0
exetypes =. 0 {::"1 exeblock =. y
NB. If the modifier's value is boxed, it is a compound modifier, necessarily bident.  We will classify it as
NB. C VN, VN C, or A A, and execute it as is sppropriate.
if. 32 = 3!:0 modblock =. (<1 1) {:: exeblock do.
  select. (0 {::"1 modblock) i. conj
    case. 0 do. NB. C VN
      ntypeval =. execmod ({.exeblock),modblock
    case. 1 do. NB. VN C
      ntypeval =. execmod modblock,{.exeblock
    case. do.  NB. A A
      ntypeval =. execmod (execmod ({.exeblock),:{.modblock) ,: {: modblock
  end.
else.
NB. If the modifier and all the operands are self-defining terms, execute the modifier, figure out the resulting
NB. part of speech, and create an appropriate type/value for it.  This will handle things like
NB. 3 b.   and 1!:1   and  1 : '...' .
NB. obsolete   if. bwand/ sdt 1} exetypes do.
  if. bwand/ sdt , exetypes do.
    ". 'exeobj =. ' , defstg =. ; 3 : 'if. 32 = 3!:0 y do. defstring__y 2 else. enparen y end.'&.> (1) {"1 exeblock
    tokennums =. ; 2 {"1 exeblock
    select. 4!:0 <'exeobj'
    case. 0 do.
      ntypeval =. createnoun (5!:5 <'exeobj');'';tokennums
    case. 1 do.
      ntypeval =. adv;(enparen defstg);tokennums
    case. 2 do.
      ntypeval =. conj;(enparen defstg);tokennums
    case. 3 do.
      ntypeval =. createverb (5!:5 <'exeobj');tokennums
    case. do.
      failparse 'Invalid type while applying modifier'
      return.
    end.
  else.
    NB. Not sdt.  Look up the locale of the modifier, and execute it.  The executed modifier must correctly guess the
    NB. part of speech that is going to be produced.
    ntypeval =. exeblock (0 createmodifier)~ 'dissectprim' , ": ifound =. ((<1 1) { exeblock) i.&1@:((e.>)"0) dissectprimindex
    NB. If the modifier was not one that we recognize, remember that, in case of parse failure
    usermodifierencountered =: usermodifierencountered >. ifound = #dissectprimindex
NB.?lintonly nobj =. localedefault
  end.
end.
ntypeval
)

NB. These settings are persistent across dissections, since they describe the user, not the problem
minimumfontsizex =: 2   NB. font size to use in main window
ttfontsizex =: 0   NB. tooltip font size
tooltipdelayx =: 2  NB. tooltip delay
tooltipdetailx =: 1   NB. tooltip detail level


NB. Following lines must match the menus!
FONTSIZECHOICES =: 8 10 12 14 16 20 24
TOOLTIPFONTSIZECHOICES =: 8 10 12 14
MAXNOUNPCTCHOICES =: 10 20 30 40 50 60 70 80 90

MAXNOUNPCTCHOICESDEFAULT =: 3   NB. limit to 30% by default

MAXEXPLORERDISPLAYFRAC =: 0.8   NB. Amount of screen to allow for nouns in explorer

NB. The tooltip size will be selected according to detail and expanded according to fontsize
TOOLTIPMINISISIZE =: 200 300,300 500,:900 600
MINIMUMISISIZE =: 300 500     NB. minimum size for graphics, needed to allow room for tooltip

TOOLTIPMAXPIXELS =: 450  NB. Max width of tooltip, in pixels
TOOLTIPMAXFRAC =: 0.8  NB. Max tooltip width, as frac of isigraph width

TOOLTIPDELAYCHOICES =: ('immed';'250';'500';'1000') ,. ('immediate';'0.25 sec';'0.5 sec';'1 sec') ,. <"0 (1 250 500 1000)
TOOLTIPDETAILCHOICES =: ('0';'1';'2') ,. ('laconic';'verbose';'tutorial') ,. <"0 (0 1 2)

fontlines =. ; <@('menu fmfontsize' , ": , ' "' , ": , '";' , LF"_ )"0 FONTSIZECHOICES
ttfontlines =. ; <@('menu fmttfontsize' , ": , ' "' , ": , '";' , LF"_ )"0 TOOLTIPFONTSIZECHOICES
sizexlines =. ; <@('menu fmmaxnounsizex' , ": , ' "' , ": , '%";' , LF"_ )"0 MAXNOUNPCTCHOICES
sizeylines =. ; <@('menu fmmaxnounsizey' , ": , ' "' , ": , '%";' , LF"_ )"0 MAXNOUNPCTCHOICES
ttdlines =. ; ('menu fmtooltipdelay' , [ , ' "' , ] , '";' , LF"_ )&.>/"1 (2) {."1 TOOLTIPDELAYCHOICES
ttdetlines =. ; ('menu fmtooltipdetail' , [ , ' "' , ] , '";' , LF"_ )&.>/"1 (2) {."1 TOOLTIPDETAILCHOICES
DISSECT=: ((,&LF&.> 'rem font;';'rem ttfont;';'rem sizex;';'rem sizey;';'rem ttdlines;';'rem ttdetlines;') ,. fontlines;ttfontlines;sizexlines;sizeylines;ttdlines;ttdetlines) stringreplace 0 : 0
pc dissect;
menupop "&Preferences";
menupop "&Font Size";
rem font;
menupopz;
menupop "&Tooltip Font Size";
rem ttfont;
menupopz;
menu fmshowstealth "Show ][";
menupopz;
menupop "&Sizes";
menupop "Max &Width as % of Screen";
rem sizex;
menupopz;
menupop "Max &Height as % of Screen";
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
menupopz;
menupop "&Help";
menu fmhelplearning "Learning Dissect";
menu fmhelpusing "Using Dissect";
menupopz;
xywh 3 4 20 12;cc fmshowerror button;cn "<<";
xywh 26 4 20 12;cc fmbwd button;cn "<";
xywh 48 4 20 12;cc fmfwd button;cn ">";
xywh 74 4 120 12;cc fmstatline static;
xywh 3 19 2 2;cc dissectisi isigraph;
pas 0 0;
rem form end;
)

DISSECT =: (((,&LF&.> 'rem font;';'rem ttfont;';'rem sizex;';'rem sizey;';'rem ttdlines;';'rem ttdetlines;') ,. fontlines;ttfontlines;sizexlines;sizeylines;ttdlines;ttdetlines) stringreplace 0 : 0) [^:IFQT DISSECT
pc dissect;
menupop "&Preferences";
menupop "&Font Size";
rem font;
menupopz;
menupop "&Tooltip Font Size";
rem ttfont;
menupopz;
menu fmshowstealth "Show ][";
menupopz;
menupop "&Sizes";
menupop "Max &Width as % of Screen";
rem sizex;
menupopz;
menupop "Max &Height as % of Screen";
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
menupopz;
menupop "&Help";
menu fmhelplearning "Learning Dissect";
menu fmhelpusing "Using Dissect";
menupopz;
bin vhh0;
minwh 6 28;cc fmshowerror button;cn "<<";
set fmshowerror tooltip Go back to initial selection;
minwh 6 28;cc fmbwd button;cn "<";
set fmbwd tooltip Undo selection;
minwh 6 28;cc fmfwd button;cn ">";
set fmfwd tooltip Redo selection;
minwh 5 28;cc fmspacer static;cn "";
bin zh1;
minwh 50 16;cc fmstatline static;
bin zz;
minwh 400 80;cc dissectisi isidraw flush;
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
wdpmove =: ([: wd 'pmovex ' , ])`([: wd 'pmove ' , ])@.IFQT
3 : '(glfontextent_jgl2_ =: glfont_jgl2_)^:0 (0)'^:(0 > 4!:0) <'glfontextent_jgl2_'  NB. defined in 8.03

NB. timer covers.  On 602 we have a single global, shared by all instances, indicating which locale the
NB. timer is running for.  Kludge, but seemingly OK since only one locale can have focus.  Called
NB. in the locale of the desired return
NB. wdtimer is the name defined in dissect locale, depending on IFQT.  The same name is copied to the instance locale
NB. if tooltips are enabled for non-QT
NB. obsolete wdtimer =: ]`(3 : 0)`([: wd 'ptimer ' , ":)@.(+/ +./\ IFQT,ALLOWNONQTTOOLTIP)  NB. 0=J6 notooltip, 1=J6 tooltip, 2=QT
wdtimer =: (3 : 0)`([: wd 'ptimer ' , ":)@.IFQT
runningtimerloc_dissect_ =: coname ''
wd 'timer ' , ": y
)

sessionyx =: 10 10   NB. Initial position of the first window
NB. Set the initial value to use for the form, so we remember it from call to call
NB. Called whenever we think the control might have moved
setsessionyx =: 3 : 0
  sessionyx_dissect_ =: 1 0 { 0 ". wdqform''
)
NB. Get the starting value
getsessionyx =: 3 : 0
sessionyx_dissect_
)

NB. ************** start of the display section (after the parse is over) ****************

NB. Either nodisplay or display is always called.  The clearing of dissectinstance is a way to prevent recursion.
NB. y is the error message, which we pass through
NB. Always called in dissect locale
nodisplay =: 3 : 0
destroy__dissectinstance ''   NB. This will clear dissectinstance
y
)

display =: 3 : 0   NB. called in dissect locale
QP^:DEBTIME'startdisplay=?6!:1'''' '
if. #dissectinstance do.
  try.
    displaymain__dissectinstance y
    if. returnobject do. ret =. dissectinstance else. ret =. 0 0 $0 end.
    dissectinstance =: 0$a:
    QP^:DEBTIME'enddisplay=?6!:1'''' '
    NB. Normal return: quiet return, unless user asked for the object id
    ret
  catch.
    NB. If error during initial display, display the error and clean up
    smoutput 'error in initial display'
    smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
    smoutput 13!:12''
    destroy__dissectinstance''
    'Error in dissect.'
  end.
else.   NB. user tried recursive execution
  destroy__dissectinstance''
  'Vivisection is illegal.'
end.
)
NB. y is the results from running the user's original sentence and our instrumented version.
displaymain =: 3 : 0  NB. called in dissectinstance locale
NB. Make sure the results are the same
NB. If the sentence ran correctly for the user, make sure we get the same result.
NB. If there is only one result, we don't compare
if. 1 < #y do.
  if. -.@-:/ y do.
QP^:(NOCLEANUP)'y '
    'dissect error: internal result does not match the result from the J session!  Aborting.' 13!:8 (1)
    return.
  end.
end.

NB. For forced tooltips (clicks on unexpandable data), we enforce a minimum display time so that
NB. the user has a chance to see that there is a tooltip there.  This value is the session-timer value
NB. at which the display can be withdrawn.
hoversessmin =: 0

NB. save the crash indicator
if. crashed =: 0 = #0 {:: y do.
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
NB. obsolete NB. If we didn't crash, remove the show error button
NB. obsolete wd ; <@(#~ -.@('fmshowerror'&(+./@:E.)));.2^:(-. crashed) DISSECT
wd DISSECT
winhwnd =: wd 'qhwndp'
NB. Initialize the user selection
NB. obsolete 'fmfontsize' wdsetitems ; (LF ,~ ":)&.> FONTSIZECHOICES
NB. obsolete 'fmfontsize' wdsetselect ": minimumfontsizex =: 2
('fmfontsize' , ": FONTSIZECHOICES {~ minimumfontsizex) wdsetvalue  '1'
('fmttfontsize' , ": TOOLTIPFONTSIZECHOICES {~ ttfontsizex) wdsetvalue  '1'
maxnoundisplaysizex =: 2#MAXNOUNPCTCHOICESDEFAULT
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '1'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '1'
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0 ,~ tooltipdelayx) wdsetvalue '1'
('fmtooltipdetail' , TOOLTIPDETAILCHOICES {::~ <0 ,~ tooltipdetailx) wdsetvalue '1'
maxnoundisplayfrac =: 0.01 * maxnoundisplaysizex { MAXNOUNPCTCHOICES
calccfms minimumfontsizex { FONTSIZECHOICES
displaystealth =: 0

NB. Initialize the parent-node indicator in every object, a: (=invalid) at the top
initparentnodes__resultroot <'dissectobj'  NB. root will point to harmless parent
NB. Init the SDT-detail indicator in every object
NB.?lintonly resultissdt_dissectmonad_ =: 0
initnounshowdetail__resultroot resultissdt__resultroot
NB. If we crashed, do an initial traversal to set selection flags to find the error
maxnoundisplaysizes =: 2 2$0  NB. Init to small display for sniff, for speed

wd 'pn *Dissecting ' , usersentence

wd 'pshow'  NB. On QT, you can't calculate the size of graphics unless you are showing the form

NB. Initialize selection history.  This sets enables for the buttons too.
initselections''

if. crashed do.
  joinlayoutsl traverse 1   NB. Don't forget the final display!
  setdisplayerror__resultroot''
end.
NB. The sniff may have set scrollpoints based on the tiny screen, so reset them all
propscroll__resultroot 0

NB. Save the size of the screen, which we will use to decide max noun size.  h w
screensize =: 3 2 { 0 ". wd 'qscreen'

NB. Set the initial selection of cells:
NB. If the result is an sdt, the user is probably noodling around with a new sentence, so select everything
NB. resultissdt__resultroot
NB. Get the size of the sentence; use its height to set scroll position for the display
sentencesize =: 0 {:: usersentence sizesentence gettokenlevels__resultroot ''
NB. Set the starting position, just below the sentence
scrolltlc =: 0 ,~ 2 + {. sentencesize
NB. Do the initial traversal, calculate the initial placement.
placeddrawing =: calcplacement''
sizedrawingandform 1
dissect_dissectisi_paint^:IFQT ''  NB. QT doesn't kick off with a paint - do one
0 0$0
NB.The initial paint event will draw the screen
NB.?lintsaveglobals
)

NB. y is 0 for normal traversal, or 1 for error traversal.  We initialize and traverse
NB. Called in instance locale
traverse =: 3 : 0
NB. Initialize for the traversal
errorlevel =: 0
snifferror =: y
NB. We keep track of which monad/dyad execution is running, so that we can propagate all errors entirely
NB. through the tree for that exec
executingmonaddyad =: 0$a:
NB. Initialize the traversal stats: we use these to decide what options to offer the user
maxactualnounsize =: 0 0  NB. The number of pixels needed to show all nouns in their entirety
stealthopencountered =: 0   NB. Set if there is a stealth op on the display
NOLAYOUTS traverse__resultroot TRAVNOUN
NB.?lintsaveglobals
)

EXPANSIONROOMAROUNDISI =: 200 100  NB. Number of pixels to leave at margin
NB. Size the isigraph and the parent, and size the drawing for display
NB. If y is 1, set the initial position based on the session history
NB. Globals sentencesize, scrolltlc, placeddrawing have been set
NB. Result is the drawing (the result of sizeplacement)
NB. Side effect: the isigraph and parent are resized (up only) as required
NB. When we resize the isigraph, we include expansion room
sizedrawingandform =: 3 : 0
initfromsess =. y
NB. Get the required size - mostly the isi, but must be wide enough for the sentence too
yxneeded =. sentencesize >. 0 {:: shifteddrawing =. scrolltlc sizeplacement placeddrawing
NB. Get the current size of the isi; if insufficient, make it bigger, with expansion added
if. initfromsess +. yxneeded +./@:> 2 3 { cyxhw =. 1 0 3 2 { 0 ". wdqchildxywh 'dissectisi' do.
  minisi =. MINIMUMISISIZE >. <. (%/ TOOLTIPFONTSIZECHOICES {~ ttfontsizex,0) * tooltipdetailx { TOOLTIPMINISISIZE
  'dissectisi' wdsetxywh 1 0 3 2 { cyxhw =. (minisi >. EXPANSIONROOMAROUNDISI + yxneeded) 2 3} cyxhw
  NB. If the main form has grown now that the isi has grown, resize it too.
  if. initfromsess do. xywh =. 1 0 3 2 { (getsessionyx'') , +/ 2 2 $ cyxhw
  else. xywh =. (0 ". wdqform'') >. 0 0 , |. +/ 2 2 $ cyxhw
  end.
  wdpmove ": xywh
NB. obsolete   if. IFQT do.
NB. obsolete   else.
NB. obsolete     wd 'pas 1 1'
NB. obsolete   end.
end.
NB. Now that we have the size of the isigraph, center the sentence in the screen area; remember brect
sentencebrect =: sentencesize (] ,: +) 0 , cyxhw (<.@-:@-)&{: sentencesize
shifteddrawing
NB.?lintsaveglobals
)



NB. lay out the grid.  Should be called in the locale of the major instance
calcplacement =: 3 : 0
NB. Check the current screensize, and calculate the box sizes in characters
maxnoundisplaysizes =: <. (maxnoundisplayfrac ,: MAXEXPLORERDISPLAYFRAC) *"1 screensize

NB. Select our drawing control, so we have something to check sizes with
wd 'psel ',winhwnd
glsel 'dissectisi'
NB. Traverse, producing the final DOL
NB. Add on the display for the final collector
'dl dyxhw dwires dres' =. {. joinlayoutsl traverse 0
if. DEBOBJ do.
  qprintf'dl dyxhw dwires dres '
end.
NB. create wiring and revise placement
NB. create the parameters for the router: the brick of start/end+1, and the
NB. list of nets, in the form <source,dest,dest...
NB. where each point is  obj#,face#,fraction
dyxhw =. +/\"2 dyxhw  NB. Convert to start/end+1; adjust to leave top & bottom margin
wireoff =. ((dl i. {."1) ,"0 1 >@:({:"1)) dwires  NB. convert each wire to obj,face,fraction form
wirenets =. (<@((<0 0)&{ , {:"2)/.~ {."2) wireoff  NB. Convert nets (same source) to net form
NB.  size is ymax,xmax
NB.  gridsize is spacing between lines
NB.  standoff is min distance between a block and a line
NB.  penalties is penalty for a turn (in units of movement)
QP^:DEBTIME'startrouter=?6!:1'''' '
dl ; (ROUTINGGRIDSIZE;WIRESTANDOFF;ROUTINGTURNPENALTY) routegrid dyxhw;<wirenets
NB.?lintsaveglobals
)

NB. y is 1 for an internal call that needs to refigure the placement
dissect_dissectisi_paint =: 3 : 0
NB. To avoid an error loop, terminate quietly if there is an error
try.
NB. if we need to refigure the placement because of a change like selection or a display parameter, do so.
  if. 1 = {. y do. placeddrawing =: calcplacement'' end.
NB. Draw the revised placement and wiring.  Save the placement to speed scrolling
  QP^:DEBTIME'startdraw=?6!:1'''' '
  drawplacement }. sizedrawingandform 0
  glpaint''
  
NB. Set the user-option buttons based on the display results
NB. If the largest noun on the display is bigger than our smallest display option,
NB. give the user the option of changing the display size
  actualpctused =. >. 100 * maxactualnounsize % screensize
  NB. Disable all choices that are two notches above the actual max size
  enablesz =. '01' {~ actualpctused >:"0 1 |.!.0 MAXNOUNPCTCHOICES  NB. Prepend 0 so that 9 eg will enable 10
  (0 { enablesz) (wdsetenable~   'fmmaxnounsizex' , ":)"0 MAXNOUNPCTCHOICES
  (1 { enablesz) (wdsetenable~   'fmmaxnounsizey' , ":)"0 MAXNOUNPCTCHOICES
NB. obsolete NB. Keep all the choices less than the actual, and append the actual too, but don't
NB. obsolete NB. ever allow a choice larger than our wired-in maximum
NB. obsolete   choicestooffer =. MAXNOUNPCTCHOICES&([ (<.&# {. ]) ] ,~ I. {. [)&.> actualpctused
NB. obsolete   if. therearechoices =.  1 < # choices =. 0 {:: choicestooffer do.
NB. obsolete     'fmmaxnounsizey' wdsetitems ; (LF ,~ ":)&.> choices
NB. obsolete     'fmmaxnounsizey' wdsetselect ": (0 { maxnoundisplaysizex) <. <:#choices
NB. obsolete   end.
NB. obsolete   'fmmaxnounsizey' wdsetshow ": therearechoices
NB. obsolete   'fmmaxnounsizeylbl' wdsetshow ": therearechoices
NB. obsolete    if. therearechoices =.  1 < # choices =. 1 {:: choicestooffer do.
NB. obsolete     'fmmaxnounsizex' wdsetitems ; (LF ,~ ":)&.> choices
NB. obsolete     'fmmaxnounsizex' wdsetselect ": (1 { maxnoundisplaysizex) <. <:#choices
NB. obsolete   end.
NB. obsolete   'fmmaxnounsizex' wdsetshow ": therearechoices
NB. obsolete   'fmmaxnounsizexlbl' wdsetshow ": therearechoices
NB. obsolete  
NB. If there are stealth operands on the display, enable the nutton and caption it
NB. according to whether we are displaying them
  if. stealthopencountered do.
NB. obsolete     'fmshowstealth' wdsetcaption displaystealth {:: 'Show ][';'Hide ]['
    'fmshowstealth' wdsetvalue ": displaystealth
  end.
  'fmshowstealth' wdsetenable ": stealthopencountered
  
NB. stealthopencountered =: 0   NB. Set if there is a stealth op on the display
  
catch.
  smoutput 'error in paint'
  smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput 13!:12''
  NB. destroy the failing locale in case of error.  If we are performing the initial display, this will also reset
  NB. dissectinstance so that we aren't corrupted next time
  destroy''
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
for_o. ~. objtable do. destroy__o '' end.
NB.?lintmsgson
resultroot =: 0$a:
if. #winhwnd do.
  wd 'psel ' , winhwnd
  NB. Remember where the window was, so we can put the next one there
  setsessionyx''
  wd 'pclose'
  winhwnd =: ''  NB. not required
end.
dissectionlist_dissect_ =: dissectionlist_dissect_ -. coname''
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
'fmshowstealth' wdsetvalue ": displaystealth =: -. displaystealth
NB. The operand is the list of types that should NOT be displayed
calcallestheights__resultroot displaystealth # 1 2
dissect_dissectisi_paint 1
)

dissect_close =: 1&destroy
dissect_cancel =: dissect_close

dissect_fmfontsize_button =: 3 : 0
NB.?lintonly fmfontsize_select =. '0'
NB. obsolete minimumfontsizex =: 0 ". fmfontsize_select
('fmfontsize' , ": FONTSIZECHOICES {~ minimumfontsizex) wdsetvalue  '0'
('fmfontsize' , ": FONTSIZECHOICES {~ minimumfontsizex_dissect_ =: y) wdsetvalue  '1'
calccfms minimumfontsizex { FONTSIZECHOICES
dissect_dissectisi_paint 1
)
(4 : 0"0 i.@#) FONTSIZECHOICES
". 'dissect_fmfontsize' , (":x) , '_button =: dissect_fmfontsize_button@(', (":y) ,'"_)'
)

dissect_fmttfontsize_button =: 3 : 0
NB. obsolete minimumfontsizex =: 0 ". fmfontsize_select
('fmttfontsize' , ": TOOLTIPFONTSIZECHOICES {~ ttfontsizex) wdsetvalue  '0'
('fmttfontsize' , ": TOOLTIPFONTSIZECHOICES {~ ttfontsizex_dissect_ =: y) wdsetvalue  '1'
)
(4 : 0"0 i.@#) TOOLTIPFONTSIZECHOICES
". 'dissect_fmttfontsize' , (":x) , '_button =: dissect_fmttfontsize_button@(', (":y) ,'"_)'
)

NB. obsolete dissect_fmmaxnounsizex_select =: 3 : 0
NB. obsolete NB.?lintonly fmmaxnounsizex_select =. '0'
NB. obsolete maxnoundisplaysizex =: (0 ". fmmaxnounsizex_select) 1} maxnoundisplaysizex
NB. obsolete maxnoundisplayfrac =: 0.01 * MAXNOUNPCTCHOICES {~  maxnoundisplaysizex
NB. obsolete dissect_dissectisi_paint 1
NB. obsolete )
NB. obsolete dissect_fmmaxnounsizey_select =: 3 : 0
NB. obsolete NB.?lintonly fmmaxnounsizey_select =. '0'
NB. obsolete maxnoundisplaysizex =: (0 ". fmmaxnounsizey_select) 0} maxnoundisplaysizex
NB. obsolete maxnoundisplayfrac =: 0.01 * MAXNOUNPCTCHOICES {~  maxnoundisplaysizex
NB. obsolete dissect_dissectisi_paint 1
NB. obsolete )
dissect_fmmaxnounsize_button =: 4 : 0
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '0'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '0'
maxnoundisplaysizex =: y x} maxnoundisplaysizex
('fmmaxnounsizey' , ": MAXNOUNPCTCHOICES {~ 0 { maxnoundisplaysizex) wdsetvalue '1'
('fmmaxnounsizex' , ": MAXNOUNPCTCHOICES {~ 1 { maxnoundisplaysizex) wdsetvalue '1'
maxnoundisplayfrac =: 0.01 * MAXNOUNPCTCHOICES {~  maxnoundisplaysizex
dissect_dissectisi_paint 1
)
(4 : 0"0 i.@#) MAXNOUNPCTCHOICES
". 'dissect_fmmaxnounsizey' , (":x) , '_button =: 0&dissect_fmmaxnounsize_button@(', (":y) ,'"_)'
". 'dissect_fmmaxnounsizex' , (":x) , '_button =: 1&dissect_fmmaxnounsize_button@(', (":y) ,'"_)'
)

dissect_fmtooltipdelay_button =: 3 : 0
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0,~ tooltipdelayx) wdsetvalue  '0'
('fmtooltipdelay' , TOOLTIPDELAYCHOICES {::~ <0,~ tooltipdelayx_dissect_ =: y) wdsetvalue  '1'
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

dissect_fmhelplearning_button =: helpshow_dissecthelplearning_

dissect_fmhelpusing_button =: helpshow_dissecthelpusing_


NB. Draw the user's sentence at the top, showing highlighting
NB. x is the sentence, in the user's spacing
NB. y is a table of (token range);display level)
NB. The characters are drawn, with appropriate colors
sizesentence =: 4 : 0
usentence =. x
NB. Reselect in case explorers were drawn
wd 'psel ' , winhwnd
glsel 'dissectisi'

NB. Create table of token#,level.  Decrement token # to account for queue-end added at front.
NB. This also deletes any boxes of y that contain 0 token numbers - these will have been added for
NB. emulation purposes, for example vi@:u to handle &.
toklev =. ; (<:@,@[ ,"0 ])&.>/"1 > y

NB. Any missing #s should be parentheses.  Assign them the level of the lowest inside token.
NB. Assign level of _1 for (, _2 for ), and start with a stack of high-value for selection level.
NB. Process as a state machine.  Then use the stacks to fill in gaps in
NB. the selection levels
toklev =. /:~ toklev , ; (;: '()') (_1 _2 ,.~&.> <@(I.@:=)"0 1) tokens =. ;: usentence
assert. (-: i.@#) {."1 toklev
toklev =. {:"1 toklev
stack =: _
NB. This isn't too bad if the sentences are reasonable
lpval =. (4 : 'select. x case. _2 do. 0 [ stack =: _ , stack case. _1 do. (stack =: (<./@(2&{.) , }.) stack) ] (1 + {. stack) case. do. 0 [ stack =: (x<.{.stack) 0} stack end.')/\.&.(,&_) toklev
rpval =. (4 : 'select. x case. _1 do. 0 [ stack =: _ , stack case. _2 do. (stack =: (<./@(2&{.) , }.) stack) ] (2 + {. stack) case. do. 0 [ stack =: (x<.{.stack) 0} stack end.')/\.&.(,&_)&.|. toklev
toklev =. toklev + lpval + rpval
assert. 0 *./@:<: toklev

NB. Get the length of each token (except the last) in the user's spacing
tokulen =. 2 -~/\ (' ' +/\@:~: usentence) I. (>: |.!.0 +/\ ' '&(+/@:~:)@> tokens)

NB. Looking at pairs of tokens, insert after each the number of blanks needed to match the
NB. user's spacing.  Give this string the proper color: the selection level if both are the same, or
NB. _1 if they differ.  Result is token;level for token and following space.
NB. Handle the last token, which is never followed by anything.
addedblanks =. tokulen (' ' #~ (- #))&.> }: tokens
addedlevel =. 2 _1:^:~:/\ toklev
utokspacelevel =. (addedblanks ,. <"0 addedlevel) ({:@] ,~ (,/)@(,:"1~  }:)) tokens ,. <"0 toklev

NB. Remove empty strings.
utokspacelevel =. (#~ *@#@>@:({."1)) utokspacelevel
NB. Get the size of the rectangles.
rectsize =. (cfms =. satzcfm {~ (_2 + #satzcfm) <. > 1 {"1 utokspacelevel) sizetext ,. txts =. 0 {"1 utokspacelevel
NB. Box them into sections that fit within the allowed part of the screen, one box per line
scrwid =. <. MAXSENTENCEWIDTH * {: screensize
boxhw =. , (((}.~) (, $:)~^:(*@#@[) <@{.~)   1 >. scrwid I.~ (+/\@:({:"1))) rectsize
NB. Get the height of each line
lh =. >./@:({."1)@> boxhw
NB. Get the start of each line, which is zero here for left justification
lw =. 0
NB. Install starting yx, and move rects into horizontal position
rects =. ; (lw ,.~ |.!.0 +/\ lh) (] ,:~"1 (+"1    (0) ,. [: |.!.0 +/\@:({:"1)))&.:>"1 0 boxhw

NB. Get the max size for the string, and return the data for drawing
NB. Draw the strings
(>./ +/"2 rects);cfms;txts;rects

)

NB. Draw the sentence.  y is the result of sizesentence, except for the brect
NB. x is the yx of the start of the region
drawsentence =: 4 : 0
tlc =. x ,:0 0
'cfms txts rects' =. y
cfms drawtext txts ,. <"2 tlc +"2 rects
)


NORANKHIST =: 0 2$a:
NORANKHISTNOUN =: ''   NB. string means noun

NB. Conjunction for tree traversal down (i. e. root to leaves)
NB. y is the argument to pass to the first node
NB. x is the traversal type: 0 to stop before nouns, 1 to go through conjunction noun operands, 2 to go through everything, 3 to get tokens for the sentence
NB. u is the verb for applying to y at each level going down; its result will be the y for the next level
NB. v produces the result, and joins multiple results.  It is applied dyadically at leaf nodes or if a non-locale is returned from proplocales,
NB.  and monadically at interior nodes.  At the leaf, x is empty; for a non-locale interior node, x is a box.  At other interior
NB.  nodes, y is the result of applying u.
NB.
NB.
NB. NOTE that users of this conjunction must interpolate a named verb to call it, so that the current locale will not be modified
NB.
NB.
traversedown =: 2 : 0
assert. 'travdown monadic'
:
NB.?lintonly proplocales =: ]
if. #nloc =. x~ '' do.
  nloc ([:  v   (v (1&{::))`(u (2 : (':';'l ([ cocurrent)~ (u traversedown v)&>/ y [ (cocurrent x) [ l =. coname''''')) v)@.(2 = 3!:0@>@[)"0 _ ) x ,&< u y
else.
  '' v u y
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
propscroll =: 'propselstopatnoun'&((3 : 'if. y <: 0 do. scrollpoints =: 0 2$0 end. <: y') traversedown 0:)

NB. obsolete 
NB. obsolete NB. Propagate selection up the tree (leaves to root), until we hit a monad/dyad execution.  y is value to select
NB. obsolete propselup =: 3 : 'selections =: y if. -. ({. copath coname'''') e. ;: ''dissectmonad dissectdyad'' do. propselup__parent y end.'
NB. obsolete 
NB. init parent nodes in all objects.  This must be done in a separate verb because only a named verb resets the locale
NB. Called in locale of the base of the tree
initparentnodes =: 'propselall'&((3 : 'coname 0 # parent =: y') traversedown 0:)

NB. obsolete NB. Discard old selections.  Called in the locale of the base of the tree.
NB. obsolete NB. Selections in nodes at level y and above have any selections past number y removed
NB. obsolete discardselectionsabovelevel =: 'propselall'&((3 : 'if. sellevel >: y do. selections =: (y + (sellevel = y) *. ((sellevel+selectable) < #selections)) (] {.~ (<. #)) selections end. y') traversedown 0:)
NB. obsolete 
NB. Clear all selections.  Called in the locale of the base of the tree.
clearselections =: 'propselall'&((3 : 'selections =: 0$a: [ ishighlightnode =: 0') traversedown 0:)

NB. Set ishighlightnode for as many levels as there are selections (so clicking initialselection highlights expansion)
prophighlightnode =: 'propselall'&((3 : 'if. y >: 0 do. ishighlightnode =: 1 end. <: y') traversedown 0:)

NB. called after sniff to indicate which nodes can have an error display
setdisplayerror =: 'propselall'&((3 : 'errorwasdisplayedhere =: {. ".''*#DOstatusstring''') traversedown 0:)

NB. init SDT-display flag in all objects.  y is the value to set
NB. Called in locale of the base of the tree
initnounshowdetail =: 'propselall'&((3 : 'y [ nounshowdetail =: y +. -. resultissdt') traversedown 0:)

NB. calculate estheights for display.  We call estheights during the upwards traversal
NB. Called in locale of the base of the tree
calcallestheights =: 'propselall'&((3 : 'y [ dispstealthoperand =: {. stealthoperand -. y') traversedown (calcestheights@]))

NB. Return selection level for each token in the input string
NB. Result is table of (token number(s));selection level
NB. Called in locale at the base of the tree
gettokenlevels =: 'propseltokens'&((3 : '<displaysellevel') traversedown (3 : ('<;y';':';'<,:x,y')))


NB. common routines used by the object locales.  Objects are subclasses of dissectobj

cocurrent 'dissectobj'
coinsert 'dissect'

EMPTYPRH =: 3 0$a:
NOPHYSREQ =: 0$a:  NB. this matches NOLAYOUTS

NB. Defaults for switches set only in certain paths
rankcalculussupported =: 1

NB. The following names must be redefined when an object is cloned
clonenames_dissect_ =: ;: 'selections scrollpoints scrolltravelers displaysellevel winhwnd errorwasdisplayedhere pointoffailure sellevel selectable stealthoperand ishighlightnode endhighlightnode'

NB. Object creation.  create the signaling variables used for communicating with the grid.
NB. y is <the tokens that these values came from
NB. Each verb-type object is responsible for creating:
NB. titlestring: the name that will appear in the display at the top of a box
NB. stealthoperand: at create time, this is set to 0 for normal verb, 1=], 2=[, 3=[:, 5=] never displayed, 6=[ never displayed
NB. valence: when the verb gets a valence (i. e. when its monad/dyad exec happens), that valence is saved.
NB. estheights: number of blocks from the bottom of the noun node to the input(s) to this node.  This list has one atom per
NB.  valid operand. Set during setvalence, when we know the valence etc.  Height of <:0 is special, and indicates a stealthoperand like ] [,
NB.  where _1 is the ignored operand and 0 is the passed-through operand.  Height of _1 means the entire branch containing the
NB.  operand will be pruned; height 0 just means that there will be no display for the stealth operand
NB. displaysellevel: like #selections, but the value to use for display.  Noun operands of conjunctions are given the same level
NB.  as the verb, for pleasing display
create =: 3 : 0
coinsert COCREATOR
NB. The following names are not modified after the object is cloned:
titlestring =: ''
tokensource =: ~. > 0 { y

NB. The following names are guaranteed modified in the clone after this object is cloned:

NB. The following names are possibly modified after cloning.  Therefore, they must be copied into the clone
NB. when the clone is created, so that a mod to the original doesn't affect the clone.
(clonenames) =: (0$a:);(0 2$0);(2 2 2$0);0;'';1;($0);_;0;0;0;0

NB.?lintonly valence =: errorlevel =: snifferror =: 1
NB.?lintonly defstring =: ":
NB.?lintonly resultissdt =: nounhasdetail =: nounshowdetail =: 0
NB.?lintonly 'displayhandlesin displayhandleout displaylevrank fillmask' =: ($0);($0);(0 3$a:);($0)
NB.?lintonly dispstealthoperand =: 0
NB.?lintonly estheights =: ,1
NB.?lintsaveglobals
''
)

NB. Object destruction
destroy =: 3 : 0
NB. if there is a grid attached to this control, destroy it.
destroyexplorer''
codestroy''
)

NB. for debugging, verify that the execution string has valid syntax
NB. y is the string, and the result
auditstg =: 3 : 0
if. ~:/ '()' +/@:="0 _ y do.
  smoutput 'unbalanced parens'
  smoutput 'Error in exestring in locale ' , >coname''
  smoutput 'string: ' , y
  estring__ =: y
end.
y
)

names =: 4!:1

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
NB. Return the new locale
cl
)

NB. Switch object processor.  y is the name of the new object processor.
NB. Replace the current path with the path to y (including y)
NB. If x is given, it is the split point: locales before the split point
NB. are replaced by the new object; locales at the split point and after are kept as is.
NB. The default split point is 'dissectobj'
changeobjtypeto =: 3 : 0
'dissectobj' changeobjtypeto y
:
afteroldobj =. (<x) (i.~ }. ]) copath coname''
beforenewobj =. (<x) (i.~ {. ]) (, copath) boxopen y
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

NB. Init the logging table.  if y is 1 (default 0), create the dyad logging table that saves an additional value
NB. for each logged item
initloggingtable =: 3 : 0
logticket =: 0 $ 0
logvalues =: 0 $ a:
if. {. y do. logvaluesd =: 0 $ 0 end.
NB.?lintonly logticket =: logvaluesd =: 0$0 [ logvalues =: 0$a:
NB.?lintonly verbex =: ]
NB.?lintsaveglobals
''
)

NB. add to log.  Always called in the locale of the parse object.  x, if given, is the suffix to use for this logentry
NB. y is the value to log; it becomes the result
addlog =: 3 : 0
NB.?lintmsgsoff
logticket =: logticket , ticket__COCREATOR =: >: ticket__COCREATOR
logvalues =: logvalues , <y
NB.?lintmsgson
y
:
NB. Dyad: log x to the dyad area
logvaluesd =: logvaluesd , x
addlog y
)

NB. create string to use to add log entry.  If y is nonempty, it is the value to pass in as x
NB. The string produces a verb of infinite rank whose value is the same as its y
logstring =: 3 : 0
(')' ,~ ('((' , ')&' ,~ (5!:5 <'y'))&,)^:(*#y) 'addlog_' , (>coname'') , '_ '
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
NB. Return the selection value to be passed down to next level
y
)
NB.propsel0 =: 'propselstopatnoun'&((3 : 'y [ selections =: selectiontodisplay y') traversedown 0:)
propsel0 =: 'propselstopatnoun'&(installsel traversedown 0:)
propsel1 =: 'propselstopatxy'&((3 : ('displaysellevel =: y')) traversedown 0:)
propsel =: propsel1@# [ propsel0


NB. ************ fwd/bwd buttons **************
cocurrent 'dissect'

NB. When we make a selection, we add it to a list of selections.  The list is a table of
NB. level;selection;locale.  The list, and the verbs that manage it, are in the instance locale (which is the same as the
NB. locale of the form).  The entry point for propagating a selection, which is called from any object locale, switches to
NB. the  instance locale.

NB. Initialize
initselections =: 3 : 0   NB. called in instance locale
NB. The selections we have made, and those that we have undone
selectionsqueue =: 0 3$a:
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
NB. y is list of selections
NB. Called from object locales
makeselection =: 3 : 0
NB.?lintonly COCREATOR =. <'dissect'
NB. When we add a selection, it invalidates redo
selectionct__COCREATOR =: #selectionsqueue__COCREATOR =: (selectionct__COCREATOR {. selectionsqueue__COCREATOR) , sellevel ; y ;< coname''
NB. If we are still looking for errors, mark this selection as one we will keep when the user says 'way back'
if. 1=snifferror__COCREATOR do. selectionctatinitialerror__COCREATOR =: selectionct__COCREATOR end.
applyselection__COCREATOR ''
0 0$0
)

cocurrent 'dissect'

NB. Look at the undo list and establish the state indicated there.  We process each selection
NB. starting at the beginning.  The last selection will be the node that gets the highlight.
NB. We process each selection in turn.  If the selection went through the endhighlightnode, we
NB. mark the locale of the selection as an inhighlightnode (which will enable highlighting for it)
applyselection =: 3 : 0   NB. runs in instance locale
clearselections__resultroot 0
if. selectionct do.
  NB. Get the locale of the last selection.  This node, and selectors contributing to it, will be enabled for highlighting
  NB.?lintmsgsoff
  lastsel =. (<(<:selectionct),2) {:: selectionsqueue
  NB.?lintmsgson
  NB.?lintonly lastsel =. <'dissectobj'
  for_s. selectionct {. selectionsqueue do.
    endhighlightnode__lastsel =: 1
    'lvl sel loc' =. s
    NB.?lintonly loc =. <'dissectobj'
    propsel__loc (sellevel__loc {. selections__loc) , sel
NB. obsolete     if. endhighlightnode__lastsel ~: 1 do. ishighlightnode__loc =: 1 end.
    NB. If we click on an initialselection, we must activate highlighting in the expansion also.  Calculate the # levels to extend highlighting
    if. endhighlightnode__lastsel ~: 1 do. prophighlightnode__loc (#selections__loc) - sellevel__loc + selectable__loc end.
  end.
  NB. Tidy up for next time, leaving no endhighlightnode set
  endhighlightnode__lastsel =: 0
end.
NB. Set button enables
wd 'psel ',winhwnd
'fmfwd' wdsetenable ": selectionct < #selectionsqueue
'fmbwd' wdsetenable  ": selectionctatinitialerror < selectionct
'fmshowerror' wdsetenable  ": selectionctatinitialerror < selectionct
0 0$0
)

NB. Undo one selection, moving it to the redo list
dissect_fmbwd_button =: 3 : 0   NB. runs in instance locale
NB. If nothing to undo, do nothing
if. selectionctatinitialerror >: selectionct do. 0 0$0 return. end.
selectionct =: <:selectionct
applyselection''
NB. Refresh the display
traverse 0
dissect_dissectisi_paint 1
)

NB. Move one selection from the redo list to the undo list, and execute it
dissect_fmfwd_button =: 3 : 0   NB. runs in instance locale
if. selectionct = #selectionsqueue do. 0 0$0 return. end.
selectionct =: >:selectionct
applyselection''
NB. Refresh the display
traverse 0
dissect_dissectisi_paint 1
)

NB. obsolete NB. The 'show error' button displayes the error state
NB. obsolete dissect_fmshowerror_button =: 3 : 0
NB. obsolete traverse 1
NB. obsolete dissect_dissectisi_paint 1
NB. obsolete )
NB. obsolete 
NB. Cut back to the initial selection: none or starting error
dissect_fmshowerror_button =: 3 : 0   NB. runs in instance locale
if. selectionctatinitialerror >: selectionct do. 0 0$0 return. end.
selectionct =: selectionctatinitialerror
applyselection''
NB. Refresh the display
traverse 0
dissect_dissectisi_paint 1
)

NB. ***************** traverse down ****************
cocurrent 'dissectobj'

NB. utilities for traversal and selection
NB. These are overridden as needed by individual modifiers.  The versions here work for simple verbs

NB. get the rank to use for this verb.
NB. Result is the rank to use for the verb's valence, or $0 if we don't know
getverbrank =: 3 : 0
if. ifdefined 'verbex' do.
  (valence { 0 1 _2) {. verbex b. 0
else.
  $0
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
NB. before execution errors; ortherwise we follow the J order
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
(errorcodenames =: ;:'ENOUN EOK ENOAGREE EFRAMINGABORT EFRAMINGEXEC EABORTED EEXEC EFRAMING ENOEXECD EUNEXECD ENOOPS ENOSEL EINVALIDOP EINVALIDVERB') =: _1 + i. 14
EEARLYERROR =: ENOAGREE,EINVALIDOP,EINVALIDVERB
NB. If there were results to display, we will create a fillmask for them.  The cases follow:
EHASVALIDFILLMASK =: ENOUN,EOK,EEXEC,EFRAMING,EUNEXECD,EFRAMINGEXEC
EHASFILLMASK =: EHASVALIDFILLMASK   NB. there are results, and a fillmask
EFAILED =: EEARLYERROR,EABORTED,EEXEC,EFRAMING,ENOEXECD,EUNEXECD,EFRAMINGABORT,EFRAMINGEXEC  NB. incomplete execution
EALLFRAMING =: EFRAMING,EFRAMINGABORT,EFRAMINGEXEC   NB. framing error, with or without others
EGENERR =: EEARLYERROR,EFRAMINGABORT,EFRAMINGEXEC,EABORTED,EEXEC,EFRAMING
EPROPERR =: ENOEXECD,EUNEXECD
ESHOULDINHERIT =: ENOUN,EOK,EEARLYERROR,EFRAMINGABORT,EFRAMINGEXEC,EABORTED,EEXEC,EFRAMING,ENOEXECD,EUNEXECD  NB. This node adds to the knowledge of u@v
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
NB. fillmask - this has the shape of the open of frame $ result, and gives status for each atom thereof.  This status is the selection
NB.  level of this node (in the upper bits), and validity information in the lower bits.  The validity is
NB.  0=normal 1=fill 3=first unexecuted cell (presumably error, but that may depend on what happened elsewhere) 2=later unexecd cell
NB.  error is calculated per result cell & propagated to atoms; fill is calculated per atom.  fillmask is valid only for nouns, or if the
NB.  unselected result has a frame with multiple cells, and is undefined otherwise
'FILLMASKNORMAL FILLMASKFILL FILLMASKUNEXECD FILLMASKERROR' =: i. 4
FILLMASKNOCOLLECT =: 4
FILLMASKCHECKER =: 1 bwlsl FILLMASKNOCOLLECT
FILLMASKSELLEVEL =: 1 bwlsl FILLMASKCHECKER
NB. errorlevel - a copy of errorlevel__COCREATOR at the time this is parsed, this indicates whether we were in a try block during
NB.  execution of this verb.  When it comes time to display error info, we don't use the result failure type for anything except
NB.  top-level errors
NB. opselin - the operand selections made on the current node.  This is initialized to empty here, and added onto whenever a layout is drawn
NB. vranks - the rank(s) of the operand. 1 or 2 numbers, but empty for a noun or for a verb that didn't execute
NB. rankhistory - 1{y holds the table of previous ranks.  We append vranks to it, to produce the rank stack for this operand.  The format of the rank stack is
NB.  (string form of name to display);(locale (NOT boxed));(rank r)[;(rank l)
NB.  If the string form is instead 0, it means that this rankhistory is a 'heavy' locale marker that should always float down to u operands
NB. resultlevel - indicates boxing of result: '' = none, normal; 0 = result replaces ops in hierarvhy (L:); 1 = box is added (&.>)
traversedowncalcselect =: 3 : 0
assert. 1 = #$y
assert. 3 <: #y
NB. Initialize the locales where detail is to come from.  We will inherit these locales from u as long as u is valid
'sellevel rankhistory selopinfo' =: 3 {. y
physreqandhighlights =: {.@> selopinfo
inputselopshapes =: selopshapes =: , }.@> selopinfo
errorlevel =: errorlevel__COCREATOR
opselin =: 0 2$a:  NB. initialize opselin to empty (=no selection)
vranks =: getverbrank selopshapes
NB. Non-atomic sellevel is a flag indicating that the nominal rank is to be ignored, because the verb runs at infinite rank
NB. (used in ^:_1 because u has a rank, but u^:_1 always has infinite rank)
if. #$sellevel do.
  vranks =: _:"0 vranks
  sellevel =: {. sellevel
end.
if. #vranks  do.  NB. forceinfinite overrides the observed verb ranks
NB. obsolete   rankhistory =: rankhistory , (;:^:_1^:(0<L.) titlestring) ; <"0 sellevel , |. vranks
  rankhistory =: rankhistory , (;:^:_1^:(0<L.) titlestring) ; (coname'') , <"0 |. vranks
end.
selectable =: 0  NB. Init unselectable unless we set otherwise
selx =. a:  NB. In case we don't set it, we need this to pick up 'all logvalues' for display purposes
NB. If a compound is bypassed for display (for example, u@:v where u fails, holding some data), we may display
NB. u rather than u@:v.  But then, selection will leave out the u@:v locale.  So, each locale keeps the
NB. name of the locale in which propsel should start; this is set to u@:v in this case
NB.
NB. inheritroot will always point to the base of the inherirtance tree: the node with the finest detail
inheritroot =: coname ''
NB. We have an inheritance chain, which ends in the stubs in 'dissect' locale
inheritedto =: inheritedfrom =: <'dissect'
NB. initialselection is set for expansion nodes, to indicate where a click will cause an expansion,
NB. and what the initial value should be
initialselection =: 0$a:
qprintf^:DEBTRAVDOWN 'snifferror__COCREATOR%,loc=?>coname''''%,type=?0{::copath coname''''%defstring 0%>uop%>vop%>cop%vranks%sellevel%selections%$y%y%rankhistory%'
qprintf^:DEBHLIGHT 'snifferror__COCREATOR%,loc=?>coname''''%,type=?0{::copath coname''''%defstring 0%y%sellevel%selections%'
if. 3 = #y do.
NB. No selector: we can't do much
  'selframe frame frames arglevel resultlevel errorcode selresult selector selopinfovalid' =: ($0);($0);a:;($0);($0);ENOSEL;(0$a:);(0$a:);0 0
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
    qprintf^:DEBTRAVDOWN '#logvalues '
NB. selector and selop already set, keep them
    NB. Since nouns appearing in u&v (ex: =&(i."0) are executed twice, so in that case
    NB. discard all but the first one.
    if. 1 < #logvalues do. logvalues =: 1 {. logvalues end.
    'selframe frame arglevel resultlevel selresult' =: ($0);($0);($0);($0);<logvalues
    errorcode =: (*#logvalues) { ENOEXECD,ENOUN
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
    NB. No ranks, must be a noun.  Since nouns appearing in u&v (ex: =&(i."0) are executed twice, so in that case
    NB. discard all but the first one.
    if. 1 < #logvalues do. logvalues =: 1 {. logvalues end.
    NB. If there are NO logvalues, there must have been an error creating the verb - some invalid form like +@2
    if. 0 = #logvalues do.
      NB. There were no verb ranks, but we are treating this as a failing verb; so add a line to rankhistory for it
NB. obsolete        rankhistory =: rankhistory , '';sellevel
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
    NB. Here for verb with agreement error
    'errorcode selector selopinfovalid selresult arglevel resultlevel frames selframe frame' =: ENOAGREE;(0$0);(0:"0 physreqandhighlights);(0$a:);($0);($0);execdframes;execdframe;execdframe
  elseif. a: -: 'selframe frame frames resultlevel arglevel' =: selopshapes calcdispframe execdframes do.
    NB. Partitioning verbs can detect agreement on infinite-rank operands eg.  (1 ]/. 2 3)
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
  elseif. a: -: selector do.
    SM^:DEBTRAVDOWN'rank-calculus probe'
NB. There were no selectors.  This means that rank calculus was applied somewhere to give us a shape without
NB. a valid selector.  The frame is valid, as just calculated, and the operand shapes too, provided this is a regular verb
    'errorcode selresult selopinfovalid' =: ENOOPS;(0$a:);(rankcalculussupported"0 physreqandhighlights)  NB. leave selector unchanged
    NB. do the rank-calculus processing of the shape, and whether we can accept a selection
    selopshapes =: ($^:(0<L.)&.> selopshapes) (}.~ #)&.> frames
    selectable =: selframe +.&*&# resultlevel
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
    qprintf^:DEBTRAVDOWN '$selopshapes $&.>selopshapes selopshapes $frames frames $frame frame resultlevel arglevel $selx '
    if. 0 e. frame do.
NB. Execution on a cell of fills.  We should have 0 or 1 result.  If 0, it means that the
NB. execution on the cell of fills failed, and we will use a scalar numeric as the replacement
      assert. 0 1 e.~ #selx
NB. create a result of the required type and shape
      if. 0 = #selx do. selresult =: ,<0 else. selresult =: , ({.selx) { logvalues end.   NB. error in fill cell - use scalar numeric
NB. we will extend fill-cell with frame
NB. Keep selector unchanged, since there was just one cell in the operand and there still will be
    else.
NB. Not fill cell.  If there is no error, we should have just the right number of results
NB. TEMP kludge!!  Error is not fatal, if we are in adverse or chasing a fill-cell.  Only too many results is always bad
      assert. ((*/ frame) > #selx) +. (*/ frame) = #selx [ 'travdowncalcselect'  NB. used to include crashed__COCREATOR
NB. If the frame is valid, but we didn't get enough results, it means something died in this verb;
NB. mark this verb as requiring detail and set the selector (if any) to select the failing item, which
NB. will be the first item we did NOT create.  OR, it could mean that we are executing on a cells of fills, which
NB. might terminate with a error, which would be ignored.
NB. See if all the cells executed
      if. (*/ frame) > #selx do.
        smoutput^:DEBTRAVDOWN 'incomplete execution'
NB. Cells did not execute.  If the selector is the one we found when sniffing errors, the error is in this cell.
NB. Mark this cell as an execution error.  But if the selector is different, there is no permanent error here: either the user
NB. changed the selector to a later cell which didn't execute anything, or we have determined that this cell
NB. was partially-executed because of an error elsewhere, and there's no error here.  In those case, we mark
NB. the cell as partially-executed, with no error
        if. (1 = snifferror__COCREATOR) *. 0 = errorlevel do.
NB. See if incomplete operation represents failure.  It does for a verb, but not for something like u@v.  In general
NB. we detect the failure at the same point J would: so 1.5 u/ y would fail on u/
          if. operationfailed'' do. setfailurepoint selector end.
NB. Continue narrowing the search for the error
NB. If there is a frame, select the first non-executing cell.  For us to get here, any selectable higher levels
NB. must have selected, so we will be adding to a selection chain.
          if. (resultlevel +.&*&# selframe) do.
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
NB. obsolete             propsel (sellevel {. selections) , < selframe getfailingisf #selx
            makeselection , < selframe getfailingisf #selx
            qprintf^:DEBTRAVDOWN 'edisp'''' $selections selections '
          end.
        end.
NB. Set the errorcode: if we are at the failure point, indicate the appropriate type of error; otherwise
NB. just call it unexecuted
        errorcode =: (#. (selector -: pointoffailure) , cellswereexecuted #selx) { ENOEXECD,EUNEXECD , EABORTED,EEXEC
      end.
NB. We need to save the selected value.  We use this to calculate the predicted frame after collection.
      selresult =: selx { logvalues NB. This is the (unopened, since it might not collect) result from this object's verb, in execution order
    end.
    
NB. Handle selection for the next level
NB. Set flag indicating whether this node can take a selection
    selectable =: selframe +.&*&# resultlevel
NB. If the selection trims down the selection of results, apply that trim to the selector.
NB. If we are sniffing and this verb failed, the final selection would fail
NB. by definition; we will have handled that case above.  We increment sellevel whenever there is a frame, so that even nonselecting
NB. frames show up in the shape display; but empty frames do not have selectors (to make the color sequence predictable).
NB. Thus, we have to skip the selection when the frame is empty
NB. We also ignore a forced selection (ex: u/ when y has 2 items), which shows up as an empty selector.  We leave the selector, so that
NB. we get predictable sellevels, but it is known not to be needed (i. e. it is created only when we have seen that the current
NB. selection is forced, on a previous traversal).  If we get a change of selection this gets reexamined.
    'seltype thissel' =. getselection rawselx  NB. classify the type of selection.  Selections not normally needed (recursion uses them)
qprintf^:DEBTRAVDOWN'seltype thissel sellevel selections '
    select. seltype  NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only, 4=autoselect of node with no frame
    case. 2 do.
NB. Forced selection: if this is the first time we see it, perform the forced selection, propagating it to lower nodes
NB. obsolete       if. unforcedselection'' do. propsel (sellevel {. selections) , thissel end.
      if. unforcedselection'' do. makeselection , thissel end.
    case. 1 do.
NB. If forced selection, don't apply thissel, because it would result in multiple ranges.  Just keep
NB. the selector we had.  Here we select for the unforced selection
NB. Calculate the selection interval corresponding to each selected result.  Put result intervals into selection order, then choose one
NB. But if there is 0 in the frame (meaning we ran on a cell of fills), selection is perfunctory and we will simply
NB. keep the old selector
      if. -. 0 e. frame do.
        selector =: <^:(0=L.) thissel selectusingisf tickettonatural frame $ selector selectticketintervals rawselx
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
      NB. we can lose the map structure of selopshapes, it it has any
      selopshapes =: selopinfovalid calcunselectedshapes selopshapes
    else.
      NB. if highlighting is enabled, even a forced one, use it to calculate highlighting and the operand shapes after selection
      NB. Some of the objects require calcphys to run always (to set globals), so we do that before we see if we will use the highlight
      newp =. calcphysandhighlights thissel
      NB. Whether we keep the highlight or not, honor the change-of-valence it represents, by replicating the old highlight as needed
      physreqandhighlights =: physreqandhighlights ($&.|.~ #) newp
      qprintf^:DEBHLIGHT'ishighlightnode '
      if. ishighlightnode do.
NB. obsolete         physreqandhighlights (($&.|.~ #) ,"1&.> ]) ,&(<sellevel)&.> newp
        physreqandhighlights =: physreqandhighlights ,"1&.> ,&(<sellevel)&.> newp
        qprintf^:DEBHLIGHT'physreqandhighlights '
      end.
NB. recalculate selopshapes now that we have the selection
      selopshapes =: calcselectedshapes thissel
      selopinfovalid =: 1:"0 selopshapes  NB. Selection means input shapes are valid for next v
    end.
    assert. ((0<L.) +. (1=#@$))@> selopshapes
  end.
  
end.
assert. 1 = #$selresult
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
    fillmask =: ((FILLMASKNOCOLLECT * collecterror) + FILLMASKSELLEVEL * sellevel) + tickettonatural frame $!.FILLMASKUNEXECD (FILLMASKNORMAL #~ #selresult) , (errorcode e. EEXEC,EUNEXECD,EABORTED,ENOEXECD) # FILLMASKERROR

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
        if. (1 = snifferror__COCREATOR) *. 0 = errorlevel do. setfailurepoint selector end.
        errorcode =: EFRAMING
        resultlevel =: 2   NB. Signify 'collection error'
      end.
    end.
NB. debug qprintf 'collected%frame%$L:0 selresult%selresult%y%'
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

qprintf^:DEBTRAVDOWN 'edisp'''' frame selframe $selresult selresult $selresultshape selresultshape selector selopinfovalid fillmask selections rankhistory selectable '
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
    maxsize ; ((0;' ';a:;(s:'');(u: ' ');0) {::~ classes i. 1) ; -. *./ (-:"1 {.) exshapes
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
NB. If we are trying to create a valid selresult out of nothing, just give it the shape of the frame
elseif. 0 = #y do. 0:"0 x
elseif. do.
  select. resultlevel
  NB. If the fillmask is boxed, the selresult is an expansion, or has a level, or is uncollectable.  In all
  NB. those cases, don't unbox the selresult, just box it into the shape/map of the fillmask.  But we still have
  NB. to bring it to the correct shape, and we add fill (empty boxes) in case the execution was short.
  case. 0 do.
    NB. If this is L:, we must assemble the result using the result map (which has the same structure as the
    NB. fillmask)
    NB.?lintonly resultseqmap =: ''
    resultseqmap (>@{) L:0 _ frame $!.a: y
  case. 1;2 do.
    tickettonatural ($x) $ y
  case. do.
  'cs fill fillreqd' =. checkframing y  NB. result cell size, fill atom (empty if unframable)
    NB. This is where we add fill as required
    <"0@(cs&{.)@>`>@.(#fill) tickettonatural ((-#cs) }. $x) $!.(<cs $ {.!.' ' fill) y
  end.
end.
)


NB. Use the fillmask to give the color for each cell.  Low-order 2 bits are 0=normal 1=fill 2=error 3=unexecd;
NB. bit 2 is set if uncollectable;
NB.  bit 3 is 0 (filled in by checkerboard); higher bits are selection levels
NB. We just add in the checkerboard
NB. x is the upper limit on selection level (after checkerboard added), y is the fillmask to create a checkerboard for
NB. if y is boxed, this must be a selection node, and we recur on the selected node, to put the checkerboard there
checkerboardfillmask =: 4 : 0
assert. 0 = L. y
sel =. ((x * FILLMASKSELLEVEL) <. (-FILLMASKCHECKER) bwand y) bwor (<:FILLMASKCHECKER) bwand y
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
NB. Result is a table, one per node, each row being sellevel;ISF for the node;locale for the node (no extra box)
NB. The ISF contains the frame and any SFOPENs called for by the resultlevel
NB. Forced selections are replaced by empty frame
accumframe_dissect_ =: (0 3$a:)"_
accumframe =: 3 : 0
QP^:DEBDOvn'accumframe for ?defstring]0%>coname''''%selframe%unforcedselection''''%sellevel%selections%'
NB. Keep taking frames as long as they are valid, i. e. as long as we have selected from them using a
NB. selector that is not a rank-calculus probe.
NB. We have to patch over the first node of monad/dyad, which looks like a noun and has no selops out;
NB. the following verb will be OK
NB. obsolete if. sellevel > #selections do. 0 3$a:  NB. < means selection exists; = means on deck; > means in the hold
if. (*#vranks) *. (0 = #selopshapes) +. (selector -: a:) do. 0 3$a:  NB. keep frames as long as there is valid shape
else.
  (sellevel ; ((<(unforcedselection'') # selframe) , (1 -: resultlevel) # SFOPEN) ; coname'') , accumframe__inheritedfrom 0
end.
)
NB. Signal early error
NB. Agreement error requires insertion of a node showing the location of the error.  For the nonce,
NB. we will abort traversal at that point.
NB. y is the dol (x operand to traverse)
NB. result is a suitable return value from traverse, viz y ,&< locale
earlyerror =: 3 : 0
NB. Since we abort the traversal, roll up the failing part and install it as the last name in the rank stack
'displayhandlesin displayhandleout displaylevrank' =: (valence { '';(,0);_0.3 0.3),1;<(<defstring 0) (<_1 0)} rankhistory
y ,&< coname''
:
NB. The dyad is used for errors detected during traversal, eg u@.v where v creates a non-atom,
NB. or datatype errors.  We might be able to give a better description of the error.
NB. x is errorcode[;description string].  We set the errorcode and, if the string is given,
NB. we replace the J string with it
'ecode estg' =. 2 {. x
errorcode =: ecode
if. 2 = 3!:0 estg do. errormessagefrominterp__COCREATOR =: estg end.
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

NB. Append locale x (default=current locale) to the chain ending in locale y
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

DISPINFO =: ;: 'displayhandlesin displayhandleout displaylevrank dispstealthoperand'
inheritu =: 3 : 0
loc =. 1 {:: y
NB.?lintonly loc =. <'dissectobj'
SM^:DEBINHU 'inheritu: in ' , (>coname'') , ' ' , defstring 0
QP^:DEBINHU'$floc >loc defstring__loc]0 edisp'''' edisp__loc'''' >selector selresult '
QP^:DEBINHU'fillmask fillmask__loc selresult selresult__loc selectable sellevel selections resultlevel resultlevel__loc '
QP^:DEBDOL2'physreqandhighlights physreqandhighlights__loc '
NB. The display information is always inherited from the last u, which creates it.
NB. The only time we wouldn't inherit is if the error is detected before the last u, example 1.5 u/ y which
NB. would detect it on u/.  We detect that by the error-point codes
if. errorcode -.@e. EABORTED,EEXEC do. (DISPINFO) =: ".@(,&'__loc')&.> DISPINFO end.
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
  end.
  
NB. If u has the error source then u@v should have failed short.   If u@v has no results,
NB. inherit the u locale to replace it
elseif. errorcode__loc e. EGENERR do.
  assert. errorcode e. EPROPERR [ 'u@v died but u@v was OK'  NB. if u died, u@v should be sick
  if. errorcode e. EHASFILLMASK do.
NB. Inherit the fact of failure, but preserve existing data.  If we failed framing or agreement, pass that up the line
    errorcode =: (#.(errorcode__loc e. EEARLYERROR) ,(errorcode__loc e. EALLFRAMING) , errorcode e. EHASVALIDFILLMASK) { EABORTED,EEXEC,EFRAMINGABORT,EFRAMINGEXEC,4#errorcode__loc  NB. Inherit the error indic
  else.
NB. u@:v has no result - replace it with u, provided u has real data
    replaceresult =. errorcode__loc e. EHASFILLMASK
    NB. We also have to inherit the status
    errorcode =: errorcode__loc
  end.
elseif. (errorcode__loc = ENOEXECD) *. (errorcode = EUNEXECD) do.
NB. If u@v had results but u didn't, the explanation must be that v failed (perhaps we should signal a different error code for u).
NB. We will have put out the error on v, so we suppress it on u.  But if this is an expansion node (which we can detect because the
NB. fillmask exists and is boxed), we will keep the display so we can select results
  if. 0 = L. fillmask do.
    errorcode =: ENOEXECD
  end.
elseif. (errorcode__loc = EUNEXECD) *. (errorcode = ENOEXECD) do.
NB. u executed incompletely, but u@v not at all??  Yes, it must be that there was an error, but we
NB. selected off the error path, so now we don't go through GENERR.  Replace u@v with u (actually the
NB. NOEXECD is probably from a monad/dyad exec)
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
    fillmask =: frame $!.(<FILLMASKUNEXECD) < fillmask__loc
  case. do.
    NB. In other cases, format the selresult according to ITS settings, and then save it according to the way it will
    NB. be formatted (i. e. by collection using the fillmask)
    NB. In all cases, we have a lower fillmask whose shape matches its result; we replicate this using the
    NB. frame of the upper node.
    fillmask =: frame $!.(<^:(*L.fillmask__loc) FILLMASKUNEXECD) ,: fillmask__loc
    if. #resultlevel__loc do.
      NB. If the lower node has resultshape, that means that each atom of the fillmask has the structure for the
      NB. corresponding atom of the selresult, so we box those atoms and run them into a list.
      selresult =: , <"0 fillmask__loc frameselresult__loc selresult__loc
    else.
      NB. If the lower node does not have resultshape, its result shape matches its fillmask, and we box that value
      NB. to be the sole value for the final result (it will be filled when this result is collected)
      NB. We box the lower fillmask (in case it has structure) and extend it with UNEXECDs
NB. obsolete      selresult =: , < fillmask__loc frameselresult__loc selresult__loc
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
  if. selectable *. (sellevel < #selections) do.
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
      if. 1 >: #$>tmodx do. if. (>tmodx) >: #selresult do. selresult =: (>:tmodx) {. selresult end. end.
      fillmask =: (< fillmask__loc) sel1} fillmask
      selresult =: (< fillmask__loc frameselresult__loc selresult__loc) tmodx} selresult
    case. do.
      NB. Normal fillmasks, which may or may not be boxed (they will be boxed if they contained some boxed detail such as
      NB. L: or each)
      NB. We install the lower fillmask directly into the upper.  No change is made to selresult.
      NB. We expand the lower fillmask to the size of a cell of the upper.
      NB. If the fillmasks have different boxing status, we box atoms of whichever is unboxed
      NB. The value to use for filling cells in the u fillmask depends on the errorcode for u.  If there
      NB. is no error, it's just normal fill
      select. * fillmask ,&L. fillmask__loc
      case. 1 0 do. fillmask__loc =: <"0 fillmask__loc
      case. 0 1 do. fillmask =: <"0 fillmask
      end.
      sel1 =. {. sel1   NB. only 1 atom allowed; make it an atom
      fillval =. <^:(*L.fillmask__loc) (FILLMASKSELLEVEL * sellevel) + (FILLMASKUNEXECD,(2#FILLMASKERROR),FILLMASKFILL) {~ (EUNEXECD,EEXEC,EFRAMINGEXEC) i. errorcode__loc
      fillmask =: (((#>sel1) }. $fillmask) ([ {.!.fillval (({.!.1 $)~ -@#)~ ($,) ]) fillmask__loc) sel1} fillmask
    end.
NB. Perhaps we should modify selresult here too?
  else.
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

NB. called in locale of an operand
NB. null rankcalculus for cases where we can take no action
NB. Nilad.  We know that the current node has no selection but has valid selopshapes.
NB. Result is 2 if it is OK to turn this into a rank-calculus probe, 0 if not.
rankcalculus =: 0:
NB. y is selopinfovalid or a selection thereof (suitable for v-types, where validity comes from the selected operands)
NB. result is operand validity: 0 if operand shapes are invalid, 1 if valid, 2 if valid but not selected, and this should turn into a rank-calculus probe
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
NB. result is operand validity: 0 if operand shapes are invalid, 1 if valid, 2 if valid but not selected, and this should turn into a rank-calculus probe
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

NB. Omnibus generator of operands to traverse
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
NB. If x is given it is a gerund that is used to modify the value of rankhistory that we use
TRAVOPSSTARTINHEAVY =: 0&(,&<)
TRAVOPSKEEPINALL =: 1&(,&<)
TRAVOPSKEEPINLIGHT =: 2&(,&<)
TRAVOPSSTARTHEAVY =: TRAVOPSSTARTINHEAVY a:
TRAVOPSKEEPALL =: TRAVOPSKEEPINALL a:
TRAVOPSKEEPLIGHT =: TRAVOPSKEEPINLIGHT a:

travops =: 3 : 0
]`'' travops y
:
'rhsel pphys val shapes' =. 4 {. y
'sval oval' =. val
opx =. 4 {:: y , <a:  NB. Indexes of operands we are using
NB. Calculate the rankhistory to use
NB. obsolete if. rhsel -: '' do. rh =. NORANKHIST else. rh =. rhsel {"1 x`:6 rankhistory end.
'rhcull rhcols' =. rhsel
NB. If we start heavy or keep light, cull the list down to those elements
select. rhcull
case. 0 do.  NB. start (always keeping heavy)
  rh =. 2 {."1 (#~ (<0) = 0&{"1) rankhistory
case. 1 do.  NB. keep all
  rh =.  x`:6 rankhistory
case. do.  NB. keep light
  rh =. (#~ (<0) ~: 0&{"1)  x`:6 rankhistory
end.
NB. Now select the requested columns
rh =. rhcols {"1 rh
NB. Choose the physreqs to use
if. #$pphys do.
  NB. pphys specified for each operand.  In case this is a change of valence, make sure we preserve the correct number of highlights (keeping y over x),
  NB. and fill the rest with empties
  opinfo =. pphys { physreqandhighlights , <EMPTYPRH
elseif. pphys do.
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

NB. Custom selection, used in picking.  If this returns 1, it means that the pick has been handled in the locale
selectionoverride =: 0:

NB. **************** code for display objects *********************

cocurrent 'dissect'

NB. ****************** place-and-route for wires ***********************

ROUTINGGRIDSIZE =: 5   NB. number of pixels between routing channels
WIRESTANDOFF =: 4  NB. min number of pixels between a wire and a block
ROUTINGTURNPENALTY =: 3   NB. number of blocks of penalty to assign to a turn
ROUTINGOCCUPANCYPENALTY =: 3   NB. number of blocks of penalty to assign to an overlap (trialroute only)
ROUTINGNEIGHBORPENALTY =: 1   NB. number of blocks of penalty to assign to a neighboring route
MINBOXSPACING =: 3 * ROUTINGGRIDSIZE  NB. Number of pixels between boxes, minimum


ROUTINGMARGIN =: 4   NB. min number of wire spacings to leave around border.  Used to calc routing area
NB. We need 1 for roundup of points, 1 that we use to mark the boundary for comp ease, 2 to allow 2 wires in

NB. y is gridsize.  Create verbs to use
initgridverbs =: 3 : 0
yxtogrid =: %&y
gridtoyx =: *&y
''
NB.?lintsaveglobals
)

NB. occupancy tells which object (if neg) or the number of routes (if nonneg) fill the cell.  We keep different counts
NB. for ns and ew
NB. Convert each gridblock to units of gridsize, covering the gridpoints that are within a standoff
NB. y is gridblocks;nets
initgrids =: 3 : 0
'gridblocks nets' =. y
NB.?lintonly 'gridsize standoff penalties' =: 5 5 5 [ 'gridblocks nets' =. (0 2 2$0);<0 $a:
rareasize =: (standoff + gridsize * >: ROUTINGMARGIN) + >./ {:"2 gridblocks
NB. Create top-left,:bottom-right+1 in grid units for each block
gscblocks =. <. yxtogrid gridblocks +"2 (2) #"0 (gridsize-standoff),standoff+gridsize
NB. Create the row;column vector for each block
gscrowcolvec =: (+ i.)&.>/@(-~/\)"2 gscblocks
NB. Negative occupancy indicates which object occupies the cell; object # - # objects
occupancy =: (<. yxtogrid rareasize) $ ,: 0 0
for_b. gscrowcolvec do.
  occupancy =: (b_index - #gscblocks) (<b)} occupancy
end.
NB. Verify that there is a routing margin all the way around
assert. *./ 0 = (<0 _1) ,&,&({&occupancy) (<a:;0 _1)
NB.?lintsaveglobals
)


NB. Place-and-route line drawer for dissect
NB. Given a block placement and a set of interblock connections, we adjust the placement
NB. as needed and create the vector necessary to connect the blocks
NB. Result is the new placement (yx only), and the list of wires as table of startyx,endyx,type (0=line 1=arc)

NB. x defines the grid: size;gridsize;standoff;penalties
NB.  size is ymax,xmax
NB.  gridsize is spacing between lines
NB.  standoff is min distance between a block and a line
NB.  penalties is penalty for a turn (in units of movement)
NB. y defines the objects:  gridblocks;nets
NB.  gridblocks is a list of (topleft y,x,:bottomright+1 y,x)
NB.  nets is a list of boxes, each containing one net, a table, as source,dest...
NB.  each of source/dest is (# of gridblock),(# of face),fraction of edge displacement from center
NB.   face # 0=top, 1=bot, 2=left, 3=right
routegrid =: 4 : 0
'gridsize standoff penalties' =: x
turnpenalty =: {. penalties
'gridblocks nets' =. y
NB. Create verb to convert input y,x to grid coordinates
initgridverbs gridsize
NB. Calculate the routing area size.  Adjust blocks to leave a minimum top/left margin,
NB. and create the routing area to leave a right/bottom margin
gridblocks =. gridblocks +"1 ([: <. 0 >. ROUTINGMARGIN&-)&.(%&gridsize) (<./ {."2 gridblocks) - WIRESTANDOFF
NB. We will perform the trial place-and-route, saving all the results so we can choose the one we like best
gridblocks =. ,: gridblocks
placementscores =. $0
while. do.
NB. Perform a trial route
  initgrids ({:gridblocks);<nets
NB. Route the nets, building up occupancy as needed
  1 routenets ({:gridblocks);<nets
NB. Score the placement: 1 point for a crossing, a zillion for occupancy>1
  placementscores =. placementscores , score =. (1000000 * 1 +./@:< , occupancy) + +/@, 1 1 -:"1 occupancy
NB. If the placement is perfect, or we have gotten the max occupancy OK and have tried enough, stop looking
NB. MAXTRIALROUTES is the 3 here
QP^:DEBROUTE'placementscores '
  if. (score = 0) +. (score < 1000000) *. (3 <: #placementscores)  do. break. end.
NB. Not perfect.  Adjust the placement for the next try.
  if. score >: 100000 do.
NB. We had occupancy > 1 in some cell.
NB. Move the blocks to leave extra space where the occupancy exceeds 1
NB. Look at the ew occupancy.  Create a state machine to process from the bottom up.  Result is the
NB. number of bump-ups needed at each position; reset when each new object is encountered.  So the
NB. number of bump-ups is in the cell BELOW the bottom of the object
NB. First we convert to (_high-value if neg, surplus occupancy otherwise);
NB. Then we add up the surplus occupancy, resetting when we hit _high-value
    bumpups =. (0 >. +)/\.   ((0 >. <:)  + _1000000 * 0&>) 1 {"1 occupancy
  else.
NB. No problem with occupancy too high, but there were wire crossings.  See if more space will get rid of them.
NB. We will add at most one space per empty area to try to avoid crossings.
NB. The lookup table produces 1 if both values are >0, _1000000 if either value negative, 0 otherwise
    bumpups =. 1 <.  (0 >. +)/\.  crosspt =. (0 0 _1000000 0 1 _1000000 _1000000 _1000000 _1000000  ) {~ 3 3 #. * occupancy
  end.
NB. Now, process the blocks from the bottom up.  Each block will look at the bump-up count from the state
NB. machine, which gives the number of cells needed since the last object was processed, and then add
NB. to that the number of bumpups from the last-processed block in its column.  The max, over the columns
NB. of the object, is the number of bumpups for the object (which we save in each column of the bumpup table
  bumpsinlastobj =. (1{$occupancy)$0
  bumpsinobjns =. (#gscrowcolvec) # 0
  for_b. \: gscrowplus1 =. >: {:@(0&{::)"1 gscrowcolvec do.
    cols =. (<b,1) {:: gscrowcolvec
    bumpsinlastobj =. (bumps =. 0 >. >./ ((<(b { gscrowplus1);cols) { bumpups) + cols { bumpsinlastobj) cols} bumpsinlastobj
    bumpsinobjns =. bumps b} bumpsinobjns
  end.
NB. Repeat for columns
  if. score >: 100000 do.
    bumpups =. (0 >. +)/\.&.|:   ((0 >. <:)  + _1000000 * 0&>) 0 {"1 occupancy
  else.
NB.?lintonly crosspt =. 2 2 $ 0
    bumpups =. 1 <.  (0 >. +)/\.&.|:   crosspt
  end.
  bumpsinlastobj =. ({.$occupancy)$0
  bumpsinobjew =. (#gscrowcolvec) # 0
  for_b. \: gsccolplus1 =. >: {:@(1&{::)"1 gscrowcolvec do.
    rows =. (<b,0) {:: gscrowcolvec
    bumpsinlastobj =. (bumps =. 0 >. >./ ((<rows;(b { gsccolplus1)) { bumpups) + rows { bumpsinlastobj) rows} bumpsinlastobj
    bumpsinobjew =. bumps b} bumpsinobjew
  end.
NB. Now move the blocks the specified number of grid positions - by moving all the OTHER blocks down (to avoid negative placement)
  gridblocks =. gridblocks , ({: gridblocks) +"1"2 1 gridtoyx (-"1~ >./) bumpsinobjns,.bumpsinobjew
QP^:DEBROUTE'bumps:?gridtoyx(-"1~>./)bumpsinobjns,.bumpsinobjew '
end.
NB. Use the placement with the best score
bestx =. (i. <./) placementscores

NB. Reinit the grid variables
initgrids (bestx { gridblocks);<nets

NB. Route again, this time to create the final routing
wires =. 0 routenets (bestx { gridblocks);<nets
NB. Return the block placement and the wires to draw
((<bestx;a:;0) { gridblocks);wires
NB.?lintsaveglobals
)

NB. lookup to convert a face number into fetch indexes to get start,end
NB. Indexes to fetch from ystart,xstart,:yend,xend to give y1y2,:x1x2
facex =: 4 2 2 2 $ 0 0  0 0  0 1  1 1    1 0  1 0  0 1  1 1   0 0  1 0  0 1  0 1   0 0  1 0  1 1  1 1
NB. perform a routing pass
NB. x is starting grid;trial flag
NB. y is gridblocks;nets
NB. trial flag is 1 if it is OK to route over old routes.  We just add up the number of times a cell is assigned to a net
NB. Result is table of wires: start,end,type (0=line 1=arc)
routenets =: 4 : 0
trialroute =: x
'gridblocks nets' =. y
QP^:DEBROUTE'<.gridblocks%gridsize '
NB. Init to no output wires
wires =. 0 5$0
if. #nets do.
NB. Convert the nets from gridblock,face,fraction to y,x,face.  We leave the direction parallel to the face
NB. on a grid boundary; the other is allowed to float
  fnets =. ; nets   NB. Flatten the nets for proc ease; will reconstitute later
  y12x12 =. (facex {~ 1&{"1 fnets) (<"1@:[ { ])"3 2 gridblocks ({~ (0&{"1)) fnets
NB. Find the center point; round to grid; clamp to within interval (the clamp will ensure no movement along perp direction
  centers =. ({."1 >. {:"1 <. [: <.@(0.5&+)&.(%&gridsize) -:@:(+/"1)) y12x12
NB. The amount to move is the given fraction of the face, rounded toward 0 to a multiple of gridsize, which we achieve by adding a small
NB. sign-dependent amount which should be good enough for screen resolutions
  adjust =. (2 {"1 fnets) <.@(0.5&+ + 1e_6 * *)&.(%&gridsize)@:* -~/"1 y12x12
  nets =. (; (1 {.~ #)&.> nets) <;.1 (centers + adjust) ,. 1 {"1 fnets
  
NB. Sort nets to route shorter ones first.  This might reduce crossings?
  nets =. (/:   +/@(>./ - <./)@:(2&{."1)@>) nets
end.
NB. Route the nets, one by one.  Accumulate the wires
for_n. nets do.
  nt =. >n
NB. Convert any directly-drawable wires to wires
  ddrawmsk =. (source =. {.nt) directdrawok dests =. }. nt
  wires =. wires , 0 ,.~ source ,&}:"1 ddrawmsk # dests
NB. Route any that are left
  if. #routedests =. (-. ddrawmsk) # dests do.
    wires =. wires , routenet source , routedests
  end.
end.
NB. Return list of wires
wires
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

NB. Angle ranges for allowable lines, startpoint to endpoint, in the screen's upside-down y coordinates
angleranges =: 12 o. _10j_1 _1j_10 1j_10 10j_1 10j1 1j10 _1j10 _10j1
NB. OK range values for each type of first face
facerange =: 1 2 3 3 , 5 6 7 7 , 7 8 0 1 ,: 3 4 5 5
NB. OK range values for each type of second face
revfacerange =: 1 0 3 2 { facerange
NB. x is source, y is table of destinations.  Each is y,x,face
NB. Result is mask, one bit per destination, indicating that it is OK to draw that wire directly
directdrawok =: 4 : 0
NB. Create the angle of each wire
angles =. angleranges I. 12 o. j.~/"1 y -"1&:(2&{."1) x
NB. See if the angle is in the OK range for the source face, and if the negative is in the OK range for the target face
angleok =. (angles e. (2{x) { facerange) *. angles e."0 1 (2 {"1 y) { revfacerange
NB. Allow the wire if no vertices in the box containing the wire's corners (adjusted inward a smidgen)
NB. We move the wire away from the face by one gridunit more than the calculation used to move the corners.
NB. Then we use those values as corners, and look to see whether the region is clear.  We allow direct routing if so.
movedpoints =. <. yxtogrid (0 1&{"1 +"1 ((, |."1) (0 ,~ -standoff) ,: (0 ,~ standoff+gridsize)) {~ 2&{"1) x , y  NB. Note other comp produced end+1; so does this
interiorok =. 0 = (0 > {."1 occupancy) +./@:,;.0~ ({. (<. ,: >:@:|@:-)"1 }.) movedpoints
angleok *. interiorok
)

NB. y is face direction, result is y,x move perpendicular and away from the face
faceperpdir =: _1 0 , 1 0 , 0 _1 ,: 0 1
NB. y is face direction, result is which of nsew is toward
nsewtoward =: 1 0 3 2
NB. y is a move, from faceperpdir; result is direction nsew
dirtodistx =: {&faceperpdir^:_1
NB. the possible turn directions from each direction index
turntable =: 2 2 2 $ 1 0 _1 0    0 1 0 _1
NB. Route one net
NB. y is a net (table of y,x,face)
NB. Globals in use: net number; trial flag; gridblocks
NB. Result is modified routing grid;table of lines
routenet =: 3 : 0
NB. Initialize the grid.  Set distances to a high value, unless not-trial and occupied, in which case _1.
NB. Set distances to _2 if too far outside the routing rectangle.
NB. The direction of movement is 1/_1 in one axis, 0 in the other.  The routing grid is set up
NB. to map 1/0 (North) to 0, _1/0(S)=1, 0/1(E)=2, 0/_1(W)=3
routdist =. 2 #"1 (*occupancy) { 1000000 , (trialroute{_1 1000000) , _2
NB. Protect the outer border to keep the routes from getting out of hand
routdist =. _10 (<<0 _1)} routdist
routdist =. _10 (<a:;<0 _1)} routdist

NB. Create the routing positions of the source and dests.  Since we have placed the handle on a gridpoint
NB. in the perp-to-face direction, it can function as the true end-of-route in that direction.  The routing position, which must
NB. be a gridpoint, is the closest gridpoint on the boundary or outside.  This will necessarily have
NB. the same parallel coordinate as the true routeend, but the other coordinate may be adjusted.
trueroutend =. <. 2 {."1 y
routend =. <. yxtogrid trueroutend + (gridsize-1) * 1 bwand  faces =. 2 {"1 y
QP^:DEBROUTE'routenet:routend=?routend '
NB. Set the distance to all endpoints as high-value, to allow moving to that boundary point.  Use a flag value so we know when we ended
NB. Record a distance of 0 to the source, so that our initial condition is, we just moved to the source point
NB. ?should do this only in correct direction?
NB. The end-of-route moves must be perpendicular to the face and away from it at the source, or towards it
NB. at the dest.  Only that direction should be initialized with 0 or high-value
initdir =. (0}~  1 bwxor {.) faces { nsewtoward
routdist =. ((#routend) {.!.2000000 (0)) (<"1 routend,.initdir)} routdist

NB. Set the source as the current point, perpendicular to the face.  Clear list of waiting points
actpoints =. ,: ({. routend) ,: ({. faces) { faceperpdir
NB. Delay line for turns.  We put turns in before any processing, so there is a wait stage for each atom
waitpoints =. turnpenalty $ < 0 2 2 $ 0
NB. Delay line for penalties.  We record the penalty in routdist before adding the point to the delay line,
NB. so there has to be one extra atom here so the point isn't removed as soon as it is added
penaltypoints =. (>: trialroute { ROUTINGOCCUPANCYPENALTY,ROUTINGNEIGHBORPENALTY) $ < 0 2 2 $ 0
neighborofst =. _2 ]\ 0 0   1 0   _1 0   0 1   0 _1   NB. penalize any route with these neighbors
NB. Repeat until all destinations have been routed:
currdist =. 1
ndestsfound =. 1  NB. the starting point has automatically been routed
if. DEBROUTE do.
routedeblog =: 0$a:
end.   NB.?lintonly routedeblog =: 0$a:
while. ndestsfound < #routend do.
  NB. Activate points that were put in the penalty box for being close to another route.
  NB. When they come back into play they are allowed to turn.
  actpoints =. actpoints , (0 {:: penaltypoints)
NB. For each active point/direction, create the turn points/directions
NB. The turn point is BEFORE the step (in the new direction) to the new position
  turns =. < ,/ (+/"2 actpoints) ,:"1 ((<a:;1;0) { actpoints) { turntable
  
  NB. Activate turn points that have come to life.  These points come to life in a halfway position,
  NB. and have to make a move to get to their final position.  So they can't turn when they come to life.
  if. #actpoints =. ~. actpoints , (0 {:: waitpoints) do.
NB. Advance each active point to the next position
    actpoints =. +/\."2 actpoints
NB. Fetch distance to target.  Delete next-points that are have been filled in this direction
    targx =. ({."2 ,. dirtodistx@:({:"2)) actpoints
    assert. 1 4 e.~ 3!:0 targx
    
NB. Fetch the values we are moving to and see if any are destinations
    if. 2000000 e. targval =. targx (<"1@[ { ]) routdist do.
NB. We reached a destination.
NB. See which destinations we reached.  Make sure each destination is reached only one time
      reachdests =. (#~ ~:@:(2&{."1)) targx #~ targmsk =. targval = 2000000
QP^:DEBROUTE'found:reachdests=?reachdests '
NB. Set the current distance in one destination point; mark all the other parts of the
NB. destination as regular points so we don't reach them again
      routdist =. 1000000 (<"1 }:"1 reachdests)} routdist
      routdist =. currdist (<"1 reachdests)} routdist
NB. Count the number of destinations filled.
      ndestsfound =. ndestsfound + #reachdests
NB. Remove the destination points from the active list
      actpoints =. (-. targmsk) # actpoints
      targval =. (-. targmsk) # targval
      targx =. (-. targmsk) # targx
    end.
    
NB. As a perf boost, cull the points with a stored distance better than we can get here
    
    if. #actpoints =. actpoints #~ valmsk =. currdist <: targval do.
NB. Keep the other values in lockstep
      targval =. valmsk # targval
      targx =. valmsk # targx
      
NB. If the new points are close to a boundary, give them a penalty
NB. If the new position is close to a parallel route, penalize it one tick (might be too much)
NB. remove the point from the active list and list it as a penalty
NB.    During trialroute, we care only about direct conflict.  During real route, penalize us for a having a neighbor
      if. trialroute do.
        penaltymask =. 0 < (({."2 ,. 0 = (<1 0)&{"2) actpoints) (<"1@[ { ]) occupancy
      else.  NB. Regular route, penalty if any neighbor occupied
        penaltymask =. 0 < +/@,"2 (({."2 actpoints) +"1/ neighborofst) (<"1@[ { ]) occupancy
      end.
      neighbors =. < penaltymask # actpoints
      actpoints =. (-. penaltymask) # actpoints
      
NB. Mark the next-points as filled at this step and keep them on the active list
NB. We have to include the routing penalty in the stored distance, lest the return route
NB. not realize this path has a penalty; and this
      newdist =. currdist + penaltymask * <:#penaltypoints   NB. putative new value
      valmsk =. newdist <: targval

if. DEBROUTE do.
routedeblog =: routedeblog , < (valmsk # targx) ,. valmsk # newdist
end.
      routdist =. (valmsk # newdist) (<"1 valmsk # targx)} routdist
    else. neighbors =. < 0 2 2 $ 0
    end.
  else. neighbors =. < 0 2 2 $ 0
  end.
  
NB. Add turns to the waiting queue, penalties to the penalty queue
  waitpoints =. (}. waitpoints) , turns
  penaltypoints =.  (}. penaltypoints) , neighbors
  
NB. tick the clock
  currdist =. currdist + 1
if. DEBROUTE do. savroutdist =: routdist end.
  assert. currdist < 1000
end.
NB. Starting at each dest, extract lines for all nets that were drawn, and mark occupancy for the net
wires =. 0 5 $0
for_d. }. routend,.faces do.
NB. get the starting position / direction for the wire
  wirestart =. currpos =. 2 {. d
NB. initialize the distance that got us to the endpoint
  currdist =. <./ (<wirestart) { routdist
NB. Init the variables that we will use to calculate next step:
NB. The direction of movement, initialized to perpendicular to the destination face.  Add this to currpos to get nextpos
  dir =. (2{d) { faceperpdir
NB. The layer number corresponding to the OPPOSITE of the direction of movement (we are
NB. going back up the wire; the distance was stored for the other direction when we came down the wire
  currlayer =. dirtodistx -dir
NB. 0 if we are going ns, 1 if ew
  ew =. 0={.dir  NB. current direction of movement
NB. The distance from nextpos to the turn positions (these are directions, amounts to add to nextpos)
  turndir =. ew |."1 (0 ,. 1 _1)  NB. If we are moving ew, turns are ns offsets
NB. The layer number corresponding to the OPPOSITE of the direction of movement after each turn
  turnlayer =. dirtodistx -turndir
  while. currdist > 0 do.  NB. we have just moved into currpos
NB. Indicate that we have moved to the place we have just stepped into
    routdist =. 0 (<currpos,currlayer)} routdist
NB. Note that also in the occupancy table
    occupancy =: (>: (<currpos,ew) { occupancy) (<currpos,ew)} occupancy
    
NB. See whether we should go straight or turn.  We have to look at all the distances
NB. and choose the smallest (straight if equal), after giving the straight move the benefit of no turn penalty.
    straightdist =. (<(nextpos =. currpos + dir),currlayer) { routdist
    turnpos =. nextpos +"1 turndir
    turndist =. (turnpos ,"1 0 turnlayer) (<"1@[ { ]) routdist
    if. (straightdist >: 0) *. straightdist <: turnpenalty + <./ (#~ >:&0) turndist do.
NB. Go straight.
      currpos =. nextpos
      currdist =. straightdist
    else.
NB. Can't go straight, must be a turn.  Find the turn direction
NB. Out the wire for the straight part, if any
      if. currpos -.@-: wirestart do. wires =. wires , (gridtoyx wirestart,currpos) , 0 end.
      olddir =. dir   NB. Save old direction for canonicalizing turn
      dir =. (turnx =. >/ (+  1000000 * 0&>) turndist) { turndir
NB. Out the wire for the turn.  Orient the lines so it is a CW 90-degree arc segment
NB. Arc format here is centerpoint,cornerpoint of 90-degree CW arc.
NB. The center is the from point in the old direction of motion, and the to point in the
NB. new direction of motion
      arccenter =. +/ (0 ~: olddir ,: dir) * currpos ,: (turnx { turnpos)
NB. The startpoint is the 4th corner of the square made up of center, old, and new
      arccorner =. (currpos + (turnx { turnpos)) - arccenter
      wires =. wires , (gridtoyx arccenter,arccorner) , 1
      wirestart =. currpos =. turnx { turnpos
      currdist =. turnx { turndist
      
NB. Since we have changed the direction, refigure all the direction-related vars (copied from above)
      currlayer =. dirtodistx -dir
      ew =. 0={.dir
      turndir =. ew |."1 (0 ,. 1 _1)
      turnlayer =. dirtodistx -turndir
    end.
  end.
NB. Out the last wire if any, after appending the true endpoint
  if. currpos -.@-: wirestart do. wires =. wires , (gridtoyx wirestart,currpos) , 0 end.
end.

NB. Append wires to connect the true endpoints to the routing endpoints
wires =. wires , (gridtoyx routend) ,. trueroutend ,. 0
NB. Return the lines
wires
NB.?lintsaveglobals
)

NB. Utility for scanned routedeblog
NB. y is a tlbr, result is the boxes of routedeblog, culled to that window
culldeblog =: 3 : 0
(2 2 $,y)&(] #~ (([: *./"1 [: >/"1 <:"1"2 1)  2&{."1) )&.>  routedeblog
)


NB. display occupancy
occ =: 3 : 0
(0 0,:_ _) occ y
:
if. -. ifdefined 'trialroute' do. trialroute =. 1 end.
if. trialroute do.
  ((0 <: <./) {  '*' , ":@(>./))"1 (2 2 $,x) ];.0 occupancy
else.
  ((0 <: <./ ) { '*' , ' -|+' {~ [: #. 0&>.)"1 (2 2 $,x) ];.0 occupancy
end.
)
NB. display route
rout =: 3 : 0
(0 0,:_ _) rout y
:
n =. x ];.0 ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 0&{)"1 y
s =. x ];.0 ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 1&{)"1 y
e =. x ];.0 ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 2&{)"1 y
w =. x ];.0 ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 3&{)"1 y
n ,"1 '   ' ,"1 s ,"1 '   ' ,"1 e ,"1 '   ' ,"1 w
)
routd =: 3 : 0
(0 0,:_ _) rout y
:
n =. ": (2 2 $,x) ];.0 (0&{)"1 y
s =. ": (2 2 $,x) ];.0 (1&{)"1 y
e =. ": (2 2 $,x) ];.0 (2&{)"1 y
w =. ": (2 2 $,x) ];.0 (3&{)"1 y
QP 'n s e w '
)


NB. *************** end of router - start of display-object management **************
RGBTOLUMINANCE =: +/@:*"1&0.2989 0.5870 0.1140


SCROLLBARWIDTH =: 14  NB. width of scrollbar in pixels
SCROLLBARCOLOR =: <192 192 192   NB. color for scrollbar - no pen
SCROLLBARENDCOLOR =: <240 240 240
SCROLLBARENDTHICKNESS =: 10
SCROLLBARTRAVELERCOLOR =: <128 128 128

FONTNUM =: '"Courier New"'    NB. Font for 'data' - numeric data, shape, rank, etc
FONTCHAR =: '"Lucida Console"'   NB. Font for 'char' - verb names, noun names, status messages
FONTIMSG =: '"Arial"'    NB. Font for easy readability - tooltips, error messages

NB. for the verb-name cell
VERBCOLOR =: 114 30 30
VERBTEXTCOLOR =: 255 255 255
VERBFONT =: FONTCHAR
VERBFONTSIZE =: 0
VERBMARGIN =: 1
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
NB. for the user's sentence
SATZCOLOR =: 190 190 190
SATZTEXTCOLOR =: 0 0 0
SATZFONT =: FONTNUM
SATZFONTSIZE =: 10
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
TOOLTIPFONT =: FONTIMSG
NB. obsolete TOOLTIPFONTSIZE =: 8
TOOLTIPMARGIN =: 1


NB. The shape colors/textcolors give the main data colors for the selection level
SHAPECOLORS =: ".;._2 (0 : 0)
200 200 255
000 255 000
255 000 255
000 255 255
255 000 000
255 255 000
)
SHAPETEXTCOLORS =: 0 0 0"1 SHAPECOLORS
NB. for the shape display of the (filled) result cell
RESULTSHAPECOLOR =: 100 50 200
RESULTSHAPETEXTCOLOR =: 255 255 255
RESULTSHAPEFONT =: '"Courier New"'
RESULTSHAPEFONTSIZE =: 0
RESULTSHAPEMARGIN =: 1

NB. Data colors are like the shape colors, but there is a checkerboard effect, so each
NB. odd color has the background dimmed a little.  The first (unselected) data color
NB. is different from the shape color, to keep them separate
DATACOLORS =: (255 255 255 (0}) SHAPECOLORS)

NB. Now spread out the data colors, providing the checkerboard
DATACOLORS =: <. ,/ DATACOLORS  *"1/ 1 1 1 ,: 0.88 0.83 0.88

DATATEXTCOLORS =: 0 0 0"1 DATACOLORS

NB. The colors for each level of highlighting.  The first highlight contrasts with normal
NB. data; thereafter we rely on the vivid colors to contrast, and we match each highlight with
NB. its shape selector; but we reeduce the intensity to a max value to ensure contrast with the
NB. background.
HIGHLIGHTCOLORS =:  <. (*    1 <. 110 % RGBTOLUMINANCE) (0 0 0 (0}) SHAPECOLORS)


FRINGECOLOR =: (128 128 128 , 200 200 0 , 255 0 0 ,: 255 255 255) ;"1 (0 0 0 1)   NB. color/border of fringes: in order label,shape,status,data

DOBORDERCOLORS =: _3 ]\ 0 0 255 0 0 255  0 0 0  255 0 0   NB. Black border for box, but red if incomplete, blue if empty
RANKCOLOR =: 0 0 255  NB. color of dashed line for high-rank ops
BOXBORDERCOLOR =: 0 0 0  NB. color of lines between boxes


NB. Highlight must specify pen width and style
HIGHLIGHTLINEWIDTH =: 2
HIGHLIGHTBORDERSTYLE =: 0 0 0,HIGHLIGHTLINEWIDTH,PS_DOT  NB. color,width,style of lines for highlight in operands (dashed)
SELECTIONBORDERSTYLE =: 0 0 0,HIGHLIGHTLINEWIDTH,PS_SOLID  NB. color,width of lines for highlight in the clicked result (solid)


WIRECOLOR =: 0 0 0   NB. Color of wires

BOXMARGIN =: 2 ($,) 2   NB. Space to leave around boxed results
BOXLINEWIDTH =: 2 ($,) 1  NB. Width of lines making boxes

EMPTYEXTENT =: <@,"0 ] 5 5   NB. Size to use for displaying empty

MAXDATASIZEYX =: 200 200

NB. Create cfms for later use.  y is the font size selected.
NB. called in locale of main instance, leaves names defined there
NB. Operands to drawtext/sizetext
NB. These take colors from the selectors, except for the first one, which tells part of speech, and the last
NB. few, which are status/unexecd/fill
calccfms =: 3 : 0
nouncfm =: < NOUNCOLOR;NOUNTEXTCOLOR;NOUNFONT;(y+NOUNFONTSIZE);NOUNMARGIN
nouncfm =: nouncfm , < (SHAPECOLORS ;"1 SHAPETEXTCOLORS) ,"1 SHAPEFONT;(y+SHAPEFONTSIZE);SHAPEMARGIN
nouncfm =: nouncfm , < STATUSCOLOR;STATUSTEXTCOLOR;STATUSFONT;(y+STATUSFONTSIZE);STATUSMARGIN
nouncfm =: nouncfm , < (DATACOLORS ;"1 DATATEXTCOLORS) ,"1 DATAFONT;(y+DATAFONTSIZE);DATAMARGIN

verbcfm =: < VERBCOLOR;VERBTEXTCOLOR;VERBFONT;(y+VERBFONTSIZE);VERBMARGIN
verbcfm =: verbcfm , < (SHAPECOLORS ;"1 SHAPETEXTCOLORS) ,"1 SHAPEFONT;(y+SHAPEFONTSIZE);SHAPEMARGIN
verbcfm =: verbcfm , < STATUSCOLOR;STATUSTEXTCOLOR;STATUSFONT;(y+STATUSFONTSIZE);STATUSMARGIN
verbcfm =: verbcfm , < (DATACOLORS ;"1 DATATEXTCOLORS) ,"1 DATAFONT;(y+DATAFONTSIZE);DATAMARGIN

RESULTSHAPECFM =: RESULTSHAPECOLOR;RESULTSHAPETEXTCOLOR;RESULTSHAPEFONT;(y+RESULTSHAPEFONTSIZE);RESULTSHAPEMARGIN
NB.?lintsaveglobals
''
)

NB. For the displayed sentence
satzcfm =: ((SATZCOLOR (0}) SHAPECOLORS) ;"1 (SATZTEXTCOLOR (0}) SHAPETEXTCOLORS)) ,"1 SATZFONT;SATZFONTSIZE;SATZMARGIN
satzcfm =: satzcfm , SATZCOLOR;SATZTEXTCOLOR;SATZFONT;SATZFONTSIZE;SATZMARGIN



NB. *********** create DOs
cocurrent 'dissectobj'


NB. y is a brick of rectangles yx,:hw, or an array of boxes containing rects at some level,
NB. or a single rect (which becomes the bbox)
NB. Result is bounding rect, a single yx,:hw
brect =: (<./@:({."2) ,: >./@:({:"2))&.:(+/\"2)^:(2<#@$)@(>@:(<S:0)^:(0<L.))


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
  bonly =. spacing + > {."1 sbbox =. findbbox@> 1 1 }. y
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
NB.  formatinfoused is information about the format that was finally selected:
NB.   pointsize
NB.  valueformat is shape;y endpixels;x endpixels[;array of boxed subnouns, with shape of the boxed noun, each box containing a DOL]
NB.   y endpixels is the ending position of each row in the display.  The number of atoms will be */ (even items of shape, counting from the end)
NB.    a gap of 1 pixel is left between blocks of rank 3, 5, etc.  This is included in the y endpixels values
NB.   x endpixels is similar for x
NB.   subnouns is provided only for boxed selresult.  It gives the valueformat for each box of the represented part of selresult.
NB.
NB.  The actual result of createDOL is the size of the allocated data area, in pixels, hw
createDOL =: 3 : 0
NB. The monad is what is called externally.  It uses the top-level values saved in the locale
formatinfoused =: y
NB. Create the data for the selresult.  We have to try to collect it, because the verb may have encountered
NB. an error before we tried to collect it, possibly leaving an uncollectable result (i. e. it would have
NB. had a framing error if it survived long enough to collect).  If collection succeeds, we use the
NB. collected value; if not, we box the atoms, collect that, and use it.  When we come to display
NB. the uncollectable value we will suppress the outer boxing in the display.
NB. If collection succeeds, we fill out an incomplete execution with fills of the appropriate type;
NB. if collection fails, we fill with spaces (OK since the values are boxed immediately, and we don't want
NB. the fills to have text)
(fillmask frameselresult selresult) createDOL formatinfoused  NB. top result is boxed
NB.?lintsaveglobals
:
NB. The dyad does the work, and calls itself if the value is boxed.  The dyad returns
NB. valueformat, which is (unused);y endpixels;x endpixels[;subDOLs]
'font fontsize margin' =. y
value =. x
NB. If the noun is empty, we can't very well calculate its display size,
NB. so just use a canned size
if. 0 e. $value do.
  'subDOLs rcextents' =. (0$a:);<EMPTYEXTENT
else.
NB. If the noun is boxed, get a DOL for each box; extract the height/width from it
NB. and add left/right margin and left line; if the CONTENTS
NB. was also boxed, add the right margin for its closing line
  if. 32 = 3!:0 value do.
    hw =. (BOXLINEWIDTH + +: BOXMARGIN) +"1 (extractdatasize + BOXLINEWIDTH * 3 < #)@> > subDOLs =. < createDOL&y&.> value
  else.
NB. If the noun is not boxed, just get the height/width for each atom
NB. We also come here for the top level, which is boxed because it might not collect
    subDOLs =. 0$a:  NB. no subnouns unless boxed
    glfontextent font , ": fontsize
    hw =. (+/ 2 2 ($,) margin) +"1 |."1 glqextent@":@> value
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
end.
NB. Assemble final result
($value);rcextents,subDOLs
NB.?lintsaveglobals
)

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
'cfmlabel cfmshape cfmstatus cfmdata' =. DOcfm
QP^:DEBDOvn 'createDOvn:?> coname''''%defstring 0%stealthoperand%'
NB. If this node is a stealth operand, whether displayed or not, remember the fact so we can give the user the option of showing it
assert. stealthoperand e. 0 1 2 3 5 6
if. stealthoperand e. 1 2 do. stealthopencountered__COCREATOR =: 1 end.
NB. If stealth verb, there is no display; but because of inheritance and suppressed detail, we might have the stealthoperand flag
NB. set in a locale that is creating a noun; we'd better create that.  We detect nouns, as usual, by absence of handles in
if. (dispstealthoperand e. 1 2 5 6) *. (*#displayhandlesin) do. dispstealthoperand return. end.

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
NB. table; contains string;level;rank[;rank].
NB. Audit the rank to enforce descending order (empty values have no effect); then we
NB. reset empty cells in the original to space; then delete rows that have no effect in the
NB. rolled-up value.  Rows (except the first) containing only infinities are known to have no effect
NB. Remove lines with no display symbol
  nonemptylevrank =. 2 ({."1 ,. |.@:}."1) (#~   ('';0) -.@:e.~ {."1) displaylevrank
  rolledlevrank =. <./@,&.>/\. &.(,&(<_)) &.|. 2 }."1 nonemptylevrank
  newrankmsk =. 1:"_1 rolledlevrank =. (a: = 2 }."1 nonemptylevrank)} rolledlevrank ,: <' '
  DOranklevels =. (<:#cfmshape) <. (3 : 'sellevel__y'"0) DOranklocales =: newrankmsk # 1 {"1 nonemptylevrank
  DOranks =: (":&.> newrankmsk # rolledlevrank) (}:"1@[ ,. ] ,. {:"1@[) newrankmsk # {."1 nonemptylevrank
  DOrankcfm =: 1 0 1 {"2^:(3={:$DOranks) cfmlabel ,:"1 (DOranklevels { cfmshape)
  rankrects =. DOrankcfm sizetext"1 0 DOranks
NB. Make the left rank left-justified, the right rank right justified.  Align each stack
NB. vertically({."1 displaylevrank) ({."1@] ,. [ ,. }."1@])
  namedesc =. (<ALIGNCENTER) addalignmentgroup ,. (<ALIGNCENTER)&addalignmentgroup"1 (ALIGNCENTER,ALIGNLEFT) addalignmentrect rankrects
end.


NB. Account for error string, if any; 0 0 if none
NB. If we are not in a try block, allow display of error only at the place where the error was detected
NB. during sniff.  This handles the case where the user makes a selection after sniff, and then there is
NB. no error detected at the point of error, and the enclosing conjunction shows its error.
if. errorwasdisplayedhere +. *errorlevel do.
  DOstatusstring =: ((6 1 3 1#'';'agreement';'framing';'invalid verb'),(*errorlevel) { errormessagefrominterp;'error') {::~ (ENOUN,EOK,ENOEXECD,EUNEXECD,ENOOPS,ENOSEL,ENOAGREE,EFRAMING,EFRAMINGABORT,EFRAMINGEXEC,EINVALIDVERB) i. errorcode
else.
  DOstatusstring =: ''
end.
if. #DOstatusstring do.
NB. If this failure is in a try path, parenthesize the error
  if. errorlevel do. DOstatusstring =: '(' , DOstatusstring , ')' end.
  thw =. cfmstatus sizetext <DOstatusstring
else. thw =. 0 0
end.
statusdesc =. ALIGNSPREAD addalignmentrect thw

NB. Create the shape/selector line if we can
NB. The shape is the concatenation of the frames, so that in an expansion node it includes the expansion.
NB. We also append the shape of the max result cell in the last node, to get the total shape of the result
NB. Create displayable frame for each selection.  This is a list of boxed strings
DOshapes =: <@;@(": L:0)@;/./ |: 0 1 {"1 af =. accumframe__inheritedtailforselectinfo''
DOshapelocales =: {:/./ |: 0 2 {"1 af   NB. The locale that makes the selection
NB.QP'DOshapes DOshapelocales af '
if. #af do.
  NB. The last locale returned by accumframe is the last one that had valid output, i. e. the last one whose
  NB. frame was used to select a result.  We use the result-cell info from THAT node to supply result info
  lastexecutednode =. {: DOshapelocales
  NB.?lintonly lastexecutednode =. <'dissectobj'
  cellshapedisp =. (": maxcellresultshape__lastexecutednode) , (fillrequired__lastexecutednode *. 0 = #resultlevel__lastexecutednode) # ' (fill)'
else.
  cellshapedisp =. ''
end.
NB. obsolete NB. Get the frame before the first sropdown; any surplus displayed shape must be the result-cell shape
NB. obsolete rankbeforedrop =. # ; SFOPEN (i.~ {. ]) ; 1 {"1 af
QP^:DEBDOvn'defstring]0 $shapetouse shapetouse errorcode sellevel selectable selections '
QP^:DEBDOvn'defstring__inheritroot]0 sellevel__inheritedtailforselectinfo sellevel__inheritroot selections__inheritroot '
QP^:DEBDOvn'maxcellresultshape__inheritroot '
NB. If we have no shape, either it's a scalar, or we have empty frame with unknown shape.
NB. obsolete cellshapedisp =. (": maxcellresultshape__inheritroot) , (fillrequired *. 0 = #resultlevel) # ' (fill)'
if. #(;DOshapes),cellshapedisp do.
NB.QP'inheritroot defstring__lastexecutednode]0 defstring__inheritroot]0 defstring]0 cellshapedisp '
  NB. The frames may include values that are beyond the last selection.  This is OK as long as they don't drop down:
  NB. they are just indicating the frames of the successive verbs; after the frame of the last verb we should append
  NB. the shape of the last result cell.  But if unselected nodes drop down, we have no idea what the frames or cellshapes
  NB. should be, so we have to delete them.
  NB. After that's done, we have to reconstruct what value to use for the 'shape of the result cell', which is always
  NB. appended as the last value in DOshapes (it is given the special result color when it is drawn).  This value will
  NB. be empty if a dropdown erased it.
  currselections =. sellevel__inheritedtailforselectinfo }. (sellevel__inheritroot + selectable__inheritroot) ((<. #) {. ]) selections__inheritroot
  NB. Get surplus frames, and result shape; delete any characters after >, delete any boxes after >
  unselectedframes =. ({.~    1 (e. + i.~) '>' = {:@>) '>'&((>:@i.~ {. ])^:e.)&.> inituframes =. ((#currselections) }. DOshapes) , <cellshapedisp
  NB. put the unselected frame back onto the selected ones
  NB. First get the surviving boxes that contain valid selections
  DOshapes =: ((#currselections) {. DOshapes) , unselectedframes
  NB. Trim down the number of locales to match the valid selections
  DOshapelocales =: DOshapes (<.&# {. ]) DOshapelocales
  NB. If we snipped off the last box (containing the special result-cell shape),
  NB. add one to carry the special color and indicate that we don't know the exact frame
  NB. Also, at this point expand DOshapes to a table: first row shapes, second row (optional) selections
  DOshapes =: ,: DOshapes , (inituframes >&# unselectedframes) # <'?'
NB. If there are selections, line them up under the boxes in the shape containing selectable values
NB. (i. e. more than one cell)
  if. #currselections do.
    NB. Get the selections, convert to rank-2 ISF, then convert each box to displayable.  Add as second row
    DOshapes =: DOshapes , ;&.> ": L:0 isftorank2 currselections
  end.
elseif. (0 < #selresult) *. (errorcode e. EHASVALIDFILLMASK) *. (0 ~: #fillatom) do.
  NB. The value, which has no shape, is a valid data value, viz an atom.  Display that shape to distinguish it
  NB. from an unselected value.  This is perforce the cell color, so make it the last thing in the list
  DOshapes =: ,: ,<'atom'
  DOshapelocales =: 0$a:
elseif. do.
  NB. No display because no selection
  DOshapes =: 0 0 $ <''
end.
if. #DOshapes do.
  NB. Convert the shapes to characters, and get the pixel extent of each string.  Start at the selection level
  NB. of this object
  shapeext =. ((sellevel__inheritedtailforselectinfo + i. {:$DOshapes) ((<. <:@#) { ]) cfmshape) sizetext"1 0"_ 1 DOshapes
  NB. Create a rect object for the shape/selections
  shapedesc =. (<ALIGNCENTER) addalignmentgroup (<ALIGNSPREAD)&addalignmentgroup@,."1@|: ALIGNSPREAD addalignmentrect shapeext
else.
  shapedesc =. 0 addalignmentrect 0 0
end.
QP^:DEBDOL'defstring]0 >coname'''' shapetouse sellevel DOshapes selections resultlevel fillmask selresult '

NB. Early error is special: it formats the status above the verb, it aborts traversal,
NB. and it suppresses the display of data.  Go ahead and
NB. handle that here.
'DOlabelpos DOshapepos DOstatuspos DOdatapos displayscrollbars' =: <,:0  NB. make sure all names defined
if. errorcode e. EEARLYERROR do.
  NB. Create rectangles for status and verb, and stack them, expanding the status line if needed
  NB. No data on early error
  picknames =: 'DOstatuspos DOlabelpos'
  arects =. ,: , alignrects > (ALIGNLEFT;ALIGNCENTER) addalignmentgroup statusdesc ,: namedesc
NB. format the DOL if any
elseif. (0 < #selresult) *. errorcode e. EHASVALIDFILLMASK do.
NB. The case of no results can happen here only if we have EUNEXECD which we passed through
NB. because of previous error.
NB. If there are ranks, create rectangles for each and stack vertically.
NB. Join the string and the ranks to create the top line
NB. Get size of shape string, plus margin.
NB. shape of result is shape of fillmask; box according to shapes of selectors
NB. Stack vertically: string/rank,shape,status,data

NB. Find the sizes to display: main, and explorer if allowed.  A table of 1 or 2 rows
  hwtable =. calcformsize valueformat =: createDOL ((<_1;2 3 4) { cfmdata)
QP^:DEBDOL'valueformat '
NB. Keep track of the size of the largest noun encountered
  maxactualnounsize__COCREATOR =: maxactualnounsize__COCREATOR >. extractDOLsize valueformat
NB. If data doesn't fit in the allocated area, append scrollbars as needed.  We install the
NB. bars here; the endpoints and traveler are added when the box is drawn
  hwtable =. hwtable +"1 SCROLLBARWIDTH * |."1 displayscrollbars =: hwtable <"1 extractDOLsize valueformat
  datadesc =. ALIGNCENTER addalignmentrect hwtable
  picknames =: 'DOlabelpos DOshapepos DOstatuspos DOdatapos'
  arects =. ,@:alignrects@:>@:((ALIGNLEFT;ALIGNCENTER)&addalignmentgroup)"_1 (namedesc , shapedesc ,: statusdesc) ,"_ _1 datadesc
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
  DOshapepospickrects =: (brect"0@{.@> shapex {"1 arects) -"2"_1 (1 0) *"1 2 shapex {"3 pickrects
  assert. (#DOshapelocales) = <: 1 { $DOshapepospickrects
end.

NB. There should always be SOMETHING to display (shape, label, or data), but during sniff we don't allow
NB. any space for the data, which could leave us with nothing.  Make sure we have a valid DOsize then
DOsize =: 2 2&>.@{:@brect"_1 pickrects


NB. If selections have changed such that this locale cannot raise an explorer, delete any old one that exists
if. 2 > #DOsize do. destroyexplorer '' end.

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
NB.?lintonly 'DOlabelpos DOshapepos DOstatuspos DOdatapos' =: <2 2 $ 0
NB.?lintonly 'DOranks DOranklevels DOshapes' =: ($0);($0);<0$a:
NB.?lintonly 'DOlabelpospickrects DOranklocales' =: (1 0 2 2$0);(0$a:)
NB.?lintonly 'DOshapepospickrects DOshapelocales' =: (1 0 2 2$0);(0$a:)
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
NB. gives the shape of the selected group of cells, which may become important fs subsequent selectors select from the group.
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
NB. obsolete     NB. See how many trailing boxes of y contain multiple values
NB. obsolete     assert. 1 = #$ysel  NB. If there is selecting, we should have applied it to y first
NB. obsolete     NB. Since we are perforce down to a single list for ysel, run all the selections into a single level-2 list.
NB. obsolete     NB. Preserve single boxes, including SFOPENs, intact
NB. obsolete     ysell2 =. ; <^:(1=L.)"0 ysel
NB. obsolete     NB. There may be multiple boxes of y containing boxes with multiple values, BUT: these boxes must
NB. obsolete     NB. be trailing boxes of y.  See how many there are.  This is the number of axes of x we can index.
NB. obsolete     NB. We take the LEADING x axes among the eligible ones, so that successive axes go in order
NB. obsolete     NB. count the trailing eligible axes
NB. obsolete     if. ranky =. 1 i.&1@:~: |. classsel ysell2 do.
NB. obsolete       NB. Remove x axes used and replace the first box(es) of x with the remainder.
NB. obsolete       usableselx =. (selrank =. ({: $ selx) <. ranky) {."1 selx
NB. obsolete       if. selrank do.
NB. obsolete         NB. There are selections to make.  They should not include SFOPEN
NB. obsolete         NB. We apply to the leading axes among those that have alternatives.
NB. obsolete         NB. The result of the selection should have the same boxing level as x: if x is a list of alternatives,
NB. obsolete         NB. so should the result be; while if x is a simple selection, so should the result be.  So, we
NB. obsolete         NB. open y all the way, then apply x at level 0
NB. obsolete         ysell2 =. ((- ranky) }. ysell2) ,  selrank (}."1 ,"1~ usableselx ({L:0 >^:L.)"0"1 {."1) (-ranky) {. ysell2
NB. obsolete         selx =. selrank }."1 selx
NB. obsolete       end.
NB. obsolete     end.
NB. obsolete     ysel =. < ysell2
  end.
end.
NB. replace the first box of x (if there is any residual x after selection), and join it to the selected y.  If y has surplus rank this will copy the surplus rank to the result
ysel ,"1 selx (,~ <)~"1^:(*@{:@$@[) }."1 x
)
NB. y is a ysel, x is a selx.  Result is list of (new ysel);(new selx)
combineyxsels =: 4 : 0"1
NB. Since we are perforce down to a single list for ysel, run all the selections into a single level-2 list.
NB. Preserve single boxes, including SFOPENs, intact
ysell2 =. , ; <^:(1=L.)"0 y
NB. There may be multiple boxes of y containing boxes with multiple values, BUT: these boxes must
NB. be trailing boxes of y.  See how many there are.  This is the number of axes of x we can index.
NB. We take the LEADING x axes among the eligible ones, so that successive axes go in order
NB. count the trailing eligible axes
if. ranky =. 1 i.&1@:~: |. classsel ysell2 do.
  NB. Remove x axes used and replace the first box(es) of x with the remainder.
  usableselx =. (selrank =. ({: $ x) <. ranky) {."1 x
  if. selrank do.
    NB. There are selections to make.  They should not include SFOPEN
    NB. We apply to the leading axes among those that have alternatives.
    NB. The result of the selection should have the same boxing level as x: if x is a list of alternatives,
    NB. so should the result be; while if x is a simple selection, so should the result be.  So, we
    NB. open y all the way, then apply x at level 0
    ysell2 =. ((- ranky) }. ysell2) ,  selrank (}."1 ,"1~ usableselx ({L:0 >^:L.)"0"1 {."1) (-ranky) {. ysell2
    x =. selrank }."1 x
  end.
end.
(,< ysell2);<x
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
sel =. (0 0,:1 1) (]  ,"1~  -@{:@$@]  |.!.0"1  ({.~ #)) > {. > y
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


cocurrent 'dissect'

NB. ***** joining display objects *****
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

NB. Create a reference to a layout
NB. y is the layout
NB. Result is reference layout, as a table
createreference =: 3 : 0
NB. The reference has the same resulthook as the main layout, but no DOLs, pixels, or wires
(((0$a:),&< 0 2 2$0),(0 2 2$a:);3&{)"1 y
)

NB. Create a self-reference to the current object (which may not exist yet)
NB. y is the position(s) of the output wire in fractional face.position form
NB. Result is reference layout, as a table
createselfreference =: 3 : 0
NB. The reference has the same resulthook as the main layout, but no DOLs, pixels, or wires
(((0$a:),&< 0 2 2$0) , (0 2 2$a:) (;<@,:) (coname'') (,<) displayhandletoposition)"0 , y
)

NB. Create empty layout, as a table
createemptylayout =: 3 : 0
,: ((0$a:),&< 0 2 2$0),(0 2 2$a:);<0 2$a:
)

MAXVERTFLOAT =: 5  NB. Number of gridcells leeway to allow a box to move up to maximize overlap

NB. y is a float encoding face.position (face is nearest integer, rest is position -0.5 to 0.5)
NB. result is face# 0-3,position
displayhandletoposition =: (] ,. -) <.@:(0.5&+)

NB. Join layouts left-to-right
NB. y is layout1,:layout2
NB. x (y offset for right box;
NB. Result is composite layout, with all wires & multiple results; a LIST
joinlayoutslr =: 3 : 0
0 1 joinlayoutslr y
:
QP^:DEBLAYOUT'Joinlayoutslr:x?x y '
'ldol lyxhw lwir lres rdol ryxhw rwir rres' =. ,y
'rofsty floatok' =. x
NB. If one of the blocks (or both) is a reference, don't bother moving anything;
NB. just join the (empty) blocks and the results
if. ldol *.&(0~:#) rdol do.
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
NB. Since the profiles coalesced identical points, we'd better do the same on out lists of important points
  lssyrng =. (ROUTINGGRIDSIZE * i. floatok} 1,MAXVERTFLOAT) +/ lssy =. ~. lssy   NB. left vert positions, shifted
  rssyrng =. (ROUTINGGRIDSIZE * i. floatok} 1,MAXVERTFLOAT) +/ rssy =. ~. rssy
NB. Each y value gives the valid x until the next higher y value; so we will keep the
NB. y values in DESCENDING order and look up in that table, so that match on an interval means
NB. that the corresponding x is valid.  We extend the table to handle searches that run off the
NB. end (i. e. are lower than all the points being looked up).
NB. To make sure we check all the points of change, we look up the left points in the right, and
NB. vice versa
  llookups =. >./"1 lprof -"1 (rprof,1e6) {~ rssy I. lssyrng  NB. left vert positions looked up in right, subtracted from left horiz pos
  rlookups =. >./"1 rprof -~"1 (lprof,0) {~ lssy I. rssyrng
NB. Try smaller vertical offsets first.  Pair each position of each side with the base position of the other side
NB. (unshifted position appears twice)
  lookbest =. <./ lookres =. , rlookups ((>. {.) ,. (>. {.)~) llookups  NB. R +0, L +0, R +1, L +1, etc...
  moves =. ROUTINGGRIDSIZE * |.!.0^:{: 0 2 #: lookres i. lookbest  NB. Interpret as L -0. R -0, L -1 etc
NB. Move each side up to the extent there is slack; move other side down the rest
  vertadj =. moves ([ (|.@:- - ]) <.) slacks
NB. Move blocks to establish spacing
  lyxhw =. lyxhw +"2 (2 2){. {. vertadj
  ryxhw =. ryxhw +"2 (0) ,:~ ({: vertadj) , >.&.(%&ROUTINGGRIDSIZE) lookbest + MINBOXSPACING + 1
end.

(ldol,rdol);(lyxhw,ryxhw);(lwir,rwir);<(lres,rres)
)

NB. x is a single DOL, y is the physreqandhighlights table for it
NB. Install the highlight rects for it, provided the operand has not been marked as a stealth path.
NB. The proviso is to prevent highlighting an operand that is nominally referred to in a stealth path but doesn't
NB. actually affect the result
addselecttoDOL =: 4 : 0"1 0
if. # ohandles =. 3 {:: x do.  NB. If there are handles, they point to the output
  loc =. {. 0 {"1 ohandles  NB. the locale of the DOL
NB.?lintonly loc =. <'dissectobj'
  QP^:DEBHLIGHT'addselect:?defstring]0 sellevel sellevel__loc >y '
  addselectedoperands__loc >y
end.
''
)

NB. Entry point when dol and locale are joined together.  This is also called from original traversal,
NB. thus needs to be in outer locale.
NB. We create the DOL for the locale named in y, and then add the operand selections originating in that locale
NB. to the places they come from
NB.
NB. If the locale is
joinlayoutsl_dissect_ =: 3 : 0
'dol loc right' =. 3 {. y , <0 2$a:
NB.?lintonly loc =. <'dissectobj'
NB. If there are operand selections, apply them to the input locales
QP^:DEBHLIGHT'dol joinlayouts:physreq=?physreqandhighlights__inheritroot__loc >loc defstring__loc]0 '
if. *#physreqandhighlights__inheritroot__loc do.
NB. The highlight requests have been consolidated by inheritu so that they now are a list for
NB. each operand, with one highlight request per sellevel.  Also, physreqandhighlights has been
NB. brought back so that it contains all the selections out to the last highlight request.
NB. For each request, we take all the
NB. physical selections before the highlight, and append the highlight
  dol addselecttoDOL physreqandhighlights__inheritroot__loc
end.
NB. if there are right-sided operands, highlight them too
NB.?lintonly y =. <'dissectobj'
res =. dol joinlayouts__loc ''
if. #right do.
NB. Install highlights
  addselecttoDOL&>/"1 right
NB. Install wires from each of the right locales to a spot on the left locale
  rightdols =. ; 0 {"1 right
  rightoutputs =. ; 3 {"1 rightdols
  leftinputs =. loc (,<)"0 1 displayhandletoposition 3 + _0.3 ^ #\ rightoutputs
  newwires =. rightoutputs ,:"1 leftinputs
  res =. (< ((<0 2) {:: res) , newwires) (<0 2)} res
NB. Remove the outputs from the rightdols so they won't appear again as outputs of the joined block
  rightdols =. (<0 2$a:) (<a:;3)} rightdols
NB. Connect the right inputs, adjusted up to the top of the left, and with movement suppressed
  res =. ,: (({. {."1 DOsize__loc),0)&joinlayoutslr@,:/ res , rightdols
NB.displayhandlesin__loc =: displayhandlesin__loc , 2.7
end.
res
)



NB. ********** draw DOs

NB. x is pen color,width[,style]
NB. y is table of yx.:yx
drawline =: 4 : 0
if. DEBGRAF do.
  'Lines: color=%j, width=%j, style=%j, xywh=%j' printf (3{.x);(3{x);(4}.x); }: ; '((%j,%j)-(%j,%j)),' vbsprintf ,"2 |."1 y
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
  'Rectangles: color=%j, pencolor=%j, xywh=%j' printf (2{.x), < }: ; '((%j,%j)-(%j,%j)),' vbsprintf ,"2 |."1 y
end.
if. 1 < #x do.
  (([: glpen PS_SOLID ,~ {:) [ glrgb@}:) 1 {:: x
else.
NB. No color, no pen
  (([: glpen (0,PS_NULL)"_) [ glrgb) irgb
end.
if. #ic do.
  glrgb irgb
  glbrush ''
else. glbrushnull''
end.
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
(<vc) drawrect > 1 {"1 y
if. DEBGRAF do.
  'Text: colors=%j/%j, font=%j%j, xy=(%j,%j), text=%j' printf vc;tc;tf;ts; (<"0 |. (2 ($,) mg) + {. 1 {:: y) , (0 { y)
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
  'Sizetext: colors=%j/%j, font=%j%j, text=%j' printf vc;tc;tf;ts; (0 { y)
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
drawDOvn"1 x { (>:*#winhwnd) {. ((#DOsize) {. (winhwnd__COCREATOR;winhwnd),.(0;1),.(1<#DOsize);0) ,. |: <"_1@> DOyx;DOsize;DOlabelpos;DOshapepos;DOstatuspos;DOdatapos;displayscrollbars;pickrects;scrollpoints
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
'hwnd hwindex explorable DOyx DOsize DOlabelpos DOshapepos DOstatuspos DOdatapos displayscrollbars pickrects scrollpoint' =. y
wd 'psel ' , hwnd
glsel 'dissectisi'
'cfmlabel cfmshape cfmstatus cfmdata' =. DOcfm
SM^:DEBDOL 'drawDOvn: ' , > coname''
NB. Save the position of the object, and as a 2x2
actyx2 =. 0 ,:~ DOyx
NB. Set clipping box to the interior of this object
glclipreset''
glclip 0 0 1 1 + , |."1 DOyx ,: DOsize

NB. See which elements are present
'labelpresent shapepresent statuspresent datapresent' =. (i. 4) e. presentx =. (;: 'DOlabelpos DOshapepos DOstatuspos DOdatapos') i. picknames

NB. Draw covering rectangles for each component - filling out to the full width of the box
if. #presentx do.
  ((<presentx;0) { FRINGECOLOR) drawrect"0 2 actyx2 +"2  ({."1 pickrects) ,."1 (0) 0} DOsize
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
  ((sellevel__inheritedtailforselectinfo + i. <: {:$DOshapes) ((<. #) { ]) cfmshape) drawtext"2^:(*@#@[) }:"2 shapeseltext
NB. Draw the result-cell shape, the last column of the first row
  RESULTSHAPECFM drawtext (<0 _1) { shapeseltext
end.

NB. draw the status string, if any
if. statuspresent do.
  cfmstatus drawtext DOstatusstring;actyx2 + DOstatuspos
end.


NB. Draw the data - if the node has data to display.  If not, we never created a DOL, so don't draw
if. datapresent do.
  assert. 0 = 4!:0 <'fillmask'  [ 'drawDOvn'
  
NB. Calculate the cliprect for the data portion, as tlhw
  cliptlhw =. (DOyx,:0) + DOdatapos   NB. startpos + tlhw rect
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
  (valueformat;dispvalue;fillmask;0;0;<cfmdata) drawDOL cliptlbr ; boxyx

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
    QP^:DEBHLIGHT2'drawhighlights:defstring=?defstring]0 hlights vf (vf)hlighttotlbr{:"1]hlights '
    mesh =. ((boxyx + 2 2 $ 1 _1 1 _1) +"2 |:"2) valueformat hlighttotlbr 2&{"1 hlights
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
  
NB. If there are scrollbars, draw them
  if. +./ displayscrollbars do.
    't l b r' =. , DOyx +"1 +/\ DOdatapos  NB. t l b r of region
    'sh sw' =. (2 * SCROLLBARENDTHICKNESS) -~ 'h w' =. ({: DOdatapos) - SCROLLBARWIDTH * |. displayscrollbars  NB. actual data h/w
    
    datahw =. extractDOLsize valueformat
    QP^:DEBDOL 'displayscrollbars DOyx DOdatapos datahw t l b r h w scrollpoint sw sh '
    scrolltravv =. scrolltravh =. 0 0
NB. draw horizontal scroll
    if. 1 { displayscrollbars do.
      SCROLLBARCOLOR drawrect (vpos =. -/\. b , SCROLLBARWIDTH) ,. (l , w)
      SCROLLBARENDCOLOR drawrect vpos ,."1 (l , SCROLLBARENDTHICKNESS) ,: (-/\. (l+w) , SCROLLBARENDTHICKNESS)
      scrolltravh =. SCROLLBARENDTHICKNESS + <. sw * 0 >. 1 <. (+/\ (1 { scrollpoint) , w) % (1 { datahw)
      SCROLLBARTRAVELERCOLOR drawrect vpos ,. -~/\ l + scrolltravh
    end.
NB. vertical
    if. 0 { displayscrollbars do.
      SCROLLBARCOLOR drawrect (hpos =. -/\. r , SCROLLBARWIDTH) ,.~ (t , h)
      SCROLLBARENDCOLOR drawrect hpos ,.~"1 (t , SCROLLBARENDTHICKNESS) ,: (-/\. (t+h) , SCROLLBARENDTHICKNESS)
      scrolltravv =. SCROLLBARENDTHICKNESS + <. sh * 0 >. 1 <. (+/\ (0 { scrollpoint) , h) % (0 { datahw)
      SCROLLBARTRAVELERCOLOR drawrect hpos ,.~ -~/\ t + scrolltravv
    end.
    scrolltravelers =: (scrolltravv ,: scrolltravh) hwindex} scrolltravelers
    QP^:DEBDOL 'scrolltravelers '
  end.
  
end.

NB. *** reset clip rect ***
glclipreset''

NB. Draw border rectangles for each component - filling out to the full width of the box
if. #presentx do.
  ((<'') ,. (<presentx;1) { FRINGECOLOR) drawrect"1 2 actyx2 +"2  ({."1 pickrects) ,."1 (0) 0} DOsize
end.

NB. Draw a hollow rectangle for the object, just to get the border line.  Color is used to show (no data,NA,all drawn,explorable)
('';((#.datapresent,explorable) { DOBORDERCOLORS),1) drawrect DOyx ,: DOsize


NB. If we drew to the explorer, we must paint it
if. 1 = hwindex do. glpaint'' end.

NB.?lintsaveglobals
)

NB. x is text-color info, a la cfmdata
NB. y is fillmask codes
NB. result is the value to use for drawtext, with stippling added to the rect color
NB. obsolete rectcolorfromfillmask =: ({~      [: < 0 ;~ (- <. 2 ^. FILLMASKCHECKER)&bwlsl)
rectcolorfromfillmask =: (<:FILLMASKNOCOLLECT)&bwand@] ,~&.> ({~      [: < 0 ;~ (- <. 2 ^. FILLMASKCHECKER)&bwlsl)

NB. x is text-color info, a la cfmdata
NB. y is fillmask codes
NB. result is the value to use for drawtext: the selected color, with stippling added
textinfofromfillmask =: ({:"1@] ((,~&.> 0&{"1) 0}"0 1 ]) ({~ {."1))    (0,FILLMASKCHECKER)&#:

NB. y is fillmask code
NB. result is 1 if the fillmask is data or plain fill; 0 if error or unexecd
fillmaskisvaliddata =: FILLMASKUNEXECD ~: FILLMASKUNEXECD&bwand

NB. x is (mask of y-onscreen);(mask of (x-onscreen)
NB. y is array of values/masks/fill etc
NB. Result is the visible values
scissortoscreen =: (1&{::@[ #"1 _1 (0)&{::@[ # ])

NB. Draw the graphics for a noun's DOL
NB. x is DOL;values;selection;boxmesh;highlights;cfminfo   selection is replicated if needed
NB.  boxmesh is not used
NB. y is (tl;:br of drawable region);yx of topleft corner or data, including scroll offset
NB. We execute the gl2 operations to draw the DOL
NB.
NB. For selection nodes, selection and boxmesh are boxed.  In this case we suppress the action of
NB. selection (which fills in rectangles) and force the action of boxmesh (which draws rectangle boundaries).
NB. We then pass the contents of the boxes to recursion, which will open them and use them
drawDOL =: 4 : 0"1
'vf data sel unused unused cfmdata' =. x
'shapeused ysizes xsizes' =. 3 {. vf
'cliptlbr dataorigin' =. y
SM^:DEBDOL 'drawDOL: ' , > coname''
QP^:DEBDOL'vf data sel xsizes ysizes '
NB. If the data is empty, draw nothing (but signal validity).  The size
NB. of the empty was accounted for when the block was created
if. 0 e. $data do.
  0   NB.  valid return
else.
  
NB.  If there are subDOLs, adjust the rects to leave a box margin
  boxyx =. dataorigin
  if. DEBOBJ do.
    'DOL: xy=(%j,%j) xsizes=%j ysizes=%j' printf (<"0 |. y),xsizes;ysizes
  end.
  
  usedd =. data
NB. Get the y and x endpoint lists, prepend a zero to give the start of the first cell,  and then
NB. adjust for the starting position of te object
  yxpositions =. boxyx (+ 0&,)&.> 1 2 { vf
NB. The shape of the array of rectangles.  We will shape the user's data and the fillmask into this shape
  flatshape =. <: #@> yxpositions   NB. <: to remove the leading 0
NB. Convert the array, of whatever rank, to a table
NB. axes is 2 boxes, giving the axis numbers that are assigned to vertical and horizontal.  We assign
NB. axes alternately, starting from the right, with the last axis always going to x
  axes =. (i. ((#~ -.) ; #~) [: |. $&1 0)@#@$ usedd  NB. 1;0 2   or 0 2;1 3
NB. axisshapes is the lengths of each axis assigned to y/x.  sizes is the total size of y/x
  sizes =. */@> axisshapes =. axes ({&.:>"0 _ $) usedd
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
NB. Create the rectangles for each atom.  This will be mxnx2x2.
  rects =. ,."1/&(}: ,. 2&(-~/\))&>/ onscreenbdys =. onscreenmskext #&.> yxpositions
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
    (cfmdata rectcolorfromfillmask >sel) drawrect"0 2 rects
  else.
NB. Selector node
NB. usedd has been converted to a table - do the same for sel
    sel =. onscreenmsk scissortoscreen sizes ($,) (;axes) |:  sel
  end.
NB. If there are subDOLs, process each of them.  The operand was boxed.
  if. 3 < #vf do.
    sdol =. onscreenmsk scissortoscreen flatshape ($,) (;axes) |: shapeused {. 3 {:: vf
    NB.  Adjust each inner box position
    (sdol ,"0 1 usedd ,"0 1 (<"0^:(0=L.) sel) ,"0 a:) ((((0$a:);<cfmdata) ,~ [) drawDOL ])"1 cliptlbr ;"2 1 (BOXLINEWIDTH + BOXMARGIN) +"1 {."2 rects
NB. Draw mesh for the rectangles - unless the boxing is because of collection error
    if. -. +./@:, 0:`(0~:FILLMASKNOCOLLECT&bwand)@.(0=L.)@> sel do.
      (BOXBORDERCOLOR,1) drawmesh (,:   [: |. 0 _1&{&.>) onscreenbdys
    end.
  else.
NB. Not boxed data; draw each cell.  If the cell is error/unexecd, delete the text, since the cell
NB. doesn't really have a value.  We leave its space as a reminder of how big it might have been
NB. Install checkboard, so it shows up at all levels
     sel =. (<:#cfmdata) checkerboardfillmask sel
    (cfmdata textinfofromfillmask sel) drawtext"1 ((fillmaskisvaliddata sel) (# ":)&.> usedd) (,<)"0 2 rects
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
  
  0
end.
)


NB. Create a layout for the current locale.
NB. DO must have been calculated already
NB. We create a layout containing the one locale
NB. y is the desired minimum start point.  We center the block on a grid boundary
NB. so that it is near the minimum without going under
createlayout =: 3 : 0
NB. DOL table is the one DOL
NB. Place the block so that its center is on a grid position
NB. Use the first (main) size
objstart =. <. >.&.(%&ROUTINGGRIDSIZE)&.(+&(-: {. DOsize)) y
dolpos =. (coname''),&<&,: objstart,:{.DOsize
NB. the result is in this locale, bottom face
,: dolpos,(0 2 2$a:);<(,:(coname''),<displayhandletoposition displayhandleout)
)

NB. join layout(s) to the current object
NB. x is table of input layout(s)
NB. y is unused - we create the current object
NB. We join the input objects, then append the DOL for the current object
NB. Result is DOL for the combined layout, as a TABLE:
NB.  locale of obj;(start,:size of object);internal wires;handles out
NB.   wires are brick, where each 2x2 is a table of start,:end, each in the format (locale;face#,fractional position)
NB.   handles in/out are a table of startpoints, (locale;single face#,fractional position)
NB. The handles come in as floating-point values with the (nearest) integer part indicating the face and the
NB. fractional part (range -0.5 to 0.5) indicating position along the face
joinlayouts =: 4 : 0
NB. Create the DO for the block to add on.  If stealth operand, just
NB. pass on the selected input layout
assert. 2 = #$x
if. DEBLAYOUT do.
  qprintf'Joinlayouts:loc=?>coname'''' x '
end.
if. 0 ~: createDO'' do.
  if. DEBLAYOUT do.
    smoutput 'Stealth object, not created'
  end.
NB. If the input is a noun result that has had detail removed, it may contain no layouts.  In that case,
NB. if we pass it to a stealth operand it must make a layout for it  Example: z + ] 3.  If there are
NB. layouts, only one of them will survive to take the place of the stealth operand; we make sure here
NB. that it is the non-stealth operand
  if. 0 = #x do. createlayout 0 0 else. (, dispstealthoperand { 0 _1 0 0 _1 _1 0) { x end.
else.
NB. If there are no earlier layouts, this had better be a noun - just create its layout
  if. 0 = #x do.
    createlayout 0 0
  else.
NB. Remove any layout (there can be only one) that has been marked as elided by stealth, by having its
NB. output handles cleared.  Join the survivors
    'upperdol upperyxhw upperwires upperresult' =. joinlayoutslr@,:/ (#~    a: ~: 3&{"1) x
    if. DEBLAYOUT do.
      qprintf'Joined upper objects:dol=?upperdol%yxhw=?upperyxhw%wires=?upperwires%$upperresult%res=?upperresult%'
    end.
NB. Look up results of upper, and put lower block midway.  Use only results that exist - the rest
NB. are references.  If there are no real results, there must be no dols either; just place the block at 0
NB. Remember, result position is centered on the midpoint of the object
    if. #respos =. (#~ >:&0) (0.5 + ((1;1)&{::"1) upperresult) ({.@] + (* {:))"0 1 (_1 0 ,~ {:"1 upperyxhw) {~ upperdol i. {."1 upperresult do.
NB. Place the block midway between the results.
      hplace =. <. ((+/ % #) respos) - -: (1 { {.DOsize) + ROUTINGGRIDSIZE
    else.
      hplace =. 0
    end.
NB. Create a block, close to the given starting place.  If there are no previous blocks, use starting y of 0
    'ldol lyxhw lwir lres' =. {. createlayout (0 >. MINBOXSPACING + >./ +/"1 {."1 upperyxhw) , hplace
    upperdol =. upperdol , ldol
    upperyxhw =. upperyxhw , lyxhw
NB. If the new block was off the left, move everything back to the right
    if. 0 > hplace =. (<0 0 1) { lyxhw do.
      upperyxhw =. upperyxhw -"2 (2 _2) {. <.&.(%&ROUTINGGRIDSIZE) hplace
    end.
NB. Add wires from outputs to inputs.  Ignore wires that connect to _1 (not a valid face, it means 'no wire'
    upperwires =. upperwires , (displayhandlesin ~: _1) # upperresult ,:"_1  (coname'') ,. <"1 displayhandletoposition displayhandlesin
    
NB. Create resulthook, from the result block
    if. DEBLAYOUT do.
      qprintf'Result objects:dol=?upperdol%yxhw=?upperyxhw%wires=?upperwires%res=?(,:(coname''''),<1,displayhandleout)%'
    end.
    ,: upperdol;upperyxhw;upperwires;<lres
  end.
end.
)

cocurrent 'dissect'

NB. ****************** drawing the placement **********************
NB. called in the locale of the form

NB. Size the placed layout, and convert wires to lines;arcs
NB. y is locales;table of yx;wires as table of y x y x type, type = 0 for wire, 1 for arc
NB. x, if given, is the scroll amount (starting position of top-left corner)
NB. result is size reqd yx;locales;yx;(lines as n 2 2 yx start,:end);(arcs as n 2 2 yx center,:corner)
sizeplacement =: 4 : 0
tlc =. x
'dos yx wires' =. 3 {. y
NB. Apply scroll offset
yx =. yx +"1 tlc
wires =. wires +"1 tlc,tlc,0
locpickrects =: 0 2 2 $ 0
for_d. picklocs =: dos do.  NB.?lintonly d =. <'dissectobj'
  locpickrects =: locpickrects , (d_index{yx),:{.DOsize__d
end.
lines =. (2 2&$)"1 (0 = 4 {"1 wires) # wires
arcs =. (2 2&$)"1 (1 = 4 {"1 wires) # wires
NB. Get the max size drawn, and set the control to just big enough to hold it
maxsize =. >./ (+/"2 locpickrects) , ,/ lines , arcs
maxsize;dos;yx;lines;arcs
NB.?lintsaveglobals
)

NB. Draw the placed layout
NB. y is locales;yx;(lines as n 2 2 yx start,:end);(arcs as n 2 2 yx center,:corner)
drawplacement =: 3 : 0
glclear''
'dos yx lines arcs' =. y
if. DEBOBJ do.
  qprintf'DOL?y '
end.
for_d. dos do.  NB.?lintonly d =. <'dissectobj'
  drawDO__d d_index{yx
end.
NB. draw wires.  reset to select graphics window
wd 'psel ',winhwnd
glsel 'dissectisi'
glclipreset''
glrgb WIRECOLOR
glpen 1,PS_SOLID
if. #lines do.
  gllines 1 0 3 2 {"1 ,"2 lines
end.
if. #arcs do.
NB. The arcs are CCW sections given by (center point,corner point).  Expand to arc, which is x y w h xa ya xz yz
NB. Create the xywh: wh = 2 * corner - center, xy = corner - wh
  yxhw =. ,"2 -~/\@:((-~ +:)~/\.)"2 arcs
NB. xa ya xz yz is xc yr xr yc if direction of arc in xy has different sign; xr yc xc yr if same sign
  aazz =. ((0 > */"1 -/"2 arcs) { (_4 ]\ 2 1 0 3  0 3 2 1)) {"1 ,"2 arcs
NB. Kludge lengthen each end by 2 pixels, because the arc is underdrawn
NB.  aazz =. (2&{."1 (] ,"1 ([ + 2&*@:*@:-)) 2&}."1) aazz
  aazz =. (+    1 _1 */ 2&*@:*@:(-/))&.(_2&([\))"1 aazz
  glarc"1 yxhw ,.&:(1 0 3 2&{"1) aazz
end.

NB. Show the sentence, with the user's spacing, highlighting according to selection level
({.sentencebrect) drawsentence }. usersentence sizesentence gettokenlevels__resultroot ''
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
NB. y is the titlestring in the line for this locale
NB. Result is a table of retcode;text
exegesisrankoverall =: (0 2$a:)"_

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

NB. obsolete NB. Get the string form of this verb, ended with LF; if y is 1, return empty if the string has just one word
NB. obsolete exegesisverbstring =: 3 : 0
NB. obsolete ,&LF^:(*@#) (#~   1 < #@;:)^:y defstring 0
NB. obsolete )
NB. obsolete 
NB. obsolete NB. Get the string form of the verb at which collection will be performed for the current locale
NB. obsolete NB. Search up to find the the collection point, and display it.
NB. obsolete NB. If y is 1, return empty if the string has just one word
NB. obsolete exegesiscollectionverbstring =: 3 : 0
NB. obsolete cp =. findcollectionpoint__parent coname''
NB. obsolete NB.?lintonly cp =. <'dissectobj'
NB. obsolete ,&LF^:(*@#) (#~   1 < #@;:)^:y defstring__cp 0
NB. obsolete )

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

NB. y is (shape of an operand),(frame), result is string describing the cell; singular if frame describes < 2 cells
NB. If x is given, it overrides the singular/plural specification
exegesisfmtcell =: 3 : 0
(1 < */ 1 {:: y) exegesisfmtcell y
:
'shape frame' =. y
NB. Convert from operand shape to cell shape
shape =. (#frame) }. shape
x exegesisplural (4 <. #shape) {:: ('atom';((":shape) , '-atom list')) , (exegesisshapewithx shape)&,&.> ' table';' brick';' subarray'
)

NB. y is (shape of a cell),(frame), result is string describing the frame; suitable for being follwed by a cell format, thus
NB. ending with 'of' or a number.  Ends with a space.  Always an indefinite article is appended, except when this starts with a number
exegesisfmtframe =: 3 : 0
'shape frame' =. y
(4 <. #frame) {:: ((exegesisindefinite 'single ');((":frame) , ' ')) , exegesisindefinite@((exegesisshapewithx frame)&,)&.> ' table of ';' brick of ';' subarray of '
)

NB. These are the morphemes we use, in the order they should appear in the final result.
NB. Some may be entended with selection levels when they are created.
NB. The number is the detail level at which the value is display
exegesismorphemes =. 3&{.@;:;._2 (0 : 0)
EXEGESISTUTORIAL 2 Tutorial
EXEGESISRANKOVERALLEXPLAIN 0 Description
EXEGESISRANKSTACKEXPLAIN 0 Description
EXEGESISRANKSTACKPOWERSTART 0 Description
EXEGESISRANKSTACKPARTITIONSTART 0 Description
EXEGESISFRAMENOFRAME 1 Frame
EXEGESISFRAMENUGATORY 1 Frame
EXEGESISFRAMENONNOUN 1 Frame
EXEGESISFRAMENOSHAPE 1 Frame
EXEGESISFRAMEVALID 1 Frame
EXEGESISFRAMESURROGATE 1 Frame
EXEGESISSHAPESELECTINGVERB 0
EXEGESISSHAPEFRAME 0
EXEGESISSHAPERESULT 0
EXEGESISDATASOURCE 1
EXEGESISDATASHAPE 1
EXEGESISDATAPATH 1
EXEGESISDATAARRANGEMENT 1
EXEGESISDATAEXPLORABLE 0
EXEGESISVERBDESC 0 Verb
EXEGESISONELINEDESC 0 Verb
)
({."1 exegesismorphemes) =: i. # exegesismorphemes
exegesislevels =: (1) 0&".@{::"1 exegesismorphemes
exegesislabels =: 2 {"1 exegesismorphemes

NB. y is a table of morphemes; turn them into a displayable string
exegesisgrammar =: 3 : 0
tt =. y
NB. Cull the morphemes that are below the user's culling level
tt =. (tooltipdetailx >: exegesislevels {~ 0 {::"1 tt) # tt
NB. Order them in grammatical order
tt =. tt /: > 0 {"1 tt
NB. The 'no frame' line is provided only to prevent dead air.  Delete it if there is anything else to say about frame.
tags =.  > 0 {"1 tt
if. ((#tags) > nft =. tags i. EXEGESISFRAMENOFRAME) *. (EXEGESISFRAMENUGATORY,EXEGESISFRAMENOSHAPE,EXEGESISFRAMENONNOUN,EXEGESISFRAMEVALID,EXEGESISFRAMESURROGATE) +./@:e. tags do.
  tt =. (<<<nft) { tt
  tags =. (<<<nft) { tags
end.
NB. Insert fences before each new nonnull topic
fencewords =. (] ((~:@] *. a: ~: [) #&.> ]) ((LF,'---') , ('---------------------',LF) ,~ ])&.>) tags { exegesislabels
NB. Run the result together, and delete all but the last LF, and any leading LF, and allow no more than 3 consecutive LF
(#~   [: -. (LF,LF,LF)&E.) (}.~    LF i.&0@:= ]) ({.~ 2 + LF i:&0@:= ]) ; fencewords ,. 1 {"1 tt
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
'labelloc opno' =. y
res =. 0 2$a:  NB. Init empty return
NB. If the source of the operands is not the original click, we'd better start with a line
NB. describing that fact
if. labelloc = coname'' do.
  vstring =. 'verb'   NB. refer to the verb this way
else.
  res =. res , EXEGESISFRAMENOSHAPE;'The arguments to this verb vary from cell to cell. You must select a single result to see the frame.',LF
  if. #inputselopshapes do.
    res =. res , EXEGESISFRAMESURROGATE;LF,'However, this verb is part of ',(opno {:: 'the x argument path of ';'the y argument path of ';''),'the compound verb:',LF,(defstring 0),CR,'whose arguments are analyzed below.',LF,LF
  end.
  vstring =. 'compound verb'
end.

if. 0 = #vranks do.
  res =. ,: EXEGESISFRAMENONNOUN;''   NB. not a verb, don't try to explain anything
elseif. (0 = #inputselopshapes) +. (0 = #selector) do.
  if. EXEGESISFRAMENOSHAPE -.@e. > 0 {"1 res do.  NB. give NOSHAPE message only once
    res =. res , EXEGESISFRAMENOSHAPE;'You must select a single result to see the frame of this verb.',LF
  end.
elseif. _ *./@:= vranks do.
  NB. If verb has infinite rank, just say so here.  We don't use the frame because u/ etc has pseudoframe and we want to 
  NB. report NOFRAME for them
  res =. ,: EXEGESISFRAMENOFRAME;'Verb has infinite rank and always applies to its entire argument',((2=#inputselopshapes)#'s'),'.',LF    NB. If only 1 cell, can't analyze
elseif. 0 = #frame do.
  res =. ,: EXEGESISFRAMENOFRAME;'Verb operates on a single cell',((2=#inputselopshapes)#' of each argument'),'.',LF    NB. If only 1 cell, can't analyze
elseif. do.
  NB. There is a frame.  Describe it
  shapes =. $^:(0<L.)&.> inputselopshapes
  NB. Set up some language about collection.  If the result is collected after this verb, say the result 'is assembled'.
  NB. But if the collection is remote, say 'will be assembled'
  if. (coname'') = cp =. findcollectionpoint__parent coname'' do.
    NB. collecting in the same place where we define the string: just use the description of that verb
NB. obsolete     vstring =. '.'
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
      ftext =. 'The argument is empty, so ', (exegesisindefinite exegesisfmtcell (0{::shapes);frame), ' of ' ,((0<#0{::shapes) exegesisplural 'fill') , ' are supplied to the ',vstring,'.',LF
    else.
      if. 1 1 -: emptyop =. 0&e.@> frames do.
        ftext =. 'The arguments are empty, so cells of fill (on the left, ' , (exegesisindefinite exegesisfmtcell shapes ,&(0&{) frames) , '; on the right, ' , (exegesisindefinite exegesisfmtcell shapes ,&(1&{) frames) , ') are supplied to the ',vstring,'.',LF
      else.
        'ename nonename' =. 'xy' \: emptyop
        ftext =. ename , ' is empty, so it is replaced by a cell of fill (' , (exegesisindefinite exegesisfmtcell shapes ,&(0&{) frames) , '); ',LF
        if. # (emptyop i. 0) {:: frames do.
          ftext =. ftext , nonename, ' is broken into ' , (exegesisfmtframe shapes ,&((emptyop i. 0)&{) frames) , (exegesisfmtcell shapes ,&((emptyop i. 0)&{) frames),' which are supplied to the ',vstring,'.',LF
        else.
          ftext =. ftext , nonename , ' is a single cell, ', (exegesisindefinite exegesisfmtcell (0{::shapes);frame) , '.  These arguments are supplied to the ',vstring,'.',LF
        end.
      end.
    end.
    ftext =. ftext , 'The frame, ' , (":frame) , ' ' , (0{::ctense) , ' prepended to the result of the ',cvstring
  case. 1 do.
    if. 1 = #vranks do.
      ftext =. 'There is only one cell, ', (exegesisindefinite exegesisfmtcell (0{::shapes);frame) , ' which is supplied to the ',vstring,'.',LF
    else.
      ftext =. 'Each argument has only one cell (on the left, ' , (exegesisindefinite exegesisfmtcell shapes ,&(0&{) frames) , '; on the right, ' , (exegesisindefinite exegesisfmtcell shapes ,&(1&{) frames) , ') which are supplied to the ',vstring,'.',LF
    end.
    ftext =. ftext , 'The frame, ' , (":frame) , ' ' , (0{::ctense) , ' prepended to the result of the ',cvstring
  case. do.
    if. 1 = #vranks do.
      ftext =. 'The argument is broken into ' , (exegesisfmtframe (0{::shapes);frame) , (exegesisfmtcell (0{::shapes);frame) , ', which will be supplied one by one to the ',vstring,'.',LF
    else.
      if. # 0 {:: frames do.
        ftext =. 'x is broken into ' , (exegesisfmtframe shapes ,&(0&{) frames) , (exegesisfmtcell shapes ,&(0&{) frames),'.',LF
      else.
        ftext =. 'x is a single cell (',(exegesisindefinite exegesisfmtcell shapes ,&(0&{) frames),') that will be replicated for use with each cell of y.',LF
      end.
      if. # 1 {:: frames do.
        ftext =. ftext , 'y is broken into ' , (exegesisfmtframe shapes ,&(1&{) frames) , (exegesisfmtcell shapes ,&(1&{) frames),'.',LF
      else.
        ftext =. ftext , 'y is a single cell (',(exegesisindefinite exegesisfmtcell shapes ,&(1&{) frames),') that will be replicated for use with each cell of x.',LF
      end.
      if. (i.&0@:=/ > frames) < <./ #@> frames do.
        ftext =. ftext , 'This is an agreement error.',LF
      elseif.
      surplusframe =. 0 -.~ bwxor/ > frames  NB. extend shorter frame with 0, XOR, remove common frame (we know no 0 in any frame)
      (0~:#surplusframe) *. 0 -.@e. #@> frames do.
        NB. cells of the short operand must be replicated, and not just a single cell.
        'short long' =. (>&#&>/ frames) |. 'x';'y'
        ftext =. ftext , 'Each ',(0 exegesisfmtcell (0{::shapes);frame),' of ' , short , ' is replicated into ' , (exegesisfmtframe '';surplusframe) ,'identical cells, and then corresponding cells of x and y are supplied one by one to the ',vstring,'.',LF
      elseif. do.
        ftext =. ftext , 'Corresponding pairs of cells are supplied one by one to the ',vstring,'.',LF
      end.
    end.
    ftext =. ftext , 'The results ',(1{::ctense),' assembled into' ,LF, (exegesisindefinite exegesisshapex frame) , ' result-cells after completion of the ',cvstring
  end.
  res =. res , EXEGESISFRAMEVALID ; ftext
end.
res
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
NB. Get size of 2 items, and of 1.  If max size is larger in both dimensions, we can use the calculated size,
NB. so pick the first one that fits.  If none fits, use max size.  In any case, that's for the main view - we can explore the entire value
  dsizes 0}~ ({. maxnoundisplaysizes) (,~ {~ >."1 i. [) (1 2,:1 1) extractDOLsizelimited y
end.
)

cocurrent 'dissectobj'

NB. Initialize the scroll point.  Nilad.
NB. Result is scroll point (in pixels) for the beginning display
initscrollpoints =: 3 : 0
(#DOsize) # ,: 0 0
)

NB. Create DO
createDO =: 3 : 'createDOvn (*#displayhandlesin) {"1 nouncfm,.verbcfm'

NB. Draw DO.  y is yx of DO
drawDO =: 3 : 'drawDOvnall y'

NB. ********************************** pick DOs
NB. Pick for display object, vectoring to the indicated type
NB. x is ('l' or 'r') flag
NB. y is main/explorer(y,x relative to pickrect);pickflags (numeric atom of left,right,ctrl,shift)
NB. no result.  selections are performed
pickDO =: 4 : 0  NB. Called in locale of the node that drew the DO
QP^:DEBPICK 'coname'''' y '
'exp yx flags' =. y
for_r. (exp{pickrects) findpickhits yx do.
  'ix pyx' =. r
  QP^:DEBPICK 'x ix pyx exp yx flags ix{::picknames '
  if. 3 = 4!:0 <name =. 'pick' , x , ix {:: picknames do. (flags,exp) name~ pyx end.
end.
0 0$0
)

NB. hover/statline is like pick, but there is no mouse button and no control keys
NB. We return the string to use for the tooltip
hoverstatDO =: 4 : 0  NB. Called in locale of the node that drew the DO
'exp yx' =. y
if. #r =. (exp{pickrects) findpickhits yx do.
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

hoverDOlabelposchartutorial =: 0 : 0
This is the name field for a named noun.  In verb blocks this will be the rank stack.
)
hoverDOlabelposranktutorial =: 0 : 0
This is the rank stack.  At the bottom of the stack is the verb whose result is displayed in the data portion of the block. Other lines in the rank stack are modifiers that affect the execution of the verb by changing the cells it operates on.

The numbers in each line indicate the rank(s) of the cells operated on by the modifier in that line.  Each rank is color-coded with the color of the selection level it uses.  This color matches the color of the frame of the modifier in the shape line.

For example, in the sentence
(i.3) +"1"2 i. 3 4 3
the rank stack for + will have 3 lines, one for each " modifier and one for the verb + .  The top line, with rank 2, relates to the overall verb +"1"2 .  The middle line, with rank 1, relates to the verb +"1.  The bottom line, with rank 0, relates to the verb + .  Selection is possible at each level.

Hovering over a line of the rank stack will provide more information.  Hovering over the verb line will describe the overall operation of the block; hovering over a modifier line will describe that modifier.
)


NB. Hovering in the label 
hoverDOlabelpos =: 4 : 0
exp =. x
if. #r =. (exp{DOlabelpospickrects) findpickhits y do.
  'ix pyx' =. {. r   NB. index of pickrect found
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
    tt =. tt , exegesisframe__frameloc labelloc;opno
    NB. Append any explanation unique to this line.  The y argument is
    NB. (1 if this locale appears more than once);(1 if this is the last locale in the stack);
    NB. Computational flags display only when both are 0, leaving the display for the overall node
    NB. appears in this stack.
    tt =. tt , exegesisrankstack__labelloc (1 < labelloc +/@:= 1 {"1 displaylevrank),(ix = <:#DOranklocales)
    NB. Finally, any explanation for the node in general.  Expansions and Finals are explained here.  Every line contributes.
    NB. We pass the title string into the line as y, and a flag set to 1 if this locale appears twice in the stack, as x.
    NB. We process bottom-up to leave explanations in reverse order, at the top of the tooltip
    if. ix = <:#DOranklocales do. tt =. tt ,~ ; (4 : '(1 < y +/@:= 1 {"1 displaylevrank) exegesisrankoverall__y&.> x')/"1 |. 2 {."1 displaylevrank end.
  else.
    tt =. ,: EXEGESISTUTORIAL ; hoverDOlabelposchartutorial
    NB. If the display was text, we will pass that text into the overall for the node
    tt =. tt , exegesisrankoverall displaylevrank
  end.
  text =. exegesisgrammar tt
else.
  text =. ''
end.
reflowtoscreensize text
)
statlineDOlabelpos =: 4 : 0
exp =. x
if. #r =. (exp{DOlabelpospickrects) findpickhits y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. 2 ~: 3!:0 displaylevrank do.
    labelloc =. ix { DOranklocales
    NB.?lintonly labelloc =. <'dissectobj'
    text =. defstring__labelloc 0
  else.
    text =. displaylevrank
  end.
else.
  text =. ''
end.
text
)

hoverDOshapepostutorial =: 0 : 0
This is the shape/selection line(s).  The first line gives the shape of the result; the second line, which is present only when a selection has been made inside the result, gives the index list or path of the selection.

The result-shape comprises the frame and the shape of the result cells.  The shape of the result cells is shown in white against a dark-blue background.  If the result-cells have varying shapes, the string '(fill)' is appended to the shape to indicate that the result contains fills (which are shown by crosshatching in the data area).

The frame, if any, is the part of the result-shape before the dark-blue result-cell shape.  The frame is color-coded to match the modifier that selects from it.

If the block is part of a compound using @, &, or &. there is no displayable result until a single result-cell has been selected.  Until that time the shape/selection lines are omitted.

A result that is a single atom (with empty shape) is indicated by the word 'atom' in the result-cell shape.

If the block includes modifiers that look inside the boxing structure, namely each, &.>, L:n, or S:n, entry into a level of boxing is indicated with '>' at the appropriate point.
)

NB. default verbs for other hovering
hoverDOshapepos =: 4 : 0
tt =. ,: EXEGESISTUTORIAL ; hoverDOshapepostutorial
exp =. x
if. #r =. (exp{DOshapepospickrects) findpickhits y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. ix < #DOshapelocales do.
    l =. ix { DOshapelocales   NB. the locale of the selection
    NB.?lintonly l =. <'dissectverb'
    tt =. tt , EXEGESISSHAPESELECTINGVERB ; 'This portion of the frame selects a cell for the verb:',LF,(defstring__l 0),CR
  else.
    tt =. tt , EXEGESISSHAPERESULT ; 'The shape of a single result-cell',((fillrequired *. 0 = #resultlevel) # '.  Some cells are padded with fill'),'.',LF
  end.
end.
reflowtoscreensize exegesisgrammar tt
)
statlineDOshapepos =: 4 : 0
exp =. x
if. #r =. (exp{DOshapepospickrects) findpickhits y do.
  'ix pyx' =. {. r   NB. index of pickrect found
  if. ix < #DOshapelocales do.
    l =. ix { DOshapelocales   NB. the locale of the selection
    NB.?lintonly l =. <'dissectverb'
    tt =. 'frame of ' , defstring__l 0
  else.
    tt =. (*#vranks) {:: 'shape of noun'; 'result shape' , (fillrequired *. 0 = #resultlevel) # ' (fill added)'
  end.
else. tt =. ''
end.
tt
)

errorlookup =: (LF&taketo ; LF&takeafter);._1 (0 : 0)
?agreement
This dyadic verb has x and y operands that cannot be matched up cell-for-cell.  Look through the rank stack to see where the error occurs.

This error is reported as a 'length error' in the J session.
?framing
The verb completed correctly on each cell, but the result-cells are of different types and cannot be assembled into a single result.

This error is reported as 'domain error' in the J session.
?invalid verb
This combination was rejected before it was even executed on its arguments.
?error
This execution resulted in an error, but overall execution continued.

If this verb is being executed on a cell of fills, the error result is treated as if it were 0.

If this verb is executed in the combination u :: v, execution continues with the v side.
?non-atomic v
In m@.v, the execution of v must result in a numeric atom.
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
Execution took too long.
?value
Execution of this verb attempted to use a name that has not been assigned.
)

hoverDOstatuspos =: 4 : 0
origemsg =. (<<<0 _1)&{^:('('={.) DOstatusstring
reflowtoscreensize ((1 {"1 errorlookup) , <'') {::~ (0 {"1 errorlookup) i. <origemsg 
)

hoverDOdatapostutorial =: 0 : 0
This is the data area, where the result of the verb is displayed.

The maximum size of the data area can be set in the Sizes menu.  If the result does not fit, scrollbars are provided; in addition you can right-click in the data area to create a fullscreen explorer window that will show the result.
)

hoverDOdatapos =: 4 : 0
exp =. x
hoveryx =. y
NB. Ignore hover in the scrollbar
NB. If the click is in the scrollbar, handle scrolling
NB. See which scrollbar, if any, the click is in
dhw =. (<exp,1) { DOdatapos
if. 0 = +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTH * |. exp { displayscrollbars do.
  disp =. ,: EXEGESISTUTORIAL ; hoverDOdatapostutorial
  NB. Start accumulating the display
  if. 2 = 3!:0 DOranks do.
    NB. This is either a noun or a monad/dyad that suppresses detail; verbs have rank stacks
    t =. 'This is a ',((*#DOranks) # 'named '),'noun.'
    if. nounhasdetail *. -. nounshowdetail do.
      t =. ' The value shown is the result of a computation that does not depend on any names.  To see the details of this computation, click anywhere in the value.'
    end.
    t =. t,LF
  else.
    NB. Not a noun.
    t =. 'The result of the verb:',LF,(defstring 0),CR
  end.
  disp =. disp , EXEGESISDATASOURCE ; t , LF
  if. sellevel <: #selections do.
    NB. Display the shape of the result
    rshape =. 0{::valueformat
    disp =. disp , EXEGESISDATASHAPE ; 'This is ',(exegesisindefinite exegesisfmtcell rshape;''),'.',LF,LF

    if. 1 < */ rshape do.
      NB. If there are selection options, display the path

      NB. y is y,x within the display rectangle.  Convert that to offset within the display of the entire noun, by adding
      NB. the offset of the top-left corner of the displayed box, and subtracting the display position of the normal
      NB. top-left, which position is 0 for unboxed, but at a boxmargin for boxed values
      selx =. ; > valueformat yxtopath BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + hoveryx

      NB. Convert the isf to a path.  Put SFOPEN at the end, then cut on SFOPEN and run boxes together.
      NB. If the last box is empty, remove it
      selpath =. }:^:(a:-:{:) <@;;._2 selx , SFOPEN
      if. 1 >: #selpath do.
        disp =. disp , EXEGESISDATAPATH ; 'You are hovering over the atom whose index list is ' , (": ; selpath),'.',LF,LF
      else.
        disp =. disp , EXEGESISDATAPATH ; 'You are hovering over the atom whose path is ' , (}: ; ,&';'&.> ''''''"_`":@.(0<#)&.> selpath),'.',LF,LF
      end.

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
  end.
  NB. If the window is explorable, but the user hasn't created an explorer window, tell him about that option
  if. 1 < #DOsize do.
    if. 0=#winhwnd do.
      disp =. disp , EXEGESISDATAEXPLORABLE ; 'This value is larger than the largest allowed on the main display. You can change the limit on the Sizes menu. You can also right-click the data to open a separate window for exploring the value.',LF
    elseif. exp=0 do.
      disp =. disp , EXEGESISDATAEXPLORABLE ; 'Right-click the data to bring up the explorer window.',LF
    elseif. do.
      disp =. disp , EXEGESISDATAEXPLORABLE ; 'Right-click the data to destroy the explorer window.',LF
    end.
  end.
  text =. exegesisgrammar disp
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
if. 0 = +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTH * |. exp { displayscrollbars do.
  if. 2 = 3!:0 DOranks do. text =. 'noun'&[^:(0=#) DOranks
  else. text =. defstring 0
  end.
  if. sellevel <: #selections do.
    rshape =. 0{::valueformat
    if. 1 < */ rshape do.
      selx =. ; > valueformat yxtopath BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + hoveryx
      selpath =. }:^:(a:-:{:) <@;;._2 selx , SFOPEN
      text =. text , '         path=' , (}: ; ,&';'&.> ''''''"_`":@.(0<#)&.> selpath)
    end.
  end.
else. text =. ''
end.
text
)

FORCEDTOOLTIPMINVISTIME =: 0.4   NB. Minimum time a forced tooltip will be displayed

NB. For all these verbs, x is (button flags,view number), y is the yx position of the click relative to start of pickrect

picklDOdatapos =: 4 : 0
QP^:DEBPICK 'valueformat scrollpoints y '
NB. Click in the data region.
'flags exp' =. x
NB. If the click is in the scrollbar, handle scrolling
NB. See which scrollbar, if any, the click is in
dhw =. (<exp,1) { DOdatapos
if. 1 = +/ sclick =. |. y >: shw =. dhw - SCROLLBARWIDTH * |. exp { displayscrollbars do.
NB. sclick is the mask indicating which axis was selected
NB. select the information for the selected axis, for analysis
  'trav clickpos end bindlist spt' =. (sclickx =. sclick i. 1)&{&.> (exp{scrolltravelers);y;shw;(1 2{valueformat);(exp{scrollpoints)
  QP^:DEBPICK 'trav clickpos end bindlist spt '
NB. Classify the click as +-creep, +-page, or click in traveler
  assert. (-: /:~) SCROLLBARENDTHICKNESS,trav,end-SCROLLBARENDTHICKNESS
  select. clickpos I.~ scrollbarsections =. SCROLLBARENDTHICKNESS,trav,end-SCROLLBARENDTHICKNESS
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
      'scrollinglocale__COCREATOR scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit' =: (coname'');sclickx;spt;(clickpos+dwo);(dwo+0 _1 { scrollbarsections);(end -~ {:>bindlist)
  NB.?lintonly scrollinglocale__COCREATOR =: <'dissectobj'
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
  
else.
NB. Not scrollbar.  Find the indexes of the clicked cell
NB. Find the y,x position of the click and go process it
  if. 1 = selres =. exp processdataclick y do.
NB. If the selection changed, redraw the screen.  If the selection was from the explorer, change the scroll in the main view
NB. to show the selected cell at top-left
    if. exp = 1 do.
      scrollpoints =: (flatyxtopixel 1 pixeltoflatyx y) 0} scrollpoints
    end.
    dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
  else.
NB. User tried to select, but we couldn't do it.  Give him a tooltip.  0=no frame, _1=no further selection, _2=empty operand
    NB. If we are in an explorer, stay here; but if on the main form, we have to switch to that locale
    formloc =. exp { COCREATOR,coname''
    NB.?lintonly formloc =. <'dissect'
    drawtooltip__formloc (y + exp { DOyx + 0 {"2 DOdatapos) ; selres { 'unselectable - no frame';'empty value is not selectable';'no further selection possible'
    hoversessmin__COCREATOR =: FORCEDTOOLTIPMINVISTIME + 6!:1''  NB. Only one tooltip at a time, so OK to put in instance locale
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

cocurrent 'dissect'

NB. Utility to look up a yx offset in a set of pickrects
NB. x is pick rectangles, YX,;HW
NB. y is y,x
NB. result is table of (index to hit rect);relative y,x in rect    empty if no hits
findpickhits =: 4 : 0
index =. (+/\"2 x) I.@:(*./"1)@:(>/"2)@:(<:"1) y
index ;"0 1 y -"1 (<index;0) { x
)

NB. ****** tooltips *****

NB. J602 emulation of ptimer.  On QT, or if tooltips disabled on J6, we will define as dissect_timer and immediately overwrite
NB. On J6 with tooltip, this redefines the timer handler
NB. obsolete ((IFQT +. -. ALLOWNONQTTOOLTIP) {:: 'sys_timer_z_' ; 'dissect_timer') =: 3 : 0
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
wdtimer 0
wd 'psel ' , winhwnd   NB. J bug on the Mac requires explicit psel
hoverdo''
0 0$0
)

NB. We have internal actions hoverstart, hoverend, hoverdo.  hoverstart is called when the mouse
NB. moves; we record where it was and set a timer, abandoning the old timer if the mouse has moved.
NB. hoverend is called whenever anything happens to abort the hover (click, focuslost, etc).  hoverdo
NB. is called when the timer expires: we then see where the cursor is and call the owner to get a tooltip.

NB. y is the mouse position yx.  Start/continue a hover timer, clearing an old one if the mouse has moved
hoverinitloc =: $0   NB. Init no hover active
MAXHOVERMOVEMENT =: 1   NB. Allow this much movement from start-of-hover position
NB. obsolete HOVERTIME =: 1000  NB. time to hover, in msec
'HOVEROFFSETY HOVEROFFSETX' =: _8 5  NB. amount to offset tooltip from the hover

NB. This is called in the locale and with the psel of whatever form is active - the main or an explorer.
NB. The timer will run in the locale this was called from
NB. x is (window type 0=main 1=exp);(1 to write to the statline (which doesn't exist on explorers))
hoverstart =: 4 : 0
'isexp writestat' =. x
if. #hoverinitloc do.  NB. We are hovering.  Does this continue the same hover?
  if. MAXHOVERMOVEMENT < >./ | y - hoverinitloc do. hoverend'' end.
end.
if. 0 = #hoverinitloc do.   NB. If no hover running (and perhaps we just cleared it), start one
NB.?lintonly wdtimer =: wd
  wdtimer (tooltipdelayx,2) {:: TOOLTIPDELAYCHOICES  NB. start the hover timer
  hoverinitloc =: y
  hoverisexp =: isexp
  NB. The same action that starts the hover timer will set the status line immediately
  if. writestat do. statlinedo y end.
  NB.?lintsaveglobals
end.
)

NB. Nilad.  Ask the owner for a tooltip and display it if there is one
NB. This runs in the hovering locale, either the main form or an explorer
hoverdo =: 3 : 0
if. #hoverinitloc do.   NB. should always be there, but we might get a late timer event
NB.?lintonly hoverinitloc =: 0 0
  NB. Get the locale of the object.  If we are on an explorer, it's just that; if on the main, we have to
  NB. look for the pickrect
  if. hoverisexp do.
    pickloc =. coname''
    yx =. hoverinitloc
  else.
    if. #pr =. locpickrects findpickhits hoverinitloc do.
      'l yx' =. {. pr
      pickloc =. l { picklocs
    else.
      pickloc =. ''
NB.?lintonly yx =. 0 0
    end.
  end.
NB.?lintonly pickloc =. <'dissectobj'
  if. #pickloc do.
    hstring =. hoverDO__pickloc hoverisexp;yx
    if. #hstring do. drawtooltip hoverinitloc;hstring end.
  end.
end.
)

NB. y is mouse position.  Write to the status line, which is a lot like a hover
NB. This runs in the hovering locale, either the main form or an explorer
statlinedo =: 3 : 0
stattext =. ''
for_r. pr =. locpickrects findpickhits y do.
  'l yx' =. r
  pickloc =. l { picklocs
NB.?lintonly pickloc =. <'dissectobj'
  stattext =. statlineDO__pickloc hoverisexp;yx
end.
'fmstatline' wdsettext stattext
0 0$0
)


NB. obsolete fmt=: 4 : 0
NB. obsolete  e=. (' ' I.@:= y),#y
NB. obsolete  LF (e {~ <: tc e I. (x+2)+}:_1,e)} y
NB. obsolete )
NB. obsolete 
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
glfontextent TOOLTIPFONT , ": ttfontsizex { TOOLTIPFONTSIZECHOICES
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
(TOOLTIPMAXPIXELS <. <. TOOLTIPMAXFRAC * 2 { 0 ". wdqchildxywh 'dissectisi') reflowtooltip y
)

NB. y is cursor position;string
NB. Draw a tooltip there after saving the pixels
drawtooltip =: 3 : 0
'cpos string' =. y
NB. There is a tooltip.  Display it.
NB. Copy the pixels we are about to overwrite
'ctly ctlx' =. 3 2 { 0 ". wdqchildxywh 'dissectisi'
'hovery hoverx' =. cpos
'ttiph ttipw' =. (TOOLTIPCOLOR;TOOLTIPTEXTCOLOR;TOOLTIPFONT;(ttfontsizex { TOOLTIPFONTSIZECHOICES);TOOLTIPMARGIN;'') sizetext <string  NB. kludge
NB. Position the tooltip to be on screen.  We try to put the bottom-left corner at the hover offset.
NB. Get desired top position; if it's off the top of the screen, switch to below the hover
if. 0 > ttipy =. HOVEROFFSETY + hovery - ttiph do. ttipy =. hovery - HOVEROFFSETY end.
NB. Get desired left position, but if that goes offscreen right, move left; then if offscreen left, move right
ttipx =. 0 >. (+   0 <. ctlx - ttipw + ]) hoverx + HOVEROFFSETX
NB. That's the topleft of the tooltip.  Now calculate the rectangle that we will use to save the pixels
NB. We have to save an extra pixel all the way around (seeming glrect error), and we have to make sure
NB. that the rectangle is all onscreen, else QT will return all 0 pixels
ttpyx =. 0 >. _1 + ttipy,ttipx
ttphw =. (2 + ttiph,ttipw) <. (ctly,ctlx) - ttpyx
tooltippixels =: glqpixels 1 0 3 2 { , tooltippixpos =: ttpyx,:ttphw
(TOOLTIPCOLOR;TOOLTIPTEXTCOLOR;TOOLTIPFONT;(ttfontsizex { TOOLTIPFONTSIZECHOICES);TOOLTIPMARGIN;'') drawtext string;2 2 $ ttipy,ttipx,ttiph,ttipw  NB. kludge
glpaint''
NB.?lintsaveglobals
)

NB. Nilad.  Turn off the hover timer.  If a tooltip is active, restore the pixels it covered
hoverend =: 3 : 0
hoverinitloc =: $0
NB.?lintonly wdtimer =: wd
wdtimer 0
if. 0 = 4!:0 <'tooltippixels' do.
  NB. if the tooltip has a minimum lifetime, delay until that lifetime has been exceeded (kludge).  Should be short
  if. 0.01 < reqddelay =. hoversessmin - 6!:1'' do. 6!:3 reqddelay end.
  glpixels (1 0 3 2 { , tooltippixpos) , tooltippixels
  glpaint''
  4!:55 <'tooltippixels'
end.
)



NB. **************************** mouse events in the graphics window ********************
NB. in the locale of the main form

NB. pick flags
'PICKLB PICKRB PICKCTRL PICKSHIFT' =: |. 1 bwlsl~ i. 4

NB. mouse button, both left and right.  Return number of picks performed.
NB. x is l or r, y is formatted sysdata
dissect_dissectisi_mbdown =: 4 : 0
hoverend''
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
for_r. pr =. locpickrects findpickhits 1 0 { y do.
  'l yx' =. r
  pickloc =. l { picklocs
NB.?lintonly pickloc =. <'dissectobj'
  x pickDO__pickloc 0;yx;#. 4 5 6 7 { y
end.
#pr
NB.?lintsaveglobals
)
dissect_dissectisi_mbldown =: 3 : 0
NB. If the user left-clicked outside a pickrect, that is the start of a scroll operation.
NB. Remember the clicked position, and the pixels in the screen (we use the presence of
NB. the screen buffer as an indicator of scroll-in-progress, and delete it when we're done,
NB. since it's big)
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
if. 0 = 'l' dissect_dissectisi_mbdown sd =. 0 ". sysdata do.
NB. Read the pixels from the end of the sentence area to the bottom of the screen
  picksentencepixels =: glqpixels 1 0 3 2 { , -~/\ sentencebrect
  scrollblock =. -~/\ (0 (1}) {: sentencebrect) ,: 3 2 { sd
  pickpixels =: ({: $ glqpixels@:,@:(|."1)) scrollblock
  pickscrollcurryx =: pickscrollstartyx =: 1 0 { sd
NB.?lintsaveglobals
end.
''
)

dissect_dissectisi_mbrdown =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
'r' dissect_dissectisi_mbdown 0 ". sysdata
''
)

NB. mouse movement.  If we are scrolling, drag the pixels along
NB. If we are dragging a scrollbar, vector to the object locale to handle that
dissect_dissectisi_mmove =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
sd =. 0 ". sysdata
if. 0 = 4!:0 <'pickpixels' do.
  pickscrollcurryx =: 1 0 { sd
  glclear''
  scrollblock =. -~/\ (0 (1}) {: sentencebrect) ,: 3 2 { sd
  glpixels (|. ({. scrollblock) + pickscrollcurryx - pickscrollstartyx) , (|. $ pickpixels) , , pickpixels
  glpixels (1 0 3 2 { , -~/\ sentencebrect) , picksentencepixels
  glpaint''
elseif. 0 = 4!:0 <'scrollinglocale' do.
NB. Perform the scroll, on the main window, but in the locale of the data
  0 scrollmmove__scrollinglocale 1 0 { sd
elseif. do.
NB. mmove not for scrolling.  If a mouse button is down, stop the hover; otherwise
NB. start or continue the hover
  if. 1 e. 4 5 8 9 10 { sd do.   NB. If any button down...
    hoverend''
  else.
    0 1 hoverstart 1 0 { sd
  end.
end.
)

NB. mouse release.  If we are scrolling, set the new offset and redraw
NB. If we are dragging a scrollbar, vector to the object locale to finish that
dissect_dissectisi_mblup =: 3 : 0
hoverend''
if. 0 = 4!:0 <'pickpixels' do.
  4!:55 ;: 'pickpixels picksentencepixels'  NB. indicate end-of-scroll
NB. Use the last-drawn position as the new position.  If it hasn't changed from the original, redraw
  if. pickscrollcurryx -.@-: pickscrollstartyx do.
    scrolltlc =: <. scrolltlc + pickscrollcurryx - pickscrollstartyx
    dissect_dissectisi_paint 0  NB. no need to recalc placement
  end.
elseif. 0 = 4!:0 <'scrollinglocale' do.
  4!:55 ;: 'scrollinglocale scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit'  NB. indicate end-of-scrollbar
end.
)

dissect_dissectisi_focuslost =: 3 : 0
hoverend''
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

NB. x is DOL descriptor, y is flatyx, result is CSF for path within the noun.  No shortcuts are used in the result
yxtopath =: 4 : 0
flatrc =. (>: y) (I.~ }:)&> 1 2 { x  NB. Look up to find containing row/col
s =. 0 {:: x   NB. shape of the noun
NB. Split the shapeused into vert;horiz, ending on horiz.  OK to add high-order 0s to
NB. ensure that there is some infix of length 2.
NB. Convert row/col to indexes.  Interleave the indexes for row/col to get cell indexes.  Remove 0
NB. if it was added
NB. Box the indexlist
indexlist =. < (-#s) {. , |: (|. |: _2&(]\)&.|. 0 0 , s) #: flatrc
NB. If there is no lower boxing level, indexlist is the result
if. 3 < #x do.
  NB. Boxed noun.  Recur to look up the next level.  Offset the yx to within the subbox
  NB. Start by selecting at this level and dropping down, followed by the later levels
  NB. Offset within the inner box by the margin plus the leading linewidth
  < (indexlist , SFOPEN) ; > ((3;indexlist) {:: x) yxtopath y - (BOXLINEWIDTH + BOXMARGIN) + flatrc ({ 0&,)&> 1 2 { x
else.
  NB. Return the indexlist as a boxed CSF (rank-3)
  <,<indexlist
end.
)

NB. Nilad.  Returns the next locale in the inheritance chain for the current node.
NB. This will usually be the next in chain, but some nodes (such as u^: when the user is selecting item 0)
NB. don't allow selection; they return an empty to stop the selection search
getnextpickloc_dissect_ =: 3 : 'inheritedfrom' 
recursiveselection_dissect_ =: -@*@{.@[   NB. x is sellevel at end: 0 if it is 0, _1 otherwise
NB. y is flattened CSF for the current cell (with higher-level selections removed), i. e. a list of {selection[,{SFOPEN..}]}...
NB. The current locale is a node that is displayed in the current box.  We see if the
NB. selection applies at this node; if so, we propagate it to all descendants.  If not,
NB. we go to the next locale in the inheritance chain and give it a chance.  We have to
NB. make sure rthat a selection in u in u@:v is not propagated to v; so it must be seen
NB. as inapplicable to u@:v.
NB. Result is 1 if we made a change and a redraw is needed, 0 if there was no frame at all
NB. (i. e. sellevel = 0 at the end of the chain), _1 if there were selections but no more allowed,
NB. _2 if selection not allowed because of empty in frame
NB. x is (sellevel of this node);(rank of cell of previous verb)
recursiveselection =: 3 : 0
0 _1 recursiveselection y
:
NB. First, try custom selection, and if it made a change, return fast since it finished the pick
if. selectionoverride'' do. 1 return. end.
'inslevel prevcellrank' =. x
QP^:DEBPICK'selecting in ?defstring]0%coname''''%x%y%initialselection%sellevel%selections%selframe%frame%resultlevel%'
selectionfound =. 0    NB. scalar = no selection
NB. If y is empty (possible if we are running to end), make it an empty list
if. 0 = #y do. y =. ,a: end.
if. #resultlevel do.
  assert. resultlevel e. 0 1
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
        if. #initialselection do. selectionfound =. ,initialselection end.
      else. selectionfound =. 0$a:
      end.
  else.
    NB. This node has frame.  Look at the leading elements of y.  If they don't match the current
    NB. selection (or if there is no current selection), we've seen enough: the selection starts
    NB. at this level.
    NB. But: if the frame contains 0, no selection can be valid (it will perforce contain an index error)
    NB. and we must abort with a status code indicating the fact
    if. 0 e. selframe do. _2 return. end.
    if. 0 = #localf do.
    NB. Forced selection.  It was propagated when first detected, so we just ignore the node and
    NB. keep looking
    elseif. sellevel >: #selections do. selectionfound =. ,<localf   NB. should never be >
    elseif. localf -.@-: sellevel {:: selections do. selectionfound =. ,<localf
    elseif. (sellevel = <:#selections) *. (*#initialselection) do. selectionfound =. ({: selections) , initialselection
    elseif. sellevel < <:#selections do. selectionfound =. 0$a:   NB. remove expansion
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
NB. obsolete   if. SINGLESELECTION do.
NB. obsolete     NB. To avoid confusion, we confine the selection to a single path.  We enforce this by clearing any selections in the tree that are
NB. obsolete     NB. at the level we are about to select.  So, if we are at sellevel=2, we will limit all selections in nodes of level 2 or higher to 2 values; then we will
NB. obsolete     NB. propagate our selection (which may have more than one added value, if it includes an initialselection).  By not looking at
NB. obsolete     NB. nodes below level 2 we will not disturb an initialselection in the level-1 node that is our parent.
NB. obsolete     discardselectionsabovelevel__resultroot__COCREATOR sellevel
NB. obsolete   end.
  NB. Now propagate the new selection
NB. obsolete   propsel (sellevel {. selections) , selectionfound
  makeselection , selectionfound
NB. obsolete NB. Clear the scroll point in all the nodes for which the selection has changed.  The old scroll point may be invalid
NB. obsolete   propscroll 1   NB. 1 causes the scroll to be unchanged in THIS node, cleared to the leaves
  1  NB. We made a change
else.
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
selx =. ; > valueformat yxtopath BOXMARGIN -~^:(3<#valueformat) (x{scrollpoints) + y
QP^:DEBPICK 'y selx '
QP^:DEBPICK 'sellevel #selections selections '
NB. If sellevel exceeds the number of selections, there's nothing really here - it must be an empty
NB. operand that displays a boundary.  Don't pick then
if. sellevel <: #selections do.
  NB. Process the cells mapped to this block, to see which one gets the selection.  Start the search in
  NB. the highest containing locale for the block, even if we displayed the value from a different one (because of error)
  tail =. findinheritedtail''
  NB.?lintonly tail =. <'dissectobj'
  recursiveselection__tail selx
else.
  0
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
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
hoverend''
yx =. EXPLORERYX -~ 1 0 { sd =. 0 ". sysdata
if. *./ yx < {:DOsize do. 'l' pickDO 1;yx;#. 4 5 6 7 { sd end.
)


NB. mouse movement.
NB. If we are dragging a scrollbar, vector to the object locale to handle that
explorer_dissectisi_mmove =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100 100 100 100 100'
sd =. 0 ". sysdata
if. 0 = 4!:0 <'scrollinglocale__COCREATOR' do.
NB. Perform the scroll, on this explorer window.  If the scroll was started in a different locale,
NB. abort it.
  if. scrollinglocale__COCREATOR -: coname'' do.
    1 scrollmmove 1 0 { sd
  else.
    explorer_dissectisi_mblup''
  end.
elseif. do.
NB. mmove not for scrolling.  If a mouse button is down, stop the hover; otherwise
NB. start or continue the hover
  if. 1 e. 4 5 8 9 10 { sd do.   NB. If any button down...
    hoverend''
  else.
    1 0 hoverstart 1 0 { sd
  end.
end.
)

NB. mouse release.  If we are scrolling, set the new offset and redraw
NB. If we are dragging a scrollbar, vector to the object locale to finish that
explorer_dissectisi_mblup =: 3 : 0
if. 0 = 4!:0 <'scrollinglocale__COCREATOR' do.
  4!:55 ;: 'scrollinglocale__COCREATOR scrollingaxis scrollingorigscrollpt scrollingorigclick scrollinglimits scrollptlimit'  NB. indicate end-of-scrollbar
end.
)

NB. right-click in explorer - delete the explorer window
explorer_dissectisi_mbrdown =: explorer_close

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
NB. We add parentheses around y if the last word of x and the first word of y are both numeric
jd =: 4 : 0
if. '.:' e.~ {. y do. y =. ' ' , y end.
if. ({: ;: x) *.&('0123456789_' e.~ {.@>) ({. ;: y) do. y =. '(' ([`(' '&(i.&0@:=)@])`(' '&,@]))} y , ')' end.  NB. Replace last space with (
x , y
)

NB. x is height(s) of v; y is height of u: always a list with 1 atom (must be the single height out of u)
NB. Result is combined heights of u-then-v: the sum, but if either operand is _1, result must be _1
combineheights =: (+`_1:@.(0><.)"0 {.)"1


cocurrent 'dissectobj'

NB. **** pick support *****
NB. y is the selection, a list
NB. result is the selection to store in the node.  This will refer to the selected item but it might
NB. be negative to suggest negative indexing
selectiontodisplay =: ]

NB. **** support expansion node ****
NB. Given a single verb, create ]@v, but with the title of ']' changed to the string form of v and the stealthoperand type set to 4
NB. (which will suppress the display node but will not count as ][ for detection purposes)
NB.
NB. It is expected that v is an expandable node.  The purpose of this collection node is to provide a place to
NB. break the chain at v.  v will always be displayed as a v-node, and the display of ] will be a u-type which will
NB. be inherited into whatever comes next, carrying the single true result of v
NB.
NB. y is the locale of v.  Result is the result line for ]@v

insertcollector =: 3 : 0
NB. Create an object to handle ]@v
NB. First, the verb ]
iop =: 1 {:: COCREATOR createverb ((']');defstring 0);($0)  NB. execute as ']', display as defstring
NB. iop is public because we might have to change it later
NB.?lintonly iop =: <'dissectobj'
stealthoperand__iop =: 0   NB. Mark the verb as displayable
NB. Now create an object for ]@u
NB. NOTE: scalar '@' is a signal that the @locale created is an expansion node
localeat 1 createmodifier _3 [\ verb;iop;($0); conj;'@';($0); verb;y;($0)

NB.?lintsaveglobals
)

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
NB. y is the number of results we actually got
NB. result is index list of the failing location, in natural order
getfailingisf =: #:

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

NB. x is selection, in ISF form
NB. y is the selectors, shaped into a form suitable for indexing
selectusingisf =: 4 : 0
NB. In this version, only a single selection is supported, possibly followed by dropping down a level, which we ignore here
NB. because it's used for display only (the selected results were collected only after dropping down)
({. > isfensureselection isftorank2 x) {^:(*@#@[) y
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

NB. x is the selected indices that matched the selector
NB. y is max size of a selresult as calculated by checkframe
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
QP^:DEBHLIGHT'addselected:defstring=?defstring]0 >coname'''' y opselin '
NB. Create sequence of boxes: H0; P0 H1; P0 P1 H2; etc.  Prepend highlight color, which is 1 more than the sellevel at which the selection was added
allh =. (>:&.>@{: ,. ((_1}&.>~ <)"0~ <\)~/@}:) (#"1~   sellevel <: >@{:) y
opselin =: opselin , (#~  a: ~: {:"1) allh  NB. If nothing left after discarding sellevel, add nothing
QP^:DEBHLIGHT'opselin allh '
)

NB. ***** Gerund management *****
NB. Nilad.  The locale called must be a noun locale.  The result is the list of verb locales that make up
NB. the gerund in the locale.  If the locale is not a gerund, the result is empty.
querygerund =: (0$a:)"_


cocurrent 'dissectrighttoleft'
NB. y is anything - intervals, results, fillmasks - that has been expanded into an array using the shape of the frame.
NB. y is in ticket order, i. e. execution order
NB. Result is the array reordered to natural order, i. e. selection order (some primitives process out of order; we reorder to match selection)
tickettonatural =: |.

NB. y is boxed selection (number only, no SFOPEN) in natural order; result is boxed selection in execution order
selectiontoticket =: 3 : 0
NB.?lintonly 'selopshapes frame selections sellevel' =: (2$a:);($0);(1$a:);0
< ({.frame) | _1 - >y
)

NB. x is the frame of the full expected result
NB. y is the number of results we actually got
NB. result is index list of the failing location, in natural order.
NB. Since only expansion nodes come through here, append SFOPEN to complete the selection
getfailingisf =: 4 : 0
NB.?lintonly SFOPEN =. SFOPEN_dissectobj_
SFOPEN ;~ x #: _1 - y  NB. count back from the end
)

cocurrent 'dissectallnouns'

NB. Inherit pickDO from default object
NB. For all these verbs, x is button flags, y is the yx position of the click relative to start of pickrect

NB. Since labeled nouns are never SDTs, ignore a click on the name
NB. Nouns are either primitives or SDTs.  Clicking on the shape will expand an SDT if possible.  Once expanded,
NB. the display becomes a verb, ad cannot be collapsed.
picklDOshapepos =: 4 : 0
NB.?lintonly 'nounhasdetail nounshowdetail COCREATOR' =: 0;0;< <'dissectobj'
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
end.
0 0$0
)

picklDOdatapos =: 4 : 0
NB. If the display of a noun's detail is suppressed, and it has detail, any click on it will turn on the detail
NB.?lintonly 'nounhasdetail nounshowdetail COCREATOR' =: 0;0;< <'dissectobj'
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
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
NB. Monad.  y is string form of the noun;name if it is a name;tokens it came from
create =: 3 : 0
NB. Not clonable
create_dissectobj_ f. 2 { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand, and the name if any.  We look into op from other locales to get the value of sdts
'op varname' =: 2 {. y
NB. If the name is empty, this must be an SDT
resultissdt =: 0 = #varname
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
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

calcestheights =: 3 : 0
estheights =: ,1  NB. give each result a height of 1
)

proplocales =: 3 : 0
(y = 3) # <tokensource
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: 4 : 0
assert. 0 = #x [ 'Noun must have no layouts'
traversedowncalcselect y  NB. To set globals, including selresult
'displayhandlesin displayhandleout displaylevrank nounhasdetail' =: ($0);1;varname;0
x ,&< coname''  NB. Return the empty DOLs
)

cocurrent 'dissectverb'
coinsert 'dissectobj'
NB. y is (string form of the verb);tokens it came from
NB. If the string form is boxed, it contains (string form);(title for display purposes)
create =: 3 : 0
create_dissectobj_ f. 1 { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand, as the display and executable form (we may modify the display form later)
'execform titlestring' =: ,&.> boxopen 0 {:: y
stealthoperand =: 1 2 3 4 5 6 0 {~ ((;:'][[:'),']';']]';'[[') i. <titlestring  NB. '[[' and ']]' are always-invisible forms
titlestring =: stealthoperand {:: titlestring; ;: '][[:]]['
NB. Every verb counts as an sdt for modifier processing.
resultissdt =: 1
NB. If this verb has a one-line description, save it
onelinedesc =: 2 {:: y , <''
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Save the number of operands for this invocation
NB. Return value is the locale name (possibly changed)
NB. y is, for each operand, whether the operand is from SDTs
setvalence =: 3 : 0
valence =: #y
resultissdt =: *./y
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: , (<valence,dispstealthoperand) {:: a: , (1;0;0;0;0;1;0) ,: (1 1;_1 0;0 _1;0 0;0 0;_1 0;0 _1)
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
NB. Apply parentheses if right conjunction operand - but only if more than 1 word
enparen^:((y>2) *. 1 < #@;: execform ) execform
)

NB. return string form of operands, including instrumentation
NB. y tells what intrumentation is needed:
NB.  (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 for inverse also)

exestring =: 3 : 0
NB. init for logging
initloggingtable ''
NB. Instrument the forward verb - bivalent
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , execform , '))'
NB.?lintonly 'logvalues logticket' =: (1$a:);$0
NB.?lintsaveglobals
)

proplocales =: 3 : 0
(y = 3) # < tokensource
)
NB. Set globals, then initialize display for the verb
traverse =: 4 : 0
assert. 1 2 e.~ #x
traversedowncalcselect y  NB. Just to set error globals
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. If no vranks, this verb must have failed to execute owing to upstream error.  Leave no levrank then
'displayhandlesin displayhandleout displaylevrank' =: ((($0);(,0);_0.3 0.3) {::~ dispstealthoperand { valence , 1 1 1 1 1 1);1;<rankhistory
NB. Pass the DOLs through, but mark a stealthoperand for removal by deleting the output handles
if. (valence = 2) *. dispstealthoperand e. 1 2 5 6 do.
  x =. a: (<3 ,~ <:3 bwand dispstealthoperand)} x
end.
x ,&< coname'' NB. no v, so no change to the DOLs
)

exegesisrankstack =: 3 : 0
if. execform -.@:-: titlestring do.
  NB. If this is a final node (execform not the same as titlestring), explain the expansion
  NB. We will know that the node has expanded if its initialselection is present
  if. (selectable+sellevel) < #selections do.  NB. expansion selected
    r =. ,: EXEGESISRANKSTACKEXPLAIN ; 'This block displays the final result of the verb:',LF,(> {: <^:(0=L.) titlestring),CR,'The block feeding into this one shows the details of the computation. Select this result again to hide the details.',LF
  else.
    r =. ,: EXEGESISRANKSTACKEXPLAIN ; 'This block displays the final result of the verb:',LF,(> {: <^:(0=L.) titlestring),CR,'To see the details of the computation, select the result to see a block containing the intermediate results.',LF
  end.
elseif.  '^:_1' -: _4 {. execform do.
  NB. If this was an inverse added by &.&.:, explain that
  r =. ,: EXEGESISRANKSTACKEXPLAIN ; 'This inverse was added to complete an operation started by &. or &.:',LF
elseif. do. r =. 0 2$a:
end.
if. #onelinedesc do.
  r =. r , EXEGESISONELINEDESC ; 'The definition of this verb is:',LF,onelinedesc,CR
end.
if. (#verbexplains) > tx =. (0{"1 verbexplains) i. <titlestring do.
  r =. r , EXEGESISVERBDESC ; ((<tx,valence) {:: verbexplains),LF
end.
r
)

NB. Quick descriptions of all primitive verbs
verbexplains =: _3 ]\ <;._2 (0 : 0)
=
=y indicates, for each item in the nub of y, whether it matches each item of y
x=y is 1 if the atoms x and y are tolerantly equal
<
<y boxes y
x<y is 1 if the atom x is tolerantly less than the atom y
<.
<.y is the largest integer not exceeding y
x<.y is the smaller of the atoms x and y
<:
<:y is y-1
x<:y is 1 if the atom x is tolerantly less than or equal to the atom y
>
>y unboxes each atom of y
x>y is 1 if the atom x is tolerantly greater than the atom y
>.
>.y is the smallest integer not less than y
x>.y is the larger of the atoms x and y
>:
>:y is y+1
x>:y is 1 if the atom x is tolerantly greater than or equal to the atom y
_:
_:y is infinity, regardless of y
x_:y is infinity, regardless of x and y
+
+y is the complex conjugate of y
x+y is x plus y
+.
+.y is a 2-atom list of the real and imaginary parts of the atom y
x+.y is x OR y if x and y are Boolean; generally, the greatest common divisor of x and y
+:
+:y is 2*y
x+:y is the negation of x OR y
*
*y is signum(y): _1 if y<0, 0 if y tolerantly=0, 1 if y>0
x*y is x times y
*.
*.y is a 2-atom list of the length and angle of the atom y, in the complex plane
x*.y is x AND y if x and y are Boolean; generally, the least common multiple of x and y
*:
*:y is y^2
x*:y is the negation of x AND y
-
-y is the negative of y
x-y is x minus y
-.
-.y is 1-y
x-.y is x, with any items removed that match cells of y
-:
-:y is y%2
x-:y is 1 if the arrays x and y match, in shape and values
%
%y is 1%y
x%y is x divided by y
%.
%.y is the matrix inverse of y (pseudoinverse if y is not square)
x%.y is ((%. y) +/ . * x)
%:
%:y is the square root of y
x%:y is the xth root of y
^
^y is e^y
x^y is x raised to the power y
^.
^.y is ln(y)
x^.y is the logarithm of y, using base x
$
$y is the shape of y
x$y is an array made by using items of y, with the frame given by x
$.
$.y creates a sparse matrix from y
x$.y performs a sparse-matrix operation
~.
~.y is the unique items of y, in their original order

~:
~:y is a Boolean for each item of y, 1 if no previous item matches it
x~:y is 1 if the atoms x and y are not tolerantly equal
|
|y is the magnitude of y
x|y is y(mod x), the remainder after dividing y by x
|.
|.y is y, in reversed item order
x|.y is y, with the items rotated left x positions
|:
|:y is the transpose of y, y with the axes running in reversed order
x|:y is y with the axes x moved to the end
,
,y is a list of all the atoms of y
x,y joins x and y into a single array, with the items of x followed by the items of y
,.
,.y is a table where each row is a list of the atoms from one item of y
x,.y is x,"_1 y, and joins corresponding items of x and y
,:
,:y is y with a leading axis of length 1 added
x,:y is an arfay with two items, the first coming from x and the second from y
#
#y is the number of items of y
x#y is an array in which each atom of x tells how many times the corresponding item of y appears
#.
#.y is 2 #. y
x#.y converts the list y to a single number, using x as the place values of the representation
#:
#:y is the binary representation of y
x#:y converts the number y to a list, using x as the place values of the representation
!
!.y is factorial(y)
x!.y if the number of combinations of y things taken x at a time
/:
/:y is the permutation that would put y into ascending order
x/:y is x sorted into ascending order using the corresponding values of y as keys
\:
/:y is the permutation that would put y into descending order
x/:y is x sorted into descending order using the corresponding values of y as keys
[
[y is y
x[y is x
]
]y is y
x]y is y
{
{y is the Cartesian product of the contents of boxes of y
x{y is a selection from y, using x to control the selection
{.
{.y is the first item of y
x{.y takes the first x items of y (last items if x is megative)
{:
{:y is the last item of y

}.
}.y is all the items of y except the first
x}.y drops the first x items of y (last items if x is megative)
}:
}:y is all the items of y except the last

".
".y executes the sentence y, giving its result (if a noun)
x".y converts the string y to numeric, using x as default in case of invalid values
":
":y converts y to string form using default conversions
x":y converts y to string form using conversions specified by x
?
?y is a random number (between 0 and 1 if y=0, a nonnegative integer less than y otherwise)
x?y is x distinct nonnegative random integers less than y
?.
?.y is like ?y but uses a fixed starting value
x?.y is like x?y but uses a fixed starting value
A.
A.y is the permutation number of the permutation y
x A.y reorders the items of y using the permtation whose number is x
C.
C.y converts the permutation y between direct and cycle form
x C.y  reorders the items of y using the permtation x
e.
e.y gives, for each opened atom of y, a list indicating which items of ;y are in it
x e.y is 1 for each cell of x that is an item of y, 0 for cells of x not in y
E.

x E.y a Boolean array, of shapes the same as that of y, with 1s at the starting points of subarrays that match x
i.
i.y is an array of consecutive natural numbers of shape y
x i.y for each cell of y, the index of the first matching item of x, or #x if there is no match
i:
i:y equally-spaced numbers between -y and +y
x i:y for each cell of y, the index of the last matching item of x, or #x if there is no match
I.
I.y for Boolean y, a list of the indexes of 1s
x I.y the index within y before which x could be inserted keeping the list in order
j.
j.y is 0j1*y
xj.y is x + 0j1 y
L.
L.y is the boxing level of y, 0 if unboxed

o.
o.y is 1p1*y
x o.y is a trigonometric function of y depending on x
p.
p.y converts polynomial y between coefficient and product-of-roots form
x p.y evaluates the polynomial x and y
p..
p..y is the first derivative of polynomial y
x p..y is the integral of polynomial y, with x the integration constant
p:
p:y is the yth prime number (2 is the 0th)
x p:y is a prime-related function of y depending on x
q:
q:y is the prime factorization of y, in ascending order
x q:y is the exponents of the prime factorization of y
r.
r.y is a complex number on the unit circle, with angle y
x r.y is the complex number with length x and angle y
s:
s:y creates a symbol to stand for the string y
x s:y is a symbol-related function of y depending on x
u:
u:y is the Unicode character corresponding to y
x u:y is a Unicode-conversion function of y depending on x
x:
x:y converts y to extended precision
x x:y is a precision-conversion function of y depending on x
;
;y is the items of the contents of y, assembled in order
x;y boxes x (and y, if y is unboxed), and joins the boxes into a list
;:
;:y is a boxed list containing the words in the string y
x;:y executes the sequential machine x on the data y
)


NB. *** traversal support ***
operationfailed =: 1:   NB. An error found during verb execution is a stopper


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
if. recursionhere =: recursionencountered__COCREATOR do.
  NB. If there is a recursion inside this execution, insert a recursion point
  uop =: 'dissectrecursionpoint' 1 createmodifier uop,yop
end.
NB.?lintonly uop =: <'dissectrecursionpoint'
NB. Tell the verb its valence; the result is the operands that are needed for display.  Here, in this non-verb,
NB. we save the operands needed by the first verb.  The rule is, we will pass to a verb ONLY the operands that
NB. it says it can use.  For comp. ease we may compute an operand but then immediately discard it.
executingvalence__COCREATOR =: 1
uop =: setvalence__uop ,resultissdt__yop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB.?lintonly uop =: <'dissectrecursionpoint'
noun;(coname'');''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y>0) (defstring__uop 0) jd ' ' , (defstring__yop 0)
)

calcestheights =: 3 : 0
estheights =: estheights__uop combineheights estheights__yop  NB. the height is the sum of verb and its noun
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , (exestring__uop '') , ' (' , (exestring__yop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(<^:(0=L.)@".@>^:(0 <: y) ;: 'uop'),(yop #~ y > 1)
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
traverse =: 4 : 0
nounhasdetail =: 0  NB. No detail, unless we suppress history later
traversedowncalcselect TRAVNOUN  NB. To set globals only - there are no inputs here
ylayo =. x traverse__yop TRAVNOUN
NB. If a noun operand failed, pull the plug and display only that result
if. errorcode__yop > EOK do. ylayo return. end.  NB. If the noun failed, this node must have failed too
NB. put this locale on the stack as the outermost monad/dyad execution
executingmonaddyad__COCREATOR =: executingmonaddyad__COCREATOR ,~ coname''
if. 1 = #ures =. (joinlayoutsl`<@.recursionhere ylayo) traverse__uop travops TRAVOPSSTARTHEAVY;0;(uopval yop);<<selresultshape__yop do.
  NB. If we don't have a locale-name to inherit from, it means that uop was an expansion node
  NB. and it took over the display of u.  We must display the result here separately.
  'displayhandlesin displayhandleout displaylevrank' =: ((,0));1;<,: 'Result after all recursions';(coname''),<_
  ures =. ures ,< coname''
else.
  ures =. inheritu ures
end.
NB. Remove the entry from the stack
executingmonaddyad__COCREATOR =: }. executingmonaddyad__COCREATOR
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displayhandlesin displaylevrank nounhasdetail physreqandhighlights__inheritroot' =: ($0);NORANKHISTNOUN;1;<NOPHYSREQ
  NOLAYOUTS ,&< coname''
else.
  ures
end.
NB.?lintsaveglobals
)

NB. Add on a description of the rank line if applicable.  It is, if this is a Final node
exegesisrankoverall =: 4 : 0
if. recursionhere do.
  if. y -: '' do.
    NB. Result has not expanded
    res =. ,: (EXEGESISRANKOVERALLEXPLAIN,0) ; 'This is the final result of a recursive verb. To see the results of all recursions, click on the result of a recursion (in a block labeled $:).',LF
  else.
    NB. Result has expanded
    res =. ,: (EXEGESISRANKOVERALLEXPLAIN,0) ; 'This is the final result of a recursive verb. The block feeding into this shows the results from each recursion.',LF
  end.
else.
  res =. 0 2$a:
end.
res
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
if. recursionhere =: recursionencountered__COCREATOR do.
  NB. If there is a recursion inside this execution, insert a recursion point
  uop =: 'dissectrecursionpoint' 1 createmodifier uop,yop,xop
end.
NB.?lintonly uop =: <'dissectverb' [ yop =: xop =: coname''
executingvalence__COCREATOR =: 2
uop =: setvalence__uop resultissdt__xop,resultissdt__yop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB.?lintonly uop =: <'dissectrecursionpoint'
noun;(coname'');''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y>0) (defstring__xop 1) jd ' ' , (defstring__uop 0) jd ' ' , (defstring__yop 0)
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '(' , (exestring__xop '') , ') ' , (exestring__uop '') , ' (' , (exestring__yop '') , '))'
)

calcestheights =: 3 : 0
NB. The height will be just one number.  Any node using a noun as operand must be prepared for that
estheights =: , >./ , (estheights__xop ,: estheights__yop) combineheights ,. estheights__uop
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(xop #~ y > 1),(<^:(0=L.)@".@>^:(0 <: y) ;: 'uop'),(yop #~ y > 1)
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
traverse =: 4 : 0
nounhasdetail =: 0  NB. No detail, unless we suppress history later
traversedowncalcselect TRAVNOUN  NB. To set globals only - there are no inputs here
ylayo =. x traverse__yop TRAVNOUN
if. errorcode__yop > EOK do. ylayo return. end.  NB. If an argument failed, this node will have failed also
xlayo =. x traverse__xop TRAVNOUN
if. errorcode__xop > EOK do. xlayo return. end.
NB. put this locale on the stack as the outermost monad/dyad execution
executingmonaddyad__COCREATOR =: executingmonaddyad__COCREATOR ,~ coname''
if. 1 = #ures =. (xlayo ,&(joinlayoutsl`<@.recursionhere) ylayo) traverse__uop travops TRAVOPSSTARTHEAVY;0;(uopval xop,yop);<selresultshape__xop ,&< selresultshape__yop do.
  NB. If we don't have a locale-name to inherit from, it means that uop was an expansion node
  NB. and it took over the display of u.  We must display the result here separately.
  'displayhandlesin displayhandleout displaylevrank' =: ((,0));1;<,: 'Result after all recursions';(coname''),2#<_
  ures =. ures ,< coname''
else.
  ures =. inheritu ures
end.
NB. Remove the entry from the stack
executingmonaddyad__COCREATOR =: }. executingmonaddyad__COCREATOR
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displayhandlesin displaylevrank nounhasdetail physreqandhighlights__inheritroot' =: ($0);NORANKHISTNOUN;1;<NOPHYSREQ
  NOLAYOUTS ,&< coname''
else.
  ures
end.
NB.?lintsaveglobals
)

NB. Add on a description of the rank line if applicable.  It is, if this is a Final node
exegesisrankoverall =: 4 : 0
if. recursionhere do.
  if. y -: '' do.
    NB. Result has not expanded
    res =. ,: (EXEGESISRANKOVERALLEXPLAIN,0) ; 'This is the final result of a recursive verb. To see the results of all recursions, click on the result of a recursion (in a block labeled $:).',LF
  else.
    NB. Result has expanded
    res =. ,: (EXEGESISRANKOVERALLEXPLAIN,0) ; 'This is the final result of a recursive verb. The block feeding into this shows the results from each recursion.',LF
  end.
else.
  res =. 0 2$a:
end.
res
)

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
recursionencountered__COCREATOR =: 0
coname''
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
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

calcestheights =: 3 : 0
estheights =: estheights__uop combineheights ,1     NB. add 1 for expansion node
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2)
)

NB. return string form of operands, including instrumentation within u but not within inverse of u
exestring =: 3 : 0
initloggingtable 1
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
traverse =: 4 : 0
traversedowncalcselect y
NB. Inside travdowncalcselect, we implied a selection of 0 even if there was no selection.  This was necessary
NB. to get the selector calculated even when there is no selection.

NB. We always traverse u.  If no recursion has been selected, we hide the expansion: we reset the selection level
NB. back to 0, and simply return the result from u.  If there is a selection, we show the expansion, by inheriting u
NB. into it and creating a layout for it, and we return the layout for the expansion with no locale, signifying that
NB. the monad/dyad should display its result without inheriting.
oprshape =. 0$a:
if. 0 = #selections do.
  bnsellevel =: <0  NB. reset selection level if the expansion is dormant
  for_l. |. yxop do.
    NB.?lintonly l =. <'dissectverb'
    oprshape =. oprshape , < selresultshape__l
  end.
else.
  NB. We need to reach into the noun operand(s) and change their selresult to show the values used for this recursion level.
  NB. We also have to recalculate the layout
  for_l. |. yxop do.
    NB.?lintonly l =. <'dissectverb' [ levelstartx =: 0
    'displaylevrank__l selresult__l nounhasdetail__l' =: (((2=#yxop) {:: 'I';' i' ,~ l_index { 'yx') , 'nput to recursion ',": (0;0) {:: selections);((levelstartx+l_index){logvalues);1
    oprshape =. oprshape , < $ L:0 > {. selresult__l
  end.
end.
NB. We suppressed the joinlayouts in the monad/dyad; do it now, now that we have the correct labels and values for the display
ures =. (; joinlayoutsl&.> x) traverse__uop travops TRAVOPSSTARTHEAVY;0;(uopval |.yxop);< oprshape
NB. return value may have been set at this point
if. #selections do.
  udol =. inheritu ures
  displaylevrank =: displaylevrank , 'Recursions';coname''
  < joinlayoutsl udol
end.  NB. instantiate expansion if it is live
)

NB. Add on a description of the rank line if applicable.  It is, if this is a Final node
exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN ; 'The contents of each box shows the result of one recursion, ordered in order of starting. Select one to see the inputs and computation that produced it.',LF,LF,'This rank stack also contains the last verb in the computation.',LF
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
NB. y is the number of results we actually got
NB. result is index list of the failing location, in natural order (i. e. selector order)
getfailingisf =: 4 : 0
NB. We need to select the starting index of the recursion that would have been next to finish - that is
NB. where the error is
NB. We add a dropdown, since this is a selection node
(< ,endforstart i. y) , SFOPEN
)


NB. y is a selector: any shape, but each selector has shape ,2; so $=?,2
NB. Result is array of boxes with one box for each selector, containing the
NB. indices of the results for each selector
findselection =: 3 : 0
NB. In this node the selector selects everything.  We just return all the indexes
NB. where valued is _1 (those are the results) 
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

NB. x is selection, in ISF form
NB. y is the selectors, shaped into a form suitable for indexing
selectusingisf =: 4 : 0
NB. We ignore y, the selectors calculated by selectticketintervals - they just include all the results, since the incoming selector
NB. was known to select everything.
NB. Instead, we develop the selector that picks out everything at the selected recursion level.  We find the
NB. indexes into valuesd/ticket where it starts and end+1s.
NB. Save the index of the startpoint - we will use this to fetch the operands
levelstartx =: ({.>x) { startlocs
selthisrecur =. ,. -~/\ levelstartx , >: endlocs {~ endforstart {~ {.>x
NB. Then, every time recursion level goes to 1 starts an included interval that runs through the next log entry (which must exist, either going up
NB. to a deeper level or down to finish this level, unless there was an error)
runlevel =. +/\ selthisrecur ];.0 logvaluesd
NB. In case there is an error and the last start never finished, append a high-value
((,. >:) 1 I.@:= runlevel) { selthisrecur ];.0 logticket , ticket__COCREATOR
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
NB. obsolete propsel ,< SFOPEN ,~ <, endforstart i. <: (endlocs { logticket) I. y
makeselection ,< SFOPEN ,~ <, endforstart i. <: (endlocs { logticket) I. y
)

NB. **** assignment ****
cocurrent 'dissectassign'
coinsert 'dissectobj'

NB. Assignment does nothing and has no display (for now).  We just have to keep things going for sentence display

NB. Assignment.  y is the stack block of the assignment fragment
create =: 3 : 0
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
NB.?lintonly uop =: vop =: <'dissectverb'
NB. Return the part of speech of the assigned value
((<2 0){y),(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
if. uopisname do.
  enparen^:(y>0) uop jd cop jd (defstring__vop 0)
else.
  enparen^:(y>0) (enparen defstring__uop 0) jd cop jd (defstring__vop 0)
end.
)
NB. No display, so no heights
calcestheights =: 3 : 0
estheights =: ,0  NB. height is immaterial: there is no input to this node
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
  auditstg '(' , uop , ' ' , cop , (exestring__vop '') , ')'
else.
  auditstg '((' , (exestring__uop '') , ' )' , cop , (exestring__vop '') , ')'
end.
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. If uop is a name, we just return its token number; if a value, we return the locale of the noun
((y = 3) # (utoken [^:uopisname uop),<tokensource),(vop #~ y > 1)
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
NB. The result is the DOL, with everything except the result of u
traverse =: 4 : 0
NB. We just pass this traversal on through
x traverse__vop y
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
NB. Since the current object cannot be a clone (it comes from u/ or the like),
NB. It is safe to assume that the top of the search path is the current modifier name
newtype =. ((valence=2) { 'monad';'dyad') ,~&.> oldtype =. {. oldpath =. 18!:2 loc =. coname''
((/: =&(<,'z')) ~. newtype , (18!:2 newtype) , }. oldpath) 18!:2 loc
''
)

NB. **** @ @: ****
NB. Save the name of the locale that handles @@: - we use it in &&: and also in fork
localeat_dissect_ =: 'dissectextendv' primlocale '@@:'

create =: 3 : 0
if. 0 = bwand/ verb , > (<0 2;0) { y do.
  failparse 'domain error: operands to ',((<1 1){::y),' must be verbs'
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
uop =: setvalence__uop resultissdt__vop
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB. Return the dispoperands from v
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__vop combineheights estheights__uop    NB. height of 0 is special flag, meaning 'stealth'
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
NB. If cop is a scalar, this is an expansion node: don't show the expansion parts
if. 0=#$cop do.
  defstring__vop y
else.
  enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
end.
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ' , cop , ' (' , (exestring__vop '') , ')))'
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Run v.  The result is dol;code where code is either a locale or an initialselection
loc =. 1 {:: doll =. x traverse__vop travops TRAVOPSKEEPLIGHT;1;(vopval selopinfovalid);<selopshapes
if. 1 = #loc do.
  NB. Normal case: no expansion, or v created a layout; show it
  x =. joinlayoutsl doll
  NB. execute u
   NB. If v is a stealth operand, rankhistory and highlighting that would have been applied on v will have disappeared,
   NB. so in that case we transfer them to u
  inheritu x traverse__uop travops ((dispstealthoperand__vop e. 1 2 5 6) {:: TRAVOPSSTARTHEAVY,&<TRAVOPSKEEPALL);0;(uopval vop);<<selresultshape__vop
else.
  NB. The expansion did not create a layout, either because it was processed as a forced selection or because it
  NB. hasn't been, or cannot be, activated.
  if. 1 = 0 {::loc do.
    NB. initial selection.  This must be an expansion node, so append SFOPEN
    initialselection =: <(2{loc),SFOPEN
    NB. v is an expansion that is not expanding, either because it can't or because it is waiting for a click to expand.
    NB. We now pretend that this node contained u rather than u@v.  We need to do this so that the highlights that
    NB. are attached to v don't get lost.  So we process u (which must be ]) as if it were the only thing
    inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
  else.
    NB. an expansion node that decided not to display.
    NB. Example is u/ y when there are 2 items, which converts to a dyad.  In this case we treat the
    NB. entire u@v as if it were v: we don't evaluate u, and just pass on the result of v without the extra info
    NB. (nothing to do here)
    (1 {:: loc) 1} doll
  end.
end.
)


NB. **** & &: ****
NB. Save the name of the locale that handles &&: - we use it in &. and &.:
localecompose_dissect_ =: 'dissectextendv' primlocale '&&:'

NB. When we find out the valence, we change this to be like @@: if monad, and proceed here only for the dyad

create =: 3 : 0
NB. If this is u&n or m&v, change the object type to bond, and switch over to that create routine
if. (,'&') -: (<1 1){::y do.
  if. noun bwand bwor/ > (<0 2;0) { y do.
    changeobjtypeto 'dissectvandnm'
    create y return.
  end.
elseif. 0 = bwand/ verb , > (<0 2;0) { y do.
  failparse 'domain error: operands to ',((<1 1){::y),' must be verbs'
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB. If this verb is in a gerund, it will not know how to proplocales if it has no valence.
NB. So we initialize valence to 0 to detect that case.
valence =: 0
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Look at the conjunction used, and use that to find the rank of this object (the derived verb)
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
NB. The monad is just like @@:, so we just point to that locale.  This also applies if u
NB. behaves like a monad
if. 1 = #y do.
  changeobjtypeto localeat
  cop =: '@' 0} cop
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

calcestheights =: 3 : 0
estheights =: , (estheights__vop0 ,: estheights__vop1) combineheights ,. estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: defstring__localeat f.

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
rankstg =. (cop -: ,'&') # '"', (')' ,~ '(]&' , defstring__vop0 3)
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
dol =. joinlayoutsl (0 1 # x) traverse__vop1 travops (TRAVOPSKEEPINLIGHT 0 1 2);1;(vopval 1 { selopinfovalid);selopshapes;1
dol =. dol ,~ joinlayoutsl (1 0 # x) traverse__vop0 travops (TRAVOPSKEEPINLIGHT 0 1 2);1;(vopval 0 { selopinfovalid);selopshapes;0
inheritu dol traverse__uop travops TRAVOPSSTARTHEAVY;0;(uopval vop0,vop1);<selresultshape__vop0 ,&< selresultshape__vop1
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
titlestring =: cop    NB. Use the & as the name in the rank stack
NB. Don't try to remember locale yet - we might clone
verboperandx =: * verb bwand (<2 0) {:: y   NB. Index of the verb operand
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. if dyad, switch to dyad routine
setvalence =: 3 : 0
if. 2 = #y do.
NB. Dyad case.  Emulate as u&n@]^:["_ 0 _
NB. We will return the locale of the overall verb
  rbkt =. 1 {:: COCREATOR createverb (']');($0)  NB. ']'
  uatr =. 1 {:: localeat 1 createmodifier _3 [\ verb;(coname'');($0);  conj;'@:';($0);  verb;rbkt;$0  NB. u&m@]
  NB.?lintonly uatr =. localeat
  lbkt =. 1 {:: COCREATOR createverb ('[');($0)  NB. '['
  uatrpwrl =. 1 {:: localepower 1 createmodifier _3 [\ verb;uatr;($0);  conj;'';($0);  verb;lbkt;$0  NB. u&m@]^:[  don't display ^:
  NB.?lintonly uatrpwrl =. localepower
  rank =. 1 {:: COCREATOR createnoun ('_ 0 _');'';($0)  NB. 0 1 0
  final =. 1 {:: localerank 1 createmodifier _3 [\ verb;uatrpwrl;($0);  conj;'';($0);  noun;rank;$0  NB. u&m@]^:["0 1 0  don't display "
  NB.?lintonly final =. localerank
  setvalence__final y
  '' insertoverride__uatrpwrl 'dissectvandnmdyad'
NB. Save the locale to use for display of this node in ^:
  disploc__uatrpwrl =: coname''
  '' insertoverride__uatr 'dissectvandnmdyad'
  disploc__uatr =: coname''
  final
else.
  valence =: #y
  'vl nl' =. <"0 verboperandx |. uop,vop
NB.?lintonly vl =. nl =. coname''
NB.?lintmsgsoff
  (verboperandx { 'uop';'vop') =: setvalence__vl verboperandx |. y , resultissdt__nl
NB.?lintmsgson
  resultissdt =: resultissdt__vl
  coname''
end.
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
vl =. verboperandx { uop,vop
NB.?lintonly vl =. coname''
estheights =: verboperandx { estheights__vl
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd '&' jd (defstring__vop 3)
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
dol =. verboperandx |. x , joinlayoutsl NOLAYOUTS traverse__nounop TRAVNOUN
NB. As we are moving from monad to dyad, add the appropriate column to the existing rankhistory to put previous monad ops on the correct side
NB. The rank value just added is now in the column the verb will be in, so its rank must be replicated on both sides.
NB. Remember, rankhistory holds yrank[,xrank], so if our verbop is index 0 (x) it goes to the RIGHT
rankhistory =: 0 1 3 2 {"1^:(-.verboperandx) (<_) (<_1 _1)} 4 {."1 rankhistory
inheritu dol traverse__verbop travops TRAVOPSKEEPALL;(verboperandx |. 0 _1);(vopval selopinfovalid);<verboperandx |. selopshapes,<selresultshape__nounop
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN ; 'The constant value is brought in as the ',(verboperandx { 'yx'),' argument.',LF
)

cocurrent 'dissectvandnmdyad'
coinsert 'dissectvandnm'

NB. This locale contains the overrides needed to display dyad & properly
NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
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
(>localecompose_dissect_)  primlocale '&.&.:'
NB. we emulate this with v^"_1@:u&[:]v.  The only thing we do here is defstring, so we can recover the original display form of the verb

create =: 3 : 0
if. 0 = bwand/ verb , > (<0 2;0) { y do.
  failparse 'domain error: operands to ',((<1 1){::y),' must be verbs'
end.
NB. Handle the special form we recognize: &.>
if. ('&.' -: (<1 1) {:: y) *. (verb -: (<2 0) {:: y) do.
  vloc =. (<2 1) {:: y
  NB.?lintonly vloc =. <'dissectverb'
  if. (,'>') -: execform__vloc do.
    changeobjtypeto 'dissecteach'
    create ({.y) ,: adv;''; ; (<1 2;2) { y return.
  end.
end.
create_dissectobj_ f. (<2 1) { y
NB. Register this object so we can clean up at end
'uop0 cop0 vop0' =: 'uop cop vop' =: 1 {"1 y  NB. Save under private names so we can return defstring
NB.?lintonly uop0 =: vop0 =: vop =: <'dissectverb' [ cop0 =: '&.:'
NB. Create an object to handle v^:_1@:u
NB. First, the verb v^:_1
iop =. 1 {:: COCREATOR createverb ((defstring__vop 2),'^:_1');($0)
NB. Now create an object for vi@:u
NB. Remove the . from &.&.: and create vi@:u
uop =: 1 {:: localeat 1 createmodifier (_3 [\ verb;iop;($0);conj;'@:';($0)) , 0 { y
NB.?lintonly uop =: <'dissectverb'

NB. Now change this locale to &&: and create i&v
NB. Replace the uop with the iop we just created, and the cop with the given conjunction, with '.' removed
NB. We leave the &. locale in the path, so that we can override
create__localecompose f. (uop;(<<<1){cop) (<0 1;1)} y
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
NB. We keep the original form
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop0 2) jd cop0 jd (defstring__vop0 3)
)


NB. **** u"n u"v m"n m"v****
NB. We also come through here for u&n m&v, with the primitive changed to '&'
localerank_dissect_ =: primlocale '"'

create =: 3 : 0
NB. Handle m"nv as a general verb
if. noun bwand (<0 0) {:: y do.
  changeobjtypeto localedefault
  create y
  return.
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
titlestring =: cop   NB. Since this may show up in the rank stack, have a title string
NB. Save the type of v
vtype =: (<2 0) {:: y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__uop
)

NB. return string form of operands, not including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd '"' jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , '"' , '(' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3) , y>0) # ;: 'uop tokensource vop'
)

NB. Nilad.  We know that the current node has no selection but has valid selopshapes.
NB. Result is 2 if it is OK to turn this into a rank-calculus probe, 0 if not.
rankcalculus =: 2:
NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. The result is the DOL, up through the result of u
NB. We do not create a cell; we just traverse u.  There is no visible indication of the rank operator, except in the
NB. frames
traverse =: 4 : 0
NB. for u"n, resolve n internally.  It will not display, but we need a result for getverbrank
if. vtype bwand noun do. NOLAYOUTS traverse__vop TRAVNOUN end.
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
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
res =. 0 2$a:
if. cop -: ,'"' do.
  if. '' -: frame do.
NB. obsolete  NB. This will have been described by its frame, but if there is a noframe message, replace
NB. obsolete  NB. it with something more tailored
NB. obsolete  if. # pos =. EXEGESISFRAMENOFRAME I.@:= > 0 {"1 y do.
    res =. ,: EXEGESISFRAMENUGATORY;'This has no effect because the argument rank',(valence{::'';' is';'s are'),' already small enough',LF
  end.
end.
res
)

NB. **** uL:n uS:n ****

primlocale 'L: S:'

create =: 3 : 0
if. 0 = verb bwand (<0 0) {:: y do.
  failparse 'domain error: left operand to ',((<1 1){::y),' must be a verb'
end.
if. 0 = noun bwand (<2 0) {:: y do.
  failparse 'domain error: right operand to ',((<1 1){::y),' must be a noun'
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

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__uop
)

NB. return string form of operands, not including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
NB. Always use '"', which is the ACTION we perform; cop is the label we use in the rank stack
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
traverse =: 4 : 0
NB. Resolve n internally.  It will not display, but we need a result for getverbrank
NOLAYOUTS traverse__vop TRAVNOUN
titlestring =: cop , ": ulevel =: ((#x) {:: '';(,0);1 2) { 3 $&.|. fillmask__vop frameselresult__vop selresult__vop
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
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
NB. This will have been described by its frame, but if there is a noframe message, replace
NB. it with something more tailored
if. levelunused do.
  res =. ,: EXEGESISFRAMENUGATORY;'This has no effect because the argument level',(valence{::'';' is';'s are'),' already small enough.',LF
else.
  if. *./ 0 = effulevels do.
    if. 1 = valence do.
      res =. ,: EXEGESISRANKSTACKEXPLAIN;'The verb is applied to the contents of each innermost box.',LF
    else.
      res =. ,: EXEGESISRANKSTACKEXPLAIN;'The verb is applied between the contents of innermost boxes of the arguments.',LF
    end.
  else.
    if. 1 = valence do.
      res =. ,: EXEGESISRANKSTACKEXPLAIN;'The verb is applied to contents that have boxing level ',(":effulevels),'.',LF
    else.
      res =. ,: EXEGESISRANKSTACKEXPLAIN;'The verb is applied between contents of x with boxing level ',(":{.effulevels),' and contents of y with boxing level',(":{:effulevels),'. Use highlighting to see the correspondence.',LF
    end.
  end.
  if. cop -: 'S:' do.
    res =. res ,EXEGESISRANKSTACKEXPLAIN;'The results from each application are assembled into a list of result-cells.',LF
  end.
end.
res
)


NB. x is the frame of the full expected result
NB. y is the number of results we actually got
NB. result is selector of the failing location, in natural order
getfailingisf =: 4 : 0
if. levelunused +. cop -: 'S:' do. x getfailingisf_dissectobj_ f. y
else. 1j1 #!.SFOPEN > resultseqmap pathfromindex y
end.
)


NB. x is the selection, y is the list of result indexes.  We return the selected index.
NB. We convert the selection into a path (by removing dropdowns); then pull the sequence number
NB. from the sequential map to be the index
selectusingisf =: 4 : 0
if. levelunused +. cop -: 'S:' do. x selectusingisf_dissectobj_ f. y
else. y {~ (SFOPEN -.~ >x) {:: resultseqmap
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
NB. Handle m/ as a general verb
if. noun bwand (<0 0) {:: y do.
  changeobjtypeto localedefault
  create y
  return.
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
titlestring =: (defstring__uop 2) , cop   NB. Default title to the value when u not used.  When u used, we take from u
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
NB. Insert a collection node after u, making ]@u.  If this is the dyad, it will be harmless
insertcollector coname''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a dyad
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop 2$y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
NB. Kludge.  We have converted u/ to ]@(u/) and marked ] as non-stealth so that it will produce
NB. a display for monad u/.  For dyad, the ] is unwanted.  We should do something to prevent
NB. @ from forcing out the v; but for the nonce, just make the ] stealth
if. valence = 2 do.
NB. Dyad u/  unset what we set for the monad
  stealthoperand__iop =: 5   NB. Hide the ]
  titlestring =: cop   NB. This is the rank-stack version of /
end.
separatevalences''
coname''
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. The monadic valence:
startmonad 'dissectrighttoleft dissectirregularops dissectselectshape dissectdisplaytwo'

calcestheights =: 3 : 0
NB. Since u is always a dyad, combine heights and add 1 for the expansion node
estheights =: , >./ estheights__uop combineheights ,1
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. Use unadorned verb on empty operand to get neutral.  Save every result of verb execution.  Also save overall result.
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')/`(' , (defstring__uop 2), '/)@.(0=#))'
)
NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
NB. If there is no selection, we simply roll up the display of everything under the name u/ and fall through,
NB. so that the value will be displayed in the collector.  We do not traverse u in this case.

NB. If there is a selection, we handle the cases of 0, 1, and 2 items specially:
NB. 0 or 1 items just pass through to display u/ at the collector without traversing u.  Exactly 2 items
NB. does not need a selector; we traverse u and pass its result to the collector.

NB. If there are more than 2 items, if there is no selector we process u/ as above, but if there is a
NB. selector we create a new node to handle the / selection.  This node will display all the results of
NB. u, allowing selection of one.  We will pass no layouts to the collector, which will display its selected result

NB. Create display type:
traversedowncalcselect y
if. frame -: ,1 do.
  formatcode =: 0   NB. forced selection
NB. We have a selection of a u/ of exactly 2 items.  Convert to dyad
  
  
  x =. x , createreference x   NB. Create reference for dyad u
NB. Remove the line we added for u/ .  selopinfo has been adjusted
NB. Replace the rankhistory line (which contains the title of the entire u/) with an indication that / was elided
NB. Don't inherit u into u/, because u/ (the collector) has a frame, while u is a single result.  But DO extend u's
NB. locale chain to u/, so that the highlights calculated in u/ are displayed
  NB. u is always executed as a dyad.  If this node selected through its forced selection, physreq will have been expanded
  NB. to dyad shape.  But if not, we take the precaution here of forcing it to be a dyad so that its highlights can carry on.
  ures =. x traverse__uop ((<'(/ on 2 items)')&((<_1 0)}))`'' travops TRAVOPSKEEPALL;0 _2;(vopval selopinfovalid);<selopshapes
  extendinheritchain 1 {:: ures   NB. add u/ to display of u.  This locale cannot be a flag (those occur only in u@v)
NB. Indicate that u@v (the final collector) should suppress its u, and use this result as the sole result
  (<0;<1 { ures) 1} ures  NB. 0;locale for suppressed expansion
  
elseif. (1 < nitems =. {.frame) *. (sellevel < #selections) do.
NB. We have a selector, and at least 2 possible selections.  Display the selector, and traverse u
  formatcode =: 1   NB. selector is displayed

NB. Mark the ] node with the 'Final' tag to avoid confusion
  titlestring__iop =: 'Final '&;^:(0=L.) titlestring__iop
  if. nitems (| = <:@[) {. ('';0) {:: isfensureselection isftorank2 sellevel { selections do. x =. x , createreference x  NB. Guaranteed selector is valid
  else. x =. x , createselfreference 1.4   NB. loop from right of box to y argument
  end.

NB. Replace the last line of rankhistory with simple '/'
NB. Traverse u to display it and its descendants, and create a display node for pre-u.  Then u will be inherited into the display
NB. of the expansion created here, which is part of the result.
  inheritu x traverse__uop ((<cop)&((<_1 0)}))`'' travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
NB. The collector will display the result from the above
elseif. do.
NB. If there is no selector yet, the u/ node vanishes, with the display being provided entirely by the collector.
NB. In this case we (1) want no new display node here; (2) need to add an additional selection level, a negative
NB. value to be used for the initial selection if there is a click on the collector.  We do both functions by placing the numeric
NB. selection value in place of the locale name to display
NB. Remove the adornment of the titlestring in the ] node - if any
  formatcode =: 2  NB. waiting to engage selector
  titlestring__iop =: 1&{::^:(0<L.) titlestring__iop
  x ;< 1;(coname'');,0   NB. 1;locale;initial selection
end.
)

exegesisrankstack =: 3 : 0
if. frame -: ,1 do.
  NB. This is the special '/ on 2 items' box.
  ,: EXEGESISRANKSTACKEXPLAIN;'The monadic verb ',LF,(defstring 0),CR,'is applied to an array with 2 items. It is displayed as a dyad, with both x and y arguments coming from the input to the monad.',LF,LF,'This block is a starting point for the dyad.',LF
else.
  select. y
  case. 1 0 do.  NB. All computation in this block
     ,: EXEGESISRANKSTACKEXPLAIN;'This block shows the intermediate results of the verb:',LF,(defstring 0),CR
  case. 0 0 do.  NB. Computation ends in another block
     ,: EXEGESISRANKSTACKEXPLAIN;'This block starts the computation of an intermediate result of the verb:',LF,(defstring 0),CR
  case. do.
     0 2$a:  NB. leave it for the overall text
  end.
end.
)

exegesisrankoverall =: 4 : 0
if. x do.
  t =. 'This block displays all the intermediate results in the execution of the verb:',LF,(defstring 0),CR,'and shows the last verb in the computation. The calculation of the result started in the block(s) marked with ''/'' .',LF
else.
  t =. 'This block calculates and displays all the intermediate results in the execution of the verb:',LF,(defstring 0),CR
end.
t =. t , 'The results are displayed as a list of boxes, where the contents of a box contains one intermediate result. '
t =. t , 'The order of results matches the order of items of y, which is the reverse of the executed order. In other words, the first result in the list is the final result of the verb. '
t =. t , 'Select any result to see how it was calculated. Selection of a result will open the selected box (indicated by the ''>'' in the selection line) and allow you to continue selections inside the box. '
,: EXEGESISRANKOVERALLEXPLAIN;t,LF,LF
)

NB. *** traversal support ***
NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);(selopshapes for next level - cells of this verb);resultlevel;arglevel
calcdispframe =: 4 : 0
NB. frame is 1 less than the number of items - if there is more than 1 item.  empty otherwise
NB. Result boxing level will be 1 if the frame is longer than 1
((nframe > 1) { ($0);1) _2} x calcdispframe_dissectobj_ f. , < (#~ >&0) nframe =. <: '' ($,) {.@($^:(0<L.))@> x
)

NB. Nilad.  Result is the selection for this node:  type;selection where type=
NB. 0=no selection, 1=normal selection, 2=forced selection, 3=pick-only
getselection =: 3 : 0
if. frame -: ,1 do. 2 ,&< a:   NB. forced selection if 2 items
elseif. selectable *. (sellevel < #selections) do. 1 ;<  sellevel { selections
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

NB. The dyadic valence:
startdyad ''

calcestheights =: 3 : 0
NB. like u"
estheights =: estheights__uop
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
)


NB. **** ^: ****
localepower_dissect_ =: primlocale '^:'

create =: 3 : 0
if. 0 = verb bwand (<0 0) {:: y do.
  failparse 'domain error: left operand to ',((<1 1){::y),' must be a verb'
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
visnoun =: * noun bwand (<2 0) {:: y  NB. Remember whether it's u^:n or u^:v
NB. We implement u^:v with two locales, because there are two levels of
NB. selection: first the selection from v, is that result is not an atom; then
NB. the expansion node and selection of powers.  We create a new
NB. node that looks like a modifier, i. e. u*^:v where * is the expansion.
NB. The display of the result of u^:v (as an inheritable u) comes from
NB. this locale; the expansion (as a v) comes from *.
NB. All we need to pass in is the locale of u and the title string (which may be different from '^:'
uop =: 'dissectpowerexpansion' 1 createmodifier (<0 1;1) {:: y
NB. Save the display form of the conjunction and the locale of v.
NB. If the display form of the conjunction is not '^:', we are acting as a surrogate for another function like dyad u&m
'cop vop' =: (<1 2;1) { y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
if. visnoun do.
  resultissdt =: resultissdt__uop
else.
  vop =: setvalence__vop y
  NB.?lintonly vop =: <'dissectverb'
  resultissdt =: resultissdt__uop *. resultissdt__vop
end.
NB. Return the dispoperands from v
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
NB. Add 1 unit to height of u to account for this node; 1/2 unit to v since it starts lower.
NB. Overall height is the larger of the two.  If v is a noun, it will have just 1 height regardless
NB. of valence; we turn that into an atom so it will replicate
estheights =: (estheights__uop combineheights ,1) >. {.^:visnoun estheights__vop combineheights ,0.5  NB. height of 0 is special flag, meaning 'stealth'
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ^: (' , (exestring__vop '') , '))^:(1:`(',(logstring__uop 0),'@])))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. Traversal up and down the tree.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
NB. traverse v, either as a noun or as a verb using the current selector
if. visnoun do.
  vdol =. NOLAYOUTS traverse__vop TRAVNOUN
NB. If noun operand failed, pull the plug and display only that result
  if. errorcode__vop > EOK do. vdol return. end.  NB. If the noun failed, this node must have failed too
else.
NB. u^:v: Create references for the input(s) and assign the correct one to u and v.
NB. x is a list of layouts (each layout a list); create a table of layouts, each (rank-2) row having orig,reference
NB. Start with original to u, ref to v; reverse if u height < v height.  The reference will go to the lower
NB. Add 1/2 unit to u since v starts lower
  xx =. (</ ((estheights__uop combineheights ,0.5) ,: estheights__vop)) |."0 2 (,: createreference)"1 x
  vdol =. (1 {"2 xx) traverse__vop y
  x =. 0 {"2 xx   NB. Pass the other operands into u^: or this node
end.
NB. Create the layout for v
vlayo =. joinlayoutsl vdol

NB. keep track of how this node is formatted, for exegesis
formatcode =: 0   NB. early error
NB. We need vval for calculating the selframe of u; but it may not be valid, in case vop failed.
NB. We have fixed calcdispframe so that it doesn't look at vval if vop failed, so we just need to
NB. get vval defined when it is valid
if. errorcode__vop -.@e. ENOOPS,ENOSEL do. vval =: fillmask__vop frameselresult__vop selresult__vop end.
NB. Perform selections for u - needed for display whether v ran or not
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. In case we are formatting this node (the usual case), save the input DOLs to it
resdol =. x ,&< coname''
NB. If v didn't run, there is really nothing we can do about u; just display it.  If v failed because it didn't select, there
NB. is hope for a later traversal
NB.?lintonly vval =: 0
if. errorcode__vop > EOK do.
  if. cop -.@-: '^:' do.   NB. m&v or u&n
    resdol =. ((estheights__uop > 0) # x) ,&< coname''
    physreqandhighlights__inheritroot =: (estheights__uop > 0) # physreqandhighlights__inheritroot
  end.
  'displayhandlesin displayhandleout displaylevrank' =: ((#0{::resdol) { ($0);(,0);_0.3 0.3),1;< (<defstring 0) (<_1 0)} rankhistory
NB. v ran. Get the actual result of v.  We know v collected successfully
elseif. isgerund vval  do.
  formatcode =: 1  NB. gerund
NB. If v produced a gerund, give up and display the value of this node
NB. Replace the end of the rank stack with the whole mess
  'displayhandlesin displayhandleout displaylevrank' =: ((#0{::resdol) { ($0);(,0);_0.3 0.3),1;< (<defstring 0) (<_1 0)} rankhistory
elseif. do.
NB. No gerund.
  
NB. Inspecting ALL the results from v, if every selection is either <0 or <_1, we will have no need for u^:
  if. logvalues__vop *./@:e. 0;_1 do.
NB. One-line request: create request, with appropriate labeling, depending on the possible values of v
NB. But if this node is not displaying as '^:', it must be coming from dyad u&m; format the overall verb and
NB. remove the x operand
    if. cop -: '^:' do.
      formatcode =: 2   NB. ^:0 or ^:_1
      labelstg =. (defstring__uop 2) , (#. 0 _1 e. ; logvalues__vop) {:: '()';'_1';'0';'(0 or _1)'
    else.
      formatcode =: 3   NB. dyad &
      labelstg =. defstring 0
      resdol =. ((estheights__uop > 0) # x) ,&< coname''
      physreqandhighlights__inheritroot =: (estheights__uop > 0) # physreqandhighlights__inheritroot
    end.
    'displayhandlesin displayhandleout displaylevrank' =: ((#0{::resdol) { ($0);(,0);_0.3 0.3),1;< (<labelstg) (<_1 0)} rankhistory
  else.
NB. Otherwise, we will run u^:.  It will possibly create a display for u, or possibly an expansion node.
    
NB. Classify the v results and choose the type of display.  The result of this classification is passed to
NB. u^: (and also used here and in our tooltip)
    
NB. If the selector is invalid, and the totality of v has at least one positive value, traverse u to get a
NB. skeletal display, and make that the (v-type) result, with no expansion node.
    
NB. If the selector is invalid and v has no positive values, there is no need to traverse u.  Open twice in case of boxed v
    traverseu =. +./ , 0&(+./@:<)@>@> logvalues__vop
    
NB. If the v value for the current selection does not require an expansion (<_1, <0, or <1), we will traverse to get
NB. a v-type display of u.  If the current selection is <0 or <_1, u^: will invalidate the selector to get a skeletal display of u.
    'skeletalu noexpansion' =. 2 3 > (<@,"0 (_1 0 1)) i. < ~. , vval
    
NB. If all the v results are <1, we never need a selector and can simply expand the result as a u-type
    vis1 =. logvalues__vop *./@:= <1
    
NB. Figure out what v value has been selected by the current selection.  If selection has not been performed,
NB. the expansion will not expand, so there must be a unique v value.  Pass that into the traversal: we will
NB. display only the powers whose sign matches the selection, and we will display only up to the selection.
    if. ($0) -: $vval do. selectedpower =. (- *)@>^:(1 = L.) vval
    elseif. sellevel < #selections do.
      sel1 =. {. > isfensureselection isftorank2 sellevel { selections  NB. first level of selection, boxed
      selectedpower =. (- *)@>^:(1 = L.) sel1 { vval
    elseif. do. selectedpower =. 0
    end.
    
NB. Create the initial selection to use when this result is clicked.  Since the initialselection is for an expansion node, append SFOPEN to it.
NB. we select according to which type (forward or inverse) will be displayed in the expansion.
NB. Create the initialselection only if we are ready to use it, i. e. if we have selected down to a single value to expand
    if. *./selopinfovalid do.
      initialselection =: <(0:^:(=&_) |selectedpower);SFOPEN
    end.

NB. Run the expansion node for u^: (as a v-type node); pass in the analysis of v.  The result is either
NB.  nothing: no expansion, no u.  display will be just u@:v
NB.  u: u expanded, but there is no expansion node.  If vis1, we treat this result as the u-type result; otherwise
NB.   we treat it as a v-type result and realize it it, then connect it to u^:v (but if the selector is 0, we
NB.   connect it to the y input instead
NB. expansion: realize it as a v-type result
    'expdol code' =. x traverse__uop traverseu;skeletalu;noexpansion;vis1;selectedpower;visnoun;< travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
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
        if. (2 = #x) do.
          resdol =. ((estheights__uop > 0) # x) ,&< coname''
          physreqandhighlights__inheritroot =: (estheights__uop > 0) # physreqandhighlights__inheritroot
        end.
        'displayhandlesin displayhandleout displaylevrank' =: ((#0{::resdol){'';(,0);_0.3 0.3),1;< (<(defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
      case. 2 do.
  NB. skeletalu: u is there for show only; the actual value comes from y.  Create a reference to y and add it as the first input.
  NB. Make the handle for the u result _1, meaning 'no wire'
        resdol =. joinlayoutsl expdol
        resdol =. resdol , createreference _1 { x
        'displayhandlesin displayhandleout displaylevrank' =: (0 _1);1;< (<'Final ' , (defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
  NB. Whenever we instantiate an expansion node, we alter the number of inputs to the u^:v node.  This invalidates the highlights from
  NB. u^:v.  But there shouldn't be any highlights anyway!  They come from the expansion.  So we just turn them off here
        physreqandhighlights__inheritroot =: NOPHYSREQ
        resdol =. resdol ,&< coname''
      case. 3 do.
  NB. Expansion node executed with u attached, or just u by itself
        resdol =. joinlayoutsl expdol
  NB. expansion node or u where the current selector is <1, use it as a v-type
        'displayhandlesin displayhandleout displaylevrank' =: (,0);1;< (<'Final ' , (defstring__uop 2) , (cop -: '^:') # visnoun {'un') (<_1 0)} rankhistory
  NB. Whenever we instantiate an expansion node, we alter the number of inputs to the u^:v node.  This invalidates the highlights from
  NB. u^:v.  But there shouldn't be any highlights anyway!  They come from the expansion.  So we just turn them off here
        physreqandhighlights__inheritroot =: NOPHYSREQ
        resdol =. resdol ,&< coname''
    end.
    
  end.
end.
NB. resdol will be our u-type result.
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand oerand).  This box contains
NB. a table of dol;highlights
prevright =. 2 {:: resdol , < 0 2$a:  NB. default to empty
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
(2 {. resdol) , (<prevright , vlayo ,&< vselect)
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
if. #inputselopshapes do.
  t =. 'The final result of the verb:',LF,(defstring 0),CR
  if. #$vval do.
    astg =. LF,'The v operand of ^: produced ' , (exegesisindefinite exegesisfmtcell ($vval);''),'.',LF
    astg =. astg , 'The verb is applied to the argument',((valence=2)#'s'),' for each atom of that array, and the result-cells are assembled into the final result.',LF
  else.
    astg =. ''
  end.

  if. *./ selopinfovalid do.
    NB. Explanatory string if v produces an array
    select. formatcode
    case. 0;3;4 do.  NB. Early error; ;dyad m&v/u&n, does not go into rank stack; traverseu: vis1, this node not displayed
    case. 1 do.  NB. Gerund
      t =. t , LF , 'dissect doesn''t analyze the gerund form of ^:, sorry.',LF
    case. 2 do.  NB. ^:0 or ^:_1
      t =. t , LF,'Because ' , ((*#$vval){::'v';'each atom of v') , 'is ' , ((#. 0 _1 e. ,vval) {:: '';'_1';'0';'0 or _1') , ', details of calculating the result are not shown.',LF
      t =. t , astg
    case. 5 do.  NB. traverseu: noexpansion
      if. #$vval do. astg =. astg , 'You have selected a result cell.',LF end.
      t =. t , astg , 'Select the result again to see details of its computation.',LF
    case. 6 do.  NB. traverseu: skeletalu (unselected)
      t =. t , astg , LF,'The selected power does not calculate u at all.',LF
    case.  do.   NB. traverseu, expansion or u created
      t =. t , astg , LF,'The block feeding into this one shows the powers that were calculated. To remove the detail, click in the result of this block.',LF
    end.
  else.
    t =. t , astg , LF,'Select a result-cell to see which v produced it.'
  end.
else.
  t =. 'This will display the final result of the verb:',LF,(defstring 0),CR
end.
,: EXEGESISRANKSTACKEXPLAIN;t
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
NB. y is the number of results we actually got
NB. result is index list of the failing location, in natural order
NB. This turns ticket order to selection order
getfailingisf =: 4 : 0
NB. Calculating the failing index for ^: is a chore.  We have to figure out what failed - the forward
NB. or the inverse - and then get an index to whichever failed.  That will set up the selector to find the
NB. failure in the expansion.  To decide what failed, we have to nose around in the expansion data
NB. First, we have to find the matching indexes in the expansion
selx =. ; findselection__uop > selector
NB. Calculate the expected frame/inversect: the number of results expected (including the 0 'result') and
NB. the number of negative results expected.  _ means 'don't know'.  If there are both positive.  This is needed
NB. so we can detect an error during sniff
fi =. (>./ , [: - <./) 0 , flatvval =. , (- *)@>^:(1 = L.) vval
NB. Count the number of forward and inverse executions
fix =. -/\. (# , +/) selx { logvaluesd__uop
NB. We ignore the first forward execution if its result is the same as the second, and not the same as all the rest;
NB.  or if there is a mix of forward and backward execs
throwaway =. (1 1 -: * fi) +. (-:/@:(2&{.) *. (-.@-: 1&|.)) (-.logvaluesd__uop) #&(selx&{) logvalues__uop
NB. correct the number of executions: forward includes the 0 value, and also counts a throwaway execution sometimes
fix =. fix - 0 ,~ 1 + 1 1 -: * throwaway
NB. Decide which direction failed - if any.  Infinities can only come up when we are going in one direction
edir =. fi i.&1@:> fix
NB. If neither direction failed, the error must have happened during framing.  Ignore that for the nonce
NB. Find the member of vval that is the largest in the direction of error.  That will be the one we select
x #: (i. >./) (edir { 1 _1 1) * flatvval
)

NB. y is the current selection (a: if forced)
NB. We never generate a highlight.  This level deals only with selections.
calcphysandhighlights =: 3 : 0
valence # < 2&{. EMPTYPRH
)




NB. **** expansion node for ^: ****
cocurrent 'dissectpowerexpansion'
coinsert 'dissectdisplaytwo dissectselectshape dissectobj'

NB. y is locale of u;titlestring to display in rank stack
create =: 3 : 0
create_dissectobj_ f. '';$0   NB. no string, no tokens
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the locale of u
'uop titlestring' =: y
NB.?lintonly uop =: <'dissectverb'
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
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

calcestheights =: 3 : 0
estheights =: estheights__uop combineheights ,1     NB. add 1 for expansion node
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) , titlestring
)

NB. return string form of operands, including instrumentation within u but not within inverse of u
exestring =: 3 : 0
initloggingtable 1
NB. If u has an unused operand, we should put @] or @[ after the inverse, because Roger can't handle x u@[^:_1 y but he can handle x u@[^:_1@[ y
monadstring =. ('@]';'@[';'') {::~ estheights__uop i.&0@:>: 0
auditstg '(' , (verblogstring '') , '(' , (logstring 0) , '@:' , (exestring__uop '') , ') :. ( ' , (logstring 1), '@:(' , (logstring__uop'') , ')@:(' , (defstring__uop 2) , '^:_1',monadstring,')))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
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
traverse =: 4 : 0
'traverseu skeletalu noexpansion vis1 selectedpower visnoun travy' =: y   NB. Unpack the added operands, info about v
assert. (6 0$0) -: $@".@> ;: 'traverseu skeletalu noexpansion vis1 selectedpower visnoun'
QP^:DEBTRAVDOWN'traverseu skeletalu noexpansion vis1 selectedpower visnoun '
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
    udol =. x traverse__uop ((<'^:1')&((<_1 0)})^:vis1)`'' travops TRAVOPSKEEPALL;1;((-.skeletalu) vopval selopinfovalid);<selopshapes
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
NB. If the selection is 2 or higher, or _2 or lower, we need a selfreference.  If 2 or higher, the selfreference
NB. will go to u; if _2 or lower, it will go to u^: .  But we will need to make sure that y is still
NB. displayed, just with the wire removed
    sel1 =. {. > isfensureselection isftorank2 sellevel { selections  NB. first level of selection, boxed
    if. floatingy =. 1 < | > sel1 do.
      ydol =. {: x
      x =. (createselfreference 1.4) (,_1)} x
    end.
NB. If this v contains a nonpositive value, that means the y value might get through without going through u.
NB. In that case, we need a wire from y to the selector.  If also the selector is 1, both the expansion and u will
NB. need to connect to y, so we will need a reference.  We will connect the reference to u, because the connection
NB. to the expansion is secure, but the connect to u might be deleted by a stealthop.
    
NB. Turning this into the arguments to the expansion and u:
    if. traverseu do.
NB. The arguments to u are:
NB.   x, if any
NB.   selfref (if 1 < |sel), or y
NB. Run u and inherit it into this node
      NB. If the selected power is negative, u will contain an inverse (or identity).  Since Roger always runs the inverse at
      NB. infinite rank, pass in a flag to u indicating that fact.
      ures =. inheritu x traverse__uop (, applyintree 0)^:(selectedpower<0) travops TRAVOPSKEEPALL;1;((-.skeletalu) vopval selopinfovalid);<selopshapes
    else.
NB. No u.  Create the expansion, with input coming from y or self
      'displayhandlesin displayhandleout displaylevrank' =: ((#x) { '';(,0);_0.3 0.3),1;< (<(defstring 0) ,( visnoun {'un') , (selectedpower<0) # ' (inv)') (<_1 0)} rankhistory
      ures =. x ,&< coname''
    end.
NB. To make sure that the display of y doesn't disappear, we add it as another input, with a hidden wire, if it was replaced by a reference
    if. floatingy do.
NB.?lintonly ydol =. 0 4$a:
      ures =. (<(0 {:: ures) , ydol) 0} ures
      displayhandlesin =: displayhandlesin , _1
NB. Add a compensating no-highlight, but only if there is a highlight.  If there is none, it means we didn't try to match the highlight to the DOL
      if. #physreqandhighlights__inheritroot do. physreqandhighlights__inheritroot =: physreqandhighlights__inheritroot , <EMPTYPRH end.
    end.
    ures ; 3   NB. Display expansion
  else.  NB. no expansion.
    0 1
  end.
end.
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
select. y
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block shows the intermediate results of the verb:',LF,(defstring 0),CR
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block starts the computation of an intermediate result of the verb:',LF,(defstring 0),CR
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)
exegesisrankoverall =: 4 : 0
if. x do.
  t =. 'This block selects from the powers of',LF,(defstring 0),CR,'and displays the selected result.'
else.
  t =. 'This block selects from the powers of',LF,(defstring 0),CR,'and displays the last verb of the computation. Computation of the power starts in the block(s) labeled ^: .',LF
end.
if. selectedpower = 0 do.
  t =. t , 'In this case, the power being computed is 0, so there is no computation to view.',LF
else.
  t =. t , LF,'The boxes in the display shows the results of succeeding applications of the verb. The first box shows power 0 (the original y argument), '
  t =. t , 'the second shows power ',(":*selectedpower),', and so on. '
  t =. t , 'Select any result to see how it was calculated. Selection of a result will open the selected box (indicated by the ''>'' in the selection line) and allow you to continue selections inside the box. ',LF
end.
,: EXEGESISRANKOVERALLEXPLAIN;t
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
NB. The first value is the input, the second is the suspect throwaway, others are actual executions.
NB. The number of valid values will become the frame.
NB. If we know from v what should be produced, use that.  Otherwise figure it out by looking at the result
if. noexpansion do.
  frm =. ''
elseif. selectedpower -.@e. _ __ do.
  frm =. , >: | selectedpower  NB. Add 1 to include 0
elseif. a: ~: selector do.
NB. Frame unknown, use whatever we actually did, of the correct sign
  selx =. calcdispselx ; findselection > selector
  frm =. , (selectedpower<0) ([ + +/@:=) selx { logvaluesd  NB. Add 1 on inverse to include the 0 power
elseif. do.
NB. rank-calculus probe, return empty frame since we can't do rank-calculus
  frm =. $0
end.
NB. Level 1 (output only) if we have an expansion
((*#frm) { ($0);1) _2} x calcdispframe_dissectobj_ f. (-valence) {. <frm
)

NB. y is the indexes that matched; result is the indexes to use, in execution order.
NB. We discard the second forward value (the first result of executing the forward) if it
NB. exists and there is an execution of the inverse.
NB. If the expansion node is to be omitted, the only use of this node is no provide
NB. an operand for u; that will be the unmodified y; so make that the only input
calcdispselx =: 3 : 0
keepmask =: (selectedpower<0) = y { logvaluesd
keepmask =: 1 (0)} keepmask
NB. If both valences were executed, the first execution (the second value) is bogus
NB. We ignore the first forward execution if its result is the same as the second, and not the same as all the rest;
NB.  or if there is a mix of forward and backward execs
throwaway =. ( 0 1 *./@:e. }. y { logvaluesd) +. (-:/@:(2&{.) *. (-.@-: 1&|.)) }. (-.logvaluesd) #&(y&{) logvalues
if. throwaway do. keepmask =: 0 (1)} keepmask end.
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

destroy =: 3 : 0
destroy_dissectobj_ f. ''
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
defstring =: 3 : 0
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

calcestheights =: 3 : 0
if. 0 = valence do.
  estheights =: ,0  NB. no input to this node
else.
  NB. If this is a recognized gerund, give the combination the height of the tallest component
  estheights =: estheights__uop >. estheights__vop
end.
)

proplocales =: 3 : 0
((y=3) +. (0~:valence)) # <^:(0=L.)@".@>^:(0 <: y) (1 , (y=3), 1) # ;: 'uop tokensource vop'
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: 4 : 0
assert. 0 = #x [ 'Noun must have no layouts'
traversedowncalcselect y  NB. To set globals, including selresult
'displayhandlesin displayhandleout displaylevrank nounhasdetail' =: ($0);1;NORANKHISTNOUN;0
x ,&< coname''  NB. Return the empty DOLs
)
NB. obsolete 
NB. obsolete 
NB. obsolete NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. obsolete NB. return no U dols
NB. obsolete traverse =: 4 : 0
NB. obsolete assert. 0 = #x
NB. obsolete traversedowncalcselect 4 {. y  NB. Just to set error globals
NB. obsolete selresultshape =: $conjex
NB. obsolete selresult =: ,<conjex
NB. obsolete 'displayhandlesin displayhandleout displaylevrank nounhasdetail' =: ($0);1;NORANKHISTNOUN;0
NB. obsolete x ,&< coname''  NB. Return the empty DOLs
NB. obsolete )
NB. obsolete 
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

destroy =: 3 : 0
destroy_dissectobj_ f. ''
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

calcestheights =: 3 : 0
estheights =: , >./^:(valence=1) |. estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. If u takes more arguments than we were given, we have to create a reference for the input.
NB. We assign the original to the estimated SHORTER side of u, which will be the longer side of u~
if. (#x) < 2 do.
  x =. |.^:(>/estheights__uop) (, createreference) x
end.
inheritu (|. x) traverse__uop travops (TRAVOPSKEEPINALL 0 1 _1 2);_2 0;(vopval selopinfovalid);< _1 0 {^:(*@#@]) selopshapes
)

exegesisrankstack =: 3 : 0
NB. We delete the frame lines
if. valence = 1 do.
  ,: EXEGESISRANKSTACKEXPLAIN;'The argument is replicated and used as both the x and y arguments',LF
else.
  ,: EXEGESISRANKSTACKEXPLAIN;'The x and y arguments are interchanged',LF
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

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
defstring__uop y
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) ,<'uop'
)

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

calcestheights =: 3 : 0
NB. No display here, so just use uop's height
estheights =: estheights__uop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (verblogstring '') , (logstring '') , '@(' , (exestring__uop '') , '))'
)

traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. If the partition is dyadic, it will need the VALUE of x.  We will extract that
NB. now.  We need the value of x after applying any selection given here.
if. 1 < #x do.
  NB. Dyadic valence
  xop =. < (0 3;0 0) {:: x  NB. locale of x operand
  NB.?lintonly xop =. <'dissectobj'
  if. errorcode__xop > EOK do.
    partitionx =: ''
  else.
    partitionx =: fillmask__xop frameselresult__xop selresult__xop
    NB. If there is a selection at this level, apply it to find the correct x value
    if. selectable *. sellevel < #selections do.
      partitionx =: (sellevel{selections) { partitionx
    end.
  end.
  NB.?lintsaveglobals
end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
)


NB. ********** the partitioning modifiers themselves *****************

NB. These routines are shared by all partition processing
cocurrent 'dissectpartition'
coinsert 'dissectobj'  NB. for lint

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

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
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
NB. Since u is always a monad, use its height for the y height and add 1 for the expansion node.
NB. x, if given, feeds into u\ directly, and so has a height of 1
estheights =: (-valence) {.!.1 estheights__uop combineheights ,1
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
cocurrent 'dissectpartitionadverb'
coinsert 'dissectpartition'  NB. for lint

create =: 3 : 0
NB. Handle noun left op as a general verb
if. noun bwand (<0 0) {:: y do.
  changeobjtypeto localedefault
  create y
  return.
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ cop =: ''
titlestring =: cop
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
NB. Create a selector, which will be the first entry point for this sequence
verb;(xop =: 'dissectpartitionselector' 1 createmodifier coname'');tokensource
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3)) # ;: 'uop tokensource'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. Use unadorned verb on empty operand to get neutral.  Save every result of verb execution.  Also save overall result.
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , cop , ')'
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
yop =: < (_1 3;0 0) {:: x  NB. locale of y operand, needed for u;.1
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Create a display for u (as a v-type).  It may or may not have detail, depending on whether anything was selected here
NB. u is always a monad, so we pass in only the last argument
NB. To assist tooltipping, we insert a heavy rank line for this locale at the end of the rankstack.
NB. This will not show, but it will mark the node that performs the computation of the selected node (if there is one)
rankhistory =: rankhistory , 0 ; (coname'')
udol =. joinlayoutsl (_1 {. x) traverse__uop travops TRAVOPSKEEPALL;(,_2);(vopval selopinfovalid);selopshapes;_1

NB. Create a display for this node, as if it were a u-type verb.  This display will be inherited into the selector.
NB. We initialize the rank stack, and it is that that will give the label for this display.
NB. The /. node always displays the entire partitioning verb, with 'Final' prepended when the /. is selectable
NB. and there has been a selection.
NB. obsolete rankstack =. ,: (('Final ' #~ selectable *. sellevel < #selections) , defstring 0) ; <"0 sellevel , |. vranks
rankstack =. ,: (('Final ' #~ selectable *. sellevel < #selections) , defstring 0) ; (coname'') , <"0  |. vranks
'displayhandlesin displayhandleout displaylevrank' =: (valence {:: ($0);(,0);_0.3 0.3);1;<rankstack
NB. The highlights for x (if any) are preserved, but the ones for y are reset, since there is no selection from u/. into u
physreqandhighlights =: (<EMPTYPRH) _1} physreqandhighlights
NB. The result of u becomes the last argument to the display of this node, along with the original x operand if
NB. there was one.
((_1 }. x) ,  udol) ,&< coname''
)

exegesisrankstack =: 3 : 0
select. y
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKPARTITIONSTART;'This block calculates the selected partition of the verb:',LF,(defstring 0),CR
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKPARTITIONSTART;'This block starts the calculation of the selected partition of the verb:',LF,(defstring 0),CR
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)

exegesisrankoverall =: 4 : 0
NB. This block is pointed to 3 times: u/. in the selector, 0 in the expansion, and /. at the start of computation
if. 0 = y do.
  NB. This block has the expansion result, and possibly the whole thing
  if. (0 ~: #inputselopshapes) *. (0 ~: #selector) do.
    if. selectable *. sellevel < #selections do.
      'ctext stext' =. (x{::'calculates';'shows the result of');''
    else.
      'ctext stext' =. (x{::'will calculate';'will show the result of');'after a selection is made.',LF
    end.
  else.
    'ctext stext' =. (x{::'will calculate';'will show the result of');'after a single partition has been selected.',LF
  end.
  if. x do.
    t =. 'This block ',ctext,' the selected partition of',LF,(defstring 0),CR,stext
  else.
    t =. 'This block ',ctext,' the selected partition of',LF,(defstring 0),CR,'including the last verb of the computation. Computation of the partition starts in the block(s) labeled ',cop,' .',LF
  end.
elseif. y -.@-: cop do.
  NB. u/. 
  t =. LF,'This block shows the result of the verb:',LF,(defstring 0),CR,'which ',(exegesispartitiondesc''),'.',LF
  if. #selframe do.
    t =. t , LF,'The results of execution on the partitions are assembled into ',(exegesisindefinite exegesisfmtcell selframe;''),' of result-cells.',LF
  end.
  if. (0 ~: #inputselopshapes) *. (0 ~: #selector) do.
    if. selectable *. sellevel < #selections do.
      t =. t , LF,'To see the calculation of a single partition, select its result',LF
    else.
      t =. t , LF ,'The computation of the selected partition starts in the block(s) labeled ',cop,' and ends in the block feeding into this one.',LF
    end.
  end.
elseif. do.
  NB. must be /. to start a computation.  That displays only as rankstack, never as overall
  t =. ''
end.
,: EXEGESISRANKOVERALLEXPLAIN;t
)

NB. conjunction partitions ;.
cocurrent 'dissectpartitionconjunction'
coinsert 'dissectpartition'  NB. for lint

create =: 3 : 0
NB. Handle noun left op as a general verb
if. noun bwand (<0 0) {:: y do.
  changeobjtypeto localedefault
  create y
  return.
elseif. 0 = noun bwand (<2 0) {:: y do.
  failparse 'domain error: right operand to ',((<1 1){::y),' must be a noun'
end.
create_dissectobj_ f. (<1 2) {  y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locale of the verb, and string form of the adv
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. titlestring is set during traversal, when we know n
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(xop =: 'dissectpartitionselector' 1 createmodifier coname'');tokensource
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop jd defstring__vop 3
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) (1 , (y=3) , 1) # ;: 'uop tokensource vop'
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. Use unadorned verb on empty operand to get neutral.  Save every result of verb execution.  Also save overall result.
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop '') , ')' , cop , '(' , (exestring__vop '') , '))'
)

NB. For traversal, we extract the string value of n and append that to the conjunction name;
NB. then we complete the traversal using the adverb form
traverse =: 4 : 0
NB. Traverse n.  It's a noun, and so must produce a single result
NOLAYOUTS traverse__vop TRAVNOUN
titlestring =: cop,":partitionn =: >{.logvalues__vop
x traverse_dissectpartitionadverb_ f. y
)

exegesisrankstack =: exegesisrankstack_dissectpartitionadverb_ f.
exegesisrankoverall =: exegesisrankoverall_dissectpartitionadverb_ f.

NB. ************** The individual partitioning modifiers *****************
cocurrent 'dissectobj'

NB. *** \ ***

'dissectpartitionadverb dissectpartition' primlocale '\'

NB. The monadic valence u\ y:
localebslashmonad_dissect_ =: startmonad ''

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
itemctiny =: '' ($,) ($^:(0<L.))@> {: x  NB. needed for \.
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame
ny =. 1 ($,) ($^:(0<L.))@> {: x
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
NB. Keep the rank of selopshapes, but use the size from the selection
NB. If selopshapes is a map, we have to get the right part
if. 1 = L. selopshapes do.
  (>: {. > y) (0} ,)&.> selopshapes
else.
  (>: {. > y) {.&.> selopshapes
end.
)
calcunselectedshapes =: calcunselectedshapes_dissectirregularops_ f.

exegesispartitiondesc =: 3 : 0
'operates on prefixes of y of increasing length' 
)

localebslashdyad_dissect_ =: startdyad ''
NB. The dyad x u\ y:

NB. x is selopshapes: box for each operand, containing $L:0 of the operand
NB. y is natural frame(s) of the executed verb
NB. result is (selframe);(frame);(frames of value to display);resultlevel;arglevel
calcdispframe =: 4 : 0
NB.?lintonly xop =: <'dissectpartitionselector'
itemctiny =: '' ($,) ($^:(0<L.))@> {: x
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.  The number of partitions is:
NB. if x is nonnegative, (the number of items of y + 1) - x, but never negative
NB. if x is negative, (the number of items of y) % |x, rounded up
if. partitionx__xop >: 0 do. ny =. , 0 >. (>: itemctiny) - partitionx__xop
else. ny =. , >. itemctiny % -partitionx__xop
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
  a: , ps (0} ,)&.> {: selopshapes
else.
  a: , (({.>y) + i. ps)&{&.> {: selopshapes
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
  (itemctiny - {. > y) (0} ,)&.> selopshapes
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
  a: , (itemctiny - ps) (0} ,)&.> {: selopshapes
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

'dissectirregularops dissectpartitionadverb dissectpartition' primlocale '/.'

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
NB. the selections are the (ysize) disgonal elements starting at (0,sel) and going down and to the left.
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
ny =. , # ~. partitionx__xop
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
NB. obsolete   a: , (#selitems) (0} ,)&.> {: selopshapes
  a: , (#selitems) (, }.)&.> {: selopshapes  NB. Works on atoms too
else.
  a: , selitems&{&.> {: selopshapes
end.
)

exegesispartitiondesc =: 3 : 0
'operates on partitions of y corresponding to identical items of x'
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
select. partitionn
case. 0 do.
  canonx =: 0&,:^:(2>#@$) partitionx__xop
  usedyshape =: ({:@$ canonx) {. shapeofy
  ny =. $0
case. 1;_1;2;_2 do.
  NB. canonize x: convert to list of boxes; then within each box, convert atom to full list, or empty to list of one partition
  canonx =: shapeofy ((<.&# {. [) ((#^:(''-:$@])) [^:(0=#@])~ ({. 1:))&.> (<.&# {. ])) <^:(0=L.) partitionx__xop
  usedyshape =: ({:@$ canonx) {. shapeofy
  NB. Get shape of result partitions
  ny =. +/@> canonx
case. 3;_3 do.
  NB. convert list to table; replace values bigger than axis by signed length of axis
  canonx =: 1 ,:^:(2>#@$@]) partitionx__xop
  NB. Trailing axes of ;.3 are included in the partitioning, so extend them by assuming a 'take everything'.
  NB. We can't just omit them from canonx and leave them in the frame, because then the selection would be
  NB. longer than canonx
  canonx =: canonx ,. (0 >. (#shapeofy) - ({:$canonx)) #"0 (0 _)
  usedyshape =: ({:@$ canonx) {. shapeofy   NB. Now always same as shapeofy
  canonx =: ({. canonx) ,: (usedyshape<|)`(,:  usedyshape * *)} {: canonx
  NB. Calculate the number of start positions: ceiling of (length of axis/movement vector) for 3,
  NB. or ceiling of (length-size/movement vector) for _3; but 1 if movement vector is 0
  ny =. (0 ~: 0{canonx)} 1 ,: >. (|0{canonx) %~ (| 1 { canonx) -~^:(partitionn=_3) usedyshape
  ny =. (#shapeofy) {.!.1 ny
case. do.
  NB.?lintonly canonx =: usedyshape =: $0
  NB.?lintonly ny =. 0
end.
NB. The pseudoframe (# of partitions) is already in the stored data, since we logged every
NB. call to u.  So make that the frame.
ny ; ny ; (($0);ny) ; a: , a:
NB.?lintsaveglobals
)

NB. y is raw selections, result is selections to use
NB. There is a quirk in u;.3, probably in the special code, such that the computation may be restarted
NB. depending on the result shape, and the changing shapes of the individual results
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
  hlit =: (>y) {&> (<;.partitionn i.@#)&.> canonx
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

NB. **** m@.vn ****

'dissectextendv' primlocale '@.'

create =: 3 : 0
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectobj'
NB. Switch to general verb if v is a noun, or if u is not a noun, or if u is not understood as a gerund
if. (noun bwand (<2 0) {:: y) +. 0 = #ulocales =: querygerund__uop '' do.
  if. 0 = noun bwand (<0 0) {:: y do.
    failparse 'domain error: verb@.v'
  end.
  changeobjtypeto localedefault
  create y
  return.
end.
NB. It is m@.v with m a gerund.  ulocales is the list of locales in the gerund
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
titlestring =: cop  NB. make this show up on rank stack
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Ignore ]@ etc.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop *. resultissdt__vop
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
NB. Add 1 unit to height of u to account for this node; 1/2 unit to v since it starts lower.
NB. Overall height is the larger of the two.
estheights =: (estheights__uop combineheights ,1) >. estheights__vop combineheights ,0.5  NB. height of 0 is special flag, meaning 'stealth'
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Create references for the input(s) and assign the correct one to u and v.
NB. x is a list of layouts (each layout a list); create a table of layouts, each (rank-2) row having orig,reference
NB. Start with original to u, ref to v; reverse if u height < v height.  The reference will go to the lower
NB. Add 1/2 unit to u since v starts lower
xx =. (</ ((estheights__uop combineheights ,0.5) ,: estheights__vop)) |."0 2 (,: createreference)"1 x
vlayo =. joinlayoutsl (1 {"2 xx) traverse__vop seloperands =. travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
x =. 0 {"2 xx   NB. Pass the other operands into u^: or this node
NB. Create the layout for v

NB. If v didn't run, there is really nothing we can do about u; just display the final result.  If v failed because it didn't select, there
NB. is hope for a later traversal.  If there is no selection, we don't know which u to display, so just display then final result then too
NB.?lintonly vval =: 0
if. (errorcode__vop > EOK) +. -. *./ selopinfovalid do.
  vselect =. <EMPTYPRH   NB. Indic no v highlight
elseif. do.
  NB. v ran, and there is only one choice for the selection.  We will be able to display u
  NB. Get the actual result of v.  We know v collected successfully
  NB. See which u-verb was selected.  This uses user data & so must be checked
  try.
    selectedop =. ulocales {~ fillmask__vop frameselresult__vop selresult__vop
  catch.
    EINVALIDOP earlyerror x return.
  end.
  NB. The selection must be a scalar.
  if. ($0) -.@-: $selectedop do. 
    (EINVALIDOP;'non-atomic v') earlyerror x return.
  end.
  NB.?lintonly selectedop =. <'dissectverb'
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
  rankhistory =: _1 {. rankhistory
end.
NB. Label the display.  Note that x may have changed number of operands, but we have the right one here
'displayhandlesin displayhandleout displaylevrank' =: ((#x) { ($0);(,0);_0.3 0.3),1;< (<defstring 0) (<_1 0)} rankhistory
NB. Bring v in as a third input to the result, wherever it came from.
NB. The v result (coming in from the right) is placed in a third box of
NB. the result (present only when there is a right-hand operand).  This box contains
NB. a table of dol;highlights
x ; (coname '') ; < ,: vlayo ,&< vselect
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
select. y
case. 1 0 do.  NB. All computation in this block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block shows the calculation of the selected gerund.',LF
case. 0 0 do.  NB. Computation ends in another block
   ,: EXEGESISRANKSTACKEXPLAIN;'This block starts the calculation of the selected gerund.',LF
case. do.
   0 2$a:  NB. leave it for the overall text
end.
)

exegesisrankoverall =: 4 : 0
if. cop -: ,'@.' do.
  t =. 'This block starts the calculation of the selected gerund.',LF
else. 
  t =. 'This block displays the result of executing',LF,(defstring 0),CR,'The selection of executed verb comes in from the right.',LF
  if. (errorcode__vop <: EOK) do.
    if. *./ selopinfovalid do.
      t =. LF,'The calculation for the selected result is shown ending in the block feeding into this one.',LF,'Computation starts in the block(s) labeled @. .'
    else.
      t =. LF,'Select a result-cell to see how it was calculated.',LF
    end.
  end.
end.
,: EXEGESISRANKOVERALLEXPLAIN;t
)

'dissectverb' primlocale '$:'

NB. y is (string form of the verb);tokens it came from
create =: 3 : 0
r =. create_dissectverb_ f. y
NB. Changes for $:
resultissdt =: 0   NB. $: is NOT an sdt
NB. Indicate that this verb-phrase contains a recursion
recursionencountered__COCREATOR =: 1
r
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectverb_ f. ''
)

setvalence =: 3 : 0
if. executingvalence__COCREATOR ~: #y do.
  failparse 'dissect restriction: recursion must have the same valence as the original execution'
end.
setvalence_dissectverb_ f. y
)

NB. We save a couple of things and then traverse as a normal verb
traverse =: 4 : 0
recursionpoint =: {. executingmonaddyad__COCREATOR
NB.?lintonly recursionpoint =: <'dissectmonad'
x traverse_dissectverb_ f. y
NB.?lintsaveglobals
)

exegesisrankstack =: 3 : 0
,: EXEGESISRANKSTACKEXPLAIN;'This block shows the result of a recursion. Select the result to see results of all recursion levels and examine any of them.',LF
)

getselection =: 3 : 0
NB. Save the ticket for this result.  If we get a selection at this node, we will send this value back to the
NB. recursion point to make the selection
currentticket =: logticket {~ {. y
getselection_dissectverb_ f. y
NB.?lintsaveglobals
)

NB. Any selection in the recursion result is passed to the recursion point.
NB. We have saved the locale of the recursion point, and 
selectionoverride =: 3 : 0
selectrecursion__uop__recursionpoint currentticket
1  NB. Indicate that we have overridden, abort selection
)

NB. **** default ****
localedefault_dissect_ =: primlocale ''
NB. Remove the last element in the search, to make this the 'search failed' locale
dissectprimindex_dissect_ =: }: dissectprimindex_dissect_

NB. This is the default object to handle unknown entities

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
  stg =. (defstring__uop 2) jd '(' jd cop jd ')' jd (defstring__vop 3)
end.
NB. We will treat this as a generic verb, except for the overrides we have in this locale
changeobjtypeto 'dissectverb'
insertoverride localedefault
NB. Pass the token number of the modifier in as the verb token number.  That will go into tokensource
create_dissectverb_ f. stg;(<1 2){y
)

NB. display height is always just 1

proplocales =: 3 : 0
(y=3) # (<tokensource) 1} >&.> ucvlocs
)


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
  if. 3 = stealthoperand__uop do.  NB. [:
    changeobjtypeto localeat
NB. Move the token values too
    create ((1 { y) ,: conj;'@:';(<0 2){y) 0 1} y
    return.
  end.
end.
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop *. resultissdt__cop
verb;(coname'');''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
vop =: setvalence__vop y
NB.?lintonly vop =: <'dissectverb'
if. vvv do.
  uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
  cop =: setvalence__cop resultissdt__uop , resultissdt__vop
NB. We have to ensure that any stealthoperand produces a _1 height for all it contributes to,
NB. so we don't assign a label to a stealthoperand
else.
  cop =: setvalence__cop resultissdt__uop , resultissdt__vop
end.
NB.?lintonly cop =: <'dissectverb'
resultissdt =: resultissdt__cop
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
NB. Join c to u and v to produce the height(s) for each operand; choose the larger to be our total
NB. height per operand.  But if nvv, use SCALAR _1 for height, which will be replicated to match the
NB. valence, and will signify that the n does not use any operand
estheights =: >./ ((_1 [^:(-. vvv) estheights__uop) ,: estheights__vop) combineheights ,. estheights__cop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen (defstring__uop 1) jd ' ' , (defstring__cop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. vvv do. uops =. 0,valence__uop,0 else. uops =. 0 1 0 end.
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop uops) , ' ' , (exestring__cop'') , ' ' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. Include u if we want non-nouns (including clone), or if it's a verb
<^:(0=L.)@".@>^:(0 <: y) (((y~:0)+.vvv),1 1) # ;: 'uop cop vop'
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. Make a reference for each operand (if vvv).  Assign the original to the higher estheight.  Since
NB. nondisplayed operands have estheight of 0, if dispoperands culls a value, it will always be
NB. the reference that is culled.
if. vvv do.
NB. Start with original to u, ref to v; reverse if u height < v height
  xx =. (</ (estheights__uop ,: estheights__vop) combineheights ,. estheights__cop) |."0 2 (,: createreference)"1 x
  dolv =. joinlayoutsl (1 {"2 xx) traverse__vop travops TRAVOPSKEEPLIGHT;1;(vopval selopinfovalid);< selopshapes
  dolu =. joinlayoutsl (0 {"2 xx) traverse__uop travops TRAVOPSKEEPLIGHT;1;(vopval selopinfovalid);< selopshapes
else.
NB. nvv.  Traverse n as a noun
  dolv =. joinlayoutsl x traverse__vop travops TRAVOPSKEEPLIGHT;1;(vopval selopinfovalid);< selopshapes
  dolu =. joinlayoutsl NOLAYOUTS traverse__uop TRAVNOUN
end.
inheritu (dolu,dolv) traverse__cop travops TRAVOPSSTARTHEAVY;0;(uopval uop,vop);< selresultshape__uop ,&< selresultshape__vop
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
verb;(coname'');''
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
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

calcestheights =: 3 : 0
NB. Because of our layout restrictions, we put v on top of u, with no way for v to
NB. fit down over the right operand of u.  Thus, the monad height is the
NB. (larger of heights of u) + (monad height of v but not if right-height of u<0)
if. valence = 1 do.
  estheights =: estheights__vop +^:(0<:{.@[) (, >./ estheights__uop)
else.
NB. The dyad heights are (left-height of u,v joined to right-height of u)
  estheights =: (,   estheights__vop combineheights ,)/ estheights__uop
end.
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen (defstring__uop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop '') , ' ' , (exestring__vop '') , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
<^:(0=L.)@".@>^:(0 <: y) ;: 'uop vop'
)

NB. Traversal up and down the tree.
NB.
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
NB. If this is a monad, make a reference for y.  Assign the original to v UNLESS v does not contribute to u
NB. (we don't use the comparative size of u because we have to place v above u)
if. 1 = #x do. x =. |.^:(0 > {: estheights__uop) (,~ createreference) x end.
dol =. joinlayoutsl (0 1 # x) traverse__vop travops (TRAVOPSKEEPINLIGHT 0 1 _1);1;(vopval _1 { selopinfovalid);selopshapes;_1
NB. Use selop0 for x, and selresult for y - but only if selop0 exists, and no travdownuops error
NB. replace the rank with the left rank of the hook, alone.  Preserve previous highlighting from u to the left operand
inheritu (dol ,~ 0 {  x) traverse__uop travops (TRAVOPSSTARTINHEAVY 0 1 2);0 _1;((vopval 0 { selopinfovalid) >. (uopval vop));<({. selopshapes),< selresultshape__vop
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
NB. Set the title string to the actual characters the user used
titlestring =: ; (<:tokensource) { ;: usersentence__COCREATOR   NB. decr tokensource to accoutn for MARK added
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
uop =: setvalence__uop y
NB.?lintonly uop =: <'dissectverb'
resultissdt =: resultissdt__uop
coname''
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
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
traverse =: 4 : 0
traversedowncalcselect y
if. errorcode e. EEARLYERROR do. earlyerror x return. end.
inheritu x traverse__uop travops TRAVOPSKEEPALL;1;(vopval selopinfovalid);<selopshapes
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
,: EXEGESISRANKSTACKEXPLAIN,t
)

NB. overrides for calcselect
calcdispframe =: 4 : 0
(1;valence # 1) _2 _1} x calcdispframe_dissectobj_ f. y
)

NB. x is the frame of the full expected result
NB. y is the number of results we actually got
NB. result is selector of the failing location, in natural order
getfailingisf =: 4 : 0
SFOPEN ;~ x getfailingisf_dissectobj_ f. y
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
minwh winsize;cc helptext edith readonly;
pas 0 0;
)

NB. Called in locale of the help form desired
helpshow =: 3 : 0
NB.?lintonly hwndp =: helpsize =: helptitle =: helptext =: ''
if. #hwndp do.
  wd 'psel ' , hwndp
  wdsetfocus 'helptext'
else.
  wd HELP rplc 'winsize';helpsize
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
helpsize =: IFQT {:: '200 200';'300 400'
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
Selection line - if you have selected a portion of a result for analysis, its index list is shown beneath the shape.
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
helpsize =: IFQT {:: '400 600';'600 800'
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


NB. 0!:1 ; <@(LF ,~ '(i. 0 0) [ dissectinstanceforregression_dissect_ 4 : ''destroy__x 0 [ dissect_dissectisi_paint__x 0''^:(0=#@]) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_base_
NB. wd@('psel dissect;pclose'"_)"0 i. 100
runtests_base_ =: 0 : 0
2 dissect '2+''a'''
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
2 dissect '2 3 +&:+: 4 5 6'   NB. must show sgreement error
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
2 dissect '0 (1 2 3 , ])"0 $0'
2 dissect '0 ([: 1 2 3"0 $)"0 $0'
2 dissect '0 (+ - ]) '''''
2 dissect '0 (1 2 3 - *)"0 $0'
2 dissect '0 (1 2 3 - *)"0 (0)'
2 dissect '0 +@* '''''
2 dissect '0 (+@* - *) '''''
2 dissect '0 (+@* *) '''''
2 dissect '0 (+ *) '''''
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
2 dissect '''`a b c'' =. +`-`%'
2 dissect '''`a b c'' =. +&+`-`%'
2 dissect 'r + s [ (''r s t'') =. 0 1 2 [ a =. ''r'';''s'';''t'''
2 dissect '-&.> i. 3'
2 dissect '-&.:> i. 3'
2 dissect '+/ 1 2 3 4 5'
2 dissect '(* -)/@> z' [ z =. <@i."0 (3 4 5 6)
2 dissect '+/@> z' [ z =. <@i."0 (3 4 5 6)
2 dissect '+/"1 z' [ z =. i. 4 3
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
2 dissect '3 ''a'''
2 dissect '   '
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
2 dissect '(3 4 ,: 2 3) <;.3 i. 10 10'
2 dissect '(3 4 ,: 2 3) <;._3 i. 10 10'
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
2 dissect '(3 4 ,: 2 3) +:;.3 i. 10 10'
2 dissect '(3 4 ,: 2 3) +:;._3 i. 10 10'
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
2 dissect '>:`($:@.>)@.(0<L.) 1 2 3;<4 5;6 7'
2 dissect '>:`($:&.>)@.(0<L.) 1 2 3;<4 5;6 7'
2 dissect '>:`(0&$:&.>)@.(0<L.) 1 2 3;<4 5;6 7'
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
2 dissect '2&:+ 5'
2 dissect '+&:5 (2)'
2 dissect '2@+ 5'
2 dissect '+@2 (5)'
2 dissect '2@:+ 5'
2 dissect '+@:2 (5)'
2 dissect '+&.5 (2)'
2 dissect '2&.+ 5'
2 dissect '+&.:5 (2)'
2 dissect '2&.:+ 5'
2 dissect '2^:+: 6'
2 dissect '2^:_1 (3)'
2 dissect '+;.- 2 3'
2 dissect '+:@:+:@+:@+: +: 4 +&+: 5 6 7'
2 dissect '+:^:2"0 i. 5'
2 dissect 'i.^:] ] _2 _1 0 1'
2 dissect '+:^:] ] _2 _1 0 1'
2 dissect '3 </. ''a'''
2 dissect '(<1 23 4) (+&.> 2&(>./\)&.>)~ (<2 3)'
2 dissect '<@i./."2 (0.5) (<1 1 1)} i. 2 3 4'
dissect 2 3 $ 3;(<'base');'qqq+3'  ; 'qqq';0;<,'6'
dissect 2 3 $ 3;(<'base');'qqq+3'  ; 'qqq';0;<6
2 dissect 'a =. /'
2 dissect 'c =. ;.'
2 dissect 't =. 2 2 $ 5'
2 dissect '+(a =. /) 3 4 5'
2 dissect '0$a:'
2 dissect '5 (6 $~ 5 + ]) '''''
2 dissect '5 (6 $~ 5 + ])"0 '''''
2 dissect '5 (6 $~ 5 + ])"0 (7)'
2 dissect '5 (6 $~ 5 [ ]) '''''
)
testsandbox_base_ =: 3 : 0
vn =. 1 2 3
vn_base_ =: 'abc'
va =. &.>
vc =. &
vv =. 3 : ('y =. y + 1';'y =. y + 2')
sentence =. 'vv y vc + va vn'
arg =. ,: 1;(coname'');sentence
arg =. arg , 'vn';0;vn
arg =. arg , 'va';1;5!:5 <'va'
arg =. arg , 'vc';2;5!:5 <'vc'
arg =. arg , 'vv';3;5!:5 <'vv'
dissect arg
)
0 : 0  NB. Testcases that fail
)

0 : 0
0!:1 ; <@(LF ,~ '3 : ''(i. 0 0) [ destroy__y 0 [ dissect_dissectisi_paint__y 0''^:(''''-:$) ' , [: enparen_dissect_ 'NB.'&taketo);._2 runtests_base_
testsandbox_base_ 1
)
   