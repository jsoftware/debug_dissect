NB. Copyright (c) Henry H. Rich, 2012-2014.  All rights reserved.

NB. Clear definitions of old locales and create anew.  This will remove hangover derfinitions. These can be small since the hold mostly verb-names
((cocreate ([ coerase))"0~   2 1 {.~ #) ;: 'dissect dissectobj dissectmonad dissectdyad dissectnoun dissectverb dissectassign dissectvandnm dissectfork dissecthook'

NB. INPROGRESS:
CLEANUP_dissect_ =: 1   NB. set to 0 for debugging to allow postmortem
DEBPARSE_dissect_ =: 0   NB. set for parser printout
DEBTRAVDOWN_dissect_ =: 0   NB. set for travdown printout
DEBVERB_dissect_ =: 0   NB. set for travdown printout
DEBLAYOUT_dissect_ =: 0   NB. display grid details
DEBGRAF_dissect_ =: 0   NB. display all drawn graphics
DEBOBJ_dissect_ =: 0  NB. display drawn-object details
DEBDOL_dissect_ =: 0  NB. display drawing locales
DEBPICK_dissect_ =: 0  NB. display pick progress
QP_dissect_   =: qprintf
SM_dissect_   =: smoutput
NB. TODO:
NB. dissect '+&.>/ z' [ z =. 1;'a';3;4
NB.  error because the same locale seems to get placed twice, owing to error
NB. dissect '(1&+@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0' 
NB.  make color-coordination better.  Rank is in wrong place - needs to go to v
NB. put a fence around route to save time?  Take hull of points, then a Manhattan standoff distance
NB. routing: penalize overlap, including overlap of straight lines.  Also, think about forcing all nets of, say, 3 dests to use router.  Have height limit on direct routes.  Use router on all nets that have a routed portion.
NB. better pn on grid
NB. If grid exists, raise it on click on result
NB. handle clicking on verb-name part to select tree
NB. create pickrects for displayed sentence, and handle clicks there
NB. plan: save preferences; debug globals; display sentence; J8
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

dissect_z_ =: [: ([: display_dissect_ <@". :: (''"_)&.>)`]@.(2=3!:0)  [: parse_dissect_ (0&# : [ (([ ; 18!:5@(''"_) ; ]) , z458095869_dissectnopath_@(''"_)) ])@getsentence_dissect_

NB. The locale dissectnopath is used to find local names.  Its path is empty.  The locale contains only one name, z458095869
cocurrent 'dissectnopath'
copath ''
NB. The verb z458095869 returns a table of defined local names.  It is a table of (name;(type from 4!:0);(numeric ranks, invertible if verb/value if noun, '' if other))
z458095869 =: (([ ,. <"0@] ,. (".@[`[`[`(rankinv_dissect_@[))@.]&.>) (4!:0)) @ ((<'z458095869') -.~ 4!:1@i.@4:)

require 'strings gl2'
require '~addons/format/printf/printf.ijs'
cocurrent 'dissect'
coinsert 'jgl2'

getsentence =: (' ' takeafter LF (i:~ }. ]) [: }:^:(LF={:) (13!:12))^:(0=#)

NB. Maximum line length that we will try to display in a grid cell

MAXSENTENCEWIDTH =: 0.5  NB. max frac of screenwidth that we allow for sentence display

ifdefined =: 0 <: [: 4!:0 <

NB. ********************** from here on is devoted to parsing J sentences ***************
NB.
NB. Parsing stuff, copied from trace.ijs
NB. sdt means self-defining term: a number or string rather than a name or a result
(x)=: 2^i.#x=. ;:'noun verb adv conj lpar rpar asgn name mark sdt'
any =: _1
avn =: adv + verb + noun
cavn=: conj + adv + verb + noun
edge=: mark + asgn + lpar
invvalences=: invmonad+invdyad

x=. ,: (edge,       verb,       noun, any      ); 0 1 1 0; '0 Monad'
x=. x, ((edge+avn), verb,       verb, noun     ); 0 0 1 1; '1 Monad'
x=. x, ((edge+avn), noun,       verb, noun     ); 0 1 1 1; '2 Dyad'
x=. x, ((edge+avn), (verb+noun),adv,  any      ); 0 1 1 0; '3 Adverb'
x=. x, ((edge+avn), (verb+noun),conj, verb+noun); 0 1 1 1; '4 Conj'
x=. x, ((edge+avn), (verb+noun),verb, verb     ); 0 1 1 1; '5 Trident'
x=. x, (edge,       cavn,       cavn, any      ); 0 1 1 0; '6 Bident'
x=. x, ((name+noun),asgn,       cavn, any      ); 1 1 1 0; '7 Is'
x=. x, (lpar,       cavn,       rpar, any      ); 1 1 1 0; '8 Paren'

PTpatterns=: >0{"1 x  NB. parse table - patterns
PTsubj    =: >1{"1 x  NB. "subject to" masks
PTactions =:  2{"1 x  NB. actions

bwand =: 17 b.    NB. bitwise and
bwor =: 23 b.    NB. bitwise or
bwxor =: 22 b.   NB. bitwise XOR
bwlsl =: 33 b.  NB. logical left shift
enclosing =: ([: > [: {. [) , ] , [: > [: {: [

prespace=: ,~ e.&'.:'@{. $ ' '"_
                      NB. preface a space to a word beginning with . or :

isname=: ({: e. '.:'"_) < {. e. (a.{~,(i.26)+/65 97)"_
                      NB. 1 iff a string y from the result of ;: is is a name

class=: 3 : 0         NB. the class of the word represented by string y
 if. y-:mark do. mark return. end.
 if. isname y do. name return. end.
 if. 10>i=. (;:'=: =. ( ) m n u v x y')i.<y do.
  i{asgn,asgn,lpar,rpar,6#name return.
 end.
 (4!:0 <'x' [ ".'x=. ',y){noun,adv,conj,verb
)
NB. *** end of copied stuff

NB. possible starting variables, in name;type;value form
startvbls =: 'xymunv' (,@[ ; '' ;~ ])"0 noun,noun,noun,(verb+sideeff),noun,(verb+sideeff)

enparen =: '( ' , ,&') '

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
dissectinstance =: '' conew 'dissect'   NB. global because must persist over return to user environment
errormessage =: 'unknown error during parsing'
try.
  parsemain__dissectinstance y
catch.
  smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput 13!:12''

  NB.  Error encountered during parse.  We indicate this with an unboxed sentence.  We
  NB. destroy the locale, since we can't continue
  destroy__dissectinstance^:CLEANUP''
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

dissectionlist =: 0$a:  NB. List of open dissect instances.  The last one is the most recent
NB. Initialization
create =: 3 : 0
dissectionlist_dissect_ =: dissectionlist_dissect_ , coname''
objtable =: 0$a:   NB. list of parse objects
ticket =: 0   NB. sequential log number
winhwnd =: ''  NB. Init to no window
NB. Save the initial environment: locale size, boxing chars
Jenvirons =: (9!:38 '')
NB. obsolete NB. Set boxing chars that we can see in the grid
NB. obsolete 9!:7 '+++++++++|-'
NB. Use lightweight locales - we use less than 100 entries usually
9!:39 (1) 1} 9!:38 ''
NB. For nodes that do not have a parallel path (i. e. all but forks and &), this locale will
NB. be the predecessor locale, and will not signal an error
errorcode =: 0
NB.?lintsaveglobals
)

NB. Add new object to the list of objects
NB. We make the newset object first to solve a subtle problem: certain locales (like assignments) coinsert an
NB. existing locale to resolve undefined names.  If a locale in the path is destroyed, it will make names
NB. like codestroy unresolvable.  So, we order the locales here so be destroy in the opposite order of creation.
newobj =: 3 : 0
objtable =: y , objtable
)

NB. Utility to create rank,invertible flags for a verbname
NB. y is name of a verb, visible in current context
NB. result is (ranks), 1 if invertible
rankinv =: ".@(,&' b. 0') , 1:@". :: 0: @(,&' b. _1')

NB. anything beginning with one of these words and ending with . is a control word
controlwords =: ;: 'assert break continue for goto label if do else elseif end return select case fcase throw try catch catchd catcht while whilst'
NB. obsolete NB. tokenize each line and remove comments - except for lint directives
NB. obsolete lines =. }:^:(1 0 -: ('NB.';'NB.?lint') ([ -: #@[ {. ])&> {:)@;:&.> lines
NB. for each line, find control words; then recollect sentences between control words; then
NB. append the line number of the line. run all the blocks together.  This deletes empty sentences, too
NB. For multiple blocks on the same line (caused by control words), give them fractional parts to
NB. distinguish them
NB. Verb, returning 1 if a word is a control word
iscw =: ('NB.' -: 3 {. >) +. e.&controlwords@(('_'&taketo)@}:&.>) *. ('.'={:)@>  NB. verb, applied to boxed word.  Any remaining comment must be a lint directive


NB. Routine to parse and execute a block
NB. inparms is the environment:
NB.  table of local variables (name;type from 4!:0;ranks if verb)
NB. the first line of the table is special: it's options;locale;sentence to execute
NB. giving the locale in which the verb will execute
NB. Result is the string form of the instrumented sentence, ready to execute
NB. As a side effect, many objects are created indicating the parse structure
NB. In paticular, resultroot is the boxed locale of the sentence result.
NB. If there is an error, resultroot is empty
parsemain =: 3 : 0   NB. runs in object locale
defnames =. }. y  NB. table of names
'options loc sentence' =. {. y

NB. Break the input into words.  If there is an error, fail.  Discard any comment
NB. obsolete try. queue =. }:^:('NB.' -: 3 {. >@{:) ;: sentence catch. queue =. 0$a: return. end.
try. queue =. ;: sentence catch. queue =. 0$a: end.
NB. Get mask of words to discard: discard leading control words, or anything starting with a control word after a non-control
dischdtl =. (*./\ ,: [: +./\ 0 , (2) </\ ]) iscw queue
NB. Get the sentence in the form the user gave it, by deleting the nonblank characters corresponding
NB. to the discarded words.
ndiscardshdtl =. dischdtl (#@(-.&' ')@;@#)"1 queue
usersentence =: ' ' (-@(i.&0@:= |.) }. i.&0@:= }. ]) sentence ((}.~ {.) }.~ -@{:@]) ndiscardshdtl i.~"0 1 (0) ,. (+/\ ,: +/\@|.) ' ' ~: sentence
NB. keep the nondiscards in the tokenized version
queue =. (+:/ dischdtl) # queue

NB. If the sentence is empty, abort
if. 0 = #queue do. failparse 'no sentence' return. end.

NB. Append an end-of-queue mark to the sentence, and initialize the stack.
NB. The stack is type;value;tokennums where value is the locale of the object producing the result, for verb and noun;
NB. or the string form, for a modifier.  Tokennums are the input token numbers that contribute to the item
queue =. mark ; queue
stack =. 4 2 $ mark;''

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
    nobj =. conew 'dissectmonad'
    stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)

  case. 2 do.  NB. dyad
    NB. Create a dyad execution block for the operands, and put that on the stack
    nobj =. conew 'dissectdyad'
    stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)

  case. 3;4 do.  NB. adverb/conjunction execution
    stack =. ((subj i. 1){.stack),(execmod exeblock),((>:subj i: 1)}. stack)

  case. 5 do.  NB. Trident N V V or V V V
    NB. Create a trident execution block for the operands, and put that on the stack
    nobj =. conew 'dissectfork'
    stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)

  case. 6 do.   NB. bident  A A, C VN, VN C, V V
    NB.?lintonly exetypes =. 0 0 [ exeblock =. '';'';''
    if. bwand/ verb , exetypes do.  NB. V V
      nobj =. conew 'dissecthook'
      stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)
    elseif. (bwand/ adv , exetypes) +. (conj = +/ conj bwand exetypes) do. NB. A A, C VN, NV C
      NB. This becomes an adverb type.  The value is the exeblock, which will be executed later
      stack =. ((subj i. 1){.stack),(adv;exeblock; ; 2 {"1 exeblock),((>:subj i: 1)}. stack)
    elseif. do.
      failparse 'Invalid bident'
      return.
    end.
  case. 7 do.  NB. assignment
    NB. Create an assignment block for the operands, and put that on the stack.
    nobj =. conew 'dissectassign'
    stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)
    NB. See if we can analyze the assignment.  If so, add to the name table.
    NB. If the assignment is not a noun value, ignore it with a warning
    if. 0 = noun bwand 2 { exetypes do.
      smoutput 'non-noun assignment ignored'
      rname =. 0$a:
    NB. See if it's a simple assignment to a name
    elseif. name = 0 { exetypes do.
      rname =. (<0 1) { exeblock  NB. boxed name
    NB. If the assignment is an AR assignment, ignore it with a warning
    elseif. (sdt+noun) ([ -: bwand) 0 { exetypes do.
      rname =. (<0 1) {:: exeblock  NB. locale of sdt
      NB.?lintonly op_dissectnoun_ =: '' [ rname =. <'dissectnoun'
      if.  2 = 3!:0 lvalue =. ". op__rname do.  NB.?lintonly [ lvalue =. ''
        if. '`' = {. lvalue do.
          smoutput 'AR assignment to ' , lvalue , ' ignored'
          rname =. 0$a:
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
    elseif. qendb e. ;:'()=.=:' do.
      stack =. ((qend;(#queue)) ;~ (lpar,rpar,2#asgn) {~  (;:'()=.=:') i. qendb) , stack
    NB. If self-defining term, create a noun block for it, mark as sdt
    elseif. (qend e. ;:'a. a: _.') +. (-. '.:' e.~ {: qend) *. ({. qend) e. '''_0123456789' do.
      nobj =. conew 'dissectnoun'
      stack =. stack ,~ (<sdt+noun) 0} create__nobj qend;'';(#queue)
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
        'objtype objvalrank' =. 1 2 { (({."1 defnames) i. <objloc) { defnames
        gloc =. objvalrank
      elseif. (<objloc =. objloc , '_' , (>loc) , '_') e. {."1 defnames do.
        NB. not found as a local, but it may have been assigned in this sentence as a global.  If so,
        NB. use that value
        'objtype objvalrank' =. 1 2 { (({."1 defnames) i. <objloc) { defnames
        gloc =. objvalrank
      elseif. do.
      NB. Nothing found in local table - set to resolve the whole thing globally
        gloc =. loc
        glopart =. qend
      end.
      NB. Now we have resolved any local that we are going to use.  If there was one, it is in
      NB. objtype/objvalrank.  But a global search may be needed: if there was
      NB. an object locative, or if the local search failed.  This search will start in locale gloc.
      NB. This search, if performed, must succeed, and we will convert the result to a type/(rank if verb)
      if. #glopart do.
        NB. First, see if this global name was assigned in this sentence.  If so, use that value
        if. (<objloc =. glopart , '_' , (>gloc) , '_') e. {."1 defnames do.
          NB. Name is in our local table.  Use that
          'objtype objvalrank' =. 1 2 { (({."1 defnames) i. objloc) { defnames
        else.
          savloc =. coname''
          NB.?lintonly savloc =. <'dissect'
          NB.?lintmsgsoff
          cocurrent gloc
          NB.?lintmsgson
          if. 3 = objtype =. 4!:0 :: _2: <glopart do.
            objvalrank =. rankinv_dissect_ f. glopart
          else.
            obtype =. _1
          end.
          cocurrent savloc
        end.
      end.
      NB. Now objtype/objvalrank are set.  If the name is a noun or verb, create a locale for it
      NB.?lintonly 'objtype objvalrank' =. 0;0 0 0 0
      select. objtype
      case. 0 do.
        nobj =. conew 'dissectnoun'
        ntypeval =. create__nobj qend;qend;(#queue)  NB. Keep name, and save name for display
      case. 1 do.
        ntypeval =. adv;qend;(#queue) 
      case. 2 do.
        ntypeval =. conj;qend;(#queue) 
      case. 3 do.
        nobj =. conew 'dissectverb'
        ntypeval =. create__nobj qend;objvalrank;(#queue)
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
        nobj =. conew 'dissectnoun'
        ntypeval =. create__nobj qend;'';(#queue)
      case. 1 do.
        ntypeval =. adv;qend;(#queue) 
      case. 2 do.
        ntypeval =. conj;qend;(#queue) 
      case. 3 do.
        nobj =. conew 'dissectverb'
        ntypeval =. create__nobj qend;(rankinv qend);(#queue)
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
  failparse 'Sentence did not produce a noun result'
  return.
end.

NB. The locale at the top of the stack is the overall result.  Save that, and return the instrumented sentence
NB. This call will fill in all the verb-to-noun locale references
resultroot =: (<1 1) {:: stack
NB.?lintonly resultroot =: <'dissectmonad'
sentence;EXE__   =: exestring__resultroot''
NB.?lintsaveglobals
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
  NB. If all the operands are self-defining terms, execute the modifier, figure out the resulting
  NB. part of speech, and create an appropriate type/value for it.  This will handle things like
  NB. 3 b.   and 1!:1   and  1 : '...' .
  if. bwand/ sdt 1} exetypes do.
    ". 'exeobj =. ' , defstg =. ; 3 : 'if. 32 = 3!:0 y do. defstring__y 2 else. enparen y end.'&.> (1) {"1 exeblock
    tokennums =. ; 2 {"1 exeblock
    select. 4!:0 <'exeobj'
    case. 0 do.
      nobj =. conew 'dissectnoun'
      ntypeval =. create__nobj (5!:5 <'exeobj');'';tokennums
    case. 1 do.
      ntypeval =. adv;(enparen defstg);tokennums
    case. 2 do.
      ntypeval =. conj;(enparen defstg);tokennums
    case. 3 do.
      nobj =. conew 'dissectverb'
      ntypeval =. create__nobj (5!:5 <'exeobj');(rankinv defstg);tokennums
    case. do.
      failparse 'Invalid type while applying modifier'
      return.
    end.
  else.
    NB. Not sdt.  Look up the locale of the modifier, and execute it.  The executed modifier must correctly guess the
    NB. part of speech that is going to be produced.
    nobj =. conew 'dissectmod' , ": ((<1 1) { exeblock) i.&1@:((e.>)"0) dissectmodindex
    NB.?lintonly nobj =. localedefault
    ntypeval =. create__nobj exeblock
  end.
end.
ntypeval
)


NB. After the sentence has been executed, return here to display


DISSECT=: 0 : 0
pc dissect;
xywh 3 19 20 20;cc dissectisi isigraph;
xywh 4 2 54 60;cc fmfontsize combolist;
xywh 60 4 24 11;cc lbl01 static;cn "Font";
xywh 88 2 42 60;cc fmmaxnounsize combolist;
xywh 131 4 69 11;cc lbl00 static;cn "Max Noun (% of scrn)";
xywh 202 4 49 12;cc fmshowstealth button;cn "Show ][";
xywh 254 4 65 12;cc fmshowerror button;cn "Show Err";
pas 0 0;
rem form end;
)

DISSECT=: 0 : 0 [^:IFQT DISSECT
pc dissect;
bin vh;
minwh 54 60;cc fmfontsize combolist;
minwh 24 11;cc lbl00 static;cn "Min Font";
bin s;
minwh 42 60;cc fmmaxnounsize combolist;
minwh 42 60;cc lbl01 static;cn "Max Noun (% of scrn)";
bin s;
minwh 49 12;cc fmshowstealth button;cn "Show ][";
minwh 65 12;cc fmshowerror button;cn "Show Error";
bin z;
minwh 20 20;cc dissectisi isidraw flush;
bin z;
pas 0 0;
rem form end;
)

NB. wd covers
wdsetitems =: ([: wd 'set ', [ , ' *' , ])`([: wd 'set ', [ , ' items *' , ])@.IFQT
wdsetselect =: ([: wd 'setselect ', [ , ' ' , ])`([: wd 'set ', [ , ' select ' , ])@.IFQT
wdsetcaption =: ([: wd 'setcaption ', [ , ' *' , ])`([: wd 'set ', [ , ' caption *' , ])@.IFQT
wdsetshow =: ([: wd 'setshow ', [ , ' ' , ])`([: wd 'set ', [ , ' show ' , ])@.IFQT
wdsetxywh =: ([: wd 'setxywhx ', [ , ' ' , ":@])`([: wd 'set ', [ , ' wh ' , [: ": _2 {. ])@.IFQT
wdqform =: ([: wd 'qformx'"_)`([: wd 'qform'"_)@.IFQT
wdqchildxywh =: ([: wd 'qchildxywhx ' , ])`([: wd 'qchildxywh ' , ])@.IFQT
wdpmove =: ([: wd 'pmovex ' , ])`([: wd 'pmove ' , ])@.IFQT

FONTSIZECHOICES =: 8 10 12 14 16
MAXNOUNPCTCHOICES =: 30 50 70 90
MAXEXPLORERDISPLAYFRAC =: 0.8   NB. Amount of screen to allow for nouns in explorer

display =: 3 : 0   NB. called in dissect locale
displaymain__dissectinstance y
)
NB. y is the results from running the user's original sentence and our instrumented version.
displaymain =: 3 : 0  NB. called in object locale
NB. Make sure the results are the same
NB. If the sentence ran correctly for the user, make sure we get the same result
if. -. -:/ y do.
  smoutput 'dissect error: internal result does not match the result from the J session!  Aborting.'
  qprintf'y '
  return.
end.

NB. save the crash indicator
if. crashed =: 0 = #0 {:: y do.
  NB. Save the error message for the crash
  errormessagefrominterp =: _6 }.^:(' error' -: {.) (<:13!:11''){::9!:8''
else.
  errormessagefrominterp =: ''
end.

NB.?lintonly COCREATOR =: <'dissectobj'
NB. Create a selector that will admit anything, without infinities
TRAVNOUN =: 0;NORANKHIST,<0 , >:ticket

NB. The argument of $0 indicates that we want to set the crash variables
NB. debug wd :: 0: 'psel dissect;pclose'
NB. If we didn't crash, remove the show error button
wd ; <@(#~ -.@('fmshowerror'&(+./@:E.)));.2^:(-. crashed) DISSECT
winhwnd =: wd 'qhwndp'
wd^:(IFQT) 'pshow;pshow sw_hide'

NB. Initialize the user selection
'fmfontsize' wdsetitems ; (LF ,~ ":)&.> FONTSIZECHOICES
'fmfontsize' wdsetselect ": minimumfontsizex =: 2
'fmmaxnounsize' wdsetitems ; (LF ,~ ":)&.> MAXNOUNPCTCHOICES
'fmmaxnounsize' wdsetselect ": maxnoundisplaysizex =. 1
calccfms minimumfontsizex { FONTSIZECHOICES
maxnoundisplayfrac =: 0.01 * maxnoundisplaysizex { MAXNOUNPCTCHOICES
displaystealth =: 0

NB. Initialize the parent-node indicator in every object
initparentnodes__resultroot 0$a:
NB. Init the SDT-detail indicator in every object
NB.?lintonly resultissdt_dissectmonad_ =: 0
initnounshowdetail__resultroot resultissdt__resultroot
NB. Init the estheights in every object
calcallestheights__resultroot $0
NB. If we crashed, do an initial traversal to set selection flags to find the error
maxnoundisplaysizes =: 2 2$0  NB. Init to small display for sniff, for speed
if. crashed do.
  joinlayoutsl traverse 1   NB. Don't forget the final display!
  setdisplayerror__resultroot''
end.
NB. Save the size of the screen, which we will use to decide max noun size.  h w
screensize =: 3 2 { 0 ". wd 'qscreen'

NB. Set the initial selection of cells:
NB. If the result is an sdt, the user is probably noodling around with a new sentence, so select everything
NB. resultissdt__resultroot
NB. Get the size of the sentence; use its height to set scroll position for the display
sentencesize =. 0 {:: usersentence sizesentence gettokenlevels__resultroot ''
NB. Set the starting position, just below the sentence
scrolltlc =: 0 ,~ 2 + {. sentencesize
NB. obsolete dissect_dissectisi_paint 0
NB. Do the initial traversal, calculate the initial placement.
placeddrawing =: calcplacement''
NB. Include expansion room
yxneeded =. 300 100 + sentencesize >. 0 {:: shifteddrawing =. scrolltlc sizeplacement placeddrawing
NB. Center the sentence in the screen area; remember size
sentencebrect =: sentencesize (] ,: +) 0 , yxneeded (<.@-:@-)&{: sentencesize 
'dissectisi' wdsetxywh 1 0 3 2 { isibox =. (1 0 { 0 ". wdqchildxywh 'dissectisi') , yxneeded
wdpmove ": , (0 ". wdqform'') ({.@[ , >.&{:)&(2 2&$) 1 0 3 2 { isibox
wd 'pshow'
NB.The initial paint event will draw the screen
NB.?lintsaveglobals
)

NB. lay out the grid.  Should be called in the locale of the major instance
calcplacement =: 3 : 0
NB. Check the current screensize, and calculate the box sizes in characters
maxnoundisplaysizes =: <. (maxnoundisplayfrac , MAXEXPLORERDISPLAYFRAC) */ screensize

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
  drawplacement }. scrolltlc sizeplacement placeddrawing
  glpaint''
catch.
  smoutput 'error in paint'
  smoutput > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput 13!:12''
end.
)


NB. Clean up everything.  y is the return value to use
destroy =: 3 : 0
NB.?lintmsgsoff
for_o. ~. objtable do. destroy__o '' end.
NB.?lintmsgson
resultroot =: 0$a:
if. #winhwnd do.
  wd 'psel ' , winhwnd
  wd 'pclose'
  winhwnd =: ''  NB. not required
end.
dissectionlist_dissect_ =: dissectionlist_dissect_ -. coname''
NB. Restore original session state.  We hope this hasn't been changed while we were running!
9!:39 Jenvirons
codestroy''
y
)

NB. The 'show error' button displayes the error state
dissect_fmshowerror_button =: 3 : 0
traverse 1
dissect_dissectisi_paint 1
)

NB. Toggle the state of stealth display
dissect_fmshowstealth_button =: 3 : 0
'fmshowstealth' wdsetcaption (displaystealth =: -. displaystealth) {:: 'Show ][';'Hide ]['
NB. The operand is the list of types that should NOT be displayed
calcallestheights__resultroot displaystealth # 1 2
dissect_dissectisi_paint 1
)

dissect_close=: destroy

dissect_fmfontsize_select =: 3 : 0
NB.?lintonly fmfontsize_select =. '0'
minimumfontsizex =: 0 ". fmfontsize_select
calccfms minimumfontsizex { FONTSIZECHOICES
dissect_dissectisi_paint 1
)

dissect_fmmaxnounsize_select =: 3 : 0
NB.?lintonly fmmaxnounsize_select =. '0'
maxnoundisplayfrac =: 0.01 * MAXNOUNPCTCHOICES {~  0 ". fmmaxnounsize_select
dissect_dissectisi_paint 1
)

NB. Draw the user's sentence at the top, showing highlighting
NB. x is the sentence, in the user's spacing
NB. y is a table of (token range);display level)
NB. The characters are drawn, with appropriate colors
sizesentence =: 4 : 0
'DSX__ DSY__'   =: x;<y
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
boxhw =. , (((}.~) (, $:)~^:(*@#@[) <@{.~)   scrwid I.~ (+/\@:({:"1))) rectsize
NB. Get the height of each line
lh =. >./@:({."1)@> boxhw
NB. Get the start of each line, which is zero here for left justification
NB. obsolete lw =. <./ -:@(scrwid - +/)@:({:"1)@> boxhw
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

NB. y is 0 for normal traversal, or 1 for error traversal.  We initialize and traverse
traverse =: 3 : 0
errorlevel =: 0
snifferror =: y
NOLAYOUTS traverse__resultroot TRAVNOUN
)

NORANKHIST =: <0 0$a:
NORANKHISTNOUN =: <''   NB. string means noun

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
if. #nloc =. proplocales x do.
  nloc ([:  v   (v (1&{::))`(u (2 : (':';'l ([ cocurrent)~ (u traversedown v)&>/ y [ (cocurrent x) [ l =. coname''''')) v)@.(2 = 3!:0@>@[)"0 _ ) x ,&< u y
else.
  '' v u y
end.
)

NB. Propagate selection down the tree (root to leaves).  y is the value to propagate.  We propagate
NB. selections to all verb operands; #selections to conjunction noun operands as well
NB. The calls to traversedown must be named verbs!!
propsel0 =: 0&((3 : 'selections =: y') traversedown 0:)
propsel1 =: 1&((3 : ('displaysellevel =: y')) traversedown 0:)
propsel =: propsel1@# [ propsel0

NB. Clear scroll point (at nodes BELOW the starting node).  y is 1 to start, and the assignment is made only if <: 0
propscroll =: 0&((3 : 'if. y <: 0 do. scrollpoints =: 0 2$0 end. <: y') traversedown 0:)


NB. Propagate selection up the tree (leaves to root), until we hit a monad/dyad execution.  y is value to select
propselup =: 3 : 'selections =: y if. -. ({. copath coname'''') e. ;: ''dissectmonad dissectdyad'' do. propselup__parent y end.'

NB. init parent nodes in all objects.  This must be done in a separate verb because only a named verb resets the locale
NB. Called in locale of the base of the tree
initparentnodes =: 2&((3 : 'coname 0 # parent =: y') traversedown 0:)

NB. called after sniff to indicate which nodes can have an error display
setdisplayerror =: 2&((3 : 'errorwasdisplayedhere =: {. ".''*#DOstatusstring''') traversedown 0:)

NB. init SDT-display flag in all objects.  y is the value to set
NB. Called in locale of the base of the tree
initnounshowdetail =: 2&((3 : 'y [ nounshowdetail =: y +. -. resultissdt') traversedown 0:)

NB. calculate estheights for display.  We call estheights during the upwards traversal
NB. Called in locale of the base of the tree
calcallestheights =: 2&((3 : 'y [ dispstealthoperand =: {. stealthoperand -. y') traversedown (calcestheights@]))

NB. Return selection level for each token in the input string
NB. Result is table of (token number(s));selection level
NB. Called in locale at the base of the tree
gettokenlevels =: 3&((3 : '<displaysellevel') traversedown (3 : ('<;y';':';'<,:x,y')))


NB. common routines used by the object locales.  Objects are subclasses of dissectobj

cocurrent 'dissectobj'
coinsert 'dissect'

NB. The following names must be redefined when an object is cloned
clonenames_dissect_ =: ;: 'selections scrollpoints displaysellevel explorer errorwasdisplayedhere'

NB. Object creation.  create the signaling variables used for communicating with the grid.
NB. y is <the tokens that these values came from
NB. Each verb-type object is responsible for creating:
NB. titlestring: the name that will appear in the display at the top of a box
NB. stealthoperand: at create time, this is set to 0 for normal verb, 1 for verbs that have no display (][), 2 for [:
NB. valence: when the verb gets a valence (i. e. when its monad/dyad exec happens), that valence is saved.
NB. estheights: number of blocks from the bottom of the noun node to the input(s) to this node.  This list has one atom per
NB.  valid operand. Set during setvalence, when we know the valence etc.  Height of <:0 is special, and indicates a stealthoperand like ] [,
NB.  where _1 is the ignored operand and 0 is the passed-through operand
NB. displaysellevel: like #selections, but the value to use for display.  Noun operands of conjunctions are given the same level
NB.  as the verb, for pleasing display
create =: 3 : 0
coinsert COCREATOR
NB. The following names are not modified after the object is cloned:
titlestring =: ''
tokensource =: ~. > 0 { y
stealthoperand =: 0   NB. set for verbs that have no display, viz [ ] [:

NB. The following names are guaranteed modified in the clone after this object is cloned:
NB. resultissdt is guaranteed set in all objects, and not initialized so that if we coinsert an object
NB. we will not have an initial value to block inheritance
NB. Before monad/dyad execution, resultissdt is set for modifier processing.  After monad/dyad, it is set
NB. for verb processing.
estheights =: 1
failingselector =: 0$a:  NB. If this is ever modified, it will be modified in both the clone and the original

NB. The following names are possibly modified after cloning.  Therefore, they must be copied into the clone
NB. when the clone is created, so that a mod to the original doesn't affect the clone.
NB. IF YOU ADD NAMES HERE YOU MUST ALSO ADD THEM IN clone!
(clonenames) =: (0$a:);(0 2$0);0;'';1

NB.?lintonly valence =: errorlevel =: snifferror =: 1
NB.?lintonly defstring =: ":
NB.?lintonly resultissdt =: nounhasdetail =: nounshowdetail =: 0
NB.?lintonly 'displayhandlesin displayhandleout displaylevrank fillmask' =: '';($0);($0);'';(0 3$a:);($0)
NB.?lintonly dispstealthoperand =: 0
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
auditstg   =: 3 : 0
if. ~:/ '()' +/@:="0 _ y do.
  smoutput 'unbalanced parens'
  smoutput 'Error in exestring in locale ' , >coname''
  smoutput 'string: ' , y 
  estring__   =: y
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
try.
  uop =: clone__uop''
  vop =: clone__vop''
catch.
end.
NB. Return the new locale
cl
)

NB. Switch object processor.  y is the name of the new object processor.
NB. We assume that the object locale is the top of the path, and we change it to the given y
changeobjtypeto =: 3 : 0
(copath~  (boxopen y) 0} copath) coname''
)

NB. Init the logging table.  y, if given, is the suffixes to use for logging
initloggingtable =: 3 : 0
nex =. (<''),<"0 y
NB.?lintmsgsoff
('logticket'&,&.> nex) =: ((#nex),0) $ 0
('logvalues'&,&.> nex) =: ((#nex),0) $ a:
NB.?lintmsgson
NB.?lintonly logticket =: 0$a: [ logvalues =: 0$0
NB.?lintonly verbex =: ]
NB.?lintsaveglobals
''
)

NB. add to log.  Always called in the locale of the parse object.  x, if given, is the suffix to use for this logentry
NB. y is the value to log
addlog =: ''&$: : (4 : 0)
NB.?lintmsgsoff
('logticket',x) =: (". 'logticket',x) , ticket__COCREATOR =: >: ticket__COCREATOR
('logvalues',x) =: (". 'logvalues',x) , <y
NB.?lintmsgson
''
)

NB. create string to use to add log entry.  y is the suffix to use, if any.
NB. The string produces a verb of infinite rank whose value is the same as its y
logstring =: 3 : 0
'([ ' , ((# # '''' , ,&'''&') y) , 'addlog_' , (>coname'') , '_)'
)

NB. Create string to add for logging a verb.  This creates a verb, named verbex, in the current locale
verblogstring =: 3 : 0
'verbex_' , (>coname'') , '_ =: '
)

NB. Create string to add for logging a conjunction result.  This creates a name, conjex, in the current locale
conjlogstring =: 3 : 0
'conjex_' , (>coname'') , '_ =: '
)

NB. ***************** traverse down ****************

NB. utilities for traversal and selection
NB. These are overridden as needed by individual modifiers.  The versions here work for simple verbs

NB. get the rank to use for this verb.
NB. y is selopinfo, 0 to 2 boxes containing (remaining shape);(frame so far)
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
NB. If snifferror__COCREATOR is set, it means we are doing the initial selection to find where the crash was.
NB. When there is a selection, we always select the last thing that happened.
NB. y is a single list with 0-4 items depending on selection status.  When operands are given, it is implied that
NB. the selector is selecting a single application of the verb.
NB.
NB. The global   selections  is maintained by the selection system, and indicates click status.  The value holds the entire selection path from 
NB.  the base selector.  If there is a selection at this level (i. e #selection is > sellevel), then sellevel{selections
NB.  if the selector to use here
NB.
NB. Object globals set here are:
NB. sellevel - the selection level of this node.  Incremented when there is a selection
NB. collected - set if the input selector to this verb selected a single input cell (i. e. frame was empty or all 1)
NB.   when this is set, we know that even if there are multiple results, it must be valid to try to collect them
NB. frame - the frame of the verb.
NB. frames - the individual franes, boxed
NB. accumframe - The frame - but when we inherit u, we add on the rank from u, so that when we are
NB.  finished, accumframe shows the ranks of the levels of selection.  Used for calculating the rank display.
NB.  accumframe may hold one extra level for unexpanded detail.  In this case, the value in the last box of accumframe
NB.   holds the value that will be used for that selector when detail is to be expanded.  This value is always negative, so that
NB.   negative values in accumframe can be discarded as not representing actual ranks
NB. errorcode - An indication of the result of applying the selection to the results of the verb, and comparing it to the
NB.  frame of the operands.  Values > 0 indicate error, 0=OK, _1=unknown because no operands; _2=unknown because no selector.
NB.  The errorcode gives the status of the input values passed to this verb, BEFORE any local selector is applied.
NB.  Values: _2,_1=unknown 0=OK others below.  If a node and its child both have errors, the larger error is used.
NB.  UNEXECD=some cells ran, but not all, but no error PREVERROR=cells ignored because of previous error
NB.  ABORTED=no cells ran EXEC=cells ran, but one failed
NB.  Values EOK and below are terminals; they should not be replaced.  Values above EOK indicate
NB.  incomplete results; the lower a value, the more precise it is, so we will replace higher values
NB.  with a lower during inheritu.
'ENOUN EOK ENOAGREE EUNEXECD EPREVERROR EEXEC EFRAMING EABORTED ENOOPS ENOSEL' =: _1 + i. 10
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
NB. selopinfo - The shape of the selected operand, and the selection that has been applied to this operand so far.  In other words,
NB.  the (remaining shape);(frame so far).  These come in from the y input, representing the state before this level, and go out
NB.  with the state after going through the local selection.  The operand shape is inferred from the rank of
NB.   the verb, so is valid even when there is no selector.  List of boxes, one per operand
NB. selopinfovalid - set if there is no frame, or if there is a local selection.  List, one per operand.  This indicates that a single operand cell
NB.   has been selected, and can therefore be used by a subsequent v verb.
NB. fillmask - this has the shape of the open of frame $ result, and gives status for each atom thereof.  This status is the selection
NB.  level of this node (in the upper bits), and validity information in the lower bits.  The validity is
NB.  0=normal 1=fill 2=first unexecuted cell (presumably error, but that may depend on what happened elsewhere) 3=later unexecd cell
NB.  error is calculated per result cell & propagated to atoms; fill is calculated per atom.  fillmask is valid only for nouns, or if the
NB.  unselected result has a frame with multiple cells, and is undefined otherwise
'FILLMASKNORMAL FILLMASKFILL FILLMASKERROR FILLMASKUNEXECD FILLMASKSELLEVEL' =: i. 5
NB. errorlevel - a copy of errorlevel__COCREATOR at the time this is parsed, this indicates whether we were in a try block during
NB.  execution of this verb.  When it comes time to display error info, we don't use the result failure type for anything except
NB.  top-level errors
NB. inheritedselection - the selection performed by the verb this node's outputs feed into.  Cleared here, and set when two nodes
NB.  are joined together
NB. vranks - the rank(s) of the operand. 1 or 2 numbers, but empty for a noun or for a verb that didn't execute
NB. rankhistory - 1{y holds the table of previous ranks.  We append vranks to it, to produce the rank stack for this operand.  The format of the rank stack is
NB.  (string form of name to display);(sel level);(rank r)[;(rank l)
traversedowncalcselect =: 3 : 0
assert. 1 = #$y
'sellevel rankhistory' =: 2 {. y
selopinfo =: }. selandxy =. 2 }. y
errorlevel =: errorlevel__COCREATOR
inheritedselection =: 0$a:
4!:55 <'fillmask'   NB. We use existence of fillmask as a flag
if. #vranks =: getverbrank selopinfo do.
  rankhistory =: rankhistory , titlestring ; <"0 sellevel , vranks
end.
qprintf^:DEBTRAVDOWN 'snifferror__COCREATOR%,loc=?>coname''''%,type=?0{::copath coname''''%defstring 0%>uop%>vop%>cop%vranks%sellevel%selections%$y%y%rankhistory%'
if. 0 = #selandxy do.
  NB. No selector: we can't do much
  'collected frame frames errorcode selresult selector selopinfo selopinfovalid' =: 0;($0);a:;ENOSEL;(0$a:);(0$a:);(0$a:);0 0
elseif.
selector =: {. selandxy
NB. The failingselector is set only during the first sniff, and used thereafter to see if we are
NB. at the point of error
if. snifferror__COCREATOR do. failingselector =: selector end.
qprintf^:DEBTRAVDOWN '$>selector >selector >failingselector '
(1 = #selandxy) +. 1 < */ }: $ > selector do.
  NB. Just a selector, but no operands.  Must be an active multiple selection, or we just haven't hit operands yet (must be a noun),
  NB. or possibly a rank-calculus probe that ran out of rank.  We will not come through here if we are sniffing errors
  NB. Select the derived verb results, using the shape of the selector as the frame.
  if. 0 = #vranks do.
qprintf^:DEBTRAVDOWN '#logvalues '
    NB. selector and selop already set, keep them
    'collected frame selresult' =: 1;($0);<({.logvalues)
    errorcode =: (*#logvalues) { EABORTED,ENOUN
  else.
    if. *#>selector do.
      assert. (,2) -: $> selector [ 'multiple selection'
      NB. If non-noun going with no operands, there was some higher-level multiple selection: we can't collect
      collected =: 0
      frame =: }:$>selector
      selresult =: (; findselection > selector) { logvalues  NB. fails if no values
      errorcode =: (EOK,EUNEXECD,EABORTED,EABORTED) {~ #. (1,(*/ frame)) > #selresult
    else.
      NB. empty selector with no operands.  This is a rank-calculus probe that ran through a verb that it couldn't predict.
      NB. Treat it as if there had been no selector.
      'collected frame errorcode selresult selector selopinfo selopinfovalid' =: 0;($0);ENOSEL;(0$a:);(0$a:);(0$a:);(0:"0 selopinfo)
    end.
  end.
  'frames selopinfo selopinfovalid' =: a:;(0$a:);(0:"0 selopinfo)
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
  'selopshapes selopselhist' =: <"1 |: > selopinfo
  if. 0 = #vranks do.
    NB. selector and selop already set, keep them
    'collected selopinfovalid frame frames errorcode selresult' =: 1;(1:"0 selopinfo);($0);a:;EOK;<({.logvalues)
  elseif.
  NB. If snifferror is set, we will automatically produce a local selector to zero in on the error.
  NB. Get the frames of the verb
  execdframe =. >./ > execdframes =. (- vranks) }.&.> ,selopshapes  NB. frame meaningful only if no framing error
  NB. Audit the frames.  They should agree. and they should match the number of results we got.
  NB. If the frame is invalid, we know that this verb is going to die before it executes; indicate agreement error
  NB. Clear the selector to short-circuit further processing
  -.@-:/ (<.&#&>/ {.&> ]) 2 {. execdframes,<$0 do.  NB. No agreement error on monad
    NB. Here for verb with agreement error
    'collected errorcode selector selopinfovalid selresult frames frame' =: 0;ENOAGREE;(0$a:);(0:"0 selopinfo);(0$a:);execdframes;execdframe
  elseif.
  NB. Calculate the shape of the selected operand.  We will
  NB. return this shape, whether there is a valid selection or not, in case rank calculus allows us to
  NB. continue calculating frames without selectors.  This also creates a fill-cell if the frame was empty.
  NB. Simulate cell-replication:  prepend any surplus frame from the
  NB. other operand, then remove the frame from each operand
NB. obsolete   selopinfo =: selopinfo (((frame }.~ ]) , }.~) #)&.> frames

  NB. frame and selresult will be the values we use for DISPLAY at this node.  At this point frame is
  NB. what we expect to see - deviation will be because of error
  NB. selopshapes will be the shapes passed into the next selection level applied to this input
  NB.  kludge - this will depend on the current selection for u\ u/. etc
  'frames selopshapes' =: calcdispframe execdframes
  frame =: >./ > frames  NB. The common "frame"
NB. obsolete   selopshapes =: selopshapes (}.~ #)&.> frames
  a: -: selector do.
    NB. There were no selectors.  This means that rank calculus was applied somewhere to give us a shape without
    NB. a valid selector.  The frame is valid, as just calculated, and the operand shapes too
    'collected errorcode selector selresult selopinfovalid' =: 0;ENOOPS;(,a:);(0$a:);(1:"0 selopinfo)
  elseif. do.
    assert. 1 = */ }: $ > selector  [ 'travdowncalcselect'
    selx =. calcdispselx ; findselection > selector
    NB. Normal case, verb with no agreement error
    NB. Assume the verb completed unless we learn otherwise
    NB. If we get to here, the operand being presented will be collected, as in u@:v .  So we can display it.
    'collected errorcode' =: 1,EOK
    NB. Also, the number of results should match the number of cells in the frame, except
    NB. when the frame contains 0, in which case there will be 0 or 1 result.
qprintf^:DEBTRAVDOWN '$selopinfo selopinfo $frames frames $frame frame $selx ' 
    if. 0 e. frame do.
      NB. Execution on a cell of fills.  We should have 0 or 1 result.  If 0, it means that the
      NB. execution on the cell of fills failed, and we will use a scalar numeric as the replacement
      assert. 0 1 e.~ #selx
      NB. create a result of the required type and shape
      if. 0 = #selx do. selresult =: <0 else. selresult =: ({.selx) { logvalues end.   NB. error in fill cell - use scalar numeric
      NB. we will extend fill-cell with frame
NB. obsolete       NB. Replace selops, which will be used by lower verbs, with a cell of fills.  But don't set islocalselgiven,
NB. obsolete       NB. so there will be no selectors or formatting - just a new operand
NB. obsolete       selopinfo =: vranks ({.@,@] {.~ ((-@(<. #)) {. ]))&.> selopinfo
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
        NB. Cells did not execute.  If the selector is the one we found when sniffing errors, the error is in this cell.
        NB. Mark this cell as an execution error.  But if the selector is different, there is no permanent error here: either the user
        NB. changed the selector to a later cell which didn't execute anything, or we have determined that this cell
        NB. was partially-executed because of an error elsewhere, and there's no error here.  In those case, we mark
        NB. the cell as partially-executed, with no error
        if. selector -: failingselector do.
          errorcode =: (*#selx) { EABORTED,EEXEC
          NB. If we are sniffing for errors, and
          NB. there is a frame, sutoset the selection (for the next level) to point to the first unexecuted cell.  Do this only
          NB. for thw top-level error
          if. snifferror__COCREATOR *. 0 = errorlevel do.
            NB. If there is a frame, select the first non-executing cell.  For us to get here, any selectable higher levels
            NB. must have selected, so we will be adding to a selection chain.
            if. *#frame do.
              NB. we should have had errors at higher levels, which will have set the previous selectors
              assert. sellevel <: #selections  [ 'error in sniff'
              NB. During sniff, each error is propagated down separately.  We append the new error to the previous.
              NB. We know the previous exists, but there might be more, if we have sniffed error before; so we
              NB. append the calculated selector to the selectors valid for this node
              NB.
              NB. In normal debugging, selectors are added from the root outward.  If a selection is made above
              NB. the root, we don't propagate the selection back to the root, on the theory that if the user
              NB. wanted to select at the root, they could have.  But in the case of error, we really want the
              NB. root to have the entire failing selector, so that it can display it; so we propagate
              NB. the selector back to the originating monad/dyad
              (propselup [ propsel) (sellevel {. selections) , < frame getfailingindex #selx
qprintf^:DEBTRAVDOWN 'errorcode selections ' 
            end.
          end.
        else.
          errorcode =: ((0=#failingselector) +. *#selx) { EABORTED,EUNEXECD  NB. Indicate partial execution, but without error
        end.
      end.
      NB. We need to save the selected value.  We use this to calculate the predicted frame after collection.
NB. obsolete       selresult =: {.^:(0=#)selr  NB. This is the (unopened, since it might not collect) result from this object's verb
      selresult =: fetchselr selx  NB. This is the (unopened, since it might not collect) result from this object's verb
    end.

    NB. Handle selection for the next level

    NB. If the selection trims down the selection of results, apply that trim to the selector.
    NB. If we are sniffing and this verb failed, the final selection would fail
    NB. by definition; we will have handled that case above.  We increment sellevel whenever there is a frame, so that even nonselecting
    NB. frames show up in the shape display; but empty frames do not have selectors (to make the color sequence predictable).
    NB. Thus, we have to skip the selection when the frame is empty
    NB. install a selector only when 
    if. (*#frame) *. (sellevel < #selections) do.   NB. kludge use 1 < */ frame?
      selopinfovalid =: 1:"0 selopinfo  NB. Selection means shapes are valid for next v
      NB. Calculate the selection interval corresponding to each selected result.  Box each one so
      NB. that when we select, we will get a length error if selection goes too deep.  Bracket the
      NB. intervals with the start & end of the selector so that we create one extra interval that works
      NB. in case of crash, to get the inputs corresponding to the nonexistent last result
NB. obsolete        newsel =. tickettonatural frame $!.(<1 2$0) 2 <\ (,>selector) enclosing selx { logticket
NB. obsolete       NB. Apply the user's selection to the selector.
NB. obsolete        selector =: <> (sellevel { selections) { newsel  NB. This is where it would crash on error
      selector =: < (sellevel { selections) { tickettonatural frame $ 2 ]\ (,>selector) enclosing selx { logticket
      NB. If (owing to error) we had missing results, newsel will have had some flag values
      NB. inserted: {:allsel is a selector for anything following the last valid result, and
      NB. is a proxy for the input cell that led to error.  The other flag value is (<1 2$0), which
      NB. stands for a cell for which there is neither valid input nor valid output (i. e. if
      NB. it is an input, is was never executed).  Look for these in unewsel, and turn on
      NB. corresponding bits in islocalselgiven to indicate validity.  A setting of 'input unexecuted'
      NB. and 'no output error' means we selected an input that was PAST the error
NB. obsolete       islocalselgiven =: 1 + +/ 2 4 * -. ((<1 2$0),{:allsel) e. , unewsel
      NB. Now make the selection.  This cannot fail, since selops and bresultafterlocalsel are both full-size,
      NB. and usel was vetted above.  (not needed now, since we don't select the actual operand)
      NB. For the operands, we need to handle cell repetition for agreement.  We don't repeat cells
      NB. is the display; we show the cells and know that they will be repeated as necessary
NB. obsolete       selops =: selops ({~ <)&.> usel (<.&# {. [)&.>  frames
      NB. remember the pasrts of the selector that were applied to each operand
      assert. *./ (sellevel { selections) (>:&#)&> frames
      selopselhist =: selopselhist ,&.> (sellevel { selections) ({.~ #)&.> frames
NB. obsolete     elseif. -. 0 e. frame do.
NB. obsolete       selector =: <>newsel
    else.
      NB. If there was no selection, we can still create a display for v if this selection produces a single cell,
      NB. which we know will be presented to v all at once.  Account for operand replication, by using the common frame
      NB. rather than the individual frames
      selopinfovalid =: ($selopinfo) $ 1 >: */ frame
    end.
    NB. If there was no selection, keep selector unchanged
  end.

  NB. reconstitute selopinfo from its component bits
  selopinfo =: selopshapes (<@,"0) selopselhist
end.

NB. Now selresult (a list or scalar) contains the unopened and unframed results.  frame contains the frame.  We will calculate a faux
NB. shape for the result, by looking at the values without opening selresult.  If they have a common
NB. shape, we will show that shape after the frame.  If the cell collected, and there was no error
NB. (meaning they all ran), we will create the opened version of selresult, which is what we will use later.
NB. It is valid only if errorcode=0 and collected=1.

'maxsize fillatom' =. checkframing selresult
NB. Calculate the fill mask for the current verb, without requiring it to collect properly
selresultshape =: frame,maxsize

NB. If the result has a frame, simulate collecting it, to detect framing error.  If not, leave
NB. fillmask undefined.  If selection was impossible, either because the were no operand shapes
NB. or a missing or empty selector, don't try to calculate fillmask for the cells (we might
NB. create a zero fillmask below)
NB. If no cells were executed, no fillmask is meaningful.
NB. If the fillmask is not created, wipe out any previously-existing one
if. errorcode e. ENOAGREE,EABORTED,ENOOPS do.
  NB. We may change the errorcode later, so if we didn't set the fillmask, we need to show that this
  NB. node can never display the value
  collected =: 0
else.
  if. 1 < */ frame do.
    NB. If this level is selectable, increment the selector level to use for this and subsequent levels - whether
    NB. we have a selector yet or not.  If this level is selectable, and we didn't qualify it down to a single input,
    NB. no selection will be possible at lower levels, no matter what happens later.  So we might as well assume that
    NB. we qualified, and we just add one to the next level if selection here was possible.
    NB. Calculate the per-item part of fillmask: the selection level (upper bits), plus validity,
    NB. which is 0=OK, 2=first missing item, 3=later missing item.  No 'first missing item' unless there is an error here
    fillmask =: (FILLMASKSELLEVEL * sellevel) + tickettonatural frame $!.FILLMASKUNEXECD (FILLMASKNORMAL #~ #selresult) , (errorcode = EEXEC) # FILLMASKERROR
    NB. Expand all the cells of selresult to
    NB. the shape of the maxsize, with FILLMASKFILL for any added cells.
    NB. Combine the per-item and per-atom parts of the fillmask
    fillmask =: fillmask + > frame $!.(<maxsize$0) maxsize&([ {.!.FILLMASKFILL (0) $~ (-#maxsize) {.!.1 $@])&.> selresult
    NB. debug qprintf 'collected%frame%$L:0 selresult%selresult%y%'
    NB. If the result contains dissimilar types, raise an error.  Treat empty as no type
    if. 0 = #fillatom do. errorcode =: EFRAMING end.   NB. If framing error, so indicate
  elseif. collected do.
    NB. If this node might display, but there was no frame, we'd better provide a fillmask.  Use the
    NB. shape of the (one or none) result, and the current selection level.  Note that there is no display
    NB. if there were no cells executed
    fillmask =: (FILLMASKSELLEVEL * sellevel) $~ frame , $@> {. selresult
  end.
NB. obsolete else.
NB. obsolete  NB. if this node is not a collector, we must clear selops since they are invalid for future use
NB. obsolete  'selresult selresultshape' =: (<'?-');$0
end.

NB. If this verb has a frame, increment the selection level, whether it allows selection or not
bnsellevel =: < sellevel + *#frame
NB. Initialize the counter of this verb's frames; may be overridden later to provicde initial selection for unexpanded detail
accumframe =: <frame

qprintf^:DEBTRAVDOWN 'collected collectedframe errorcode frame $selresult selresult selresultshape selector selopinfovalid selopinfo fillmask selections rankhistory '
NB.?lintsaveglobals
)

NB. Check for collectability, and calculate a fill cell
NB. y is a selresult - possibly empty list of boxes containing results.
NB. result is shape of max opened cell;atom for fillcell, or maxshape;empty if not collectable
NB. An empty y is considered not collectable.
checkframing =: 3 : 0
NB. Extract the shapes of the operands, reversed, as a list
if. 0 e. $y do.
  0;$0
else.
  bshp =. , |.@$&.> y
  NB. Create the mask of which cells would be fill if the cells were opened.  Calculate the largest
  NB. result, and replace each box with that result, with the unfilled values zeroed.
  maxsize =. |. >./ (>./ #@> bshp)&({.!.1)@> bshp
  if. 2 <: +/ classes =. 0 ~: 16b74dd 16b0802 16b8020 16b10000 16b20000  bwand bwor/ ((0 -.@e. $) * 3!:0)@> y do.
    maxsize;$0   NB. framing error
  else.
    maxsize ;< (0;' ';a:;(s:'');(u: ' ');0) {::~ classes i. 1
  end.
end.
)

NB. Return the value of selresult
NB. y is unused
NB. x, if given, is result of checkframing
NB. result is framingok;framed selresult
NB. if framing failed, the box around each component of selresult is retained; otherwise the
NB. boxing is removed and the results collected
frameselresult =: 3 : 0
(checkframing selresult) frameselresult y
:
'cs fill' =. x  NB. result cell size, fill atom (empty if unframable)
(*#fill) ;< <"0@(cs&{.)@>`>@.(#fill) tickettonatural frame $!.(<cs $ {.!.' ' fill) selresult
)

NB. Utilities for inspecting accumframe.  The last box of accumframe may be a prospective value, i. e. the value used
NB. for selecting unexpanded detail.  Except when we are selecting, we want to ignore the negative values
NB. We put this selector into accumframe so that it will be passed along as the DOL is moved from locale to locale
NB. For all of these, y is accumframe
NB. Boxed accumframe, including only actual shapes
afact =: }:^:(0>{.@>@{:)
NB. Flattened accumframe, with negative values removed
afflat =: (#~ >:&0)@;
NB. Flattened accumframe, with 1 in the first position of each box.  Used to reshape the selector
afkey =: [: ; [: {.&1&.> #@>
NB. Number of values in accumframe
afcount =: +/@:(0 <: ;)

NB. obsolete 
NB. obsolete NB. y is errorcode(s), result is true if it is OK for child nodes to select
NB. obsolete travdownselok =: -.@(+./@:e.&1 2 3 4 5)
NB. obsolete 
NB. obsolete NB. Calculate operands to use for u a
NB. obsolete NB. y is sellevel,selector,selops
NB. obsolete NB. global errorcode has been set by traversal of a
NB. obsolete NB. Result is operands to pass into u
NB. obsolete NB. If there was an agreement error, suppress the selector
NB. obsolete NB. If there was any other error, or if the adverb didn't collect,
NB. obsolete NB.  suppress the operands
NB. obsolete travdownadvuops =: 3 : 0
NB. obsolete y {.~ 2 <.^:(-.collected) _ 1 2 {~ 0 1 5 I. errorcode
NB. obsolete )
NB. obsolete NB. x is vdisplayrcds, y is operands to travdown
NB. obsolete NB. We produce the input operands to travdown for the left side of a fork or & .  If there was an error
NB. obsolete NB. on the right side, we know the left side will not run, so we discard its selops (keeping the selection if any)
NB. obsolete travdownv0ops =: 4 : 0
NB. obsolete ({.~ 2 <. #)^:(-. travdownselok errfromdisplayrcd x) y
NB. obsolete )
NB. obsolete 

NB. Signal agreement error
NB. Agreement error requires insertion of a dyad showing the location of the error.  For the nonce,
NB. we will abort traversal at that point.
NB. y is the dol (x operand to traverse)
NB. result is a suitable return value from traverse, viz y ,&< locale
agreementerror =: 3 : 0
NB. Since we abort the traversal, roll up the failing part and install it as the last name in the rank stack
'displayhandlesin displayhandleout displaylevrank' =: _0.3 0.3;0;<(<defstring 0) (<_1 0)} rankhistory
y ,&< coname''
)

NB. Inherit information from u and v
NB. After u has been traversed, we roll its info into the current object for later display
NB. y is result of traverse: dol ,&< locale of dol
NB. Result is the dol ,&< locale to display: usually this locale, but if this locale had no results and the lower
NB. locale did, the lower locale
NB. x is the locales of v.  If any of them had an error at level 0, signal PREVERROR here
NB. If u had an error, the main locale should have also (except for fill-cells and try).  If both had errors,
NB. keep the one from u
inheritu =: 4 : 0
vlocs =. x
'dol loc' =. y
if. DEBDOL do.
smoutput 'inheritu: in ' , (>coname'') , ' ' , defstring 0
qprintf'>loc defstring__loc]0 '
qprintf'errorcode errorcode__loc '
end.
displocale =. coname''
NB.?lintonly loc =. <'dissectobj'
displayhandlesin =: displayhandlesin__loc
displayhandleout =: displayhandleout__loc
displaylevrank =: displaylevrank__loc
dispstealthoperand =: dispstealthoperand__loc
NB. If the u was uncollectable, or if it executed no cells, it can't contribute to the fillmask
NB. and its fillmask is guaranteed undefined.  Likewise, if there was no selector, there will be no fillmask
NB. Append the frame of u to the ranks calculated at this level.  If u lacked selectors, the frame
NB. will be empty, and we will ignore it.  If a parent of u had an empty selector, the actual numbers
NB. in the frame might have been replaced by placeholders of _1, but their number will be right.
accumframe =: accumframe , accumframe__loc

select. errorcode
case. EOK;EUNEXECD;EEXEC;EFRAMING;ENOOPS;ENOUN do.
  if. 0 = 4!:0 <'fillmask__loc' do.  NB.?lintonly  fillmask =: fillmask__loc =: ''
  NB. The folding-in of fillmask depends on whether errors were encountered.  If there was no error,
  NB. or if the error was an agreement error, we know that all the calculated fillmasks are
  NB. commensurate.  In that case,
    NB. If a fillmask was calculated at the lower level, it should be more accurate than the
    NB. fillmask for the current level, or at least more detailed.  If this level has a selection, we
    NB. know ipso facto that this level had a frame and therefore a fillmask; in that case, insert
    NB. the lower fillmask into this level's, after expanding it to match the collected result-cells
    NB. (because the selected cell may need fill to fit into the final result)
    NB.  If this level does not have a selection, the lower
    NB. fillmask must cover the same cells as this level, and this level might have no frame and
    NB. therefore no fillmask; so in that case use the lower fillmask as this level's entire fillmask.
    NB.  If the higher verb failed in the middle of execution, that should be possible only if u
    NB. also failed in the middle of execution.  Insert the fillmask as in the no-error case
    NB.  If the higher verb had no operands, it must be a monad/dyad execution;
    NB. it will perforce have no selection, so just pick up the fillmask from the u
    if. (*#frame) *. (sellevel < #selections) do.
      NB. The value to use for filling cells in the u fillmask depends on the errorcode for u.  If there
      NB. is no error, it's just normal fill
      fillval =. (FILLMASKUNEXECD,FILLMASKERROR,FILLMASKFILL) {~ (EUNEXECD,EEXEC) i. errorcode__loc
      fillmask =: (((#>sellevel { selections) }. $fillmask) {.!.fillval fillmask__loc) (sellevel { selections)} fillmask
    else. fillmask =: fillmask__loc
    end.
  end.

  NB. If there is an error code in both the old and new u records, 
  NB. keep the one that has more specific information
  if. EOK < errorcode do.
    if. EOK < errorcode__loc do. errorcode =: errorcode <. errorcode__loc end.  NB. errors old & new.  
  NB. obsolete case. _1 1 do.   NB. old error, new is in runout
  NB. obsolete   errorcode =: (*errorlevel) { 4 3  NB. errors old & new.  use (error) code, ignore new shape
    NB. If there was a fatal error in any v, we will display it; signal PREVERROR
    for_l. vlocs do.  NB.?lintonly l =. <'dissectobj'
      if. (EPREVERROR<errorcode__l) *. 0=errorlevel__l do. errorcode =: EPREVERROR end. 
    end.
  end.

case. EABORTED do.
  NB. If the top verb aborted, it has nothing to show for the execution, so if u has any results,
  NB. we should display it instead of the parent.  Change of errorcode has already been performed on u
  if. errorcode__loc < EABORTED do.
    displocale =. loc
  else.
  NB. but if u didn't execute, and we know the parent node tried and failed, it might be because
  NB. v failed (it could also be an error in monad/dyad exec).  If v failed, error will have been displayed in v; take it out of display in the parent
    for_l. vlocs do.  NB.?lintonly l =. <'dissectobj'
      if. (EPREVERROR<errorcode__l) *. 0=errorlevel__l do. errorcode =: EPREVERROR end. 
    end.
  end.
case. ENOSEL do.
case. do.
  assert. 0 [ 'invalid errorcode in inheritu'
  NB. The other codes are invalid for the following reasons:
  NB. ENOAGREE - agreement error aborts traversal
  NB. EPREVERROR - error code is assigned only here and in inheritv
end.

dol ,&< displocale
)

NB. called in locale of an operand
NB. Nilad.  result is the operand to use for traverse
createuop =: 3 : 0
NB. The operand is a box, containing the (shape);(selectors from previous levels)
NB. Sinze the selresult starts a new chain of selectors, the initial previous selectors is always empty
<selresultshape ; $0
)

NB. Figure out which path, v0 or v1, created an error
NB. Called only during snifferror
NB. x is locale of v0, y is locale of v1
NB. We figure out which path is responsible for the error, and
NB. clear the failingselector in the other path, so the other path will never signal error
isolateverror =: 4 : 0
NB.?lintonly x =. y =. <'dissectobj'
NB. If both paths have a hard error, mask one of them
NB. Because v0 and v1 are called with exactly the same ranks (they appear either in
NB. &, which has rank mv mv, or fork, which has rank _ and therefore the same rank for u and v),
NB. we can count the selresults from each verb and figure that the failure occurred in whichever
NB. one was executed less often.  If they are executed the same number of times, finger v1, since
NB. v1 is executed first.
if. (errorcode__x,errorcode__y) *./@:e. EEXEC,EABORTED,ENOAGREE do.
  NB. Among the hard error codes, there is a severity order.  ABORTED/NOAGREE fail before
  NB. executing anything, and are worse than EXEC (and ABORTED/NOAGREE have meaningless values for selresult).
  NB. If both have ABORTED/NOAGREE, fail y; if only one does, fail that one; otherwise fail the
  NB. one with fewer results, y if tied
  okloc =. x   NB. set default
  select. (errorcode__x,errorcode__y) e. EABORTED,ENOAGREE
  case. 0 0 do.  NB. EXEC,EXEC
    if. selresult__y <&# selresult__x do. okloc =. y end.
    NB. v1 executed more often, or the same number of times.  Disable error in v0
  case. 1 0 do.  NB. NOAGREE,EXEC
    failloc =. x
    NB. EXEC,NOAGREE   and NOAGREE,NOAGREE fail y 
  end.
  failingselector__okloc =: 0$a:
end.
0 0$0
)

NB. null rankcalculus for cases where we can take no action
NB. y is selector,operands for the next operation
NB. result is replacement selector,operands.  We know that the result did not collect,
NB. so the result must discard the operands unless we are able to say something about
NB. what this verb does with them.  Here we discard the operands; if the verb can do better,
NB. it will override this verb
rankcalculus =: {.
NB. y is vlocale (or v1locale)
NB. x, if given, is v0locale
NB. Result is operands to pass on to traversal of u
NB. If v had an error, we wipe out the selector and the operands, and reset the error code in the
NB.  calling locale to suppress its error display
NB. If v didn't collect, we don't use its operands
travdownuops =: 3 : 0
NB.?lintonly y =. <'dissectobj'
if. errorcode__y > EOK do.
  , bnsellevel , NORANKHIST
else.
  bnsellevel , NORANKHIST , rankcalculus^:(-. collected__y) selector , createuop__y ''
end.
:
NB.?lintonly x =. y =. <'dissectobj'
NB. In the dyad, x is v0 and y is v1
if. (errorcode__x , errorcode__y) +./@:> EOK do.
  , bnsellevel , NORANKHIST
else.
  bnsellevel , NORANKHIST , rankcalculus^:(collected__x *: collected__y) selector , (createuop__x '') , (createuop__y '')
end.
)

NB. x tells which ranks to use: must be a list or a:
NB. y is the selector + other operands
NB. Result is y, with sellevel and ranks for v
NB. we pull the ranks according to x (ranks are textstring;level;left[;right])
travdownvops =: 4 : 0
bnsellevel , (< x {"1 rankhistory) , y
)


NB. **************** code for display objects *********************

cocurrent 'dissect'

NB. ****************** place-and-route for wires ***********************

ROUTINGGRIDSIZE =: 5   NB. number of pixels between routing channels
WIRESTANDOFF =: 4  NB. min number of pixels between a wire and a block
ROUTINGTURNPENALTY =: 3   NB. number of blocks of penalty to assign to a turn

ROUTINGMARGIN =: 2   NB. min number of wire spacings to leave around border.  Used to calc routing area

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
initgrids =: 3 : 0
NB.?lintonly 'gridsize standoff penalties' =: 5 5 5 [ 'gridblocks nets' =: (0 2 2$0);<0 $a:
rareasize =: (standoff + gridsize * >: ROUTINGMARGIN) + >./ {:"2 gridblocks
NB. Create top-left,:bottom-right+1 in grid units for each block
gscblocks =. <. yxtogrid gridblocks +"2 (2) #"0 (gridsize-standoff),standoff+gridsize
NB. Create the row;column vector for each block
gscrowcolvec =: (+ i.)&.>/@(-~/\)"2 gscblocks
NB. Negative occupancy indicates which object occupies the cell
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
'gridblocks nets' =: y
NB. Create verb to convert input y,x to grid coordinates
initgridverbs gridsize
NB. Calculate the routing area size.  Adjust blocks to leave a minimum top/left margin,
NB. and create the routing area to leave a right/bottom margin
gridblocks =: gridblocks +"1 ([: <. 0 >. ROUTINGMARGIN&-)&.(%&gridsize) (<./ {."2 gridblocks) - WIRESTANDOFF
initgrids''
NB. Route the nets, building up occupancy as needed
1 routenets gridblocks;<nets
NB. As a perf boost, skip the placement adjustment if there are no spots occupied more than once
if. 1 +./@:< , occupancy do.
  NB. Move the blocks to leave extra space where the occupancy exceeds 1
  NB. Look at the ew occupancy.  Create a state machine to process from the bottom up.  Result is the
  NB. number of bump-ups needed at each position; reset when each new object is encountered.  So the
  NB. number of bump-ups is in the cell BELOW the bottom of the object
  bumpups =. ((0 <: [) * (+<:))/\. 1 {"1 occupancy
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
  bumpups =. ((0 <: [) * (+<:))/\.&.|: 0 {"1 occupancy
  bumpsinlastobj =. ({.$occupancy)$0
  bumpsinobjew =. (#gscrowcolvec) # 0
  for_b. \: gsccolplus1 =. >: {:@(1&{::)"1 gscrowcolvec do.
    rows =. (<b,0) {:: gscrowcolvec
    bumpsinlastobj =. (bumps =. 0 >. >./ ((<rows;(b { gsccolplus1)) { bumpups) + rows { bumpsinlastobj) rows} bumpsinlastobj
    bumpsinobjew =. bumps b} bumpsinobjew
  end.
  NB. Now move the blocks the specified number of grid positions - by moving all the OTHER blocks down (to avoid negative placement)
  gridblocks =: gridblocks +"1"2 1 gridtoyx (-"1~ >./) bumpsinobjns,.bumpsinobjew
end.

NB. Reinit the grid variables
initgrids''

NB. Route again, this time to create the final routing
wires =. 0 routenets gridblocks;<nets
NB. Return the block placement and the wires to draw
({."2 gridblocks);wires
NB.?lintsaveglobals
)

NB. lookup to convert a face number into fetch indexes to get start,end
NB. obsolete NB. We will fetch those indexes at rank 1 from a list of ystart,xstart,yend,xend
NB. obsolete NB. to fetch pt1y,pt1x,pt2y,pt2x
NB. obsolete facex =: _4 ]\ 0 1 0 3   2 1 2 3   0 1 2 1   0 3 2 3
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
  NB. obsolete nets =. (1&{"1 ,.~ 2&{"1 (2&{.@] + (* _2&{. - 2&{.))"0 1 (,"2 G__ =: gridblocks) {~ [: <"1 (0&{"1) ;"0 1 facex {~ (1&{"1))&.> N__   =: nets
end.

NB. Route the nets, one by one.  Accumulate the wires
for_n. nets do.
  nt =. >n
  NB. Convert any directly-drawable wires to wires
  ddrawmsk =. (source=.{.nt) directdrawok dests =. }. nt
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
NB. Route one net
NB. y is a net (table of y,x,face)
NB. Globals in use: net number; trial flag; gridblocks
NB. Result is modified routing grid;table of lines
dirtodistx =: (_2 ]\ 1 0 _1 0 0 1 0 _1)&i.
NB. the possible turn directions from each direction index
turntable =: 2 2 2 $ 1 0 _1 0    0 1 0 _1
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
NB. in the face direction, it can function as the true end-of-route.  The routing position, which must
NB. be a gridpoint, is the closest gridpoint on the boundary or outside.  This will necessarily have
NB. the same parallel coordinate as the true routeend, but the other coordinate may be adjusted.
trueroutend =. <. 2 {."1 y
routend =. <. yxtogrid trueroutend + (gridsize-1) * 1 bwand  faces =. 2 {"1 y
NB. obsolete routend =. <. yxtogrid (rawpoints =. 2 {."1 y) + (faces =. 2 {"1 y) { gridsize * 0 0 , 1 0 , 0 0 ,: 0 1
NB. obsolete trueroutend =. ((,. -.) faces > 1) {"0 1"1 2 rawpoints ,."1 gridtoyx routend
NB. Set the distance to all points as high-value, to allow moving to that boundary point.  Use a flag value so we know when we ended
NB. Record a distance of 0 to the source, so that our initial condition is, we just moved to the source point
routdist =. ((#routend) {.!.2000000 ,: 4 $ 0) (<"1 routend)} routdist

NB. Set the source as the current point, perpendicular to the face.  Clear list of waiting points
actpoints =. ,: ({. routend) ,: ({. faces) { faceperpdir
waitpoints =. turnpenalty $ < 0 2 2 $ 0

NB. Repeat until all destinations have been routed:
currdist =. 1
ndestsfound =. 1  NB. the starting point has automatically been routed
while. ndestsfound < #routend do.
  NB. For each active point/direction, create the turn points/directions
  turns =. < ,/ (+/"2 actpoints) ,:"1 ((<a:;1;0) { actpoints) { turntable
  
  NB. Activate delayed points that have come to life
  if. #actpoints =. ~. actpoints , 0 {:: waitpoints do.

    NB. Advance each active point to the next position
    actpoints =. +/\."2 actpoints

    NB. Fetch distance to target.  Delete next-points that are have been filled in this direction
    targx =. ({."2 ,. dirtodistx@:({:"2)) actpoints
assert. 1 4 e.~ 3!:0 targx
    valmsk =. currdist <: targval =. targx (<"1@[ { ]) routdist

    if. 2000000 e. targval do.
      NB. We reached a destination.
      NB. See which destinations we reached.  Make sure each destination is reached only one time
      reachdests =. (#~ ~:@:(2&{."1)) targx #~ targmsk =. targval = 2000000
      NB. Set the current distance in one destination point; mark all the other parts of the
      NB. destination as regular points so we don't reach them again
      routdist =. 1000000 (<"1 }:"1 reachdests)} routdist
      routdist =. currdist (<"1 reachdests)} routdist
      NB. Count the number of destinations filled.
      ndestsfound =. ndestsfound + #reachdests
      NB. Remove the destination points from the active list
      valmsk =. valmsk *. -. targmsk
    end.

    if. #actpoints =. valmsk # actpoints do.
      NB. Mark the next-points as filled at this step and keep them on the active list
      routdist =. currdist (<"1 valmsk # targx)} routdist

    end.
  end.

  NB. Add turns to the waiting queue
  waitpoints =. (}. waitpoints) , turns

  NB. tick the clock
  currdist =. currdist + 1
  assert. currdist < 1000
end.
NB. Starting at each dest, extract lines for all nets that were drawn, and mark occupancy for the net
wires =. 0 5 $0
for_d. }. routend,.faces do.
  NB. get the starting position / direction for the wire
  dir =. (2{d) { faceperpdir
  wirestart =. currpos =. 2 {. d
  NB. initialize the distance that got us to the endpoint
  currdist =. <./ (<wirestart) { routdist
  while. currdist > 0 do.
    ew =. 0={.dir  NB. current direction of movement
    NB. Indicate that we have moved to the place we have just stepped into
    routdist =. 0 (<currpos,dirtodistx -dir)} routdist
    NB. Note that also in the occupancy table
    occupancy =: (>: (<currpos,ew) { occupancy) (<currpos,ew)} occupancy
    NB. Find the direction of next movement.  If we can continue forward into an empty cell,
    NB. do so; otherwise see if we can turn; otherwise plow forward (must be a trial route with overlap)

    NB. See if the current wire can be extended.  If so, do so and continue
    if. currdist (> *. 0 <: ]) newdist =. (<(nextpos =. currpos + dir),dirtodistx -dir) { routdist do.
      currpos =. nextpos
      currdist =. newdist
    else.
      NB. Can't go stright, must be a turn.  Find the turn direction
      NB. Out the wire for the straight part, if any
      if. currpos -.@-: wirestart do. wires =. wires , (gridtoyx wirestart,currpos) , 0 end.
      ew =. -. ew   NB. switch direction
      turnpos =. (currpos + dir) +"1 ddir =. |."1^:ew 1 _1 ,. 0
      turndist =. (turnpos ,"1 0 dirtodistx -ddir) (<"1@[ { ]) routdist
      olddir =. dir   NB. Save old direction for canonicalizing turn
      dir =. (turnx =. >/ (turndist<0)} turndist ,: 1000000) { ddir
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
    end.
  end.
  NB. Out the last wire if any, after appending the true endpoint
  if. currpos -.@-: wirestart do. wires =. wires , (gridtoyx wirestart,currpos) , 0 end.
end.

NB. Append wires to connect the true endpoints to the routing endpoints
wires =. wires , (gridtoyx routend) ,. trueroutend ,. 0
NB. Return the lines
wires
)

NB. display occupancy
occ =: 3 : 0
if. -. ifdefined 'trialroute' do. trialroute =. 1 end.
if. trialroute do.
  ((0 <: <./) {  '*' , ":@(>./))"1 occupancy
else.
  ((0 <: <./ ) { '*' , ' -|+' {~ [: #. 0&>.)"1 occupancy
end.
)
NB. display route
rout =: 3 : 0
  n =. ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 0&{)"1 y
  s =. ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 1&{)"1 y
  e =. ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 2&{)"1 y
  w =. ('*0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ([ {~ 0 >. ] <. <:@#@[) 3&{)"1 y
  n ,"1 '   ' ,"1 s ,"1 '   ' ,"1 e ,"1 '   ' ,"1 w 
)

NB. *************** end of router - start of display-object management **************
SCROLLBARWIDTH =: 14  NB. width of scrollbar in pixels
SCROLLBARCOLOR =: <192 192 192   NB. color for scrollbar - no pen
SCROLLBARENDCOLOR =: <240 240 240
SCROLLBARENDTHICKNESS =: 10
SCROLLBARTRAVELERCOLOR =: <128 128 128

NB. for the verb-name cell
VERBCOLOR =: 114 30 30
VERBTEXTCOLOR =: 255 255 255
VERBFONT =: '"Arial"'
VERBFONTSIZE =: 0
VERBMARGIN =: 1
NB. for noun body, top level
NOUNCOLOR =: 200 200 255
NOUNTEXTCOLOR =: 0 0 0
NOUNFONT =: '"Arial"'
NOUNFONTSIZE =: 0
NOUNMARGIN =: 1
NB. for status messages
STATUSCOLOR =: 255 0 0
STATUSTEXTCOLOR =: 255 255 255
STATUSFONT =: '"Arial"'
STATUSFONTSIZE =: 2
STATUSMARGIN =: 1
NB. for the user's sentence
SATZCOLOR =: 190 190 190
SATZTEXTCOLOR =: 0 0 0
SATZFONT =: '"Courier New"'
SATZFONTSIZE =: 10
SATZMARGIN =: 1
NB. for the shape, top level
SHAPEFONT =: '"Courier New"'
SHAPEFONTSIZE =: 0
SHAPEMARGIN =: 1
NB. for data, except top level
DATAFONT =: '"Courier New"'
DATAFONTSIZE =: 0
DATAMARGIN =: 1
NB. The shape colors/textcolors give the main data colors for the selection level
SHAPECOLORS =: ".;._2 (0 : 0)
200 200 255
153 255 000
204 255 000
255 255 000
255 204 000
255 154 000
255 104 000
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
NB. The last elements of DATACOLORS are the special types:
NB. _3=unexecuted, _2=error, _1=fill
DATACOLORS =: DATACOLORS , _3 ]\ 255 192 203  255 0 0  0 255 255
DATACOLORS =: <. ,/ DATACOLORS  *"1/ 1 1 1 ,: 0.83 0.83 0.94

DATATEXTCOLORS =: 0 0 0"1 DATACOLORS



FRINGECOLOR =: (220 220 220 , 200 200 0 , 255 0 0 ,: 255 255 255) ;"1 (0 0 0 1)   NB. color/border of fringes: in order label,shape,status,data

DOBORDERCOLORS =: _3 ]\ 0 0 255 0 0 255  0 0 0  255 0 0   NB. Black border for box, but red if incomplete, blue if empty
RANKCOLOR =: 0 220 128  NB. color of dashed line for high-rank ops
BOXBORDERCOLOR =: 0 0 0  NB. color of lines between boxes
HIGHLIGHTBORDERSTYLE =: 0 0 0 2  NB. color,width of lines for highlight
WIRECOLOR =: 0 0 0   NB. Color of wires

BOXMARGIN =: 2   NB. Space to leave around boxed results

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

NB. y is a dol (i. e. valueformat)
NB. result is hw of the DOL.  This is the maximum of x and y, plus a margin if
NB. the data is boxed
extractDOLsize =: (((+:BOXMARGIN) * 3 < #) + {:@>@(1 2&{))"1

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
NB.  valueformat is shape;y endpixels;x endpixels[;subnouns]
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
NB. obsolete 'DOcellsize DOfillatom' =: checkframing selresult
NB. obsolete dispvalue =. <"0@(DOcellsize&{.)@>`>@.(#DOfillatom) frame $!.(<DOcellsize $ {.!.' ' DOfillatom) selresult
NB. obsolete valueformat =: dispvalue createDOL formatinfoused  NB. top result is boxed
(1 {:: (DOframinginfo =: checkframing selresult) frameselresult '') createDOL formatinfoused  NB. top result is boxed
NB.?lintsaveglobals
:
NB. The dyad does the work, and calls itself if the value is boxed.  The dyad returns
NB. valueformat, which is (unused);y endpixels;x endpixels[subDOLs]
'font fontsize margin' =. y
value =. x

NB. obsolete NB. If the noun is too big, cut it down
NB. obsolete NB. This is just to make sure we don't try to format a gigantic noun.  After we have the exact sizes,
NB. obsolete NB. we will trim the shape to the size needed.
NB. obsolete NB. maxyx is in pixels.  We assume that an atom codes as a single 7x9 character (worst case) and we trim to fit in
NB. obsolete NB. maxyx
NB. obsolete value =. (maxyx % 9 7) (] {.~ ((] ([ <. ({.~ -@#)~) [: >.@, [: %~/\. [ ,~ _2 |.\&.|. ]) $@])) value
NB. If the noun is empty, we can't very well calculate its display size,
NB. so just use a canned size
if. 0 e. $value do.
  'subDOLs rcextents' =. (0$a:);<EMPTYEXTENT
else.
  NB. If the noun is boxed, get a DOL for each box; extract the height/width from it
  if. 32 = 3!:0 value do.
    hw =. extractDOLsize@> > subDOLs =. < createDOL&y&.> value
  else.
  NB. If the noun is not boxed, just get the height/width for each atom
  NB. We also come here for the top level, which is boxed because it might not collect
    subDOLs =. 0$a:  NB. no subnouns unless boxed
    glfont font , ": fontsize
    hw =. (+:margin) +"1 |."1 glqextent@":@> value
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
  NB. obsolete rcextents =. (*/\.@:}.&.> rcshapes)   (] + [: +/\ 0 (_1)} [: +/ #@] ($   1 {.~ -)"0 [)^:(*@#@[)&.>   rcextents
  rcextents =. +/\&.> bdynos +&.> rcextents

NB. obsolete   rcextents =. rcextents ([ {.~ 1 >. I.)&.> maxyx
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
if. #y do. DOcfm =: y end.
'cfmlabel cfmshape cfmstatus cfmdata' =. DOcfm
SM^:DEBDOL 'createDOvn: ' , > coname''
NB. If stealth verb, there is no display; but because of inheritance and suppressed detail, we might have the stealthoperand flag
NB. set in a locale that is creating a noun; we'd better create that.  We detect nouns, as usual, by absence of handles in
if. (dispstealthoperand e. 1 2) *. (*#displayhandlesin) do. dispstealthoperand return. end.

NB. We need a graphics object selected so we can check text sizes.  We use the
NB. main form, because the isigraph may not be opened on the explorer yet
wd 'psel ' , winhwnd__COCREATOR
glsel 'dissectisi'

NB. Create the top line: name (if any), flanked by rank(s) (if any)
NB. We are creating one box that will describe the top line
NB. Get size of verb/name string, plus margin
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
NB. obsolete   nonemptylevrank =. ([^:(*@#@>@[)/\. {."1 displaylevrank) (<a:;0)} displaylevrank
  nonemptylevrank =. (#~   (<'') ~: {."1) displaylevrank 
  rolledlevrank =. <./@,&.>/\. &.(,&(<_)) &.|. 2 }."1 nonemptylevrank
NB. obsolete   newrankmsk =. (~: *. 1 (0}) [: -. (<_)&(*./@:="1)) rolledlevrank =. (a: = 2 }."1 nonemptylevrank)} rolledlevrank ,: <' '
  newrankmsk =. 1:"_1 rolledlevrank =. (a: = 2 }."1 nonemptylevrank)} rolledlevrank ,: <' '
  DOranklevels =. (_4 + #cfmshape) <. newrankmsk # > 1 {"1 nonemptylevrank
  DOranks =: (":&.> newrankmsk # rolledlevrank) (}:"1@[ ,. ] ,. {:"1@[) newrankmsk # {."1 nonemptylevrank
  DOrankcfm =: 1 0 1 {"2^:(3={:$DOranks) cfmlabel ,:"1 (DOranklevels { cfmshape)
  rankrects =. DOrankcfm sizetext"1 0 DOranks
  NB. Make the left rank left-justified, the right rank right justified.  Align each stack
  NB. vertically({."1 displaylevrank) ({."1@] ,. [ ,. }."1@])
  namedesc =. (<ALIGNCENTER) addalignmentgroup (ALIGNLEFT,ALIGNLEFT) addalignmentrect DOrankcfm sizetext"1 0 DOranks
end.


NB. Account for error string, if any; 0 0 if none
NB. If we are not in a try block, allow display of error only at the place where the error was detected
NB. during sniff.  This handles the case where the user makes a selection after sniff, and then there is
NB. no error detected at the point of error, and the enclosing conjunction shows its error.
if. errorwasdisplayedhere +. *errorlevel do.
  select. errorcode
  case. ENOSEL;ENOOPS;EOK;EUNEXECD;EPREVERROR;ENOUN do.
    DOstatusstring =: ''
  case. ENOAGREE;EFRAMING do.
    DOstatusstring =:  ('agreement';'framing') {::~ (ENOAGREE,EFRAMING) i. errorcode
  case. do.
    DOstatusstring =:  (*errorlevel) {:: errormessagefrominterp;'error'
  end.
else.
  DOstatusstring =: ''
end.

if. #DOstatusstring do.
  NB. If this failure is in a try path, parenthesize the error
  if. errorlevel do. DOstatusstring =: '(' , DOstatusstring , ')' end.
NB. obsolete   glfont ; ":&.> 2 3 { cfmstatus
NB. obsolete   thw =. (+: 4 {:: cfmstatus) + |. glqextent DOstatusstring
  thw =. cfmstatus sizetext <DOstatusstring
else. thw =. 0 0
end.
statusdesc =. ALIGNSPREAD addalignmentrect thw

NB. Create the shape/selector line if we can
NB. We get this from the shape of the result, unless there is an error; then we get it from
NB. accumframe, which gets us the frame (we don't know the full shape)
NB. We create this line if there are selections (even if there is an error) or if
NB. there is a shape (from selresultshape if no error, or accumframe if error).  There will
NB. always be a shape EXCEPT for agreement error, which doesn't execute
NB. The result shape is valid if it collected with meaningful execution
if. normalresult =. collected *. (0 < #selresult) *. -. errorcode e. ENOAGREE,EABORTED do.
  shapetouse =. selresultshape
else.
  shapetouse =. afflat accumframe   NB. delete unexpanded detail
end.
NB. If we have no shape, either it's a scalar, or we have empty frame with unknown shape.
NB. in both cases, it's OK to elide the shape/selector line.
if. #shapetouse do.
  NB. allocate the shape into boxes whose lengths match the lengths of the nonempty boxes of accumframe
  NB. The last box of DOshapes (possibly empty) is the part of the shape that is not in the frame,
  NB. in other words the shape of the (filled) result cell.  This will be drawn in a special color, and
  NB. no selection will be drawn below it.
NB. assert. shapetouse >:&# ;accumframe  not valid, when extra internal ranks, such as from u/, are possible
NB. obsolete   DOshapes =: ,: (1 ,~ ; {.&1&.> #@> accumframe) (({.~ #) <;.1 ]) shapetouse
  DOshapes =: ,: (afkey afact accumframe) ([ <;.1 ({.~ #)~) shapetouse  NB. Remove last box of accumframe if neg
  NB. If there are selections, line them up under the boxes in the shape containing selectable values
  NB. (i. e. more than one cell)
  if. sellevel < #selections do.
    DOshapes =: DOshapes , (sellevel }. selections) (] #^:_1 ({.~ +/))"1 (1 < */)@> DOshapes
  end.
  NB. append the result cell.  If there are selections this will be duplicated; no problem, since it is discarded for display
  if. normalresult do.
    NB. when we have a shape, use it to calculate the shape of the result cell
    DOshapes =: DOshapes ,. < (afcount accumframe) }. shapetouse
  else.
    NB. If we are using a frame rather than a shape, use result shape of '...'
    DOshapes =: DOshapes ,. <'...'
  end.
NB. obsolete    DOshapes =: DOshapes ([ ,. #@[ {. ]) '...';''
NB. obsolete  NB. If, after all this, we had a scalar (empty shape), replace it with 's'
NB. obsolete  if. 0 = {: $ DOshapes do. DOshapes =: ,: <'s' end.
  NB. Convert the shapes to characters, and get the pixel extent of each string.  Start at the selection level
  NB. of this object
  shapeext =. ((sellevel + i. {:$DOshapes) ((<. _4 + <:@#) { ]) cfmshape) sizetext"1 0"_ 1 DOshapes =: ":&.> DOshapes
  NB. Create a rect object for the shape/selections
  shapedesc =. (<ALIGNCENTER) addalignmentgroup (<ALIGNSPREAD)&addalignmentgroup@,."1@|: ALIGNSPREAD addalignmentrect shapeext
else.
  shapedesc =. 0 addalignmentrect 0 0
end.
NB. Agreement error is special: it formats the status above the verb, it aborts traversal,
NB. and it suppresses the display of data.  Go ahead and
NB. handle that here.
'DOlabelpos DOshapepos DOstatuspos DOdatapos displayscrollbars' =: <,:0  NB. make sure all names defined
if. errorcode = ENOAGREE do.
  NB. Create rectangles for status and verb, and stack them, expanding the status line if needed
  picknames =: 'DOstatuspos DOlabelpos'
  arects =. ,: , alignrects > (ALIGNLEFT;ALIGNCENTER) addalignmentgroup statusdesc ,: namedesc
NB. format the DOL if any
elseif. normalresult do.
  NB. The case of no results can happen here only if we have EUNEXECD which we passed through
  NB. because of previous error.
  NB. If there are ranks, create rectangles for each and stack vertically.
  NB. Join the string and the ranks to create the top line
  NB. Get size of shape string, plus margin.
  NB. shape of result is shape of fillmask; box according to shapes of selectors
  NB. Stack vertically: string/rank,shape,status,data

  NB. Find the sizes to display: main, and explorer if allowed.  A table of 1 or 2 rows
  hwtable =. calcformsize valueformat =: createDOL ((<_1;2 3 4) { cfmdata)

  NB. If data doesn't fit in the allocated area, append scrollbars as needed.  We install the
  NB. bars here; the endpoints and traveler are added when the box is drawn
  if. +./ displayscrollbars =: hwtable <"1 extractDOLsize valueformat do. hwtable =. hwtable +"1 SCROLLBARWIDTH * |."1 displayscrollbars end.

  datadesc =. ALIGNCENTER addalignmentrect hwtable
  picknames =: 'DOlabelpos DOshapepos DOstatuspos DOdatapos'
  arects =. ,@:alignrects@:>@:((ALIGNLEFT;ALIGNCENTER)&addalignmentgroup)"_1 (namedesc , shapedesc ,: statusdesc) ,"_ _1 datadesc
elseif. do.
  NB. Create rectangles for verb and status, and stack them, expanding the status line if needed
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
(picknames =: ({.pickok) # ;: picknames) =: |: pickok #"_1 arects
NB.?lintmsgson
NB. There should always be SOMETHING to display (shape, label, or data), but during sniff we don't allow
NB. any space for the data, which could leave us with nothing.  Make sure we have a valid DOsize then
DOsize =: 2 2&>.@{:@brect"_1 pickrects

NB. obsolete DOhandlepos =: displayhandlesin */ 0 (0}) DOsize

NB. If selections have changed such that this locale cannot raise an explorer, delete any old one that exists
if. 2 > #DOsize do. destroyexplorer '' end.

NB. Set the initial scrollpoints, in case the noun is big
scrolltravelers =: (2 2$0)"1 scrollpoints =: initscrollpoints''

NB.?lintonly 'DOlabelpos DOshapepos DOstatuspos DOdatapos' =: <2 2 $ 0
NB.?lintonly 'DOranks DOranklevels DOshapes' =: ($0);($0);<0$a:
0  NB. object created, say so
NB.?lintsaveglobals
)

NB. y is a brick of rectangles yx,:hw, or an array of boxes containing rects at some level,
NB. or a single rect (which becomes the bbox)
NB. Result is bounding rect, a single yx,:hw
brect =: (<./@:({."2) ,: >./@:({:"2))&.:(+/\"2)^:(2<#@$)@(>@:(<S:0)^:(0<L.))


NB. x is alignment(s) for a single rectangle
NB. y is hw
NB. result is boxed hw,:alignment
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
NB. obsolete   bbox =. (+/@:(>./"1)@:({."1) , +/@:(>./)@:({:"1)) bonly
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

cocurrent 'dissect'

NB. ********** draw DOs

NB. x is pen color,width[,style]
NB. y is table of yx.:yx
drawline =: 4 : 0
if. DEBGRAF do.
  'Lines: color=%j, width=%j, style=%j, xywh=%j' printf (3{.x);(3{x);(4}.x); }: ; '((%j,%j)-(%j,%j)),' vbsprintf ,"2 |."1 y
end.
glrgb 3 {. x
glpen 2 {. 3 }. x
gllines ,"2 |."1 y
0 0$0
)

NB. x is color,width[,pen] either a list or a table for each line
NB. y is (list of starting y);(list of starting x),:(x start/end positions for y lines);(y start/endpositions for x lines)
drawmesh =: 4 : 0
x drawline"1 3 (, |."1)&>/ ,."0 1&.>/ y
)


NB. x is interior color;(pen color,width).  If pen is omitted, null is used
NB. if color is empty, use null brush
NB. y is yx,:hw of rectangles to draw with that color
drawrect =: 4 : 0
if. 0 e. $y do. return. end.
NB. obsolete y =. (#~ [: -. 0 e."1 {:"2) y
ic =.> {. x
if. DEBGRAF do.
  'Rectangles: color=%j, pencolor=%j, xywh=%j' printf (2{.x), }: ; '((%j,%j)-(%j,%j)),' vbsprintf ,"2 |."1 y
end.
if. 1 < #x do.
  (([: glpen 0 ,~ {:) [ glrgb@}:) 1 {:: x
else.
  NB. No color, no pen
  (([: glpen 0 5"_) [ glrgb) ic
  glpen 0 5
end.
if. #ic do.
  glrgb ic
  glbrush ''
else. glbrushnull''
end.
glrect 0 0 1 1 +"1^:(-.IFQT) ,"2 |."1 y
0 0$0
)

NB. x is (background color[;line color]);text color;text font;text size;yx margin around text (scalar or yx)
NB. y is text;yx,:hw of box
NB. Draw the rectangle, then draw the text
drawtext =: 4 : 0"1
'vc tc tf ts mg' =. x
NB. Draw the rectangles
(<vc) drawrect > 1 {"1 y
if. DEBGRAF do.
  'Text: colors=%j/%j, font=%j%j, xy=(%j,%j), text=%j' printf vc;tc;tf;ts; (<"0 |. mg + {. 1 {:: y) , (0 { y)
end.
NB. Select font & color
glrgb tc
gltextcolor''
glfont tf , ": ts
NB. Draw the strings, offset by the margin.
(gltext@[   [: gltextxy@|. mg + {.)&>/ y
)

NB. same parms as drawtext, except for the text boxsize
NB. Result is the hw of the box needed
sizetext =: 4 : 0"1
'vc tc tf ts mg' =. x
if. DEBGRAF do.
  'Sizetext: colors=%j/%j, font=%j%j, text=%j' printf vc;tc;tf;ts; (0 { y)
end.
glfont tf , ": ts
NB. Add margins all around, return in hw format
(+:mg) + |. glqextent >{.y
)

cocurrent 'dissectobj'

EXPLORERYX =: 0 0   NB. The place on the explorer isigraph to start the display
NB. Draw the main object, and also the explorer if it is active
NB. If x is given, it selects the view(s) to draw
drawDOvnall =: 3 : 0
a: drawDOvnall y
:
if. #y do. DOyx =: (#DOsize) {. y ,: EXPLORERYX end.
drawDOvn"1 x { (>:*#explorer) {. ((#DOsize) {. (winhwnd__COCREATOR;explorer),.(0;1),.(1<#DOsize);0) ,. |: <"_1@> DOyx;DOsize;DOlabelpos;DOshapepos;DOstatuspos;DOdatapos;displayscrollbars;pickrects;scrollpoints
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
    NB. DOranks contains [rank],string,rank.  Draw them all
    DOrankcfm drawtext DOranks ,"0 <"2 actyx2 +"2 > DOlabelpos
  end.
end.

NB. draw the shape/selection line, if any
if. shapepresent do.
NB. Draw the shapes/selections.  Start at the selection level of this object
  NB. get the text,position for the shapes/selections, which are a rank-2 array
  shapeseltext =. DOshapes ,"0 actyx2&+&.> |: (,"3) 0 _1 |: > DOshapepos
  NB. draw frame/selections, which are all but the last column
  ((sellevel + i. <: {:$DOshapes) ((<. _4 + <:@#) { ]) cfmshape) drawtext"2^:(*@#@[) }:"2 shapeseltext
  NB. Draw the result-cell shape, the last column of the first row
  RESULTSHAPECFM drawtext (<0 _1) { shapeseltext
end.

NB. draw the status string, if any
if. statuspresent do.
  cfmstatus drawtext DOstatusstring;actyx2 + DOstatuspos
end.
NB. obsolete NB. Calculate the selection level for each cell.  This will be overlaid with a checkerboard pattern to
NB. obsolete NB. give the color number of each cell.
NB. obsolete initsel =. > ({.@[`(<@}.@[)`])}&.>/ (|. (,~&.> [: +: >:@:i.@#) ,~&.>/\.&.|. selections) , (< 0 $~ 0{::valueformat)

NB. Draw the data - if the node has data to display.  If not, we never created a DOL, so don't draw
if. datapresent do.
  assert. 0 = 4!:0 <'fillmask'  [ 'drawDOvn'
  NB. Use the fillmask to give the color for each cell.  Low-order 2 bits are 0=normal 1=fill 2=error 3=unexecd
  NB. Convert that to color # (from bits 1 up) if normal, or _2 _4 _6 otherwise; then add in a checkerboard
  sel =. (FILLMASKSELLEVEL -~ #cfmdata) <. (* (<:FILLMASKSELLEVEL) bwand fillmask)} (_1 bwlsl fillmask) ,&,: (_2 * (<:FILLMASKSELLEVEL) bwand fillmask)
  NB. Checkerboard: works for scalars too.  Create a checkerboard cell of rank no more than 2, then
  NB. replicate as needed for higher rank, so that there is a predictable odd/even pattern within each rank-2 cell
  sel =. sel + (({.   0 1 $~ 1&bwor) ({.~ -@(2<.#)) $ sel)"2 sel

  NB. We must always extend the data to match the frame, so that we show the full operand in case there were
  NB. unexecuted cells.  If the fill atom is nonnull, it means that the result is collectable, and we collect it.  If
  NB. not, we have to show the boxed atoms.
NB. obsolete  dispvalue =. <"0@(DOcellsize&{.)@>`>@.(#DOfillatom) frame $!.(<DOcellsize $ {.!.' ' DOfillatom) selresult
  'frameok dispvalue' =. DOframinginfo frameselresult ''
  NB. After the data is drawn, draw a highlighting rectangle for the item selection(s), if any.
  NB. We take all the selectors there are, up the length of the frame of this level; but if there aren't enough, we don't highlight.  Discard
  NB. selectors for higher levels.  The only way we can get more selectors than frame is during sniff, where the lower
  NB. selection is propagated up automatically
  highlightrects =. (<^:(*@#) (>: afcount accumframe) ((I.~ +/\@:(#@>)) {. ]) sellevel ((< #) # ;@}.) selections) , inheritedselection

NB. obsolete   DOtruncated =: (valueformat;((<"0@>@((;frame)&($!.(<' '))))`>@.DOLcollected selresult);sel;DOLcollected;<cfmdata) drawDOL actyx + DOdatapos
  NB. position the start point so that the selected scroll data starts in the window
  (valueformat;dispvalue;sel;frameok;highlightrects;<cfmdata) drawDOL scrollpoint -~ DOyx + {. DOdatapos

  NB. If there are scrollbars, draw them
  if. +./ displayscrollbars do.
    't l b r' =. , DOyx +"1 +/\ DOdatapos  NB. t l b r of region
    'sh sw' =. (2 * SCROLLBARENDTHICKNESS) -~ 'h w' =. ({: DOdatapos) - SCROLLBARWIDTH * |. displayscrollbars  NB. actual data h/w
    
    datahw =. extractDOLsize valueformat
QP^:DEBDOL 'displayscrollbars DOyx DOdatapos datahw t l b r h w scrollpoint sw sh '
    scrolltravv =. scrolltravh =. 0 0
    NB. draw horizontal scroll
    if. 1 { displayscrollbars do.
      SCROLLBARCOLOR drawrect ([ smoutput) (vpos =. -/\. b , SCROLLBARWIDTH) ,. (l , w)
      SCROLLBARENDCOLOR drawrect ([ smoutput) vpos ,."1 (l , SCROLLBARENDTHICKNESS) ,: (-/\. (l+w) , SCROLLBARENDTHICKNESS)
      scrolltravh =. SCROLLBARENDTHICKNESS + <. sw * 0 >. 1 <. (+/\ (1 { scrollpoint) , w) % (1 { datahw)
      SCROLLBARTRAVELERCOLOR drawrect ([ smoutput) vpos ,. -~/\ l + scrolltravh
    end.
    NB. vertical
    if. 0 { displayscrollbars do.
      SCROLLBARCOLOR drawrect ([ smoutput) (hpos =. -/\. r , SCROLLBARWIDTH) ,.~ (t , h)
      SCROLLBARENDCOLOR drawrect ([ smoutput) hpos ,.~"1 (t , SCROLLBARENDTHICKNESS) ,: (-/\. (t+h) , SCROLLBARENDTHICKNESS)
      scrolltravv =. SCROLLBARENDTHICKNESS + <. sh * 0 >. 1 <. (+/\ (0 { scrollpoint) , h) % (0 { datahw)
      SCROLLBARTRAVELERCOLOR drawrect ([ smoutput) hpos ,.~ -~/\ t + scrolltravv
    end.
    scrolltravelers =: (scrolltravv ,: scrolltravh) hwindex} scrolltravelers
QP^:DEBDOL 'scrolltravelers '
  end.

end.
NB. Draw border rectangles for each component - filling out to the full width of the box
if. #presentx do.
  ((<'') ,. (<presentx;1) { FRINGECOLOR) drawrect"1 2 actyx2 +"2  ({."1 pickrects) ,."1 (0) 0} DOsize
end.

NB. Draw a rectangle for the object, just to get the border.  Color is used to show (no data,NA,all drawn,explorable)
('';((#.datapresent,explorable) { DOBORDERCOLORS),1) drawrect DOyx ,: DOsize

NB. If we drew to the explorer, we must paint it
if. 1 = hwindex do. glpaint'' end.

NB. obsolete NB. After we have drawn the local copy, refresh the grid, if there is one
NB. obsolete if. #grid do. drawgridcontrol'' end.
NB. obsolete 
NB. obsolete 
NB. obsolete DOyx,:DOsize
NB.?lintsaveglobals
)

NB. Draw the graphics for a noun's DOL
NB. x is DOL;values;selection;boxmesh;highlights;cfminfo   selection is replicated if needed
NB.  boxmesh is 1 or 2 if the mesh should be drawn for a boxed operand.  It is 0 only for the
NB.  top-level display of an uncollectable result, which has each atom boxed.  We display the
NB.  atoms but not the boxing.  boxmesh is 0 or 1 for the top level, or 2 for recursions.  We
NB.  use this to fill the rectangles only on the top level
NB. y is yx of top-left corner
NB. We execute the gl2 operations to draw the DOL
drawDOL =: 4 : 0"1
'vf data sel boxmesh hlights cfmdata' =. x
'shapeused ysizes xsizes' =. 3 {. vf
SM^:DEBDOL 'drawDOL: ' , > coname''
NB. If the data is empty, draw nothing (but signal validity).  The size
NB. of the empty was accounted for when the block was created
if. 0 e. $data do.
  0   NB.  valid return
else.
  NB.  If there are subDOLs, adjust the rects to leave a box margin
  boxyx =. y + BOXMARGIN * 3 < #vf
  if. DEBOBJ do.
    'DOL: xy=(%j,%j) xsizes=%j ysizes=%j' printf (<"0 |. y),xsizes;ysizes
  end.

NB. obsolete   NB. Extract only the part we used
  usedd =. data
  NB. Create the rectangles for each atom.  This will be mxnx2x2.
  rects =. (boxyx,:0) +"2 ysizes ,."1/&(|.!.0 ,. 2 (-~/\) 0&,) xsizes
  NB. *** following code copied in grid display ***
  NB. Convert the array, of whatever rank, to a table
  NB. axes is 2 boxes, giving the axis numbers that are assigned to vertical and horizontal.  We assign
  NB. axes alternately, starting from the right, with the last axis always going to x
  axes =. (i. ((#~ -.) ; #~) [: |. $&1 0)@#@$ usedd  NB. 1;0 2   or 0 2;1 3
  NB. axisshapes is the lengths of each axis assigned to y/x.  sizes is the total size of y/x
  sizes =. */@> axisshapes =. axes ({&.:>"0 _ $) usedd
  usedd =. (2 {. $ rects) {. sizes ($,) (;axes) |: usedd
  NB. Extract and reshape the selection information, too.  sel should either be an atom or have
  NB. one atom per data cell.  The data may be truncated, though, so we bring sel up to the
  NB. rank of the shapeused, and then truncate it to shapeused size (using sel as a fill, in case sel
  NB. was an atom).  Then shape to 2D, and trim to the displayable part
  sel =. (2 {. $ rects) {. sizes ($,) (;axes) |: shapeused {.!.({.,sel) ((-$shapeused) {.!.1 $sel) ($,) sel
  NB. Before filling the cells the first time, initialize the rectangles to the colors given by the fillmask.  This
  NB. is to give the right color to cells that are not drawn at all (empty contents) or whose contents do not fill
  NB. the cell, because of other larger values.
  if. boxmesh < 2 do.
    ((<sel;0) { cfmdata) drawrect"0 2 rects
  end.
  NB. If there are subDOLs, process each of them.  The operand was boxed.
  if. 3 < #vf do.
    sdol =. (2 {. $ rects) ($,) (;axes) |: shapeused {. 3 {:: vf
    (sdol ,"0 1 usedd ,"0 (<"0 sel)) (((2;(0$a:);<cfmdata) ,~ [) drawDOL ])"1 {."2 rects
    NB. Draw mesh for the rectangles - unless suppressed
    if. boxmesh do.
      (BOXBORDERCOLOR,1) drawmesh (boxyx&(+&.>) ,: [: |. boxyx +&.> 0 _1&{&.>) ysizes ;&(0&,) xsizes
    end.
  else.
    NB. Not boxed; draw each cell.  If the cell is error/unexecd, delete the text, since the cell
    NB. doesn't really have a value.  We leave its space as a reminder of how big it might have been
    (sel { cfmdata) drawtext"1 ((sel >: _2 * FILLMASKFILL) (# ":)&.> usedd) (,<)"0 2 rects 
  end.

  NB. Draw borders at any boundary (except the first) where a rank rolls over.  The width of the line
  NB. is the number of ranks that rolled over simultaneously.  We see which rectangles start
  NB. on a new boundary, and use the start position to get the line
  NB. Get number of boundaries for each row/col: 0=not a bdy, 1=rank-2 bdy, etc
  NB. Add 1 pixel of width to nonzero boundaries
  bdynos =. axisshapes (+ *)@}.@:(0&(i.&1@:~:)@|."1)@(#: i.)&.> 2 {. $ rects
  NB. Create table of startpoint,width for each line.  Discard first point of bdynos (always a big
  NB. value for the first cell) and the last point of sizes (gives position of the last cell).  We
  NB. are left with internal boundaries.  Back up the position by the width of the boundary, and discard
  NB. zero boundaries
  if. +/ #@> startwidth =. bdynos (*@[ # ,.~)&.> ysizes ;&}: xsizes do.
    (RANKCOLOR ,"1 (0) ,.~ {:"1 ; startwidth) drawmesh (boxyx +&.> {."1&.> startwidth) ,: |. boxyx +&.> (0,{:)&.> ysizes ; xsizes
  end.

  NB. Draw highlighting rectangle(s).  Each rectangle contains a series of indexes.  Draw as a mesh
  NB. We get the border of the rectangle by adding 0/1 to the bottom 2 indexes, then extending with 0
  NB. to full shape, then converting to cell number, then looking that up in the row/column ending table
  if. #hlights do.
    NB. To handle <2 axes, we will add 2 leading 0 axes to the highlight selector.
    NB. We compensate by adding 2 to all the axis numbers, and inserting a leading axis.
    NB. If there are no axes to add to, there are 2 cases: 1 axis, which is ($0);,0: we turn that
    NB. into 1;0 2 - and 0 axis, which we turn into 1;1 - repetition is OK, since the repeated axis is the
    NB. added axis.  We add leading 0 (actual value immaterial) to axisshapes to match the shape
    NB. If the highlight comes from another path, it may be longer than the shape of this operand (that would
    NB. correspond to cell replication) - delete surplus highlight axes
    ext =. (([ ,: +)    1 1 {.~"1 -@#) @ (0 0 , (;axes)&(<.&# {. ]))&.> hlights  NB. 2 vectors: indexes of top-left,:bottom-right
    axed =. (((, 2 + {:!._1)&.> axes)   ([ {&.:>"0 1"1 #@;@[ {."1 ])    ])&.> ext  NB. indexes of top;left,:bottom;right
    rad =. ((0 ,&.> axisshapes)   #.&>"1   ])&.> axed  NB. row/col# for top,left,:bottom,right
    NB. If the highlight comes from a grid, it might be out of the displayable bounds.  When that happens, we want to change the
    NB. value to an offscreen value that will be clipped, so we don't show a partial cell as selected.
    seld =. rad((<."1&.> <@:(#@>))   ({&>"1~ >)~"0 1   (, 100 + {:)&.>@]) ysizes ;&(0&,) xsizes  NB. pixel pos for top,left,:bottom,right unboxed
    mesh =. (boxyx +"2 |:"2) seld  NB. convert to top,bottom,:left,right, adjust for rectangle origin
    NB. Expand to size of axisshapes, split into y and x axes
    NB. Create delta-y and delta-x
    NB. create indexes of ymin ymax ,: xmin xmax
    NB. Convert to pixel numbers
    NB. Adjust for rectangle origin
    NB. Create (ystart,yend);(xstart,xend),:(xstart,xend);(ystart,yend)
    NB. Draw mesh
    HIGHLIGHTBORDERSTYLE (drawmesh   [: (,: |.) ;/)"1 2 mesh
NB. y is (list of starting y);(list of starting x),:(x start/end positions for y lines);(y start/endpositions for x lines)
  end.

NB. obsolete   NB. If there was a rectangle for every value, signal validity
NB. obsolete   ($data) ~:&(*/) 2 {. $rects 
  0
end.
)

NB. hop the boundary from one train to the next by installing this verb's selector (if any) as the highlight of
NB. the verb(s) that feed into it.  We find those verbs by looking at the result locales in the input DOLs.
NB. The selector to use for each operand is the CELL that contributed to the selection; therefore,
NB. we use the frames, and take only as much of the last selector as that frame admits.  This gives the
NB. address of the desired cell; to place that against the operand we have to discard trailing axes that
NB. do not exist for the operand - this emulates cell replication.
NB. y is the DOLs for the input operands
calchighlight =: 3 : 0
if. $selopinfo do.   NB. selection is invalid if this verb didn't collect
  NB. Create a highlight for each operand - if there are selections here
NB. obsolete   propselectors =. (#@> frames) <@;@(}:@] , (((<. #) {. ]) {:))"0 1 (>:sellevel) {. selections
NB. obsolete   if. #accumselect =. ; (>:sellevel) {. selections do.
  if. (*#frame) *. (sellevel < #selections) do.
    accumselect =. ; (>:sellevel) {. selections
    for_l. 0 {"1 ; 3 {"1 y do.   NB.?lintonly l =. <'dissectobj'
      NB. For each operand, the rank of the cell comes from the rank of this verb; the accumulated selection
      NB. gives the address of the cell.  We must preserve only as many trailing axes of the selector
      NB. as exist in the operand, i. e. (operand rank)-(verb rank)
      NB. The referred-to object may not have been dreated for display, so don't refer to formatting variables
NB. obsolete       if. picknames__l e.~ <'DOdatapos' do.  NB. if data was formatted...
NB. obsolete         NB.?lintonly selopselhist =: 2 # <i.6
NB. obsolete         frank =. # 0 {:: valueformat__l  NB. rank of the formatted noun
NB. obsolete         NB. The operand we are selecting from must be at least as big as a cell.  nounrank-verbrank is the
NB. obsolete         NB. length of the noun-frame; we take at most that many leading selections
NB. obsolete NB. obsolete        assert. frank >: l_index { vranks [ 'inheritedselection'
NB. obsolete         if. 1: frank >: l_index { vranks do.
        inheritedselection__l =: inheritedselection__l , l_index { selopselhist
NB. obsolete         end.
NB. obsolete       end.
    end.
  end.
end.
''
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
,: dolpos,(0 2 2$a:);<(,:(coname''),<1,displayhandleout)
)

NB. join layout(s) to the current object
NB. x is table of input layout(s)
NB. y is unused - we create the current object
NB. We join the input objects, then append the DOL for the current object
NB. Result is DOL for the combined layout:
NB.  locale of obj;(start,:size of object);internal wires;handles out
NB.   wires are brick, where each 2x2 is a table of start,:end, each in the format (locale;face#,fractional position)
NB.   handles in/out are a table of startpoints, (locale;single face#,fractional position)
joinlayouts =: 4 : 0
NB. Create the DO for the block to add on.  If stealth operand, just
NB. pass on the selected input layout
assert. 2 = #$x
if. DEBLAYOUT do.
  qprintf'Joinlayouts:loc=?>coname'''' x '
end.
if. 0 ~: createDO'' do.
  if. DEBLAYOUT do.
    smoutput'Stealth object, not created'
  end.
  NB. If the input is a noun result that has had detail removed, it may contain no layouts.  In that case,
  NB. if we pass it to a stealth operand it must make a layout for it  Example: z + ] 3
  if. 0 = #x do. createlayout 0 0 else. (, dispstealthoperand { 0 0 _1) { x end.
else.
  NB. If there are no earlier layouts, this had better be a noun - just create its layout
  if. 0 = #x do.
    createlayout 0 0
  else.
    'upperdol upperyxhw upperwires upperresult' =. joinlayoutslr/ x
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
    NB. Add wires from outputs to inputs
    upperwires =. upperwires , upperresult ,:"_1  (coname'') ,. <"1 (0) ,. displayhandlesin

    NB. Create resulthook, from the result block
    if. DEBLAYOUT do.
      qprintf'Result objects:dol=?upperdol%yxhw=?upperyxhw%wires=?upperwires%res=?(,:(coname''''),<1,displayhandleout)%'
    end.
    ,: upperdol;upperyxhw;upperwires;<lres
  end.
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
NB. The screen size of the layout is {:@$@> 1 2 { layout
NB. If a layout is a reference only, it will have empty DOLtable and margins

NB. Create a reference to a layout
NB. y is the layout
NB. Result is reference layout, as a table
createreference =: 3 : 0
NB. The reference has the same resulthook as the main layout, but no DOLs, pixels, or wires
(((0$a:),&< 0 2 2$0),(0 2 2$a:);3&{)"1 y
)

NB. Create a self-reference to the current object (which may not exist yet)
NB. y is the position(s) of the output wire
NB. Result is reference layout, as a table
createselfreference =: 3 : 0
NB. The reference has the same resulthook as the main layout, but no DOLs, pixels, or wires
(((0$a:),&< 0 2 2$0) , (0 2 2$a:) (;<@,:) (coname'') (,<) 1 , ])"0 , y
)

NB. Create empty layout, as a table
createemptylayout =: 3 : 0
,: ((0$a:),&< 0 2 2$0),(0 2 2$a:);<0 2$a:
)

MINBOXSPACING =: 3 * ROUTINGGRIDSIZE  NB. Number of pixels between boxes, minimum
MAXVERTFLOAT =: 5  NB. Number of gridcells leeway to allow a box to move up to maximize overlap

NB. Join layouts left-to-right
NB. x and y are layouts
NB. Result is composite layout, with all wires & multiple results
joinlayoutslr =: 4 : 0
if. DEBLAYOUT do.
  qprintf'Joinlayoutslr:x?x y '
end.
'ldol lyxhw lwir lres' =. x
'rdol ryxhw rwir rres' =. y
NB. If one of the blocks (or both) is a reference, don't bother moving anything;
NB. just join the (empty) blocks and the results
if. ldol *.&(0~:#) rdol do.
  NB. bottom-justify the blocks.  slacks is how much slack is left at the top of each block.  One
  NB. of these values is 0; round the other to an even number of grids
  slacks =. <.@(0.5&+)&.(%&ROUTINGGRIDSIZE) (- <./) (>./ +/"1 {."1 lyxhw) , (>./ +/"1 {."1 ryxhw) 
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
  lssyrng =. (ROUTINGGRIDSIZE * i. MAXVERTFLOAT) +/ lssy =. ~. lssy   NB. left vert positions, shifted
  rssyrng =. (ROUTINGGRIDSIZE * i. MAXVERTFLOAT) +/ rssy =. ~. rssy
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

NB. Entry point when dol and locale are joined together.  This is called from original traversal,
NB. thus needs to be in outer locale
joinlayoutsl_dissect_ =: 3 : 0
'dol y' =. y
NB.?lintonly y =. <'dissectobj'
dol joinlayouts__y ''
)


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
glpen 1 0
if. #lines do.
  gllines 1 0 3 2 {"1 ,"2 lines
end.
if. #arcs do.
  NB. The arcs are CCW sections given by (center point,corner point).  Expand to arc, which is x y w h xa ya xz yz
  NB. Create the xywh: wh = 2 * corner - center, xy = corner - wh
  yxhw =. ,"2 -~/\@:((-~ +:)~/\.)"2 arcs
  NB. xa ya xz yz is xc yr xr yc if direction of arc in xy has different sign; xr yc xc yr if same sign
  aazz =. ((0 > */"1 -/"2 arcs) { (_4 ]\ 0 3 2 1  2 1 0 3)) {"1 ,"2 arcs
  NB. Kludge add 1 pixel to aa, in the direction away from zz, because the arc is underdrawn
  aazz =. (2&{."1 (] ,"1 [ + *@:-) 2&}."1) aazz
  glarc"1 yxhw ,.&:(1 0 3 2&{"1) aazz
end.

NB. Show the sentence, with the user's spacing, highlighting according to selection level
({.sentencebrect) drawsentence }. usersentence sizesentence gettokenlevels__resultroot ''


NB.?lintsaveglobals
)

NB. **************************** mouse events in the graphics window ********************
NB. in the locale of the main form

NB. x is pick rectangles, YX,;HW
NB. y is y,x
NB. result is table of (index to hit rect);relative y,x in rect    empty if no hits
findpickhits =: 4 : 0
index =. (+/\"2 x) I.@:(*./"1)@:(>/"2)@:(<:"1) y
index ;"0 1 y -"1 (<index;0) { x
)

NB. pick flags
'PICKLB PICKRB PICKCTRL PICKSHIFT' =: |. 1 bwlsl~ i. 4

NB. mouse button, both left and right.  Return number of picks performed.
NB. x is l or r, y is formatted sysdata
dissect_dissectisi_mbdown =: 4 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100'
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
'r' dissect_dissectisi_mbdown 0 ". sysdata
''
)

NB. mouse movement.  If we are scrolling, drag the pixels along
NB. If we are dragging a scrollbar, vector to the object locale to handle that
dissect_dissectisi_mmove =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100'
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
end.
)

NB. mouse release.  If we are scrolling, set the new offset and redraw
NB. If we are dragging a scrollbar, vector to the object locale to finish that
dissect_dissectisi_mblup =: 3 : 0
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

cocurrent 'dissectobj'

NB. x is view number, y is yx (in the window)
NB. Result is (x,y) in the flattened display within which the yx falls
pixeltoflatyx =: 4 : 0
(1 2 { valueformat) I.&> >: (x{scrollpoints) + y
)

NB. y is flatyx, result is index list within the noun
flatyxtoindexlist =: 3 : 0
s =. 0 {:: valueformat   NB. shape of the noun
NB. Split the shapeused into vert;horiz, ending on horiz.  OK to add high-order 0s to
NB. ensure that there is some infix of length 2.
NB. Convert row/col to indexes.  Interleave the indexes for row/col to get cell indexes.  Remove 0
NB. if it was added
(-#s) {. , |: (|. |: _2&(]\)&.|. 0 0 , s) #: y
)

NB. y is y,x position
NB. We process a click on that cell.  We convert the y,x to a cell address, change the selectors for the click,
NB. and redraw the screen
processdataclick =: 3 : 0
selx =. flatyxtoindexlist y
QP^:DEBPICK 'y selx '
QP^:DEBPICK 'accumframe sellevel #selections selections '
assert. (#selx) >: afcount accumframe  [  'pick rank error'
assert. sellevel <: #selections
NB. Extract the part of accumframe that holds valid selectors.  The last box, if negative, holds
NB. the default selector for unexpanded detail
af =. afact accumframe
NB. Split the selection into boxes according to the list of ranks of u operands.
NB. Ignore indexes beyond the accumframe.  Prepend the selections that are inherited from above,
NB. and append the selector for unexpanded detail, if any
bselx =. (sellevel {. selections) , (selx (] <;.1 ({.~ #)) (afkey af)) , (accumframe }.~ #af)
NB. If the new selection is the same as the old, do nothing (for speed).  Consider only the
NB. part of selections up to the length of bselx (there may be a spurious excess after sniff).
NB. bselx is known to contain no empty boxes
if. -. selections (({.~ #) -: ]) bselx do.
QP^:DEBPICK 'bselx selections '
  NB. See how many of the new selections match the old; take all of them, plus one more.
  NB. We know there is a mismatch somewhere before the end of bselx
  NB. Make that the selection in all subnodes
  propsel bselx ([ {.~ [: >: ({.~ #) i.&0@:= ]) selections
  NB. Clear the scroll point in all the nodes for which the selection has changed.  The old scroll point may be invalid
  propscroll 1   NB. 1 causes the scroll to be unchanged in THIS node, cleared to the leaves
  dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
end.
)

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
NILRET
)

NB. **************** end display objects ************************


NB. ***************** explorer control for objects

EXPLORER=: 0 : 0
pc explorer;
xywh 0 0 ?;cc dissectisi isigraph;
pas 0 0;pcenter;
rem form end;
)
EXPLORER=: 0 : 0 [^:IFQT EXPLORER
pc explorer;
minwh ?;cc dissectisi isidraw;
pas 0 0;pcenter;
rem form end;
)

NB. Called in the locale of the object
createexplorer =: 3 : 0
NB. Start the window definition, so we can use existence of 'explorer' to indicate destination
NB. Create the isigraph and finish creating the form
wd ctl   =. '?' (taketo , (": |. -:^:(-.IFQT) {: DOsize) , takeafter) EXPLORER
wd 'pshow'
explorer =: wd 'qhwndp'
NB. Draw the object on the explorer form
1 drawDOvnall ''
)

NB. Use destroy to remove the explorer window without writing to the main form, as for example
NB. when the old explorer is invalid
destroyexplorer =: 3 : 0
if. #explorer do.
  wd 'psel ', explorer
  wd 'pclose'
  explorer =: ''
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
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100'
yx =. EXPLORERYX -~ 1 0 { sd =. 0 ". sysdata
if. *./ yx < {:DOsize do. 'l' pickDO 1;yx;#. 4 5 6 7 { sd end.
)


NB. mouse movement.
NB. If we are dragging a scrollbar, vector to the object locale to handle that
explorer_dissectisi_mmove =: 3 : 0
NB.?lintonly sysdata =. '100 100 100 100 100 100 100 100'
sd =. 0 ". sysdata
if. 0 = 4!:0 <'scrollinglocale__COCREATOR' do.
  NB. Perform the scroll, on this explorer window.  If the scroll was started in a different locale,
  NB. abort it.
  if. scrollinglocale__COCREATOR -: coname'' do.
    1 scrollmmove 1 0 { sd
  else.
    explorer_dissectisi_mblup''
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
NB. We add parentheses if the last word of x and the first word of y are both numeric
jd =: 4 : 0
if. '.:' e.~ {. y do. y =. ' ' , y end.
if. ({: ;: x) *.&('0123456789_' e.~ {.@>) ({. ;: y) do. y =. '(' ([`(' '&(i.&0@:=)@])`(' '&,@]))} y , ')' end.  NB. Replace last space with (
x , y
)

NB. x is height(s) of v, y is height(s) of u
NB. Result is combined height: the sum, but if either operand is _1, result must be _1; or if 0, must be 0.
NB. Monads may have rank 0 or 1, so we atomize all singletons
combineheights =: (0:`+`_1:@.(*@<.))"0&({.^:(1=#))

NB. ******************* class-dependent portion of display and pick support **********************
cocurrent 'dissectobj'

NB. Calculate the size in pixels to allocate on the main form
NB. y is valueformat
NB. result is table of allowed sizes.  If exploring is allowed, the second row is the size for exploring
calcformsize =: 3 : 0
sz =. extractDOLsize y
}:^:(sz -: {.) maxnoundisplaysizes <."1 sz
)

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
pickDO =: 4 : 0
QP^:DEBPICK 'coname'''' y '
'exp yx flags' =. y
for_r. (exp{pickrects) findpickhits yx do.
  'ix pyx' =. r
QP^:DEBPICK 'x ix pyx exp yx flags ix{::picknames '
  if. 3 = 4!:0 <name =. 'pick' , x , ix {:: picknames do. (flags,exp) name~ pyx end.
end.
0 0$0
)

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
  processdataclick exp pixeltoflatyx y
end.
)

NB. right click - if explorable, do the explorer stuff
pickrDOdatapos =: 4 : 0
if. 1 < #DOsize do.
  if. 0 = #explorer do. createexplorer''
  else. explorer_close''
  end.
end.
)

NB. *** traversal support ***
NB. y is natural frame(s) of the executed verb
NB. result is (frame of value to display);(selopshapes for next level - cells of this verb)
calcdispframe =: 3 : 0
selopshapes (] ,&< (}.~ #)&.>) y
)

NB. y is all the indexes that were selected by  the selector
NB. Result is the selectors to display (a list), in order.  The atom count should match the frame returned by calcdispframe
calcdispselx =: ]

NB. y is the result of calcdispselx.  Result is the matching values.  These values will be collected to
NB. produce the displayed result.  We might have to add boxing if we want to ensure collectibility
NB. Result always stays in execution order
fetchselr =: 3 : 0
y { logvalues
)

NB. x is the frame of the full expected result
NB. y is the number of results we actually got
NB. result is index list of the failing location
getfailingindex =: #:

NB. y is the intervals for each ticket, expanded into an array using the shape of the frame
NB. Result is the array reordered to natural order (some primitives process out of order; we reorder to match selection)
tickettonatural =: ]

cocurrent 'dissectnoun'

NB. Inherit pickDO from default object
NB. For all these verbs, x is button flags, y is the yx position of the click relative to start of pickrect

NB. Since labeled nouns are never SDTs, ignore a click on the name
NB. Nouns are either primitives or SDTs.  Clicking on the shape will expand an SDT if possible.  Once expanded,
NB. the display becomes a verb, ad cannot be collapsed.
picklDOshapepos =: 4 : 0
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
end.
0 0$0
)

picklDOdatapos =: 4 : 0
NB. If the display of a noun's detail is suppressed, and it has detail, any click on it will turn on the detail
if. nounhasdetail > nounshowdetail do.
  nounshowdetail =: 1
  dissect_dissectisi_paint__COCREATOR 1  NB. display the updated selection
else.
  NB. If the noun is displaying detail, treat a click in the data area same as for a verb result
  x picklDOdatapos_dissectobj_ f. y
end.
)

cocurrent 'dissectexpandable'  NB. locale for items that expand when name is clicked

NB. Here are the object locales for creating the parse table
NB. Each object is responsible for responding to the entry points:
NB. create - create the locale and return a proper stack line (type;locale)
NB. execute - set the locales of the noun operands of verbs
NB. defstring - return executable string without instrumentation
NB.  the operand gives the context of use: 0=verb or right op of verb, 1=left op of verb, 2=left op of modifier, 3=right op of modifier 
NB. exestring - return executable string, after instrumentation has been added.  Set locale info
NB.  y is (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 if inverse also needed)
NB. value - return value of noun, or result of verb.  For verb, valid only after execution

NB. In addition, verbs have the entry point rank which returns the rank of the verb

NB. The object is responsible for storing its value, its rank, and pointers to subobjects

NB. In the layout of the graphics, we assume that each block produces only one result cell,
NB. on a row by itself,
NB. which will be overlapped with the appropriate input position of the next block

cocurrent 'dissectmonad'
coinsert 'dissectobj'
NB. Monad.  y is the locales of the verb and the noun
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ yop =: coname''
NB. Tell the verb its valence; the result is the operands that are needed for display.  Here, in this non-verb,
NB. we save the operands needed by the first verb.  The rule is, we will pass to a verb ONLY the operands that
NB. it says it can use.  For comp. ease we may compute an operand but then immediately discard it.
setvalence__uop ,resultissdt__yop
resultissdt =: resultissdt__uop
NB. Don't bother fixing estheights, since this is terminal.  No tokens either.
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
''
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , (exestring__uop 0 1 0) , ' (' , (exestring__yop 0 1 0) , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
uop,(yop #~ y > 1)
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
traversedowncalcselect TRAVNOUN  NB. To set globals only - there are no inputs here
dol =. joinlayoutsl x traverse__yop TRAVNOUN
yop inheritu dol traverse__uop travdownuops yop
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displayhandlesin displaylevrank nounhasdetail' =: ($0);NORANKHISTNOUN,<1
  NOLAYOUTS ,&< coname''
end.
)

cocurrent 'dissectdyad'
coinsert 'dissectobj'
NB. Dyad.
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'xop uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ yop =: xop =: coname''
setvalence__uop resultissdt__xop,resultissdt__yop
resultissdt =: resultissdt__uop
NB. Don't bother fixing estheights, since this is terminal
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
auditstg '(' , (logstring '') , '(' , (exestring__xop 0 2 0) , ') ' , (exestring__uop 0 2 0) , ' (' , (exestring__yop 0 2 0) , '))'
)

calcestheights =: 3 : 0
''
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(xop #~ y > 1),uop,(yop #~ y > 1)
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
traversedowncalcselect TRAVNOUN  NB. To set globals only - there are no inputs here
dol =. joinlayoutsl x traverse__xop TRAVNOUN
dol =. dol , joinlayoutsl x traverse__yop TRAVNOUN
(xop,yop) inheritu dol traverse__uop xop travdownuops yop
NB. If detail is turned off, display only the final result
if. -. nounshowdetail do.
  'displayhandlesin displaylevrank nounhasdetail' =: ($0);NORANKHISTNOUN,<1
  NOLAYOUTS ,&< coname''
end.
)


NB. Terminal nouns - names or self-defining terms
cocurrent 'dissectnoun'
coinsert 'dissectobj'
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

NB. obsolete NB. Return value of result
NB. obsolete value =: 3 : 0
NB. obsolete 0 {:: logvalues
NB. obsolete )
NB. obsolete 
calcestheights =: 3 : 0
''
)

proplocales =: 3 : 0
(y = 3) # <tokensource
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: 4 : 0
assert. 0 = #x
traversedowncalcselect y  NB. To set globals, including selresult
'displayhandlesin displayhandleout displaylevrank nounhasdetail' =: ($0);0;varname;0
x ,&< coname''  NB. Return the empty DOLs
)

cocurrent 'dissectverb'
coinsert 'dissectobj'
NB. y is (string form of the verb);rank;tokens it came from
create =: 3 : 0
create_dissectobj_ f. 2 { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand
titlestring =: 0 {:: y
invok =: (1;3) {:: y
stealthoperand =: 1 2 3 0 {~ (;:'][[:') i. <titlestring
NB. Every verb counts as an sdt for modifier processing.
resultissdt =: 1
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Save the number of operands for this invocation
NB. Return value indicates which operands will be used
NB. y is, for each operand, whether the operand is from SDTs
setvalence =: 3 : 0
valence =: #y
resultissdt =: *./y
NB. obsolete NB. [ and ], as dyads, are treated as monads.  Also, they have no display.
NB. obsolete NB. Height of a stealthoperand is 0 to avoid assigning a label to it
NB. obsolete estheights =: 1 * , 1:^:(valence=1) 2 2 #: >: 1 2 i. stealthoperand  NB. 0=] 1=[ 2=normal
NB. Height of a stealthoperand is 0 monadic, 0 _1 dyadic.  Others have height 1
0 0$0
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: (<valence,dispstealthoperand) {:: a: , 2 4 $ 1;0;0;0 ; 1 1;_1 0;0 _1;0 0
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
NB. Apply parentheses if right conjunction operand - but only if more than 1 word
enparen^:((y>2) *. 1 < #@;: titlestring ) titlestring
)

NB. return string form of operands, including instrumentation
NB. y tells what intrumentation is needed:
NB.  (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 for inverse also)

exestring =: 3 : 0
'inp val inv' =. y
NB. Init list of logvbls we will use
vars =. ''
NB. Instrument the forward verb - bivalent
fv =. '(' , (logstring '') , '@(' , (verblogstring '') , titlestring , '))'
NB. If inputs needed, capture them all - for both valences
if. inp do.
  vars =. vars ,'xy'
  fv =. '(' , fv , '@' , (logstring 'y') , ' :(' , (logstring 'x') , '@[ ' , fv , (logstring 'y') , '@[)"(' , titlestring , '))'
end.
NB. If inverse also needed, do it too
if. inv do.
  opi =. titlestring , '^:_1'
  iv =. '(' , (logstring 'I') , '@(' , opi , '))'
  vars =. vars , 'I'
  if. inp do.
    vars =. vars ,'XY'
    iv =. '(' , iv , '@' , (logstring 'Y') , ' :(' , (logstring 'X') , '@[ ' , iv , (logstring 'Y') , '@[)"(' , opi , '))'
  end.
  fv =. '(' , fv , ':. ' , iv , ')' 
end.
NB. init for logging
initloggingtable vars
NB.?lintonly 'logvalues logticket' =: (1$a:);$0
NB.?lintsaveglobals
auditstg fv
)

proplocales =: 3 : 0
(y = 3) # < tokensource
)
NB. Set globals, then initialize display for the verb
traverse =: 4 : 0
assert. 1 2 e.~ #x
traversedowncalcselect y  NB. Just to set error globals
calchighlight x
NB. If no vranks, this verb must have failed to execute owing to upstream error.  Leave no levrank then
NB. obsolete 'displaytype displayhandlesin displayhandleout displaylevrank' =: 'verb';((($0);(,0);_0.3 0.3) {::~ stealthoperand { valence , 1 1 1);0;op;((*#vranks) # ,:sellevel,vranks)
'displayhandlesin displayhandleout displaylevrank' =: ((($0);(,0);_0.3 0.3) {::~ dispstealthoperand { valence , 1 1 1);0;<rankhistory
NB. Pass the DOLs through, but delete any stealthoperand.  This is where we trim the display
if. (valence = 2) *. dispstealthoperand e. 1 2 do.
  x =. (<<< <:dispstealthoperand) { x
end.
x ,&< coname'' NB. no v, so no change to the DOLs
)

NB. **** assignment ****
cocurrent 'dissectassign'
coinsert 'dissectobj'

NB. Assignment does nothing and has no display (for now).  We just have to keep things going for sentence display

NB. Assignment.  y is the locales of the lvalue, copula, rvalue
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands.  uop is the locale of a noun, or the string for a name
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop=:<'dissectnoun' [ vop=: <'dissectverb'
NB. Remember if uop is a name
uopisname =: name = (<0 0) {:: y
utoken =: (<0 2) { y
NB. Since we don't participate in traversal, fix it so that references to this locale are picked up
NB. by the object of assignment.  We will take resultissdt from the assigner
coinsert vop
noun;(coname'');tokensource
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
''
)

NB. This will only be called if the rvalue is a verb; in that case, pass the call on
setvalence =: 3 : 0
setvalence__vop y
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
if. uopisname do.
  auditstg '(' , uop , ' ' , cop , (exestring__vop 0 1 0) , ')'
else.
  auditstg '((' , (exestring__uop 0 1 0) , ' )' , cop , (exestring__vop 0 1 0) , ')'
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
dissectmodindex =: 0$a:  NB. list of (<list of boxed modifier words)

NB. x is object class(es) to include, default 'dissectobj'
NB. y is string containing the modifiers that will be handled in this
NB. locale.  Result is the locale name.  Side effect: index extended
NB. MAJOR SIDE EFFECT: locale is changed
modlocale =: ''&$: : (4 : 0)
NB.?lintmsgsoff
cocurrent@(0&cocreate)@([ coerase) newloc =. <'dissectmod' , ": <: # dissectmodindex_dissect_ =: dissectmodindex_dissect_ , <;: y
NB.?lintmsgson
coinsert x , ' dissectobj'
18!:4 newloc   NB. No named verb from here to the end!
i. 0 0
)

NB. **** @ @: ****
modlocale '@@:'

NB. Save the name of the locale that handles @@: - we use it in &&: and also in fork
localeat_dissect_ =: coname''

create =: 3 : 0
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
NB. Ignore ]@ etc.
NB. obsolete if. stealthoperand__uop e. 1 2 do.
NB. obsolete   verb;vop;tokensource
NB. obsolete else.
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop
verb;(coname'');tokensource
NB. obsolete end.
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
setvalence__vop y
setvalence__uop resultissdt__vop
resultissdt =: resultissdt__uop
NB. Return the dispoperands from v
0 0 $0
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__vop combineheights estheights__uop    NB. height of 0 is special flag, meaning 'stealth'
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop 0 1 0) , ' ' , cop , ' (' , (exestring__vop 0,valence,0) , ')))'
)

NB. Return the locales for propsel.  If we got here through capped fork, we have to format accordingly
proplocales =: 3 : 0
if. 0 = 4!:0 <'vvv' do.
  NB. capped fork
  ((y = 3) # <tokensource),uop,vop
else.
  NB. normal operation
  uop,((y = 3) # <tokensource),vop
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
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
dol =. joinlayoutsl x traverse__vop a: travdownvops selector , (*./ selopinfovalid) # selopinfo
vop inheritu dol traverse__uop travdownuops vop
)


NB. **** & &: ****
modlocale '&&:'

NB. Save the name of the locale that handles &&: - we use it in &. and &.:
localecompose_dissect_ =: coname''

NB. When we find out the valence, we change this to be like @@: if monad, and proceed here only for the dyad

create =: 3 : 0
NB. If this is u&n or m&v, change the object type to bond, and switch over to that create routine
if. noun bwand bwor/ > (<0 2;0) { y do.
  changeobjtypeto 'dissectvandnm'
  create y
return.
end.
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
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
  return.
else.
NB. For dyad, we need to clone v.
  vop1 =: clone__vop ''
  vop0 =: vop
  NB.?lintonly vop0 =: vop1 =: coname''
  valence =: #y
  setvalence__vop0 {. y
  setvalence__vop1 {: y
  setvalence__uop resultissdt__vop0 , resultissdt__vop1
  resultissdt =: resultissdt__uop
end.
0 0 $0
NB. We always get both operands for v, since we have cloned vop0/vop1 (it's not worth saving the operand).
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: (estheights__vop0,estheights__vop1) combineheights estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: defstring__localeat f.

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
rankstg =. (cop -: ,'&') # '"', (')' ,~ '(]&' , defstring__vop0 3)
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , '(' , (exestring__vop0 0,valence__vop0,0) , '@[ ' , (exestring__uop 0 1 0) , (exestring__vop1 0,valence__vop1,0) , '@] ' , ')' , rankstg , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. For highlighting the sentence, we need only one clone.  Use the first
uop,((y = 3) # <tokensource),vop0,(y ~: 3) # vop1
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
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
dol =. joinlayoutsl (0 1 # x) traverse__vop1 0 1 2 travdownvops selector , ((1 { selopinfovalid) # 1) { selopinfo
dol =. dol ,~ joinlayoutsl (1 0 # x) traverse__vop0 0 1 2 travdownvops selector , ((0 { selopinfovalid) # 0) { selopinfo
if. snifferror__COCREATOR do. vop0 isolateverror vop1 end.   NB. On the first pass, figure out where the error is
(vop0,vop1) inheritu dol traverse__uop vop0 travdownuops vop1
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
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
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
valence =: #y
assert. valence = 1 [ 'u&n m&v dyad not supported'
'vl nl' =. <"0 verboperandx |. uop,vop
NB.?lintonly vl =. nl =. coname''
setvalence__vl verboperandx |. y , resultissdt__nl
resultissdt =: resultissdt__vl
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
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop 0 2 0) , '&' , (exestring__vop 0 2 0) , '))'
NB.?lintsaveglobals
)

NB. Return the locales for propsel
proplocales =: 3 : 0
(((verboperandx = 0) +. (y > 0)) # uop),((y = 3) # < tokensource),(((verboperandx = 1) +. (y > 0)) # vop)
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
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
dol =. verboperandx |. x , joinlayoutsl NOLAYOUTS traverse__nounop TRAVNOUN
NB. obsolete 'dol oloc' =. dol traverse__verbop travdownuops nounop,<selresultshape__nounop
nounop inheritu dol traverse__verbop bnsellevel , NORANKHIST , selector , (*./ selopinfovalid) # verboperandx |. selopinfo,createuop__nounop ''
)

NB. **** &. &.: ****
(>localecompose_dissect_) modlocale '&.&.:'
NB. we emulate this with v^"_1@:u&[:]v.  The only thing we do here is defstring, so we can recover the original display form of the verb

create =: 3 : 0
create_dissectobj_ f. (<2 1) { y
NB. Register this object so we can clean up at end
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: <'dissectverb' [ cop =: '&.:'
NB. Create an object to handle v^:_1@:u
NB. First, the verb v^:_1
nobj =. conew 'dissectverb'
NB. We have to make the object look as if it was created in the main parser
COCREATOR__nobj =: COCREATOR
iop =. 1 {:: create__nobj ((defstring__vop 2),'^:_1');0 0 0 0;($0)  NB. Verb for the inverse, no linenums
NB. Now create an object for vi@:u
nobj =. conew >localeat
COCREATOR__nobj =: COCREATOR
NB. Remove the . from &.&.: and create vi@:u
uop =: 1 {:: create__nobj (_3 [\ verb;iop;($0);conj;'@:';($0)) , 0 { y

NB. Now change this locale to &&: and create i&v
NB. Replace the uop with the iop we just created, and the cop with the given conjunction, with '.' removed
NB. We leave the &. locale in the path, so that we can override
NB. obsolete coinsert localecompose
NB. obsolete changeobjtypeto localecompose
create__localecompose f. (uop;(<<<1){cop) (<0 1;1)} y
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
NB. We keep the original form 
defstring =: 3 : 0
enparen^:(y=3) (defstring__vop__uop 2) jd cop jd (defstring__vop 3)
)


NB. **** u"n u"v m"n m"v****
modlocale '"'

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
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
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
setvalence__uop y
resultissdt =: resultissdt__uop
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
NB. obsolete auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop 0 1 0) , ' ' , cop , ' (' , (exestring__vop 0,valence,0) , ')))'
auditstg '(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop 0 1 0) , ')' , cop , '(' , (exestring__vop 0,valence,0) , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
uop,((y = 3) # < tokensource),(y > 0) # vop
)

NB. y is selector,operands for the next operation
NB. result is replacement selector,operands.  We know that the result did not collect
rankcalculus =: 3 : 0
NB. selopinfo will be correct; we just need to keep the selection alive, so that we can
NB. accumulate shapes from lower levels.  But if there are no shapes, don't keep alive
a: 0}^:(1 < #@]) y
)

NB. Traversal up and down the tree.
NB. The input y gives the selection level and inherited state of selection, which is passed to travdowncalcselect,
NB. The result is the DOL, up through the result of u
NB. We do not create a cell; we just traverse u.  There is no visible indication of the rank operator, except in the
NB. frames
traverse =: 4 : 0
NB. for u"n, resolve n internally.  It will not display, but we need a result for getverbrank
if. vtype bwand noun do. NOLAYOUTS traverse__vop TRAVNOUN end.
traversedowncalcselect y
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
'' inheritu x traverse__uop bnsellevel , rankhistory ; rankcalculus^:(-.*./selopinfovalid) selector , selopinfo
)

NB. overrides for calcselect

NB. get the rank to use for this verb.
NB. y is selopinfo, 0 to 2 boxes containing (remaining shape);(frame so far)
NB. Result is the rank to use for the verb's valence, or $0 if we don't know
getverbrank =: 3 : 0
rank =. $0
NB. We use the actual executed rank unless this verb has negative rank and there are operands.
NB. in that case we calculate the rank to use after referring to the actual operand rank
if. vtype bwand noun do.
  if. _1 e. * nrank =. 3 $&.|. 1 {:: frameselresult__vop '' do.
    if. #y do.
      rank =. 0 >. (#@(0&{::)@> y) (] + (* <&0)) (valence { 0 1 _2) {. nrank
    end.
  end.
end.
if. 0 = #rank do. rank =. getverbrank_dissectobj_ f. y end.
rank
)
NB. **** m/ u/ ****
'dissectexpandable' modlocale '/'

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
titlestring =: (defstring__uop 2) , cop   NB. Default title to the value when u not used.  When u used, we take from u
NB.?lintonly uop =: coname'' [ cop =: ''
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
NB. The descendant is always executed as a dyad
setvalence =: 3 : 0
valence =: #y
setvalence__uop 2$y
resultissdt =: resultissdt__uop
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
NB. Since u is always a dyad, combine heights if we are a monad
estheights =: <./^:(valence=1) estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. valence = 1 do.
  NB. Use unadorned verb on empty operand to get neutral.  Save every result of verb execution.  Also save overall result.
  auditstg '((' , (logstring '') , '@:(' , (verblogstring '') , (logstring '') , '@:(' , (exestring__uop 0 1 0) , ')/`(' , (defstring__uop 2), '/)@.(0=#))))'
else.
  NB. Simulate dyad u/ by creating a rank
  auditstg '((' , (logstring '') , '@:(' , (verblogstring '') , (exestring__uop 0 1 0) , ')"(_ (_1)} ' , (defstring__uop 2), ' b. 0))"_)'
end.
)

NB. Return the locales for propsel
proplocales =: 3 : 0
uop,(y=3) # <tokensource
)

NB. get the rank to use for this verb.
NB. We treat u/ like u"(lu,_)
getverbrank =: 3 : 0
if. valence = 1 do. _ else. _ (1}) getverbrank_dissectobj_ f. y end.
)


NB. Traversal up and down the tree.
NB. 
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
if. valence = 1 do.  NB. monad u/
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
    NB. We have a selection of a u/ of exactly 2 items.  Convert to dyad
    x =. x , createreference x   NB. Create reference for dyad u
    NB. Remove the line we added for u/
    x traverse__uop bnsellevel , (}:rankhistory) ; rankcalculus^:(-.*./selopinfovalid) selector , 2 # selopinfo
    NB. No inheritu, because this locale plays no part in the display
  elseif. (1 < nitems =. {.frame) *. (sellevel < #selections) do.
    NB. We have a selector, and more than 2 possible selections.  Display the selector, and traverse u
    if. nitems (| = <:@[) {. sellevel {:: selections do. x =. x , createreference x  NB. Guaranteed selector is valid
    else. x =. x , createselfreference 0.4   NB. loop from right of box to y argument
    end.

    NB. No calchighlight needed - it will be done in u
    NB. Replace the last line of rankhistory with simple '/'
    NB. Traverse u to display it and its descendants, and create a display node for pre-u.  u will be inherited into the display
    NB. of the selector created here, which is part of the result
    dol =. joinlayoutsl '' inheritu x traverse__uop bnsellevel , ((<cop) (<_1 0)} rankhistory) ; rankcalculus^:(-.*./selopinfovalid) selector , 2 # selopinfo
    'displayhandlesin displayhandleout displaylevrank' =: (,0);0;<rankhistory
    dol ,&< coname''
  elseif. do.
    NB. Display u/ as a monolith
    NB. We are subsuming u in the display of this node.  Just display the values we have selected
    NB. The accumframe here holds the (negative) starting selection value, if there is unexpanded detail
    'accumframe displayhandlesin displayhandleout displaylevrank' =: (<_1:"0 frame);(,0);0;<rankhistory
    NB. no selection, so no calchighlight needed
    x ,&< coname'' NB. no v, so no change to the DOLs
  end.
NB. 
else.  NB. dyad
  traversedowncalcselect y
  calchighlight x
  if. errorcode = ENOAGREE do. agreementerror x return. end.
  '' inheritu x traverse__uop bnsellevel , rankhistory ; rankcalculus^:(-.*./selopinfovalid) selector , selopinfo
end.
)

NB. *** traversal support ***
NB. y is natural frame(s) of the executed verb
NB. result is (frame of value to display);(selopshapes for next level - cells of this verb)
calcdispframe =: 3 : 0
if. valence = 1 do.
  NB. There are selops; the number of results is one less than the number of items.
  NB. Return empty if # items is 0 or 1
  calcdispframe_dissectobj_ f. , < (#~ >&0) <: {. {.@> selopshapes
else. calcdispframe_dissectobj_ f. y
end.
)

NB. y is all the indexes that were selected by  the selector
NB. Result is the selectors to display (a list), in order.  The atom count should match the frame returned by calcdispframe
calcdispselx =: 3 : 0
if. valence = 1 do.
  NB. If the frame is empty, take the last result; otherwise drop the last result IF there are more results than the frame calls for
  {:`(}:^:(({.frame)<:#)) @.(*#frame) y
else. calcdispselx_dissectobj_ f. y
end.
)

NB. y is the result of calcdispselx.  Result is the matching values.  These values will be collected to
NB. produce the displayed result.  We might have to add boxing if we want to ensure collectibility
NB. Result always stays in execution order
fetchselr =: 3 : 0
if. valence = 1 do.
  NB. If this verb is expanding, box each result so we don't collect the different executions together
  <"0^:(*#frame) y { logvalues
else. fetchselr_dissectobj_ f. y
end.
)

NB. x is the frame of the full expected result
NB. y is the number of results we actually got
NB. result is index list of the failing location
getfailingindex =: 4 : 0
if. valence = 1 do. x #: _1 - y  NB. count back from the end
else. getfailingindex_dissectobj_ f. y
end.
)

NB. y is the intervals for each ticket, expanded into an array using the shape of the frame
NB. Result is the array reordered to natural order; reversed, since u/ processes in reverse order
tickettonatural =: 3 : 0
if. valence = 1 do. |. y else. y end.
)

NB. **** um`vn ****
modlocale '`'

create =: 3 : 0
create_dissectobj_ f. (<1 2) { y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: '' [ conjex =: ''
resultissdt =: resultissdt__uop *. resultissdt__vop
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) jd cop jd (defstring__vop 3)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (conjlogstring '') , (defstring__uop 2) , ' ' , cop , (defstring__vop 3) , ')'
)

calcestheights =: 3 : 0
''
)

proplocales =: 3 : 0
(y=3) # uop,(<tokensource),vop
)

NB. Set globals, then initialize display for the noun.  There must be no DOLs, and we
NB. return no U dols
traverse =: 4 : 0
assert. 0 = #x
traversedowncalcselect 2 {. y  NB. Just to set error globals
calchighlight x
selresult =: <conjex
'displayhandlesin displayhandleout displaylevrank nounhasdetail' =: ($0);0;NORANKHISTNOUN,<0
x ,&< coname''  NB. Return the empty DOLs
)



NB. **** u~ m~ ****
modlocale '~'

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
NB.?lintonly uop =: coname'' [ cop =: ''
NB. obsolete refdetail =: 0  NB. This locale creates a reference
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
setvalence__uop 2$y
resultissdt =: resultissdt__uop
0 0$0
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: <./^:(valence=1) |. estheights__uop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y=3) (defstring__uop 2) , cop
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop 0 1 0) , ' ' , cop , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
uop,(y=3) # <tokensource
)

NB. Traversal up and down the tree.
NB. 
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
traversedowncalcselect y
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
NB. If u takes more arguments than we were given, we have to create a reference for the input.
NB. We assign it to the estimated longer side of u.
if. (#x) < 2 do.
  x =. |.^:(*>/estheights__uop) (, createreference) x
end.
'' inheritu (|. x) traverse__uop bnsellevel , (0 1 _1 2 {"1 rankhistory) ; (*./ selopinfovalid) # selector , _1 0 { selopinfo
)


NB. **** default ****
modlocale ''
localedefault_dissect_ =: coname''
NB. Remove the last element in the search, to make this the 'search failed' locale
dissectmodindex_dissect_ =: }: dissectmodindex_dissect_

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
  stg =. (defstring__uop 2) jd cop jd (defstring__vop 3)
end.
NB. obsolete changeobjtypeto 'dissectverb'
NB. We will treat this as a generic verb, except for the overrides we have in this locale
((18!:2~    {. ,  'dissectverb' ; }.) 18!:2) coname''
NB. Pass the token number of the modified in as the verb token number.  That will go into tokensource
create_dissectverb_ f. stg;0 0 0 0;(<1 2){y
)

NB. display height is always just 1

proplocales =: 3 : 0
(y = 3) # (<tokensource) 1} >&.> ucvlocs
)


NB. **** fork ****
cocurrent 'dissectfork'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: cop =: coname''
NB. Remember whether this is an nvv-type fork
if. vvv =: verb = (<0 0) {:: y do.
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
NB. obsolete NB. If cop is a stealth, we can omit one branch altogether
NB. obsolete if. stealthoperand__cop do.
NB. obsolete   (2 {. y{~0 2 1 0{~stealthoperand__cop),tokensource
NB. obsolete else.
NB. obsolete   'xrefdetail yrefdetail' =: 0  NB. This locale creates references
NB. Set resultissdt for modifier processing
resultissdt =: resultissdt__uop *. resultissdt__vop *. resultissdt__cop
verb;(coname'');''
NB. obsolete end.
NB.?lintsaveglobals
)

destroy =: 3 : 0
destroy_dissectobj_ f. ''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
setvalence__vop y
if. vvv do.
  setvalence__uop y
  setvalence__cop resultissdt__uop , resultissdt__vop
  NB. We have to ensure that any stealthoperand produces a 0 height for all it contributes to,
  NB. so we don't assign a label to a stealthoperand
else.
  setvalence__cop resultissdt__uop , resultissdt__vop
end.
resultissdt =: resultissdt__cop
0 0$0
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
if. vvv do.
  estheights =: >./ estheights__cop combineheights (estheights__uop ,: estheights__vop)
else.
  estheights =: ({: estheights__cop) combineheights estheights__vop
end.
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen (defstring__uop 1) jd ' ' , (defstring__cop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
if. vvv do. uops =. 0,valence__uop,0 else. uops =. 0 1 0 end.
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop uops) , ' ' , (exestring__cop 0,valence__cop,0) , ' ' , (exestring__vop 0,valence__vop,0) , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. obsolete ((y > 1) # uop),cop,((y > 1) # vop)
(uop),cop,(vop)
)

NB. Traversal up and down the tree.
NB. 
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
traversedowncalcselect y
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
NB. Make a reference for each operand (if vvv).  Assign the reference to the higher estheight.  Since
NB. nondisplayed operands have estheight of 0, if dispoperands culls a value, it will always be
NB. the reference that is culled.
if. vvv do.
  x =. (</ estheights__cop combineheights (estheights__uop ,: estheights__vop)) |."0 2 (,: createreference)"1 x
  dolv =. joinlayoutsl (1 {"2 x) traverse__vop a: travdownvops selector , (*./ selopinfovalid) # selopinfo
  dolu =. joinlayoutsl (0 {"2 x) traverse__uop a: travdownvops selector , (*./ selopinfovalid) # selopinfo
  if. snifferror__COCREATOR do. uop isolateverror vop end.   NB. On the first pass, figure out where the error is
else.
  NB. nvv.  Traverse n as a noun
  dolv =. joinlayoutsl x traverse__vop a: travdownvops selector , (*./ selopinfovalid) # selopinfo
  dolu =. joinlayoutsl NOLAYOUTS traverse__uop TRAVNOUN
end.
(vop,vvv#uop) inheritu (dolu,dolv) traverse__cop uop travdownuops vop
)

NB. **** hook ****
cocurrent 'dissecthook'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. a:
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname''
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
NB. obsolete refdetail =: 0  NB. This locale creates references
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
setvalence__vop {: y
setvalence__uop 2$y
NB. dispoperands is set from u
resultissdt =: resultissdt__uop
0 0$0
NB.?lintsaveglobals
)

calcestheights =: 3 : 0
estheights =: >./^:(valence=1) estheights__uop (*@[ * +) 0 , estheights__vop
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen (defstring__uop 1) jd ' ' , (defstring__vop 0)
)

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , (exestring__uop 0,valence__uop,0) , ' ' , (exestring__vop 0,valence__vop,0) , '))'
)

NB. Return the locales for propsel
proplocales =: 3 : 0
NB. obsolete uop,(y > 1) # vop
uop,vop
)

NB. Traversal up and down the tree.
NB. 
NB. The result is the DOL, up through the result of u
traverse =: 4 : 0
traversedowncalcselect y
calchighlight x
if. errorcode = ENOAGREE do. agreementerror x return. end.
NB. If this is a monad, make a reference for y, assigning the label to v, since it will always be used
if. 1 = #x do. x =. (,~ createreference) x end.
dol =. joinlayoutsl (0 1 # x) traverse__vop 0 1 _1 travdownvops selector , ((_1 { selopinfovalid) # _1) { selopinfo
NB. Use selop0 for x, and selresult for y - but only if selop0 exists, and no travdownuops error
NB. replace the rank with the left rank of the hook, alone
vop inheritu (dol ,~ 0 {  x) traverse__uop (<0 1 2 {"1 rankhistory) 1} (((0 { selopinfovalid) #  0) { selopinfo) ((,3)}   1 1 1 0 1&(#^:_1))^:(1 4 -: ,&#) travdownuops vop 
)


NB. 0!:1 ; <@(LF ,~ '(i. 0 0) [ 3 : ''destroy__y 0'' dissectinstance_dissect_ [ ' , enparen_dissect_);._2 runtests_base_
NB. wd@('psel dissect;pclose'"_)"0 i. 100
runtests_base_ =: 0 : 0
dissect '2+''a'''
dissect '2,''a'''
dissect '2 3+''a'''
dissect '1 2 + ''ab'''
dissect '1 2 +@+ ''ab'''
dissect '1 2 +&+ ''ab'''
dissect '1 2 +&+~ ''ab'''
dissect '''ab'' +&+ 1 2'
dissect '1 2 +@(]"0) ''ab'''
dissect '1 2 +@(0:"0) ''ab'''
dissect '0 1 2 + 1 2'
dissect '+@+ ''a'''
dissect '+@{. ''a'''
dissect '0 +&+ ''a'''
dissect '0 +&+ ''ab'''
dissect '0 +&:+ ''a'''
dissect '''a''+&+ 0'
dissect '''ab''+&+ 0'
dissect '''a''+&:+ 0'
dissect '+&{. ''a'''
dissect '+&:+ ''a'''
dissect '+&2 (3 4)'
dissect '3&* (3 4)'
dissect '+&''a'' (3 4)'
dissect '(+&2)@:(2&*) 4 6'
dissect '3 4 +"1 i. 3 2'
dissect '(i. 3 2) +"1 (3 4)'
dissect '(i. 3 2) +"1 i. 3 2'
dissect '(i. 3 2) +"1 i. 3 1'
dissect '(i. 3 2) +"1 i. 1 1'
dissect '2 3 +@]&> 5 6'
dissect '(i. 3 2) +@]"1 i. 1 1'
dissect '(i. 3 2) +@["1 i. 1 1'
dissect 'i.@(0&{) ''a'''
dissect 'i."0 (1 2)'
dissect '+~ i. 2 3'
dissect '3 4 +~ i. 2 3'
dissect '3 4 +~ i. 3 2'
dissect '3 4 +@]~ i. 3 2'
dissect '3 4 +@[~ i. 3 2'
dissect '3 4 +~ i. 2 3'
dissect '3 4 (+ - *) 0 1'
dissect '0 1 2 (+ - *) 0 1'
dissect '0 1 2 (+ - 0:) 0 1'
dissect '0 1 2 (0: - *) 0 1'
dissect '0 1 2 (1:"0 - 0:"0) 0 1'
dissect '0 1 2 (+ - ]) 0 1'
dissect '0 1 2 ([ - -) 0 1'
dissect '0 1 2 ([ - ]) 0 1'
dissect '0 1 2 (- + * % -)"0 (3 4 5)'
dissect '0 1 (+ 0:) ''ab'''
dissect '0 1 (+ {.) ''ab'''
dissect '0 1 (+ ]) 1 2 3'
dissect '(0 1 2 + 0 1"_) 5'
dissect '0 1 2 + '''''
dissect '0 1 2 + '' '''
dissect '0 (+ - *) '''''
dissect '0 (1 2 3 - *) '''''
dissect '0 (1 2 3 - *)"0 '''''
dissect '0 (1 2 3 , ])"0 $0'
dissect '0 ([: 1 2 3"0 $)"0 $0'
dissect '0 (+ - ]) '''''
dissect '0 (1 2 3 - *)"0 $0'
dissect '0 (1 2 3 - *)"0 (0)'
dissect '0 +@* '''''
dissect '0 (+@* - *) '''''
dissect '0 (+@* *) '''''
dissect '0 (+ *) '''''
dissect '(#@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
dissect 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';0' [ z =. 2
dissect 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';''q''' [  z =. 2
dissect '(1&+@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
dissect '(i.@# ((}.>) ,. ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
dissect '(i.@# ((}.>) ,&< ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
dissect '(i.@# ((}.>) , ({.>))"0 ]) b' [ b =. ;:'The quick brown fox'
dissect '0 1 2 3 {~ 2'
dissect '(i. 2 3) {~ 2'
dissect '(i. 3 2) {~ 2'
dissect '2 ([: |: ([ = [: +/ [: ([: |: ] #: [: i. */) 2 $~ ]) #"1 [: ([: |: ] #: [: i. */) 2 $~ ])4'
dissect '('' O'' {~ (] !~ [: i. >:) >/ [: i. [: >./ ] !~ [: i. >:) 8'
dissect '1 2 +"_1 0 (1 2)'
dissect '1 2 ,"_1 i. 2 3'
dissect 'y =. 2 + 5'
dissect 'zzz + 5 [ zzz =. 6'
dissect '''a b c'' =. i. 3'
dissect '''`a b c'' =. +`-`%'
dissect 'r + s [ (''r s t'') =. 0 1 2 [ a =. ''r'';''s'';''t'''
dissect '-&.> i. 3'
dissect '-&.:> i. 3'