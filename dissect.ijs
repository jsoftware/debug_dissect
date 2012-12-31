NB. Copyright (c) Henry H. Rich, 2012.  All rights reserved.

NB. INPROGRESS:

NB. TODO:
NB. Make all error displays come from condselection.  Have displayable-error status created during travdown & propagated
NB.  through displayrcds - & reset appropriately when fill-cell handled.
NB. Stress multilevel selects incl boxing
NB. Select multiple ds '*@#@> ''a'';''b'';''cd'''  dclick, selector 2 0 - does not produce final selector.  Need to out any final localselresult if not outed before final collector
NB. release
NB. Show wilch axes is filled by placement of *
NB. Handle assignments
NB. default needs to traverse noun operands - if not sdts
NB. handle negative rank
NB. remove ref/labels
NB. Fix token numbers in sentence display - use only number of token.  Include parens enclosing all selected tokens
NB. bring x operand in below y in grid ?
NB. ]&v not handled; (u ]) not handled
NB. allow display of stealthoperands
NB. Support assignment to debug globals
NB. Use user's spacing in sentence display


NB. dissect - 2d grid-based single-sentence debugger

NB. the call is:
NB. [options] dissect sentence
NB. where sentence is a string to be executed.  The sentence is parsed and modified so that every verb execution creates
NB. looging information about its input and outputs.  Then the modified sentence is executed (in the same context as the original
NB. dissect verb), and then the results are displayed in 2d form using a grid control.  Flow of the sentence generally runs from top
NB. to bottom in the grid.  Clicks on the grid set variables to control what information is displayed.

ds_z_ =: [: ([: display_dissect_ 0:@". :: 1:)`>@.(32=3!:0)  [: parse_dissect_ (0&# : [ (([ ; 18!:5@(''"_) ; ]) , z458095869_dissectnopath_@(''"_)) ])

NB. The locale dissectnopath is used to find local names.  Its path is empty.  The locale contains only one name, z458095869
cocurrent 'dissectnopath'
copath ''
NB. The verb z458095869 returns a table of defined local names.  It is a table of (name;(type from 4!:0);(numeric ranks, invertible if verb/value if noun, '' if other))
z458095869 =: (([ ,. <"0@] ,. (".@[`[`[`(rankinv_dissect_@[))@.]&.>) (4!:0)) @ ((<'z458095869') -.~ 4!:1@i.@4:)

require 'strings gl2'
cocurrent 'dissect'
coinsert 'jgl2'

3 : 0 ''
if. IFJ6 do. IFGTK =. -. IFCONSOLE
else.
  require 'gtkwd wdclass'
end.
if. IFGTK do.
require 'grid'
end.
''
)

NB. Maximum line length that we will try to display in a grid cell

MAXLINELEN =: 300


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
dissectinstance =: '' conew 'dissect'
errormessage =: 'unknown error during parsing'
try.
  parsemain__dissectinstance y
catch.
  smoutput syserr   =. > (errnum =. <:13!:11'') { 9!:8''  NB. string form of emsg
  smoutput sysstg   =. 13!:12''

  NB.  Error encountered during parse.  We indicate this by boxing the sentence.  We
  NB. destroy the locale, since we can't continue
  destroy__dissectinstance''
  <errormessage
end.
NB.?lintsaveglobals
)

NB. Signal failure of the parse.  y is the error message
NB. We keep it in the main dissect locale for ease, since it is valid only over the parse
failparse =: 3 : 0
errormessage_dissect_ =: y
13!:8 (1)
)

NB. Initialization
create =: 3 : 0
objtable =: 0$a:   NB. list of parse objects
ticket =: 0   NB. sequential log number
winhwnd =: ''  NB. Init to no window
NB. Save the initial environment: locale size, boxing chars
Jenvirons =: (9!:6'') ;< (9!:38 '')
NB. Set boxing chars that we can see in the grid
9!:7 '+++++++++|-'
NB. Use lightweight locales
9!:39 (0) 1} 9!:38 ''
NB.?lintsaveglobals
)

NB. Add new object to the list of objects
newobj =: 3 : 0
objtable =: objtable , y
)

NB. Utility to create rank,invertible flags for a verbname
NB. y is name of a verb, visible in current context
NB. result is (ranks), 1 if invertible
rankinv =: ".@(,&' b. 0') , 1:@". :: 0: @(,&' b. _1')

NB. anything beginning with one of these words and ending with . is a control word
controlwords =: ;: 'assert break continue for goto label if do else elseif end return select case fcase throw try catch catchd catcht while whilst'
NB. tokenize each line and remove comments - except for lint directives
lines =. }:^:(1 0 -: ('NB.';'NB.?lint') ([ -: #@[ {. ])&> {:)@;:&.> lines
NB. for each line, find control words; then recollect sentences between control words; then
NB. append the line number of the line. run all the blocks together.  This deletes empty sentences, too
NB. For multiple blocks on the same line (caused by control words), give them fractional parts to
NB. distinguish them
NB. Verb, returning 1 if a word is a control word
iscw =: e.&controlwords@(('_'&taketo)@}:&.>) *. ('.'={:)@>  NB. verb, applied to boxed word.  Any remaining comment must be a lint directive


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
try. queue =. }:^:('NB.' -: 3 {. >@{:) ;: sentence catch. queue =. 0$a: return. end.
NB. If the sentence contains control words, discard any leading control words and then take non-control words
NB. stopping before the first control word.
queue =. ((i.&0@] }. ({.~ # <. [: >: 1 i.~ 2&(</\))) iscw) queue

NB. If the sentence is empty, abort
if. 0 = #queue do. failparse 'no sentence' return. end.

NB. Save the tokenized sentence in a global for later use
toksentence =: queue

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
    NB. Create an assignment block for the operands, and put that on the stack.  The type of the assignment
    NB. will be the type of the rvalue
failparse 'assignment not supported'
    nobj =. conew 'dissectassign'
    stack =. ((subj i. 1){.stack),(create__nobj exeblock),((>:subj i: 1)}. stack)

    NB. Now try to handle the case of names assigned within a sentence and reused later.
    NB. extract the type of the rvalue, in 4!:0 form.  If it's a verb, go get its rank
    if. 3 = rtype =. (noun,adv,conj) i. cavn bwand 2 { exetypes do.
      vobj =. (<2 1) { exeblock
      NB.?lintmsgsoff
      rrank =. _ _ _ NB. rank__vobj''
      NB.?lintmsgson
    else.
      rrank =. $0
    end.

    NB. The left operand is a name or a noun.  
    NB. If the lvalue is a name, make it global if the assignment is global.
    rname =. (<0 1) { exeblock
    if. name = 0 { exetypes do.
NB. ...wrong      if. (<'=:') -: 1 { exetypes do. rname =. (('_',loc,'_') ,~ ])&.> rname end.
    NB. If it is a noun, we can handle it only if it is a
    NB. self-defining term.  In that case, turn it into a list of names.  If the noun starts with '`',
    NB. change the type of the rvalue to 'verb of infinite rank'    NB. Add definitions to the local name-table for each surviving name
    else.  NB.?lintonly [ rname =. <'dissectnoun'
      if. sdt bwand 0 { exetypes do.
        if. 2 = 3!:0 lvalue =. value__rname'' do.  NB.?lintonly [ lvalue =. ''
          if. '`' = {. lvalue do.
            rrank =. _ _ _
            lvalue =. }. lvalue
          end.
          rname =. ;: :: (a:$~0:) lvalue
        else.
          failparse 'Invalid assignment'
          return.
        end.
      else.
        rname =. 0$a:
      end.
    end.
    defnames =. (rname ,"0 1 rtype;rrank) , defnames

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
        'objtype objvalrank' =. 1 2 { (({."1 defnames) i. objloc) { defnames
        gloc =. objvalrank
      else.
      NB. Nothing found in local table - set to resolve the whole thing globally
        gloc =. loc
        glopart =. qend
      end.
      NB. Now we have resolved any local that we are going to use.  If there was one, it is in
      NB. objtype/objvalrank.  But a global search may be needed: if there was
      NB. an object locative, or if the local search failed.  This search will start in locale gloc.
      NB. This search, if performed, must succeed, and we will convert the result to a type/(rank if verb)
      if. #glopart do.
        savloc =. coname''
        NB.?lintmsgsoff
        cocurrent gloc
        NB.?lintmsgson
        if. 3 = objtype =. 4!:0 <glopart do.
          objvalrank =. rankinv_dissect_ f. glopart
        else.
          objvalrank =. $0
        end.
        cocurrent savloc
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
        failparse 'error in object type'
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
ES__   =: exestring__resultroot''
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


NB. After the sentence has been executed, return here to display the grid

NB. The colors we can use, each given a name
MAXSELLEVEL =: 5   NB. number of different selection levels we have colors for
'colornames colorpalette' =: ({."1 ,&< ".@(;:^:_1)@(}."1)) ;:;._2 (0 : 0)
empty  0 0 0 20 20 20
input  0 0 0 255 255 255
verb   0 0 0 255 200 200
qualifier 255 255 255 128 100 100
qualifiedop 100 255 255 128 100 100
qualifiedres 100 255 255 128 100 100
result0 0 0 0 200 200 255
selector0 0 0 0 153 255 000
result1 0 0 0 , <. 0.8 * 153 255 000
selector1 0 0 0 204 255 000
result2 0 0 0 , <. 0.8 * 204 255 000
selector2 0 0 0 255 255 000
result3 0 0 0 , <. 0.8 * 255 255 000
selector3 0 0 0 255 204 000
result4 0 0 0 , <. 0.8 * 255 204 000
selector4 0 0 0 255 154 000
result5 0 0 0 , <. 0.8 * 255 154 000
selector5 0 0 0 255 104 000
shape 255 255 255 0 100 100
invalid 255 0 0 255 255 255
error 255 255 255 255 0 0
label  0 0 0 230 230 255
reference  0 0 0 230 230 255
)
MINFONTCHOICES =: 7 8 9 10 12
valuefonts =: ('"Courier New" ' , ":)&.> MINFONTCHOICES
valuenames =. ('value' , ":)&.> i. # MINFONTCHOICES
'fontnames fontpalette' =: <"1 |: (valuenames ,. valuefonts) , ({. , <@(;:^:_1)@}.)@> <@(<;._1)@:(' '&,);._2 (0 : 0)
empty "Courier New" 10
verb "Arial bold" 12
qualifier "Arial" 10
selector "Arial" 10
shape "Arial" 10
invalid "Arial" 12
error "Arial" 12
label "Courier New bold" 10
reference "Courier New bold" 10
)


DISSECT=: 0 : 0
pc dissect;
xywh 3 43 457 414;cc parsegrid isigraph rightscale bottomscale;
xywh 3 3 360 13;cc sentence edit es_autohscroll rightscale;
xywh 364 0 26 60;cc minfont combolist leftmove rightmove;
xywh 392 1 25 11;cc lbl00 static;cn "Min Font";
xywh 416 0 26 60;cc maxfont combolist leftmove rightmove;
xywh 444 1 17 11;cc lbl01 static;cn "Max";
xywh 364 15 26 60;cc maxnounsize combolist leftmove rightmove;
xywh 392 17 69 11;cc lbl00 static;cn "Max Noun (% of scrn)";
xywh 3 20 359 22;cc inst00 static;cn "inst00";
xywh 384 29 48 12;cc showerror button;cn "Show Error";
pas 0 0;
rem form end;
)

MAXNOUNPCTCHOICES =: 30 50 70 90
display =: 3 : 0   NB. called in dissect locale
displaymain__dissectinstance y
)
displaymain =: 3 : 0  NB. called in object locale
NB. save the crash indicator
if. crashed =: y do.
  NB. Save the error message for the crash
  errormessagefrominterp =: _6 }.^:(' error' -: {.) (<:13!:11''){::9!:8''
else.
  errormessagefrominterp =: ''
end.

NB.?lintonly COCREATOR =: <'dissect'
NB. Create a selector that will admit anything, without infinities
selectall =: 0 , >:ticket

NB. If we crashed, do an initial traversal to set selection flags to find the error
if. crashed do. traversedown__resultroot ($0);selectall end.
NB. The argument of $0 indicates that we want to set the crash variables
NB. debug wd :: 0: 'psel dissect;pclose'
wd DISSECT
wd 'set inst00 *', 'Click to select/unselect; CTRL-click to sel/unsel up the tree.  / in shape shows frame, * in shape = fills added.  Green boxes are selectors; Dclick to edit.  Click verbs or "n to get more selectors'
winhwnd =: wd 'qhwndp'
parsegrid=: '' conew 'jzgrid'
wd^:(-.IFJ6) 'pshow;pshow sw_hide'

NB. Initialize the user selection
wd 'set minfont *', ; (LF ,~ ":)&.> MINFONTCHOICES
wd 'setselect minfont ' , ": minimumfontsizex =: 0
wd 'set maxfont *', ; (LF ,~ ":)&.> MINFONTCHOICES
wd 'setselect maxfont ' , ": maximumfontsizex =: <: # MINFONTCHOICES

wd 'set maxnounsize *', ; (LF ,~ ":)&.> MAXNOUNPCTCHOICES
wd 'setselect maxnounsize ' , ": maxnoundisplaysizex =: 1

wd 'setshow showerror ', ": crashed

NB. Figure out the screen fraction used by 1 char in each of our sizes
fontpixelsizes =: 10 1 %~"1 ('0123456789') (glqextent@[ glfont@>)"_ 0 valuefonts
NB. Insert the fixed formatting
setnames__parsegrid t =. _2 ]\ ('GRIDID';'parsegrid'),('CELLALIGN',&<1),('CELLCOLORS',&<colorpalette),('CELLFONTS',&<fontpalette),('GRIDHWNDC',&<wd 'qhwndc parsegrid'),('GRIDPID',&<'dissect')

NB. Set the initial selection of cells:
NB. If the result is an sdt, the user is probably noodling around with a new sentence, so select everything
NB.?lintonly resultissdt_dissectmonad_ =: 0
if. resultissdt__resultroot do. gridcommand__resultroot =: 1 1 end.

showgrid''
wd^:(IFJ6) 'pshow'
NB.?lintmsgsoff
evtloop^:(-.IFJ6)''
NB.?lintmsgson
NB.?lintsaveglobals
)

NB. y is a number, result is boxed character string 0=A, 1=B, 26=AA, 
disprefnum =: [: <@(-.&' ')"1 ' ABCDEFGHIJKLMNOPQRSTUVWXYZ' {~ 0 1 +"1 (0 26)&#: 
NB. display the grid.  Should be called in the locale of the major instance
showgrid =: 3 : 0
NB. Check the current screensize, and calculate the box sizes in characters
wd 'psel ',winhwnd
gridwh =. 2 3 { 0 ". wd 'qchildxywhx parsegrid'
fontcharsizes =: gridwh %"1 fontpixelsizes

NB. Propagate grid selection info through the tree
highlighttokens =: $0
traverseup__resultroot ,:0 0
traversedown__resultroot 0;selectall

NB. Write the RTF for the sentence, with highlighted tokens marked
wd 'set sentence *' , toksentence createsentencertf ~. highlighttokens
NB. Build the grid.  The top-level item will not need any xy operands
NB. First, clear the counter of references
referenceticket =: 0   NB. 1 is the first ticket
griddata =. creategridobj__resultroot ''
NB. debug qprintf '$griddata griddata '
NB. Now extract the grid information from the final grid object.  Replace empty cells with our default
cd =. 0 {:: griddata   NB. The grid stuff
emptycell =. <'';(6$0);''
cd =. (a: = cd)}cd,:emptycell
NB. Save the verbs to use for mouse events
callbacks =: 2&{@> cd
NB. Delete duplicate callbacks.  We put identical callbacks in for dyad editselector, because we didn't
NB. know which one would end up surviving.  If both did, we now need to delete the leftmost one, leaving
NB. a blank cell
NB. Order the cells from right to left, then find duplicates that are nonblank, convert to indices,
NB. blow away the cells
if. # extrasx =. 0 _1 bwxor"1 |."1 ($ #: I.@(~: < ('edit' -: 4&{.)@>)@,) |. |: callbacks do.  NB. Fails if 0 replacements
  cd =. emptycell (<"1 extrasx)} cd
  callbacks =: 2&{@> cd  NB. refresh the list
end.
NB. debug qprintf '$cd cd '
'cellcolor cellfont celledit cellref celllbl celldetail' =. 0 1 |: > 1&{@> cd
NB. the displayable data
celldata =. 0&{@> cd
NB. Install the reference letter wherever there is a reference and no detail.  We must assign the letter
NB. without referring to which cells get detail, so the letters don't change
allrefs =. /:~ ~. 0 -.~ , cellref  NB. The cells that need letters
if. #refcells =. <"1 ($ #: I.@,) (*cellref) *. -. celldetail do.
  celldata =. (disprefnum allrefs i. refcells { cellref) refcells} celldata  NB. This fails if execd with no refs
  NB. Change the font to the font for a reference.  Leave the color as reference color
NB.  cellcolor =. (colornames i. <'reference') refcells} cellcolor
  cellfont =. (fontnames i. <'reference') refcells} cellfont
end.
show__parsegrid _2 ]\ ('CELLDATA',&<celldata),('CELLEDIT',&<celledit),('CELLCOLOR',&<cellcolor),('CELLFONT',&<cellfont)
wd^:(-.IFJ6) 'pshow'
0 2$a:
NB.?lintsaveglobals
)

NB. *** RTF stuff

selectedfont =: 'Courier New'
selectedsize =: '18'


NB. Return RTF prefix;suffix
rtfprefsuff =: 3 : 0
pref =. '{\rtf1 ',LF
pref =. pref , '{\colortbl\red0\green0\blue0;\red255\green0\blue0;\red0\green0\blue255;\red0\green160\blue0;}',LF
pref =. pref , '{\fonttbl ',LF,'{\f0 ',selectedfont,';}',LF,'}',LF
(pref , '\ucl\f0\fs',selectedsize,' ',LF);'}'
)

NB. y is a color number, result is RTF to set that color
rtfcolor =: '\cf' , ' ' ,~ ":

NB. x is the tokenized sentence
NB. y is a list of token numbers to be highlighted
NB. Result is RTF for the sentence, with highlighted tokens bold and colored
NB. Currently stubbed out so as not to need RTF
createsentencertf =: 4 : 0
tok =. <: y
NB. If a token begins with .:, prefix it with a space
sentence =. ' '&,^:('.:'e.~{.)&.> x
; sentence return.
NB. Escape \ { } by prefixing with \
sentence =. ([: }. (1 j. 0 ,~ e.&'\{}') #!.'\' (' '&,))&.> sentence
NB. Create list of start tokens: highlight values where the previous valus is not highlighted
starttok =. tok -. >: tok
NB. Create list of end tokens: highlight values where the next value is not highlighted
endtok =. tok -. <: tok
NB. Put highlighting into the appropriate tokens
(rtfprefsuff '') enclosing ; endtok ,&(rtfcolor 0)&.>@{`[`]} starttok (rtfcolor 1)&,&.>@{`[`]} sentence
)

NB. *** end of RTF

NB. Clean up everything.  y is the return value to use
destroy =: 3 : 0
NB.?lintmsgsoff
for_o. ~. objtable do. destroy__o '' end.
NB.?lintmsgson
resultroot =: 0$a:
if. #winhwnd do.
  destroy__parsegrid''
  wd 'psel ' , winhwnd
  wd 'pclose'
  winhwnd =: ''  NB. not required
end.
NB. Restore original session state.  We hope this hasn't been changed while we were running!
9!:7 (0) {:: Jenvirons
9!:39 (1) {:: Jenvirons
codestroy''
y
)

NB. The 'show error' button displayes the error state
dissect_showerror_button =: 3 : 0
traversedown__resultroot ($0);selectall
showgrid''
)

dissect_close=: destroy

dissect_minfont_select =: 3 : 0
NB.?lintonly minfont_select =. '0'
minimumfontsizex =: 0 ". minfont_select
showgrid''
)

dissect_maxfont_select =: 3 : 0
NB.?lintonly minfont_select =. '0'
maximumfontsizex =: 0 ". minfont_select
showgrid''
)

dissect_maxnounsize_select =: 3 : 0
NB.?lintonly maxnounsize_select =. '0'
maxnoundisplaysizex =: 0 ". maxnounsize_select
showgrid''
)


NB. Grid event handler, called in the locale of the major instance
parsegrid_gridhandler =: 3 : 0
NB.?lintonly Row__parsegrid =. Col__parsegrid =. Cell__parsegrid =. 0
try.
  NB. Get the index to the cell
  select. y
  case. 'click';'dblclick';'rclick' do.
    cindex =. <Row__parsegrid,Col__parsegrid
  case. 'change' do.
    cindex =. <Cell__parsegrid
  case. ;: 'edit changed' do.
    1 return.
  case. do.
    0 return.
  end.
  if. #cindex do.
    if. #h =. cindex {:: callbacks do.
      ". 'h =. ' , h
      NB.?lintmsgsoff
      h y
      NB.?lintmsgson
    else.
      0
    end.
  else.
    0
  end.
catch.
  smoutput 'error in gridhandler'
  smoutput 'locale: ' , >coname''
  smoutput 13!:12 ''
  smoutput (9!:8 '') {~ <: 13!:11 ''
  0
end.
)
 
NB. common routines used by the object locales.  Objects are subclasses of dissectobj

cocurrent 'dissectobj'
coinsert 'dissect'

NB. Object creation.  create the signaling variables used for communicating with the grid.
NB. y is the tokens that these values came from
NB. Each verb-type object is responsible for creating:
NB. stealthoperand: at create time, this is set to 0 for normal verb, 1 for verbs that have no display (][), 2 for [:
NB. valence: when the verb gets a valence (i. e. when its monad/dyad exec happens), that valence is saved.  Calls during
NB.  traverseup and traversedown use the valence, making a complete traversal
NB. dispoperands: when valence is set, dispoperands is set to indicate which operand(s) are actually used by the display
NB.  of the verb.  The shape is ,valence.  Every caller to creategridobj must ensure that only the used operands are
NB.  sent (this to avoid the chance that a node would become a label node and then later be deleted for non-use).  We
NB.  note that only dyads ever have ignored operands.
create =: 3 : 0
NB. gridclickedr: the result was clicked
NB. griddclickedr: the result was double-clicked
NB. gridclickede: the entity was clicked
NB. gridusersel: what the user typed for a selection
tokensource =: ~. > 0 { y
detaillevel =: 0   NB. Init no detail
gridcommand =: 0 0
gridusersel =: ''
stealthoperand =: 0   NB. set for verbs that have no display, viz [ ] [:
NB. errorcode =: 0   NB. Set to 1=length error 2=framing error 3=some other crash if we traverse for crash
resultissdt =: 0  NB. Set if the result of this object is derived wholly from SDTs
NB.?lintonly valence =: 1
NB.?lintsaveglobals
''
)

NB. Object destruction
destroy =: 3 : 0
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

NB. *************** traverse up **************

NB. Default operations for traverseup
NB. the fetch from this table is (command,prev detaillevel)
NB. Result is new detaillevel
NB. command is _2=remove verb detail _1=remove all detail 0=nop 1=add select detail 2=add verb detail
traverseuptblloc =: _8 ]\ (_2 bwand i. 8) , (2 bwor _2 bwand i. 8) , (4 bwor _2 bwand i. 8) , 0 0 2 2 0 0 2 2 , (8 # 0)

NB. verb to combine two traverseup streams into one
NB. used when a verb with valence 1 splits into multiple click streams (as in hook, fork, ~)
NB. Only one stream should be active, but just in case not, we don't bwor, but instead pick a nonzero
NB. y has shape 2 2, result must have shape 1 2
traverseupmergestreams =: ,:@((0={.)`]})

NB. Main logic for click support.  This handles the action at a single cell.
NB. y is the forwarded information from the cell below:
NB. (prev cell displays detail),(command _2=remove verb detail _1=remove all detail 0=nop 1=add select detail 2=add verb detail)
NB. Result is the values to be forwarded up from this cell (same shape & values as y)
NB. We manage the globals gridcommand, detaillevel, gridusersel
NB. gridcommand is set by grid clicks, and contains (command for this cell),(command to forward).  We clear it to 0 0 after use
NB. detaillevel bit 0=display detail for lower level (shape), bit 1=display selector detail, bit 2=display verb detail (only in verbs)
NB.
NB. The wrapper verb, traverseup, will return one row for each of its operands
traverseuprollup =: 3 : 0"1
assert. (,2) -: $y [ 'travrollup'
'prevdisp prevcommand' =. y
assert. -. +./ prevcommand *.&:(0&~:) gridcommand
NB. debug qprintf 'defstring{.0 prevdisp prevcommand gridcommand detaillevel '
NB. Save commands to use here, combined from the passed-in command and the local commands; and then clear the command after use.
NB. If this node is already unselected, do not propagate an incoming unselect command (but allow origination
NB. of unselect commands here: that happens on a double-click, where the click unselects and the dclick propagates)
'gcloc gcfwd' =. gridcommand + prevcommand NB. >. _2 , (detaillevel>1) { 0 _2  Pass all commands up
gridcommand =: 0 0
NB. Get the command to use: the command input, plus any command stored at this node.  Only 1 will be set
NB. Using the old detaillevel and the command input, get the new detaillevel.
NB. Add in the previous level's request for local detail
detaillevel =: prevdisp bwor (<gcloc,detaillevel) { traverseuptblloc
NB. If we are not displaying detail, clear any outstanding usersel
if. detaillevel < 2 do. gridusersel =: '' end.
NB. Get the command to forward to the next level: the command input plus any pass-on command stored at this node.
NB. We pass on the selected status of this node, and the pass-on command, EXCEPT if the pass-on command is _1 (unselect)
NB. and this node was already unselected, stop the unselect chain
NB. debug qprintf 'detaillevel gcfwd '
(2 <: detaillevel) , gcfwd
NB.?lintsaveglobals
)

NB. ***************** traverse down ****************

NB. Selection calculation for traversedown
NB. If there are input selectors, we see if this node qualifies the input still
NB. further.  We calculate the level to be used for the next selection (which is incremented from
NB. the current level if this level makes a selection), and the new selection, including the
NB. selected operands and the selected result.  We leave a lot of stuff lying around in globals for
NB. use by creategridobj; what we return is the selector we use at this node (if any) and a flag
NB. indicating whether the result of that selection is a singleton (which means we can collect it)
NB.
NB. If sellevel is empty, it means we are doing the initial selection to find where the crash was.
NB. When there is a selection, we always select the last thing that happened.
NB. y is a single list with 0-4 items depending on selection status.  When operands are given, it is implied that
NB. the selector is selecting a single application of the verb.
NB.
NB. As a side effect, if the detaillevel of the current locale is 2, we append the token numbers of this locale
NB. to the list of highlightable tokens.  We do this here because we don't want to highlight paths that are not traversed
NB. because of failure
NB.
NB. Object globals set here are:
NB. collected - set if the input selector to this verb selected a single input cell (i. e. frame was empty or all 1)
NB.   when this is set, we know that even if there are multiple results, it must be valid to try to collect them
NB. isselvalid - set when the input selected multiple cells (provided we are given operands).  This means that
NB.   when the verb is selected, we should offer a selector to trim the path
NB. frame - the frame of the verb
NB.  frames - the individual franes, boxed
NB. errorcode - An indication of the result of applying the selection to the results of the verb, and comparing it to the
NB.  frame of the operands.  Values > 1 indicate error, 0=OK.  The errorcode gives the
NB.  status of the input values passed to this verb, BEFORE any local selector is applied.
NB.  Values: 1=agreement error 2=unexecuted cell (inferred in u operand when u&v both have exec errors) 3=execution error (this verb did not compute all the selected results)
NB.  4=framing error (error during collection)  Every collectible result is collected
NB.  and displayed.
NB. islocalselgiven - An indication of the validity of the local selector, i. e. the selector that is added on in this cell
NB.   and applied to the u and v of this cell.  A local selector does not have any effect outside the compound it is
NB.   generated in.
NB.   0=no selection made, 1=selection invalid, 3=selection included the first nonexecuted cell, i. e. is where execution blew up
NB.   5=selection included cells AFTER the first nonexecuted cell, i. e. unexecuted inputs 7=selection is entirely valid
NB. selresult - the result of applying the selection to the given operands.  selresult is valid only if the result is
NB.   collectible, and is the result after opening - we open it so we can make sure it collects properly, and we set the errorcode
NB.   if it didn't.  We have to do this to make sure uncollectable results are not passed into u operands.  Any single selection is
NB.   known to be collectable - multiples go through the more-than-one-selection path
NB. collectedshape - when we open selresult for collection, we calculate the frame/shape of the result and
NB.   whether there was fill.  This is scooped up when the display record is created.  This is valid even if there was a framing error
NB. selector - the selector to use for subnodes of the current node, created by applying the local selector to the selector
NB.  given in the input arguments.  If there is an early error (agreement), or if the local selector gives an error,
NB.  the selector is an empty list, which will suppress further analysis.  If the selector contains multiple ranges, we assume that
NB. we are just waiting for a final collection
NB. selops - The input operands after going through the local selection.  These will be valid if selector is nonempty.
NB.  these are the operands passed down the tree.  If input selector specified a multiple selection, selops will be empty.  The result of this
NB.  is that a multiple selection will let one valid selops out (it will be displayed in the initial selector) and then
NB.  nothing will happen until the final collector.  Selops is a table, with one row per operand and 0 or 1 columns depending on validity
NB. localselresult - The result of this verb after culling using the local selector (unopened).  Used in verb display when
NB.  there is a qualifier
traversedowncalcselect =: 3 : 0
assert. 1 = #$y
snifferror =. 0=#sellevel =: > {. y
selops =: ,. }. selandxy =. }. y
NB. append to the list of highlighted tokens
if. detaillevel >: 1 do. highlighttokens__COCREATOR =: highlighttokens__COCREATOR , tokensource end.
NB. debug qprintf 'snifferror%,loc=?>coname''''%defstring 0%>uop%>vop%>cop%detaillevel%$y%y%'
if. 0 = #selandxy do.
  NB. No selector: we can't do much
  'collected isselvalid frame frames errorcode islocalselgiven selresult selector selops localselresult' =: 0;0;($0);a:;0;0;'?';(0$a:);(1 0$a:);<(0$a:)
elseif.
selector =: {. selandxy
(1 = #selandxy) +. 1 < */ }: $ > {. selandxy do.
  NB. Just a selector, but no operands.  Must be an active multiple selection, or we just haven't hit operands yet.
  NB. We will not come through here if we are sniffing errors
  NB. Select the derived verb results, using the shape of the selector as the frame.
  NB. If the frame matches the results, we can call this a collector and collect and display the results.
  frame =: }:$>{. selandxy
  selresult =: (; findselection > {. selandxy) { logvalues  NB. fails if no values
  errorcode =: 3 * (*/ frame) > #selresult
  collected =: (*/ frame) = 1
  'isselvalid frames islocalselgiven selops localselresult' =: 0;a:;0;(0 0$a:);<(0$a:)
  NB. We can pass the selector to u, which will collect; but not to v
elseif. do.
NB.?lintonly selresult =: localselresult =: <'?'
  NB. This is the main line.  We still have operands, which means that we are selecting a single application of
  NB. the current verb.  We will inspect the operands to check for early error; then we will get the selected results
  NB. and shape them according to the operand shapes.  This produces selresult, frame, and frames. At that time, we will check for aborted execution.
  NB. After we have accounted for operands and results, we will validate the local selector, and apply it to
  NB. the operands and results to produce selops and localselresult.
  NB.
  NB. We know that we MUST be processing a single selection and can therefore collect any result that comes out of this path
  islocalselgiven =: 0   NB. Init to no local sel
  assert. 1 = */ }: $ > {. selandxy [ 'travdowncalcselect'
  NB. If snifferror is set, we will automatically produce a local selector to zero in on the error.
  selr =. (selx =. ; findselection > selector) { logvalues
  opshapes =. , $&.> selops  NB. Inputs to derived verb
  frame =: >./ > frames =: (- vranks =. (verbex b. 0) {~ (i. + <:) (#selops)) }.&.> opshapes  NB. frame meaningful only if no framing error
  NB. Audit the frames.  They should agree. and they should match the number of results we got.
  NB. If the frame is invalid, we know that this verb is going to die before it executes; indicate agreement error
  NB. Clear the selector to short-circuit further processing
  if. -. -:/ (<.&#&>/ {.&> ]) 2 {. frames,<$0 do.  NB. No agreement error on monad
    'collected isselvalid errorcode islocalselgiven selector selops selresult localselresult' =: 0;0;1;0;(0$a:);(0#"1 selops);'?';<(0$a:)
    if. snifferror do. detaillevel =: 7 end.
  else.
    NB. Assume the verb completed unless we learn otherwise
    errorcode =: 0
    NB. The verb started execution.  We can always collect it.
    collected =: 1
    NB. Also, the number of results should match the number of cells in the frame, except
    NB. when the frame contains 0, in which case there will be 0 or 1 result.
NB. debug qprintf '$selops selops frames $frame frame $selr selr ' 
    if. 0 e. frame do.
      NB. Execution on a cell of fills.  We should have 0 or 1 result.  If 0, it means that the
      NB. execution on the cell of fills failed, and we will use a scalar numeric as the replacement
      assert. 0 1 e.~ #selr
      if. 0 = #selr do. selr =. <0 end.   NB. error in fill cell - use scalar numeric
      NB. create a result of the required type and shape
      selresult =: {. selr  NB. we will extend fill-cell with frame
      NB. Replace selops, which will be used by lower verbs, with a cell of fills.  But don't set islocalselgiven,
      NB. so there will be no selectors or formatting - just a new operand
      selops =: vranks ({.@,@] {.~ (((-@(<. #)) {. ])   $))&.> selops
      NB. Keep selector unchanged, since there was just one cell in the operand and there still will be
    else.
      NB. Not fill cell.  If there is no error, we should have just the right number of results
NB. TEMP kludge!!  Error is not fatal, is we are in adverse or chasing a fill-cell.  Only too many results is always bad
      assert. ((*/ frame) > #selr) +. (*/ frame) = #selr [ 'travdowncalcselect'  NB. used to include crashed__COCREATOR
      NB. If the frame is valid, but we didn't get enough results, it means something died in this verb;
      NB. mark this verb as requiring detail and set the selector (if any) to select the failing item, which
      NB. will be the first item we did NOT create.  OR, it could mean that we are executing on a cells of fills, which
      NB. might terminate with a error, which would be ignored.
      NB. See if all the cells executed
      if. (*/ frame) > #selr do.
        NB. Cells did not execute.  Presumably the verb crashed.  Mark this cell as an execution error
        errorcode =: 3
        NB. If we are sniffing for errors, set detaillevel to show that this path has the error, and
        NB. if there is a frame, sutoset the selector to point to the first unexecuted cell
        if. snifferror do.
          detaillevel =: 7
          NB. If there is a frame, select the first non-executing cell
          if. 1 < */ frame do.
            gridusersel =: }. ; (';' , ":)&.> frame #: #selr
          end.
            NB. We rely on an important observation: if u@v selects a single result cell, we can be
            NB. sure that v (in the next level of traversal) will select to a single input cell.  Therefore,
            NB. if vcollected is set (meaning only 1 input), the next level will not have selection enabled,
            NB. since they will perforce have a single input cell.
            NB. This will make us fall through to the islocalsel processing below, which will properly handle
            NB. the case where we select the cell in error
        end.
      end.
      NB. If the cell executed fully and is collectible, we need to save the selected value.  We use this to calculate
      NB. the predicted frame after collection
      selresult =: {.^:(0=#)selr  NB. This is the (unopened, since it might not collect) result from this object's verb
    end.

    NB. Calculate the selection interval corresponding to each selected result.  Box each one so
    NB. that when we select, we will get a length error if selection goes too deep.  Bracket the
    NB. intervals with the start & end of the selector so that we create one extra interval that works
    NB. in case of crash, to get the inputs corresponding to the nonexistent last result
    newsel =. frame $!.(<1 2$0) allsel =. 2 <\ (,>selector) enclosing selx { logticket

    NB. If the selector trims down the selection of results, apply that trim to the selectors,
    NB. the results, and the selected operands.  If we are sniffing and this verb failed, the final selection would fail
    NB. by definition; we will have handled that case above.
    if. *#gridusersel do.
      islocalselgiven =: 1
      NB. If the user (or us, above) gave a selector, use it.
      try.
        NB. Get the user's selection, and convert to canonical double-boxed form
        NB. If it is a list of boxes, treat it as axes and add one box.  If it is a single box,
        NB. also treat it as axes by boxing inside
        usel =. (<@<)`(<"0&.>`<@.(1<#))`]@.(2 <. L.) ". gridusersel
        if. #$usel do. 13!:8 (1) end.  NB. After canonizing, usel MUST be a scalar box

        NB. Apply the user's selection to the selector.
        unewsel =. usel { newsel  NB. This is where it would crash on error

        NB. If the selection didn't change anything, treat it as if omitted
        if. unewsel -: newsel do. islocalselgiven =: 0
        NB. If this leaves nothing selected, treat it as error
        elseif. 0 e. $unewsel do. islocalselgiven =: 2
            NB. No failure possible in the following
        elseif. do.
        end.
          NB. selops and bresultafterlocalsel will hold values after the local selector
          NB. Now that we have a valid selection, make that the value we return
      catch.
        NB.?lintonly usel =. unewsel =. newsel
        islocalselgiven =: 2
      end.
      NB. Now islocalselgiven = 0 if no selection, 1 if valid selection, 2 if invalid
      NB. Now, if the selector is valid, see how well it selects.  Try selecting from
      NB. the inputs and the result, and make a note of which ones worked.
      if. 1 = islocalselgiven do.  NB. Still OK
        selector =: <>unewsel   NB. Use this selector
        NB. If (owing to error) we had missing results, newsel will have had some flag values
        NB. inserted: {:allsel is a selector for anything following the last valid result, and
        NB. is a proxy for the input cell that led to error.  The other flag value is (<1 2$0), which
        NB. stands for a cell for which there is neither valid input nor valid output (i. e. if
        NB. it is an input, is was never executed).  Look for these in unewsel, and turn on
        NB. corresponding bits in islocalselgiven to indicate validity.  A setting of 'input unexecuted'
        NB. and 'no output error' means we selected an input that was PAST the error
        islocalselgiven =: islocalselgiven + +/ 2 4 * -. ((<1 2$0),{:allsel) e. , unewsel
        NB. Now make the selection.  This cannot fail, since selops and bresultafterlocalsel are both full-size,
        NB. and usel was vetted above.
        NB. For the operands, we need to handle cell repetition for agreement.  We don't repeat cells
        NB. is the display; we show the cells and know that they will be repeated as necessary
        selops =: selops ({~ <)&.> usel (<.&# {. [)&.>  frames
        NB. If the selection is valid, select the cells
        if. islocalselgiven = 7 do. localselresult =: usel { frame $ selresult end.
      else. islocalselgiven =: islocalselgiven <. 1  NB. Change 2 to 1, indicating error
      end.
    elseif. -. 0 e. frame do.
      selector =: <>newsel
    end.
    NB. Now islocalselgiven is: 0=not given 1=yes,invalid 3=yes,input only valid 5=yes,unexecd input 7=yes,all valid
    NB. If we still have a selector, note that we will be allowed to use it
    isselvalid =: 1 = #selector
  end.
end.
NB. Now selresult contains the unopened and unframed results.  frame contains the frame.  We will calculate a faux
NB. shape for the result, by looking at the values without opening selresult.  If they have a common
NB. shape, we will show that shape after the frame.  If the cell collected, and there was no error
NB. (meaning they all ran), we will create the opened version of selresult, which is what we will use later.
NB. It is valid only if errorcode=0 and collected=1.

NB. Calculate the frame display for the current verb, without requiring it to collect properly
NB. Extract the shape of the operand, reversed
if. 0 e. $selresult do.
  bshp =. ($selresult)$a:
  shp =. $0
else.
  bshp =. |.@$&.> selresult
  NB. Calculate the shape
  shp =. |. >./@:>@, bshp
end.
NB. See if there will be fill when the cells are opened
filled =. 1 < # ~. (>./ #@> bshp)&({.!.1)@> bshp
collectedframe =: 's' [^:(0=#frame,shp)   (filled#'*') ,~ (":frame) , '/' , ": shp
if. collected *. (errorcode=0) do.
NB. debug qprintf 'collected%frame%$L:0 selresult%selresult%y%'
  try.
    selresult =: > frame $ selresult
  catch.
    errorcode =: 4   NB. If framing error, so indicate
    if. snifferror do. detaillevel =: 7 end.
  end.
else.
  NB. if this node is not a collector, we must clear selops since they are invalid for future use
  selresult =: '?'
end.
NB. If this level is selectable, increment the selector level to use for this and subsequent levels - whether
NB. we have a selector yet or not.  If this level is selectable, and we didn't qualify it down to a single input,
NB. no selection will be possible at lower levels, no matter what happens later.  So we might as well assume that
NB. we qualified, and we just add one to the next level if selection here was possible.
bnsellevel =: < MAXSELLEVEL <. sellevel + 1 < */ frame

NB. debug qprintf 'collected errorcode islocalselgiven detaillevel frame $selresult selresult selector $selops selops '
NB.?lintsaveglobals
)

NB. y is errorcode(s), result is true if it is OK for child nodes to select
travdownselok =: -.@(+./@:e.&1 2)
NB. y is vdisplayrcd(s)
NB. Result is operands to pass on to traversal of v
NB. If v had an error, we wipe out the selector and the operands
NB. If v didn't collect, we don't use its operands
travdownuops =: 3 : 0
'selok valsok' =. *./\ (travdownselok errfromdisplayrcd y) , collected
bnsellevel , (valfromdisplayrcd y) ,~^:valsok   selok # selector
)

NB. ************* stuff for creating the grid display ************

NB. Build a cell to go into the grid object
NB. y describes the cell:
NB. text;colorname;fontname[;locale;callback verbname[;referencenumber]] if given; it it begins with 'edit', the cell is editable
NB. The format of a cell is:
NB. <text;(color,font,editable,reference info);verbname including locale if editable
NB. For references, reference info is   ref#,ref,detail  where
NB. the reference number is unique for all cells sharing the same data; label is 0 in the labeled cell, 1 in references;
NB. detail is 0 if the data is the reference letter (which can be removed if arrows are drawn), 1 if it is detail that should always be displayed
NB. We use the current locale's setting of detaillevel etc. to decide how much data to show
creategridcell =: 3 : 0"1
NB.?lintonly minimumfontsizex__COCREATOR =. maxnoundisplaysizex__COCREATOR =. maximumfontsizex__COCREATOR =. 0
'text cname fname loc cv ref' =. y , (#y) }. '';'';'';'';'';0 0 0
if. 0 = #loc do. loc =. >coname'' end.
NB. format the text specially, for different types, detected by fname
select. fname
case. 'shape' do.
  if. 0 = #text do.
    text =. 's'
  end.
case. 'error' do.
  if. 0 = #text do.
    text =. 'error'
  end.
case. 'invalid' do.
  if. 0 = #text do.
    text =. 'invalid'
  end.
case. ;: 'value' do.
  if. 0 e. $text do.
    text =. '(empty)' , (1 ~: #$text) # LF,'$=',":$text
    fname =. 'shape'
  end.
end.
NB. For 'value' types, which contain potentially large stuff,
NB. convert the text to character form, choosing the correct font size, and eliding text that is too large
if. 'value' -: fname do.
  NB. Find out how large a box (in characters) we are allowed, based on detaillevel and other settings
  NB. allcharwh is the character capacity of each font size
  'maxw maxh' =. maxcharwh =. minimumfontsizex__COCREATOR { allcharwh =. <. (0.01 * (detaillevel>0) { 10,maxnoundisplaysizex__COCREATOR { MAXNOUNPCTCHOICES) * fontcharsizes__COCREATOR
  NB. If it won't fit vertically, convert each item individually and choose
  NB.  as many items as will fit, or, if only 1 can, remove leading axis. Repeat
  NB.  till it fits or there are no more axes to remove
  nl =. +/&.:<:@:(*/\)@}:@$  NB. number of lines in display of a non-empty array.  We don't care about empties here
  vclip =. 0  NB. Inidicate we have not truncated vertically
  while. maxh < lct =. nl ": text do.
    if. 1 >: #$text do. break. end.
    vclip =. 1
    if. 1 <: maxx =. <. (maxh%lct)*#text do.
      text =. maxx {. text break.
    end.
    text =. {. text 
  end.
  NB. Convert to character table
  text =. ,:^:(0 >. 2 - #@$) (,/)@:(,&' '"2)"3^:(0 >. 2 -~ #@$) ": text
  NB. If the result still won't fit vertically, truncate it to the size of the box
  if. maxh < #text do.
    vclip =. 1
    text =. maxh {. text
  end.

  NB. Insert vertical ... to indicate missing stuff
  if. vclip do. text =. text , '...' end.

  NB. If horizontal truncation, insert ... on each line
  if. maxw < {:$text do. text =. (maxw {."1 text) ,"1 '...' end.

  NB. Pick font size to use, based on final size.  Use biggest font that fits
  fname =. fname , ": minimumfontsizex__COCREATOR >. maximumfontsizex__COCREATOR <. i:&1 (1) 0} (|.$text) ([: *./"1 <:"1) allcharwh
end.
<(": text) ; ((colornames i. <cname) , (fontnames i. <fname),('edit' -: 4 {. cv),ref) ; (,&('_' , loc ,'_')^:(*@#) cv)
)

NB. Create a reference, and perhaps a label
NB. y is a grid specifier,<character argument for 'clickref'.  Grid actions will toggle the
NB. cell (prefix,'refdetail')
NB. x is the value to display if detail is requested (which we check using the value 'refdetail'
NB. Result is 2 grid specifiers: first is the replacement for y, second
NB. is a new grid specifier for the reference.
NB. If the last thing in y's gridobj is a reference or label, we don't
NB. do anything to y, we just use its reference number and create a reference to it
NB. for the result. 
NB. Otherwise we append a label to y and then make the reference.
creategridref =: 4 : 0"_ 1
NB.?lintonly referenceticket__COCREATOR =. 0
'lgobj linof lrsof clickpfx' =. y
rdet =. ". clickpfx,'refdetail'
NB. If y ends with a reference, extract the reference #
if. 0 = refno =.(_1 _1;1;3) {:: :: 0: lgobj do.
NB. If y does not end with a reference, assign a new number and create a label
  refno =. referenceticket__COCREATOR =: >: referenceticket__COCREATOR
  lgobj =. lgobj gridstackright@,&< nc   =. creategridcell '';'label';'label';'';'';refno,0,0
end.
NB. Create a reference for the new label and return it as a gridspec
rgobj =. creategridcell ('' [^:(-.rdet) x);'reference';'value';'';(('''' enclosing clickpfx),'&clickref');refno,1,rdet
((<lgobj) 0} 3 {. y) ,: (1 1 $rgobj);($0);_1
)

NB. x is a grid object, y is offset to inputs, possibly neg
NB. result is the absolute positions of the inputs in the first row
gridobjinpos =: 4 : 0
({:$x) | y
)

NB. y is a table of 1 or 2 grid specifiers
NB. Result is the minimum distance between the result cells, after they have been
NB. squashed together as much as possible
gridabutxy =: 3 : 0
if. 2 > #y do.
  $0
else.
  'xgobj ygobj' =. 0 {"1 y
  NB. Find the minimum possible spacing between the x and y values, by seeing how far the
  NB. xop can slide into the rightop while leaving an empty space.
  NB. Calculate the rolled right margin of the xop.  This is, for each row, the number of
  NB. cells that can be encroached on from the right, without making nonempty cells into
  NB. diagonal neighbors.  This will generally be 0, indicating that the right side is
  NB. full.  We have to account for inputs and outputs, which would occur on the
  NB. right (kludge - we could use the result and input offsets)
  xrmgn =. 3 <./\ 0 0 , 0 ,~ xgobj ({:@$@[ (<:@[ - |) i:&1@:~:"1) a:
  NB. For the y values, find the number of encroachables on the left. This, plus the number of
  NB. empties on the right, is the amount of slack in each row.  The minimum amount of slack,
  NB. across all rows, is the amount of overlap permitted between the rows.  This is deducted
  NB. from the width of yop to get the minimum spacing.  Deduct 1 cell of slack to leave a gap
  NB. The last row of y is special: it is the result, plus one optional shape indicator which
  NB. we will ignore for spacing purposes
  ylmgn =. (<:{:$ygobj) _1} ygobj i.&1@:~:"1 a:
  ylmgn =. ygobj i.&1@:~:"1 a:
  ({:$ygobj) - <: <./ xrmgn +&((- xrmgn <.&# ylmgn)&{.) ylmgn
end.
)

NB. x is grid specifier for u
NB. y is 2 grid specifiers, for operands to u
NB. Result is the grid specifier for the combined xy, combined using the spacing
NB. given by the u object
gridjoinxyspacingu =: 4 : 0
'xgobj ygobj' =. 0 {"1 y
'ugobj uinof' =. 2 {. x
NB. Calculate left-right spacing: x goes where the input says to go; and if y
NB. is bigger than that, make sure we have 
totlxywid =. ({:$ygobj) >. xltoyr =. ({:$xgobj) + -~/ uinpos =. ugobj gridobjinpos uinof
totlxyhght =. xgobj >.&# ygobj  NB. We align at the bottom
xygobj =. (xltoyr ,~ -totlxyhght) {. xgobj  NB. x op wrt right edge
ygobj =. (-totlxywid ,~ totlxyhght) {. ygobj  NB. y op
xygobj =. (-totlxywid ,~ totlxyhght) {. xygobj  NB. x op wrt left edge
xygobj =. (a: = xygobj)} xygobj ,: ygobj
NB. The result offset comes from the u input offset, adjusted for
NB. the difference in size between u and the result
rsof =. uinpos + totlxywid - {:$ugobj
xygobj;rsof;rsof
)

NB. x is grid specifier for u
NB. y is grid specifier(s), for operands to u
NB. Result is the grid specifier for the combined xyu, combined using the spacing
NB. given by the u object
gridjoinxyu =: 4 : 0
if. 2 = #y do.
  'xygobj xyinof xyrsof' =. x gridjoinxyspacingu y
else.
  'xygobj xyinof xyrsof' =. {. y
end.
NB. If you need to support uinof ~: _1, do it here
(gridstackright xygobj;{.x);xyinof;_1
)

NB. y is a list of boxes, each containing a gridobj.  Result is one big grid object, aligned right
gridstackright =: (;@(({."1&.>~ -) >./@:({:@$@>)))"1

NB. *** this is where a result row gets added to the gridobj ***

NB. Create the display record used by condcreateselection, by scooping up globals
NB. y is any previous displayrcds that have not been put out, i. e. displayrcd for
NB. us that are awaiting collection.  Many may collect simultaneously.
NB. If this selection is not collecting or if it had a pre-framing error,
NB. don't look at frame.
selectiondisplayrcd =: 3 : 0
res =. (collected;selresult;islocalselgiven;detaillevel;errorcode;sellevel) , (6}.y) , <collectedframe 
NB. If there is an error code in the old record, carry it through to the new;
NB. also ignore any frame in the new
if. #y do.
  if. * 4{:: y do.
    res =. }: (4{y) 4} res  NB. keep old error code, ignore new shape
  end.
end.
res
)
NB. Extract the operand from a display record, for use in continued selection
valfromdisplayrcd =: 1&{"1

NB. Extract and open the error code from a display record
errfromdisplayrcd =: 4&{::"1

NB. Extract and open the collector flag from a display record
collfromdisplayrcd =: 0&{::"1


NB. If there is an error in u and v, give priority to v
NB. y is a table of 2 display records (if it's only one, we just leave it alone)
NB. If both records show error, we change u's error to 'unexecd', because v
NB. is known to execute first & therefore must have died before u
displayrcdfixuverror =: 3 : 0
if. 2 = #y do.
  if. *.&(e.&1 2 3 4)&(4&{::)/ y  do.
    (<2) (<0 4)} y
  else. y
  end.
else.  y
end.
)

NB. If inputs are selectable, add a selector row to them
NB. The selector row has a place where the user fills in the selection, and
NB. 1 or 2 places where the selected output is shown
NB. y is a table of gridspecs for the input(s)
NB. Result is the same table of gridspecs, with rows added as required
NB. We expect the selection globals have been defined
NB. Modify this if inputs are not always _1
condcreateinitialselector =: 3 : 0
NB. If the cell is selected for display, and selection is possible -
NB. we have selectors, and more than 1 result is selected -
NB. create a selection block.  If the user has given a selection, use it
NB. Get the labels to use for selection cells added at this level
NB. The SELECTOR is for the level before the SELECTION
NB. We put the selector in each operand, because we can't tell which operand might
NB. be deleted by an error somewhere in the other stack.  We will remove
NB. the duplicate editselectors before display
'sorlbl svalbl' =. ('selector';'result') ,&.> ":&.> 0 1 + sellevel
NB. Create the selector if there is something to select and we are showing detail, or if there is an early error.
NB. debug qprintf 'detaillevel errorcode isselvalid frame '
if. (* 2 bwand detaillevel) *. (errorcode e. 1) +. isselvalid *. (1 < */ frame) do.
  NB. next, the result cell(s)
  if. errorcode e. 1 do.
    NB. Since we won't be deleting either side, show just one (disabled) selector
    selsel =. (-#y) {. creategridcell gridusersel;sorlbl;'selector'   NB. No editing allowed for early error
    NB. If there is an early error, show it
    seld =. creategridcell 'agreement';'error';'error'
  else.
    NB. Not early error; display the selector and allow editing
    selsel =. creategridcell gridusersel;sorlbl;'selector';'';'editselector'
    NB. Display the current status of the selector
    select. islocalselgiven
    case. 0 do.
      NB. No selector yet; show the frame and wait for input
      seld =. creategridcell ('frame=',":frame);svalbl;'shape'
      if. 2=#y do. seld =. seld ,~ creategridcell ' ';svalbl;'shape' end.  NB. Use space to avoid 'empty'
    case. 1 do.
      NB. Invalid selector
      seld =. creategridcell 'invalid';'invalid';'invalid'
    case. 3;7 do.
      NB. Selector is valid, though it may have failed.  Display the selected value
      seld =. creategridcell selops ,"1 svalbl;'value'
    case. do.  NB. Must be 5
      NB. Selector selected unexecuted cells.  That's invalid
      seld =. creategridcell '(unexecd)';'invalid';'invalid'
    end.
  end.
  NB. Add the selection row
  (<@gridstackright ({."1 y) ,"0 selsel <@,"0 seld) (<a:;0)} y
else.
  NB. No initial selection allowed, leave input unchanged
  y
end.
)

NB. y is gridspec
NB. x is selectiondisplayrcd, created for the current locale
NB. m is 0 for v, 1 for u, 2 for final collector.  The difference is that v calls create a selection whenever
NB. detail is enabled and the selector is not given; and final collectors always produce a result
NB. We use in the locale where islocalselgiven and detaillevel are defined.
NB. For v, this is the v locale; for u, it is the locale of u@v.
NB. This relies on the fact that u@v creates a selector for v only when
NB. the selector in u@v selects a single input cell of both u@v and v; which means
NB. that v will never want to do its own selection while it is being displayed
NB. for u@v.
NB. We use the presence of a locale to indicate whether this is is a collector: if
NB. no locale is given, we must be at the end of a selector block, and we enable
NB. the cell for clicks
NB. Result is new grid selection
NB. We used the info from the displayrcd for display; but as for whether we display at all, we look
NB. at the calling locale, which is above u & v.
condcreateselection =: 1 : 0("1 1)   NB. used for selectors & their results
:
gs =. y
'vcall final' =. m = 0 2
'collect val lsel det ecode bsel' =. 6 {. x
frm =. 6 }. x
NB. Create a display record if this is a collector, and either we have a valid
NB. selector, or we have no selector and detail is requested
NB. 
if. final +. collect *. (islocalselgiven e. 3 7) +. vcall +. 0: (islocalselgiven e. 0) *. *(det bwand 2) do.
  if. ecode e. 1 2 3 do.
    NB. unexecd cell or execution error - show the error, don't try to collect
    disptype =. 'invalid'
    dispdata =. (ecode-1){::'agreement';'(unexecd)';'(error)'
    dispshape =. ' '
    dispfont =. 'invalid'
  else.
    NB. OK or framing error - both show the shape
    dispshape =. }: ; (,&LF)&.> ~. frm
    if. ecode = 4 do.
      'dispdata disptype dispfont' =. 'framing';'error';'error'
    else.
      disptype =. ('result' , ":) bsel
      dispfont =. 'value'
      dispdata =. val
    end.
  end.
  cobj =. creategridcell dispdata;disptype;dispfont;'';<collect#'clickresult'

  NB. If we are supposed to show detail, append the shape
  NB. We display detail if the caller is displaying detail, or if the displayrcd showed detail
  NB. This makes detail operate over two levels
  if. 1 <: det >. detaillevel do.
    cobj =. (creategridcell dispshape;'shape';'shape') , cobj
  end.
  (<gridstackright ({."1 gs) ,< cobj) 0} gs
else.
  y
end.
)

NB. y is a selector: any shape, but each selector has shape ,2; so $=?,2
NB. Result is array of boxes with one box for each selector, containing the
NB. indices of the results for each selector
NB. The selector is (interval]
findselection =: 3 : 0
<@([ + i.@-~)/"1 logticket I. >: y
)

NB. ***** Grid actions *****
clickresult =: 3 : 0
NB.?lintonly Ctrl__parsegrid__COCREATOR =. Value__parsegrid__COCREATOR =. 0
select. y
fcase. 'rclick' do.  NB. for Mac, which brings CTRL-click in as rclick, we treat it as crtl-click
  Ctrl__parsegrid__COCREATOR =: 1
case. 'click' do.
  NB. If CTRL pressed, pass the command through the tree
  gridcommand =: 2 $ ((* detaillevel bwand 2) { 1 _1) , (-.Ctrl__parsegrid__COCREATOR) # 0
  showgrid__COCREATOR ''
end.
0
)

clickverb =: 3 : 0
select. y
case. 'click' do.
  gridcommand =: 0 ,~ (* detaillevel bwand 4) { 2 _2
  showgrid__COCREATOR ''
end.
0
)

NB. Click on a reference toggles the call for refdetail in the locale that created the reference
clickref =: 4 : 0
select. y
case. 'click' do.
  NB.?lintmsgsoff
  (x,'refdetail') =: -. (x,'refdetail')~
  NB.?lintmsgson
  showgrid__COCREATOR ''
end.
0
)

editqualifier =: 3 : 0
NB.?lintonly Ctrl__parsegrid__COCREATOR =. Value__parsegrid__COCREATOR =. 0
select. y
case. 'change' do.
  gridusersel =: Value__parsegrid__COCREATOR
  gridcommand =: 0 ,~ *#gridusersel
  showgrid__COCREATOR ''
  0
case. do.  NB. click, dclick
  1
end.
)
editselector =: editqualifier

NB. join display strings
NB. x and y are strings to create the display form of a sentence
NB. Result is the two strings joined together
NB. We add parentheses if the last word of x and the first word of y are both numeric
jd =: 4 : 0
if. '.:' e.~ {. y do. y =. ' ' , y end.
if. ({: ;: x) *.&('0123456789_' e.~ {.@>) ({. ;: y) do. y =. '(' ([`(' '&(i.&0@:=)@])`(' '&,@]))} y , ')' end.  NB. Replace last space with (
x , y
)

NB. Here are the object locales for creating the parse table
NB. Each object is responsible for responding to the entry points:
NB. create - create the locale and return a proper stack line (type;locale)
NB. execute - set the locales of the noun operands of verbs
NB. defstring - return executable string without instrumentation
NB.  the operand gives the context of use: 0=verb or right op of verb, 1=left op of verb, 2=left op of modifier, 3=right op of modifier 
NB. exestring - return executable string, after instrumentation has been added.  Set locale info
NB.  y is (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 if inverse also needed)
NB. value - return value of noun, or result of verb.  For verb, valid only after execution

NB. The following can only be run after execution of the sentence:
NB. creategridobj - create grid object, with size & handle info
NB. y is minimum spacing between operands
NB. Result is grid specifier for the result
NB.  <(array of boxes holding characters;offsets in first row to inputs;offset in last row of result (always _1))

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
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ yop =: coname''
NB. Tell the verb its valence; the result is the operands that are needed for display.  Here, in this non-verb,
NB. we save the operands needed by the first verb.  The rule is, we will pass to a verb ONLY the operands that
NB. it says it can use.  For comp. ease we may compute an operand but then immediately discard it.
dispoperands =: setvalence__uop resultissdt__yop
resultissdt =: resultissdt__uop
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
enparen^:(y>0) (defstring__uop 0) jd ' ' , (defstring__yop 0)
)

NB. return string form of operands, including instrumentation
NB. y is ignored - always 0 1 1
exestring =: 3 : 0
initloggingtable ''
auditstg '(' , (logstring '') , (exestring__uop 0 1 0) , ' (' , (exestring__yop 0 1 0) , '))'
)

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseup__yop traverseup__uop traverseuprollup y
NB. The result from this level is immaterial, since it never feeds up (it's a noun node)
NB.?lintsaveglobals
)

NB. Tree traversal, down
traversedown =: 3 : 0
NB. Evaluate the noun operand (selecting all, but that shouldn't matter since noun nodes
NB. are always collectors and we will get the collected result); feed that into the verb to allow
NB. selection.
traversedowncalcselect y  NB. To set globals only - there are no inputs here
udisplayrcd =: traversedown__uop bnsellevel,selector , valfromdisplayrcd traversedown__yop bnsellevel,<selectall__COCREATOR
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. Create the gridobj for the noun, join to verb
NB. Create a collector for the result
NB. If this node is a rollup of sdts, don't show the detail unless the user asks for it
if. resultissdt *. detaillevel < 2 do.
  (1 1 $ creategridcell (valfromdisplayrcd udisplayrcd);'input';'value';'';'clickresult');($0);_1
else.
  go =. creategridobj__uop dispoperands # ,: creategridobj__yop y
  (selectiondisplayrcd udisplayrcd) 2 condcreateselection^:(0 = stealthoperand__uop) go
end.
)


cocurrent 'dissectdyad'
coinsert 'dissectobj'
NB. Dyad.  y is the locales of x, u, y
create =: 3 : 0
NB. not clonable
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands
'xop uop yop' =: 1 {"1 y
NB.?lintonly uop =: <'dissectverb' [ yop =: xop =: coname''
dispoperands =: setvalence__uop resultissdt__xop,resultissdt__yop
resultissdt =: resultissdt__uop
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
,/ 1 traverseup__xop`traverseup__yop\ traverseup__uop traverseuprollup y
NB. The result from this level is immaterial, since it never feeds up (it's a noun node)
NB.?lintsaveglobals
)

NB. Tree traversal, down
traversedown =: 3 : 0
NB. If the selector selects a single result, that is our selected result.
NB. For this node, the yop must be a collector, and therefore we will ignore any selection it makes.
NB. The last item in the chain must be a noun, which will ignore its y.  So we don't need
NB. a y to the call to the upper node - this value is used only at the very end of the chain
NB. Since y is a collector, we know that it will have a full result and no selectors
NB. Since u is a collector, it too will have a result and no selectors.  That will become our result.
traversedowncalcselect y
udisplayrcd =: traversedown__uop bnsellevel,selector , (traversedown__xop ,&valfromdisplayrcd traversedown__yop) bnsellevel,<selectall__COCREATOR
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. Create the gridobj for the verbex and operands
NB. Create a collector for the result
NB. If this node is a rollup of sdts, don't show the detail unless the user asks for it
if. resultissdt *. detaillevel < 2 do.
  (1 1 $ creategridcell (valfromdisplayrcd udisplayrcd);'input';'value';'';'clickresult');($0);_1
else.
  go =. {.`creategridobj__uop@.(0 = stealthoperand__uop) dispoperands # (creategridobj__xop ,: creategridobj__yop) y
  (selectiondisplayrcd udisplayrcd) 2 condcreateselection go  NB. final selectors must be unconditional
end.
)

NB. Terminal nouns - names or self-defining terms
cocurrent 'dissectnoun'
coinsert 'dissectobj'
NB. Monad.  y is string form of the noun;name if it is a name;tokens it came from
create =: 3 : 0
NB. Not clonable
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand, and the name if any
'op varname' =: 2 {. y
NB. If the name is empty, this must be an SDT
resultissdt =: 0 = #varname
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
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

NB. Return value of result
value =: 3 : 0
0 {:: logvalues
)

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseuprollup y
NB. The result from this level is immaterial, since it never feeds up (terminal noun node);
NB. But in case it is the n in an nvv fork, return empty numeric which will turn into the correct number of 0s
NB.?lintsaveglobals

)
NB. Tree traversal, down
traversedown =: 3 : 0
traversedowncalcselect y  NB. Just to set error globals
selectiondisplayrcd''
NB.?lintsaveglobals
)

NB. Return grid specifier
creategridobj =: 3 : 0
NB. The grid for a noun is
NB. ( )     [name]
NB. (shape) value
NB. The parts besides the value are optional
gridobj =. 1 1 $ creategridcell (nval =. 0 {:: logvalues);'input';'value';'';'clickresult'
if. #varname do.
  gridobj =. gridobj ,~ creategridcell (varname);'input';'value'
end.
NB. If we are supposed to show detail, append the shape
if. 1 <: detaillevel do.
  gridobj =. a: ,. gridobj
  gridobj =. (creategridcell ($nval);'shape';'shape') (<_1 0)} gridobj
end.
gridobj;($0);_1
)


cocurrent 'dissectverb'
coinsert 'dissectobj'
NB. y is (string form of the verb);rank;tokens it came from
create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operand
op =: 0 {:: y
invok =: (1;3) {:: y
stealthoperand =: 1 2 3 0 {~ (;:'][[:') i. <op
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Save the number of operands for this invocation
NB. Return value indicates which operands will be used
NB. y is, for each operand, whether the operand is from SDTs
setvalence =: 3 : 0
valence =: #y
resultissdt =: *./y
NB. [ and ], as dyads, are treated as monads.  Also, they have no display.
dispoperands =: , 1:^:(valence=1) 2 2 #: >: 1 2 i. stealthoperand  NB. 0=] 1=[ 2=normal
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: 3 : 0
NB. Apply parentheses if right conjunction operand - but only if more than 1 word
enparen^:((y>2) *. 1 < #@;: op ) op
)

NB. return string form of operands, including instrumentation
NB. y tells what intrumentation is needed:
NB.  (1 if inputs need logging) , (1 2 3 for monad/dyad/unknown) , (1 for inverse also)

exestring =: 3 : 0
'inp val inv' =. y
NB. Init list of logvbls we will use
vars =. ''
NB. Instrument the forward verb - bivalent
fv =. '(' , (logstring '') , '@(' , (verblogstring '') , op , '))'
NB. If inputs needed, capture them all - for both valences
if. inp do.
  vars =. vars ,'xy'
  fv =. '(' , fv , '@' , (logstring 'y') , ' :(' , (logstring 'x') , '@[ ' , fv , (logstring 'y') , '@[)"(' , op , '))'
end.
NB. If inverse also needed, do it too
if. inv do.
  opi =. op , '^:_1'
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
NB. The verbex is different from modifiers, because selection in it create qualifiers rather than selectors.
NB. We ignore forwarded requests to set detaillevel, and only honor forwarded requests to turn it off.  We
NB. honor local requests to do either.
NB. debug qprintf 'tupvb$y=?$y op y '
valence # traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
selectiondisplayrcd''
NB.?lintsaveglobals
)



NB. Return grid specifier
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. This is a single verb execution, not including a collector, which must be separately provided
NB. [(qualx)]  verb   (qualy)
NB.            (qual) (qualresult)
NB. The parenthesized parts are included if the cell has been selected for editing, and
NB. the selection in the cell is valid
NB. ] and [ have no display, just create an empty gridsel
if. stealthoperand e. 1 2 do.
  gridobj =. 0 0$a:
  inputspacing =. $0
else.
  NB. Input comes in column [0] and _1.  Qualifier in _2.  Calculate the width and create first row
  inputspacing =. gridabutxy y
  NB. Use the spacing for the inputs, since the verb has no special needs.  Make the dyad width
  NB. at least 2, the monad at least 1, and then add 1 to space between columns.  Column 0 of the
  NB. dyad is where the x operand goes
  gridobj =. ,: (>: 1 >. {. 2 >. inputspacing) $ a:

  NB. Install the verb
  NB. If there is a frame for this verb, make the verb-cell editable.  But not if there was an agreement error, which we can't
  NB. select our way out of.  We will display that error at the collector 
  estring =. ((errorcode ~: 1) *. editok =. 1 < */ frame) # 'clickverb'
  gridobj =. (creategridcell op;'verb';'verb';'';estring) (<0,_2)} gridobj
  NB. If dyad, install an empty verb-colored cell in each spacer position
  if. valence = 2 do.
    gridobj =. (creategridcell '';'verb';'verb';'';estring) (<0;<<_1 _2)} gridobj
  end.
  NB. If the cell is displaying detail and it is open for editing or there was an early error, create the qualifier
  NB. If this cell is unselectable and it carries an error, make sure we display the error
  if. (errorcode e. 3) *. editok *: isselvalid do.
    gridobj =. (creategridcell errormessagefrominterp__COCREATOR;'error';'error') (<0;valence {. _1 0)} gridobj
  elseif. errorcode = 1	do.
    NB. Early error.  Display the error next to the verb
    gridobj =. (creategridcell 'agreement';'error';'error') (<0;valence {. _1 0)} gridobj
  elseif. errorcode = 2 do.
    NB. Unexecuted cell
    gridobj =. (creategridcell 'unexecd';'invalid';'invalid') (<0;valence {. _1 0)} gridobj
  elseif. (* 4 bwand detaillevel) *. editok *. isselvalid do.
    NB. no error that preempts further analysis
    NB. No early error, so there may be a selection.  If there is one, show its results; if not,
    NB. show the frame
    gridobj =. gridobj , a:
    gridobj =. (creategridcell gridusersel;'qualifier';'qualifier';'';'editqualifier') (<1,_2)} gridobj
    select. islocalselgiven
    case. 0 do.
      NB. No selection yet, show the frame
      gridobj =. (creategridcell ' ';'qualifiedop';'value') (<0;valence {. _1 0)} gridobj
      gridobj =. (creategridcell ('frame=',":frame);'qualifiedres';'shape') (<1 _1)} gridobj
    case. 1;5 do.
      NB. Selector invalid or unexecuted
      gridobj =. (creategridcell ' ';'qualifiedop';'value') (<0;valence {. _1 0)} gridobj
      gridobj =. (creategridcell ((islocalselgiven = 5){::'invalid';'unexecd');'invalid';'invalid') (<1 _1)} gridobj
    case. 3;7 do.
      NB. The selector is valid; the result may or may not be
      gridobj =. (creategridcell selops ,"1 'qualifiedop';'value') (<0;(-#selops) {. 0 _1)} gridobj
      if. islocalselgiven = 3 do.
        gridobj =. (creategridcell errormessagefrominterp__COCREATOR;'error';'error') (<1 _1)} gridobj
      else.
        try.
          dispdata =. >localselresult  NB. try opening
          'disptype dispfont' =. 'qualifiedres';'value'
        catch.
          'dispdata disptype dispfont' =. 'framing';'error';'error'
        end.
        gridobj =. (creategridcell dispdata;disptype;dispfont) (<1 _1)} gridobj
      end.
    end.
  end.
end.
(gridobj;((0:"0 inputspacing),_1);_1) gridjoinxyu y
NB.?lintsaveglobals
)

NB. **** assignment ****
cocurrent 'dissectassign'
coinsert 'dissectobj'

NB. Object to handle assignments

NB. Unknown modifiers create verbs (we hope).  We will create something that looks like a verb -
NB. it will be the display form of the modified input operands.  We will then pretend to be a verb.
NB. y is the exeblock for the modifier, either 2 or 3 boxes
create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
NB.?lintsaveglobals
)



NB. ********** handling of modifiers **************
NB. Each group of modifiers has a locale to create objects of its type

cocurrent 'dissect'
dissectmodindex =: 0$a:  NB. list of (<list of boxed modifier words)

NB. y is string containing the modifiers that will be handled in this
NB. locale.  Result is the locale name.  Side effect: index extended
NB. MAJOR SIDE EFFECT: locale is changed
modlocale =: 3 : 0
NB.?lintmsgsoff
cocurrent newloc =. <'dissectmod' , ": <: # dissectmodindex_dissect_ =: dissectmodindex_dissect_ , <;: y
NB.?lintmsgson
coinsert 'dissectobj'
18!:4 newloc
i. 0 0
)

NB. **** @ @: ****
modlocale '@@:'

NB. Save the name of the locale that handles @@: - we use it in &&: and also in fork
localeat_dissect_ =: coname''

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
NB. Ignore ]@ etc.
if. stealthoperand__uop e. 1 2 do.
  verb;vop;tokensource
else.
  verb;(coname'');tokensource
end.
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
dispoperands =: setvalence__vop y
setvalence__uop resultissdt__vop
resultissdt =: resultissdt__uop
NB. Return the dispoperands from v
dispoperands
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseup__vop traverseup__uop traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
vdisplayrcd =: traversedown__vop bnsellevel , selector , , valence {. selops
selectiondisplayrcd udisplayrcd =: traversedown__uop travdownuops vdisplayrcd
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is operand(s)
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. Create initial selector, which will show if there is a selection possible - including agreement and selection errors
udisplayrcd 1 condcreateselection creategridobj__uop ,: vdisplayrcd 0 condcreateselection creategridobj__vop condcreateinitialselector y
NB. No stealthoperand v in @@: - they are turned into verbs
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
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
NB. Look at the conjunction used, and use that to find the rank of this object (the derived verb)
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
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
  dispoperands =: setvalence__uop resultissdt__vop0 , resultissdt__vop1
  resultissdt =: resultissdt__uop
end.
valence # 1
NB. We always get both operands for v, since we have cloned vop0/vop1 (it's not worth saving the operand).
NB. We will use dispoperands to trim the call to uop, if it is not a real dyad
NB.?lintsaveglobals
)

NB. return string form of operands, not including instrumentation
defstring =: defstring__localeat f.

NB. return string form of operands, including instrumentation
exestring =: 3 : 0
initloggingtable ''
rankstg =. (cop -: ,'&') # '"', (defstring__vop 3)
auditstg '(' , (logstring '') , '@(' , (verblogstring '') , '(' , (exestring__vop0 0,valence__vop0,0) , '@[ ' , (exestring__uop 0 1 0) , (exestring__vop1 0,valence__vop1,0) , '@] ' , ')' , rankstg , '))'
)

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
NB. Only the dyad gets here
traverseup =: 3 : 0
,/ 1 traverseup__vop0`traverseup__vop1\ traverseup__uop traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
vdisplayrcd =: displayrcdfixuverror (traversedown__vop0@{. ,: traversedown__vop1@{:) bnsellevel ,. selector ,"1 valence {. selops
selectiondisplayrcd udisplayrcd =: traversedown__uop travdownuops vdisplayrcd
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is grdobj
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. Create initial selector, which will show if there is a selection possible - including agreement and selection errors
go =. 1 creategridobj__vop0`creategridobj__vop1\ condcreateinitialselector y
if. 0 = stealthoperand__vop do.
  go =. vdisplayrcd 0 condcreateselection go
end.
NB. NOTE the dispoperands here is local; we passed 1 1 to the higher level
udisplayrcd 1 condcreateselection creategridobj__uop dispoperands # go
)

NB. **** u&n m&v ****

cocurrent 'dissectvandnm'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
NB. Don't try to remember locale yet - we might clone
verboperandx =: * verb bwand (<2 0) {:: y
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
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
dispoperands =: 1
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
NB. Only the dyad gets here
traverseup =: 3 : 0
NB. One branch out of u goes to the noun, the other becomes our output
'verbsel nounsel' =. verboperandx |. traverseup__verbop traverseuprollup y
traverseup__nounop ,: nounsel
,: verbsel
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
vdisplayrcd =. traversedown__nounop 0;<selectall__COCREATOR
if. 0 = errorcode__nounop do.
  udisplayrcd =. traversedown__verbop bnsellevel , (verboperandx |. (,selops) , valfromdisplayrcd vdisplayrcd) ,~^:collected selector
else. udisplayrcd =. ''
end.
selectiondisplayrcd udisplayrcd
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is min spacing between inputs (ignored for this monadic valence)
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. No selector is possible, so we do not produce a collector, which must be supplied elsewhere
go =. creategridobj__nounop ''
NB. If noun failed, keep only its display
if. 0 = errorcode__nounop do.
  NB. join noun gobj with the monadic input value; forward to the verb
  go =. creategridobj__verbop dispoperands # verboperandx |. y , go
end.
go
NB.?lintsaveglobals
)

NB. **** &. &.: ****
modlocale '&.&.:'
NB. we emulate this with v^"_1@:u&v

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
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
uop =. 1 {:: create__nobj (_3 [\ verb;iop;($0);conj;'@:';($0)) , 0 { y

NB. Now change this locale to &&: and create i&v
NB. Replace the uop with the iop we just created, and the cop with the given conjunction, with '.' removed
changeobjtypeto localecompose

create (uop;(<<<1){cop) (<0 1;1)} y
NB.?lintsaveglobals
)


NB. **** u"n u"v ****
modlocale '"'

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: ''
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
dispoperands =: setvalence__uop y
resultissdt =: resultissdt__uop
NB. We take all operands here, so we can check for agreement
valence # 1
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseup__uop traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
selectiondisplayrcd udisplayrcd =: traversedown__uop bnsellevel , (travdownselok errorcode) # selector , , valence {. selops
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is operand(s)
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. If there are multiple results (i. e. frame has more than 1 element), create
NB. a line for the rank operator; then create a selector (optionally).
NB. Put a rank operator only in sides where the frame has options
if. isselvalid *. 1 < */ frame do.
  rankcells =. ((1<*/)&.> dispoperands # frames) #&.> <@(1 1&$)@creategridcell (('"' , ":)&.> (dispoperands # ((<: + i.) valence) { verbex b. 0)),"0 1 'verb';'verb';'';'clickresult'
  y =. (<@gridstackright ({."1 y) ,. rankcells) (<a:;0)} y
end.
NB. Create and append the grid object for u, which must align on the right
udisplayrcd 1 condcreateselection creategridobj__uop dispoperands # condcreateinitialselector y
)

NB. **** um`vn ****
modlocale '`'

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname'' [ cop =: '' [ conjex =: ''
noun;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
NB. There is nothing to say at traversal time, since this is a conjunction returning a noun.
NB. We could instrument the noun operands and show them, but since tie is basically used to create
NB. gerunds, that seems useless.  We will just report the value.
NB. This is a runt displayrcd, but we know that only the value part is used, since we don't display detail
,<conjex
)

NB. Return grid specifier
NB. y is operand(s)
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. The grid for a gerund is
NB. (value)
gridobj =. 1 1 $ creategridcell ((2 <: detaillevel) { 'gerund';<conjex),'input';'value';'';'clickresult'
gridobj;($0);_1
NB.?lintsaveglobals
)


NB. **** u~ m~ ****
modlocale '~'

create =: 3 : 0
if. noun bwand (<0 0) {:: y do.
  NB. Treat m~ using default modifier
  changeobjtypeto localedefault
  create y return.
end.
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
newobj__COCREATOR coname''
NB. Save the operands - locales of the verbs, and string form of the conj
'uop cop' =: 1 {"1 y
NB.?lintonly uop =: coname'' [ cop =: ''
refdetail =: 0  NB. This locale creates a reference
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
dispoperands =. setvalence__uop 2$y
resultissdt =: resultissdt__uop
NB. We have what the next level needs; what we need is the reverse
+./^:(valence=1) |. dispoperands
NB. We have set up the args correctly so that we just pass through all display operands that we see.
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. We don't call traverserollup because ~ has no visible presence
traverseup =: 3 : 0
NB. We do have to make sure that the outbound valence matches the valence of this verb
traverseupmergestreams^:(valence=1) traverseup__uop y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect (2&{. , |.&:(2&}.))^:(2<#) y
NB. Here is where we swap the operands for calculation purposes
selectiondisplayrcd udisplayrcd =: traversedown__uop bnsellevel , (-. errorcode e. 1) # selector , , valence {. selops
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is gridspec(s)
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. For the dyad, we have to insert a selector, since ~ has the rank of its operand and we need to
NB. match what the interpreter does.  Since the monad has infinite rank, it will never select
go =. condcreateinitialselector |. y   NB. monad or dyad.  switch before selector, so it stays in the middle
NB. ~ has no visible display - it just alters the connections
if. valence < #y do.
NB. If u takes more operands than we have inputs, create a label for the y operand, and a reference for the x
  go =. (> {. , selops) creategridref {. go,<''
end.
NB. Create and append the grid object for u, which must align on the right
NB. If there is a local selector, append the selected value
udisplayrcd 1 condcreateselection creategridobj__uop go   NB. monad or dyad
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
create_dissectobj_ f. < ; 2 {"1 y
NB. Get the locales
if. 2 = #y do.
'uop cop' =. 1 {"1 y
NB.?lintonly uop =. <'dissectverb' [ cop =. ''
stg =. (defstring__uop 2) jd cop
else.
'uop cop vop' =. 1 {"1 y
NB.?lintonly uop =. vop =. <'dissectverb' [ cop =. ''
stg =. (defstring__uop 2) jd cop jd (defstring__vop 3)
end.
changeobjtypeto 'dissectverb'
create stg;0 0 0 0;tokensource
)


NB. **** fork ****
cocurrent 'dissectfork'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop cop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: cop =: coname''
NB. Remember whether this is an nvv-type fork
if. vvv =: verb = (<0 0) {:: y do.
  NB. If it's vvv, see if starts with [: .  If so, go process it as u@:v
  if. 3 = stealthoperand__uop do.  NB. [:
    changeobjtypeto localeat
    create (((<1;0 1) { y) ,: conj;'@:') (<0 1;0 1)} y
    return.
  end.
end.
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
NB. If cop is a stealth, we can omit one branch altogether
if. stealthoperand__cop do.
  (2 {. y{~0 2 1 0{~stealthoperand__cop),tokensource
else.
  'xrefdetail yrefdetail' =: 0  NB. This locale creates references
  verb;(coname'');tokensource
end.
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
dispoperandsc =: setvalence__cop 2
NB. dispoperands is indexed (uv;xy) giving the need for operands in each verb
dispoperands =: setvalence__vop y
if. vvv do.
  dispoperands =: dispoperands ,:~ setvalence__uop y
else.
  dispoperands =: (valence#0) ,: dispoperands
end.
dispoperandsc =: setvalence__cop resultissdt__uop , resultissdt__vop
resultissdt =: resultissdt__cop
dispoperandbool =: +./ dispoperandsc * dispoperands
NB. dispoperands here is set to how many copies we need; we return the boolean version
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
NB. cop is always a dyad & so will produce 2 results, which we must then route to u and v.  We
NB. collect the outputs of u & v and then combine them to produce a final result to be forwarded on.
NB. We use dispoperands here (which we don't do for @ & etc.) so that a click on a u that does not use its
NB. x, say, will not propagate selection up through the x branch.  For @ & etc it was OK to propagate, since
NB. the whole branch would be deleted from display (?)
traverseupmergestreams^:(valence=1) dispoperands * 1 traverseup__uop`traverseup__vop\ dispoperandsc * traverseup__cop traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
NB. Run v, then u, then c.  Stop in case of error
NB. run v and save the selected result, in case it is valid.  Run in the order vop,uop because
NB. That's the order the interpreter uses.  This will catch the failure in the side that actually failed
vdisplayrcd =: displayrcdfixuverror (traversedown__uop ,: traversedown__vop) bnsellevel , selector , , valence {. selops
NB. If v failed, upgrade the error code in u
errorcode__uop =: errfromdisplayrcd {. vdisplayrcd
selectiondisplayrcd cdisplayrcd =: traversedown__cop travdownuops vdisplayrcd
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is table of grid specifiers, one per operand
NB. Result is grid specifier for the result
NB. No selectors, because this verb has infinite rank.
creategridobj =: 3 : 0
NB. See how many copies of each input are needed.  If more than 1, make a reference.  Result is
NB. a box for each operand, containing a table of as many gridspecs as we will need for that operand
d =. (#"1~  +./) dispoperandsc * dispoperands  NB. The columns that are used, which will match y
NB. There is one column of d for each row of y, and one row for each of u and v
NB. For each item in y, create a grid reference if the item is used twice.  Result
NB. is a box for each item of y, containing a table, each row of which is a gridspec.
NB. Since the traversal used all operands, we have to delete any that are not dispoperands
by =. (+/d) <@((# ,:@}.@}:)`((>@{. creategridref }.)@])@.(2=[))"0 1 (dispoperandbool # valence {. , selops) ,"0 1 y ,. <"0 (#y) {. 'xy' 
NB. Create the operand selectors for each of u and v.  For each verb/operand, create a box containing the
NB. selection to be made for each verb.  This selector will have one row for each of u and v, and
NB. one column for each operand.  We will assign the x operand label to u and the y operand label
NB. to v.  Rotate if dyad, so monad y label goes to v.  Empty list if an operand is unused
selxy =. (#&.>  (bwxor"1  (0 _1) {.~ -@{:@$)@:<:@:(+/\)) d
NB. Select the operands for each of u and v.  Result is 2 boxes, each containing a table of
NB. gridspecs
ops =. selxy {&.>"1 by
NB. Run u and v.  Note that u and v might have no operands (if c has display valence of 1).
NB. In that case, we don't run the verb
if. (-.vvv) +. *#op =. ; {. ops do.
  ugridsel =. ,: creategridobj__uop op
  if. vvv *. 0 = stealthoperand__uop do. ugridsel =. ({.vdisplayrcd) 0 condcreateselection ugridsel end.
else. ugridsel =. 0 3 $ a:
end.
if. (*#op =. ; {: ops) do.
  vgridsel =. ,: creategridobj__vop op
  if. 0 = stealthoperand__vop do. vgridsel =. ({:vdisplayrcd) 0 condcreateselection vgridsel end.
else. vgridsel =. 0 3 $ a:
end.
NB. Run c
assert. *ugridsel >.&# vgridsel [ 'fork with no operands'
cdisplayrcd 1 condcreateselection creategridobj__cop ugridsel , vgridsel
)

NB. **** hook ****
cocurrent 'dissecthook'
coinsert 'dissectobj'

create =: 3 : 0
create_dissectobj_ f. < ; 2 {"1 y
NB. Register this object so we can clean up at end
NB. Save the operands - locales of the verbs/nouns.  'cop' is the middle verb, for similarity with the others
'uop vop' =: 1 {"1 y
NB.?lintonly uop =: vop =: coname''
NB. Wait till here to add to object list so it doesn't show up twice
newobj__COCREATOR coname''
refdetail =: 0  NB. This locale creates references
verb;(coname'');tokensource
NB.?lintsaveglobals
)

destroy =: 3 : 0
codestroy''
)

NB. Set the valence used for executing this verb, and propagate to descendants
setvalence =: 3 : 0
valence =: #y
setvalence__vop {: y
dispoperands =: 1 [^:(valence=1) dispoperandsu =: setvalence__uop 2$y
NB. ([ v) =[  (] v) =mv  (u []) m=u~ d=u (=u~~)
NB. dispoperands is set from u
resultissdt =: resultissdt__uop
dispoperands
NB.?lintsaveglobals
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

NB. Tree traversal, up
NB. traverseuprollup does the work, we just make structural connections here
traverseup =: 3 : 0
traverseupmergestreams^:(1=valence) ({. , traverseup__vop@:((,1)&{)) traverseup__uop traverseuprollup y
NB.?lintsaveglobals
)

NB. Tree traversal, down
NB. traversedowncalcselect does the work; we preserve the structure here
traversedown =: 3 : 0
traversedowncalcselect y
NB. run v and save the selected result, in case it is valid
vdisplayrcd =: traversedown__vop bnsellevel , selector , , {: selops
NB. If the selection from v was valid, then the v result must have been properly selected, and we will
NB. send its value into u.  If v did not have valid inputs, u cannot either, and will retain just the selection info
selectiondisplayrcd udisplayrcd =: traversedown__uop bnsellevel , (({.selops) , valfromdisplayrcd vdisplayrcd) ,~^:collected selector
NB.?lintsaveglobals
)

NB. Return grid specifier
NB. y is table of grid specifiers, one per operand
NB. Result is grid specifier for the result
creategridobj =: 3 : 0
NB. We must copy the input if the valence is 1 and both copies are used.
if. (valence=1) *. 2 = +/ dispoperandsu do.
  y =. |. (> {. selops) creategridref {. y ,. <''
end.
NB. If v contributes, run it
if. {: dispoperandsu  do.
  go =. vdisplayrcd 0 condcreateselection creategridobj__vop (,_1) { y
else.
  go =. 0 3 $ a:
end.
NB. Run u
udisplayrcd 1 condcreateselection creategridobj__uop (({. dispoperandsu) # (,0) { y) , go
)


0 : 0
ds '2+''a'''
ds '2,''a'''
ds '2 3+''a'''
ds '1 2 + ''ab'''
ds '1 2 +@+ ''ab'''
ds '1 2 +&+ ''ab'''
ds '1 2 +&+~ ''ab'''
ds '''ab'' +&+ 1 2'
ds '1 2 +@(]"0) ''ab'''
ds '1 2 +@(0:"0) ''ab'''
ds '0 1 2 + 1 2'
ds '+@+ ''a'''
ds '+@{. ''a'''
ds '0 +&+ ''a'''
ds '0 +&+ ''ab'''
ds '0 +&:+ ''a'''
ds '''a''+&+ 0'
ds '''ab''+&+ 0'
ds '''a''+&:+ 0'
ds '+&{. ''a'''
ds '+&:+ ''a'''
ds '+&2 (3 4)'
ds '3&* (3 4)'
ds '+&''a'' (3 4)'
ds '(+&2)@:(2&*) 4 6'
ds '3 4 +"1 i. 3 2'
ds '(i. 3 2) +"1 (3 4)'
ds '(i. 3 2) +"1 i. 3 2'
ds '(i. 3 2) +"1 i. 3 1'
ds '(i. 3 2) +"1 i. 1 1'
ds '2 3 +@]&> 5 6'
ds '(i. 3 2) +@]"1 i. 1 1'
ds '(i. 3 2) +@["1 i. 1 1'
ds 'i.@(0&{) ''a'''
ds 'i."0 (1 2)'
ds '+~ i. 2 3'
ds '3 4 +~ i. 2 3'
ds '3 4 +~ i. 3 2'
ds '3 4 +@]~ i. 3 2'
ds '3 4 +@[~ i. 3 2'
ds '3 4 +~ i. 2 3'
ds '3 4 (+ - *) 0 1'
ds '0 1 2 (+ - *) 0 1'
ds '0 1 2 (+ - 0:) 0 1'
ds '0 1 2 (0: - *) 0 1'
ds '0 1 2 (1:"0 - 0:"0) 0 1'
ds '0 1 2 (+ - ]) 0 1'
ds '0 1 2 ([ - -) 0 1'
ds '0 1 2 ([ - ]) 0 1'
ds '0 1 2 (- + * % -)"0 (3 4 5)'
ds '0 1 (+ 0:) ''ab'''
ds '0 1 (+ {.) ''ab'''
ds '0 1 (+ ]) 1 2 3'
ds '0 1 2 + '''''
ds '0 1 2 + '' '''
ds '0 (+ - *) '''''
ds '0 (1 2 3 - *) '''''
ds '0 (1 2 3 - *)"0 '''''
ds '0 (1 2 3 , ])"0 $0'
ds '0 ([: 1 2 3"0 $)"0 $0'  NB. should be $=0 0 3
ds '0 (+ - ]) '''''
ds '0 (1 2 3 - *)"0 (0)'
ds '0 +@* '''''
ds '0 (+@* - *) '''''
ds '0 (+@* *) '''''
ds '0 (+ *) '''''
ds '(#@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
z =. 2
ds 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
ds 'z (# >)"1 ] 2 2 $ ''abc'';''b'';''cd'';''q'''
ds '(1&+@>)"1 ] 2 2 $ ''abc'';''b'';''cd'';0'
)

