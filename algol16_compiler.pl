/*
    Wojciech Pratkowiecki II UWr
    Algol 16 cross compiler

    lexer:
    operators: + - * < <= > >= = <> := ;
    key words: and begin call div do done else end fi if local mod not or procedure program read return then value while write
    comments: char sing sequences started with (* and ended with *)
    identificators: sequences of ASCII letters, digits 0-9, signs _ and '

*/

lexer(Tokens) -->
   blank_sign,
   (  (  ":=",      !, { Token = tokAssgn }
      ;  ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "+",       !, { Token = tokAdd }
      ;  "-",       !, { Token = tokSub }
      ;  "*",       !, { Token = tokMul }
      ;  "=",       !, { Token = tokE }
      ;  "<>",      !, { Token = tokD }
      ;  "<=",      !, { Token = tokLE }
      ;  "<",       !, { Token = tokL } 
      ;  ">=",      !, { Token = tokGE }
      ;  ">",       !, { Token = tokG }
      ;  ",",       !, { Token = tokComma } 
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (and, tokAnd),
                                     (begin, tokBegin),
                                     (call, tokCall),
                                     (div, tokDiv),
                                     (do, tokDo),
                                     (done, tokDone),
                                     (else, tokElse),
                                     (end, tokEnd),
                                     (fi, tokFi),
                                     (if, tokIf),
                                     (local, tokLocal),
                                     (mod, tokMod),
                                     (not, tokNot),
                                     (or, tokOr),
                                     (procedure, tokProc),
                                     (program, tokProgram),
                                     (read, tokRead),
                                     (return, tokReturn),
                                     (then, tokThen),
                                     (value, tokValue),
                                     (while, tokWhile),
                                     (write, tokWrite) ]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

blank_sign -->
   [Char], { code_type(Char, space) }, !, blank_sign.
blank_sign -->
    "(*", !, comment.
blank_sign -->
   [].
comment -->
      "*)", !, blank_sign.
comment -->
      [_], !, comment.
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) ; A = 39 ; A = 95 }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

/*
  PARSER
  Desription of algol 16 syntax in exercise content
*/

program(Ast) -->
   [tokProgram],
   [tokVar(Program_Id)],
   block(Bl),
   { Ast = (Program_Id, Bl) }.
block(Bl) --> 
  declarations(Dec),
  [tokBegin],
  instruction_set(Instr),
  [tokEnd],
  { Bl = block_(Dec, Instr) }.

declarations(Dec) -->
    single_dec(Dec2), !, declarations(Rest), { append(Dec2, Rest, Dec) }.
declarations([]) -->
    [].

single_dec(Dec) -->
     (declarator(Dec) ; procedure(Dec) ).

declarator(Dec) -->
    [tokLocal], vars(Dec).

vars(Var) -->
    single_var(V), ( [tokComma], !, vars(Rest), 
    {Var = [variable(V) | Rest] }
    ; [], { Var = [variable(V)] } ).
single_var(Var) -->
      [tokVar(Var)]. 

procedure(Proc) -->
    [tokProc], proc_name(Name), [tokLParen], proc_arg(Args), [tokRParen], block(Bl),
    { Proc = [proc(Name, Args, Bl)] }.
proc_arg(Args) -->
    proc_arg_set(Args), !.
proc_arg([]) -->
    [].
proc_arg_set(Args) --> 
    single_proc_arg(Arg), ( [tokComma], !, proc_arg_set(Rest),
    { Args = [Arg | Rest] }
    ; [], { Args = [Arg] } ).
single_proc_arg(Arg) -->
    [tokValue], !, single_var(V), { Arg = variable(V) }.
single_proc_arg(Arg) -->
    single_var(V), { Arg = variable(V) }.

instruction_set(Ast) -->   
   instruction(Instr),
   (  [tokSColon], !, instruction_set(Rest),
         { append([Instr], Rest, Ast) }
   ;  [],
         { Ast = [Instr] }
   ).

instruction(Instr) -->
   (  [tokWhile], !, bool_expr(Bool), [tokDo], instruction_set(Body), [tokDone],
          { Instr = while(Bool, Body) }
   ;  [tokIf], !, bool_expr(Bool), [tokThen], instruction_set(ThenPart),
         (  [tokElse], !, instruction_set(ElsePart), [tokFi],
               { Instr = if(Bool, ThenPart, ElsePart) }
         ;  [tokFi],
               { Instr = if(Bool, ThenPart) }
         )
   ;  [tokCall], !, proc_call(Call),
         { Instr = call_(Call) }
   ;  [tokReturn], !, arith_expr(Expr),
         { Instr = return_(Expr) }
   ;  [tokRead], !, [tokVar(Id)], 
         { Instr =  read_(variable(Id)) }
   ;  [tokWrite], !, arith_expr(Expr),
          { Instr = write_(Expr) }
   ;  [tokVar(Var), tokAssgn], arith_expr(Expr),
         { Instr = assgn_(variable(Var), Expr) }
   ).
   
proc_call(Proc) -->
    proc_name(PName), [tokLParen], arguments(Arg), [tokRParen],
        { Proc = ( PName, Arg )}.
proc_name(ProcName) -->
    [tokVar(ProcName)].
arguments(Args) -->
    ( args_set(Args), ! ; [], { Args = [] } ).

args_set(Args) -->
    simple_arg(Arg), ( [tokComma], !, args_set(Rest),
    { Args = [Arg | Rest] }
    ; [], { Args = [Arg] }  ).

simple_arg(Arg) -->
    arith_expr(Arg).

arith_expr(Expr) -->
   summand(Summand), arith_expr(Summand, Expr).

arith_expr(Acc, Expr) -->
   additive_op(Op), !, summand(Summand),
      { Acc1 = [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

summand(Expr) -->
   factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
      { Acc1 = [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

factor(Expr) -->
    [tokSub], !, simp_expr(NewExpr), { Expr = -NewExpr }.
factor(Expr) -->
    simp_expr(Expr).

simp_expr(Expr) -->
    [tokLParen], !, arith_expr(Expr), [tokRParen].
simp_expr(Expr) -->
    atom_expr(Expr).

atom_expr(Expr) -->
    [tokNumber(N)], !, { Expr = constant(N) }.
atom_expr(Expr) -->
    proc_call(Call), !, { Expr = call_(Call) }.
atom_expr(Expr) -->
    [tokVar(Var)], { Expr = variable(Var) }.


bool_expr(Bool) -->
   conjunct(Conjunct), bool_expr(Conjunct, Bool).

bool_expr(Acc, Bool) -->
   [tokOr], !, conjunct(Conjunct),
      { Acc1 = [or, Acc, Conjunct] }, bool_expr(Acc1, Bool).
bool_expr(Acc, Acc) -->
   [].

conjunct(Conjunct) -->
   condit(Condit), conjunct(Condit, Conjunct).

conjunct(Acc, Conjunct) -->
   [tokAnd], !, condit(Condit),
      { Acc1 = [and, Acc, Condit] }, conjunct(Acc1, Conjunct).
conjunct(Acc, Acc) -->
   [].

condit(Condit) -->
    [tokNot], !, rel_expr(NewCondit), { Condit = not(NewCondit) }.
condit(Condit) -->
    rel_expr(Condit).

rel_expr(Expr) -->
   (  [tokLParen], bool_expr(Expr), !, [tokRParen]
   ;  arith_expr(LExpr), rel_op(Op), arith_expr(RExpr),
         { Expr = [Op, LExpr, RExpr] }
   ).

additive_op(+) -->
   [tokAdd], !.
additive_op(-) -->
   [tokSub].

multiplicative_op(*) -->
   [tokMul], !.
multiplicative_op(//) -->
   [tokDiv], !.
multiplicative_op(mod) -->
   [tokMod].

rel_op(=) -->
   [tokE], !.
rel_op(<>) -->
   [tokD], !.
rel_op(<) -->
   [tokL], !.
rel_op(<=) -->
   [tokLE], !.
rel_op(>) -->
   [tokG], !.
rel_op(>=) -->
   [tokGE].
/*
    End of parser.
    The rest of file is algol 16 compiler.
    First step of compilation is translating the abstract syntax tree to macro-assembler, which is much more friendly for user, then macro-instructions are translated to Sextium® III words
*/

/*
      Memory organization:
      #########################
      0xffff#    variable 1   #
      0xfffe#    variable 2   #
      .....
      .....
      0xXXXX#    variable n   #
      0xXXXX#    stack begin  #
      .....
      .....
      0xXXXX# last stack elem # <- (Stack_Pointer)
      .....
      .....
      0x0000###################
*/

%ACC := *Adr
macro_compile(VarEnv, _, _, variable(Id), Macro) :-
    member((Id, Adr), VarEnv),
    Macro = [const(Adr), swapa, load].
%ACC := *Adr * -1
macro_compile(VarEnv, _, _, -variable(Id), Macro) :-
    member((Id, Adr), VarEnv),
    Macro = [const(Adr), swapa, load, swapd, const(65535), swapd, mul].
%ACC := Const  
macro_compile(_, _, _, constant(Const), [const(Const)]).
%ACC := Const * -1
macro_compile(_, _, _, -constant(Const), [const(Const), swapd, const(65535), swapd, mul]).
% ACC := Expr 
% *Adr := ACC
macro_compile(VarEnv, SP, ProcEnv, assgn_(variable(Id), Expr), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr, Macro_Expr),
    member((Id, Adr), VarEnv),
    append(Macro_Expr, [swapa, const(Adr), swapa, store], Macro).
/*
  Arithmetic operations algorithm:
    ACC := value(Expr1);
    push ACC;
    ACC := value(Expr2);
    DR := ACC;
    pop to ACC;
    Make operation
*/
macro_compile(VarEnv, SP, ProcEnv, [+, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    NewSP is SP - 1,
    macro_compile(VarEnv, NewSP, ProcEnv, Expr2, Macro_Expr2),
    append([Macro_Expr1, [swapa, const(SP), swapa, store], Macro_Expr2, [swapd, const(SP), swapa, load, add]], Macro).

macro_compile(VarEnv, SP, ProcEnv, [-, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    NewSP is SP - 1,
    macro_compile(VarEnv, NewSP, ProcEnv, Expr2, Macro_Expr2),
    append([Macro_Expr1, [swapa, const(SP), swapa, store], Macro_Expr2, [swapd, const(SP), swapa, load, sub]], Macro).

macro_compile(VarEnv, SP, ProcEnv, [*, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    NewSP is SP - 1,
    macro_compile(VarEnv, NewSP, ProcEnv, Expr2, Macro_Expr2),
    append([Macro_Expr1, [swapa, const(SP), swapa, store], Macro_Expr2, [swapd, const(SP), swapa, load, mul]], Macro).

macro_compile(VarEnv, SP, ProcEnv, [//, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    NewSP is SP - 1,
    macro_compile(VarEnv, NewSP, ProcEnv, Expr2, Macro_Expr2),
    append([Macro_Expr1, [swapa, const(SP), swapa, store], Macro_Expr2, [swapd, const(SP), swapa, load, div]], Macro).

macro_compile(VarEnv, SP, ProcEnv, [mod, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    NewSP is SP - 1,
    macro_compile(VarEnv, NewSP, ProcEnv, Expr2, Macro_Expr2),
    append([Macro_Expr1, [swapa, const(SP), swapa, store], Macro_Expr2, [swapd, const(SP), swapa, load, div, const(65520), swapd, shift]], Macro).

/*
    Bool Operators:

    - = :
        a = b <=> a - b = 0

          ACC := a - b
          if(ACC == 0)
            jump L1
          ACC := 0
          jump L2
        L1:
          ACC := 1
        L2:
          return ACC

    - <> :
        a <> b <=> ~(a = b) 
        generate the same seqence of instructions as '=', swap 0 and 1

    - < :
        a < b algorithm:
          if (a < 0) jump L1;
          if (b < 0) jump False;
          jump L2;
          L1:
          if (b >= 0) jump True;
          L2:
          if (a - b < 0) jump True;
          False:
          AR := 0;
          jump End;
          True:
          AR := 1;
          End:
          ACC := AR;
    - > :
        a > b <=> b < a
    - <= :
        a <= b <=> ~(a > b)

        ACC := (a > b)
        ACC := 1 - ACC
    - >= :
        a >= b <=> ~(a < b)

        ACC := (a < b)
        ACC := 1 - ACC

*/
macro_compile(VarEnv, SP, ProcEnv, [=, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, [-, Expr1, Expr2], SubMacro),
    append(SubMacro, [swapa, const(L1), swapa, branchz, const(0), swapa, const(L2), jump, label(L1), const(1), swapa, label(L2), swapa], Macro).

macro_compile(VarEnv, SP, ProcEnv, [<>, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, [-, Expr1, Expr2], SubMacro),
    append(SubMacro, [swapa, const(L1), swapa, branchz, const(1), swapa, const(L2), jump, label(L1), const(0), swapa, label(L2), swapa], Macro).


macro_compile(VarEnv, SP, ProcEnv, [<, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Macro_Expr1),
    macro_compile(VarEnv, SP, ProcEnv, Expr2, Macro_Expr2),
    macro_compile(VarEnv, SP, ProcEnv, [-, Expr1, Expr2], SubMacro),
    append([Macro_Expr1, [swapa, const(L1), swapa, branchn], Macro_Expr2, [swapa, const(False), swapa, branchn, const(L2), jump, label(L1)], Macro_Expr2, [swapa, const(L2), swapa, branchn, const(True), jump, label(L2)], SubMacro, [swapa, const(True), swapa, branchn, label(False), const(0), swapa, const(End), jump, label(True), const(1), swapa, label(End), swapa]], Macro).

macro_compile(VarEnv, SP, ProcEnv, [>, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, [<, Expr2, Expr1], Macro).

macro_compile(VarEnv, SP, ProcEnv, [<=, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, [>, Expr1, Expr2], NewMacro),
    append(NewMacro, [swapd, const(1), sub], Macro).

macro_compile(VarEnv, SP, ProcEnv, [>=, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, [<, Expr1, Expr2], NewMacro),
    append(NewMacro, [swapd, const(1), sub], Macro).

/*
  Bool Expresions:

  - AND :   Expr1 and Expr2
    ACC := Res(Expr1)
    if (ACC == 0)
      jump L1
    ACC := Res(Expr2)
    L1:
      return ACC

  - NOT:    not Expr
    ACC := Res(Expr)
    ACC := 1 - ACC
    return ACC

  - OR:   Expr1 or Expr2
    ACC := Res(Expr1)
    if (ACC == 0)
      jump L1
    jump L2
    L1:
      ACC := Res(Expr2)
    L2:
      return ACC
*/
macro_compile(VarEnv, SP, ProcEnv, [and, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Res1),
    append(Res1, [swapa, const(L1), swapa, branchz], IfFalse),
    macro_compile(VarEnv, SP, ProcEnv, Expr2, Res2),
    append(IfFalse, Res2, NewMacro),
    append(NewMacro, [label(L1)], Macro).

macro_compile(VarEnv, SP, ProcEnv, not(Expr), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr, Macro_Expr),
    append(Macro_Expr ,[swapd, const(1), sub], Macro).

macro_compile(VarEnv, SP, ProcEnv, [or, Expr1, Expr2], Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr1, Res1),
    append(Res1, [swapa, const(L1), swapa, branchz, swapa, const(L2), jump, label(L1)], IfElse),
    macro_compile(VarEnv, SP, ProcEnv, Expr2, Res2),
    append(IfElse, Res2, NewMacro),
    append(NewMacro, [swapa, label(L2), swapa], Macro).
/*
  Loops and conditions

  - if BoolExpr then Instr fi :
      ACC := result(BoolExpr);
      if (ACC == 0) 
        jump Fi;
      ACC := result(Instr);
      Fi:

  - if BoolExpr then TrueInstr else FalseInstr fi :
      ACC := result(BoolExpr);
      if (ACC == 0) 
        jump Else;
      ACC := result(TrueInstr);
      jump Fi;
      Else:
      ACC := result(FalseInstr);
      Fi:

  - while BoolExpr do Instr done :
      While:
      ACC := result(BoolExpr);
      if (ACC == 0)
        jump Done;
      ACC := result(Instr);
      jump While;
      Done:

*/
macro_compile(VarEnv, SP, ProcEnv, if(BoolExpr, ThenSection), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, BoolExpr, BoolRes),
    append(BoolRes, [swapa, const(Fi), swapa, branchz], IfFalse),
    make_macro(VarEnv, SP, ProcEnv, ThenSection, MacroThen),
    append(IfFalse, MacroThen, NewMacro),
    append(NewMacro, [label(Fi)], Macro).

macro_compile(VarEnv, SP, ProcEnv, if(BoolExpr, ThenSection, ElseSection), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, BoolExpr, BoolRes),
    append(BoolRes, [swapa, const(Else), swapa, branchz], JumpElse),
    make_macro(VarEnv, SP, ProcEnv, ThenSection, MacroThen),
    append(MacroThen, [swapa, const(Fi), jump, label(Else)], NewMacroThen),
    make_macro(VarEnv, SP, ProcEnv, ElseSection, MacroElse),
    append(MacroElse, [swapa, label(Fi), swapa], ReadyMacro),
    append(JumpElse, NewMacroThen, NewMacro),
    append(NewMacro, ReadyMacro, Macro).

macro_compile(VarEnv, SP, ProcEnv, while(BoolExpr, Instr), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, BoolExpr, BoolRes),
    append([label(While)|BoolRes], [swapa, const(Done), swapa, branchz], WhileMacro),
    make_macro(VarEnv, SP, ProcEnv, Instr, InstrMacro),
    append(InstrMacro, [const(While), jump, label(Done)], NewMacro),
    append(WhileMacro, NewMacro, Macro).
/*
  Read and Write instructions are loading the instruction code to ACC and call it.
*/
macro_compile(VarEnv, _, _, read_(variable(Id)), Macro) :-
    member((Id, Adr), VarEnv),
    Macro = [const(Adr), swapa, const(1), syscall, store].

macro_compile(VarEnv, SP, ProcEnv, write_(Expr), Macro) :-
    macro_compile(VarEnv, SP, ProcEnv, Expr, Macro_Expr),
    append(Macro_Expr, [swapd, const(2), syscall], Macro).
/*
  - call:
     * extend the Procedure Enviroment with all the procedures that are going to be able to be called
     * compile the procedure with new Procedure Enviroment, add uniqe label End at the begin of enviroment
     * label End is the place that the procedure ends
*/
macro_compile(_, SP, ProcEnv, call_((ProcName, _)), Macro) :-
    member((ProcName, Vars, Dec, Instr), ProcEnv),
    extend_proc_env(Vars, Dec, Extended, ProcEnv),
    make_macro(Vars, SP, [ret(End)|Extended], Instr, Macro1),
    append(Macro1, [const(0), swapa, label(End), swapa], Macro).
/*
  - return:
     * jump to the End label without executing following instructions
*/
macro_compile(VarEnv, SP, [ret(End) | T], return_(Expr), Macro) :-
    macro_compile(VarEnv, SP, [ret(End) | T], Expr, Macro_Expr),
    append(Macro_Expr, [swapa, const(End), jump], Macro).

% extend_proc_env - add all procedures that are in 'calling range' to Procedure Enviroment
extend_proc_env(_,[],Res, Res).

extend_proc_env(Var, [proc(ProcName, _, block_(Dec2, Instr)) | T], Res, Acc) :-
    extend_proc_env(Var, T, Res, [(ProcName, Var, Dec2, Instr) | Acc]).

% make_macro - compile block of instructions    
make_macro(_, _, _, [], []).
make_macro(VarEnv, SP, ProcEnv, [H|T], [H1|T1]) :-
    macro_compile(VarEnv, SP, ProcEnv, H, H1),
    make_macro(VarEnv, SP, ProcEnv, T, T1).
% main predicate, preper the enviroments and compile instructions
compile((_,block_(Vars, Instr)), Macro) :-
    make_var_adr(Vars, 65535, SP, VarAdr, [], ProcEnv, []),
    make_macro(VarAdr, SP, ProcEnv, Instr, Macro1),
    flatten(Macro1, MacroInstr),
    num_label(MacroInstr, 0),
    unfold_macro(MacroInstr, Unfolded),
    flatten(Unfolded, Macro).    

% make_var_adr - for declaration block of program get all the variables and procedures and place them in enviroments
make_var_adr([], FinalSP, FinalSP, Res, Res, Proc, Proc) :- !.   
make_var_adr([variable(H) | VarT], N, FinalSP, Res, Acc, ProcRes, ProcAcc) :-
    NewN is N - 1,
    make_var_adr(VarT, NewN, FinalSP, Res, [(H, N) | Acc], ProcRes, ProcAcc).
make_var_adr([proc(ProcName, _, block_(Dec, Instr)) | T], N, FinalSP, Res, Acc, ProcRes, ProcAcc) :- 
    make_var_adr(T, N, FinalSP, Res, Acc, ProcRes, [(ProcName, Acc, Dec, Instr) | ProcAcc]).

% flatten - after compiling we've got the list of lists (which can also include lists) of instructions, so we need to make one consistent list form them
flatten(L, Res) :- 
    flatten(L, Res, []), !.

flatten([], Res, Res).

flatten([H|T], Res, Acc) :-
    \+ is_list(H),
    append(Acc, [H], NewAcc),
    flatten(T, Res, NewAcc).

flatten([H|T], Res, Acc) :-
    is_list(H),
    flatten(H, FlH),
    append(Acc, FlH, NewAcc),
    flatten(T, Res, NewAcc).

% every label is just the number of Sextium® III word that we want jump to in some cases. num_label place those word's numbers
num_label([], _).

num_label([H|T], Num) :-
    H = label(X), !,
    X = Num,
    num_label(T, Num).

num_label([H|T], Num) :-
    H = const(_), !,
    NewNum is Num + 2,
    num_label(T, NewNum).

num_label([_|T], Num) :- NewNum is Num + 1, num_label(T, NewNum).

% syscall Halt at the end of program
unfold_macro([], [36864, 0, 4096]).

unfold_macro([MacroH | MacroT], [ResH | ResT]) :-
    get_dec(MacroH, ResH),
    unfold_macro(MacroT, ResT).
% for instance: instruction syscall nop nop nop is 0x1000 = 4096
get_dec(ret(_), []).
get_dec(label(_), []).
get_dec(syscall, 4096).
get_dec(load, 8192).
get_dec(store, 12288).
get_dec(swapa, 16384).
get_dec(swapd, 20480).
get_dec(branchz, 24576).
get_dec(branchn, 28672).
get_dec(jump, 32768).
get_dec(const(N), [36864, N]).
get_dec(add, 40960).
get_dec(sub, 45056).
get_dec(mul, 49152).
get_dec(div, 53248).
get_dec(shift, 57344).
get_dec(nand, 61440).

% dec_to_hex(Integer_List) - print the list in hexadecimal system (as a seqence of Sextium® III words)
dec_to_hex([]).
dec_to_hex([H|T]) :- format(atom(Atom), '~`0t~16R~4| ', H), write(Atom), dec_to_hex(T).
% lexer and parser
parse(CharCodeList, Absynt) :-
   phrase(lexer(Lex), CharCodeList),
   phrase(program(Absynt), Lex).

algol16(Source, SextiumBin) :-
  parse(Source, ParsedCode),
  compile(ParsedCode, SextiumBin).
  