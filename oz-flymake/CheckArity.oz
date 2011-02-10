%%
%% Check a source file for right arity when applying imported procedures.
%% Will not work...
%%  - if a procedure is passed around, not just used as imported
%%  - with nested functors
%%  - if the .ozf files were not compiled on the same computer.
%%
%% Wolfgang.Meyer@gmx.net
%%
functor
import
   Application
   System
   Compiler
   Narrator
   Open
   ProcedureArityFinder(findArity:FindArity)
prepare
   fun {AllArgumentsAreExpressions T}
      {Member T
       [fApply fEq fOpApply fColonEquals fDotAssign fOpApplyStatement fObjApply fRecord
	fRaise fColon fOrElse fAndThen fSideCondition fNot fFun fAt fOpenRecord]}
   end
   
   fun {AllArgumentsAreStatements T}
      {Member T
       [fFunctor fPrepare fMeth fDefine fProc]
      }
   end
   
   fun {FirstArgIsExpression T}
      {Member T
       [fCaseClause fBoolCase fCase fLockThen]}
   end
   
   fun {LogicStmt T}
      {Member T
       [fDis fOr fCond]}
   end
   
   fun {PropagatesToAllArguments T}
      {Member T
       [fLocal fTry fCatch fFinally fThread]}
   end
   
   fun {Leaf T}
      {Member T
       [fDollar fSkip fAtom fInt fFloat fStepPoint fSelf fVar fEscape fImport fExport
	fNoCatch fNoFinally fNoElse fFail fWildcard]}
   end
define
   proc {Main}
      Args = {Application.getArgs record(source(single type:string optional:false))}
   in
      case {ReadFile Args.source} of nothing then
	 {System.showInfo "File not found."}
	 {Application.exit 2}
      [] just(Functor) then
	 case {Parse Functor} of [Tree] then
	    {Check Args.source Functor Tree}
	    if @Errors == 0 then {Application.exit 0}
	    else
	       {System.showInfo "%** ------------------ rejected ("#@Errors#" error(s))"}
	       {Application.exit 1}
	    end
	 else
	    {System.showInfo "Parse error."}
	    {Application.exit 2}
	 end
      end
   end

   %% \return just(Text) or nothing
   fun {ReadFile FileName}
      File Text
   in
      try
	 File = {New Open.file init(url:FileName flags:[read text])}
	 {File read(list:?Text size:all)}
	 {File close}
	 just({Filter Text fun {$C} C\=13 end})
      catch _ then
	 if {IsDet File} then try {File close} catch _ then skip end end
	 nothing
      end
   end

   fun {Parse Str}
      {Compiler.parseOzVirtualString
       Str
       {New Narrator.'class' init($) _}
       fun {$ _} false end 
       {NewDictionary}
      }
   end

   Errors = {NewCell 0}
   FileName
   
   proc {Check FN Src Tree}
      Mode = case Tree of 'fFunctor' then expression else statement end
   in
      !FileName = FN
      {CheckTree Src Tree Mode}
   end

   %% mutually recursive with CheckOther
   proc {CheckTree Src Tree Mode}
      case Tree of fApply(Proc Args Coords) then
	 case {ToProcName Proc}
	 of nothing then skip
	 [] just(ProcName) then
%	    {System.showInfo ProcName#", "#Mode#", "#Coords.2}
	    ArityUsed = {Length Args}
	    + if Mode == expression then 1 else 0 end
	    - if {DollarInArgs Args} then 1 else 0 end
	    CorrectArity = {FindArity ProcName Src nil}
	 in
	    case CorrectArity of unknown then skip
	    [] missing then {ShowNotFoundError ProcName Coords}
	    [] just(A) then
	       if ArityUsed \= A then
		  !Errors := @Errors + 1
		  {ShowArityError ProcName A ArityUsed Coords}
	       end
	    end
	 end
      else
	 skip
      end
      {CheckOther Src Tree Mode}
   end

   proc {ShowNotFoundError Proc Coords}
      pos(_ Line Col ...) = Coords
   in
      {System.showInfo
       "%***************** extended static analysis error ***************\n"#
       "%**\n"#
       "%** procedure of module not found (CHECKARITY)\n"#
       "%**\n"#
       "%** Procedure:            "#Proc#"\n"#
       "%** in file \""#FileName#"\", line "#Line#", column "#Col#"\n\n"
      }
   end
   
   proc {ShowArityError Proc CorrectArity UsedArity Coords}
      pos(_ Line Col ...) = Coords
   in
      {System.showInfo
       "%***************** extended static analysis error ***************\n"#
       "%**\n"#
       "%** illegal arity in application (CHECKARITY)\n"#
       "%**\n"#
       "%** Arity found:          "#UsedArity#"\n"#
       "%** Expected:             "#CorrectArity#"\n"#
       "%** Procedure:            "#Proc#"\n"#
       "%** in file \""#FileName#"\", line "#Line#", column "#Col#"\n\n"
      }
   end
   
   proc {CheckOther Src Tree Mode}
      proc {Check X}
	 {CheckTree Src X Mode}
      end
      proc {CheckExpr X}
	 {CheckTree Src X expression}
      end
      proc {CheckStmt X}
	 {CheckTree Src X statement}
      end
      T = {Label Tree}
   in
      case Tree of fAnd(A B ...) then {CheckStmt A} {Check B}
      elseif T == pos then skip
      elseif {Atom.is Tree} then skip
      elseif {Leaf T} then skip
      elseif {List.is Tree} then {ForAll Tree Check}
      elseif {AllArgumentsAreExpressions T} then {Record.forAll Tree CheckExpr}
      elseif {AllArgumentsAreStatements T} then {Record.forAll Tree CheckStmt}
      elseif {FirstArgIsExpression T} then
	 First|Rest = {Record.toList Tree}
      in
	 {CheckExpr First} {ForAll Rest Check}
      elseif {PropagatesToAllArguments T} then {Record.forAll Tree Check}
      elseif {LogicStmt T} then Clauses = Tree.1 in
	 {ForAll Clauses Check}
      elsecase Tree
      of fFOR(_ Body ...) then {CheckStmt Body} %% TODO: parse Decl
      [] fClass(ClassName _ Meths ...) then
	 {CheckExpr ClassName} {ForAll Meths CheckStmt}
      [] fChoice(Choices ...) then {ForAll Choices Check}
      else %% unknown
	 {System.show Tree}
	 fail
	 {Record.forAll Tree Check}
      end
   end
   
   fun {ToProcName Proc}
      case Proc of fVar(A ...) then just({Atom.toString A})
      [] fOpApply('.' [fVar(Functor ...) fAtom(Feat ...)] ...) then
	 just({VirtualString.toString Functor#"."#Feat})
      else nothing
      end
   end

   fun {DollarInArgs As}
      {Some As
       fun {$ A}
	  case A of fDollar(...) then true
	  [] fRecord(_ Fs ...) then
	     {Some Fs fun {$ F}
			 case F of fColon(_ P) andthen {Label P} == fDollar then true
			 [] fDollar(...) then true
			 else false
			 end
		      end}
	  else false
	  end
       end
       }
   end
   
   {Main}
end