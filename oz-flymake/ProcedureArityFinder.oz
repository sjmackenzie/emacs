functor
import
   Compiler at 'x-oz://system/Compiler'
   Module at 'x-oz://system/Module.ozf'
   Narrator at 'x-oz://system/Narrator'
   Open at 'x-oz://system/Open'
   OPIEnv at 'x-oz://system/OPIEnv.ozf'
   OS at 'x-oz://system/OS.ozf'
   Property at 'x-oz://system/Property.ozf' 
   Regex at 'x-oz://contrib/regex'
export
   FindArity
define
   %%
   %% General support functions
   %%
   fun {FileExists FileName}
      try F in
	 F = {New Open.file init(name: FileName flags: [read])}
	 {F close()}
	 true
      catch _ then false
      end
   end

   fun {RemoveSingleQuotes Str}
      case Str of &'|R then
	 {RemoveSingleQuotes R}
      [] S|R then S|{RemoveSingleQuotes R}
      [] nil then nil
      end
   end

   %% Like CondSelect.
   %% \return Maybe T
   local
      NotFound = {NewName}
   in
      fun {MaybeSelect Rec Feat}
	 case {CondSelect Rec Feat NotFound}
	 of !NotFound then nothing
	 [] X then just(X)
	 end
      end
   end

   %% Escape characters with special meaning in regexs.
   %% incomplete
   fun {REscape Xs}
      case Xs
      of &$|Xr then &\\|&$|{REscape Xr}
      [] X|Xr then X|{REscape Xr}
      [] nil then nil
      end
   end
   
   fun {SubList Xs S E}
      {List.take {List.drop Xs S} E-S}
   end

   %% Create a syntax tree for some Oz code.
   %% \param Str Oz source code as a string.
   fun {Parse Str}
      {Compiler.parseOzVirtualString
       Str
       {New Narrator.'class' init($) _}
       fun {$ _} false end 
       {NewDictionary}
      }
   end

 
   %%
   %% Finding values in context
   %%
   
   %% Result: a record that maps local functor names to URLs
   %% and local procedure names to pairs of functors names and procedure export names,
   %% e.g.:
   %% 
   %% unit('FunctorName':'Functor.ozf'
   %%      'Add':'FunctorName'#add
   %%      'AnotherFunctor':'path/AnotherFunctor.ozf'
   %%      'Remove':'AnotherFunctor'#remove
   %% )
   %%
   %% \param Buffer source code of a functor as a string
   %% \return lookup record (see above); 'empty record' in case of parse error
   local
      CacheLock = {Lock.new}
      LastBuffer = {NewCell nil}
      LastImportSection = {NewCell nothing}
   in
      fun {ParseImportSection Buffer}
	 %% \param Item part of import spec as syntax tree 
	 %% \return [PrintName#Location] where Location can be a functor location or a pair Functor#feature
	 fun {ExtractImportItem Item}
	    case Item of fImportItem(fVar(ModuleName _)
				     Bindings
				     ImportAt) then
	       {Append
		case ImportAt of fImportAt(fAtom(I _)) then [ModuleName#I] else nil end
		{Map {Filter Bindings
		      fun {$ B} case B of fVar(_ _)#fAtom(_ _) then true else false end end
		     }
		 fun {$ fVar(LocalName _)#fAtom(ExportName _)} LocalName#(ModuleName#ExportName) end
		}
	       }
	    end
	 end
	 
	 fun {ExtractImports Clause}
	    case Clause
	    of fImport(Items _) then {Map Items ExtractImportItem}
	    [] fRequires(Items _) then {Map Items ExtractImportItem}
	    else nil
	    end
	 end
	 
	 %% don't parse the complete text as it might be incomplete and have syntax errors
	 %% -> remove everything after the first 'define' or 'prepare' (outside of comments) and add "define skip end".
	 Head = {Value.byNeed fun {$}
				 {Append
				  {ByteString.toString {Regex.split "^[^%\n]*(define|prepare)" Buffer}.1}
				  " define skip end"}
			      end}
      in
	 lock CacheLock then
	    if Buffer == @LastBuffer then @LastImportSection
	    else
	       case {Parse Head}
	       of [fFunctor(_ Fs _)] then
		  LastBuffer := Buffer
		  LastImportSection := {List.toRecord unit
					{Flatten {Map Fs ExtractImports}}}
		  @LastImportSection
	       else %% not a syntactically correct functor head
		  unit(unit:unit)
	       end
	    end
	 end
      end
   end
   
   %% Define FullEnvironment (a compiler environment like in the OPI)
   local
      OPICompiler = {New Compiler.engine init}
   in
      %% add the full OPI environment
      {OPICompiler enqueue(mergeEnv(OPIEnv.full))}
      %% feed code in OZRC (OPI user configuration) (from OPI.oz)
      case {OS.getEnv 'HOME'} of false then skip
      elseof HOME then
	 OZRC = {OS.getEnv 'OZRC'}
      in
	 if OZRC \= false andthen {FileExists OZRC} then
	    {OPICompiler enqueue(feedFile(OZRC))}
	 elseif {FileExists {Property.get 'oz.dotoz'}#'/ozrc'} then
	    {OPICompiler enqueue(feedFile({Property.get 'oz.dotoz'}#'/ozrc'))}
	 elseif {FileExists HOME#'/.oz/ozrc'} then
	    {OPICompiler enqueue(feedFile(HOME#'/.oz/ozrc'))}
	 elseif {FileExists HOME#'/.ozrc'} then   % note: deprecated
	    {OPICompiler enqueue(feedFile(HOME#'/.ozrc'))}
	 end
      end

      FullEnvironment = {OPICompiler enqueue(getEnv($))}
   end

   ModuleDict = {NewDictionary}
   ModuleDictLock = {NewLock}
   
   %% Returns the value named in CurrentWord in the context of CurrentBuffer.
   %% \param CurrentWord Oz-word under the cursor
   %% \param CurrentBuffer contents of current Emacs buffer as a string
   %% \param LinkPatterns patterns for dynamic linking (see --linkPattern option)
   %% \return just(value) or nothing
   fun {GetNamedValue CurrentWord CurrentBuffer LinkPatterns}
      ImportSection = {Value.byNeed fun {$} {ParseImportSection CurrentBuffer} end}

      %% Get module by functor location.
      %% \return Maybe Module
      fun {LinkModule Loc}
	 [M] = {Module.link [Loc]}
      in
	 try
	    {Wait M} %% make it needed to find out about failure right here
	    just(M)
	 catch _ then nothing end
      end

      %% Find a module which is dynamically linked.
      %% \return Maybe Module
      fun {FindDynamicallyLinkedModule Functor}
	 DefaultPattern = "[ <Name> ] = { Module.link ![ <URL> ] }" % normal " ": optional; " !": mandatory space
	 URLRegex = "(\"|')[^\"']+(\"|')"
	 %% Create a regex from a linking pattern
	 fun {PatternToRegex Pattern}
	    case Pattern
	    of &<|&N|&a|&m|&e|&>|R then
	       Functor#{PatternToRegex R}
	    [] &<|&U|&R|&L|&>|R then
	       URLRegex#{PatternToRegex R}
	    [] & |&!|R then % mandatory whitespace
	       "[ \t\n]+"#{PatternToRegex R}
	    [] & |R then % optional whitespace
	       "[ \t\n]*"#{PatternToRegex R}
	    [] &[|R then % escape [
	       "\\["#{PatternToRegex R}
	    [] &]|R then % escape ]
	       "\\]"#{PatternToRegex R}
	    [] X|R then
	       [X]#{PatternToRegex R}
	    [] nil then nil
	    end
	 end
	 Regexs = {Map {Map DefaultPattern|LinkPatterns PatternToRegex} VirtualString.toString}
      in
	 for R in Regexs return:Return default:nothing do
	    case {Regex.search R CurrentBuffer}
	    of match(0:Start#_ ...) then
	       Linking = {List.drop CurrentBuffer Start}
	    in
	       case {Regex.search URLRegex Linking}
	       of match(0:S#E ...) then
		  URL = {String.toAtom {SubList Linking S+1 E-1}}
	       in
		  {Return {LinkModule URL}}
	       else skip
	       end
	    else skip
	    end
	 end
      end
      
      %% Get module by local functor print name.
      %% \return Maybe Module
      fun {FindModule Functor}
	 lock ModuleDictLock then
	    if {Dictionary.member ModuleDict Functor} then
	       ModuleDict.Functor
	    else
	       ModuleDict.Functor :=
	       case {MaybeSelect ImportSection Functor}
	       of just(URL) then {LinkModule URL}
	       [] nothing then
		  case {MaybeSelect FullEnvironment Functor}
		  of just(M) then just(M)
		  [] nothing then %% try to load local functor imported without URL
		     case {LinkModule {VirtualString.toAtom {Atom.toString Functor}#".ozf"}}
		     of just(M) then just(M)
		     [] nothing then %% try to load system functor
			case {LinkModule {VirtualString.toAtom "x-oz://system/"#{Atom.toString Functor}#".ozf"}}
			of just(M) then just(M)
			[] nothing then %% try to load functor imported with "Module.link" or similar
			   {FindDynamicallyLinkedModule Functor}
			end
		     end
		  end
	       end
	       ModuleDict.Functor
	    end
	 end
      end

      %% \return Maybe Procedure
      fun {LookupQualifiedValue Function Functor}
	 case {FindModule Functor}
	 of just(M) then
	    case {MaybeSelect M Function} of just(V) then just(V)
	    [] nothing then missing
	    end
	 [] nothing then nothing
	 end
      end

      Functor Function
   in
      %% Is it a qualified identifier?
      {String.token CurrentWord &. ?Functor ?Function}
      if Function \= nil then
	 {LookupQualifiedValue {String.toAtom {RemoveSingleQuotes Function}}
	  {String.toAtom Functor}}
      else
	 CW = {String.toAtom {RemoveSingleQuotes CurrentWord}}
      in
	 %% otherwise: try to find out the context from the import section
	 case {MaybeSelect ImportSection CW}
	 of just(Functor#Function) then
	    {LookupQualifiedValue Function Functor}
	 [] nothing then %% or take it from the environment
	    {MaybeSelect FullEnvironment CW}
	 end
      end
   end %% GetNamedValue
   
   fun {FindArity ProcName FunctorDef LinkPatterns}
      case {GetNamedValue ProcName FunctorDef LinkPatterns}
      of just(P) andthen {Procedure.is P} then just({Procedure.arity P})
      [] nothing then unknown
      [] missing then missing
      end
   end   
end
