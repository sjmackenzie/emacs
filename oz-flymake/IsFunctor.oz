%%
%%  Analyzes an Oz source file and returns 0 if it looks like a functor and 1 otherwise.
%%
functor
import
   Open
   Application
   Regex at 'x-oz://contrib/regex'
define
   fun lazy {ReadLinesFrom File}
      case {File getS($)}
      of false then nil
      [] Line then Line|{ReadLinesFrom File}
      end
   end

   class TextFile from Open.file Open.text end

   [Filename] = {Application.getArgs plain}
   Src = {New TextFile init(name:Filename flags:[read text])}

   %% if the first non-empty, non-comment line starts with "functor"
   Res = 
   for Line in {ReadLinesFrom Src} default:1 return:Return do
      if {Regex.search "^[ \t]*$" Line} \= false then skip %% skip empty lines
      elseif {Regex.search "^[ \t]*%" Line} \= false then skip %% skip comment lines
      else
	 if {Regex.search "^[^%\n]*functor" Line} \= false then {Return 0}
	 else {Return 1}
	 end
      end
   end
   {Src close}
   {Application.exit Res}
end
