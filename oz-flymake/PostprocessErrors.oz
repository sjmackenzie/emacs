%%
%%  Merge error messages of the Oz compiler into single lines
%%  such that Emacs flymake can parse them.
%%
functor
import
   Open
   Application
define
   fun lazy {ReadLinesFrom File}
      case {File getS($)}
      of false then nil
      [] Line then Line|{ReadLinesFrom File}
      end
   end

   fun lazy {MergeErrorLines Lines}
      fun {RemoveCR X}
	 {Filter X fun {$ Y} Y \= &\r end}
      end
      fun lazy {Merge Xs CL}
	 case Xs of X|Xr then
	    case X of &%|&*|&*|R then
	       {Merge Xr {Append CL {RemoveCR R}}}
	    else
	       CL|X|{Merge Xr nil}
	    end
	 else
	    [CL]
	 end
      end
   in
      {Merge Lines nil}
   end
   
   proc {WriteLinesTo File Xs}
      case Xs of X|Xr then
	 {File putS(X)}
	 {WriteLinesTo File Xr}
      else skip
      end
   end

   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin flags:[read text])
	   }
   StdErr = {New TextFile init(name:stderr flags:[write text])}
   {WriteLinesTo StdErr {MergeErrorLines {ReadLinesFrom StdIn}}}
   {Application.exit 0}
end
