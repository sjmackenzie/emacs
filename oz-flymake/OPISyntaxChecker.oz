%%
%% Compile Oz source code as if fed to the emulator in the OPI.
%% Does not generate any code but prints the normal compiler error messages to stdout.
%% (For use with Emacs flymake).
%%
functor
import
   OPIEnv at 'x-oz://system/OPIEnv.ozf'
   Compiler
   OS
   %% Path was buggy before Mozart 1.4.0, but Path.exists was OK
   Path at 'x-oz://system/os/Path.ozf'
   Application
   System
   Error
define
   local
      /** %% Returns ozrc file according conventions (cf. oz/doc/opi/node4.html).
      %% */
      fun {GetInitFile}
	 if {OS.getEnv 'OZRC'} \= false
	 then {OS.getEnv 'OZRC'}
	 elseif {Path.exists {OS.getEnv 'HOME'}#'/.oz/ozrc'}
	 then {OS.getEnv 'HOME'}#'/.oz/ozrc'
	 elseif {Path.exists {OS.getEnv 'HOME'}#'/.ozrc'}
	 then {OS.getEnv 'HOME'}#'/.ozrc'
	 else nil
	 end
      end
   in
      /** %% Feeds OZRC file to MyCompiler. The OZRC is searched for at the usual places according conventions (cf. oz/doc/opi/node4.html).
      %% */ 
      proc {FeedInitFile MyCompiler}
	 InitFile = {GetInitFile}
      in
	 if InitFile \= nil then
	    {MyCompiler enqueue(feedFile(InitFile))}
	 end
      end
   end
   ErrorPrinter
   thread
      Stream
   in
      ErrorPrinter = {NewPort Stream}
      for Msg in Stream do
	 case Msg of message(Msg ...) then
	    {System.showInfo {Error.messageToVirtualString Msg}}
	 [] quit then {Application.exit 0}
	 else skip
	 end
      end
   end
   [Filename] = {Application.getArgs plain}
   Engine = {New Compiler.engine init}
   {Engine enqueue(mergeEnv(OPIEnv.full))}
   {Engine register(ErrorPrinter)}
   {FeedInitFile Engine}
   {Engine enqueue(setSwitch(feedtoemulator false))}
   {Engine enqueue(setSwitch(codegen false))}
   {Engine enqueue(feedFile(Filename unit))}
   {Wait {Engine enqueue(ping($))}}
   %% we exit in this way to ensure that no messages are lost
   {Port.send ErrorPrinter quit}
end
