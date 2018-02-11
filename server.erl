-module(server).
-export([start/0, signUp/3, signIn/3, createFile/4, appendToFile/3, readFile/2, addFile/4, getFolderContent/1, createFolder/2, writeFile/2,
		getFileNames/2, addPermissions/3, getWritePermissions/2, getWritePermissionsShared/2, returnWritePermissions/2,
		returnWritePermissionsShared/2, moveToFolder/2, moveToParentFolder/1, readSharedFile/2, writeSharedFile/2, deleteFile/2,
		connect/2, signOut/1, setName/1, setVersionName/2, saveOldVersion/2, getPathForOldVersion/2]).

%%%%%%%%%%%% public functions

start() -> 	file:make_dir("Files"),
			%register(filedestroyer, spawn(fun() -> fileDestroyer() end)),
			register(server, spawn(fun() -> loop([]) end)).

connect(Node, Pid) ->
	rpc({connect, Node, Pid}).

signUp(Node, Username, Password) -> 
	rpc({signUp, Node, Username, Password}).

signIn(Node, Username, Password) ->
	rpc({signIn, Node, Username, Password}).

getFileNames(Node, Keyword) -> 
	rpc({getFileNames, Node, Keyword}).

readFile(Node, Name) -> 
	rpc({readFile, Node, Name}).

writeFile(Node, Name) ->
	rpc({writeFile, Node, Name}).

deleteFile(Node, Name) ->
	rpc({deleteFile, Node, Name}).

readSharedFile(Node, Path) -> 
	rpc({readSharedFile, Node, Path}).

writeSharedFile(Node, Path) ->
	rpc({writeSharedFile, Node, Path}).

addFile(Node, Name, File, Keywords) ->
	rpc({addFile, Node, Name, File, Keywords}).
	
addPermissions(Node, Name, NewUsers) -> 
	rpc({addPermissions, Node, Name, NewUsers}).

createFile(Node, Name, Content, Keywords) ->
	rpc({createFile, Node, Name, Content, Keywords}).

appendToFile(Node, Name, Bytes) ->
	rpc({appendToFile, Node, Name, Bytes}).

getWritePermissions(Node, Name) ->
	rpc({getWritePermissions, Node, Name}).

getWritePermissionsShared(Node, Path) ->
	rpc({getWritePermissionsShared, Node, Path}).

returnWritePermissions(Node, Name) ->
	rpc({returnWritePermissions, Node, Name}).

returnWritePermissionsShared(Node, Path) ->
	rpc({returnWritePermissionsShared, Node, Path}).

createFolder(Node, Name) ->
	rpc({createFolder, Node, Name}).

getFolderContent(Node) ->
	rpc({getFolderContent, Node}).

moveToFolder(Node, Name) ->
	rpc({moveToFolder, Node, Name}).

moveToParentFolder(Node) ->
	rpc({moveToParentFolder, Node}).

signOut(Node) ->
	rpc({signOut, Node}).

%% CHECK IF USER IS LOGGED IN
loginCheck(From, Node, Clients) ->
	case get(Node) of
		undefined -> 
			From ! {server, 'You need to sign in first'},
			loop(Clients);
		_ -> ok
	end.

%%%%%%%%%%%%%%%%% RPC

rpc(Request) ->
	server ! {self(), Request},
	receive
		{server, Response} -> Response
	end.

loop(Clients) ->
	process_flag(trap_exit, true),
	io:format("\n----------------------\n CLIENTS ONLINE: "),
	io:format(lists:flatten(io_lib:format("~p", [Clients]))),
	io:format(".\n----------------------\n NODES ONLINE: "),
	lists:map(fun({L, _})-> 
			case get(L) of
				undefined -> '';
				_ -> io:format(lists:flatten(io_lib:format("~p, ", [get(L)])))
			end
		end, Clients),
	
	io:format(".\n----------------------\n"),
	
	receive
		{From, {connect, Node, Pid}} ->
			link(Pid),
			From ! {server, ok},
			loop([{Node, Pid} | Clients]);
		{From, {signUp, Node, Username, Password}} ->
			Ret = privateSignUp(Node, Username, Password),
			From ! {server, Ret},
			loop(Clients);
		{From, {signIn, Node, Username, Password}} ->
			Ret = privateSignIn(Node, Username, Password),
			From ! {server, Ret},
			loop(Clients);
		{From, {addFile, Node, Name, File, Keywords}} -> 
			loginCheck(From, Node, Clients),
			Ret = privateAddFile(Node, Name, File, Keywords),
			From ! {server, Ret},
			loop(Clients);
		{From, {deleteFile, Node, Name}} -> 
			loginCheck(From, Node, Clients),
			Ret = privateDeleteFile(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {createFile, Node, Name, Content, Keywords}} ->
			loginCheck(From, Node, Clients),
			Ret = privateCreateFile(Node, Name, Content, Keywords),
			From ! {server, Ret},
			loop(Clients);
		{From, {readFile, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateReadFile(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {readSharedFile, Node, Path}} ->
			loginCheck(From, Node, Clients),
			Ret = privateReadSharedFile(Node, Path),
			From ! {server, Ret},
			loop(Clients);
		{From, {writeFile, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateWriteFile(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {writeSharedFile, Node, Path}} ->
			loginCheck(From, Node, Clients),
			Ret = privateWriteSharedFile(Node, Path),
			From ! {server, Ret},
			loop(Clients);
		{From, {getWritePermissions, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateGetWritePermissions(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {getWritePermissionsShared, Node, Path}} ->
			loginCheck(From, Node, Clients),
			Ret = privateGetWritePermissionsShared(Node, Path),
			From ! {server, Ret},
			loop(Clients);
		{From, {returnWritePermissions, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateReturnWritePermissions(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {returnWritePermissionsShared, Node, Path}} ->
			loginCheck(From, Node, Clients),
			Ret = privateReturnWritePermissionsShared(Node, Path),
			From ! {server, Ret},
			loop(Clients);
		{From, {getFileNames, Node, Keyword}} ->
			loginCheck(From, Node, Clients),
			Ret = privateGetFileNames(Node, Keyword),
			From ! {server, Ret},
			loop(Clients);
		{From, {addPermissions, Node, Name, NewUsers}} ->
			loginCheck(From, Node, Clients),
			Ret = privateAddPermissions(Node, Name, NewUsers),
			From ! {server, Ret},
			loop(Clients);
		{From, {createFolder, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateCreateFolder(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {getFolderContent, Node}} ->
			loginCheck(From, Node, Clients),
			Ret = privateGetFolderContent(Node),
			From ! {server, Ret},
			loop(Clients);
		{From, {moveToFolder, Node, Name}} ->
			loginCheck(From, Node, Clients),
			Ret = privateMoveToFolder(Node, Name),
			From ! {server, Ret},
			loop(Clients);
		{From, {moveToParentFolder, Node}} ->
			loginCheck(From, Node, Clients),
			Ret = privateMoveToParentFolder(Node),
			From ! {server, Ret},
			loop(Clients);
		{From, {signOut, Node}} ->
			loginCheck(From, Node, Clients),
			Pid = getClientPid(Node, Clients),
			Ret = privateSignOut(Pid, Clients),
			From ! {server, Ret},
			loop(Clients);
		{'EXIT', From, Why} ->
			Term = [From, Why],
			io:format(lists:flatten(io_lib:format("~p", [Term]))),
			privateExit(From, Clients),
			loop(remove(From, Clients));
		{From, _} ->
			From ! {server, 'invalid argument'},
			loop(Clients)
	end.

%%%%%%%%%%%%%%%%% PRIVATE FUNCTION

%read_text(IoDevice, File) ->
%	case file:read_line(IoDevice) of
%		({ok, Line}) -> read_text(IoDevice, File++Line);
%		(eof)        -> File
%	end.

%get_owners([]) -> [];
%get_owners([{file, Owner, _, _, _, _}|T]) -> [Owner]++get_owners(T).

getParentFolder([H1,H2,_|T]) -> H1++"/"++getParentFolder([H2]++T);
getParentFolder([H|_]) -> H.

getClientPid(Node, Clients) ->
	{_, Pid} = hd(lists:filter(fun({Node2, _}) -> Node =:= Node2 end, Clients)),
	Pid.

remove(From, Clients) ->  
	lists:filter(fun({_, Pid}) -> Pid =/= From end, Clients).

upVersion(Version) ->
	[L, D] = string:tokens(Version, "."),
	L++"."++integer_to_list(list_to_integer(D)+1).

setName(Name) ->
	case string:tokens(Name, ".") of
		[_, _ | _] -> Name;
		[H] -> H++".txt"
	end.

setVersionName(Name, Version) ->
	case string:tokens(Name, ".") of
		[H1, H2 | _] -> H1++Version++"."++H2;
		[H] -> H++Version++".txt"
	end.

saveOldVersion(Path, Version) -> 
	NewPath = getPathForOldVersion(string:tokens(Path, "/"), Version),
	filelib:ensure_dir(NewPath),
	{ok, File} = file:read_file(Path),
	file:write_file(NewPath, File).



getPathForOldVersion([H | []], Version) -> setVersionName(H, Version);
getPathForOldVersion([H | T], Version) when H =:= "Files" -> "OldFiles/"++getPathForOldVersion(T, Version);
getPathForOldVersion([H | T], Version) -> H++"/"++getPathForOldVersion(T, Version).

%% PRIVATE RPC FUNCTIONS

privateSignUp(Node, Username, Password) ->
	case dblogic:getUser(Username) of
	[] -> 
		put(Node, "Files/"++atom_to_list(Username)),
		file:make_dir("Files/"++atom_to_list(Username)),
		file:make_dir("Files/"++atom_to_list(Username)++"/Shared"),
		dblogic:resetUsersUid(Node),
		dblogic:addUser(Node, Username, Password),
		Ret = ok;
	_ -> 
		Ret = 'user already exits'
	end,
	Ret.

privateSignIn(Node, Username, Password) ->
	case dblogic:getLogin(Username, Password) of
		[] -> 
			Ret = 'wrong username or password.';
		[{user, _, Username, Password}] ->
			put(Node, "Files/"++atom_to_list(Username)),
			dblogic:resetUsersUid(Node),
			dblogic:deleteUser(Username),
			dblogic:addUser(Node, Username, Password),
			Ret = ok
	end,
	Ret.

privateAddFile(Node, Name, File, Keywords) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(get(Node)++"/"++setName(Name)) of
		{ok, _} ->
			Ret = 'You already own a file with that name';
		_ ->
			file:write_file(get(Node)++"/"++setName(Name), File),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			Log = atom_to_list(Username)++" added a new file named:  "++setName(Name)++", at "++integer_to_list(Hour)++":"
					++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."
					++integer_to_list(Month)++"."++integer_to_list(Year)++".",
			dblogic:addFile(Username, setName(Name), get(Node)++"/"++setName(Name), "1.0", [Log], [Username], Keywords, writable),
			Ret = ok
	end,
	Ret.

privateDeleteFile(Node, Name) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(get(Node)++"/"++setName(Name)) of
		{ok, _} ->
			dblogic:deleteFile(Username, get(Node)++"/"++setName(Name)),
			os:cmd("rm -f "++get(Node)++"/"++setName(Name)),
			Ret = ok;
		_ ->
			Ret = 'A file with that name was not found'
	end,
	Ret.

privateCreateFile(Node, Name, Content, Keywords) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(get(Node)++"/"++Name) of
		{ok, _} ->
			Ret = 'You already own a file with that name';
		_ ->
			file:write_file(get(Node)++"/"++Name, Content),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			Log = atom_to_list(Username)++" created a new file named:  "++Name++", at "++integer_to_list(Hour)++":"
					++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."
					++integer_to_list(Month)++"."++integer_to_list(Year)++".",
			dblogic:addFile(Username, setName(Name), get(Node)++"/"++setName(Name), "1.0", [Log], [Username], Keywords, writable),
			Ret = ok
	end,
	Ret.

privateReadFile(Node, Name) ->
	case file:read_file_info(get(Node)++"/"++Name) of
		{ok, _} ->
			Cmd = "cp "++get(Node)++"/"++setName(Name)++" "++get(Node)++"/tmp"++setName(Name)++" && (gedit "++get(Node)++"/tmp"++setName(Name)++" &) && chmod 444 "++get(Node)++"/tmp"++setName(Name),
			os:cmd(Cmd),
			timer:sleep(5000),
			os:cmd("rm -f "++get(Node)++"/tmp"++setName(Name)),
			Ret = ok;
		_ ->
			Ret = 'File was not found or you do not have permissions.'
	end,
	Ret.

privateReadSharedFile(Node, Path) -> 
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(Path) of
		{ok, _} ->
			case dblogic:getFile(Username, Path) of
				[] -> Ret = 'File was not found or you do not have permissions.';
				[{file, _, Name, Path, _, _, _, _, _}] ->
					Cmd = "cp "++Path++" "++get(Node)++"/tmp"++Name++" && (gedit "++get(Node)++"/tmp"++Name++" &) && chmod 444 "++get(Node)++"/tmp"++Name,
					os:cmd(Cmd),
					timer:sleep(5000),
					os:cmd("rm -f "++get(Node)++"/tmp"++Name),
					Ret = ok
			end;
		_ ->
			Ret = 'File was not found or you do not have permissions.'
	end,
	Ret.

privateWriteFile(Node, Name) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(get(Node)++"/"++setName(Name)) of
		{ok, _} ->
			case dblogic:getFile(Username, get(Node)++"/"++setName(Name)) of
				[] -> 
					Ret = 'File was not found or you do not have permissions.';
				[{file, _, _, _, _, _, _, _, Username}] ->
					Cmd = "gedit "++get(Node)++"/"++setName(Name)++" &",
					os:cmd(Cmd),
					Ret = ok;
				_ -> Ret = 'You need to request write permissions before editing a file.'
			end;
		_ ->
			Ret = 'File was not found or you do not have permissions.'
	end,
	Ret.

privateWriteSharedFile(Node, Path) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case file:read_file_info(Path) of
		{ok, _} ->
			case dblogic:getFile(Username, Path) of
				[] -> 
					Ret = 'File was not found or you do not have permissions.';
				[{file, _, _, _, _, _, _, _, Username}] ->
					Cmd = "gedit "++Path++" &",
					os:cmd(Cmd),
					Ret = ok;
				_ -> Ret = 'You need to request write permissions before editing a file.'
			end;
		_ ->
			%filelib:ensure_dir(get(Node)),
			Ret = 'File was not found or you do not have permissions.'
	end,
	Ret.

privateGetWritePermissions(Node, Name) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case dblogic:getFile(Username, get(Node)++"/"++Name) of
		[] -> 
			Ret = 'File was not found or you do not have permissions.';
		[{file, Owner, Name, Path, Version, Log, Users, Keywords, writable}] ->
			dblogic:deleteFile(Owner, Path),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			dblogic:addFile(Owner, Name, Path, Version, Log++[atom_to_list(Username)++" started editing this file at "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."++integer_to_list(Month)++"."++integer_to_list(Year)++"."], Users, Keywords, Username),
			Ret = ok;
		_ ->
			Ret = 'Cannot grant write permissions, file is being edited.'
	end,
	Ret.

privateGetWritePermissionsShared(Node, Path) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case dblogic:getFile(Username, Path) of
		[] -> 
			Ret = 'File was not found or you do not have permissions.';
		[{file, Owner, Name, Path, Version, Log, Users, Keywords, writable}] ->
			dblogic:deleteFile(Owner, Path),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			dblogic:addFile(Owner, Name, Path, Version, Log++[atom_to_list(Username)++" started editing this file at "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."++integer_to_list(Month)++"."++integer_to_list(Year)++"."], Users, Keywords, Username),
			Ret = ok;
		_ ->
			Ret = 'Cannot grant write permissions, file is being edited.'
	end,
	Ret.

privateReturnWritePermissions(Node, Name) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case dblogic:getFile(Username, get(Node)++"/"++Name) of
		[] -> 
			Ret = 'File was not found or you do not have permissions.';
		[{file, Owner, Name, Path, Version, Log, Users, Keywords, Username}] ->
			dblogic:deleteFile(Owner, Path),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			dblogic:addFile(Owner, Name, Path, upVersion(Version), Log++[atom_to_list(Username)++" stoped editing this file at "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."++integer_to_list(Month)++"."++integer_to_list(Year)++"."], Users, Keywords, writable),
			saveOldVersion(Path, Version),
			Ret = ok;
		_ ->
			Ret = 'You do not have write permissions over this file.'
	end,
	Ret.

privateReturnWritePermissionsShared(Node, Path) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case dblogic:getFile(Username, Path) of
		[] -> 
			Ret = 'File was not found or you do not have permissions.';
		[{file, Owner, Name, Path, Version, Log, Users, Keywords, Username}] ->
			dblogic:deleteFile(Owner, Path),
			{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
			dblogic:addFile(Owner, Name, Path, upVersion(Version), Log++[atom_to_list(Username)++" stoped editing this file at "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."++integer_to_list(Month)++"."++integer_to_list(Year)++"."], Users, Keywords, writable),
			saveOldVersion(Path, Version),
			Ret = ok;
		_ ->
			Ret = 'You do not have write permissions over this file.'
	end,
	Ret.

privateAddPermissions(Node, Name, NewUsers) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	case dblogic:getFile(Username, get(Node)++"/"++setName(Name)) of
		[{file, Username, Name, Path, Version, Log, Users, Keywords, writable}] ->
			dblogic:deleteFile(Username, Path),
			dblogic:addFile(Username, Name, Path, Version, Log, Users ++ NewUsers, Keywords, writable),
			Ret = {ok, Name};
		[{file, Username, _, _, _, _, _, _, _}] -> Ret = 'Cannot add permissions while file is being edited';
		_ -> Ret = 'File was not found or you do not have permissions'
	end,
	Ret.

privateGetFileNames(Node, Keyword) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	dblogic:getAllFiles(Username, Keyword).

privateCreateFolder(Node, Name) ->
	case file:make_dir(get(Node)++"/"++Name) of
		{error, eexist} -> Ret = 'Error, directory already exits';
		{error, enoent} -> Ret = 'Error, a component of dir does not exist';
		{error, enospc} -> Ret = 'Error, no space left';
		{error, _} -> Ret = 'Unknown error';
		ok -> Ret = ok
	end,
	Ret.

privateGetFolderContent(Node) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	Tmp = "Files/"++atom_to_list(Username)++"/Shared",
	case get(Node) of
		Tmp -> Ret = dblogic:getSharedFiles(Username);
		_ -> Ret = string:tokens(os:cmd("ls "++get(Node)), "\n")
	end,
	Ret.

privateMoveToFolder(Node, Name) ->
	case filelib:is_dir(get(Node)++"/"++Name) of
		true -> 
			put(Node, get(Node)++"/"++Name),
			Ret = ok;
		false ->
			Ret = 'Folder does not exist'
	end,
	Ret.

privateMoveToParentFolder(Node) ->
	{user, _, Username, _} = hd(dblogic:getUserByUid(Node)),
	Tmp = "Files/"++atom_to_list(Username),
	case get(Node) of
		Tmp -> 
			Ret = 'You do not have permissions for that folder';
		_ ->
			put(Node, getParentFolder(string:tokens(get(Node), "/"))),
			Ret = ok
	end,
	Ret.

privateSignOut(Pid, Clients) ->
	{Node, Pid} = hd(lists:filter(fun({_, From}) -> Pid =:= From end, Clients)),
	{user, _, Username, Password} = hd(dblogic:getUserByUid(Node)),
	erase(Node),
	dblogic:deleteUser(Username),
	dblogic:addUser('', Username, Password),
	dblogic:returnAllWritePermissions(Username),
	ok.

privateExit(From, Clients) ->
	{Node, From} = hd(lists:filter(fun({_, Pid}) -> Pid =:= From end, Clients)),
	case get(Node) of
		undefined -> ok;
		_ -> privateSignOut(From, Clients)
	end,
	ok.