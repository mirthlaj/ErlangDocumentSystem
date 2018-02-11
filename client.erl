-module(client).
-export([signUp/2, signIn/2, createFile/3, addFile/3, appendToFile/2, readFile/1, createFolder/1, getFolderContent/0, writeFile/1,
		getFileNames/1, addPermissions/2, getWritePermissions/1, returnWritePermissions/1, getWritePermissionsShared/1, 
		returnWritePermissionsShared/1, moveToFolder/1, moveToParentFolder/0, readSharedFile/1, writeSharedFile/1,
		deleteFile/1, connect/1, signOut/0]).

connect(Server) ->
	register(client, spawn(fun() -> start(Server) end)).

start(Server) ->
	Ret = rpc:call(Server, server, connect, [node(), self()]),
	io:format(pid_to_list(self())),
	loop(Server, node()),
	Ret.

loop(Server, Node) ->
	receive
		{From, {signUp, Username, Password}} ->
			Ret = rpc:call(Server, server, signUp, [Node, Username, Password]),
			From ! {client, Ret},
			loop(Server, Node);
		{From, {signIn, Username, Password}} ->
			io:format(pid_to_list(self())),
			From ! {client, rpc:call(Server, server, signIn, [Node, Username, Password])},
			loop(Server, Node);
		{From, {getFileNames, Keyword}} ->
			From ! {client, rpc:call(Server, server, getFileNames, [Node, Keyword])},
			loop(Server, Node);
		{From, {readFile, Name}} ->
			From ! {client, rpc:call(Server, server, readFile, [Node, Name])},
			loop(Server, Node);
		{From, {readSharedFile, Path}} ->
			From ! {client, rpc:call(Server, server, readSharedFile, [Node, Path])},
			loop(Server, Node);
		{From, {writeFile, Name}} ->
			From ! {client, rpc:call(Server, server, writeFile, [Node, Name])},
			loop(Server, Node);
		{From, {deleteFile, Path}} ->
			From ! {client, rpc:call(Server, server, deleteFile, [Node, Path])},
			loop(Server, Node);
		{From, {writeSharedFile, Path}} ->
			From ! {client, rpc:call(Server, server, writeSharedFile, [Node, Path])},
			loop(Server, Node);
		{From, {addPermissions, Name, NewUsers}} ->
			From ! {client, rpc:call(Server, server, addPermissions, [Node, Name, NewUsers])},
			loop(Server, Node);
		{From, {createFile, Name, Content, Keywords}} ->
			From ! {client, rpc:call(Server, server, createFile, [Node, Name, Content, Keywords])},
			loop(Server, Node);
		{From, {addFile, Name, File, Keywords}} ->
			From ! {client, rpc:call(Server, server, addFile, [Node, Name, File, Keywords])},
			loop(Server, Node);
		{From, {appendToFile, Name, Bytes}} ->
			From ! {client, rpc:call(Server, server, appendToFile, [Node, Name, Bytes])},
			loop(Server, Node);
		{From, {getWritePermissions, Name}} ->
			From ! {client, rpc:call(Server, server, getWritePermissions, [Node, Name])},
			loop(Server, Node);
		{From, {getWritePermissionsShared, Path}} ->
			From ! {client, rpc:call(Server, server, getWritePermissionsShared, [Node, Path])},
			loop(Server, Node);
		{From, {returnWritePermissions, Name}} ->
			From ! {client, rpc:call(Server, server, returnWritePermissions, [Node, Name])},
			loop(Server, Node);
		{From, {returnWritePermissionsShared, Path}} ->
			From ! {client, rpc:call(Server, server, returnWritePermissionsShared, [Node, Path])},
			loop(Server, Node);
		{From, {createFolder, Name}} ->
			From ! {client, rpc:call(Server, server, createFolder, [Node, Name])},
			loop(Server, Node);
		{From, {getFolderContent}} ->
			From ! {client, rpc:call(Server, server, getFolderContent, [Node])},
			loop(Server, Node);
		{From, {moveToFolder, Name}} ->
			From ! {client, rpc:call(Server, server, moveToFolder, [Node, Name])},
			loop(Server, Node);
		{From, {moveToParentFolder}} ->
			From ! {client, rpc:call(Server, server, moveToParentFolder, [Node])},
			loop(Server, Node);
		{From, {signOut}} ->
			From ! {client, rpc:call(Server, server, signOut, [Node])},
			loop(Server, Node)
	end.

rpc(Request) ->
	client ! {self(), Request},
	receive
		{client, Response} -> Response
	end.

signUp(Username, Password) -> rpc({signUp, Username, Password}).

signIn(Username, Password) -> rpc({signIn, Username, Password}).

getFileNames(Keyword) -> rpc({getFileNames, Keyword}).

readFile(Name) -> rpc({readFile, Name}).

readSharedFile(Path) -> rpc({readSharedFile, Path}).

writeFile(Name) -> rpc({writeFile, Name}).

deleteFile(Name) -> rpc({deleteFile, Name}).

writeSharedFile(Path) -> rpc({writeSharedFile, Path}).

addPermissions(Name, NewUsers) -> rpc({addPermissions, Name, NewUsers}).

createFile(Name, Content, Keywords) -> rpc({createFile, Name, Content, Keywords}).

addFile(Name, File, Keywords) -> rpc({addFile, Name, File, Keywords}).

appendToFile(Name, Bytes) -> rpc({appendToFile, Name, Bytes}).

getWritePermissions(Name) -> rpc({getWritePermissions, Name}).

getWritePermissionsShared(Path) ->rpc({getWritePermissionsShared,Path}).

returnWritePermissions(Name) -> rpc({returnWritePermissions, Name}).

returnWritePermissionsShared(Path) -> rpc({returnWritePermissionsShared, Path}).

createFolder(Name) -> rpc({createFolder, Name}).

getFolderContent() -> rpc({getFolderContent}).

moveToFolder(Name) -> rpc({moveToFolder, Name}).

moveToParentFolder() -> rpc({moveToParentFolder}).

signOut() -> rpc({signOut}).










