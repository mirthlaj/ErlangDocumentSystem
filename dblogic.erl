-module(dblogic).
-include_lib("stdlib/include/qlc.hrl").
-export([init/0, addUser/3, getUser/1, getLogin/2, deleteUser/1, addFile/8, getSharedFiles/1, %getFileUsers/1,
		getUserByUid/1,	resetUsersUid/1, getAllFiles/2, getFile/2, deleteFile/2, returnAllWritePermissions/1]).

-record(user, {uid, username, password}).

-record(file, {owner, name, path, version, log, users, keywords, access}).

init() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(type, user)
	catch
		exit: _ ->
			mnesia:create_table(user, [{attributes, record_info(fields, user)}, {type,bag}, {disc_copies, [node()]}])
	end,
	try
		mnesia:table_info(type, file)
	catch
		exit: _ ->
			mnesia:create_table(file, [{attributes, record_info(fields, file)}, {type,bag}, {disc_copies, [node()]}])
	end.
	

addUser(Uid, Username, Password) ->
	AF = fun() ->
		mnesia:write(#user{uid = Uid, username = Username, password = Password})
	end,
	mnesia:transaction(AF).


deleteUser(Username) ->
	AF = fun() ->
			Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= Username]),
			Results = qlc:e(Query),
	
			F = fun() -> 
				lists:foreach(
				fun(Result) -> 
					mnesia:delete_object(Result) 
				end, Results)
			end,
			mnesia:transaction(F)
	end,
	{atomic, Ret} = mnesia:transaction(AF),
	Ret.

getUserByUid(Uid) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(user), X#user.uid =:= Uid]),
		qlc:e(Query)
	end,
	{atomic, User} = mnesia:transaction(AF),
	User.

resetUsersUid(Uid) ->
	Users = getUserByUid(Uid),
	lists:map(
		fun({user, _, Username, Password}) ->
			deleteUser(Username),
			addUser('', Username, Password)
		end, Users).

getUser(Username) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= Username]),
		qlc:e(Query)
		%lists:filter(fun(U) -> U#user.username =:= Username end, Results)
		%hd(Results)
	end,
	{atomic, User} = mnesia:transaction(AF),
	User.

getLogin(Username, Password) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= Username, X#user.password =:= Password]),
		qlc:e(Query)
	end,
	{atomic, User} = mnesia:transaction(AF),
	User.

addFile(Owner, Name, Path, Version, Log, Users, Keywords, Access) ->
	AF = fun() ->
		mnesia:write(#file{owner = Owner, name = Name, path = Path, version = Version, log = Log, users = Users, keywords = Keywords, access = Access})
	end,
	mnesia:transaction(AF).

getAllFiles(Username, Keyword) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(file), lists:any(fun(User) -> User =:= Username end, X#file.users),
									lists:all(fun(Elm) -> lists:any(fun(Key) -> Key =:= Elm end, X#file.keywords) end, Keyword)]),
		Results = qlc:e(Query),
		lists:map(fun(File) -> File#file.name end, Results)
	end,
	{atomic, Files} = mnesia:transaction(AF),
	Files.

getSharedFiles(Username) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(file), lists:any(fun(User) -> User =:= Username end, X#file.users),
									X#file.owner =/= Username]),
		Results = qlc:e(Query),
		lists:map(fun(File) -> File#file.path end, Results)
	end,
	{atomic, Files} = mnesia:transaction(AF),
	Files.

%getFileUsers(Name) ->
%	AF = fun() ->
%		Query = qlc:q([X || X <- mnesia:table(file), X#file.name =:= Name]),
%		Results = qlc:e(Query),
%		hd(lists:map(fun(Item) -> Item#file.users end, Results))
%	end,
%	mnesia:transaction(AF).

getFile(Username, Path) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(file), lists:any(fun(User) -> User =:= Username end, X#file.users),
									X#file.path =:= Path]),
		qlc:e(Query)
	end,
	{atomic, File} = mnesia:transaction(AF),
	File.

deleteFile(Owner, Path) ->
	AF = fun() ->
			Query = qlc:q([X || X <- mnesia:table(file), X#file.owner =:= Owner, X#file.path =:= Path]),
			Results = qlc:e(Query),
	
			F = fun() -> 
				lists:foreach(
				fun(Result) -> 
					mnesia:delete_object(Result) 
				end, Results)
			end,
			mnesia:transaction(F)
	end,
	{atomic, Ret} = mnesia:transaction(AF),
	Ret.

returnAllWritePermissions(Username) ->
	Files = getFileBeingEditedBy(Username),
	{{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
	lists:map(fun(Path) -> 
		[{file, Owner, Name, Path, Version, Log, Users, Keywords, Username}] = getFile(Username, Path),
		deleteFile(Owner, Path),
		addFile(Owner, Name, Path, Version, Log++[atom_to_list(Username)++" stoped editing this file at "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++", on the "++integer_to_list(Day)++"."++integer_to_list(Month)++"."++integer_to_list(Year)++"."], Users, Keywords, writable)
	end, Files).



getFileBeingEditedBy(Username) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(file), Username =:= X#file.access]),
		Results = qlc:e(Query),
		lists:map(fun(File) -> File#file.path end, Results)
	end,
	{atomic, Files} = mnesia:transaction(AF),
	Files.




