-- Pseudocode mixed with Haskell on the graph ML idea

-- basic data types
data User = User {name::String, age:: Int, role:: Role} -- Object User
data Post = Post {title::String, body:: Text} -- Object Post
data Arrow = User -writes{when::Date | require = right }-> Post -- Object 'writes' that is an Arrow between User and Post. 'require = right' means 'writes' is mandatory for Posts

-- permissions
allow action User = True | action -source-> user is Admin  -- allow any action on a User if it is initiated by the Admin
						 | action -source-> node is Trusted -- allow any action on a User if it is initiated from the Trusted Node (e.g., server)

allow create Post = True | create -source-> user is LoggedIn -- allow creation of the Post only if it is initiated by the logged in User
allow [action <- | update, delete ] Post p = True | (action -source-> user -writes-> p) is True -- allow update / delete on the post only by the Author

-- Startup on the Server (Master Node)
Graph mainGraph = loadgraph mainGraph "last 50 posts" -- initial load of the data object graph
publish mainGraph -- making mainGraph available on the other nodes


-- Client (Web Client Node)
Graph clientGraph <-synchronize-> (GVM -> mainNode -> mainGraph) -- making sure clientGraph is synchronized (according to permissions!) with mainGraph (whatever is published)

{-
TODO: basic setup - 
- authenticate a user:
- login
- logout

TODO: GUI part, let's start with the web
- view list of posts
	- view / edit a post
	- delete a post
- create new post
- view all posts for a specific user
-}

-- authentication

credentials >> authenticate -- get credentials from somewhere, negotiate with the server node, authenticate a user on the given node => need remote method calls or some such

-- remote function call:
authenticate (GVM -> mainNode) credentials >> processResult
-- LOOOOTS of questions here: if async, (>> processResult), how do we handle call backs? if sync - everything freezes... Errors?
-- so if async: write as if it's synced. then it gets processed and all reactivities fire up correspondingly. so, REACTIVITY is key.

-- OK, let's try to detail authentication process:
-- GVM - contains ALL nodes information, synchronizes them between server nodes for redundancy (ping server nodes, if down - reconnect clients or something)
-- login: ______ password: ________ [submit] = widgetLogin


widgetLoginBehavior = show wxLoggedIn : wxLoggedOut ? GVM -> currentUser is LoggedIn -- reactive behavior dependnence defined
show wxLoggedIn = "<p> Welcome, {#name}! </p>" -- html
wxLoggedIn -bind-> GVM -> currentUser -- REDUNDANT! we are already tied in the widgetLoginBehavior!

show wxLoggedOut = "<p>login: {login}<br>password: {password}<br><button data-action='login'/>" -- html: needs to be redone to support templates etc
showResult -observe-> authenticate -observe-> wxLoggedOut -- declarative statement that authenticate should process events from wxLoggedOut; showResult observes result of authenticate in turn - so, sync or async is irrelevant
authenticate (login, password) = authenticate (GVM -> mainNode) login (encrypt password) -- what the action does