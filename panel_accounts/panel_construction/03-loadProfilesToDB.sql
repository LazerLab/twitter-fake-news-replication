## to get in:
%  mysql yourDBHere [...]
# (the flag local-infile is needed if we want to use "load data local infile" during the session)

#  increase tmp table size, so it doesn't spend eons writing stuff to disk
SET tmp_table_size = 1024 * 1024 * 64; 		 # == 64M, as opposed to the observed default of 16M
SET myisam_sort_buffer_size = 256*1024*1024; # as opposed to the observed default of 8M. This value comes from MySQL docs example.
# to check: SHOW VARIABLES LIKE 'tmp_table_size'


create table if not exists exampleProfiles (id bigint primary key, name varchar(400), handle varchar(100),
	location varchar(400), nameHandleWords varchar(400))
	engine=MyISAM charset='utf8mb4';	   # charset is for unicode
load data local infile '/home/lfriedl/twitter-fake-news-replication/matching/sample-data/03-profiles-name-words.txt' into table exampleProfiles
	charset 'utf8mb4' fields escaped by '' ignore 1 lines
	(id, @skipField, name, handle, location, nameHandleWords);

# note: before running this, we adjusted an admin setting on the database so that it would include words with as few as 2 letters in the index 
create fulltext index idx_indexWords on exampleProfiles (nameWords, handleWords);		

