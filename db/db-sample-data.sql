begin transaction;

insert into tree values (default, 'portage', 'main tree', 'portage.com', 'gentoo foundation', 'portage@gentoo.org', 'organisation', 1, 'stable', 'official', '/portage7/', 12);
insert into tree values (default, 'gg7', 'gg7 tree', 'gg7.com', 'gg7', 'portage@gg7.com', 'person', 2, 'unstable', 'unofficial', '/overlays/gg7/', 48);

insert into repo values (default, 1, 'git', 'git://portage.com', now());
insert into repo values (default, 1, 'svn', 'svn://portage.com', now());
insert into repo values (default, 2, 'git', 'git://gg7.com', now());

insert into sync values (default, 'failed', 1, '2014-07-02');
insert into sync values (default, 'failed', 2, '2014-07-02 00:00:05');
insert into sync values (default, 'failed', 3, '2014-07-02 00:00:10');

insert into sync values (default, 'completed', 1, '2014-07-03');
insert into sync values (default, 'failed', 3, '2014-07-03 00:00:05');

insert into sync values (default, 'failed', 1, '2014-07-04');
insert into sync values (default, 'completed', 2, '2014-07-04 00:00:05');
insert into sync values (default, 'failed', 3, '2014-07-04 00:00:10');

insert into sync values (default, 'failed', 1, '2014-07-05');
insert into sync values (default, 'failed', 2, '2014-07-05 00:00:05');
insert into sync values (default, 'failed', 3, '2014-07-05 00:00:10');

insert into sync values (default, 'failed', 1, '2014-07-06');
insert into sync values (default, 'failed', 2, '2014-07-06 00:00:05');
insert into sync values (default, 'failed', 3, '2014-07-06 00:00:10');

-- commit;
