begin transaction;

-- drop all tables and types
-- drop schema public cascade;
-- create schema public;

create table tree
( primary key (id)

, id                 serial    not null
, name               citext    not null      check (name <> '')
, description        citext    not null      check (description <> '')
, homepage           text      not null      check (homepage <> '')

-- on second thought i don't think we should normalise this out
, owner_name         citext    not null      check (owner_name <> '')
, owner_email        citext    not null      check (owner_email <> '')

-- person/project
, owner_type         citext    not null      check (owner_type <> '') check (owner_type = trim(lower(owner_type)))

, priority           integer   not null

-- core/stable/testing/experimental/graveyard
, quality            citext    not null      check (quality <> '') check (quality = trim(lower(quality)))

-- official/unofficial
, status             citext    not null      check (status <> '') check (status = trim(lower(status)))

-- TODO: calculated by gentoostats
-- , popularity   double precision     not null     default 0

-- tree location on the FS (absolute path with a trailing slash)
, path               text      not null      check (left(path, 1) = '/') check (right(path, 1) = '/')

-- sync every X hours
, update_hours       integer   not null      check (update_hours > 0)

, unique (path)
);

create table repo
( primary key (id)

, id                 serial    not null
, tree_id            integer   not null      references tree
, type               citext    not null      check (type <> '') check (type = trim(lower(type)))
, url                text      not null      check (url <> '')

-- Ideally we want to use the timestamp of the last commit (and not the mtime
-- from our FS).
, mtime        timestamp not null

, unique (tree_id, type, url)
);

create table feed
( primary key (tree_id, url)

, tree_id            integer   not null      references tree
, url                text      not null      check (url <> '')
);

create type sync_status as enum ('queued', 'running', 'completed', 'failed');

-- synchronisation (to be used as a queue)
create table sync
( primary key (id)

, id                 serial         not null
, status             sync_status    not null
, repo_id            integer        not null      references repo
, ts                 timestamp      not null
);

-- ┌─────────┬─────────────────────┬─────────────────────┬────────────────┐
-- │ tree_id │      completed      │       latest        │ failure_streak │
-- ├─────────┼─────────────────────┼─────────────────────┼────────────────┤
-- │       1 │ 2014-07-04 00:00:05 │ 2014-07-06 00:00:10 │              4 │
-- │       2 │ ¤                   │ 2014-07-06 00:00:10 │              5 │
-- └─────────┴─────────────────────┴─────────────────────┴────────────────┘
create view sync_stats as
  with
    trees_and_syncs as (
      select r.tree_id as tree_id
           , s.status as sync_status
           , s.ts as sync_ts
      from sync s
      join repo r on (s.repo_id = r.id)
    ),
    completed_sync_stats as (
      select t.id as tree_id
           , ( select max(sync_ts) as completed
               from trees_and_syncs tas
               where t.id = tas.tree_id
                 and sync_status = 'completed'
             )
      from tree t
      group by tree_id
    )
  select css.tree_id
       , css.completed
       , ( select max(tas.sync_ts)
           from trees_and_syncs tas
           where tas.tree_id = css.tree_id
         ) as latest
       , ( select count(*)
           from trees_and_syncs tas
           where css.tree_id = tas.tree_id
             and sync_status = 'failed'
             and (css.completed is null or sync_ts >= css.completed)
         ) as failure_streak
  from completed_sync_stats css
  order by css.tree_id
;

create table category_name
( primary key (id)

, id                 serial    not null
, name               citext    not null      check (name <> '') check (name = trim(lower(name)))
);

-- create unique index category_name_lower_name_idx on category (lower(name));
create unique index category_name_idx on category_name (name);
cluster category_name using category_name_idx;

create table category
( primary key (tree_id, category_name_id)

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name

, description        citext
, mtime              timestamp not null
);

create index on category (category_name_id);

create table package_name
( primary key (id)

, id                 serial    not null
, name               citext    not null      check (name <> '') check (name = trim(lower(name)))
);

create unique index package_name_idx on package_name (name);
cluster package_name using package_name_idx;

create table package
( primary key (tree_id, category_name_id, package_name_id)

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name

-- Natural name for package, example: LibreOffice (for app-office/libreoffice)
, natural_name       text

-- A long description of the package in freetext
, description        citext

, long_description   citext

-- TODO:
-- changelog
-- upstream

, mtime              timestamp not null
);

create index on package (category_name_id);
create index on package (package_name_id);

create table herd_name
( primary key (id)

, id                 serial    not null
, name               text      not null      check (name <> '') check (name = trim(lower(name)))

, unique (name)
);

create table package_herd
( primary key (tree_id, category_name_id, package_name_id, herd_name_id)
, foreign key (tree_id, category_name_id, package_name_id) references package

, tree_id            integer  not null       references tree
, category_name_id   integer  not null       references category_name
, package_name_id    integer  not null       references package_name

, herd_name_id       integer  not null       references herd_name
);

create table maintainer_email
( primary key (id)

, id                 serial    not null
, email              text      not null      check (email <> '') check (email = trim(lower(email)))

-- TODO: <!ELEMENT maintainer ( email, (description| name)* )>

, unique (email)
);

create table package_maintainer
( primary key (tree_id, category_name_id, package_name_id, maintainer_email_id)

, tree_id             integer not null       references tree
, category_name_id    integer not null       references category_name
, package_name_id     integer not null       references package_name
, maintainer_email_id integer not null       references maintainer_email
);

create table useflag_name
( primary key (id)

, id                 serial    not null
, name               citext    not null      check (name <> '') check (name = trim(lower(name)))
);

create unique index useflag_name_idx on useflag_name (name);
cluster useflag_name using useflag_name_idx;

create table package_useflag_description
( primary key (tree_id, category_name_id, package_name_id, useflag_name_id)

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name
, useflag_name_id    integer   not null      references useflag_name

, description        text      not null      check (description <> '')
);

create table keyword_name
( primary key (id)

, id                 serial    not null
, name               citext    not null      check (name <> '') check (name = trim(lower(name)))

, unique (name)
);

create table homepage_url
( primary key (id)

, id                 serial    not null
, url                text      not null      check (url <> '')

, unique (url)
);

create table ebuild
( primary key (tree_id, category_name_id, package_name_id, version)

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name
, version            citext    not null

, eapi               text      not null
, depend             text
, rdepend            text
, license            text
, slot               text

-- ebuild path on the FS
, path               text      not null      check (left(path, 1) = '/')

, mtime              timestamp not null
);

create table ebuild_homepages
( primary key (tree_id, category_name_id, package_name_id, version, homepage_url_id)
, foreign key (tree_id, category_name_id, package_name_id, version) references ebuild

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name
, version            citext    not null

, homepage_url_id    integer   not null      references homepage_url
);

create table ebuild_keywords
( primary key (tree_id, category_name_id, package_name_id, version, keyword_name_id)
, foreign key (tree_id, category_name_id, package_name_id, version) references ebuild

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name
, version            citext    not null

, keyword_name_id    integer   not null      references keyword_name
);

create table ebuild_useflags
( primary key (tree_id, category_name_id, package_name_id, version, useflag_name_id, default_choice)
, foreign key (tree_id, category_name_id, package_name_id, version) references ebuild

, tree_id            integer   not null      references tree
, category_name_id   integer   not null      references category_name
, package_name_id    integer   not null      references package_name
, version            citext    not null

, useflag_name_id    integer   not null      references useflag_name

-- "Add + or - before the name of the use flag in IUSE to turn it on or off by default."
--   '+gtk' -> default_choice = true
--   '-gtk' -> default_choice = false
--   'gtk'  -> default_choice = NULL
, default_choice     boolean
);

-- commit;
