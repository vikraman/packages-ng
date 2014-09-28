comment on table category is 'Identified by (tree + category_name)';
comment on table ebuild is 'Identified by (tree + category_name + package_name + version)';
comment on table feed is 'RSS feeds';
comment on table package is 'Identified by (tree + category_name + package_name)';
comment on table package_useflag_description is 'A description for a particular (tree + category_name + package_name + useflag_name)';
comment on table sync is 'Tree synchronisation tasks (used as a queue)';
comment on table tree is 'An ebuild repository/overlay';
comment on table ebuild_useflags is '(tree + category_name + package_name + version + useflag_name + default_choice)';
