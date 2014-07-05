#!/bin/sh
# Usage: sudo -u postgres ./db-recreate.sh
# TODO: replace this with puppet

set -e
set -x

. ./db-env.sh

dropdb --username=postgres "$PGDATABASE" || true
dropuser --username=postgres "$PGUSER" || true

createuser --username=postgres --no-password "$PGUSER"
createdb --username=postgres --owner="$PGUSER" "$PGDATABASE"

psql --username=postgres --command="ALTER USER $PGUSER WITH PASSWORD '$PGPASSWORD';"
psql --username=postgres --command="ALTER SCHEMA public OWNER TO $PGUSER;"
psql --username=postgres --command="CREATE EXTENSION IF NOT EXISTS citext WITH SCHEMA public;"
