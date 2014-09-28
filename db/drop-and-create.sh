#!/bin/bash

set -e # exit on errors
set -x # echo executed commands

if ! (/sbin/service postgresql-9.3 status 2>/dev/null | grep --quiet 'status: started'); then
  echo 'postgres is not running!'
  exit 1
fi

SCRIPT_DIRNAME=$(dirname "$(readlink -f "$0")")

. "${SCRIPT_DIRNAME}/env.sh"

dropdb --username=postgres "$PGDATABASE" || true
dropuser --username=postgres "$PGUSER" || true

createuser --username=postgres --no-password "$PGUSER"
createdb --username=postgres --owner="$PGUSER" "$PGDATABASE"

psql --username=postgres --command="ALTER USER $PGUSER WITH PASSWORD '$PGPASSWORD';"
psql --username=postgres --command="ALTER SCHEMA public OWNER TO $PGUSER;"
psql --username=postgres --command="CREATE EXTENSION IF NOT EXISTS citext WITH SCHEMA public;"
