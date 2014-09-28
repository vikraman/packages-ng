#!/bin/bash

set -e # exit on errors

if ! (/sbin/service postgresql-9.3 status 2>/dev/null | grep --quiet 'status: started'); then
  echo 'postgres is not running!'
  exit 1
fi

SCRIPT_DIRNAME=$(dirname "$(readlink -f "$0")")

. "${SCRIPT_DIRNAME}/env.sh"

if [ "$(psql -tAc "SELECT 1 FROM pg_database WHERE datname='$PGDATABASE'")" != "1" ]; then
  echo '$PGDATABASE not found!'
  exit 1
fi

if [ ! -d "${SCRIPT_DIRNAME}/dumps" ]; then
  mkdir "${SCRIPT_DIRNAME}/dumps"
fi

DUMP="${SCRIPT_DIRNAME}/dumps/${PGHOST}-${PGDATABASE}-$(date '+%Y.%m.%d-%H%M%S').sql.xz"

echo -n "creating '${DUMP}'..."
pg_dump --inserts --verbose --serializable-deferrable | xz --stdout > "$DUMP"
echo " done"
