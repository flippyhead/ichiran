#!/bin/bash

echo "========================="
echo "Starting ichiran DB init!"
echo "========================="

# Download the latest dump file if it doesn't exist
DUMP_URL="https://github.com/tshatrov/ichiran/releases/download/ichiran-240107/jmdict-070124.pgdump"
if [ ! -f "ichiran.pgdump" ]; then
    echo "Downloading database dump from ${DUMP_URL}..."
    curl -L ${DUMP_URL} --output ichiran.pgdump
    chmod o+r ichiran.pgdump
else
    echo "Using existing database dump file..."
fi

# Connection details
export PGHOST=localhost
export PGPORT=5432
export PGUSER=postgres
export PGPASSWORD=he7rSOeYCrgTjHo
export PGDATABASE=postgres

# Terminate existing connections and drop database
echo "Terminating existing connections..."
psql -h $PGHOST \
     -p $PGPORT \
     -U $PGUSER \
     -d postgres \
     -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'jmdict';"

echo "Dropping existing database if it exists..."
dropdb -h $PGHOST \
       -p $PGPORT \
       -U $PGUSER \
       --if-exists \
       jmdict

# Create database with UTF8
echo "Creating database..."
createdb -h $PGHOST \
        -p $PGPORT \
        -U $PGUSER \
        -E 'UTF8' \
        -T template0 \
        jmdict

# Drop all existing tables if any exist
echo "Dropping all existing tables..."
psql -h $PGHOST \
     -p $PGPORT \
     -U $PGUSER \
     -d jmdict \
     -c "DO \$\$ DECLARE
         r RECORD;
     BEGIN
         FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = 'public') LOOP
             EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
         END LOOP;
     END \$\$;"

# Restore the dump
echo "Restoring database from dump..."
pg_restore -h $PGHOST \
          -p $PGPORT \
          -U $PGUSER \
          -d jmdict \
          --no-owner \
          --no-privileges \
          --clean \
          --if-exists \
          -v \
          ichiran.pgdump

echo "========================="
echo "Finished ichiran DB init!"
echo "========================="