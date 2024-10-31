#!/bin/bash

echo "Checking Neon postgres server status..."
# while : ; do
#     pg_isready -h ep-falling-sunset-a4546ynz.us-east-1.aws.neon.tech -U default > /dev/null && break;
#     sleep 1;
# done

echo "Postgres is ready, starting main container init."
init-all;

echo "All set, awaiting commands."
sleep infinity;