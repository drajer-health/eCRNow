#!/bin/bash

# Stop on error
set -e

# Variables
DATA_DIR=/mnt/pgsql/data

#start postgreSQL
echo "starting postgreSQL ... "

if [ ! -d "/mnt/pgsql/data" ]; then #check whether the DB is initialized.
    echo "Initializing database"
    echo "postgres" > /tmp/admin_pass
    su postgres -c "initdb --username=postgres --pwfile=/tmp/admin_pass -D /mnt/pgsql/data"
    echo "Database initialized"
    echo "host    all             all             all                     md5" >> /mnt/pgsql/data/pg_hba.conf 
    echo "listen_addresses = '*'   " >> /mnt/pgsql/data/postgresql.conf
	echo "Updated pg_hba.conf and postgresql.conf" 
fi

su postgres -c "pg_ctl -D $DATA_DIR -l ${DATA_DIR}/logfile start "

echo "started postgreSQL"

# keep the stdin
/bin/bash
tail -f /dev/null