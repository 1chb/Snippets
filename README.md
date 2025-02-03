# Snippets
Small lose and fun Haskell fragments

# Installing Postgres on a Debian system, e.g. MX Linux (System V)

> sudo apt install postgresql

> sudo service postgresql status
15/main (port 5432): online

> sudo -i -u postgres
postgres@ > psql
postgres=# CREATE DATABASE mydb;
CREATE DATABASE
postgres=# CREATE USER myuser WITH PASSWORD 'mypassword';
CREATE ROLE
postgres=# GRANT ALL PRIVILEGES ON DATABASE mydb TO myuser;
GRANT
postgres=# \connect mydb
mydb=# GRANT ALL ON SCHEMA public TO myuser;
GRANT
mydb=# \quit

postgres@ > exit
> psql -h localhost mydb myuser
Password for user myuser: (mypassword)
psql (15.10 (Debian 15.10-0+deb12u1))
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, compression: off)
Type "help" for help.

mydb=> \dt
         List of relations
 Schema |   Name   | Type  | Owner  
--------+----------+-------+--------
 public | greeting | table | myuser
(1 row)

mydb=> \d greeting
                                  Table "public.greeting"
 Column  |       Type        | Collation | Nullable |               Default                
---------+-------------------+-----------+----------+--------------------------------------
 id      | bigint            |           | not null | nextval('greeting_id_seq'::regclass)
 message | character varying |           | not null | 
Indexes:
    "greeting_pkey" PRIMARY KEY, btree (id)

mydb=> INSERT INTO greeting (message) VALUES ('Aroun soustei'), ('Hej, hallå');
INSERT 0 2
